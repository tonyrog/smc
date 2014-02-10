//
// smc_drv.c   SMC darwin / erlang driver
//
//

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <IOKit/IOKitLib.h>

#include "erl_driver.h"

#define ATOM(NAME) am_ ## NAME
#define INIT_ATOM(NAME) am_ ## NAME = driver_mk_atom(#NAME)

// Hack to handle R15 driver used with pre R15 driver
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif

#define PORT_CONTROL_BINARY

#define INT_EVENT(e) ((int)((long)(e)))

#define CMD_OPEN        1
#define CMD_CLOSE       2
#define CMD_IOCALL      3
#define CMD_DEBUG       4

#define KERNEL_INDEX_SMC 2
#define KERNEL_INDEX_MOTION_SENSOR 5

static inline int32_t get_int32(uint8_t* ptr)
{
    uint32_t value = (ptr[0]<<24) | (ptr[1]<<16) | (ptr[2]<<8) | (ptr[3]<<0);
    return (int32_t) value;
}

static inline uint32_t get_uint32(uint8_t* ptr)
{
    uint32_t value = (ptr[0]<<24) | (ptr[1]<<16) | (ptr[2]<<8) | (ptr[3]<<0);
    return value;
}

static inline uint16_t get_uint16(uint8_t* ptr)
{
    uint16_t value = (ptr[0]<<8) | (ptr[1]<<0);
    return value;
}

static inline uint8_t get_uint8(uint8_t* ptr)
{
    uint8_t value = (ptr[0]<<0);
    return value;
}

static inline void put_uint16(uint8_t* ptr, uint16_t v)
{
    ptr[0] = v>>8;
    ptr[1] = v;
}

static inline void put_uint32(uint8_t* ptr, uint32_t v)
{
    ptr[0] = v>>24;
    ptr[1] = v>>16;
    ptr[2] = v>>8;
    ptr[3] = v;
}

typedef struct _smc_ctx_t
{
    ErlDrvPort port;
    const char*  name;    // current Service name
    int          is_open; // is connection open
    io_connect_t conn;    // service connection
} smc_ctx_t;

static int  smc_drv_init(void);
static void smc_drv_finish(void);
static void smc_drv_stop(ErlDrvData);
static void smc_drv_output(ErlDrvData, char*, ErlDrvSizeT);
static void smc_drv_event(ErlDrvData d, ErlDrvEvent e,
			  ErlDrvEventData ed);
static void smc_drv_ready_input(ErlDrvData, ErlDrvEvent);
static void smc_drv_ready_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData smc_drv_start(ErlDrvPort, char* command);
static ErlDrvSSizeT smc_drv_ctl(ErlDrvData,unsigned int,char*,ErlDrvSizeT,char**, ErlDrvSizeT);
static void smc_drv_timeout(ErlDrvData);
static void smc_drv_stop_select(ErlDrvEvent, void*);

ErlDrvTermData am_ok;
ErlDrvTermData am_error;
ErlDrvTermData am_undefined;

static ErlDrvEntry smc_drv_entry;

#define DLOG_DEBUG     7
#define DLOG_INFO      6
#define DLOG_NOTICE    5
#define DLOG_WARNING   4
#define DLOG_ERROR     3
#define DLOG_CRITICAL  2
#define DLOG_ALERT     1
#define DLOG_EMERGENCY 0
#define DLOG_NONE     -1

#ifndef DLOG_DEFAULT
#define DLOG_DEFAULT DLOG_NONE
#endif

#define DLOG(level,file,line,args...) do { \
	if (((level) == DLOG_EMERGENCY) ||				\
	    ((debug_level >= 0) && ((level) <= debug_level))) { \
	    emit_log((level),(file),(line),args);			\
	}								\
    } while(0)

#define DEBUGF(args...) DLOG(DLOG_DEBUG,__FILE__,__LINE__,args)
#define INFOF(args...)  DLOG(DLOG_INFO,__FILE__,__LINE__,args)
#define NOTICEF(args...)  DLOG(DLOG_NOTICE,__FILE__,__LINE__,args)
#define WARNINGF(args...)  DLOG(DLOG_WARNING,__FILE__,__LINE__,args)
#define ERRORF(args...)  DLOG(DLOG_ERROR,__FILE__,__LINE__,args)
#define CRITICALF(args...)  DLOG(DLOG_CRITICAL,__FILE__,__LINE__,args)
#define ALERTF(args...)  DLOG(DLOG_ALERT,__FILE__,__LINE__,args)
#define EMERGENCYF(args...)  DLOG(DLOG_EMERGENCY,__FILE__,__LINE__,args)

static int debug_level = DLOG_DEFAULT;

static void emit_log(int level, char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    if ((level == DLOG_EMERGENCY) ||
	((debug_level >= 0) && (level <= debug_level))) {
	int save_errno = errno;
	va_start(ap, line);
	fmt = va_arg(ap, char*);
	fprintf(stderr, "%s:%d: ", file, line); 
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\r\n");
	va_end(ap);
	errno = save_errno;
    }
}


kern_return_t SMCOpen(char* name, io_connect_t *conn)
{
    kern_return_t result;
    mach_port_t   masterPort;
    io_iterator_t iterator;
    io_object_t   device;

    IOMasterPort(MACH_PORT_NULL, &masterPort);
    
    CFMutableDictionaryRef matchingDictionary = IOServiceMatching(name);
    result = IOServiceGetMatchingServices(masterPort, matchingDictionary, &iterator);
    if (result != kIOReturnSuccess) {
        printf("Error: IOServiceGetMatchingServices() = %08x\n", result);
        return 1;
    }

    device = IOIteratorNext(iterator);
    IOObjectRelease(iterator);
    if (device == 0) {
        printf("Error: no SMC found\n");
        return 1;
    }

    result = IOServiceOpen(device, mach_task_self(), 0, conn);
    IOObjectRelease(device);
    if (result != kIOReturnSuccess) {
        printf("Error: IOServiceOpen() = %08x\n", result);
        return 1;
    }
    return kIOReturnSuccess;
}

kern_return_t SMCClose(io_connect_t conn)
{
    return IOServiceClose(conn);
}


kern_return_t IOCall(io_connect_t conn, int index, 
		     void* ibuf, size_t ibufsize,
		     void* obuf, size_t* obufsizep) 
{
#if (MAC_OS_X_VERSION_MIN_REQUIRED <= MAC_OS_X_VERSION_10_4)
    // Only for 32-bit !
    // Sould be replace with IOConnectCall* (from 10.5 )
    return IOConnectMethodStructureIStructureO(conn,index,
					       ibufsize,
					       obufsizep,
					       ibuf,
					       obuf);
#else
    // Nice! change all arguments around a bit, great improvment and 
    // also backward compatible ... ?
    return IOConnectCallStructMethod(conn,index,
				     ibuf,
				     ibufsize,
				     obuf,
				     obufsizep);
#endif
}

/* general control reply function */
static ErlDrvSSizeT ctl_reply(int rep, void* buf, ErlDrvSizeT len,
			      char** rbuf, ErlDrvSizeT rsize)
{
    char* ptr;

    if ((len+1) > rsize) {
#ifdef PORT_CONTROL_BINARY
	ErlDrvBinary* bin = driver_alloc_binary(len+1);
	if (bin == NULL) 
	    return -1;
	ptr = bin->orig_bytes;	
	*rbuf = (char*) bin;
#else
	if ((ptr = driver_alloc(len+1)) == NULL)
	    return -1;
	*rbuf = ptr;
#endif
    }
    else
	ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}


// setup global object area
// load atoms etc.

static int smc_drv_init(void)
{
    debug_level = DLOG_DEFAULT;
    DEBUGF("smc_driver_init");
    INIT_ATOM(ok);
    INIT_ATOM(error);
    INIT_ATOM(undefined);
    return 0;
}

// clean up global stuff
static void smc_drv_finish(void)
{
}

static ErlDrvData smc_drv_start(ErlDrvPort port, char* command)
{
    (void) command;
    smc_ctx_t* ctx;

    if ((ctx = (smc_ctx_t*) driver_alloc(sizeof(smc_ctx_t))) == NULL) {
	errno = ENOMEM;
	return ERL_DRV_ERROR_ERRNO;
    }
    memset(ctx, '\0', sizeof(smc_ctx_t));
    ctx->port = port;
    DEBUGF("smc_drv: start (%s)", command);
#ifdef PORT_CONTROL_BINARY
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
#endif
    return (ErlDrvData) ctx;
}

static void smc_drv_stop(ErlDrvData d)
{
    smc_ctx_t* ctx = (smc_ctx_t*) d;
    if (ctx->is_open)
	SMCClose(ctx->conn);
    driver_free(ctx);
}

static ErlDrvSSizeT smc_drv_ctl(ErlDrvData d, 
				 unsigned int cmd, char* buf0, ErlDrvSizeT len,
				 char** rbuf, ErlDrvSizeT rsize)
{
    smc_ctx_t* ctx = (smc_ctx_t*) d;
    uint8_t* buf = (uint8_t*) buf0;

    DEBUGF("smc_drv: ctl: cmd=%u, len=%d", 
	   cmd, len);
    switch(cmd) {
    case CMD_OPEN: {
	if (len != 0) goto badarg;
	if (ctx->is_open) goto badarg;
	if (SMCOpen("AppleSMC", &ctx->conn) != kIOReturnSuccess)
	    goto badarg;
	ctx->name = "AppleSMC";
	ctx->is_open = 1;
	goto ok;
    }
    
    case CMD_CLOSE: {
	if (ctx->is_open) {
	    SMCClose(ctx->conn);
	    ctx->is_open = 0;
	}
	goto ok;
    }

    case CMD_IOCALL: {
	size_t   txlen;
	size_t   rxlen;
	uint8_t* txbuf;
	uint8_t* rxbuf;
	uint8_t  tbuf[1024];
	
	if (!ctx->is_open) goto badarg;
	if (len < 2) goto badarg;
	rxlen   = get_uint16(buf);
	txbuf   = buf+2;
	txlen   = len-2;
	if (rxlen > 1024)
	    goto badarg;
	rxbuf = tbuf;
	memset(rxbuf, '\0', rxlen);
	
	if (IOCall(ctx->conn, KERNEL_INDEX_SMC, 
		   txbuf, txlen,
		   rxbuf, &rxlen) != kIOReturnSuccess)
	    goto badarg;
	return ctl_reply(3, rxbuf, rxlen, rbuf, rsize);
    }

    case CMD_DEBUG: {
	if (len != 4)
	    goto badarg;
	debug_level = get_uint32(buf);
	goto ok;
    }

    default:
	goto badarg;
    }

ok:
    return ctl_reply(0, NULL, 0, rbuf, rsize);
badarg:
    errno = EINVAL;
    goto error;
error:
    {
        char* err_str = erl_errno_id(errno);
	return ctl_reply(255, err_str, strlen(err_str), rbuf, rsize);
    }
}


static void smc_drv_output(ErlDrvData d, char* buf, ErlDrvSizeT len)
{
    (void) d;
    (void) buf;
    (void) len;
    // smc_ctx_t* ctx = (smc_ctx_t*) d;
    DEBUGF("smc_drv: output");
}

static void smc_drv_outputv(ErlDrvData d, ErlIOVec *ev)
{
    (void) d;
    (void) ev;
    // smc_ctx_t* ctx = (smc_ctx_t*) d;
    DEBUGF("smc_drv: outputv");
}

static void smc_drv_event(ErlDrvData d, ErlDrvEvent e,
			  ErlDrvEventData ed)
{
    (void) d;
    (void) e;
    (void) ed;
    // smc_ctx_t* ctx = (smc_ctx_t*) d;
    DEBUGF("smc_drv: event called");
}

static void smc_drv_ready_input(ErlDrvData d, ErlDrvEvent e)
{
    (void) d;
    (void) e;
    // smc_ctx_t* ctx = (smc_ctx_t*) d;
    DEBUGF("smc_drv: ready_input called");
}

static void smc_drv_ready_output(ErlDrvData d, ErlDrvEvent e)
{
    (void) d;
    (void) e;
    // smc_ctx_t* ctx = (smc_ctx_t*) d;
    DEBUGF("smc_drv: ready_output called");
}

// operation timed out
static void smc_drv_timeout(ErlDrvData d)
{
    (void) d;
    DEBUGF("smc_drv: timeout");
}

static void smc_drv_stop_select(ErlDrvEvent event, void* arg)
{
    (void) arg;
    DEBUGF("smc_drv: stop_select event=%d", INT_EVENT(event));
    close(INT_EVENT(event));
}

DRIVER_INIT(smc_drv)
{
    ErlDrvEntry* ptr = &smc_drv_entry;

    DEBUGF("smc DRIVER_INIT");

    ptr->driver_name = "smc_drv";
    ptr->init  = smc_drv_init;
    ptr->start = smc_drv_start;
    ptr->stop  = smc_drv_stop;
    ptr->output = smc_drv_output;
    ptr->ready_input  = smc_drv_ready_input;
    ptr->ready_output = smc_drv_ready_output;
    ptr->finish = smc_drv_finish;
    ptr->control = smc_drv_ctl;
    ptr->timeout = smc_drv_timeout;
    ptr->outputv = smc_drv_outputv;
    ptr->ready_async = 0;
    ptr->flush = 0;
    ptr->call = 0;
    ptr->event = smc_drv_event;
    ptr->extended_marker = ERL_DRV_EXTENDED_MARKER;
    ptr->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    ptr->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    ptr->driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
    ptr->process_exit = 0;
    ptr->stop_select = smc_drv_stop_select;
    return ptr;
}
