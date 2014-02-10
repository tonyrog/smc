%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2013, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author tony <tony@rogvall.se>
%%% @doc
%%%    Apple SMC (System Managment control) api
%%% @end
%%% Created :  22 Aug 2013 by tony <tony@rogvall.se>

-module(smc).
-behaviour(gen_server).

-export([read_key/1]).
-export([read_index/1]).
-export([read_info/1]).
-export([read_bytes/2]).
-export([write_bytes/2]).

-export([read_all/0]).
-export([read_motion/0]).
-export([read_fans/0]).
-export([read_fan/1]).
-export([read_temp/0]).
-export([set_backlight/1]).

-export([i/0]).
-export([foldl/2, foldr/2]).
-export([decode/2, decode/3]).

-export([io_call/2]).  %% lowest-level
-export([debug/1]).

%% gen_server api
-export([start/0,
	 start_link/0]).
-export([stop/0]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-import(lists, [reverse/1]).

-include("../include/smc.hrl").
-define(dbg(F,A), ok).

-define(SMC_PORT, smc_port).
-define(SMC_SRV,  smc_srv).

-define(CMD_OPEN,        1).
-define(CMD_CLOSE,       2).
-define(CMD_IOCALL,      3).
-define(CMD_DEBUG,       4).

-define(is_uint8(T), (((T) band (bnot 16#ff)) =:=  0)).
-define(is_uint16(T), (((T) band (bnot 16#ffff)) =:=  0)).
-define(is_uint32(T), (((T) band (bnot 16#ffffffff)) =:=  0)).

-define(APPLESMC_MAX_DATA_LENGTH, 32).

-define(SMC_CMD_READ_BYTES,    5).
-define(SMC_CMD_WRITE_BYTES,   6).
-define(SMC_CMD_READ_KEY_COUNT,7). %% count of what?
-define(SMC_CMD_READ_INDEX,    8).
-define(SMC_CMD_READ_KEYINFO,  9).
-define(SMC_CMD_READ_PLIMIT,   11).
-define(SMC_CMD_READ_VERS,     12).

-define(KERNEL_INDEX_SMC,            2). 
-define(KERNEL_INDEX_MOTION_SENSOR,  5).

%% 
%% This is the native structure 
%% But the SMC data is in big endian
%%
-define(SMCK_vers(Major,Minor,Build,Pad,Release),
	Major:8,Minor:8,Build:8,Pad:8,Release:16/native).
-define(SMCK_vers_z, ?SMCK_vers(0,0,0,0,0)).

-define(SMCK_limit(Version,Length,Cpu,Gpu,Mem),
	Version:16/native,Length:16/native,
	Cpu:32/native,Gpu:32/native,Mem32:32/native).
-define(SMCK_limit_z, ?SMCK_limit(0,0,0,0,0)).

-define(SMCK_key_info(Size,Type,Attrib,Pad),
	Size:32/native,Type:4/binary,Attrib:8,Pad:24).

-define(SMCKeyData(Key,Vers,Limit,Pad0,
		   Size,Type,Attrib,Pad1,
		   Result,Status,Data8,Pad2,Data32,Bytes),
	<<Key:4/binary,
	  Vers:6/binary,Pad0:16,Limit:16/binary,
	  ?SMCK_key_info(Size,Type,Attrib,Pad1),
	  Result:8,Status:8,Data8:8,Pad2:8,Data32:32/native,
	  Bytes:32/binary>>).

%% find the key at index I
read_index(I) when is_integer(I) ->
    Cmd = ?SMCKeyData(<<0,0,0,0>>, <<0:6/unit:8>>, <<0:16/unit:8>>, 0,
		      0,<<0:4/unit:8>>,0,0,
		      0,0,?SMC_CMD_READ_INDEX,0,I,<<0:32/unit:8>>),
    ReplySize = byte_size(Cmd),
    ?dbg("read_index: Cmd = ~p, size=~w\n", [Cmd, ReplySize]),
    case io_call(Cmd, ReplySize) of
	{ok,_Rep=?SMCKeyData(RKey,_,_,_Pad0,  %% key,vers,limit
			    _Size, _Type,_Attrib1,_Pad1,
			    _Result,_Status,_Data8,_Pad2,_Data32,_Bytes)} ->
	    ?dbg("Rep = ~p, Size=~w\n", [_Rep,_Size]),
	    <<K3,K2,K1,K0>> = RKey,
	    {ok,<<K0,K1,K2,K3>>};
	Error ->
	    Error
    end.

read_info(Key=(<<K0,K1,K2,K3>>)) ->
    RKey = <<K3,K2,K1,K0>>,
    Cmd = ?SMCKeyData(RKey, <<0:6/unit:8>>, <<0:16/unit:8>>, 0,
		      0,<<0:4/unit:8>>,0,0,
		      0,0,?SMC_CMD_READ_KEYINFO,0,0,<<0:32/unit:8>>),
    ReplySize = byte_size(Cmd),
    ?dbg("Cmd = ~p, size=~w\n", [Cmd, ReplySize]),
    case io_call(Cmd, ReplySize) of
	{ok,_Rep=?SMCKeyData(_,_,_,_Pad0,  %% key,vers,limit
			     Size, RType,_Attrib1,_Pad1,
			     _Result,_Status,_Data8,_Pad2,_Data32,_Bytes)} ->
	    <<T3,T2,T1,T0>> = RType,
	    Type = <<T0,T1,T2,T3>>,
	    ?dbg("Rep = ~p, Size=~w, Type=~p\n", [_Rep,Size,Type]),
	    {ok,{Key,Type,Size}};
	Error ->
	    Error
    end.

read_bytes(Key=(<<K0,K1,K2,K3>>), Size) when is_integer(Size), Size >= 0 ->
    RKey = <<K3,K2,K1,K0>>,
    Cmd = ?SMCKeyData(RKey, <<0:6/unit:8>>, <<0:16/unit:8>>, 0,
		      Size, <<0:4/unit:8>>,0,0,
		      0,0,?SMC_CMD_READ_BYTES,0,0,<<0:32/unit:8>>),
    ?dbg("Cmd = ~p\n", [Cmd]),
    ReplySize = byte_size(Cmd),
    case io_call(Cmd,ReplySize) of
	{ok,_Rep=?SMCKeyData(_,_,_,_Pad0,
			    _Size,_Type,_Attrib,_Pad1,
			    _Result,_Status,_Data8,_Pad2,_Data32,
			    Bytes)} ->
	    ?dbg("Rep = ~p\n", [_Rep]),
	    <<Data:Size/binary,_/binary>> = Bytes,
	    {ok,{Key,Data}};
	Error ->
	    Error
    end.

write_bytes(_Key=(<<K0,K1,K2,K3>>), Data) when is_binary(Data), 
					       byte_size(Data) =< 32 ->
    RKey = <<K3,K2,K1,K0>>,
    Size = byte_size(Data),
    Pad  = 32 - Size,
    Cmd = ?SMCKeyData(RKey, <<0:6/unit:8>>, <<0:16/unit:8>>, 0,
		      Size, <<0:4/unit:8>>,0,0,
		      0,0,?SMC_CMD_WRITE_BYTES,0,0,
		      <<Data/binary,0:Pad/unit:8>>),
    ?dbg("Cmd = ~p\n", [Cmd]),
    ReplySize = byte_size(Cmd),
    ?dbg("ReplySize = ~w\n", [ReplySize]),
    case io_call(Cmd,ReplySize) of
	{ok,_Rep=?SMCKeyData(_,_,_,_Pad0,
			     _Size,_Type,_Attrib,_Pad1,
			     _Result,_Status,_Data8,_Pad2,_Data32,
			     _Bytes)} ->
	    ?dbg("Rep = ~p\n", [_Rep]),
	    ok;
	Error ->
	    Error
    end.

%% set backlight pwm value (only works as super user!)
set_backlight(Value) ->
    write_bytes(<<"LKSB">>, <<Value:16>>).

read_key(Key) ->
    case read_info(Key) of
	{ok,{Key,Type,Size}} ->
	    case read_bytes(Key,Size) of
		{ok,{Key,Data}} ->
		    Value = decode(Type,Key,Data),
		    {ok,{Key,Type,Value}};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

i() ->
    foldl(fun({_I,K,Type,Value},_Acc) ->
		  io:format("~s ~s : ~p\n", [K,Type,Value])
	  end, ok).

read_all() ->
    foldr(fun({_I,K,Type,Value},Acc) ->
		  [{K,Type,Value}|Acc]
	  end, []).

foldl(Fun, Acc) ->
    {ok,{_,_,N}} = read_key(<<"#KEY">>),
    lists:foldl(
      fun(I,Acci) ->
	      {ok,K} = read_index(I),
	      {ok,{_,Type,Value}} = read_key(K),
	      Fun({I,K,Type,Value},Acci)
      end, Acc, lists:seq(0,N-1)).

foldr(Fun, Acc) ->
    {ok,{_,_,N}} = read_key(<<"#KEY">>),
    lists:foldl(
      fun(I,Acci) ->
	      {ok,K} = read_index(I),
	      {ok,{_,Type,Value}} = read_key(K),
	      Fun({I,K,Type,Value},Acci)
      end, Acc, lists:seq(N-1,0,-1)).

%%
%% read_motion can be used for orientaion as well!
%%
read_motion() ->
    %% 250.5! 
    {ok,{_,<<X:16/signed>>}} = read_bytes(<<"MO_X">>, 2),
    {ok,{_,<<Y:16/signed>>}} = read_bytes(<<"MO_Y">>, 2),
    {ok,{_,<<Z:16/signed>>}} = read_bytes(<<"MO_Z">>, 2),
    {X/250.5,Y/250.5,Z/250.5}.

read_fans() ->
    {ok,{_,_,Mode}} = read_key(<<"FS! ">>),
    case read_key(<<"FNum">>) of
	{ok,{_,_,N}} ->
	    [{{fan,I},read_fan_(I,Mode)} || I <- lists:seq(0, N-1)];
	_ ->
	    []
    end.

read_fan(I) when is_integer(I), I>=0, I=<9 ->
    {ok,{_,_,Mode}} = read_key(<<"FS! ">>),
    read_fan_(I,Mode).


read_fan_(I, Mode) ->
    X = $0+I,
    lists:append(
      [opt_read_key(<<$F,X,$I,$D>>, id),
       opt_read_key(<<$F,X,$A,$c>>, actual_speed),
       opt_read_key(<<$F,X,$M,$n>>, minimum_speed),
       opt_read_key(<<$F,X,$M,$x>>, maximum_speed),
       opt_read_key(<<$F,X,$S,$f>>, safe_speed),
       opt_read_key(<<$F,X,$T,$g>>, target_speed),
       if Mode band (1 bsl I) =:= 0 -> [{mode,auto}];
	  true -> [{mode,forced}]
       end]).

read_temp() ->    
    lists:append([
       opt_read_temp_key(<<"TG0T">>, gpu_heatsink1),
       opt_read_temp_key(<<"TH0P">>, hdd_proximity),
       opt_read_temp_key(<<"TO0P">>, odd_proximity),
       opt_read_temp_key(<<"Tm0P">>, mlb_proximity),

       opt_read_temp_key(<<"Tp0P">>, power_supply_proximity),
       opt_read_temp_key(<<"TW0P">>, wireless_airport_proximity),

       opt_read_temp_key(<<"TA0P">>, ambient),

       opt_read_temp_key(<<"TC0D">>, cpu_die),
       opt_read_temp_key(<<"TC0P">>, cpu_proximity),
       opt_read_temp_key(<<"TC0H">>, cpu_heatsink),

       opt_read_temp_key(<<"TG0D">>, gpu_die),
       opt_read_temp_key(<<"TG0P">>, gpu_proximity),
       opt_read_temp_key(<<"TG0H">>, gpu_heatsink),

       opt_read_temp_key(<<"TB0T">>, enclosure_bottom_side_0),
       opt_read_temp_key(<<"TB1T">>, enclosure_bottom_side_1),
       opt_read_temp_key(<<"TB2T">>, enclosure_bottom_side_2),
       opt_read_temp_key(<<"TB3T">>, enclosure_bottom_side_3),

       
       %% {<<"TN0D">>,<<"sp78">>,59.0},
       opt_read_temp_key(<<"TN0P">>, northbridge_pos_1),
       %% {<<"TTF0">>,<<"sp78">>,0.0},
       %% {<<"Th0H">>,<<"sp78">>,0.0},
       opt_read_temp_key(<<"Th0H">>, main_heat_sink_1),
       opt_read_temp_key(<<"Th1H">>, main_heat_sink_2),
       opt_read_temp_key(<<"ThFH">>, main_heat_sink_15)
       %% {<<"Ts0P">>,<<"sp78">>,31.0625}
       %% {<<"Ts0S">>,<<"sp78">>,40.44140625}
       ]).

%% sp78 with some special values
opt_read_temp_key(Key, Name) ->
    case read_bytes(Key, 2) of
	{ok,{_,<<0:16>>}} ->        [];
	{ok,{_,<<16#7FE7:16>>}} ->  [{Name,hot}];
	{ok,{_,<<16#8400:16>>}} ->  [{Name,unstable}];
	{ok,{_,<<16#8300:16>>}} ->  [{Name,below_minimum}];
	{ok,{_,<<16#8200:16>>}} ->  [{Name,failed_to_initialize}];
	{ok,{_,<<16#8100:16>>}} ->  [{Name,skipped}];
	{ok,{_,<<16#8000:16>>}} ->  [{Name,read_errror}];
	{ok,{_,<<Val:16/signed>>}} -> [{Name,Val/256.0}];
	_E = {error,_} -> []
    end.

opt_read_key(Key, Name) ->
    case read_key(Key) of
	{ok,{_,_,Value}} -> [{Name,Value}];
	_ -> []
    end.

decode(Type, Value) ->
    decode(Type, <<0,0,0,0>>, Value).

decode(<<"ui8 ">>, _Key, <<Val:8/unsigned>>) -> Val;
decode(<<"ui8",0>>, _Key, Val) -> Val;  %% bug?
decode(<<"ui16">>, _Key, <<Val:16/unsigned>>) -> Val;
decode(<<"ui32">>, _Key, <<Val:32/unsigned>>) -> Val;
decode(<<"si8 ">>, _Key, <<Val:8/signed>>) -> Val;
decode(<<"si16">>, _Key, <<Val:16/signed>>) -> Val;
decode(<<"{pwm">>, _Key, <<Val:16/unsigned>>) -> Val;
decode(<<"flag">>, _Key, <<Val:8/unsigned>>) -> Val;
decode(<<"char">>, _Key, <<Val:8/signed>>) -> Val;
decode(<<"fp1f">>, _Key, <<Val:16>>) -> Val/32768.0;
decode(<<"fp2e">>, _Key, <<Val:16>>) -> Val/16384.0;
decode(<<"fp3d">>, _Key, <<Val:16>>) -> Val/8192.0;
decode(<<"fp4c">>, _Key, <<Val:16>>) -> Val/4096.0;
decode(<<"fp5b">>, _Key, <<Val:16>>) -> Val/2048.0;
decode(<<"fp6a">>, _Key, <<Val:16>>) -> Val/1024.0;
decode(<<"fp79">>, _Key, <<Val:16>>) -> Val/512.0;
decode(<<"fp88">>, _Key, <<Val:16>>) -> Val/256.0;
decode(<<"fp97">>, _Key, <<Val:16>>) -> Val/128.0;
decode(<<"fpa6">>, _Key, <<Val:16>>) -> Val/64.0;
decode(<<"fpb5">>, _Key, <<Val:16>>) -> Val/32.0;
decode(<<"fpc4">>, _Key, <<Val:16>>) -> Val/16.0;
decode(<<"fpd3">>, _Key, <<Val:16>>) -> Val/8.0;
decode(<<"fpe2">>, _Key, <<Val:16>>) -> Val/4.0;
decode(<<"fpf1">>, _Key, <<Val:16>>) -> Val/2.0;
decode(<<"sp78">>, _Key, <<Val:16/signed>>) -> Val / 256.0;

%% ACID - AC adapter id is provided as little enidan
%% #define kACCRCBit       56   // size 8
%% #define kACIDBit        44  // size 12
%% #define kACPowerBit     36  // size 8
%% #define kACRevisionBit  32  // size 4
%% #define kACSerialBit    8   // size 24
%% #define kACFamilyBit    0   // size 8
%% <<Family:8,Serial:24,Rev:4,Power:8,ID:12,CRC:8>>) ->
decode(<<"ch8*">>, <<"ACID">>,  <<Bits:64/little>>) ->
    Family = (Bits bsr 0) band 16#ff,
    Serial  = (Bits bsr 8) band 16#ffffff,
    Revision = (Bits bsr 32) band 16#f,
    Power = (Bits bsr 36) band 16#ff,
    ID = (Bits bsr 44) band 16#fff,
    CRC = (Bits bsr 56) band 16#ff,
    #adapter_info { family = Family, serial = Serial, revision = Revision,
		    power = Power, id = ID, crc=CRC };
decode(<<"ch8*">>, _Key, Val) -> Val;

decode(<<"{lsc">>, _Key, <<ModChange:16,ModBright:16,ScaleConst:16,
			   ScaleMod:8, RampDur:8,PowerSw:8,MinTicks:8>>) ->
    ScaleMode = decode_scale_mode(ScaleMod),
    #lms_config { modv_max_change_per_tick = ModChange,
		  modv_brightness_breathe_min = ModBright,
		  scale_constant = ScaleConst,
		  scale_mode = ScaleMode,
		  ramp_duration = RampDur,
		  power_switch_overrides_sil = PowerSw,
		  min_ticks_to_target = MinTicks };

decode(<<"{lsd">>, _Key, <<MS:16,ME:16,ST:16,ET:16>>) ->
    #lms_dwell { mid_to_start_ratio = MS,
		 mid_to_end_ratio = ME,
		 start_ticks = ST,
		 end_ticks = ET };
decode(<<"{lsf">>, _Key, <<Ceil:16,Change:16,Adjust:16>>) ->
    #lms_flare { modv_flare_ceiling = Ceil,
		 modv_min_change = Change,
		 flare_adjust = Adjust };

decode(<<"{fds">>, _Key, <<Typ,Zone,Loc,_Pad:8,Func/binary>>) ->
    LocTab = 
	{'LEFT_LOWER_FRONT','CENTER_LOWER_FRONT','RIGHT_LOWER_FRONT',
	 'LEFT_MID_FRONT'  ,'CENTER_MID_FRONT'  , 'RIGHT_MID_FRONT'     ,
	 'LEFT_UPPER_FRONT' , 'CENTER_UPPER_FRONT' , 'RIGHT_UPPER_FRONT' ,
	 'LEFT_LOWER_REAR'  , 'CENTER_LOWER_REAR'  , 'RIGHT_LOWER_REAR'  ,
	 'LEFT_MID_REAR'    , 'CENTER_MID_REAR'    , 'RIGHT_MID_REAR'    ,
	 'LEFT_UPPER_REAR'  , 'CENTER_UPPER_REAR'  , 'RIGHT_UPPER_REAR' },
    Location = try element(Loc+1, LocTab) of
		   L -> L
	       catch
		   _ -> Loc
	       end,
    FanTypeTab = { 'FAN_PWM_TACH' , 'FAN_RPM' , 'PUMP_PWM' , 'PUMP_RPM' , 
		   'FAN_PWM_NOTACH' , 'EMPTY_PLACEHOLDER' },
    Type = try element(Typ+1, FanTypeTab) of
	       T -> T
	   catch
	       _ -> Typ
	   end,
    #fan_type_desc_struct {
       type = Type,
       zone = Zone,
       location = Location,
       func = decode_string(Func) };

decode(<<"{ala">>, _Key, <<M:16,B:16/signed,R:16>>) ->
    #als_lux_line { m=M, b=B, r=R };


decode(<<"{alc">>, _Key, <<I2CTime:16,AdcTime:16,LMax:16,LMin:16,
			 ELow:16,EHigh:16,Reflect:16,Sensors:8,LidDelay:8>>) ->
    #als_config { i2c_time = I2CTime, adc_time = AdcTime,
		  lmax = LMax, lmin = LMin,  elow = ELow, ehigh = EHigh,
		  reflect = Reflect, sensors = Sensors, lid_delay = LidDelay };

decode(<<"{ali">>, _Key, <<Typ:8, ValidWhenLidClosed:8, ControlSil:8,_:8>>) ->
    Type = try element(Typ+1, { 'NoSensor', 'BS520', 'TSL2561CS', 
				'LX1973A', 'ISL29003' }) of
	       T -> T
	   catch
	       _ -> Typ
	   end,
    #als_sensor { type = Type, valid_when_lid_closed = ValidWhenLidClosed,
		  control_sil = ControlSil };
decode(<<"{alr">>, _Key, <<Base:16/signed, Coefv:16, Inflv:16,
			   Low:16/signed, High:16/signed>>) ->
    #als_therm { base = Base, coefv = Coefv, inflv = Inflv,
		 low = Low, high = High };
decode(<<"{alt">>, _Key, <<Low:16, High:16>>) ->
    #als_lux_thresh { low = Low, high = High };

decode(<<"{alv">>, _Key, <<Valid:8,HighGain:8,Chan0:16,Chan1:16,RoomLux:32>>) ->
    #als_value { valid = Valid, high_gain = HighGain,
		 chan0 = Chan0, chan1 = Chan1,
		 room_lux = RoomLux / 16384.0 };

decode(<<"{lim">>, _Key, <<Cpu,Gpu,Mem>>) ->
    #plimits { cpu = Cpu,
	       gpu = Gpu,
	       mem = Mem };
decode(<<"{lsm">>, _Key, <<ScaleMod>>) ->
    decode_scale_mode(ScaleMod);

decode(<<"{lso">>, _Key, <<Select,Ramp>>) ->
    #lms_override_behavior {
       target_behavior = decode_lms_select(Select),
       ramp = decode_flag(Ramp)
      };
decode(<<"{lss">>, _Key, <<Sel>>) ->
    decode_lms_select(Sel);

decode(_, _Key, Val) -> {raw,Val}.


decode_flag(0) -> false;
decode_flag(_) -> true.

decode_string(Bin) when is_binary(Bin) ->
    trim_string_(binary_to_list(Bin)).

decode_scale_mode(ScaleMod) ->
    try element(ScaleMod+1, { scale_als, scale_tod, scale_const }) of
	S -> S
    catch
	_ -> ScaleMod
    end.

decode_lms_select(Sel) ->
    try element(Sel+1, { off, on, breathe, bright_no_scale }) of
	S -> S
    catch
	_ -> Sel
    end.

trim_string_(Cs) ->
    trim_(reverse(trim_(reverse(Cs)))).

trim_([$\s|Cs]) -> trim_(Cs);
trim_([0|Cs]) -> trim_(Cs);
trim_(Cs) -> Cs.

%% @doc
%% Read/Write command.
%% @end
-spec io_call(Input::binary(),
	     OuputLen::integer()) ->
		    {ok, binary()} | {error, posix()}.

io_call(Input, OutputLen) ->
    call(?SMC_PORT, ?CMD_IOCALL,<<OutputLen:16, Input/binary>>).


debug(Level) when is_integer(Level) ->
    call(?SMC_PORT, ?CMD_DEBUG,<<Level:32>>).

%% call directly to a registered - bypass gen_server 
call(Port, Cmd, Data) ->
    case erlang:port_control(Port, Cmd, Data) of
	<<0>> ->
	    ok;
	<<255,E/binary>> -> 
	    {error, erlang:binary_to_atom(E, latin1)};
	<<1,Y>> -> {ok,Y};
	<<2,Y:16/native-unsigned>> -> {ok, Y};
	<<4,Y:32/native-unsigned>> -> {ok, Y};
	<<8,Y:64/native-unsigned>> -> {ok, Y};
	<<3,Return/binary>> -> {ok,Return}
    end.

%%--------------------------------------------------------------------
%% @doc
%% smc server just used to keep the port going.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> ok.

start_link() ->
    gen_server:start_link({local, ?SMC_SRV}, ?MODULE, [], []).

%% @private
start() ->
    application:start(smc).

stop() ->
    application:stop(smc).

-record(state, { port} ).

%% @private
init([]) ->
    Driver = "smc_drv", 
    ok = erl_ddll:load_driver(code:priv_dir(smc), Driver),
    Port = erlang:open_port({spawn_driver, Driver},[binary]),
    true = erlang:register(?SMC_PORT, Port),
    call(Port, ?CMD_OPEN, <<>>),
    {ok, #state{ port=Port }}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error,bad_call}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    call(State#state.port, ?CMD_CLOSE, <<>>),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
