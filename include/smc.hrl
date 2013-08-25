%%% @author tony <tony@rogvall.se>
%%% @copyright (C) 2013, tony
%%% @doc
%%%      SMC records
%%% @end
%%% Created :  5 Apr 2013 by tony <tony@rogvall.se>
%%%

-ifndef(__SMC_HRL__).
-define(__SMC_HRL__, true).

-type uint32() :: 0..4294967295.
-type uint16() :: 0..65535.
-type int16()  :: -32768 .. 32767.
-type uint8() :: 0..255.
-type posix() :: atom().
-type flag_t()  :: uint8().
-type pwm_t() :: uint16().

-type scale_mode() :: scale_als |   %% Use ALS autoscale
		      scale_tod |   %% Use TOD autoscale
		      scale_const.  %% Scale only by a constant

-type lms_select() :: off | on | breathe | bright_no_scale.

%% SIL = system indicator light


%% "{ala"
-record(als_lux_line, {
	m :: uint16(), %% Slope of line.
	b :: int16(),  %% Y-Intercept of line.
	r :: uint16()  %% Region
	}).

%% "{alc"
-record(als_config, { 
	  i2c_time :: uint16(),  %% Int interval (ms) for ALS I2C task.
	  adc_time :: uint16(),  %% Int interval (ms) for ALS ADC ISR.
	  lmax :: uint16(),      %% Maximum cd/m^2 for SIL.
	  lmin :: uint16(),      %% Minimum cd/m^2 for SIL.
	  elow :: uint16(),      %% Low room illum threshold (lux).
	  ehigh :: uint16(),     %% High room illum threshold (lux).
	  reflect :: uint16(),   %% Bezel reflection coefficient.
	  sensors :: uint8(),    %% Actual number of ALS sensors in system.
	  lid_delay :: uint8()   %% Delay after lid opens (in tenths of seconds)
	                         %% during which ALS readings don't affect the 
	                         %%   SIL.
	 }).

-type alstype() :: 'NoSensor' | 'BS520' | 'TSL2561CS' | 'LX1973A' | 'ISL29003'.

%% "{ali"
-record(als_sensor, {
	  type :: alstype(),   %% Type of sensor.
	  valid_when_lid_closed :: flag_t(), 
	  %% TRUE if no lid or if sensor works with
	  %% closed lid.  FALSE otherwise.
	  control_sil :: flag_t()
	  %% TRUE if the SIL brightness depends on
	  %% this sensor's value.  FALSE otherwise.
	 }).

%% "{alr" - ALS analog lux temperature coefficients.
-record(als_therm, {
	  base :: int16(),   %% Temperature baseline (deg C, FP16.0)
	  coefv :: uint16(), %% Temperature coeff (ADC Counts/deg C, FP12.4)
	  inflv :: uint16(), %% Thermal compensation inflection point voltage
	                        %%   (ADCCounts, FP16.0)
	  low :: int16(),    %% Low temperature boundary (deg C, FP16.0)
	  high :: int16()    %% High temperature boundary (deg C, FP16.0)
	}).

%% "{alt" ALS analog lux calculation thresholds.
-record(als_lux_thresh, {
	  low :: uint16(),  %% ADC threshold while in low gain.
	  high :: uint16() %% ADC threshold while in high gain.
	 }).

%% "{alv" ALSValue structure contains latest ambient light info from 1 sensor
-record(als_value, {
	  valid :: flag_t(),         %% If TRUE, data in this struct is valid.
	  high_gain :: flag_t(),     %% If TRUE, ui16Chan0/1 are high-gain
	                           %% readings.  If FALSE, ui16Chan0/1 are
                                   %% low-gain readings.
	  chan0 :: uint16(),       %% I2C channel 0 data or analog(ADC) data.
	  chan1 :: uint16(),       %% I2C channel 1 data.
	  %% The following field only exists on systems that send ALS change
	  %% notifications to the OS:
	  room_lux :: uint32()     %% Room illumination in lux, FP18.14.
	 }).

-type fan_location() :: 
	'LEFT_LOWER_FRONT' | 'CENTER_LOWER_FRONT' | 'RIGHT_LOWER_FRONT' |
	'LEFT_MID_FRONT'   | 'CENTER_MID_FRONT'   | 'RIGHT_MID_FRONT'   |
	'LEFT_UPPER_FRONT' | 'CENTER_UPPER_FRONT' | 'RIGHT_UPPER_FRONT' |
	'LEFT_LOWER_REAR'  | 'CENTER_LOWER_REAR'  | 'RIGHT_LOWER_REAR'  |
	'LEFT_MID_REAR'    | 'CENTER_MID_REAR'    | 'RIGHT_MID_REAR'    |
	'LEFT_UPPER_REAR'  | 'CENTER_UPPER_REAR'  | 'RIGHT_UPPER_REAR'.

-type fan_type() :: 'FAN_PWM_TACH' | 'FAN_RPM' | 'PUMP_PWM' | 'PUMP_RPM' | 
		    'FAN_PWM_NOTACH' | 'EMPTY_PLACEHOLDER'.


%% "{fds" - Fan Diag description
-record(fan_type_desc_struct, {
	  type :: fan_type(),
	  zone :: uint8(),
	  location :: fan_location(),
	  %% pad :: uint8()        %% padding to get us to 16 bytes
	  func :: list()    %% [DIAG_FUNCTION_STR_LEN]
	 }).

%% "{lim"
-record(plimits, {
	cpu :: uint8(),
	gpu :: uint8(),
	mem :: uint8()
	}).
	
%% "{lsc"
-record(lms_config, {
	modv_max_change_per_tick :: pwm_t(), %% Max PWM change per 1/152 sec
	modv_brightness_breathe_min :: pwm_t(),     %% Breathe dwell PWM setting
        scale_constant :: uint16(),         %% Scale constant (1.15 fixed-point
                                          %% representation) if not using
                                          %%   ALS or TOD scaling
  scale_mode :: scale_mode(),      %% Scale by ALS, TOD, or constant
  ramp_duration :: uint8(),           %% Ramp length (equals 152 *
                                          %%   ramp time in seconds)
  power_switch_overrides_sil :: flag_t(),    %% TRUE if pressing the power
                                          %%   switch should force the
                                          %%   SIL to full brightness
  min_ticks_to_target :: uint8()        %% Slow the slew rate so that
                                         %%   it takes at least this many
                                         %%   ticks to reach the target
                                         %%   from the prev PWM value.
}).

%% "{lsd"
-record(lms_dwell, { 
	  mid_to_start_ratio :: uint16(),
	  mid_to_end_ratio   :: uint16(),
	  start_ticks        :: uint16(),
	  end_ticks          :: uint16()
	}).

%% "{lsf
-record(lms_flare, {
	%% Flare algorithm is active below this value.
	modv_flare_ceiling :: pwm_t(),  
        modv_min_change :: pwm_t(),  %% Minimum rate of change while flaring.
	flare_adjust :: uint16()    %% Smaller value causes stronger flare as
                                   %% PWM value descends below modvFlareCeiling.
	}).

%% "{lsm"  scale_mode() !

%% "{lso"
-record(lms_override_behavior, {
	target_behavior :: lms_select(),
	ramp :: flag_t()
	}).
			   

%% special coding of ch8* ACID
-record(adapter_info, {
	family,
	serial,     %% serial number
	revision,
	power,      %% watts
	id,
	crc
	}).

-endif.
