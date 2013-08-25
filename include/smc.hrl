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
-type flag()  :: uint8().

%% SIL = system indicator light
-record(lms_dwell, { 
	  mid_to_start_ratio :: uint16(),
	  mid_to_end_ratio   :: uint16(),
	  start_ticks        :: uint16(),
	  end_ticks          :: uint16()
	}).

%% "{ala"
-record(als_lux_line, {
	  m :: uint16(), %% Slope of line.
	  b :: int16(),  %% Y-Intercept of line.
	  r :: uint16()  %% Region
	 }).

%% "{alc"
-record(als_config, { 
	  i2ctime :: uint16(),   %% Int interval (ms) for ALS I2C task.
	  adctime :: uint16(),   %% Int interval (ms) for ALS ADC ISR.
	  lmax :: uint16(),      %% Maximum cd/m^2 for SIL.
	  lmin :: uint16(),      %% Minimum cd/m^2 for SIL.
	  elow :: uint16(),      %% Low room illum threshold (lux).
	  ehigh :: uint16(),     %% High room illum threshold (lux).
	  reflect :: uint16(),   %% Bezel reflection coefficient.
	  alssensors :: uint8(), %% Actual number of ALS sensors in system.
	  liddelay :: uint8()    %% Delay after lid opens (in tenths of seconds)
	                         %% during which ALS readings don't affect the 
	                         %%   SIL.
	 }).

-type alstype() :: 'NoSensor' | 'BS520' | 'TSL2561CS' | 'LX1973A' | 'ISL29003'.

%% "{ali"
-record(als_sensor, {
	  type :: alstype(),   %% Type of sensor.
	  valid_when_lid_closed :: flag(), 
	  %% TRUE if no lid or if sensor works with
	  %% closed lid.  FALSE otherwise.
	  control_sil :: flag()
	  %% TRUE if the SIL brightness depends on
	  %% this sensor's value.  FALSE otherwise.
	 }).

%% "{alr" - ALS analog lux temperature coefficients.
-record(als_therm, {
	 tempbase :: int16(),   %% Temperature baseline (deg C, FP16.0)
	 tempcoefv :: uint16(), %% Temperature coeff (ADC Counts/deg C, FP12.4)
	 tempinflv :: uint16(), %% Thermal compensation inflection point voltage
	                        %%   (ADCCounts, FP16.0)
	 templow :: int16(),    %% Low temperature boundary (deg C, FP16.0)
	 temphigh :: int16()    %% High temperature boundary (deg C, FP16.0)
	}).

%% "{alt" ALS analog lux calculation thresholds.
-record(als_luxthrsh, {
	  thrsh_low :: uint16(),  %% ADC threshold while in low gain.
	  thrsh_high :: uint16() %% ADC threshold while in high gain.
	 }).

%% "{alv" ALSValue structure contains latest ambient light info from 1 sensor
-record(als_value, {
	  valid :: flag(),         %% If TRUE, data in this struct is valid.
	  high_gain :: flag(),     %% If TRUE, ui16Chan0/1 are high-gain
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


-endif.
