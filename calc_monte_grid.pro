pro calc_monte_for_func $
   , real_x = real_x_in $
   , func = func_name $
   , params = params $
   , uc_x_linear = uc_x_linear $
   , uc_y_linear = uc_y_linear $
   , uc_x_log = uc_x_log $
   , uc_y_log = uc_y_log $
   , bias_x_linear = bias_x_linear $
   , bias_y_linear = bias_y_linear $
   , bias_x_log = bias_x_log $
   , bias_y_log = bias_y_log $
   , bootstrap = bootstrap $
   , n_boot = n_boot $
   , obs_x = obs_x $
   , obs_y = obs_y   

;+
; NAME:
;
; calc_monte_for_func
;
; PURPOSE:
;
; Generate a Monte Carlo data set given a functional form, parameters,
; a set of abcissa values, and specified uncertainties.
;
; CATEGORY:
;
; Science tool.
;
; CALLING SEQUENCE:
;
;pro calc_monte_for_func $
;    , real_x = real_x_in $
;    , func = func_name $
;    , params = params $
;    , uc_x_linear = uc_x_linear $
;    , uc_y_linear = uc_y_linear $
;    , uc_x_log = uc_x_log $
;    , uc_y_log = uc_y_log $
;    , bias_x_linear = bias_x_linear $
;    , bias_y_linear = bias_y_linear $
;    , bias_x_log = bias_x_log $
;    , bias_y_log = bias_y_log $
;    , bootstrap = bootstrap $
;    , n_boot = n_boot $
;    , obs_x = obs_x $
;    , obs_y = obs_y   
;
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; logarithmic and/or linear point-by-point and/or overall uncertainty
;
; bootstrap : turn on/off bootstrapping (ON by default)
; 
; n_boot : the number of data to draw from the real parent distribution
; to make the Monte Carlo
;
; KEYWORD PARAMETERS:
;
; None
;
; OUTPUTS:
;
; obs_x : the Monte Carlo X
; obs_y : the Monte Carlo Y
;
; OPTIONAL OUTPUTS:
;
; None
;
; COMMON BLOCKS:
;
; None
;
; SIDE EFFECTS:
;
; Inordinate confidence in a poor description of the data.
;
; RESTRICTIONS:
;
; None
;
; PROCEDURE:
;
; Sample, random numbers, rinse, repeat
;
; EXAMPLE:
;
; none
;
; MODIFICATION HISTORY:
;
; Documented - Oct 11 aleroy@nrao
; Generalized - Spring 14 aleroy@nrao
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS AND DEFINITIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; FUNCTION NAME (DEFAULT TO POWERLAW)
  if n_elements(func_name) eq 0 then $
     func_name = "gridfit_powerlaw"

; BOOTSTRAPPING BEHAVIOR
  if keywoord_set(bootstrap) then $
     bootstrap = 1B
  
  if keyword_set(bootstrap) and n_nelements(n_boot) eq 0 then $
     n_boot = n_elements(real_x)

; DEFAULT UNCERTAINTIES
  if n_elements(uc_x_linear) eq 0 then $
     uc_x_linear = 0.0

  if n_elements(uc_y_linear) eq 0 then $
     uc_x_linear = 0.0

  if n_elements(uc_x_log) eq 0 then $
     uc_gas_log = 0.0

  if n_elements(uc_y_log) eq 0 then $
     uc_gas_log = 0.0

  if n_elements(bias_x_linear) eq 0 then $
     bias_x_linear = 0.0

  if n_elements(bias_y_linear) eq 0 then $
     bias_y_linear = 0.0

  if n_elements(bias_x_log) eq 0 then $
     bias_x_log = 0.0

  if n_elements(bias_y_log) eq 0 then $
     bias_y_log = 0.0

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; GENERATE TRUE VALUES
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  use_ind = where(finite(real_x_in), n_pts)
  if n_pts eq 0 then begin
     message, "No finite input values.", /info
     return
  endif
  real_x = real_x_in[use_ind]

  if keyword_set(bootstrap) then begin
     boot_ind = floor((randomu(seed, n_boot))*n_pts)
     real_x = real_x[boot_ind]
  endif
  
  real_y = call_function(func_name, real_x, params)
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; ADD NOISE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; THE SIMULATED X DISTRIBUTION
  obs_x = real_x
; ... LOG UNCERTAINTY
  if uc_x_log ne 0.0 then $
     obs_x *= 10.^(randomn(seed, n_pts)*uc_x_log)
; ... LINEAR UNCERTAINTY
  if uc_x_linear ne 0.0 then $
     obs_x += randomn(seed, n_pts)*uc_x_linear
; ... A LINEAR OFFSET
  if bias_x_linear ne 0.0 then $
     obs_x += randomn(seed)*bias_x_linear
; ... A LOG OFFSET
  if bias_x_log ne 0.0 then $
     obs_x *= 10^(randomn(seed)*uc_x_log)

; THE SIMULATED X DISTRIBUTION
  obs_y = real_y
; ... LOG UNCERTAINTY
  if uv_y_log ne 0.0 then $
     obs_y *= 10.^(randomn(seed, n_pts)*uc_y_log)
; ... LINEAR UNCERTAINTY
  if uv_y_linear ne 0.0 then $
     obs_y += randomn(seed, n_pts)*uc_y_linear
; ... A LINEAR OFFSET
  if bias_y_linear ne 0.0 then $
  obs_y += randomn(seed)*bias_y_linear
; ... A LOG OFFSET
  if bias_y_log ne 0.0 then $
     obs_y *= 10^(randomn(seed)*bias_y_log)

end
