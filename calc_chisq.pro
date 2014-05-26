function calc_chisq $
   , model_pdf = model_pdf_in $
   , model_ul = model_ul_in $
   , obs_pdf = obs_pdf_in $
   , obs_ul = obs_ul_in $
   , normalize = normalize $
   , min_ct = min_ct

; COPY THE INPUT MODEL VALUES
  model_pdf = model_pdf_in
  model_ul = model_ul_in

  obs_pdf = obs_pdf_in
  obs_ul = obs_ul_in

; DEFAULT TO ALLOWING NO LESS THAN ONE EXPECTED DATA POINT PER CELL
; WHEN CALCULATING CHI-SQUARED.
  if n_elements(min_ct) eq 0 then $
     min_ct = 1.0

; IF NORMALIZATION IS REQUEST, SET THE COUNTS IN THE MODEL TO MATCH
; THE NUMBER OF OBSERVATIONS
  if keyword_set(normalize) then begin
     total_obs = total(obs_pdf) + total(obs_ul)
     total_model = total(model_pdf) + total(model_ul)

     model_pdf = model_pdf*total_obs/total_model
     model_ul = model_ul*total_obs/total_model
  endif

; CALCULATE CHI-SQUARED, ASSUMING POISSON-LIKE ERRORS

  chisq = total((model_pdf - obs_pdf)^2 / (model_pdf > min_ct), /nan)
  chisq += total((model_ul - obs_ul)^2 / (model_ul > min_ct), /nan)

; RETURN
  return, chisq
  
end
   
