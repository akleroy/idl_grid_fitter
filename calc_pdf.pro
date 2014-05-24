function calc_pdf $
   , x = x_in $
   , y = y_in $
   , min_x = min_x $
   , max_x = max_x $
   , min_y = min_y $
   , max_y = max_y $
   , binsize_x = binsize_x $
   , binsize_y = binsize_y $
   , normalize = normalize $
   , limit_y = limit_y $
   , ul = ul $
   , axis_x=axis_x $
   , axis_y=axis_y
   

; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$
; PARSE INPUTS, SET DEFAULTS
; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$

; COPY THE INPUT MODEL VALUES
  x = x_in
  y = y_in

; IF THE RANGE ISN'T SPECIFIED THEN DERIVE IT FROM THE DATA
  if n_elements(min_x) eq 0 then $
     min_x = min(x, /nan)

  if n_elements(max_x) eq 0 then $
     max_x = max(x, /nan)
  
  if n_elements(min_y) eq 0 then $
     min_y = min(y, /nan)

  if n_elements(max_y) eq 0 then $
     max_y = max(y, /nan)

; DEFAULT TO GRIDDING CELLS 0.25 DEX IN SIZE
  if n_elements(binsize_x) eq 0 then $
     binsize_x = 0.25

  if n_elements(binsize_y) eq 0 then $
     binsize_y = 0.25

; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$
; GRID AND CALCULATE
; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$

  if n_elements(limit_y) gt 0 then begin
     lim = where(y lt limit_y, lim_ct)     
     if lim_ct gt 0 then begin
        y_lim = y[lim]
        x_lim = x[lim]
        keep = where(y ge limit_y, keep_ct)
        if keep_ct gt 0 then begin
           x = x[keep]
           y = y[keep]
        endif else begin
;          A DUMMY DATA SET THAT YIELDS AN EMPTY GRID
           x = [min_x-1.0]
           y = [min_y-1.0]
        endelse
     endif
  endif

; GRID THE MODEL DATA
  pdf = grid_data(x, y $
                  , xmin=min_x, xmax=max_x, binsize_x=binsize_x $
                  , ymin=min_y, ymax=max_y, binsize_y=binsize_y $
                  , /regular, /nan $
                  , xaxis_out = axis_x $
                  , yaxis_out = axis_y $
                 )

; GRID THE UPPER LIMIT DATA
  if n_elements(limit_y) gt 0 then begin
     if n_elements(x_lim) gt 0 then begin
        ul = histogram(x_lim, min=min_x, max=max_x, binsize=binsize_x)
     endif else begin
;       ... EMPTY BY CONSTRUCTION
        ul = histogram([min_x-1], min=min_x, max=max_x, binsize=binsize_x)
     endelse
  endif
  
; NORMALIZE IF REQUESTED
  if keyword_set(normalize) then begin
     denom = (total(pdf)+total(ul))*1.0
     pdf = pdf/denom
     ul = ul/denom
  endif

  return, pdf

end
   
