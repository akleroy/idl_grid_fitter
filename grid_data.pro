function grid_data $
   , xdata $
   , ydata $
   , wt=wt $
   , xmin=xmin $
   , xmax=xmax $
   , ymin=ymin $
   , ymax=ymax $
   , nx=nx $
   , ny=ny $ 
   , binsize_x=binsize_x $
   , binsize_y=binsize_y $
   , regular=regular $
   , reverse_indices = ri $
   , twod_ind = twod_ind $
   , xaxis_out = xaxis_out $
   , yaxis_out = yaxis_out $
   , quiet=quiet $
   , nan=nan
  

;+
; NAME:
;
; grid_data
;
; PURPOSE:
;
; Take data (X,Y) and grid it into an image of point density in parameter
; space. Improvements over 'hist_2d' are that the grids do not have to be
; regular and 'grid_data' accepts a weight for each data point. This comes at
; the cost of speed.
;
; N.B. Data equal to the lower edge of a bin is taken to be in that bin.
;
; CATEGORY:
;
; Data analysis tool.
;
; CALLING SEQUENCE (IRREGULAR CASE):
;
; data_grid = grid_data(xdata, ydata, wt=wt $
;                       , xaxis_out = xaxis_out, yaxis_out = yaxis_out $
;                       , xmin=xmin, xmax=xmax $
;                       , ymin=ymin, ymax=ymax)
;
; INPUTS:
;
; xdata - the x value for each data point
; ydata - the y value for each data point
;
; OPTIONAL INPUTS:
;
; wt    - optional weighting parameter (defaults to 1)
; xmin  - the minimum x value for each bin
; xmax  - the maximum x value for each bin
; ymin  - the minimum y value for each bin
; ymax  - the maximum y value for each bin
;
; KEYWORD PARAMETERS:
;
; regular - tell GRID_DATA that this is the regular case, which avoids a
;           couple of NBIN-long, WHERE-intensive for loops. This should be an
;           option most of the time.
;
; nan     - tells grid data to ignore any data pair that contains a value for
;           which finite = 0 (i.e., infinite or NaN data). This gobbles
;           memory, so if you are pushing the envelope don't use this.
;
; OUTPUTS:
;
; a grid containing the number of data points in each cell modified by the
; weighting scheme, if any.
;
; OPTIONAL OUTPUTS:
;
; reverse_indices - the 'reverse indices' following the histogram conventions
; twod_ind - the one dimensional index inside the grid of each data point
; xaxis_out - the value at the middle of each bin along the x axis
; yaxis_out - as xaxis_out but for the y axis
;
; MODIFICATION HISTORY:
;
; Documented - 9 Sep 2007 leroy@mpia.de
; Collapsed to 1d, should speed things up - 28 Nov 2008 leroy@mpia.de
; Bins now include their lower edges - 28 Nov 2008 leroy @mpia
; Bug squashing (A. Schruba) - 1 Dec 08 leroy@mpia
; Added xaxis_out and yaxis_out at FB's suggestion - 28 May 08 leroy@mpia
;
; TO BE DONE
;
; Error checking could still be much more rigorous.
;
;-

; ON AN ERROR EXIT GRID_DATA AND THEN STOP
  on_error, 2

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; ERROR CHECKING
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; CHECK FOR NANS...
  use = where(finite(xdata) and finite(ydata),usect)
  ndata = n_elements(xdata)
  if usect ne ndata then begin
      if keyword_set(nan) then begin
          xdata_in = xdata
          ydata_in = ydata
          if usect gt 0 then begin
              xdata = xdata[use]
              ydata = ydata[use]
          endif else begin
              message, "No finite data pairs. Returning.", /info
              return, -1
          endelse
      endif else begin
          message, 'Requires only finite data or /nan flag. Returning', /info
          return, -1
      endelse
  endif

; ASSUME THAT THE DATA ARE REGULAR IF THE USER IS SUPPLYING ANY KIND OF GRID
; INFORMATION RATHER THAN AN ENTIRE AXIS
  grid_keywords = $
     total(n_elements(xmin) + n_elements(xmax) + n_elements(nx) + $
           n_elements(ymin) + n_elements(ymax) + n_elements(ny) + $
           n_elements(binsize_x) + n_elements(binsize_y))
  
  if grid_keywords gt 0 AND (keyword_set(regular) eq 0) then begin     
     if keyword_set(quiet) eq 0 then $
        message, 'Assuming regular case.', /info
     regular=1
  endif

  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFINE THE GRID
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; REGULAR CASE
  if keyword_set(regular) then begin
      if n_elements(xmin) eq 0 then $
        minval_x = min(xdata,/nan) $
      else $
        minval_x = min(xmin)

      if n_elements(xmax) eq 0 then $
        maxval_x = max(xdata,/nan) $
      else $
        maxval_x = max(xmax)

      if n_elements(ymin) eq 0 then $
        minval_y = min(ydata,/nan) $
      else $
        minval_y = min(ymin)

      if n_elements(ymax) eq 0 then $
        maxval_y = max(ydata,/nan) $
      else $
        maxval_y = max(ymax)
      
      if ((n_elements(nx) eq 0) and $
          (n_elements(binsize_x) eq 0)) then begin
          nx = 10 > n_elements(xmin)
          binsize_x = (maxval_x - minval_x)/(1.0*nx)  
      endif else if (n_elements(nx) eq 0) then begin
          nx = ceil((maxval_x - minval_x)/binsize_x)
      endif else begin
          binsize_x = (maxval_x - minval_x)/(1.0*nx)
      endelse
      
      if ((n_elements(ny) eq 0) and $
          (n_elements(binsize_y) eq 0)) then begin
          ny = 10 > n_elements(ymin)
          binsize_y = (maxval_y - minval_y)/(1.0*ny)
      endif else if (n_elements(ny) eq 0) then begin
          ny = ceil((maxval_y - minval_y)/binsize_y)
      endif else begin
          binsize_y = (maxval_y - minval_y)/(1.0*ny)
      endelse

      nx = long(nx)
      ny = long(ny)

; MAKE OUTPUT AXES
      xaxis_out = minval_x + (findgen(nx)+0.5)*binsize_x
      yaxis_out = minval_y + (findgen(ny)+0.5)*binsize_y

  endif else begin
; IRREGULAR CASE
      nx = long(n_elements(xmin))
      ny = long(n_elements(ymin))

; MAKE OUTPUT AXES BY SIMPLE AVERAGING THE BIN EDGES
      xaxis_out = 0.5 * (xmin + xmax)
      yaxis_out = 0.5 * (ymin + ymax)
  endelse

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; THE REGULAR CASE IS EASY
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if keyword_set(regular) then begin
      xpix = long(floor((xdata - minval_x) / binsize_x))
      ypix = long(floor((ydata - minval_y) / binsize_y))
  endif else begin
      if keyword_set(quiet) ne 0 then begin
          message,'Allowing irregular gridding. Are you sure this is what you want?', /info
      endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; THE IRREGULAR CASE IS HARDER : I HAVEN'T COME UP WITH A SLICK WAY TO AVOID A
; LOOP HERE... IF SOMEONE READING THIS THINKS OF ONE, EMAIL ME!
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

      ypix = lonarr(n_elements(ydata)) - 1
      for i = 0L, ny-1 do begin        
          ind = where((ydata ge ymin[i]) and (ydata lt ymax[i]),ct)
          if ct gt 0 then ypix[ind] = i
      endfor
      
      xpix = lonarr(n_elements(xdata)) - 1
      for i = 0L, nx-1 do begin
          ind = where((xdata ge xmin[i]) and (xdata lt xmax[i]),ct)
          if ct gt 0 then xpix[ind] = i
      endfor

  endelse

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; NOW THAT WE HAVE MAPPED EACH PIXEL TO THE GRID, USE 'HISTOGRAM'
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  twod_ind = xpix + nx*ypix
  twod_ind = $
    twod_ind*(xpix ge 0)*(ypix ge 0)  - 1*((xpix le -1) or (ypix le -1))
  twod_ind = $
    twod_ind*(xpix lt nx)*(ypix lt ny)  - 1*((xpix ge nx) or (ypix ge ny))
  ; x > xmax catch?
  hist = histogram(twod_ind,binsize=1, min=0, max=nx*ny-1, reverse_indices=ri)

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; IF THERE IS NO WEIGHTING, WE'RE DONE; ELSE USE REVERSE INDICES TO WEIGHT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(wt) eq 0 then begin
     grid = reform(hist, nx, ny)
  endif else begin      
     grid = fltarr(nx, ny)
     for i = 0L, n_elements(grid)-1 do $
        grid[i] = (ri[i] ne ri[i+1]) ? total(wt[ri[ri[i]:ri[i+1]-1]]) : 0
  endelse
  
  if keyword_set(nan) and  (usect ne ndata) then begin
     xdata = xdata_in
     ydata = ydata_in
  endif

  return, grid

end                             ; OF GRID_DATA
