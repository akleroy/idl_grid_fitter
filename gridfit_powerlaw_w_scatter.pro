pro gridfit_powerlaw_w_scatter, x, p

;
; A power law with log scatter split between the two dimensions. Note that
; we modify x here!
;

  n = n_elements(x)
  scatter_1d = p[2]/sqrt(2.)
  
  y = p[0]*x^(p[1])
  x *= 10.^(randomn(seed, n)*scatter_1d)
  y *= 10.^(randomn(seed, n)*scatter_1d)

  return, y

end
