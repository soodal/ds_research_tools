function get_omi_descend , orb, sza, lats ,what=what, pix=pix

if keyword_set(what) then begin 
  print, 'function get_omi_descend , orb, sza, lats ,what=what'
  return,-1
endif

lat = lats
if size(lat,/n_di) eq 2 then lat = lats(*,4)

da         = where( orb ne 0)
nprof      = n_elements(orb)
descend    = intarr(nprof) 
descend(*) = 0

minorb = min(orb(da))
maxorb = max(orb(da))
norb   = maxorb - minorb + 1

if nprof eq 1 then begin
   descend(*) = 0
endif else if nprof gt 1 then begin
  ; for iorb = 0 , norb -1 do begin
  ;      da = where (orb eq minorb + iorb, ct)
  ;      if ct eq 0 then continue
  ;
  ;      print , minorb + iorb, ct, min(sza(da)), min(lat(da))
  ;      if ct le 200 or min(sza(da)) ge 70 or min(lat(da)) gt 60. then descend(da) =1
  ; endfor

 for iorb = 0 , norb-1 do begin    
  for ipix = min(pix) , max(pix) do begin
    da = where( orb eq minorb + iorb and pix eq ipix, ct)
     if ct eq 0 then continue
     ALAT = lat (da)
     diff = [alat, alat(ct-1)]
     diff = diff(1:ct) - diff(0:ct-1)
     ada   = da(where( diff le 0, nda))
     if nda ne 0 then descend(ada) = 1
  endfor
 endfor
endif 



return, descend

END function
