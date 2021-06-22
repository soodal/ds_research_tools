pro mydcbar, labels, colors, unit , vert=vert, step=step, nolast=nolast, nofirst=nofirst,last=last, charsize=charsize, coord=coord

nlev = n_elements(labels)
ncol = n_elements(colors)
IF nlev ne ncol then begin
   print, 'nlev =/ ncol'
   stop
endif

IF not keyword_set(charsize) then charsize = 1.1
IF not keyword_set(coord) THEN BEGIN
   IF not keyword_set(vert) THEN BEGIN
        x0 = !p.position(0) & y0 = !p.position(1) - 0.05
        xs = !p.position(2) - !p.position(0)
	ys = 0.02
   ENDIF ELSE BEGIN
        x0 = !p.position(2)+0.01  & y0 = !p.position(1)
        xs = 0.015
	ys = !p.position(3) - !p.position(1)
        if unit ne '' then ys = ys - 0.02
   ENDELSE
   coord = [x0,y0, x0+xs, y0+ys]
ENDIF
IF not keyword_set (step) then step = 1
first = 0 & if keyword_Set (nofirst) then first = 1
levelind = indgen(ncol)

if step gt 1 then begin
  levelind = where ( (levelind + first) mod step eq 0 , nda)
endif else begin
  levelind = where( levelind ge first , nda)
endelse
  if keyword_Set (nolast) then begin
     if max(levelind) eq nlev -1 then levelind = levelind(0:nda-2)
  endif
  if keyword_Set (last) then begin
     if max(levelind) ne nlev -1 then levelind = [levelind, nlev-1]
  endif

labels0 = labels
labels(*)  = ''
labels(levelind) = labels0(levelind)
IF not keyword_set(vert) THEN BEGIN
    DCBar_geun, colors, LABELS=labels, charsize=charsize, position = coord, color='black'
   
ENDIF ELSE BEGIN
    DCBar_geun, colors, LABELS=labels, charsize=charsize, position = coord,/VERTICAL, color='black'
    xyouts,  coord(0), coord(3)+0.01, unit, /normal, charsize=1
ENDELSE

return


end 
