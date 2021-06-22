


; This subroutine reads a retrieval from a file assuming the file is positioned at the
; beginning of a pixel
pro read_omil2_pix, lun, nlayer, ntp, nf, nw, nalb, ngas, naer, rms, avgres, dfs, info, intalb, $
    retalb, cfrac, ctp,  cldflg, ai, taod, tsca, saod, atmos, ozprof, toz, stratoz, tropoz, fitvar, avgk, $
    waves, fitspec, simrad, clmrad, actrad, tracegas, correl, covar, contri, wf, snr, line, pexval, ring, $
    traceprof, traceavgk, tracecontri, varname, numwin, aline, apix

pexval = 1

; error handling
on_ioerror, bad   ; error handling


; initialize some variables
line = ' ' &  tmpchar = '  '
nalb =  0  &  ntp   = 0  &  nflay  = 0  & endlay = 0 & stlay = 0 
nf = 0     &  ngas  = 0  &  nrf    = 0  & nlayer = 0 & nw = 0 
cldflg = 0 

; read rms, avgres, dfs, info
readf, lun, line
;readf, line, rms, avgres, dfs, info
readf, lun, line
temparr= str_sep(strcompress(strtrim(line, 2)), ' ')
ntemp  = n_elements(temparr)
nwin   = (ntemp-4)/2

if numwin eq 0 then numwin= nwin
rms=fltarr(numwin+1) & avgres=fltarr(numwin+1)
dfs = 0.0 & info = 0.0
rms(*) = 1000. & avgres(*) = 1000.

if ntemp eq 4 then begin
    reads, line, rms, avgres, dfs, info
    stop
endif else if ntemp eq 8 or ntemp eq  6 then begin 
  if nwin eq numwin then begin
    temparr=float(temparr)
    rms(0) = temparr(0) & avgres(0) = temparr(1)
    dfs=temparr(2) & info = temparr(3)
    rms(1:nwin) = temparr(4:4+nwin-1)
    avgres(1:nwin) = temparr(4+nwin:ntemp-1)   
  endif
endif

; read albedo, clouds, and aerosols
readf, lun, line
reads, line, tmpchar, nalb, format='(A8, I3)'
intalb = fltarr(nalb) & retalb = intalb
reads, line, tmpchar, nalb, intalb, retalb, format='(A8,I3,12E10.2)'
readf, lun, tmpchar, cfrac, cod, ctp, cldflg, format='(A8,3F10.3, I3)'
readf, lun, line 
reads, line, tmpchar, naer, format='(A8, I3)' & naer = fix(naer)
aerwavs = fltarr(naer) & taod = fltarr(naer) & tsca  = taod & saod = taod & ssca = taod
reads, line, tmpchar, naer, aerwavs, taod, tsca, saod, ssca, ai, format='(A8, I3, 406E10.2)'

; read atmos and ozone profiles
readf, lun, tmpchar, nlayer, ntp, format='(A31, 2I5)'
atmos  = fltarr(3, nlayer +1) & ozprof = fltarr(5, nlayer)
readf, lun, tmpchar
temp = fltarr(3) & readf, lun, idum, temp
atmos(*, 0) = temp
temp = fltarr(9, nlayer) & readf, lun, temp
atmos(*, 1:nlayer) = temp(1:3, *) & ozprof = temp(4:8, *)

toz  = fltarr(3) & tropoz = fltarr(3) & stratoz = fltarr(3)
readf, lun, line & line = strmid(line, 25, 100)
temp = fltarr(4) & reads, line, temp & toz = temp(1:3)
readf, lun, line & line = strmid(line, 25, 100)
temp = fltarr(4) & reads, line, temp & stratoz = temp(1:3)
readf, lun, line & line = strmid(line, 25, 100)
temp = fltarr(4) & reads, line, temp & tropoz = temp(1:3)

; read the following selected output
; fitted tracegas gases, fitted variables, correlation matrix, covariance matrix, 
; contribution function, averaging kernels, fitting residuals
tracegas = 0.  & fitvar = 0. & correl  = 0.0 
avgk  = 0.  & covar  = 0. & contri  = 0.0
waves = 0.  & simrad = 0. & fitspec = 0.0  & ring = 0.0
over = 0    & actrad = 0. & clmrad  = 0.0
wf = 0.0    &  snr = 0.0
traceprof = 0.0 & traceavgk = 0.0 & tracecontri=0.0
fitspec0=0.0 &  simrad0 = 0.0 & rad347=0.0
while (not eof(lun) and over eq 0) do begin
    readf, lun, line
    ; read fitted tracegas gases
    if strpos(line, 'Measurement Reflectance at 347nm') ne -1 then begin
       readf, lun, line    
       rad347 = fltarr(2)   
       reads, line, rad347
    endif else if strmid(line, 0, 12) eq 'Fitted trace' then begin
       line = strmid(line, 36, 100)
       reads, line, ngas
       readf, lun, line
       if ngas gt 0 then begin
           tracegas = fltarr(10, ngas) & temp  = fltarr(10)
           ;traceprof = fltarr(nlayer, ngas)
       endif
       for i = 0, ngas - 1 do begin
           readf, lun, line  & line = strmid(line, 9, 100)
           reads, line, temp & tracegas(*, i) = temp
       endfor
    endif else if strmid(line, 0, 14) eq 'Trace gas aver'  and ngas ne 0 then begin
        traceprof = fltarr(ngas, nlayer)
        traceavgk = fltarr(ngas, nlayer)
        temp = fltarr(nlayer)
        ;for i = 0, ngas - 1 do begin
        ;    readf, lun, temp
        ;    traceprof(i, *) = temp
        ;    readf, lun, temp
        ;    traceavgk(i, *) = temp
        ;endfor
        for i = 0, ngas - 1 do begin
            readf, lun, line
            readf, lun, line
        endfor    
    endif else if strmid(line, 0, 14) eq 'Trace gas cont' and ngas ne 0  then begin
        line = strmid(line, 33, 100)
        reads, line, nq, nw, format = '(I5,  I5)'        
        tracecontri = fltarr(ngas, nw)
        temp = fltarr(nw)
        for i = 0, ngas - 1 do begin
            readf, lun, temp
            tracecontri(i, *) = temp
        endfor     
    endif else if strmid(line, 0, 16) eq 'Fitted variables' then begin
        reads, line, tmpchar, nf, format = '(A35, I5)'
        fitvar = fltarr(3, nf) & temp = fltarr(3)
        varname = strarr(nf)
        for i = 0, nf - 1 do begin
            readf, lun, line  
            varname(i) = strtrim(strmid(line, 3, 6),2)
            line = strmid(line, 9, 100)           
            reads, line, temp & fitvar(*, i) = temp
        endfor
    endif else if strmid(line, 0, 11) eq 'Correlation' then begin
        line = strmid(line, 20, 100)
        reads, line, nrf
        correl = fltarr(nrf, nrf)
        for i = 0, nrf - 1 do begin
            temp = fltarr(i+1)
            readf, lun, idum, temp, format='(I2,1x,100f6.2)'
            correl(i, 0:i) = temp
        endfor
    endif else if strmid(line, 0, 10) eq 'Covariance' then begin
        line = strmid(line, 19, 100)
        reads, line, nflay
        covar = fltarr(nflay, nflay)
        readf, lun, covar
    endif else if strmid(line, 0, 16) eq 'Average kernel: ' then begin

        reads, line, tmpchar, nflay, stlay, endlay, format='(A16, 3I5)'
        avgk = fltarr(nflay, nflay)
        readf, lun, avgk
    endif else if strmid(line, 0, 12) eq 'Contribution' then begin
        reads, line, tmpchar, nw, format = '(A23, I5)'
        contri = fltarr(nflay, nw)
        readf, lun, contri
    endif else if strmid(line, 0, 9) eq 'Weighting' then begin
        reads, line, tmpchar, nw, nf, format = '(A20, 2I5)'
        wf = dblarr(nf, nw)
        readf, lun, wf
        wf = transpose(wf)
    endif else if strmid(line, 0, 11) eq 'Measurement' then begin
        reads, line, tmpchar, nw, format = '(A19, I5)'
        snr = fltarr(nw)
        readf, lun, snr
    endif else if strmid(line, 0, 7) eq 'Fit res' then begin
        reads, line, tmpchar, nw, format = '(A14, I5)'
        lines = strarr(nw) 
        ; modified by jbak to include "actrad", here
        readf, lun, lines
        nspec=n_elements(str_sep(strcompress(strtrim(lines(0), 2)), ' '))
        data = fltarr(nspec, nw)
        reads, lines, data
        waves=fltarr(nw)     & fitspec=waves           &  simrad = waves & actrad = waves
        waves(*) = data(0, *)  & fitspec(*) =data(1, *)  &  simrad(*)=data(2, *) 
      	if nspec eq 4 then begin
       	   actrad (*) = data(nspec-1,*)
        endif
    endif else if strmid(line, 0, 8) eq 'Ring+Fit' then begin
        reads, line, tmpchar, nw, format = '(A20, I5)'
        data = fltarr(4, nw) & readf, lun, data
        waves=fltarr(nw)     & fitspec=waves  &  simrad = waves  & ring = waves
        waves(*) = data(0, *) & fitspec(*) =data(1, *) & simrad(*)=data(2, *) & ring(*) = data(3, *)
        
    endif else if strmid(line, 0, 12) eq 'Radiance Cal' then begin
        reads, line, tmpchar, nw, format = '(A22, I5)'
        data = fltarr(5, nw) & readf, lun, data
        waves=fltarr(nw) & fitspec=waves &  simrad = waves & actrad=waves & clmrad=waves
        waves(*) = data(0, *)     & fitspec(*) = data(1, *)  &  simrad(*)=data(2, *)   
        clmrad(*) = data(3, *)    & actrad(*)  = data(4, *)
    endif else if strmid(line, 0, 12) eq 'Radiance clm' then begin     
        reads, line, tmpchar, nw, format = '(A18, I10)'
        data = fltarr( nw) & readf, lun, data
        clmrad  = data
    endif else if strmid(line, 0, 4) eq 'Line' then begin
        over = 1
    endif    
endwhile

return

bad: pexval = -1


return
end
