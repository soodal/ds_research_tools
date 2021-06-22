;===================================================================================================
; This program reads SAO OE ozprof output file
;
; PROGRAM NAME:
;  read_omil2_file
;
; PURPOSE:
;  Read SAO OE ozprof output file
;
; PROCESS:
;
; REFERENCE:
;
; INPUT:
;
; OUTPUT:
;
; DEVELOPER:
;  Jbak
;  Daegeun Shin (geun)
;  Satellite Remote Sensing Laboratory
;  Division of Earth Environmental System
;  College of Natural Science
;  Pusan National University
;                  Tel: +82-51-510-2172
;                  E-mail: daegeun@pusan.ac.kr
;
; REVISION HISTORY:
;  Jbak version 0.
;  added silence etc .. (geun)
;  modified nw0 addfac from constant value 6
;
;  Copyright (C) 2017 Satellite Remote Sensing Laboratory, PNU
;  All right reserved.
;
;===================================================================================================


   ;  npix = 0l &  nprof = 0l & ntemp = 0l
   ;  for i = 0, nfile - 1 do begin;

   ;    com = 'grep -i Trop ' + fnames(i) + ' | wc -l'
   ;    spawn, com, result
   ;    reads, result, ntemp
   ;    nprof = nprof + ntemp

   ;    com = 'grep -i Line ' + fnames(i) + ' | wc -l'
   ;    spawn, com, result
   ;    reads, result, ntemp
   ;    npix = npix + ntemp
   ;    print, i, fnames(i), nprof, npix
   ; endfor


pro read_omil2_file, fnames, nl, nf, nalb, ngas, naer,nprof, $
    omilon, omilat, omisza, omivza, omiaza, omirms, omiavgres, $
    omicfrac, omictp, omicldflg, omiai, omiutc,omintp,   $ 
    ominw, omisaa, omiexval, ominiter, ominspike, omiglint, omildwt, omisnow, $
    omimon, omiday, omiyear, omipix, omiline, omitaod, omitsca, omisaod, omialb, atmos, $
    ozprofs, omicol, omitrace,  omifitvar, omiavgk, omicorrel, omicovar, omicontri, omifitspec, $
    omisimrad, omiwaves, omiclmrad, omiactrad, omiwf, omisnr, omiring, $
    nfail, flon, flat, fsza, fvza, faza, fmon, fday, fyear, $
    fpix,  fline, fsaa, fexval, fniter, fnspike, fglint, fldwt, fsnow,  $
    orbits=orbits, orbspix = orbspix, orbepix = orbepix, omiorb=omiorb,varname = varname, $
    get_trace=get_trace, get_fitvar=get_fitvar, get_correl=get_correl, get_covar=get_covar, $
    get_avgk = get_avgk, get_contri = get_contri, get_fitres=get_fitres, fxpos=fxpos, lxpos=lxpos, $
    get_radcal = get_radcal, get_wf = get_wf, get_snr = get_snr, get_ring=get_ring,  $
    omitraceprof=omitraceprof, omitraceavgk=omitraceavgk, omitracecontri=omitracecontri, omialb0=omialb0, $
    omiptrp = omiptrp, omiztrp=omiztrp, silence=silence
    

if not arg_present(fxpos) then fxpos = 1
if not arg_present(lxpos) then lxpos = 60

nfile   = n_elements(fnames)
orbits    = lonarr(nfile)
orbspix = lonarr(nfile)   &   orbepix = lonarr(nfile)

; find number of pixels in the data
; updated by Jbak 2016m0408

if nfile lt 1000 then begin  
   npix = 0l &  nprof = 0l 
   mind = intarr(nfile) 
   for ifile = 0 , nfile -1 do begin 

     nlines = file_lines(fnames(ifile))
     lines  = strarr(nlines)
     close, 1 &  openr, 1, fnames(ifile)
     readf, 1, lines
     close, 1
      tmp1 = where( strpos(lines ,'Trop') ne -1, ntmp1)
      tmp2 = where( strpos(lines, 'Exit Status') ne -1, ntmp2)
      if ntmp1 eq 0 and ntmp2 eq 0 then mind(ifile) = -1
      if ntmp1 gt ntmp2 then stop
      nprof = nprof + ntmp1
      npix  = npix  + ntmp2
      if not keyword_set (silence) then begin
        print, 'ifile','npix', 'nprof', 'ntmp1', 'ntmp2', format='(10a10)'
        print, ifile, npix, nprof, ntmp1, ntmp2, format='(10i10)'
      endif

   endfor 
   nfail = npix - nprof        ;changed from nfail = npix - nprof + 1 to nfail = npix-nprof by Jbak
   tmp    = where( mind eq 0, nfile)
   if nfile eq 0 then return
   fnames = fnames(tmp)
   if not keyword_set (silence) then begin
     print, 'Number of pixels: ', npix
     print, 'Number of retrievals: ', nprof
   endif

endif else begin
   nprof = nfile
   nfail = 1000
endelse
;nprof = nprof +1 
if (nprof le 0) then return

; read one retrieval to initialize necessary array size 
i = 0l  & found=0

while found eq 0 and i lt nfile do begin

  line = ' '
  openr, lun, fnames(i), /get_lun
  result = fstat(lun)
  if result.size lt 1024 then begin
    close, lun & free_lun, lun
    i = i + 1
    continue
  endif

  readf, lun, line
  exval = -1
  while exval lt 0 do begin
    readf, lun, line, line, line
    readf, lun, exval  
  endwhile
  if exval lt 0 then begin
    print, 'No valid pixels found!!!'
    i = i + 1
    continue
  endif 

  aline = 0 &  apix=0 & numwin = 0

  read_omil2_pix, lun, nl, ntp, nf, nw, nalb, ngas, naer, rms, avgres, dfs, info, intalb, $
    retalb, cfrac, ctp,  cldflg, ai, taod, tsca, saod, atm, ozprof, toz, stratoz, tropoz, $
    fitvar, avgk, waves, fitspec, simrad, clmrad, actrad, tracegas, correl, covar, contri, wf, $
    snr, line, pexval, ring, traceprof, traceavgk, tracecontri, varname, numwin, aline, apix
     
  close, lun & free_lun, lun
  if pexval ge 0 then begin
    found=1 & break
  endif else begin
    print, i, pexval, toz(0)
  endelse

i = i + 1
endwhile

nalb = 3
if found eq 0 then begin
    print, 'Empty data!!!'
    return
endif

; initialize variables for successful retrievals
omilon =   fltarr(nprof, 5) &  omilat = omilon
omisza =   fltarr(nprof)    &  omivza = omisza   & omiaza = omisza
numwin   = n_elements(rms)-1
omirms   = fltarr(nprof, numwin+1)  & omiavgres = omirms
omicfrac = omisza            & omictp = omisza    & omiutc = omisza  & omiai = omisza 
omiptrp  = omisza            & omiztrp= omisza
omintp   = intarr(nprof)    & omiexval = omintp  & ominiter = omintp
omisaa   = omintp           & omiglint = omintp  & omildwt = omintp
omisnow  = omintp           & omimon = omintp    & omiday   = omintp  & omiyear = omintp 
omipix   = omintp           & omiline = omintp   & ominspike = omintp & ominw = omintp
omicldflg = omintp


if arg_present(omiorb) then omiorb = lonarr(nprof)

; aerosols and albedo
omitaod = fltarr(nprof, 68)  &  omitsca  = omitaod  &  omisaod = omitaod
omialb  = fltarr(nprof, nalb)  & nalb0 = nalb  & naer0 = naer
omialb0  = fltarr(nprof, nalb)
; p, z, t, ap, apstd, ret, retstd,
atmos = fltarr(nprof, 3, nl+1) & ozprofs = fltarr(nprof, 5, nl)
  
; column ozone
omicol = fltarr(nprof, 3, 6)  ; TO, SCO, TCO (val, s+n, n, dfs,aval)



; optional variables
do_trace = 0  & do_fitvar = 0 & do_correl = 0  & do_covar = 0
do_contri = 0 & do_avgk = 0   & do_res = 0     & do_radcal = 0  & do_ring = 0
omitrace = 0. & omitraceprof = 0.0 & omifitvar = 0.  & omicorrel = 0. & omicovar = 0.0
omitraceavgk = 0. & omitracecontri = 0.0
omicontri = 0. & omisimrad = 0.0 & omifitspec = 0.0 & omiwaves = 0.0
omiavgk = 0.0  & omiclmrad = 0.0 & omiactrad = 0.0  & omiring = 0.0

do_wf = 0   & do_snr = 0 & omiwf = 0.0 & omisnr = 0.0 

do_traceprof = 0 & do_traceavgk = 0 & do_tracecontri = 0
addfac=10
nw0 = nw + addfac
if (n_elements(tracegas) gt 1 and keyword_set(get_trace)) then begin
    do_trace = 1
    omitrace = fltarr(nprof, 10, ngas)
    if n_elements(traceprof) gt 1 then begin
        omitraceprof=fltarr(nprof, ngas, nl)
        do_traceprof = 1
    endif
    if n_elements(traceavgk) gt 1 then begin
        omitraceavgk=fltarr(nprof, ngas, nl)
        do_traceavgk = 1
    endif
    if n_elements(tracecontri) gt 1 then begin
        omitracecontri=fltarr(nprof, ngas, nw0)
        do_tracecontri = 1
    endif        
endif

if (n_elements(fitvar) gt 1 and keyword_set(get_fitvar)) then begin
    do_fitvar = 1
    omifitvar = fltarr(nprof, 3, nf)
endif

if (nf gt 1 and keyword_set(get_correl) ) then begin
    do_correl = 1
    omicorrel = fltarr(nprof, nf, nf)
endif

if (n_elements(covar) gt 1 and keyword_set(get_covar)) then begin
    do_covar = 1
    omicovar = fltarr(nprof, nl, nl)
endif

if (n_elements(avgk) gt 1 and keyword_set(get_avgk)) then begin
    do_avgk   = 1
    omiavgk    = fltarr(nprof, nl, nl)
endif

if (n_elements(waves) gt 1 and keyword_set(get_fitres)) then begin
    do_res    = 1
    omifitspec = fltarr(nprof, nw0)
    omisimrad  = omifitspec
    omiwaves   = omifitspec

endif

if (n_elements(ring) gt 1 and keyword_set(get_ring)) then begin
    do_ring    = 1
    omiring = fltarr(nprof, nw0)
endif

if (n_elements(waves) gt 1 and keyword_set(get_radcal)) then begin
    do_radcal    = 1
    omiwaves    = fltarr(nprof, nw0)
    omisimrad  = fltarr(nprof, nw0)
    omifitspec = fltarr(nprof, nw0)
    ;omifitspec = 0.0 & omisimrad = 0.0 & omiwaves = 0.0
    omiclmrad  = fltarr(nprof, nw0)
    omiactrad  = fltarr(nprof, nw0)
endif

if (n_elements(contri) gt 1 and keyword_set(get_contri)) then begin
    do_contri = 1
    omicontri  = fltarr(nprof, nl, nw0)
endif

if (n_elements(wf) gt 1 and keyword_set(get_wf)) then begin
    do_wf = 1
    nf = n_elements(wf(0, *))  & nw0 = n_elements(wf(*, 0)) + addfac
    omiwf  = dblarr(nprof, nw0, nf)
   ; help , omiwf
endif

if (n_elements(snr) gt 1 and keyword_set(get_snr)) then begin
    do_snr = 1
    nw0 = n_elements(snr) + addfac 
    omisnr  = fltarr(nprof, nw0)
endif
  


; variables for unsucessful retrievals

if (nfail gt 0) then begin
    flon = fltarr(nfail, 5)  & flat = flon
    fsza = fltarr(nfail)  & fvza = fsza  & faza = fsza  & futc = fsza
    fline = intarr(nfail) & fpix =fline  & fmon = fline & fday = fline
    fyear = fline & fexval = fline  & fniter = fline & fsaa = fline & fnspike = fline
    fldwt = fline  & fglint = fline & fsnow = fline
endif else begin
    flon = 999  & flat = flon
    fsza = 999  & fvza = fsza  & faza = fsza  & futc = fsza
    fline = 999 & fpix =fline  & fmon = fline & fday = fline
    fyear = fline & fexval = fline  & fniter = fline & fsaa = fline & fnspike = fline
    fldwt = fline  & fglint = fline & fsnow = fline
endelse

; start to read the data
iprof = 0l &   ifail = 0l
tmpchar = ' '  & saac = ' '  & utc = ' '
temp = fltarr(13)
apix = 0 & aline = 0 & saa = 0 & nspike = 0 & ldwt = 0  & glint = 0  & snow = 0 & niter = 0
year = 0 & mon = 0 & day = 0 & hour = 0 & minute = 0 & second = 0.0

ifile = 0l
while ifile lt nfile do begin
;for ifile = 0, nfile - 1 do begin
    pos1 = strpos(fnames(ifile), '_o')
    pos2 = strpos(fnames(ifile), '-o')
    pos = max([pos1, pos2])

    if pos gt 0 then begin
        orbit = strmid(fnames(ifile), pos+2, 5)

        if strnumber2( orbit) eq 1 then begin
            fnames0 = strmid(fnames, pos+2,strlen(fnames) - pos)
            pos1 = strpos(fnames0(ifile), '_o')
            pos2 = strpos(fnames0(ifile), '-o')
            pos = max([pos1, pos2])
            orbit = strmid(fnames0(ifile), pos+2, 5)
            if pos eq 0 then stop
        endif
    endif else begin
        orbit=-9999
    endelse
    orbits(ifile) = orbit
    orbspix(ifile) = iprof
         
    openr, lun, fnames(ifile), /get_lun
     
    if result.size lt 1024 then begin       
       close, lun & free_lun, lun
       ifile = ifile + 1
       continue
   endif
   
    line = ' '
    while strmid(line, 0, 4) ne 'Line' do begin
        readf, lun, line
    endwhile


    while not eof(lun) do begin

       ; error handling
        on_ioerror, bad         ; error handling

        reads, line, tmpchar, aline, apix, utc, format =  '(A11,2I5,1X,A28)'

        reads, utc, year, mon, day, hour, minute, second, format='(I4,1x,I2,1x,I2,1x,I2,1x,I2,1x,F9.3)'
        autc = (second / 60. + minute) / 60. + hour
          
        ; read geolocation, viewing geometry
        readf, lun, temp
        lon = temp([1, 3, 5, 7, 9]) & lat = temp([0, 2, 4, 6, 8])
        sza = temp(10) & vza = temp(11)  & aza = temp(12)

        ; read exit status and other flags
        readf, lun, line
        readf, lun, exval, niter, saac, nspike, ldwt, glint, snow, format = '(2I4,A4,4I4)'


        if strtrim(saac) eq 'T' then saa = 1  else saa = 0


        if (exval ge 0) then begin
            omipix (iprof) = apix & omiline (iprof) = aline
            omiyear(iprof) = year & omimon (iprof) =  mon
            omiday(iprof) = day & omiutc(iprof) = autc
            omilon(iprof, *) = lon & omilat(iprof, *) = lat
            omisza(iprof) = sza & omivza(iprof) = vza & omiaza(iprof) = aza
            omiexval(iprof) = exval & ominiter(iprof) = niter & omisaa(iprof) = saa    & ominspike(iprof) = nspike
            omiglint(iprof) = glint & omildwt(iprof) = ldwt  & omisnow(iprof) = snow
           
            read_omil2_pix, lun, nl, ntp, nf, nw, nalb, ngas, naer, rms, avgres, dfs, info, intalb,     $
                retalb, cfrac, ctp,  cldflg, ai, taod, tsca, saod, atm, ozprof, toz, stratoz, tropoz, fitvar, avgk, $
                waves, fitspec, simrad, clmrad, actrad, tracegas, correl, covar, contri, wf, snr, line, pexval, ring, $
                traceprof, traceavgk, tracecontri, varname, numwin, aline, apix
             
            if apix ge fxpos and apix le lxpos then begin
                   
                IF pexval ge  1 then begin
                omirms(iprof, *) = rms  & omiavgres(iprof, *) = avgres  
                omicol(iprof, 0, 3) = dfs  
                temp0 = [0, cum_total(reform(ozprof(0,*)))]
                temp1 = [0, cum_total(reform(ozprof(1,*)))]
                omicol(iprof, *, 4) = [temp0(nl), temp0(ntp), temp0(nl)-temp0(ntp)]
                omicol(iprof, *, 5) = [temp1(nl), temp1(ntp), temp1(nl)-temp1(ntp)]
                ;print ,iprof, reform(omicol(iprof, *,4))
                omialb(iprof, 0:nalb-1) = retalb(0:nalb-1)
                omialb0(iprof, 0:nalb-1) = intalb(0:nalb-1)    
                omicfrac(iprof) =  cfrac & omictp(iprof) = ctp
                omicldflg(iprof) =  cldflg & omiai(iprof) = ai
                omitaod(iprof, 0:naer-1)  = taod(0:naer-1) & omitsca(iprof, 0:naer-1) = tsca(0:naer-1) 
                omisaod(iprof, 0:naer-1) = saod(0:naer-1)
                
                atmos(iprof, 0:2, *) = atm
                ozprofs(iprof, *, *) = ozprof
                omintp(iprof) = ntp
                ominw(iprof) = nw
                
                omicol(iprof, 0, 0:2) = toz
                omicol(iprof, 1, 0:2) = stratoz
                omicol(iprof, 2, 0:2) = tropoz
                omiptrp (iprof) = atmos(iprof, 0, ntp)
                omiztrp (iprof) = atmos(iprof, 1, ntp)
                
                if do_trace eq 1 then begin
                    omitrace(iprof, *, *) =  tracegas
                    if do_traceprof eq 1 then omitraceprof(iprof, *, *) =  traceprof
                    if do_traceavgk eq 1 then omitraceavgk(iprof, *, *) =  traceavgk
                    if do_tracecontri eq 1 then omitracecontri(iprof, *, 0:nw-1) =  tracecontri
                endif
                 
                if do_fitvar eq 1 then omifitvar(iprof, *, *) = fitvar
                if do_avgk eq 1   then omiavgk(iprof, *,*) = avgk
                if do_correl eq 1 then omicorrel(iprof, *, *) = correl
                if do_covar eq 1 then omicovar(iprof, *,*) =  covar
                if do_contri eq 1 then omicontri(iprof,*, 0:nw-1) = contri
                if do_wf eq 1 then omiwf(iprof, 0:nw-1, *) = wf
                if do_snr eq 1 then omisnr(iprof, 0:nw-1) = snr
                if do_res eq 1   then begin
                    omifitspec(iprof, 0:nw-1) = fitspec
                    omisimrad(iprof, 0:nw-1)  = simrad   
                    omiwaves(iprof, 0:nw-1)  = waves                   
                endif
                if do_ring eq 1 then omiring(iprof, 0:nw-1) = ring

                if do_radcal eq 1   then begin
                    omifitspec(iprof, 0:nw-1) = fitspec
                    omisimrad(iprof, 0:nw-1)  = simrad   
                    omiwaves(iprof, 0:nw-1)  = waves
                    omiclmrad(iprof, 0:nw-1) = clmrad ;+ actrad - simrad
                    omiactrad(iprof, 0:nw-1) = actrad
                endif
                if arg_present(omiorb) then omiorb(iprof) = orbit
   
                orbepix(ifile) = iprof
                iprof = iprof + 1
                endif
                
            endif 
            
        endif else begin

            if apix ge fxpos and apix le lxpos then begin                
                fpix (ifail) = apix & fline (ifail) = aline
                fyear(ifail) = year & fmon (ifail) =  mon
                fday(ifail) = day & futc(ifail) = autc
                flon(ifail, *) = lon & flat(ifail, *) = lat
                fsza(ifail) = sza & fvza(ifail) = vza & faza(ifail) = aza
                fexval(ifail) = exval & fniter(ifail) = niter & fsaa(ifail) = saa    & fnspike(ifail) = nspike
                fglint(ifail) = glint & fldwt(ifail) = ldwt  & fsnow(ifail) = snow
               ; print , ifail, exval, sza
                ifail = ifail + 1
                nfail = ifail
             endif
                
             if (not eof(lun)) then readf, lun, line
        endelse
    endwhile
    
   goto, GOOD

   bad: 
   ;iprof = iprof - 1  ;should be deleted, commented by jbak, 1/17/2013
   ;print, fnames(ifile), omipix(iprof+1), omiline(iprof+1), iprof, omiexval(iprof+1),omiexval(iprof)
   good:
   close, lun & free_lun, lun
   
   ifile = ifile + 1
endwhile
;endfor



; getdfs
if do_avgk eq 1 then begin
    i = 0l
    while i lt nprof do begin
        omicol(i, 1, 3) = 0.0
        for j = 0, omintp(i)-1 do begin
            omicol(i, 1, 3) = omicol(i, 1, 3) + omiavgk(i, j, j)
        endfor
        omicol(i, 2, 3) = omicol(i, 0, 3) - omicol(i, 1, 3) 
        i = i + 1
    endwhile
endif

; get some retrieval statistics
;print, '# of output files                      = ', nfile
;print, '# of sucessful/unsuccessful retrievals = ', nprof, nfail
;print, 'Average total DFS                      = ', total(omicol(*, 0, 3))/nprof
;print, 'Average stratospheric DFS              = ', total(omicol(*, 1, 3))/nprof
;print, 'Average tropospheric DFS               = ', total(omicol(*, 2, 3))/nprof
;print, 'Average relative residual in radiance  = ', total(omiavgres)/nprof
;print, 'Average random error in total ozone    = ', total(omicol(*, 0, 1))/nprof
;print, 'Average random error in strat ozone    = ', total(omicol(*, 1, 1))/nprof
;print, 'Average random error in trop  ozone    = ', total(omicol(*,
;2, 1))/nprof

return
end

; test program
;; main program
;dir = '/home/xliu/OzoneFit/OZBOREAS-OMI/testout/omilv2/'
;fnames = findfile(dir + '*.out', count=nfile)
;
;read_omil2_file, fnames, nl, nf, nalb, ngas, naer, $
;    nprof, omilon, omilat, omisza, omivza, omiaza, omirms, omiavgres, omicfrac, omictp, omiutc,  $ 
;    omintp, ominw, omisaa, omiexval, ominiter, ominspike, omiglint, omildwt, omisnow, $
;    omimon, omiday, omiyear, omipix, omiline, omitaod, omitsca, omisaod, omialb, atmos, $
;    ozprofs, omicol, omitrace, omifitvar, omiavgk, omicorrel, omicovar, omicontri, omifitspec, $
;    omisimrad, omiwaves, $
;    nfail, flon, flat, fsza, fvza, faza, fmon, fday, fyear, $
;    fpix,  fline, fsaa, fexval, fniter, fnspike, fglint, fldwt, fsnow
;
;stop
;
;end


