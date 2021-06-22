pro  get_pos, nx, ny, x0,x1,x2,y0,y1,y2, positions, col=col

ic = 0
positions = fltarr(4,nx*ny)
IF keyword_set(col) then begin
   FoR i = 0 , nx -1 do begin
    FOR j = 0, ny-1 do begin
   xx  = x0 + (x1+x2)*i
   yy  = y0 - (y1+y2)*j
    positions(*,ic) = [xx,yy-y1, xx + x1,yy]
    ic = ic + 1
   ENDFOR
 ENDFOR
endif else begin

 FOR j = 0, ny-1 do begin
   FoR i = 0 , nx -1 do begin
   xx  = x0 + (x1+x2)*i
   yy  = y0 - (y1+y2)*j
    positions(*,ic) = [xx,yy-y1, xx + x1,yy]
    ic = ic + 1
   ENDFOR
 ENDFOR
endelse

end 




;----------------------------------------------------------------------------------------------------------------------------------
FILE_NAME     = '/home/JDB/TOOLS/GEMS_H5/GEM_TEST_L2__O3__PR_20150113T140000_20150113T140012_00001_00_000001_20160705T092946.h5'
figname       = '/home/JDB/test.ps'
thepix        =  15  
;---------------------------------------------------------------------------------------------------------------------------------


; gems data
PATH_DAT_NAME = '/HDF5/SWATHS/O3Profile/Data Fields/'
PATH_GEO_NAME = '/HDF5/SWATHS/O3Profile/Geolocation Fields/'
ozprof0   = read_h5_data ( file_name, path_dat_name, 'O3')
aozprof0  = read_h5_data ( file_name, path_dat_name, 'O3Apriori')
exval     = read_h5_data ( file_name, path_dat_name, 'ProcessingQualityFlags')
rms       = read_h5_data ( file_name, path_dat_name, 'RootMeanSquareErrorOfFit')
lat0      = read_h5_data ( file_name, path_geo_name, 'Latitude')
ps0       = read_h5_data ( file_name, path_geo_name, 'Pressure')
sza0      = read_h5_data ( file_name, path_geo_name, 'SolarZenithAngle')

; dimension
dims      = size(ozprof0,/dim)
nl        = dims(0)-1
nxtrack   = dims(1)
nytrack   = dims(2)
pix       = fltarr(nxtrack, nytrack)
for  i    = 0 , nytrack -1 do pix(*,i) = indgen(nxtrack) + 1

; select good pixels
sel = where( exval gt 0 and exval lt 100 and rms le 3 and pix eq thepix and sza0 le 80, nsel)

; define plotting varialbes
;    original data : [nlayers, npix, nline]
;    plotting data : [nline, nalyers] for one pix
lat     = lat0(sel)
ozprof  = fltarr(nsel, nl)
aozprof = fltarr(nsel, nl)
ps      = fltarr(nsel, nl+1) 
FOR i = 0 , nl  do begin
   if i le nl -1 then begin
      tmp = ozprof0(i,*,*)
      ozprof(*,i) = tmp(sel)
      tmp = aozprof0(i,*,*)
      aozprof(*,i) = tmp(sel)
   endif
      tmp     = ps0(i,*,*)
      ps(*,i) = tmp(sel)
ENDFOR
pmid  = fltarr(nsel,nl) & for i = 0 , nl-1 do pmid(*,i)=exp((alog(ps(*,i)) + alog(ps(*,i+1)))*0.5)


;;;;;;;;;;;;;;;;;;;;
; PLOT DATA
;;;;;;;;;;;;;;;;;;;;
set_plot, 'ps'
device, file=figname, /portrait, /inches, /color, xsize=7, ysize=5., xoffset=0.5, yoffset=0.5
loadct,33
!x.minor=10
!y.minor=10
!p.background=0
get_pos, 1,2,0.1,0.7,0.14, 0.9,0.3, 0.14, position
charsize = 2
xdata = lat &  xtitle = '!6Latitude'
ydata = pmid & ytitle = '!6Pressure(hPa)'
xrange = [min(xdata),max(xdata)]
xrange = [-90,90] & xtick = 30

yrange = [1000,1]
ytickv    = [1000,100,10,1,0.1] 
ytickname ='!6'+['10!U3!N','10!U2!N','10!U1!N','10!U0!N', '10!U-1!N']
seltick   = where ( ytickv ge yrange(1), nytick )
ytickv    = ytickv(seltick)
ytickname = ytickname(seltick)

;+++++++++ ozone++++++++++++
!p.multi = [0, 1,3]
loadct, 33
!p.charsize =charsize
  ydata1 = ydata
  xdata1 = xdata 
  level  = [-999, findgen(21)*2.5,1000]  &  ncol   = n_elements(level)-1
  color  = findgen(ncol)*250/ncol + 2

FOR i = 0 , 1 do begin
  data = ozprof(*,0:nl-1)  & title = '!6 Ozone Profile Retreivals'
  if  i eq 1 then begin
    data = aozprof(*,0:nl-1) & title = '!6 Apriori Profile'
  endif
  !p.position = position(*,i)
  plot,   indgen(2), indgen(2), /nodata, xrange=xrange, yrange=yrange, xtickinterval = xtick,$
        title=title, xtitle = xtitle, ytitle=ytitle, yticks=n_elements(ytickv)-1,ytickv=ytickv, ytickname=ytickname, /ylog
  contour, data, xdata1, ydata1, levels=level, c_colors=color, /cell, /over
  ;oplot, lat0, ptrp, linestyle=2, color=200
  ;oplot, xdata, ctp, color=255, thick=5
  colorbar_2, level, color, format='(i2.2)', charsize=1.25, /right, /col, /nofra,/nofirst, $
             lowleft=[!p.position(2)+0.01, !p.position(1)], ys=!p.position(3) - !p.position(1),xs=0.01,levelind=findgen(ncol)*3
 ; mycolorbar, level, color, unit_of_o3,format, /vert, step=step,  charsize=1.25,/nofirst ;, lowleft=lowleft, what=what
ENDFOR
device, /close
display_ps2png, figname
END
