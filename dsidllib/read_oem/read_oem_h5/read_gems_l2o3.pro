pro convert_TAI93_to_utc, tai93, year, mon, day, utc

   caldat, tai93/60./60./24. + julday(1,1,1993,0,0), mon, day, year, utc, mm, sec

   utc  = utc + (mm+ sec/60.)/60.

end

pro read_gems_l2o3 , file_name, gems, ngood

nl  = 24
STR = {alt:fltarr(nl+1), pres:fltarr(nl+1), avgk:fltarr(nl, nl), o3:fltarr(nl), ao3:fltarr(nl),$
       lat:0.0, lon:0.0,sza:0.0, cfrac:0.0, exval:0, rms:0.0,$
       year:0, mon:0, day:0, utc:0.0}

PATH_GEO_NAME = '/HDF5/SWATHS/O3Profile/Geolocation Fields/'
PATH_DAT_NAME = '/HDF5/SWATHS/O3Profile/Data Fields/'

data_list = ['EffectiveCloudFractionUV', 'ProcessingQualityFlags', 'RootMeanSquareErrorOfFit',$
            'AveragingKernel', 'O3' , 'O3Apriori']
geo_list  = ['Latitude' ,'Longitude', 'SolarZenithAngle', 'Time','Altitude' ,'Pressure']

n_data = n_elements(data_list)
n_geo  = n_elements(geo_list)

file_id         = H5F_open (file_name)
group_id_geo    = H5G_OPEN (file_id, path_geo_name)
group_id_dat    = H5G_OPEN (file_id, path_dat_name)
first = 0 
FOR k = 0 , n_geo -1 do BeGIN
    data_name   = geo_list(k)
    dataset_id  = H5D_OPEN (group_id_geo, data_name)
    data        = h5D_read (dataset_id)
    h5d_close, dataset_id 
    if first eq 0 and data_name eq 'Latitude'  then begin
       npix  = n_elements(data(*,0))
       nline = n_elements(data(0,*))
       gems = replicate(str, npix, nline)
       gems.lat = data
       first = 1
    endif else if first eq 0 and data_name ne 'Latitude' then begin 
       print, ' the first data should be latitude'
       stop
    endif

    IF data_name eq 'Longitude'           then gems.lon  = data
    IF data_name eq 'SolarZenithAngle'    then gems.sza  = data
    IF data_name eq 'Pressure'            then gems.pres(0:nl) = (data) ; should be hpa
    IF data_name eq 'Altitude'            then gems.alt(0:nl)  = (data) ; should be km
    IF data_name eq 'Time'                then begin
       time = data
       da = where( data ge 0 , nda)
       if nda eq 0 then continue
       convert_TAI93_to_utc, data(da), year, mon, day, utc
       for j = 0 , npix -1 do begin
         gems(j,da).year = transpose(year)
         gems(j,da).mon =  transpose(mon)
         gems(j,da).day =  transpose(day)
         gems(j,da).utc =  transpose(utc)
       endfor
    ENDIF
ENDFOR
H5G_close, group_id_geo

FOR k = 0 , n_data -1 do BeGIN
    data_name   = data_list(k)
    dataset_id  = H5D_OPEN (group_id_dat, data_name)
    data        = h5D_read (dataset_id)
    h5d_close,  dataset_id 
    IF data_name eq 'EffectiveCloudFractionUV' then gems.cfrac = data
    IF data_name eq 'ProcessingQualityFlags'   then gems.exval = data
    IF data_name eq 'RootMeanSquareErrorOfFit' then gems.rms   = data
    IF data_name eq 'AveragingKernel'          then gems.avgk  = data
    IF data_name eq 'O3'                       then gems.o3(0:nl-1)   = data(0:nl-1,*,*)
    IF data_name eq 'O3Apriori'                then gems.ao3(0:nl-1)  = data(0:nl-1,*,*)
ENDFOR
H5G_close, group_id_dat
H5F_close, file_id

da = where( gems.lat ge -90 , ngood)
gems  = gems(da)
END
