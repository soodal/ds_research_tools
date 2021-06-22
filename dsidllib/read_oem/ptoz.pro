; jbak :표준기압/높이를 이용한 기압고도간 변환식을 코딩화

function PtoZ, P, reverse=reverse

; from Chamberlain 1978, US Standard Atm. 1976
Zstd = [0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0,55.0,60.0,$
      65.0,70.0,75.0,80.0,85.0,90.0,95.0,100.0]
Pstd = [1013.25,540.5,265.,121.1,55.29,25.49,11.97,5.746,2.871,1.491,$
   .7978,.4253,.2196,.1093,.05221,.02388,.01052,.00446,.00184,.00076,.00032]

Nlev = 21 ;Nstdlev


IF not keyword_Set( reverse) then begin

Px = float(P)

ix = -1
for i = 0,Nlev-2 do begin
  if (Px ge Pstd(i+1)) and (Px lt Pstd(i)) then ix = i
endfor
if(ix lt 0) then begin
  f1 = "('*** PtoZ(',f8.2,') is out of range:',f8.5,' <= P <=',f8.2, ' ***')"
  print, Px,Pstd(Nlev-1),Pstd(0),format=f1
  if(Px gt Pstd(0)) then begin
    ix = 0
  endif else begin
    ix = Nlev-2
  endelse
endif

a0 = alog10(Pstd(ix))
a1 = alog10(Pstd(ix+1))
fac = (alog10(Px)-a0)/(a1-a0)
Zx = Zstd(ix) + fac*(Zstd(ix+1)-Zstd(ix))

; use pressure altitude
Zx = -alog10(Px/1013.25) * 16.

return, Zx

ENDIF ELSE BEGIN

Zx = P


Zstd = [0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0,55.0,60.0,$
      65.0,70.0,75.0,80.0,85.0,90.0,95.0,100.0]
Pstd = [1013.25,540.5,265.,121.1,55.29,25.49,11.97,5.746,2.871,1.491,$
   .7978,.4253,.2196,.1093,.05221,.02388,.01052,.00446,.00184,.00076,.00032]

px = fltarr(n_elements(zx)) 
da = where( zx ge 0 and zx le 100)
tempz = Zx(da)
tempz = tempz( sort(tempz))  
tempp = spline(zstd ,alog(pstd),tempz )
tempp = exp(tempp)
Px(da) = tempp( sort(tempp) ) 

return, Px


ENDELSE
end
