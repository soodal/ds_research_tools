pro read_omil2_iter_file, file, nf, $
ninter, name, chis, dfn, dfs, infor, $
x1, sn, ss, x0, xerr, xerrn, last_iter=last_iter, onlyoz=onlyoz

unit =9
lines = file_lines(file)

;;;;;;; out parameters ;;;;;;;;;;;
ninter = fix((lines-1)/(8+nf))
name = strarr(nf)
X1 = fltarr(ninter, nf) 
Sn = fltarr(ninter, nf)
SS = fltarr(ninter, nf)
X0 = fltarr(ninter, nf)
Xerr = fltarr(ninter, nf)
Xerrn = fltarr(ninter, nf)

chis  = fltarr(ninter, 3)
DFN   = fltarr(ninter, 2)
DFS   = fltarr(ninter, 2)
INFOR = fltarr(ninter, 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

line =strarr(1)
line8=strarr(8)
linef=strarr(nf)

openr, unit, file 
readf, unit, line
FOR it = 0, ninter-1 do begin
     tmpchar=strarr(1)
     tmp    =fltarr(1)
     tmp    =fltarr(1)
     readf, unit, line8     
     readf, unit, linef
     
     reads, line8(1), tmpchar, tmp, format='(a14,e12.6)'
     chis(it,0) = tmp
     reads, line8(2), tmpchar, tmp, format='(a14,e12.6)'     
     chis(it,1) = tmp
     reads, line8(3), tmpchar, tmp, format='(a25,e12.6)'
     chis(it,2) = tmp
     reads, line8(4), tmpchar, tmp1, tmpchar, tmp2, format='(a14,e12.6,a14,e12.6)'
     DFN (it, 0) = tmp1
     DFN (it, 1) = tmp2
     reads, line8(5), tmpchar, tmp1,tmpchar, tmp2, format='(a14,e12.6,a14,e12.6)'
     DFS (it, 0) = tmp1
     DFS (it, 1) = tmp2
     reads, line8(6), tmpchar, tmp, format='(a24,e12.6,a12,e12.6)'
     INFOR (it, 0) = tmp1 
     INFOR (it, 1) = tmp2
     
     FOR f = 0 , nf-1 do begin      
     tmpchar=strarr(1)
     tmp = strarr(6)
     reads, linef(f), tmpchar,tmp , format='(A6, 6e14.6)'
     name(f) = tmpchar
     X1(it, f)  = tmp(0) 
     Sn(it, f)  = tmp(1)
     SS(it, f)  = tmp(2)
     X0(it, f)  = tmp(3)
     Xerr(it, f) = tmp(4)
     Xerrn(it, f) = tmp(5)
     
     ENDFOR
     
ENDFOR
close, unit

if keyword_set( onlyoz) then begin
fidx = where(strmid(strtrim(name),0,4) eq 'oz01')
lidx = where(strmid(strtrim(name),0,4) eq 'oz24')
X1 = reform( x1(*, fidx:lidx )) 
Sn = reform( Sn(*, fidx:lidx ))
SS = reform( SS(*, fidx:lidx ))
X0 = reform( X0(*, fidx:lidx ))
Xerr = reform( Xerr(*, fidx:lidx))
Xerrn= reform( Xerrn(*, fidx:lidx))
name = reform( name(fidx:lidx))
endif

if keyword_set( last_iter) then begin
X1 = reform( x1(ninter-1, * )) 
Sn = reform( Sn(ninter-1, * ))
SS = reform( SS(ninter-1, * ))
X0 = reform( X0(ninter-1, * ))
Xerr = reform( Xerr(ninter-1, * ))
Xerrn= reform( Xerrn(ninter-1, * ))

chis  = reform(chis(ninter-1, *))
DFN   = reform(chis(ninter-1, *))
DFS   = reform(chis(ninter-1, *))
INFOR = reform(chis(ninter-1, *))

endif




end