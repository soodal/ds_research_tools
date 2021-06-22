function zmid, inz, pmid=pmid

nl = n_elements(inz)
outz = (inz(0:nl-2)+inz(1:nl-1))*0.5
if keyword_Set(pmid) then outz = exp((alog(inz(0:nl-2))+alog(inz(1:nl-1)))*0.5)
return, outz

end
~
