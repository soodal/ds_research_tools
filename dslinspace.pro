FUNCTION linspace, initial, final, n
	Compile_Opt idl2

	array=double(initial)+(double(final)-double(initial))*FINDGEN(n+1)/(n)
	RETURN, array
END


