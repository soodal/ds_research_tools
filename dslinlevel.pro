FUNCTION dslinlevel, initial, final, n, array
	Compile_Opt idl2

	array=double(initial)+(double(final)-double(initial))*FINDGEN(n)/(n-1)
	RETURN, array
END
