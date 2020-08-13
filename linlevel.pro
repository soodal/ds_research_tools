FUNCTION linlevel, initial, final, n, array
	Compile_Opt idl2

	array=initial+(final-initial)*FINDGEN(n)/(n-1)
	RETURN, array
END


