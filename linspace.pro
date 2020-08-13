FUNCTION linspace, initial, final, n
	Compile_Opt idl2

	array=initial+(final-initial)*FINDGEN(n+1)/(n)
	RETURN, array
END


