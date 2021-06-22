function strnumber2, char
   ref  = 'qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM[{]};:,<.>/?`~!@#$%^&*()-_=+\|'
   nlen = strlen(char)
   out = 1
   for i = 0 , nlen-1 do begin
      the = strmid(char, i,1)
          
      if strpos(ref,the) eq -1 then begin
         out = 0 & return, out
      endif
   endfor
end 
