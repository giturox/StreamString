unit StringConverters;

{$mode ObjFPC}{$H+}


interface
    uses
        Classes, SysUtils;

    function IntToString(value:LongInt):UTF8String;
	  function FloatToString(value:double; totaldigits, decimals:integer):UTF8String;


implementation
	  function IntToString(value:LongInt):UTF8String;
	  begin
	    str(value , result);
	  end;

	  function FloatToString(value:double; totaldigits, decimals:integer):UTF8String;
	  begin
	    str(value:totaldigits:decimals,result);
	  end;

end.


