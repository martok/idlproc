{%MainUnit parser.pas}

const
  yi_maxlevels = 5;

var
  yi_stack: array[0..yi_maxlevels] of record
    yyinput           : Text;        (* input and output file *)
    yyline            : String;      (* current input line *)
    yylineno, yycolno : Integer;     (* current input position *)
    fn                : AnsiString;
    prev_wrap         : yywrap_t;
  end;
  yi_level: integer;

function yinclude_wrapone(): Boolean;
begin
  Close(yyinput);
  Result:= yi_level = 0;
  if not Result then begin;
    Dec(yi_level);
    yyinput:= yi_stack[yi_level].yyinput;
    yyline:= yi_stack[yi_level].yyline;
    yylineno:= yi_stack[yi_level].yylineno;
    yycolno:= yi_stack[yi_level].yycolno;
    yywrap:= yi_stack[yi_level].prev_wrap;
    yyfilename:= yi_stack[yi_level].fn;
  end;
end;

function yinclude_push(const incfile: ansistring): Boolean;
var
  nfn: string;
begin
  nfn:= yinclude_expand(incfile);
  if nfn = '' then begin
    yyerror('Include file not found: '+incfile);
    yyabort;
    Exit(false);
  end;

  yi_stack[yi_level].yyinput:= yyinput;
  yi_stack[yi_level].yyline:= yyline;
  yi_stack[yi_level].yylineno:= yylineno;
  yi_stack[yi_level].yycolno:= yycolno;    
  yi_stack[yi_level].prev_wrap:= yywrap;      
  yi_stack[yi_level].fn:= yyfilename;
  inc(yi_level);                      
  yywrap:= @yinclude_wrapone;
  AssignFile(yyinput, nfn);
  Reset(yyinput);
  yyfilename:= nfn;
  yyline:= '';
  yylineno:= 0;
  yycolno:= 0;
  Result:= true;
end;








