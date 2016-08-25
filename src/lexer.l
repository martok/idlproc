%{
{
Vorspann
 ****************************************************************************}
%}

%{

(* Note: Keywords are determined by scanning a keyword table, rather
   than including the keyword patterns in the Lex source which is done
   in the original version of this file. I prefer this method, because
   it makes the grammar itself more readable (handling case-insensitive
   keywords in Lex is quite cumbersome, e.g., you will have to write
   something like [Aa][Nn][Dd] to match the keyword `and'), and also
   produces a more (space-) efficient analyzer (184 states and 375
   transitions for the keyword pattern version, against only 40 states
   and 68 transitions for the keyword table version). *)

function upper(str : String) : String;
  (* converts str to uppercase *)
  var i : integer;
  begin
    for i := 1 to length(str) do
      str[i] := upCase(str[i]);
    upper := str
  end(*upper*);

function is_keyword(id : string; var token : integer) : boolean;
  const
    id_len = 20;
  type
    Ident = string[id_len];
  const
    no_of_keywords = 13;
    keyword : array [1..no_of_keywords] of Ident = (
      'ATTRIBUTE', 'CONST', 'ENUM', 'IN', 'INOUT', 'INTERFACE', 'LIBRARY', 'MODULE', 'OUT', 'READONLY', 'STRUCT', 'TYPEDEF',  'UNION'
    );
    keyword_token : array [1..no_of_keywords] of integer = (    
      _ATTRIBUTE,  _CONST,  _ENUM,  _IN,  _INOUT,  _INTERFACE,  _LIBRARY,  _MODULE,  _OUT,  _READONLY,  _STRUCT,  _TYPEDEF,  _UNION
    );
  var m, n, k : integer;
  begin
    id := upper(id);
    (* binary search: *)
    m := 1; n := no_of_keywords;
    while m<=n do
      begin
        k := m+(n-m) div 2;
        if id=keyword[k] then
          begin
            is_keyword := true;
            token := keyword_token[k];
            exit
          end
        else if id>keyword[k] then
          m := k+1
        else
          n := k-1
      end;
    is_keyword := false
  end(*is_keyword*);

  procedure commenteof;
  begin
    writeln('unexpected EOF inside comment at line ', yylineno);
  end(*commenteof*);

  procedure skip_until_eol;
  var c: char;
  begin
    c:=get_char;
    while not (c in [#0, newline]) do
     c:=get_char;
  end;

  function take_until_eol: char;  
  var c: char;
  begin
    c:=get_char;
    while not (c in [#0, newline]) do begin
      c:=get_char;
    end;
    take_until_eol:= c;
  end;

%}

D [0-9]    
H [0-9a-fA-F]
%%

%{

var c  : char;
    kw : integer;

%}

[a-zA-Z_]([a-zA-Z0-9_])*	if (not st_in_attrib_list) and
                           is_keyword(yytext, kw) then
                          return(kw)
                        else
                          return(ID);

\"[^\"]*\"              |
\'[^\']*\'              |
"L"\"[^\"]*\"           |
"L"\'[^\']*\'           return(CSTRING);
{D}+[Uu]?[Ll]?[Ll]?     begin
                          while yytext[length(yytext)] in ['L','U','l','u'] do
                            Delete(yytext,length(yytext),1);
                          return(NUMBER);
                        end;
"0x"[0-9A-Fa-f]*[Uu]?[Ll]?[Ll]?
                        begin
                         (* handle pre- and postfixes *)
                         if copy(yytext,1,2)='0x' then
                           begin
                              delete(yytext,1,2);
                              yytext:='$'+yytext;
                           end;
                         while yytext[length(yytext)] in ['L','U','l','u'] do
                           Delete(yytext,length(yytext),1);
                         return(NUMBER);
                        end;
{D}+(\.{D}+)?([Ee][+-]?{D}+)?
                        return(NUMBER);
{H}{8}-{H}{4}-{H}{4}-{H}{4}-{H}{12}
                        return(IID);


":"                     return(COLON); 
";"                     return(SEMICOLON);
","                     return(COMMA);
"="                     return(EQUAL);

"("                     return(LKLAMMER); 
")"                     return(RKLAMMER);
"["                     return(LECKKLAMMER);
"]"                     return(RECKKLAMMER); 
"{"                     return(LGKLAMMER);
"}"                     return(RGKLAMMER);

"/*"                    begin
                          repeat
			    c := get_char;
			    case c of
			      '*' : begin
				      c := get_char;
				      if c='/' then exit else unget_char(c)
				    end;
                              #0 : begin
                                     commenteof;
                                     exit;
                                   end;
			    end;
			  until false;
                        end;
"//"                    skip_until_eol;
"%{"                    begin
                          repeat
                            c:= take_until_eol;
                            if c = #0 then begin
                              commenteof;
                              exit;
                            end;
                            c:= get_char;
			    case c of
			      '%' : begin
				      c := get_char;
				      if c='}' then begin
                                        return(EXTENSION);
                                        exit;
                                      end else unget_char(c)
				    end;
                              #0 : begin
                                     commenteof;
                                     exit;
                                   end;
			    end;
			  until false;
                        end;
"#"[ \t]*"include"      begin
                          take_until_eol;
                        end;

[ \t\n\f]               ;
.                       begin
                           writeln(erroutput, 'Illegal character in line ',yylineno, ' col ', yycolno);
                           writeln(erroutput, '"',yyline,'"');
                           return(ILLEGAL);
                        end;
%%

// end.





