program idl2pas;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, parser, lexlib, YaccLib
  { you can add units after this };

begin
  AssignFile(yyinput, ExtractFilePath(ParamStr(0)) + '..\tests\nsIVariant.idl');
  Reset(yyinput);
  parser.yyparse;
  ReadLn;
end.

