program idl2pas;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, parser, lexlib, YaccLib, pnode, IDLParser
  { you can add units after this };

var
  idlp: TIDLParser;
  pr: TPNode;
begin
  idlp:= TIDLParser.Create;
  try
    pr:= idlp.ParseFile('..\tests\nsIVariant.idl');
    WritePNodeTree(pr);
  finally
    FreeAndNil(idlp);
  end;
  ReadLn;
end.

