program idl2pas;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, parser, lexlib, YaccLib, pnode, IDLParser, ConsumerBase,
  ConsumerPascal
  { you can add units after this };

var
  ff: string;
  idlp: TIDLParser;
  pr: TPNode;
  pp: TIDLConsumer;
begin
  ff:= '..\tests\nsIVariant.idl';
  idlp:= TIDLParser.Create;
  try
    pr:= idlp.ParseFile(ff);
    pp:= TConsumerPascal.Create(pr, ff);
    try
      pp.Execute(ChangeFileExt(ff, '.pas'));
    finally
      FreeAndNil(pp);
    end;
  finally
    FreeAndNil(idlp);
  end;

end.

