program idlproc;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, parser, lexlib, YaccLib, pnode, IDLParser, ConsumerBase,
  ConsumerPascal, uGetOpt, getopts
  { you can add units after this };


const
  OptionsLong: array[1..1] of TOption = (
    (Name: 'dependencies'; Has_Arg: 0; Flag: nil; Value: 'D')
  );
  OptionShort = 'D';

var
  Consumer: TIDLConsumer = nil;
  FNameIn: string = '';
  FNameOut: string = '';
  DoDependencies: boolean = false;

procedure ProcessOption(const opt: string; const OptArg: string);
begin
  case opt of
    'D': DoDependencies:= true;
  else
    WriteLn(ErrOutput, 'Unknown option: ', opt);
  end;
end;

var
  cons: string;
  lastopt, workf: Integer;
  Dependencies: TStringList;

  procedure TranslateFile(fn: string);
  var
    idlp: TIDLParser;
    pr: TPNode;
    fo: string;
  begin
    if not FileExists(fn) then begin
      WriteLn('File not found: ', fn);
      halt(1);
    end;
    idlp:= TIDLParser.Create;
    try
      pr:= idlp.ParseFile(fn);
      if not Assigned(pr) then begin
        WriteLn('Failed to parse file: ', fn);
        halt(1);
      end;
      Consumer.Setup(pr, fn);
      if LastDelimiter('/\', FNameOut) = Length(FNameOut) then
        fo:= FNameOut + ChangeFileExt(ExtractFileName(fn), Consumer.DefaultExt)
      else if FNameOut = '-' then
        fo:= ''
      else
        fo:= FNameOut;
      Consumer.Execute(fo);
    finally
      FreeAndNil(idlp);
    end;
  end;

  procedure ConsumerDependency(Sender: TObject; depfile: string);
  var
    d: string;
  begin
    d:= ExpandFileName(ExtractFilePath(TIDLConsumer(Sender).RootFile) + depfile);
    Dependencies.Add(d);
  end;

begin
  if ParamCount < 2 then begin
    WriteLn('Invalid arguments, must have at least language and first IDL file.');
    halt(1);
  end;

  cons:= ParamStr(1);
  if SameText(TConsumerPascal.Name, cons) then
    Consumer:= TConsumerPascal.Create
  else
    begin
      WriteLn('Invalid consumer language: ',cons);
      halt(1);
    end;

  FNameIn:= '';
  FNameOut:= IncludeTrailingPathDelimiter(GetCurrentDir);

  OptInd:= 2;
  lastopt:= HandleAllOptions(OptionShort, @OptionsLong[1], @ProcessOption);

  if lastopt <= ParamCount then begin
    FNameIn:= ParamStr(lastopt);
    inc(lastopt);
    if lastopt <= ParamCount then
      FNameOut:= ParamStr(lastopt);
  end;

  Dependencies:= TStringList.Create;
  try
    if DoDependencies then
      Consumer.OnDependency:= @ConsumerDependency;

    Dependencies.Add(FNameIn);

    workf:= 0;
    while workf < Dependencies.Count do begin
      TranslateFile(Dependencies[workf]);
      inc(workf);
    end;
  finally
    FreeAndNil(Dependencies);
  end;
end.

