unit ConsumerPascal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ConsumerBase, dateutils, pnode;

type
  TConsumerPascal = class(TIDLConsumer)
  private
    fTypeWritten: boolean;
    procedure StartType;
  protected
    function GetName: string; override;
    procedure Produce; override;

    procedure ProcessNode(p: TPNode);
    function ConvertType(t: string): string;
    function ConvertParameter(p: TPNode): string;
  end;

implementation

{ TConsumerPascal }

function TConsumerPascal.GetName: string;
begin
  Result:= 'Pascal';
end;

procedure TConsumerPascal.Produce;
var
  i: Integer;
begin
  Print('{ GENERATED FILE, DO NOT EDIT! [Created %sZ] }', [FormatDateTime('YYYY-MM-DD HH:NN:SS',LocalTimeToUniversal(Now))]);
  Print('unit %s;', [ChangeFileExt(ExtractFileName(OutName),'')]);
  Print('');
  Print('{$mode objfpc}');   
  Print('{$calling stdcall}');
  Print('');
  Print('interface');
  Print('');
  fTypeWritten:= false;

  for i:= 0 to Root.Children.Count - 1 do begin
    ProcessNode(Root.Children[i]);
  end;
  if fTypeWritten then
    IndentLess;
  Print('implementation');
  Print('');              
  Print('end.');
end;

procedure TConsumerPascal.ProcessNode(p: TPNode);
var
  s: String;
  ss: array of string;
  c: TPNode;
begin
  case p.nTyp of
    ntExtension: begin
      if AnsiSameText(p.Name, 'FPC') or AnsiSameText(p.Name, 'pascal') then begin
        for s in p.ExtCode do begin
          Print(s);
        end;
      end;
    end;
    ntLibrary: begin
      StartType;
      Print('{ Typelib: %s }', [p.Name]);
      for c in p.Children do
        ProcessNode(c);
    end;
    ntInterface: begin
      StartType;
      s:= '';
      SetLength(ss, 0);
      if Assigned(p.Parent) then begin
        for c in p.Parent.Children do begin
          SetLength(ss, length(ss)+1);
          ss[high(ss)]:= c.Name;
        end;
        s:= ' (' + ''.Join(', ', ss) + ')';
      end;
      Print('%s = interface%s',[p.Name,s]);
      IndentMore;
      Print('[''{%s}'']', [p.Attributes.Values['uuid']]);
      for c in p.Children do
        ProcessNode(c);
      IndentLess;
      Print('end;');
      Print('');
    end;
    ntConst: begin
      s:= '';
      if Assigned(p.Typ) then
        s:= ': ' + ConvertType(p.typ.Name);
      Print('const %s%s = %s;', [p.Name, s,p.Value.Name]);
    end;      
    ntMethod: begin
      SetLength(ss, 0);
      for c in p.Children do begin
        SetLength(ss, length(ss)+1);
        ss[high(ss)]:= ConvertParameter(c);
      end;
      if Length(ss) > 0 then
        s:= '('+''.Join('; ',ss)+')'
      else
        s:= '';
      if p.Typ.Name <> 'void' then
       Print('function %s%s: %s;',[p.name, s, ConvertType(p.Typ.Name)])
      else
        Print('procedure %s%s;',[p.name, s]);
    end;
    ntProperty: begin
      Print('function Get_%s: %s;',[p.name, ConvertType(p.Typ.Name)]);
      if not p.ReadOnly then begin
        Print('procedure Set_%s(Value: %s);',[p.name, ConvertType(p.Typ.Name)]);
      end;
    end;
  end;
end;

procedure TConsumerPascal.StartType;
begin
  if not fTypeWritten then begin
    Print('type');
    IndentMore;
  end;
  fTypeWritten:= true;
end;

function TConsumerPascal.ConvertType(t: string): string;
begin
  Result:= t;
end;

function TConsumerPascal.ConvertParameter(p: TPNode): string;
begin
  case p.Inoutspec of
    PARAM_IN,
    PARAM_DEFAULT: Result:= '';
    PARAM_OUT: Result:= 'out ';
    PARAM_INOUT: Result:= 'var ';
  end;
  Result += p.Name;
  Result += ': ';
  Result += ConvertType(p.Typ.Name);
end;

end.

