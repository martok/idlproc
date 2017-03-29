unit ConsumerPascal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ConsumerBase, dateutils, pnode;

type
  TConsumerPascal = class(TIDLConsumer)
  private
    fCurrentBlock: (blRoot, blType, blConst);
    procedure StartType;
    procedure StartConst;
    procedure EndBlock;
    function ConvertMethod(p: TPNode): string;
  protected
    procedure Produce; override;

    procedure ProcessNode(p: TPNode);
    function ConvertType(t: AnsiString; PointerChar: Char = 'P'): string;
    function ConvertParameter(p: TPNode): string;
    function ConvertImmediate(p: TPNode): string;
    procedure EmitBuiltinTypes;
  public
    class function Name: string; override;
    class function DefaultExt: string; override;
  end;

implementation

{ TConsumerPascal }

class function TConsumerPascal.Name: string;
begin
  Result:= 'Pascal';
end;

class function TConsumerPascal.DefaultExt: string;
begin
  Result:= '.pas';
end;

procedure TConsumerPascal.Produce;
var
  i: Integer;
begin
  Print('{ GENERATED FILE, DO NOT EDIT! [Created %sZ] }', [FormatDateTime('YYYY-MM-DD HH:NN:SS',LocalTimeToUniversal(Now))]);
  Print('unit %s;', [ChangeFileExt(ExtractFileName(OutName),'')]);
  Print('');
  Print('{$mode objfpc}{$H+}');   
  Print('{$calling stdcall}');
  Print('');
  Print('interface');
  Print('');
  fCurrentBlock:= blRoot;

  EmitBuiltinTypes;

  for i:= 0 to Root.Children.Count - 1 do begin
    ProcessNode(Root.Children[i]);
  end;
  EndBlock;
  Print('implementation');
  Print('');              
  Print('end.');
end;

procedure TConsumerPascal.ProcessNode(p: TPNode);
var
  es: boolean;
  s, t: String;
  ss: array of string;
  c: TPNode;
  l: integer;
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
      Print('{ Typelib: %s }', [p.Name]);
      for c in p.Children do
        ProcessNode(c);
    end;
    ntInterface: begin
      StartType;
      if p.Attributes.IndexOf('forward') >= 0 then begin
        Print('%s = interface;',[p.Name]);
      end else begin
        s:= '';
        SetLength(ss, 0);
        if Assigned(p.Parent) then begin
          for c in p.Parent.Children do begin
            SetLength(ss, length(ss)+1);
            ss[high(ss)]:= c.Name;
          end;
          s:= '(' + ''.Join(', ', ss) + ')';
        end;
        Print('%s = interface%s',[p.Name,s]);
        IndentMore;
        if p.Attributes.IndexOfName('uuid') < 0 then
          Fatal('Interface %s has no uuid attribute.',[p.Name]);
        Print('[%s]', [ConvertImmediate(p.AttrValue('uuid'))]);
        for c in p.Children do
          ProcessNode(c);
        IndentLess;
        Print('end;');
        Print('');
      end;
    end;
    ntConst: begin
      StartConst;
      s:= ConvertImmediate(p.Value);
      if Assigned(p.Typ) then begin
        t:= ConvertType(p.typ.Name);
        if Assigned(p.Attributes) and (p.Attributes.IndexOf('static_cast')>=0) then
          s:= Format('%s(%s)', [t, s]);
        Print('%-30s : %s = %s;', [p.Name, t, s]);
      end else begin
        Print('%-30s = %s;', [p.Name, s]);
      end;
    end;
    ntMethod: begin
      Print(ConvertMethod(p)+';');
    end;
    ntProperty: begin
      s:= ConvertType(p.Typ.Name);
      Print('function Get%s: %s;',[p.name, s]);
      if not p.ReadOnly then begin
        Print('procedure Set%s(Value: %s);',[p.name, s]);
      end;                                             
      if p.ReadOnly then
        Print('property %s: %s read Get%0:s;',[p.name, s])
      else
        Print('property %s: %s read Get%0:s write Set%0:s;',[p.name, s]);
    end;
    ntModule: begin
      Print('{ Import Section %s}', [p.Name]);
      if p.Attributes.IndexOfName('dllname')>= 0 then begin
        StartConst;
        s:= UpperCase(p.Name)+'_LIBNAME';
        Print('%s = %s;', [s, QuotedStr(p.Attributes.Values['dllname'])]);
      end;
      for c in p.Children do
        if c.nTyp = ntConst then
          ProcessNode(c);
      for c in p.Children do
        if c.nTyp = ntMethod then
          Print('%s; external %s name %s;',[ConvertMethod(c), s, QuotedStr(c.Name)]);
      Print('');
    end;
    ntTypeAlias: begin
      StartType;
      if p.Typ.nTyp = ntMethod then
        Print('%s = %s;', [p.Name, ConvertMethod(p.Typ)])
      else
        Print('%s = type %s;', [p.Name, ConvertType(p.Typ.Name, '^')]);
    end;
    ntEnum: begin
      StartType;
      es:= Assigned(p.Attributes) and (p.Attributes.Values['enumsize']>'');
      if es then
        Print('{$MINENUMSIZE %s}', [p.Attributes.Values['enumsize']]);
      l:= 0;
      for c in p.Children do begin
        if l < Length(c.Name) then
          l:= Length(c.Name);
      end;
      inc(l, 4);
      Print('%s = (', [p.Name]);
      IndentMore;
      s:= ',';
      for c in p.Children do begin
        if c = p.Children.Last then
          s:= '';
        if Assigned(c.Value) then
          Print('%-*s = %s%s',[l,c.Name, ConvertImmediate(c.Value), s])
        else
          Print('%s%s',[c.Name, s]);
      end;
      IndentLess;
      Print(');');
      if es then  
        Print('{$MINENUMSIZE DEFAULT}');
    end;
    ntStruct: begin
      StartType; 
      Print('%s = record',[p.Name,s]);
      IndentMore;
      for c in p.Children do
        ProcessNode(c);
      IndentLess;
      Print('end;');
      Print('');
    end;
    ntField: begin
      Print('%s: %s;',[p.Name, ConvertType(p.Typ.Name)]);
    end;
  else
    WriteLn('Unprocessed Node Type: ',p.nTyp);
  end;
end;

procedure TConsumerPascal.EmitBuiltinTypes;
var
  pn: TPNode;
begin
  pn:= TPNode.Create(ntTypeAlias);
  pn.Name:= 'PUTF8Char';
  pn.Typ:= TPNode.Create(ntIdentifier);
  pn.Typ.Name:= 'CString';
  ProcessNode(pn);
end;

procedure TConsumerPascal.StartType;
begin
  if fCurrentBlock <> blType then begin
    EndBlock;
    Print('type');
    fCurrentBlock:= blType;
    IndentMore;
  end;
end;

procedure TConsumerPascal.StartConst;
begin
  if fCurrentBlock <> blConst then begin
    EndBlock;
    Print('const');
    fCurrentBlock:= blConst;
    IndentMore;
  end;
end;

procedure TConsumerPascal.EndBlock;
begin
  if fCurrentBlock > blRoot then
    IndentLess;
  Print('');
  fCurrentBlock:= blRoot;
end;

function TConsumerPascal.ConvertType(t: AnsiString; PointerChar: Char): string;
var
  arr,a: string;
  ptrs: integer;
  p: integer;
begin
  t:= Trim(t);
  arr:= '';
  while AnsiLastChar(t) = ']' do begin
    p:= LastDelimiter('[',t);
    a:= Copy(t, p+1, Length(t)-p-1);
    arr:= Format('array[0..%s-1] of ',[a]) + arr;
    Delete(t, p, MaxInt);
    t:= TrimRight(t);
  end;
  ptrs:= 0;
  while AnsiLastChar(t) = '*' do begin
    Inc(ptrs);
    SetLength(t, Length(t) - 1);
    t:= TrimRight(t);
  end;
  case t of
    'Boolean': Result:= 'Boolean';
    'UInt8': Result:= 'Byte';
    'UInt16': Result:= 'Word';
    'UInt32': Result:= 'Cardinal';
    'UInt64': Result:= 'QWord';
    'Int8': Result:= 'Shortint';
    'Int16': Result:= 'Smallint';
    'Int32': Result:= 'Integer';
    'Int64': Result:= 'Int64';
    'Pointer': Result:= 'Pointer';
    'PtrInt': Result:= 'PtrInt';
    'PtrUInt': Result:= 'PtrUInt';
    'CString': Result:= 'PAnsiChar';
    'CUTF8String': Result:= 'PUTF8Char';
    'CWString': Result:= 'PUnicodeChar';
    'IID': Result:= 'TGuid';
  else
    Result:= t;
  end;
  Result:= StringOfChar(PointerChar, ptrs) + Result;
  Result:= arr + Result;
end;

function TConsumerPascal.ConvertParameter(p: TPNode): string;
begin
  case p.Inoutspec of             
    PARAM_DEFAULT:
      if Assigned(p.Attributes) and (p.Attributes.IndexOf('const')>=0) then
        Result:= 'const '
      else
        Result:= '';
    PARAM_IN:
      if Assigned(p.Attributes) and (p.Attributes.IndexOf('const')>=0) then
        Result:= 'constref '
      else
        Result:= '';
    PARAM_OUT: Result:= 'out ';
    PARAM_INOUT: Result:= 'var ';
  end;
  Result += p.Name;
  Result += ': ';
  Result += ConvertType(p.Typ.Name);
end;

function TConsumerPascal.ConvertMethod(p: TPNode): string;
var
  ss: TStringArray;
  c: TPNode;
  s: string;
begin
  SetLength(ss, 0);
  for c in p.Children do begin
    SetLength(ss, length(ss)+1);
    ss[high(ss)]:= ConvertParameter(c);
  end;
  if Length(ss) > 0 then
    s:= '('+''.Join('; ', ss)+')'
  else
    s:= '';
  if p.Typ.Name <> 'void' then
    Result:= Format('function %s%s: %s', [p.name, s, ConvertType(p.Typ.Name)])
  else
    Result:= Format('procedure %s%s', [p.name, s]);
end;

function TConsumerPascal.ConvertImmediate(p: TPNode): string;
begin
  case p.nTyp of
    ntValueIID: Result:= QuotedStr(p.GUID.ToString(false));
    ntValueRef: Result:= p.Name;
    ntValueNumber: Result:= p.Name;
    ntValueStr: Result:= QuotedStr(p.Name);
  end;
end;

end.

