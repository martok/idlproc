unit IDLParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pnode, FileUtil;


type
  TIncludeFailAction = (faIgnore, faFatal);
  TIDLParser = class
  private
    fIncludeFailure: TIncludeFailAction;
  public
    constructor Create;
    destructor Destroy; override;
    function ParseFile(const aFileName: string): TPNode;
    function PrepareParsedNode(const aNode: TPNode): boolean;

    property IncludeFailure: TIncludeFailAction read fIncludeFailure write fIncludeFailure;
  end;

implementation

uses
  parser, LexLib;

{ TIDLParser }

constructor TIDLParser.Create;
begin
  inherited Create;
  fIncludeFailure:= faIgnore;
end;

destructor TIDLParser.Destroy;
begin
  inherited Destroy;
end;

function yinc_expand(fn: AnsiString): AnsiString;
var
  bp: String;
begin
  fn:= AnsiDequotedStr(Trim(fn), '"');
  bp:= ExtractFilePath(yyfilename);
  Result:= SearchFileInPath(fn, bp, '', PathSeparator, []);
end;

function TIDLParser.ParseFile(const aFileName: string): TPNode;
var
  xn: string;
  node: TPNode;
begin
  Result:= nil;
  xn:= ExpandFileName(aFileName);
  AssignFile(LexLib.yyinput, xn);
  try
    Reset(LexLib.yyinput);
    parser.yyfilename:= xn;
    parser.yinclude_expand:= @yinc_expand;
    yyparseresult:= nil;
    if (parser.yyparse = 0) and Assigned(yyparseresult) then begin
      node:= yyparseresult;
      if PrepareParsedNode(node) then
        Result:= node;
    end;
  finally
    { lexer closes file }
  end;
end;

function TIDLParser.PrepareParsedNode(const aNode: TPNode): boolean;

  function ProcessRecursive(var n: TPNode): boolean;
  var
    i: integer;   
    n2: TPNode;
  begin
    Result:= true;
    case n.nTyp of
      ntExtension: begin
        n.ExtCode:= TStringList.Create;
        n.ExtCode.Text:= n.Name;
        n.Name:= n.ExtCode[0];
        n.ExtCode.Delete(0);
      end;
    end;
    // Rewrite Attributes for easier access
    if Assigned(n.Attribs) then begin
      n.Attributes:= TStringList.Create;
      n.Attributes.CaseSensitive:= false;
      for n2 in n.Attribs.Children do begin
        if Assigned(n2.Value) then
          n.Attributes.AddObject(n2.Name + n.Attributes.NameValueSeparator + n2.Value.Name, n2.Value)
        else
          n.Attributes.Add(n2.Name);
      end;
      n.Attribs:= nil;
    end;

    if Assigned(n.Children) then begin
      i:= 0;
      while i < n.Children.Count do begin
        n2:= n.Children[i];
        if not ProcessRecursive(n2) then
          Exit(false);
        // allow nodes to replace themselves
        if Assigned(n2) then begin
          n.Children[i]:= n2;
          inc(i);
        end
        else
          n.Children.Delete(i);
      end;
    end;
  end;

var
  tn: TPNode;
begin
  tn:= aNode;
  Result:= ProcessRecursive(tn);
end;

end.

