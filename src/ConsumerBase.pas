unit ConsumerBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pnode;

type
  TIDLConsumer = class
  private
    fRoot: TPNode;
    fRootFile: string;
  protected
    OutFile: TextFile;
    OutName: string;
    IndentLevel: integer;
    function GetName: string; virtual; abstract;
    procedure Produce; virtual; abstract;

    procedure IndentMore;
    procedure IndentLess;
    procedure Print(s: string);
    procedure Print(fmt: string; Args: array of const);
  public
    constructor Create(aRoot: TPNode; aRootFile: string);
    destructor Destroy; override;
    property Root: TPNode read fRoot;
    property RootFile: string read fRootFile;
    property Name: string read GetName;

    procedure Execute(aOutFile: string);
  end;



implementation

{ TIDLConsumer }

constructor TIDLConsumer.Create(aRoot: TPNode; aRootFile: string);
begin
  inherited Create;
  fRoot:= aRoot;
  fRootFile:= aRootFile;
end;

destructor TIDLConsumer.Destroy;
begin
  inherited Destroy;
end;

procedure TIDLConsumer.Execute(aOutFile: string);
begin
  AssignFile(OutFile, aOutFile);
  try
    OutName:= aOutFile;
    Rewrite(OutFile);

    IndentLevel:= 0;
    Produce;
  finally
    Close(OutFile);
  end;
end;

procedure TIDLConsumer.IndentMore;
begin
  inc(IndentLevel);
end;

procedure TIDLConsumer.IndentLess;
begin
  Dec(IndentLevel);
end;

procedure TIDLConsumer.Print(s: string);
begin
  WriteLn(OutFile, '':IndentLevel*2, s);
end;

procedure TIDLConsumer.Print(fmt: string; Args: array of const);
begin
  Print(Format(fmt, Args));
end;

end.

