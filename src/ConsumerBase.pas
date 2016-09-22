unit ConsumerBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pnode;

type
  TDependencyEvent = procedure (Sender: TObject; depfile: string);

  TIDLConsumerClass = class of TIDLConsumer;
  TIDLConsumer = class
  private
    fRoot: TPNode;
    fRootFile: string;
    fOnDependency: TDependencyEvent;
  protected
    OutFile: TextFile;
    OutName: string;
    IndentLevel: integer;
    procedure Produce; virtual; abstract;

    procedure DoDependency(depfile: string);
    procedure IndentMore;
    procedure IndentLess;
    procedure Print(s: string);
    procedure Print(fmt: string; Args: array of const);
    procedure Fatal(s: string);
    procedure Fatal(fmt: string; Args: array of const);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Setup(aRoot: TPNode; aRootFile: string);
    property Root: TPNode read fRoot;
    property RootFile: string read fRootFile;

    property OnDependency: TDependencyEvent read fOnDependency write fOnDependency;

    procedure Execute(aOutFile: string);
    class function Name: string; virtual; abstract;
    class function DefaultExt: string; virtual; abstract;
  end;



implementation

{ TIDLConsumer }

constructor TIDLConsumer.Create;
begin
  inherited Create;
end;

destructor TIDLConsumer.Destroy;
begin
  inherited Destroy;
end;

procedure TIDLConsumer.Setup(aRoot: TPNode; aRootFile: string);
begin
  fRoot:= aRoot;
  fRootFile:= aRootFile;
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
  s:= TrimRight(s);
  if s > '' then
    WriteLn(OutFile, '':IndentLevel*2, s)
  else
    WriteLn(OutFile, '');
end;

procedure TIDLConsumer.Print(fmt: string; Args: array of const);
begin
  Print(Format(fmt, Args));
end;

procedure TIDLConsumer.Fatal(s: string);
begin
  WriteLn(ErrOutput,'Fatal error generating output from ',fRootFile);
  WriteLn(ErrOutput, '  ', s);
  halt(1);
end;

procedure TIDLConsumer.Fatal(fmt: string; Args: array of const);
begin
  Fatal(Format(fmt, Args));
end;

procedure TIDLConsumer.DoDependency(depfile: string);
begin
  if Assigned(fOnDependency) then
    fOnDependency(Self, depfile);
end;

end.

