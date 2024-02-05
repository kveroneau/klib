unit ScriptSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TOutputEvent = procedure(data: string) of object;
  TInputEvent = function: string of object;
  TRunFileEvent = procedure(AFile: string) of object;
  TOpCodeEvent = procedure(op: char) of object;

  { TScriptSystem }

  TScriptSystem = class(TComponent)
  private
    FOnInput: TInputEvent;
    FOnOpCode: TOpCodeEvent;
    FOnOutput: TOutputEvent;
    FOnRunFile: TRunFileEvent;
    FVar: string;
    FPC, FRT: integer;
    FRunning: boolean;
    FData: string;
    procedure SetRunning(AValue: boolean);
    procedure SetScript(AValue: string);
  protected

  public
    function GetString: string;
    function GetInt: integer;
    procedure Step;
    procedure Run;
  published
    property Script: string read FData write SetScript ;
    property SVar: string read FVar write FVar;
    property PC: integer read FPC;
    property ReturnCode: integer read FRT;
    property Running: boolean read FRunning write SetRunning;
    property OnOutput: TOutputEvent read FOnOutput write FOnOutput;
    property OnInput: TInputEvent read FOnInput write FOnInput;
    property OnRunFile: TRunFileEvent read FOnRunFile write FOnRunFile;
    property OnOpCode: TOpCodeEvent read FOnOpCode write FOnOpCode;
  end;

implementation

{ TScriptSystem }

procedure TScriptSystem.SetRunning(AValue: boolean);
begin
  if FRunning=AValue then Exit;
  FRunning:=AValue;
end;

procedure TScriptSystem.SetScript(AValue: string);
begin
  FData:=AValue;
  FPC:=1;
  FRT:=0;
  FVar:='';
  FRunning:=True;
end;

function TScriptSystem.GetString: string;
var
  ch: char;
begin
  Result:='';
  repeat
    ch:=FData[FPC];
    if ch <> '~' then
      Result:=Result+ch;
    Inc(FPC);
    if FPC > Length(FData) then
      Exit;
  until ch = '~';
end;

function TScriptSystem.GetInt: integer;
begin
  Result:=StrToInt(GetString);
end;

procedure TScriptSystem.Step;
var
  op: char;
begin
  op:=FData[FPC];
  Inc(FPC);
  case op of
    'P': FOnOutput(GetString);
    'I': FVar:=FOnInput();
    'R': FOnRunFile(GetString);
    '!': FRT:=GetInt;
    'X': FRunning:=False;
  else
    if Assigned(FOnOpCode) then
      FOnOpCode(op)
    else
      FRunning:=False;
  end;
end;

procedure TScriptSystem.Run;
begin
  FPC:=1;
  FRT:=0;
  repeat
    Step;
    if FPC > Length(FData) then
      FRunning:=False;
  until not FRunning;
end;

end.
