unit Laz6502;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, lib6502;

type

  { T6502 }

  T6502 = class(TComponent)
  private
    F6502: TM6502;
    FRunning: Boolean;
    FOnCycle: TNotifyEvent;
    function GetIRQ: word;
    function GetNMI: word;
    function GetOnCall: T6502Event;
    function GetOnRead: T6502Event;
    function GetOnWrite: T6502Event;
    function GetRegA: byte;
    function GetRegP: byte;
    function GetRegPC: word;
    function GetRegS: byte;
    function GetRegX: byte;
    function GetRegY: byte;
    function GetRST: word;
    procedure SetIRQ(AValue: word);
    procedure SetNMI(AValue: word);
    procedure SetOnCall(AValue: T6502Event);
    procedure SetOnRead(AValue: T6502Event);
    procedure SetOnWrite(AValue: T6502Event);
    procedure SetRegA(AValue: byte);
    procedure SetRegP(AValue: byte);
    procedure SetRegPC(AValue: word);
    procedure SetRegS(AValue: byte);
    procedure SetRegX(AValue: byte);
    procedure SetRegY(AValue: byte);
    procedure SetRST(AValue: word);
  protected

  public
    property cpu: TM6502 read F6502;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
    procedure Run;
    procedure Tick;
    procedure Step;
  published
    property Running: Boolean read FRunning write FRunning;
    property OnRead: T6502Event read GetOnRead write SetOnRead;
    property OnWrite: T6502Event read GetOnWrite write SetOnWrite;
    property OnCall: T6502Event read GetOnCall write SetOnCall;
    property OnCycle: TNotifyEvent read FOnCycle write FOnCycle;
    property RegA: byte read GetRegA write SetRegA;
    property RegX: byte read GetRegX write SetRegX;
    property RegY: byte read GetRegY write SetRegY;
    property RegP: byte read GetRegP write SetRegP;
    property RegS: byte read GetRegS write SetRegS;
    property RegPC: word read GetRegPC write SetRegPC;
    property VectorNMI: word read GetNMI write SetNMI;
    property VectorRST: word read GetRST write SetRST;
    property VectorIRQ: word read GetIRQ write SetIRQ;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I Laz6502_icon.lrs }
  RegisterComponents('klib',[T6502]);
end;

{ T6502 }

function T6502.GetOnCall: T6502Event;
begin
  Result:=F6502.OnCall;
end;

function T6502.GetIRQ: word;
begin
  Result:=F6502.GetVector(VEC_IRQ);
end;

function T6502.GetNMI: word;
begin
  Result:=F6502.GetVector(VEC_NMI);
end;

function T6502.GetOnRead: T6502Event;
begin
  Result:=F6502.OnRead;
end;

function T6502.GetOnWrite: T6502Event;
begin
  Result:=F6502.OnWrite;
end;

function T6502.GetRegA: byte;
begin
  Result:=F6502.Registers^.a;
end;

function T6502.GetRegP: byte;
begin
  Result:=F6502.Registers^.p;
end;

function T6502.GetRegPC: word;
begin
  Result:=F6502.Registers^.pc;
end;

function T6502.GetRegS: byte;
begin
  Result:=F6502.Registers^.s;
end;

function T6502.GetRegX: byte;
begin
  Result:=F6502.Registers^.x;
end;

function T6502.GetRegY: byte;
begin
  Result:=F6502.Registers^.y;
end;

function T6502.GetRST: word;
begin
  Result:=F6502.GetVector(VEC_RST);
end;

procedure T6502.SetIRQ(AValue: word);
begin
  F6502.SetVector(VEC_IRQ, AValue);
end;

procedure T6502.SetNMI(AValue: word);
begin
  F6502.SetVector(VEC_NMI, AValue);
end;

procedure T6502.SetOnCall(AValue: T6502Event);
begin
  F6502.OnCall:=AValue;
end;

procedure T6502.SetOnRead(AValue: T6502Event);
begin
  F6502.OnRead:=AValue;
end;

procedure T6502.SetOnWrite(AValue: T6502Event);
begin
  F6502.OnWrite:=AValue;
end;

procedure T6502.SetRegA(AValue: byte);
begin
  F6502.Registers^.a:=AValue;
end;

procedure T6502.SetRegP(AValue: byte);
begin
  F6502.Registers^.p:=AValue;
end;

procedure T6502.SetRegPC(AValue: word);
begin
  F6502.Registers^.pc:=AValue;
end;

procedure T6502.SetRegS(AValue: byte);
begin
  F6502.Registers^.s:=AValue;
end;

procedure T6502.SetRegX(AValue: byte);
begin
  F6502.Registers^.x:=AValue;
end;

procedure T6502.SetRegY(AValue: byte);
begin
  F6502.Registers^.y:=AValue;
end;

procedure T6502.SetRST(AValue: word);
begin
  F6502.SetVector(VEC_RST, AValue);
end;

constructor T6502.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRunning:=False;
  FOnCycle:=Nil;
  F6502:=TM6502.Create;
end;

destructor T6502.Destroy;
begin
  F6502.Free;
  inherited Destroy;
end;

procedure T6502.Reset;
begin
  F6502.Reset;
end;

procedure T6502.Run;
begin
  FRunning:=True;
  repeat
    F6502.Step;
    if Assigned(FOnCycle) then
      FOnCycle(Self);
    Application.ProcessMessages;
  until not FRunning;
end;

procedure T6502.Tick;
begin
  F6502.Tick;
end;

procedure T6502.Step;
begin
  F6502.Step;
end;

end.
