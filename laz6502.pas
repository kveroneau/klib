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
    FCore: byte;
    FOnCall, FOnRead, FOnWrite: T6502Event;
    FRunning: Boolean;
    FOnCycle: TNotifyEvent;
    function GetCores: byte;
    function GetIRQ: word;
    function GetMemory: TBytes;
    function GetNMI: word;
    function GetRegA: byte;
    function GetRegisters: PM6502_Registers;
    function GetRegP: TCPUFlags;
    function GetRegPC: word;
    function GetRegS: byte;
    function GetRegX: byte;
    function GetRegY: byte;
    function GetRegAX: word;
    function GetRST: word;
    procedure SetCore(AValue: byte);
    procedure SetCores(AValue: byte);
    procedure SetIRQ(AValue: word);
    procedure SetNMI(AValue: word);
    procedure SetOnCall(AValue: T6502Event);
    procedure SetOnRead(AValue: T6502Event);
    procedure SetOnWrite(AValue: T6502Event);
    procedure SetRegA(AValue: byte);
    procedure SetRegP(AValue: TCPUFlags);
    procedure SetRegPC(AValue: word);
    procedure SetRegS(AValue: byte);
    procedure SetRegX(AValue: byte);
    procedure SetRegY(AValue: byte);
    procedure SetRegAX(AValue: word);
    procedure SetRST(AValue: word);
    procedure SetRunning(AValue: Boolean);
  protected

  public
    property Cores: byte read GetCores write SetCores;
    property Core: byte read FCore write SetCore;
    property cpu: TM6502 read F6502;
    property Memory: TBytes read GetMemory;
    property Registers: PM6502_Registers read GetRegisters;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddEvent(typ: M6502_CallbackType; addr: word);
    procedure SetVector(vec, addr: word);
    function GetVector(vec: word): word;
    function PopStack: byte;
    procedure PushStack(b: byte);
    function RTS: word;
    function RTI: word;
    procedure Reset;
    procedure Run;
    procedure Tick;
    procedure Step;
    procedure NMI;
    procedure IRQ;
    procedure ResetMemory;
  published
    property Running: Boolean read FRunning write SetRunning;
    property OnRead: T6502Event read FOnRead write SetOnRead;
    property OnWrite: T6502Event read FOnWrite write SetOnWrite;
    property OnCall: T6502Event read FOnCall write SetOnCall;
    property OnCycle: TNotifyEvent read FOnCycle write FOnCycle;
    property RegA: byte read GetRegA write SetRegA;
    property RegX: byte read GetRegX write SetRegX;
    property RegY: byte read GetRegY write SetRegY;
    property RegAX: word read GetRegAX write SetRegAX;
    property RegP: TCPUFlags read GetRegP write SetRegP;
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

function T6502.GetIRQ: word;
begin
  Result:=F6502.GetVector(FCore, VEC_IRQ);
end;

function T6502.GetCores: byte;
begin
  Result:=F6502.Cores;
end;

function T6502.GetMemory: TBytes;
begin
  Result:=F6502.Memory.Bytes;
end;

function T6502.GetNMI: word;
begin
  Result:=F6502.GetVector(FCore, VEC_NMI);
end;

function T6502.GetRegA: byte;
begin
  Result:=F6502.Registers[FCore]^.a;
end;

function T6502.GetRegisters: PM6502_Registers;
begin
  Result:=F6502.Registers[FCore];
end;

function T6502.GetRegP: TCPUFlags;
begin
  Result:=F6502.Registers[FCore]^.p;
end;

function T6502.GetRegPC: word;
begin
  Result:=F6502.Registers[FCore]^.pc;
end;

function T6502.GetRegS: byte;
begin
  Result:=F6502.Registers[FCore]^.s;
end;

function T6502.GetRegX: byte;
begin
  Result:=F6502.Registers[FCore]^.x;
end;

function T6502.GetRegY: byte;
begin
  Result:=F6502.Registers[FCore]^.y;
end;

function T6502.GetRegAX: word;
begin
  Result:=(RegX shl 8)+RegA;
end;

function T6502.GetRST: word;
begin
  Result:=F6502.GetVector(FCore, VEC_RST);
end;

procedure T6502.SetCore(AValue: byte);
begin
  if FCore=AValue then Exit;
  if AValue > F6502.Cores-1 then Exit;
  FCore:=AValue;
end;

procedure T6502.SetCores(AValue: byte);
begin
  F6502.Cores:=AValue;
end;

procedure T6502.SetIRQ(AValue: word);
begin
  F6502.SetVector(FCore, VEC_IRQ, AValue);
end;

procedure T6502.SetNMI(AValue: word);
begin
  F6502.SetVector(FCore, VEC_NMI, AValue);
end;

procedure T6502.SetOnCall(AValue: T6502Event);
begin
  F6502.OnCall[FCore]:=AValue;
  FOnCall:=AValue;
end;

procedure T6502.SetOnRead(AValue: T6502Event);
begin
  F6502.OnRead[FCore]:=AValue;
  FOnRead:=AValue;
end;

procedure T6502.SetOnWrite(AValue: T6502Event);
begin
  F6502.OnWrite[FCore]:=AValue;
  FOnWrite:=AValue;
end;

procedure T6502.SetRegA(AValue: byte);
begin
  F6502.Registers[FCore]^.a:=AValue;
end;

procedure T6502.SetRegP(AValue: TCPUFlags);
begin
  F6502.Registers[FCore]^.p:=AValue;
end;

procedure T6502.SetRegPC(AValue: word);
begin
  F6502.Registers[FCore]^.pc:=AValue;
end;

procedure T6502.SetRegS(AValue: byte);
begin
  F6502.Registers[FCore]^.s:=AValue;
end;

procedure T6502.SetRegX(AValue: byte);
begin
  F6502.Registers[FCore]^.x:=AValue;
end;

procedure T6502.SetRegY(AValue: byte);
begin
  F6502.Registers[FCore]^.y:=AValue;
end;

procedure T6502.SetRegAX(AValue: word);
begin
  RegA:=AValue and $ff;
  RegX:=AValue shr 8;
end;

procedure T6502.SetRST(AValue: word);
begin
  F6502.SetVector(FCore, VEC_RST, AValue);
end;

procedure T6502.SetRunning(AValue: Boolean);
begin
  F6502.Running[FCore]:=AValue;
  FRunning:=AValue;
end;

constructor T6502.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRunning:=False;
  FCore:=0;
  FOnCycle:=Nil;
  F6502:=GetMOS6502;
  FCore:=F6502.NewCore;
end;

destructor T6502.Destroy;
begin
  inherited Destroy;
end;

procedure T6502.AddEvent(typ: M6502_CallbackType; addr: word);
begin
  F6502.AddEvent(FCore, typ, addr);
end;

procedure T6502.SetVector(vec, addr: word);
begin
  F6502.SetVector(FCore, vec, addr);
end;

function T6502.GetVector(vec: word): word;
begin
  Result:=F6502.GetVector(FCore, vec);
end;

function T6502.PopStack: byte;
begin
  Result:=F6502.PopStack(FCore);
end;

procedure T6502.PushStack(b: byte);
begin
  F6502.PushStack(FCore, b);
end;

function T6502.RTS: word;
begin
  Result:=F6502.RTS(FCore);
end;

function T6502.RTI: word;
begin
  Result:=F6502.RTI(FCore);
end;

procedure T6502.Reset;
begin
  F6502.Reset(FCore);
end;

procedure T6502.Run;
begin
  Running:=True;
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

procedure T6502.NMI;
begin
  F6502.NMI(FCore);
end;

procedure T6502.IRQ;
begin
  F6502.IRQ(FCore);
end;

{$DEFINE CL6502}
procedure T6502.ResetMemory;
{$IFNDEF CL6502}
var
  r,w,c: T6502Event;
  x: byte;
{$ENDIF}
begin
  if FRunning then
    Exit;
  {$IFDEF CL6502}
  F6502.ResetMemory;
  {$ELSE}
  FRunning:=False;
  r:=F6502.OnRead[FCore];
  w:=F6502.OnWrite[FCore];
  c:=F6502.OnCall[FCore];
  x:=F6502.Cores;
  Dec(x);
  Inc(x);
  F6502.Cores:=x;
  F6502.OnRead[FCore]:=r;
  F6502.OnWrite[FCore]:=w;
  F6502.OnCall[FCore]:=c;
  {$ENDIF}
end;

end.
