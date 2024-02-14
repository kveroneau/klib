unit MOS6502;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, lib6502, Dev6502, Memory6502, Storage6502;

type

  { TMOS6502 }

  TMOS6502 = class(TComponent)
  private
    FActive: Boolean;
    FMemory: TBytesStream;
    FMemoryClass: T6502Memory;
    FStorage: T6502Storage;
    F6502: PM6502;
    FResetVector: word;
    FRunning, FThreaded: Boolean;
    FDevice: T6502Device;
    FOnActivate: TNotifyEvent;
    function GetRegA: byte;
    function GetRegAX: word;
    function GetRegP: TCPUFlags;
    function GetRegPC: word;
    function GetRegS: byte;
    function GetRegX: byte;
    function GetRegY: byte;
    procedure SetActive(AValue: Boolean);
    procedure SetRegA(AValue: byte);
    procedure SetRegAX(AValue: word);
    procedure SetRegP(AValue: TCPUFlags);
    procedure SetRegPC(AValue: word);
    procedure SetRegS(AValue: byte);
    procedure SetRegX(AValue: byte);
    procedure SetRegY(AValue: byte);
    procedure SetResetVector(AValue: word);
    procedure SetRunning(AValue: Boolean);
  protected

  public
    property Threaded: Boolean read FThreaded write FThreaded;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadInto(strm: TStream; addr: word);
    procedure Step;
  published
    property Active: Boolean read FActive write SetActive;
    property Running: Boolean read FRunning write SetRunning;
    property Device: T6502Device read FDevice write FDevice;
    property Memory: T6502Memory read FMemoryClass write FMemoryClass;
    property Storage: T6502Storage read FStorage write FStorage;
    property ResetVector: word read FResetVector write SetResetVector;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property RegA: byte read GetRegA write SetRegA;
    property RegX: byte read GetRegX write SetRegX;
    property RegY: byte read GetRegY write SetRegY;
    property RegAX: word read GetRegAX write SetRegAX;
    property RegP: TCPUFlags read GetRegP write SetRegP;
    property RegS: byte read GetRegS write SetRegS;
    property RegPC: word read GetRegPC write SetRegPC;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MOS 6502',[TMOS6502]);
end;

{ TMOS6502 }

procedure TMOS6502.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  if AValue then
  begin
    if not Assigned(FMemoryClass) then
      Exit;
    FMemory:=FMemoryClass.Memory;
    F6502:=M6502_new(Nil, FMemory.Bytes, Nil);
    FDevice.Memory:=FMemory;
    M6502_setVector(F6502, VEC_RST, FResetVector);
    if Assigned(FOnActivate) then
      FOnActivate(Self);
    M6502_reset(F6502);
  end
  else
  begin
    FRunning:=False;
    FDevice.Memory:=Nil;
    M6502_delete(F6502);
    F6502:=Nil;
    FMemory:=Nil;
    FMemoryClass.Active:=False;
  end;
  FActive:=AValue;
end;

function TMOS6502.GetRegA: byte;
begin
  if FActive then
    Result:=F6502^.registers^.a
  else
    Result:=0;
end;

function TMOS6502.GetRegAX: word;
begin
  if FActive then
    Result:=(RegX shl 8)+RegA
  else
    Result:=0;
end;

function TMOS6502.GetRegP: TCPUFlags;
begin
  if FActive then
    Result:=F6502^.registers^.p
  else
    Result:=[];
end;

function TMOS6502.GetRegPC: word;
begin
  if FActive then
    Result:=F6502^.registers^.pc
  else
    Result:=0;
end;

function TMOS6502.GetRegS: byte;
begin
  if FActive then
    Result:=F6502^.registers^.s
  else
    Result:=0;
end;

function TMOS6502.GetRegX: byte;
begin
  if FActive then
    Result:=F6502^.registers^.x
  else
    Result:=0;
end;

function TMOS6502.GetRegY: byte;
begin
  if FActive then
    Result:=F6502^.registers^.y
  else
    Result:=0;
end;

procedure TMOS6502.SetRegA(AValue: byte);
begin
  if FActive then
    F6502^.registers^.a:=AValue;
end;

procedure TMOS6502.SetRegAX(AValue: word);
begin
  RegA:=AValue and $ff;
  RegX:=AValue shr 8;
end;

procedure TMOS6502.SetRegP(AValue: TCPUFlags);
begin
  if FActive then
    F6502^.registers^.p:=AValue;
end;

procedure TMOS6502.SetRegPC(AValue: word);
begin
  if FActive then
    F6502^.registers^.pc:=AValue;
end;

procedure TMOS6502.SetRegS(AValue: byte);
begin
  if FActive then
    F6502^.registers^.s:=AValue;
end;

procedure TMOS6502.SetRegX(AValue: byte);
begin
  if FActive then
    F6502^.registers^.x:=AValue;
end;

procedure TMOS6502.SetRegY(AValue: byte);
begin
  if FActive then
    F6502^.registers^.y:=AValue;
end;

procedure TMOS6502.SetResetVector(AValue: word);
begin
  if FResetVector=AValue then Exit;
  if FActive then
    M6502_setVector(F6502, VEC_RST, AValue);
  FResetVector:=AValue;
end;

procedure TMOS6502.SetRunning(AValue: Boolean);
begin
  if FRunning=AValue then Exit;
  if not FActive then
    Exit;
  FRunning:=AValue;
end;

constructor TMOS6502.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMemory:=Nil;
  F6502:=Nil;
  FRunning:=False;
  FActive:=False;
end;

destructor TMOS6502.Destroy;
begin
  if Assigned(F6502) then
    M6502_delete(F6502);
  if Assigned(FMemory) then
    FMemory.Free;
  FRunning:=False;
  inherited Destroy;
end;

procedure TMOS6502.LoadInto(strm: TStream; addr: word);
begin
  strm.Read(FMemory.Bytes[addr], strm.Size);
end;

procedure TMOS6502.Step;
begin
  if FRunning then
  begin
    M6502_step(F6502);
    if not FThreaded and Assigned(FDevice) then
      FDevice.DeviceRun;
  end;
end;

end.
