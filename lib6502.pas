unit lib6502;

{$mode objfpc}{$H+}

interface

{$IFNDEF UNIX}
{$IFNDEF LAZARUS_IDE}
{$L lib6502.o}
{$ENDIF}
{$ENDIF}

uses Classes, sysutils;

const
  {$IFDEF WINDOWS}
  lib6502x = '6502.DLL';
  {$ELSE}
  lib6502x = '6502';
  {$ENDIF}
  VEC_NMI = $fffa;
  VEC_RST = $fffc;
  VEC_IRQ = $fffe;

Type
  PM6502  = ^M6502;
  PM6502_Callbacks  = ^M6502_Callbacks;
  PM6502_Registers  = ^M6502_Registers;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  M6502_Callback = function(mpu: PM6502; address: word; data: byte): longint; cdecl;
  M6502_CallbackTable = array[0..65535] of M6502_Callback;
  M6502_Memory = array[0..65535] of byte;
  M6502_DumpBuf = array[0..63] of char;
  M6502_CallbackType = (ctRead, ctWrite, ctCall);

  M6502_Registers = record
      a: byte;
      x: byte;
      y: byte;
      p: byte;
      s: byte;
      pc: word;
  end;

  M6502_Callbacks = record
      read: M6502_CallbackTable;
      write: M6502_CallbackTable;
      call: M6502_CallbackTable;
  end;

  M6502 = record
      registers: ^M6502_Registers;
      memory: ^M6502_Memory;
      callbacks: ^M6502_Callbacks;
      flags: word;
      ticks: byte;
  end;

  T6502Event = function(Sender: TObject; addr: word; data: byte): longint of object;

  { TM6502 }

  TM6502 = class(TObject)
  private
    FMemory: TBytesStream;
    F6502: PM6502;
    FMPUIndex: byte;
    FOnRead, FOnWrite, FOnCall: T6502Event;
    function GetRegisters: PM6502_Registers;
    function ProcessRead(addr: word): longint;
    function ProcessWrite(addr: word; data: byte): longint;
    function ProcessCall(addr: word): longint;
  public
    property Registers: PM6502_Registers read GetRegisters;
    property Memory: TBytesStream read FMemory;
    property OnRead: T6502Event read FOnRead write FOnRead;
    property OnWrite: T6502Event read FOnWrite write FOnWrite;
    property OnCall: T6502Event read FOnCall write FOnCall;
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure NMI;
    procedure IRQ;
    procedure Run;
    procedure Tick;
    procedure Step;
    procedure SetCallback(typ: M6502_CallbackType; addr: word; fn: M6502_Callback);
    function GetCallback(typ: M6502_CallbackType; addr: word): M6502_Callback;
    procedure AddEvent(typ: M6502_CallbackType; addr: word);
    procedure SetVector(vec, addr: word);
    function GetVector(vec: word): word;
    function PopStack: byte;
    function RTS: word;
    function RTI: word;
    procedure WriteByte(b: byte);
    procedure Write(data: string);
    function ReadByte: byte;
    function GetString(addr: word): string;
    procedure LoadFrom(s: TStream);
    procedure LoadInto(s: TStream; addr: word);
    procedure SaveInto(s: TStream; addr, size: word);
  end;

function M6502_new(registers: PM6502_Registers; memory: M6502_Memory; callbacks: PM6502_Callbacks): PM6502; cdecl; external lib6502x;
function M6502_new(registers: PM6502_Registers; memory: TBytes; callbacks: PM6502_Callbacks): PM6502; cdecl; external lib6502x;
procedure M6502_reset(mpu:PM6502); cdecl; external lib6502x;
procedure M6502_nmi(mpu:PM6502); cdecl; external lib6502x;
procedure M6502_irq(mpu:PM6502); cdecl; external lib6502x;
procedure M6502_run(mpu:PM6502); cdecl; external lib6502x;
procedure M6502_tick(mpu:PM6502); cdecl; external lib6502x;
procedure M6502_step(mpu:PM6502); cdecl; external lib6502x;
function M6502_disassemble(mpu: PM6502; addr: word; buffer: M6502_DumpBuf): longint; cdecl; external lib6502x;
procedure M6502_dump(mpu: PM6502; buffer: M6502_DumpBuf); cdecl; external lib6502x;
procedure M6502_delete(mpu: PM6502); cdecl; external lib6502x;

function M6502_getCallback(mpu: PM6502; typ: M6502_CallbackType; addr: word): M6502_Callback;
procedure M6502_setCallback(mpu: PM6502; typ: M6502_CallbackType; addr: word; fn: M6502_Callback);
function M6502_getVector(mpu: PM6502; vec: word): word;
procedure M6502_setVector(mpu: PM6502; vec, addr: word);

function M6502_popStack(mpu: PM6502): byte;
procedure M6502_pushStack(mpu: PM6502; b: byte);
function M6502_RTS(mpu: PM6502): word;
function M6502_RTI(mpu: PM6502): word;

implementation

type
  PMPUList = ^TMPUList;
  TMPUList = record
      i: TM6502;
      p: PM6502;
  end;

var
  MPUList: Array[0..31] of PMPUList;

function M6502_getCallback(mpu: PM6502; typ: M6502_CallbackType; addr: word
  ): M6502_Callback;
begin
  case typ of
    ctRead: Result:=mpu^.callbacks^.read[addr];
    ctWrite: Result:=mpu^.callbacks^.write[addr];
    ctCall: Result:=mpu^.callbacks^.call[addr];
  end;
end;

procedure M6502_setCallback(mpu: PM6502; typ: M6502_CallbackType; addr: word;
  fn: M6502_Callback);
begin
  case typ of
    ctRead: mpu^.callbacks^.read[addr]:=fn;
    ctWrite: mpu^.callbacks^.write[addr]:=fn;
    ctCall: mpu^.callbacks^.call[addr]:=fn;
  end;
end;

function M6502_getVector(mpu: PM6502; vec: word): word;
begin
  Result:=(mpu^.memory^[vec+1] shl 8)+mpu^.memory^[vec];
end;

procedure M6502_setVector(mpu: PM6502; vec, addr: word);
begin
  mpu^.memory^[vec]:=addr and $ff;
  mpu^.memory^[vec+1]:=addr shr 8;
end;

function M6502_popStack(mpu: PM6502): byte;
begin
  with mpu^.registers^ do
  begin
    {$R-}Inc(s);{$R+}
    Result:=mpu^.memory^[$100+s];
  end;
end;

procedure M6502_pushStack(mpu: PM6502; b: byte);
begin

end;

function M6502_RTS(mpu: PM6502): word;
var
  ptr: word;
begin
  ptr:=M6502_popStack(mpu);
  ptr:=(M6502_popStack(mpu) shl 8)+ptr;
  Result:=ptr+1;
end;

function M6502_RTI(mpu: PM6502): word;
var
  ptr: word;
begin
  mpu^.registers^.p:=M6502_popStack(mpu);
  ptr:=M6502_popStack(mpu);
  ptr:=(M6502_popStack(mpu) shl 8)+ptr;
  Result:=ptr;
end;

function NextMPU: byte;
var
  i: byte;
begin
  Result:=255;
  for i:=0 to High(MPUList) do
    if MPUList[i] = Nil then
    begin
      Result:=i;
      Exit;
    end;
end;

function GetMPU(mpu: PM6502): TM6502;
var
  i: byte;
begin
  for i:=0 to High(MPUList) do
    if MPUList[i]^.p = mpu then
    begin
      Result:=MPUList[i]^.i;
      Exit;
    end;
end;

function read6502(mpu: PM6502; addr: word; data: byte): longint; cdecl;
begin
  Result:=GetMPU(mpu).ProcessRead(addr);
end;

function write6502(mpu: PM6502; addr: word; data: byte): longint; cdecl;
begin
  Result:=GetMPU(mpu).ProcessWrite(addr, data);
end;

function call6502(mpu: PM6502; addr: word; data: byte): longint; cdecl;
begin
  Result:=GetMPU(mpu).ProcessCall(addr);
end;

{ TM6502 }

function TM6502.GetRegisters: PM6502_Registers;
begin
  Result:=F6502^.registers;
end;

function TM6502.ProcessRead(addr: word): longint;
begin
  if Assigned(FOnRead) then
    Result:=FOnRead(Self, addr, 0);
end;

function TM6502.ProcessWrite(addr: word; data: byte): longint;
begin
  if Assigned(FOnWrite) then
    Result:=FOnWrite(self, addr, data);
end;

function TM6502.ProcessCall(addr: word): longint;
begin
  if Assigned(FOnCall) then
    Result:=FOnCall(Self, addr, 0);
end;

constructor TM6502.Create;
var
  i: byte;
begin
  i:=NextMPU;
  FMemory:=TBytesStream.Create;
  FMemory.Size:=65536;
  F6502:=M6502_new(Nil, FMemory.Bytes, Nil);
  New(MPUList[i]);
  MPUList[i]^.i:=Self;
  MPUList[i]^.p:=F6502;
  FMPUIndex:=i;
end;

destructor TM6502.Destroy;
begin
  Dispose(MPUList[FMPUIndex]);
  MPUList[FMPUIndex]:=Nil;
  M6502_delete(F6502);
  FMemory.Free;
  inherited Destroy;
end;

procedure TM6502.Reset;
begin
  M6502_reset(F6502);
end;

procedure TM6502.NMI;
begin
  M6502_nmi(F6502);
end;

procedure TM6502.IRQ;
begin
  M6502_irq(F6502);
end;

procedure TM6502.Run;
begin
  M6502_run(F6502);
end;

procedure TM6502.Tick;
begin
  M6502_tick(F6502);
end;

procedure TM6502.Step;
begin
  M6502_step(F6502);
end;

procedure TM6502.SetCallback(typ: M6502_CallbackType; addr: word;
  fn: M6502_Callback);
begin
  M6502_setCallback(F6502, typ, addr, fn);
end;

function TM6502.GetCallback(typ: M6502_CallbackType; addr: word
  ): M6502_Callback;
begin
  Result:=M6502_getCallback(F6502, typ, addr);
end;

procedure TM6502.AddEvent(typ: M6502_CallbackType; addr: word);
var
  fn: M6502_Callback;
begin
  case typ of
    ctRead: fn:=@read6502;
    ctWrite: fn:=@write6502;
    ctCall: fn:=@call6502;
  end;
  SetCallback(typ, addr, fn);
end;

procedure TM6502.SetVector(vec, addr: word);
begin
  M6502_setVector(F6502, vec, addr);
end;

function TM6502.GetVector(vec: word): word;
begin
  Result:=M6502_getVector(F6502, vec);
end;

function TM6502.PopStack: byte;
begin
  Result:=M6502_popStack(F6502);
end;

function TM6502.RTS: word;
begin
  Result:=M6502_RTS(F6502);
end;

function TM6502.RTI: word;
begin
  Result:=M6502_RTI(F6502);
end;

procedure TM6502.WriteByte(b: byte);
var
  pc: word;
begin
  pc:=Registers^.pc;
  FMemory.Bytes[pc]:=b;
  Inc(pc);
  Registers^.pc:=pc;
end;

procedure TM6502.Write(data: string);
begin
  Move(data[1], FMemory.Bytes[Registers^.pc], Length(data));
  Inc(Registers^.pc, Length(data));
end;

function TM6502.ReadByte: byte;
begin
  with Registers^ do
  begin
    Result:=FMemory.Bytes[pc];
    Inc(pc);
  end;
end;

function TM6502.GetString(addr: word): string;
var
  pc: word;
  c: char;
begin
  Result:='';
  pc:=Registers^.pc;
  Registers^.pc:=addr;
  repeat
    c:=Chr(ReadByte);
    if c <> #0 then
      Result:=Result+c;
  until c = #0;
  Registers^.pc:=pc;
end;

procedure TM6502.LoadFrom(s: TStream);
var
  addr: word;
begin
  addr:=s.ReadWord;
  s.Read(FMemory.Bytes[addr], s.Size);
end;

procedure TM6502.LoadInto(s: TStream; addr: word);
begin
  s.Read(FMemory.Bytes[addr], s.Size);
end;

procedure TM6502.SaveInto(s: TStream; addr, size: word);
begin
  s.Write(FMemory.Bytes[addr], size);
end;

initialization
  FillByte(MPUList, SizeOf(MPUList), 0);

end.
