unit lib6502;

{
  Converted from lib6502.h into a Pascal unit.  This unit can work with either an object file, or
  with a shared object file otherwise known as a Dynamically Linked Library on the Windows platform.

  lib6502 repo can be found here: https://github.com/ShonFrazier/lib6502/

  Use the standard make procedure with this project to generate a lib6502.o object file.
  This object file can then be linked directly with this unit and your program code to create
  a singular binary image for your end-user.  If you wish to generate an external shared library,
  here's how:

  On Linux: gcc -shared -o lib6502.so lib6502.o
  Then place this file into your standard library directory, usually /usr/lib

  On Windows: i686-w64-mingw32-gcc -g -O3 -fPIC -c -o6502.OBJ lib6502.c
  This will compile the C library into a Windows COFF-compatible OBJ file.

  Create Windows DLL: i686-w64-mingw32-gcc -shared -o 6502.DLL 6502.OBJ

  I also have this written in my notes and will correct if this is actually needed:
  i686-w64-mingw32-ld -shared 6502.OBJ -lmsvcrt
  This was done, as I was running into a Microsoft Visual C Runtime error at one point when using it.
}

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

  {$PACKENUM 1}{$PACKSET 1}
  CPUFlags = (flagC, flagZ, flagI, flagD, flagB, flagX, flagV, flagN);
  TCPUFlags = Set of CPUFlags;

  M6502_Registers = record
      a: byte;
      x: byte;
      y: byte;
      p: TCPUFlags;
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
    F6502: Array[0..31] of PM6502;
    FRunning: Array[0..31] of Boolean;
    FOnRead, FOnWrite, FOnCall: Array[0..31] of T6502Event;
    function GetCores: byte;
    function GetOnCall(core: byte): T6502Event;
    function GetOnRead(core: byte): T6502Event;
    function GetOnWrite(core: byte): T6502Event;
    function GetRegisters(core: byte): PM6502_Registers;
    function GetCore(mpu: PM6502): byte;
    function GetRunning(core: byte): Boolean;
    procedure DeleteCore(core: byte);
    function ProcessRead(mpu: PM6502; addr: word): longint;
    function ProcessWrite(mpu: PM6502; addr: word; data: byte): longint;
    function ProcessCall(mpu: PM6502; addr: word): longint;
    procedure SetCores(AValue: byte);
    procedure SetOnCall(core: byte; AValue: T6502Event);
    procedure SetOnRead(core: byte; AValue: T6502Event);
    procedure SetOnWrite(core: byte; AValue: T6502Event);
    procedure SetRunning(core: byte; AValue: Boolean);
  public
    property Cores: byte read GetCores write SetCores;
    property Running[core: byte]: Boolean read GetRunning write SetRunning;
    property Registers[core: byte]: PM6502_Registers read GetRegisters;
    property Memory: TBytesStream read FMemory;
    property OnRead[core: byte]: T6502Event read GetOnRead write SetOnRead;
    property OnWrite[core: byte]: T6502Event read GetOnWrite write SetOnWrite;
    property OnCall[core: byte]: T6502Event read GetOnCall write SetOnCall;
    constructor Create;
    destructor Destroy; override;
    function NewCore: byte;
    procedure ResetCores;
    procedure ResetMemory;
    procedure Reset(core: byte);
    procedure NMI(core: byte);
    procedure IRQ(core: byte);
    procedure Run;
    procedure Tick;
    procedure Step;
    procedure SetCallback(core: byte; typ: M6502_CallbackType; addr: word; fn: M6502_Callback);
    function GetCallback(core: byte; typ: M6502_CallbackType; addr: word): M6502_Callback;
    procedure AddEvent(core: byte; typ: M6502_CallbackType; addr: word);
    procedure SetVector(core: byte; vec, addr: word);
    function GetVector(core: byte; vec: word): word;
    function PopStack(core: byte): byte;
    procedure PushStack(core: byte; b: byte);
    function RTS(core: byte): word;
    function RTI(core: byte): word;
    procedure WriteByte(b: byte);
    procedure Write(data: string);
    procedure WriteInto(data: string; addr: word);
    function ReadByte: byte;
    function GetString(addr: word): string;
    procedure LoadFrom(s: TStream);
    procedure LoadInto(s: TStream; addr: word);
    procedure LoadInto(s: TStream; addr, size: word);
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

function GetMOS6502: TM6502;

implementation

var
  { Only one instance of this is allowed per application. }
  MOS6502: TM6502;

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
  with mpu^.registers^ do
  begin
    mpu^.memory^[$100+s]:=b;
    {$R-}Dec(s);{$R+}
  end;
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
  b: byte;
begin
  b:=M6502_popStack(mpu);
  Move(b,mpu^.registers^.p,1);
  ptr:=M6502_popStack(mpu);
  ptr:=(M6502_popStack(mpu) shl 8)+ptr;
  Result:=ptr;
end;

function GetMOS6502: TM6502;
begin
  if not Assigned(MOS6502) then
    MOS6502:=TM6502.Create;
  Result:=MOS6502;
end;

function read6502(mpu: PM6502; addr: word; data: byte): longint; cdecl;
begin
  Result:=MOS6502.ProcessRead(mpu, addr);
end;

function write6502(mpu: PM6502; addr: word; data: byte): longint; cdecl;
begin
  Result:=MOS6502.ProcessWrite(mpu, addr, data);
end;

function call6502(mpu: PM6502; addr: word; data: byte): longint; cdecl;
begin
  Result:=MOS6502.ProcessCall(mpu, addr);
end;

{ TM6502 }

function TM6502.GetRegisters(core: byte): PM6502_Registers;
begin
  Result:=F6502[core]^.registers;
end;

function TM6502.GetOnCall(core: byte): T6502Event;
begin
  Result:=FOnRead[core];
end;

function TM6502.GetCores: byte;
var
  i: Integer;
begin
  Result:=0;
  for i:=0 to High(F6502) do
    if Assigned(F6502[i]) then
      Inc(Result);
end;

function TM6502.GetOnRead(core: byte): T6502Event;
begin
  Result:=FOnRead[core];
end;

function TM6502.GetOnWrite(core: byte): T6502Event;
begin
  Result:=FOnWrite[core];
end;

function TM6502.GetCore(mpu: PM6502): byte;
var
  i: byte;
begin
  for i:=0 to High(F6502) do
    if F6502[i] = mpu then
    begin
      Result:=i;
      Exit;
    end;
end;

function TM6502.GetRunning(core: byte): Boolean;
begin
  Result:=FRunning[core];
end;

function TM6502.NewCore: byte;
var
  i: byte;
begin
  Result:=255;
  for i:=0 to High(F6502) do
    if F6502[i] = Nil then
    begin
      F6502[i]:=M6502_new(Nil, FMemory.Bytes, Nil);
      Result:=i;
      Exit;
    end;
end;

procedure TM6502.ResetCores;
var
  i: integer;
begin
  for i:=0 to High(F6502) do
    if Assigned(F6502[i]) then
    begin
      M6502_delete(F6502[i]);
      F6502[i]:=M6502_new(Nil, FMemory.Bytes, Nil);
    end;
end;

procedure TM6502.ResetMemory;
begin
  FillByte(FMemory.Bytes[0], FMemory.Size, 0);
end;

procedure TM6502.DeleteCore(core: byte);
begin
  if F6502[core] = Nil then
    Exit;
  M6502_delete(F6502[core]);
  F6502[core]:=Nil;
end;

function TM6502.ProcessRead(mpu: PM6502; addr: word): longint;
var
  core: byte;
begin
  core:=GetCore(mpu);
  if Assigned(FOnRead[core]) then
    Result:=FOnRead[core](Self, addr, 0);
end;

function TM6502.ProcessWrite(mpu: PM6502; addr: word; data: byte): longint;
var
  core: byte;
begin
  core:=GetCore(mpu);
  if Assigned(FOnWrite[core]) then
    Result:=FOnWrite[core](self, addr, data);
end;

function TM6502.ProcessCall(mpu: PM6502; addr: word): longint;
var
  core: byte;
begin
  core:=GetCore(mpu);
  if Assigned(FOnCall[core]) then
    Result:=FOnCall[core](Self, addr, 0);
end;

procedure TM6502.SetCores(AValue: byte);
var
  i: integer;
  c: byte;
begin
  c:=Cores;
  if c=AValue then Exit;
  if AValue > High(F6502) then Exit;
  if AValue > c then
    for i:=0 to (AValue-c-1) do
      NewCore;
  if AValue < c then
    for i:=0 to (c-AValue) do
      DeleteCore(c-i);
end;

procedure TM6502.SetOnCall(core: byte; AValue: T6502Event);
begin
  FOnCall[core]:=AValue;
end;

procedure TM6502.SetOnRead(core: byte; AValue: T6502Event);
begin
  FOnRead[core]:=AValue;
end;

procedure TM6502.SetOnWrite(core: byte; AValue: T6502Event);
begin
  FOnWrite[core]:=AValue;
end;

procedure TM6502.SetRunning(core: byte; AValue: Boolean);
begin
  FRunning[core]:=AValue;
end;

constructor TM6502.Create;
begin
  if Assigned(MOS6502) then
    raise Exception.Create('Only a single instance of TM6502 can exist!');
  FMemory:=TBytesStream.Create;
  FMemory.Size:=65536;
  FillByte(F6502, SizeOf(F6502), 0);
  FillByte(FRunning, SizeOf(FRunning), 1);
  MOS6502:=Self;
end;

destructor TM6502.Destroy;
var
  i: integer;
begin
  MOS6502:=Nil;
  for i:=0 to High(F6502) do
    if Assigned(F6502[i]) then
      M6502_delete(F6502[i]);
  FMemory.Free;
  inherited Destroy;
end;

procedure TM6502.Reset(core: byte);
begin
  M6502_reset(F6502[core]);
end;

procedure TM6502.NMI(core: byte);
begin
  M6502_nmi(F6502[core]);
end;

procedure TM6502.IRQ(core: byte);
begin
  M6502_irq(F6502[core]);
end;

procedure TM6502.Run;
begin
  if Cores = 1 then
    M6502_run(F6502[0]);
end;

procedure TM6502.Tick;
var
  i: Integer;
begin
  for i:=0 to High(F6502) do
    if FRunning[i] then
      if Assigned(F6502[i]) then
        M6502_tick(F6502[i]);
end;

procedure TM6502.Step;
var
  i: Integer;
begin
  for i:=0 to High(F6502) do
    if FRunning[i] then
      if Assigned(F6502[i]) then
        M6502_step(F6502[i]);
end;

procedure TM6502.SetCallback(core: byte; typ: M6502_CallbackType; addr: word;
  fn: M6502_Callback);
begin
  M6502_setCallback(F6502[core], typ, addr, fn);
end;

function TM6502.GetCallback(core: byte; typ: M6502_CallbackType; addr: word
  ): M6502_Callback;
begin
  Result:=M6502_getCallback(F6502[core], typ, addr);
end;

procedure TM6502.AddEvent(core: byte; typ: M6502_CallbackType; addr: word);
var
  fn: M6502_Callback;
begin
  case typ of
    ctRead: fn:=@read6502;
    ctWrite: fn:=@write6502;
    ctCall: fn:=@call6502;
  end;
  SetCallback(core, typ, addr, fn);
end;

procedure TM6502.SetVector(core: byte; vec, addr: word);
begin
  M6502_setVector(F6502[core], vec, addr);
end;

function TM6502.GetVector(core: byte; vec: word): word;
begin
  Result:=M6502_getVector(F6502[core], vec);
end;

function TM6502.PopStack(core: byte): byte;
begin
  Result:=M6502_popStack(F6502[core]);
end;

procedure TM6502.PushStack(core: byte; b: byte);
begin
  M6502_pushStack(F6502[core], b);
end;

function TM6502.RTS(core: byte): word;
begin
  Result:=M6502_RTS(F6502[core]);
end;

function TM6502.RTI(core: byte): word;
begin
  Result:=M6502_RTI(F6502[core]);
end;

procedure TM6502.WriteByte(b: byte);
var
  pc: word;
begin
  pc:=Registers[0]^.pc;
  FMemory.Bytes[pc]:=b;
  Inc(pc);
  Registers[0]^.pc:=pc;
end;

procedure TM6502.Write(data: string);
begin
  Move(data[1], FMemory.Bytes[Registers[0]^.pc], Length(data));
  Inc(Registers[0]^.pc, Length(data));
end;

procedure TM6502.WriteInto(data: string; addr: word);
begin
  Move(data[1], FMemory.Bytes[addr], Length(data));
end;

function TM6502.ReadByte: byte;
begin
  with Registers[0]^ do
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
  pc:=Registers[0]^.pc;
  Registers[0]^.pc:=addr;
  repeat
    c:=Chr(ReadByte);
    if c <> #0 then
      Result:=Result+c;
  until c = #0;
  Registers[0]^.pc:=pc;
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

procedure TM6502.LoadInto(s: TStream; addr, size: word);
begin
  s.Read(FMemory.Bytes[addr], size);
end;

procedure TM6502.SaveInto(s: TStream; addr, size: word);
begin
  s.Write(FMemory.Bytes[addr], size);
end;

initialization
  MOS6502:=Nil;

finalization
  if Assigned(MOS6502) then
    MOS6502.Free;

end.
