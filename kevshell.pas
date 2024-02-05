unit KevShell;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Konsole,
  StrUtils, FileUtil, memcard, lib6502;

type

  { TKevShell }

  TKevShell = class(TKonsole)
  private
    FProgramPath: TStringList;
    FProgramList: TStringList;
    FResources: TMemcard;
    FMemcard: TMemcard;
    F6502: TM6502;
    FCore: byte;
    FSleepTime: byte;
    FOnExitCommand: TNotifyEvent;
    procedure KonsoleCommand(Sender: TObject; cmd, params: string);
    procedure ListPrograms;
    function FindProgram(const prog: string): string;
    procedure HandleUnknown(const cmd, param: string);
    procedure RunProgram(const fname, param: string);
    procedure CallAddress(const addr: word);
    procedure StrEqual;
    procedure StrCat;
    procedure DoExec;
    procedure LoadResource;
    procedure SaveResource;
    procedure FindResource;
    procedure OpenCard;
    procedure FindType;
    procedure LoadBlock;
    procedure SaveBlock;
    procedure LoadFile;
    procedure LoadModule;
    procedure StdIn;
    function HandleCall(Sender: TObject; addr: word; data: byte): longint;
  protected

  public
    property ProgramList: TStringList read FProgramList;
    property Resources: TMemcard read FResources;
    property Memcard: TMemcard read FMemcard;
    property MOS6502: TM6502 read F6502;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EnableDebugBin;
    procedure ScanPrograms;
    procedure Init6502;
    function GetParam1: string;
    function GetParam2: string;
  published
    property OnExitCommand: TNotifyEvent read FOnExitCommand write FOnExitCommand;
    property SleepTime: byte read FSleepTime write FSleepTime;
  end;

procedure Register;

implementation

const
  KSH_HOME = '/btrfs/KevShell/';
  DEFAULT_SLEEP = 1;
  KSH_VERSION = '0.5b';

procedure Register;
begin
  {$I kevshell_icon.lrs}
  RegisterComponents('klib',[TKevShell]);
end;

{ TKevShell }

procedure TKevShell.KonsoleCommand(Sender: TObject; cmd, params: string);
begin
  if cmd = 'echo' then
    WriteLn(params)
  else if cmd = 'ver' then
    WriteLn(' * KevShellUI version '+KSH_VERSION)
  else if cmd = 'cls' then
    ClrScr
  else if cmd = 'ls' then
    ListPrograms
  else if cmd = 'scan' then
    ScanPrograms
  else if cmd = 'call' then
    CallAddress(Hex2Dec(params))
  else if cmd = 'peek' then
    WriteLn(' * $'+params+' = $'+HexStr(F6502.Memory.Bytes[Hex2Dec(params)], 2))
  else if cmd = 'exit' then
  begin
    if Assigned(FOnExitCommand) then
      FOnExitCommand(Self)
  end
  else
    HandleUnknown(cmd, params);
end;

function TKevShell.HandleCall(Sender: TObject; addr: word; data: byte): longint;
begin
  case addr of
    $ff00: Write(GetParam1);
    $ff01: DoExec;
    $ff02: SetCurrentDir(GetParam1);
    $ff03: StrEqual;
    $ff04: F6502.WriteInto(GetCurrentDir+#0, F6502.GetVector(FCore, $ffe0));
    $ff05: Write(IntToStr(F6502.GetVector(FCore, $ffe0)));
    $ff06: StrCat;
    $ff07: LoadResource;
    $ff08: SaveResource;
    $ff09: FindResource;
    $ff0a: OpenCard;
    $ff0b: FindType;
    $ff0c: LoadBlock;
    $ff0d: SaveBlock;
    $ff0e: LoadFile;
    $ff0f: LoadModule;
    $ff10: StdIn;
    $fff0: F6502.Running[FCore]:=False;
  end;
  Result:=F6502.RTS(FCore);
end;

constructor TKevShell.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnCommand:=@KonsoleCommand;
  Font.Color:=clGreen;
  FProgramPath:=TStringList.Create;
  FProgramPath.Add(KSH_HOME+'bin/');
  if DirectoryExists(GetUserDir+'.KevShell') then
    FProgramPath.Add(GetUserDir+'.KevShell/');
  FProgramPath.Add(GetCurrentDir+'/');
  FProgramList:=Nil;
  FSleepTime:=DEFAULT_SLEEP;
  FMemcard:=Nil;
  Enabled:=False;
end;

destructor TKevShell.Destroy;
begin
  if Assigned(FMemcard) then
    FMemcard.Free;
  if Assigned(FProgramList) then
    FProgramList.Free;
  FProgramPath.Free;
  if Assigned(FResources) then
    FResources.Free;
  inherited Destroy;
end;

procedure TKevShell.EnableDebugBin;
begin
  FProgramPath.Add(GetCurrentDir+'/tests/');
end;

procedure TKevShell.ScanPrograms;
var
  lst: TStringList;
  i, s: integer;
begin
  Write(' * Performing scan of programs...');
  if Assigned(FProgramList) then
    FProgramList.Clear
  else
    FProgramList:=TStringList.Create;
  for s:=0 to FProgramPath.Count-1 do
  begin
    lst:=FindAllFiles(FProgramPath.Strings[s], '*.prg', False);
    try
      for i:=0 to lst.Count-1 do
        FProgramList.Add(ExtractFileName(Copy2Symb(lst.Strings[i], '.')));
    finally
      lst.Free;
    end;
  end;
  Write('done, '+IntToStr(FProgramList.Count)+' programs available.');
  Write(#$a);
end;

procedure TKevShell.Init6502;
var
  blkid: byte;
  blk: TMemoryStream;
  info: TBlockInfo;
begin
  FResources:=TMemCard.Create(GetUserDir+'KevShell.kmc', 1024);
  Write(' * Initializing the 6502 runtime environment...');
  Write(#$a);
  F6502:=GetMOS6502;
  FCore:=F6502.NewCore;
  Write('   * Core '+IntToStr(FCore)+' initializing...');
  F6502.OnCall[FCore]:=@HandleCall;
  F6502.AddEvent(FCore, ctCall, $ff00);
  F6502.AddEvent(FCore, ctCall, $ff01);
  F6502.AddEvent(FCore, ctCall, $ff02);
  F6502.AddEvent(FCore, ctCall, $ff03);
  F6502.AddEvent(FCore, ctCall, $ff04);
  F6502.AddEvent(FCore, ctCall, $ff05);
  F6502.AddEvent(FCore, ctCall, $ff06);
  F6502.AddEvent(FCore, ctCall, $ff07);
  F6502.AddEvent(FCore, ctCall, $ff08);
  F6502.AddEvent(FCore, ctCall, $ff09);
  F6502.AddEvent(FCore, ctCall, $ff0a);
  F6502.AddEvent(FCore, ctCall, $ff0b);
  F6502.AddEvent(FCore, ctCall, $ff0c);
  F6502.AddEvent(FCore, ctCall, $ff0d);
  F6502.AddEvent(FCore, ctCall, $ff0e);
  F6502.AddEvent(FCore, ctCall, $ff0f);
  F6502.AddEvent(FCore, ctCall, $ff10);
  F6502.AddEvent(FCore, ctCall, $fff0);
  F6502.SetVector(FCore, VEC_IRQ, $fff0);
  F6502.SetVector(FCore, $ffa0, 1000);
  F6502.Memory.Bytes[$ffa2]:=$ff;
  {$IFDEF DEBUG}
  F6502.Memory.Bytes[$ffa3]:=$ff;
  {$ELSE}
  F6502.Memory.Bytes[$ffa3]:=$00;
  {$ENDIF}
  Write('done.');
  Write(#$a);
  Write('   * Loading Boot program into memory...');
  blkid:=FResources.FindType($76, @info);
  if blkid > 0 then
  begin
    blk:=FResources.ReadBlock(blkid);
    try
      F6502.SetVector(FCore, VEC_RST, F6502.LoadFrom(blk));
    finally
      blk.Free;
    end;
    Write('done.');
    Write(#$a);
    F6502.Reset(FCore);
    repeat
      F6502.Step;
      Sleep(FSleepTime);
      Application.ProcessMessages;
    until not F6502.Running[FCore];
  end
  else
  begin
    Write('failed.');
    Write(#$a);
  end;
  WriteLn('KevShell '+KSH_VERSION+' started.');
  Prompt:='*';
  Enabled:=True;
end;

procedure TKevShell.ListPrograms;
var
  i: integer;
begin
  WriteLn(' > Available Programs <');
  for i:=0 to FProgramList.Count-1 do
    WriteLn(' * '+FProgramList.Strings[i]);
end;

function TKevShell.FindProgram(const prog: string): string;
var
  lst: TStringList;
  s: integer;
begin
  Result:='';
  for s:=0 to FProgramPath.Count-1 do
  begin
    lst:=FindAllFiles(FProgramPath.Strings[s], prog+'.prg', False);
    if lst.Count > 0 then
      Result:=FProgramPath.Strings[s]+prog+'.prg';
    lst.Free;
  end;
end;

procedure TKevShell.HandleUnknown(const cmd, param: string);
var
  fname: string;
begin
  if FProgramList.IndexOf(Copy2Symb(cmd, '.')) > -1 then
  begin
    fname:=FindProgram(Copy2Symb(cmd, '.'));
    if fname = '' then
      Exit;
    RunProgram(fname, param);
  end
  else
    WriteLn('?SYNTAX ERROR');
end;

procedure TKevShell.RunProgram(const fname, param: string);
var
  f: TMemoryStream;
  tmp: string;
begin
  if not FileExists(fname) then
    raise Exception.Create('Program not Found');
  F6502.Memory.Bytes[$ffa2]:=0;
  f:=TMemoryStream.Create;
  try
    f.LoadFromFile(fname);
    F6502.SetVector(FCore, VEC_RST, F6502.LoadFrom(f));
  finally
    f.Free;
  end;
  F6502.Reset(FCore);
  tmp:=param+#0;
  F6502.WriteInto(tmp, $ff70);
  F6502.Running[FCore]:=True;
  repeat
    F6502.Step;
    Sleep(FSleepTime);
    Application.ProcessMessages;
  until not F6502.Running[FCore];
end;

procedure TKevShell.CallAddress(const addr: word);
begin
  F6502.Registers[FCore]^.pc:=addr;
  F6502.Running[FCore]:=True;
  repeat
    F6502.Step;
    Sleep(FSleepTime);
    Application.ProcessMessages;
  until not F6502.Running[FCore];
end;

procedure TKevShell.StrEqual;
begin
  if GetParam1 = GetParam2 then
    F6502.Registers[FCore]^.a:=$ff
  else
    F6502.Registers[FCore]^.a:=$00;
end;

procedure TKevShell.StrCat;
var
  ptr: word;
  s: string;
begin
  ptr:=F6502.GetVector(FCore, $ffe0);
  s:=F6502.GetString(ptr)+GetParam2;
  F6502.WriteInto(s+#0, ptr);
end;

procedure TKevShell.DoExec;
begin
  {Exec(GetParam1, GetParam2);
  F6502.Registers[FCore]^.a:=DosExitCode;}
  WriteLn(' * Not implemented yet.');
end;

procedure TKevShell.LoadResource;
var
  blkid: byte;
  blk: TMemoryStream;
  info: TBlockInfo;
begin
  blkid:=F6502.Memory.Bytes[$ffe0];
  if (blkid < 1) or (blkid > 15) then
    raise Exception.Create('Invalid BlockID');
  FResources.GetInfo(blkid, @info);
  if info.typno = 0 then
    raise Exception.Create('Invalid Block');
  blk:=FResources.ReadBlock(blkid);
  try
    F6502.LoadInto(blk, F6502.GetVector(FCore, $ffe2), info.total);
  finally
    blk.Free;
  end;
end;

procedure TKevShell.SaveResource;
var
  blkid: byte;
  blk: TMemoryStream;
  info: TBlockInfo;
begin
  blkid:=F6502.Memory.Bytes[$ffe0];
  if (blkid < 1) or (blkid > 15) then
    raise Exception.Create('Invalid BlockID');
  FResources.GetInfo(blkid, @info);
  info.appno:=$60;
  info.typno:=F6502.Memory.Bytes[$ffe1];
  info.total:=F6502.GetVector(FCore, $ffe4);
  info.nextid:=0;
  if info.total > FResources.BlockSize then
    raise Exception.Create('Resource to large for card');
  blk:=TMemoryStream.Create;
  try
    F6502.SaveInto(blk, F6502.GetVector(FCore, $ffe2), info.total);
    blk.Size:=FResources.BlockSize;
    FResources.WriteBlock(blkid, blk, @info);
  finally
    blk.Free;
  end;
end;

procedure TKevShell.FindResource;
var
  info: TBlockInfo;
begin
  F6502.Registers[FCore]^.a:=FResources.FindType(F6502.Memory.Bytes[$ffe0], @info);
end;

procedure TKevShell.OpenCard;
begin
  if Assigned(FMemcard) then
    FMemcard.Free;
  FMemcard:=TMemCard.Create(GetParam1, 1024);
end;

procedure TKevShell.FindType;
var
  info: TBlockInfo;
begin
  if not Assigned(FMemcard) then
    Exit;
  F6502.Registers[FCore]^.a:=FMemcard.FindType(F6502.Memory.Bytes[$ffe0], @info);
end;

procedure TKevShell.LoadBlock;
var
  blkid: byte;
  blk: TMemoryStream;
  info: TBlockInfo;
begin
  blkid:=F6502.Memory.Bytes[$ffe0];
  if (blkid < 1) or (blkid > 15) then
    raise Exception.Create('Invalid BlockID');
  FMemcard.GetInfo(blkid, @info);
  if info.typno = 0 then
    raise Exception.Create('Invalid Block');
  blk:=FMemcard.ReadBlock(blkid);
  try
    F6502.LoadInto(blk, F6502.GetVector(FCore, $ffe2), info.total);
  finally
    blk.Free;
  end;
end;

procedure TKevShell.SaveBlock;
var
  blkid: byte;
  blk: TMemoryStream;
  info: TBlockInfo;
begin
  blkid:=F6502.Memory.Bytes[$ffe0];
  if (blkid < 1) or (blkid > 15) then
    raise Exception.Create('Invalid BlockID');
  FMemcard.GetInfo(blkid, @info);
  info.appno:=$60;
  info.typno:=F6502.Memory.Bytes[$ffe1];
  info.total:=F6502.GetVector(FCore, $ffe4);
  info.nextid:=0;
  if info.total > FMemcard.BlockSize then
    raise Exception.Create('Resource to large for card');
  blk:=TMemoryStream.Create;
  try
    F6502.SaveInto(blk, F6502.GetVector(FCore, $ffe2), info.total);
    blk.Size:=FMemcard.BlockSize;
    FMemcard.WriteBlock(blkid, blk, @info);
  finally
    blk.Free;
  end;
end;

procedure TKevShell.LoadFile;
var
  f: TMemoryStream;
  fname: string;
begin
  fname:=GetParam1;
  if not FileExists(fname) then
    raise Exception.Create('File not found');
  f:=TMemoryStream.Create;
  try
    f.LoadFromFile(fname);
    F6502.LoadInto(f, F6502.GetVector(FCore, $ffe2));
    F6502.SetAX(FCore, f.Size);
  finally
    f.Free;
  end;
end;

procedure TKevShell.LoadModule;
var
  f: TMemoryStream;
  fname: string;
begin
  fname:=FindProgram(GetParam1);
  if fname = '' then
    raise Exception.Create('Module not found');
  f:=TMemoryStream.Create;
  try
    f.LoadFromFile(fname);
    F6502.SetAX(FCore, F6502.LoadFrom(f));
  finally
    f.Free;
  end;
end;

procedure TKevShell.StdIn;
var
  line: string;
begin
  ReadLn(line);
  if F6502.Registers[FCore]^.y  = 1 then
    F6502.SetAX(FCore, StrToInt(line))
  else if F6502.Registers[FCore]^.y = 2 then
    F6502.SetAX(FCore, Hex2Dec(line));
  F6502.WriteInto(line+#0, F6502.GetVector(FCore, $ffe0));
end;

function TKevShell.GetParam1: string;
begin
  Result:=F6502.GetString(F6502.GetVector(FCore, $ffe0));
end;

function TKevShell.GetParam2: string;
begin
  Result:=F6502.GetString(F6502.GetVector(FCore, $ffe2));
end;

end.
