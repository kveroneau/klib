unit qvfs;

{$mode ObjFPC}{$H+}
{$PackEnum 1}

interface

uses
  Classes, SysUtils, cmdsys;

type
  TVFSType = (ftFolder, ftFile, ftText, ftSource, ftUI, ftProject, ftSession,
              ftExec, ftLibrary, ftImage, ftDiskImage, ftDLO, ftDSK, ftUser,
              ftVFS, ftKMC);

  PVFSFile = ^TVFSFile;
  TVFSFile = record
    fna: string[10];
    typ: TVFSType;
    wh: string[10];
    data: string[60];
  end;

function VFSShell(command, wh: string): string;
function GetVFSFile(AFile, ALoc: string): string;
function GetVFSData(AFile, ALoc: string): PVFSFile;

implementation

const
  VFS_VERSION = 'v0.2b';
  {$IFDEF DEBUG}
  VFS_FILE = 'debug.dat';
  {$ELSE}
  VFS_FILE = 'vfs.dat';
  {$ENDIF}

var
  vfs: Array[0..255] of PVFSFile;

procedure InitVFS;
var
  f: File of TVFSFile;
  i: Integer;
begin
  FillByte(vfs[0], SizeOf(vfs), 0);
  if not FileExists(VFS_FILE) then
    Exit;
  Assign(f, VFS_FILE);
  Reset(f);
  for i:=0 to FileSize(f)-1 do
  begin
    New(vfs[i]);
    Read(f, vfs[i]^);
  end;
  Close(f);
end;

procedure DoneVFS;
var
  i: Integer;
begin
  for i:=0 to 255 do
    if vfs[i] <> Nil then
      Dispose(vfs[i]);
end;

procedure ReloadVFS;
begin
  DoneVFS;
  InitVFS;
end;

procedure SaveVFS;
var
  f: File of TVFSFile;
  i: Integer;
begin
  Assign(f, VFS_FILE);
  Rewrite(f);
  for i:=0 to 255 do
    if vfs[i] <> Nil then
      Write(f, vfs[i]^);
  Close(f);
end;

function NextFree: integer;
var
  i: integer;
begin
  for i:=0 to 255 do
    if vfs[i] = Nil then
    begin
      Result:=i;
      Exit;
    end;
  Result:=-1;
end;

function GetVFSFile(AFile, ALoc: string): string;
var
  f: PVFSFile;
begin
  f:=GetVFSData(AFile, ALoc);
  if f = Nil then
    Result:=''
  else
    Result:=f^.data;
end;

function GetVFSData(AFile, ALoc: string): PVFSFile;
var
  i: integer;
begin
  for i:=0 to 255 do
    if vfs[i] <> Nil then
      if (vfs[i]^.wh = ALoc) and (vfs[i]^.fna = AFile) then
      begin
        Result:=vfs[i];
        Exit;
      end;
  Result:=Nil;
end;

function GetIcon(AType: TVFSType): string;
begin
  case AType of
    ftFolder: Result:='[=-=]';
    ftFile: Result:='[=?=]';
    ftText: Result:='[tXt]';
    ftSource: Result:='[sRc]';
    ftUI: Result:='[%Ui]';
    ftProject: Result:='<$^$>';
    ftSession: Result:='[-==]';
    ftExec: Result:='[//>]';
    ftLibrary: Result:='<sYs>';
    ftUser: Result:='[*@*]';
  else
    Result:='[ ? ]';
  end;
end;

function ListDir(ADir: string): string;
var
  buf: string;
  i: Integer;
begin
  buf:='Listing of '+ADir+#10;
  for i:=0 to 255 do
    if vfs[i] <> Nil then
      if vfs[i]^.wh = ADir then
        buf:=buf+' '+GetIcon(vfs[i]^.typ)+' '+vfs[i]^.fna+#10;
  Result:=buf;
end;

function MakeDir(ADir, ALoc: string): string;
var
  i: integer;
begin
  i:=NextFree;
  if i = -1 then
  begin
    Result:='No space left!';
    Exit;
  end;
  New(vfs[i]);
  with vfs[i]^ do
  begin
    fna:=ADir;
    typ:=ftFolder;
    wh:=ALoc;
    data:='VFS Directory created using the Portfolio Server VFS Shell.';
  end;
  Result:='Directory Created.';
end;

function MakeFile(AFile, ALoc: string): string;
var
  i: Integer;
begin
  i:=NextFree;
  if i = -1 then
  begin
    Result:='No space left!';
    Exit;
  end;
  New(vfs[i]);
  with vfs[i]^ do
  begin
    fna:=AFile;
    typ:=ftFile;
    wh:=ALoc;
    data:='VFS File created using the Portfolio Server VFS Shell.';
  end;
  Result:='File Created.';
end;

function SetContent(command, ALoc: string): String;
var
  f: PVFSFile;
  AFile, AData: String;
begin
  AFile:=getToken(command);
  AData:=getToken(command);
  f:=GetVFSData(AFile, ALoc);
  if f = Nil then
    Exit;
  Result:='Previous content: '+f^.data;
  f^.data:=AData;
end;

function SetType(AFile, ALoc: string; typ: Integer): String;
var
  f: PVFSFile;
begin
  f:=GetVFSData(AFile, ALoc);
  if f = Nil then
    Exit;
  Result:='Previous type: '+IntToStr(ord(f^.typ));
  f^.typ:=TVFSType(typ);
end;

procedure RemoveFile(AFile, ALoc: string);
var
  i: integer;
begin
  for i:=0 to 255 do
    if vfs[i] <> Nil then
      if (vfs[i]^.fna = AFile) and (vfs[i]^.wh = ALoc) then
      begin
        Dispose(vfs[i]);
        vfs[i]:=Nil;
        Exit;
      end;
end;

function VFSShell(command, wh: string): string;
var
  cmd: string;
begin
  cmd:=getToken(command);
  case cmd of
    'ver': Result:='Portfolio VFS '+VFS_VERSION;
    'sync': SaveVFS;
    'reload': ReloadVFS;
    'list': Result:=ListDir(wh);
    'mkdir': Result:=MakeDir(getToken(command), wh);
    'mkfile': Result:=MakeFile(getToken(command), wh);
    'show': Result:=GetVFSFile(getToken(command), wh);
    'set': Result:=SetContent(command, wh);
    'settype': Result:=SetType(getToken(command), wh, StrToInt(getToken(command)));
    'rmfile': RemoveFile(getToken(command), wh);
  else
    Result:='Invalid VFS Command.';
  end;
end;

initialization
  InitVFS;

finalization
  SaveVFS;
  DoneVFS;

end.

