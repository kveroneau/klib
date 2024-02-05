unit memcard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TCardFlags = bitpacked record
    Encrypted, Executable, Ex1, Ex2, Ex3, Ex4, Ex5, Ex6: Boolean;
  end;
  PBlockInfo = ^TBlockInfo;
  TBlockInfo = packed record
    title: String[20];
    typno: Byte;
    appno: Byte;
    nextid: Byte;
    total: Word;
  end;

  EInvalidMemcard = Class(Exception);

  { TMemCard }

  TMemCard = class(TObject)
  public
    constructor Create(fname: string; block_size: Integer);
    constructor Create(strm: TStream);
    destructor Destroy; override;
    procedure Format;
    function ReadBlock(blkid: Integer): TMemoryStream;
    procedure WriteBlock(blkid: Integer; blk: TStream);
    procedure WriteBlock(blkid: Integer; blk: TStream; info: PBlockInfo);
    procedure DeleteBlock(blkid: Integer);
    procedure GetInfo(blkid: Integer; info: PBlockInfo);
    procedure SetInfo(blkid: Integer; info: PBlockInfo);
    procedure Flush;
    function FindFree: Integer;
    function FindType(typno: Integer; info: PBlockInfo): Integer;
    function FindApp(appno: Integer; info: PBlockInfo): Integer;
    function BlockList: TStringList;
    procedure ReadStruct(blkid: Integer; var buf: Pointer; count: Integer);
    procedure WriteStruct(blkid: Integer; const buf: Pointer; count: Integer);
    function ReadString(blkid: Integer): string;
    procedure WriteString(blkid: Integer; const s: string);
    procedure SaveToStream(strm: TStream);
    procedure LoadFromStream(strm: TStream);
    procedure CopyBlock(blkid: Integer; target: TMemCard);
    procedure CopyCard(target: TMemCard);
    function LoadData(blkid: Integer): TMemoryStream;
    procedure SaveData(title: string; typno, appno: Integer; strm: TStream);
  private
    FMemory: TMemoryStream;
    FFileName: string;
    FBlockSize: Integer;
    FCardFlags: TCardFlags;
    function CheckHeader: Boolean;
  public
    property BlockSize: Integer read FBlockSize;
  end;

implementation

type
  TSignature = Array[0..3] of Char;

const
  MC_SIG: TSignature = ('K','M','C','*');
  MC_VER = 1;

type
  PHeader = ^THeader;
  THeader = packed record
    sig: TSignature;
    ver: Byte;
    block_size: Byte;
    flags: TCardFlags;
  end;

{ TMemCard }

constructor TMemCard.Create(fname: string; block_size: Integer);
begin
  FMemory:=TMemoryStream.Create;
  FFileName:=fname;
  FBlockSize:=block_size;
  if not FileExists(fname) then
    Format
  else
    FMemory.LoadFromFile(fname);
  if not CheckHeader then
    Raise EInvalidMemcard.Create('Invalid Memory Card selected!');
end;

constructor TMemCard.Create(strm: TStream);
begin
  FMemory:=TMemoryStream.Create;
  FFileName:='strm.dat';
  FBlockSize:=0;
  FMemory.LoadFromStream(strm);
  if not CheckHeader then
    Raise EInvalidMemcard.Create('Invalid Memory Card data from stream!');
end;

destructor TMemCard.Destroy;
begin
  FMemory.SaveToFile(FFileName);
  FMemory.Free;
  inherited Destroy;
end;

procedure TMemCard.Format;
var
  hdr: PHeader;
  info: PBlockInfo;
  i: Integer;
begin
  FMemory.SetSize(16*FBlockSize);
  FMemory.Position:=0;
  {$IFDEF DEBUG}
  FillChar(FMemory.Memory^, FMemory.Size, 0);
  {$ENDIF}
  New(hdr);
  hdr^.sig:=MC_SIG;
  hdr^.ver:=MC_VER;
  hdr^.block_size:=FBlockSize div 512;
  hdr^.flags.Encrypted:=False;
  hdr^.flags.Executable:=False;
  FCardFlags:=hdr^.flags;
  FMemory.Write(hdr^, SizeOf(hdr^));
  Dispose(hdr);
  New(info);
  With info^ do
  begin
    title:='NONAME';
    typno:=0;
    appno:=0;
    nextid:=0;
    total:=0;
  end;
  for i:=0 to 15 do
    FMemory.Write(info^, SizeOf(info^));
  Dispose(info);
  FMemory.SaveToFile(FFileName);
end;

function TMemCard.ReadBlock(blkid: Integer): TMemoryStream;
begin
  Result:=TMemoryStream.Create;
  Result.SetSize(FBlockSize);
  FMemory.Position:=blkid*FBlockSize;
  FMemory.Read(Result.Memory^, FBlockSize);
end;

procedure TMemCard.WriteBlock(blkid: Integer; blk: TStream);
begin
  FMemory.Position:=blkid*FBlockSize;
  blk.Position:=0;
  FMemory.CopyFrom(blk, FBlockSize);
end;

procedure TMemCard.WriteBlock(blkid: Integer; blk: TStream; info: PBlockInfo);
begin
  WriteBlock(blkid, blk);
  SetInfo(blkid, info);
end;

procedure TMemCard.DeleteBlock(blkid: Integer);
var
  info: PBlockInfo;
begin
  New(info);
  With info^ do
  begin
    title:='NONAME';
    typno:=0;
    appno:=0;
    nextid:=0;
    total:=0;
  end;
  SetInfo(blkid, info);
  Dispose(info);
end;

procedure TMemCard.GetInfo(blkid: Integer; info: PBlockInfo);
begin
  if (blkid < 1) or (blkid > 15) then
    Exit;
  FMemory.Position:=SizeOf(TBlockInfo)*blkid+SizeOf(THeader)-SizeOf(TBlockInfo);
  FMemory.Read(info^, SizeOf(info^));
end;

procedure TMemCard.SetInfo(blkid: Integer; info: PBlockInfo);
begin
  if (blkid < 1) or (blkid > 15) then
    Exit;
  FMemory.Position:=SizeOf(TBlockInfo)*blkid+SizeOf(THeader)-SizeOf(TBlockInfo);
  FMemory.Write(info^, SizeOf(info^));
end;

procedure TMemCard.Flush;
begin
  FMemory.SaveToFile(FFileName);
end;

function TMemCard.FindFree: Integer;
var
  info: PBlockInfo;
begin
  New(info);
  Result:=FindType(0, info);
  if info^.appno <> 0 then
    Result:=0;
  Dispose(info);
end;

function TMemCard.FindType(typno: Integer; info: PBlockInfo): Integer;
var
  i: Integer;
begin
  i:=0;
  FMemory.Position:=SizeOf(THeader);
  repeat
    FMemory.Read(info^, SizeOf(info^));
    Inc(i);
  until (info^.typno = typno) or (i > 15);
  if i > 15 then
    i:=0;
  Result:=i;
end;

function TMemCard.FindApp(appno: Integer; info: PBlockInfo): Integer;
var
  i: Integer;
begin
  i:=0;
  FMemory.Position:=SizeOf(THeader);
  repeat
    FMemory.Read(info^, SizeOf(info^));
    Inc(i);
  until (info^.appno = appno) or (i > 15);
  if i > 15 then
    i:=0;
  Result:=i;
end;

function TMemCard.BlockList: TStringList;
var
  i: Integer;
  info: PBlockInfo;
begin
  FMemory.Position:=SizeOf(THeader);
  Result:=TStringList.Create;
  New(info);
  for i:=1 to 15 do
  begin
    FMemory.Read(info^, SizeOf(info^));
    Result.Add(info^.title);
  end;
  Dispose(info);
end;

procedure TMemCard.ReadStruct(blkid: Integer; var buf: Pointer; count: Integer);
begin
  FMemory.Position:=blkid*FBlockSize;
  FMemory.Read(buf^, count);
end;

procedure TMemCard.WriteStruct(blkid: Integer; const buf: Pointer; count: Integer);
begin
  FMemory.Position:=blkid*FBlockSize;
  FMemory.Write(buf^, count);
end;

function TMemCard.ReadString(blkid: Integer): string;
begin
  FMemory.Position:=blkid*FBlockSize;
  Result:=FMemory.ReadAnsiString;
end;

procedure TMemCard.WriteString(blkid: Integer; const s: string);
begin
  FMemory.Position:=blkid*FBlockSize;
  FMemory.WriteAnsiString(s);
end;

procedure TMemCard.SaveToStream(strm: TStream);
begin
  FMemory.SaveToStream(strm);
end;

procedure TMemCard.LoadFromStream(strm: TStream);
begin
  FMemory.LoadFromStream(strm);
  if CheckHeader then
    WriteLn('Header OK.');
end;

procedure TMemCard.CopyBlock(blkid: Integer; target: TMemCard);
var
  blk: TMemoryStream;
  info: PBlockInfo;
  nb: Integer;
begin
  nb:=target.FindFree;
  if nb < 1 then
    Exit;
  New(info);
  GetInfo(blkid, info);
  blk:=ReadBlock(blkid);
  target.WriteBlock(nb, blk, info);
  blk.Free;
  Dispose(info);
end;

procedure TMemCard.CopyCard(target: TMemCard);
var
  blk: TMemoryStream;
  info: PBlockInfo;
  nb, i: Integer;
begin
  blk:=TMemoryStream.Create;
  blk.SetSize(FBlockSize);
  New(info);
  for i:=1 to 15 do
  begin
    nb:=target.FindFree;
    if nb < 1 then
      Break;
    GetInfo(i, info);
    FMemory.Position:=i*FBlockSize;
    FMemory.Read(blk.Memory^, FBlockSize);
    target.WriteBlock(nb, blk, info);
  end;
  Dispose(info);
  blk.Free;
end;

function TMemCard.LoadData(blkid: Integer): TMemoryStream;
var
  info: PBlockInfo;
  bid: Byte;
begin
  New(info);
  GetInfo(blkid, info);
  Result:=TMemoryStream.Create;
  if info^.total > FBlockSize then
    Result.SetSize(info^.total)
  else
    Result.SetSize(FBlockSize);
  bid:=blkid;
  repeat
    FMemory.Position:=bid*FBlockSize;
    Result.Write(FMemory.Memory^, FBlockSize);
    bid:=info^.nextid;
    if bid > 0 then
      GetInfo(bid, info);
  until bid = 0;
  Dispose(info);
end;

procedure TMemCard.SaveData(title: string; typno, appno: Integer; strm: TStream
  );
var
  info: PBlockInfo;
  blkid: Integer;
  total, i: Integer;
begin
  blkid:=FindFree;
  if blkid < 1 then
    Exit;
  New(info);
  info^.title:=title;
  info^.typno:=typno;
  info^.appno:=appno;
  info^.nextid:=0;
  info^.total:=strm.Size;
  total:=info^.total div FBlockSize;
  strm.Position:=0;
  for i:=0 to total do
  begin
    info^.nextid:=FindFree;
    SetInfo(blkid, info);
    FMemory.Position:=blkid*FBlockSize;
    FMemory.CopyFrom(strm, FBlockSize);
  end;
  info^.nextid:=0;
  SetInfo(blkid, info);
  Dispose(info);
end;

function TMemCard.CheckHeader: Boolean;
var
  hdr: PHeader;
begin
  New(hdr);
  FMemory.Position:=0;
  FMemory.Read(hdr^, SizeOf(hdr^));
  FBlockSize:=hdr^.block_size*512;
  FCardFlags:=hdr^.flags;
  if hdr^.sig = MC_SIG then
    Result:=True
  else
    Result:=False;
  Dispose(hdr);
  if FCardFlags.Encrypted then
    WriteLn('Encrypted Card detected.');
  if FCardFlags.Executable then
    WriteLn('Executable Card detected.');
end;

end.

