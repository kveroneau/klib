unit Storage6502;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dev6502;

type

  { T6502Storage }

  T6502Storage = class(T6502Device)
  private
    FFileName: string;
    FAddr: word;
    FPages: byte;
    FLoadOnStart: Boolean;
    procedure LoadInFile;
    procedure SaveOutFile;
  protected
    function GetDeviceType: byte; override;
    procedure SetMemory(AValue: TBytesStream); override;
  public
    procedure DeviceRun; override;
  published
    property LoadOnStart: Boolean read FLoadOnStart write FLoadOnStart;
    property FileName: string read FFileName write FFileName;
    property Address: word read FAddr write FAddr;
    property Pages: byte read FPages write FPages;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MOS 6502',[T6502Storage]);
end;

{ T6502Storage }

procedure T6502Storage.LoadInFile;
var
  s: TMemoryStream;
begin
  s:=TMemoryStream.Create;
  try
    s.LoadFromFile(FFileName);
    s.Read(FMemory.Bytes[FAddr], FPages*256);
  finally
    s.Free;
  end;
end;

procedure T6502Storage.SaveOutFile;
var
  s: TMemoryStream;
begin
  s:=TMemoryStream.Create;
  try
    s.Write(FMemory.Bytes[FAddr], FPages*256);
    s.SaveToFile(FFileName);
  finally
    s.Free;
  end;
end;

function T6502Storage.GetDeviceType: byte;
begin
  Result:=$d0;
end;

procedure T6502Storage.SetMemory(AValue: TBytesStream);
begin
  inherited SetMemory(AValue);
  if Assigned(AValue) and FLoadOnStart then
    LoadInFile;
end;

procedure T6502Storage.DeviceRun;
begin
  if FMemory.Bytes[$cffa] = $40 then
    LoadInFile
  else if FMemory.Bytes[$cffa] = $60 then
    SaveOutFile;
end;

end.
