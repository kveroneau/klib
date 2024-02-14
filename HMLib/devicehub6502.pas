unit DeviceHub6502;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dev6502;

type

  { T6502DeviceHub }

  T6502DeviceHub = class(T6502Device)
  private
    FDevices: Array[0..7] of T6502Device;
    function GetDevice(i: integer): T6502Device;
    procedure SetDevice(i: integer; AValue: T6502Device);
  protected
    procedure SetMemory(AValue: TBytesStream); override;
    function GetDeviceType: byte; override;
  public
    property Device[i: integer]: T6502Device read GetDevice write SetDevice;
    procedure DeviceRun; override;
    function FindType(devtype: byte): T6502Device;
  published
    property Device0: T6502Device read FDevices[0] write FDevices[0];
    property Device1: T6502Device read FDevices[1] write FDevices[1];
    property Device2: T6502Device read FDevices[2] write FDevices[2];
    property Device3: T6502Device read FDevices[3] write FDevices[3];
    property Device4: T6502Device read FDevices[4] write FDevices[4];
    property Device5: T6502Device read FDevices[5] write FDevices[5];
    property Device6: T6502Device read FDevices[6] write FDevices[6];
    property Device7: T6502Device read FDevices[7] write FDevices[7];
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MOS 6502',[T6502DeviceHub]);
end;

{ T6502DeviceHub }

function T6502DeviceHub.GetDevice(i: integer): T6502Device;
begin
  Result:=FDevices[i];
end;

procedure T6502DeviceHub.SetDevice(i: integer; AValue: T6502Device);
begin
  FDevices[i]:=AValue;
end;

procedure T6502DeviceHub.SetMemory(AValue: TBytesStream);
var
  i: integer;
begin
  inherited SetMemory(AValue);
  if AValue = Nil then
  begin
    for i:=0 to 7 do
      if Assigned(FDevices[i]) then
        FDevices[i].Memory:=Nil;
  end
  else
  begin
    for i:=0 to 7 do
      if Assigned(FDevices[i]) then
        FDevices[i].Memory:=AValue;
  end;
end;

function T6502DeviceHub.GetDeviceType: byte;
begin
  Result:=$80;
end;

procedure T6502DeviceHub.DeviceRun;
var
  i: Integer;
begin
  for i:=0 to 7 do
    if Assigned(FDevices[i]) then
      FDevices[i].DeviceRun;
end;

function T6502DeviceHub.FindType(devtype: byte): T6502Device;
var
  i: integer;
begin
  Result:=Nil;
  for i:=0 to 7 do
    if Assigned(FDevices[i]) and (FDevices[i].DeviceType = devtype) then
    begin
      Result:=FDevices[i];
      Exit;
    end;
end;

end.
