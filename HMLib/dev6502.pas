unit Dev6502;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { T6502Device }

  T6502Device = class(TComponent)
  private

  protected
    FMemory: TBytesStream;
    procedure SetMemory(AValue: TBytesStream); virtual;
    function GetDeviceType: byte; virtual; abstract;
  public
    property DeviceType: byte read GetDeviceType;
    property Memory: TBytesStream write SetMemory;
    procedure DeviceRun; virtual; abstract;
  published

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MOS 6502',[T6502Device]);
end;

{ T6502Device }

procedure T6502Device.SetMemory(AValue: TBytesStream);
begin
  FMemory:=AValue;
end;

end.
