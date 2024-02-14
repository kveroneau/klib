unit rom6502;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type

  { T6502ROM }

  T6502ROM = class(TComponent)
  private
    FActive: Boolean;
    FAddress: word;
    FROMFile: string;
    FROMStream: TStream;
    function GetROMStream: TStream;
  protected

  public
    destructor Destroy; override;
    property ROMStream: TStream read GetROMStream write FROMStream;
  published
    property Active: Boolean read FActive write FActive;
    property Address: word read FAddress write FAddress;
    property ROMFile: string read FROMFile write FROMFile;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MOS 6502',[T6502ROM]);
end;

{ T6502ROM }

function T6502ROM.GetROMStream: TStream;
var
  s: TMemoryStream;
begin
  if Assigned(FROMStream) then
  begin
    FROMStream.Position:=0;
    Result:=FROMStream;
  end
  else if FileExists(FROMFile) then
  begin
    s:=TMemoryStream.Create;
    s.LoadFromFile(FROMFile);
    FROMStream:=s;
    Result:=s;
  end
  else
    Result:=Nil;
end;

destructor T6502ROM.Destroy;
begin
  if Assigned(FROMStream) then
    FROMStream.Free;
  inherited Destroy;
end;

end.
