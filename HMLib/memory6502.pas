unit Memory6502;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, rom6502;

type

  { T6502Memory }

  T6502Memory = class(TComponent)
  private
    FActive: Boolean;
    FMemory: TBytesStream;
    FROMs: Array[0..3] of T6502ROM;
    function GetMemory: TBytesStream;
    procedure LoadInROMs;
    procedure SetActive(AValue: Boolean);
  protected

  public
    destructor Destroy; override;
    property Memory: TBytesStream read GetMemory;
  published
    property Active: Boolean read FActive write SetActive;
    property ROM0: T6502ROM read FROMs[0] write FROMs[0];
    property ROM1: T6502ROM read FROMs[1] write FROMs[1];
    property ROM2: T6502ROM read FROMs[2] write FROMs[2];
    property ROM3: T6502ROM read FROMs[3] write FROMs[3];
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MOS 6502',[T6502Memory]);
end;

{ T6502Memory }

procedure T6502Memory.LoadInROMs;
var
  i: integer;
begin
 for i:=0 to 3 do
   if Assigned(FROMs[i]) then
     if FROMs[i].Active then
       FROMs[i].ROMStream.Read(FMemory.Bytes[FROMs[i].Address], FROMs[i].ROMStream.Size);
end;

procedure T6502Memory.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  if AValue then
  begin
    FMemory:=TBytesStream.Create;
    FMemory.Size:=65536;
    LoadInROMs;
  end
  else if Assigned(FMemory) then
    FreeAndNil(FMemory);
  FActive:=AValue;
end;

destructor T6502Memory.Destroy;
begin
  if Assigned(FMemory) then
    Active:=False;
  inherited Destroy;
end;

function T6502Memory.GetMemory: TBytesStream;
begin
  if not Assigned(FMemory) then
    Active:=True;
  Result:=FMemory;
end;

end.
