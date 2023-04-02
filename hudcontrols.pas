{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit HUDControls;

{$warn 5023 off : no warning about unused units}
interface

uses
  SegmentDisplay, HUDToggle, HUDButton, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('SegmentDisplay', @SegmentDisplay.Register);
  RegisterUnit('HUDToggle', @HUDToggle.Register);
  RegisterUnit('HUDButton', @HUDButton.Register);
end;

initialization
  RegisterPackage('HUDControls', @Register);
end.
