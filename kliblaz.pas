{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit klibLaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  Konsole, Laz6502, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Konsole', @Konsole.Register);
  RegisterUnit('Laz6502', @Laz6502.Register);
end;

initialization
  RegisterPackage('klibLaz', @Register);
end.
