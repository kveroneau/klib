{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit klibLaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  LazMemcard, LazNetcard, LazKClient, Konsole, Laz6502, KevShell, NetMemo, 
  EGAConsole, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazMemcard', @LazMemcard.Register);
  RegisterUnit('LazNetcard', @LazNetcard.Register);
  RegisterUnit('LazKClient', @LazKClient.Register);
  RegisterUnit('Konsole', @Konsole.Register);
  RegisterUnit('Laz6502', @Laz6502.Register);
  RegisterUnit('KevShell', @KevShell.Register);
  RegisterUnit('NetMemo', @NetMemo.Register);
  RegisterUnit('EGAConsole', @EGAConsole.Register);
end;

initialization
  RegisterPackage('klibLaz', @Register);
end.
