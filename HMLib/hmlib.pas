{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit HMLib;

{$warn 5023 off : no warning about unused units}
interface

uses
  MOS6502, Dev6502, Card6502, TerminalCard, SegmentCard, Memory6502, rom6502, 
  thread6502, Storage6502, CardSlots6502, DeviceHub6502, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('MOS6502', @MOS6502.Register);
  RegisterUnit('Dev6502', @Dev6502.Register);
  RegisterUnit('Card6502', @Card6502.Register);
  RegisterUnit('TerminalCard', @TerminalCard.Register);
  RegisterUnit('SegmentCard', @SegmentCard.Register);
  RegisterUnit('Memory6502', @Memory6502.Register);
  RegisterUnit('rom6502', @rom6502.Register);
  RegisterUnit('Storage6502', @Storage6502.Register);
  RegisterUnit('CardSlots6502', @CardSlots6502.Register);
  RegisterUnit('DeviceHub6502', @DeviceHub6502.Register);
end;

initialization
  RegisterPackage('HMLib', @Register);
end.
