unit 6502Device;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  T6502Device = class(TComponent)
  private

  protected

  public

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MOS 6502',[T6502Device]);
end;

end.
