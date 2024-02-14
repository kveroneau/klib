unit thread6502;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MOS6502, CardSlots6502, DeviceHub6502;

type

  { T6502Thread }

  T6502Thread = class(TThread)
  private
    F6502: TMOS6502;
  protected
    procedure Execute; override;
  public
    constructor Create(cpu: TMOS6502);
  end;

implementation

{ T6502Thread }

procedure T6502Thread.Execute;
var
  i: Integer;
  slots: T6502CardSlots;
  hub: T6502DeviceHub;
begin
  F6502.Threaded:=True;
  slots:=Nil;
  hub:=Nil;
  if F6502.Device.DeviceType = $c0 then
    slots:=F6502.Device as T6502CardSlots
  else if F6502.Device.DeviceType = $80 then
    hub:=F6502.Device as T6502DeviceHub;
  F6502.Running:=True;
  repeat
    F6502.Step;
    for i:=0 to 7 do
      if Assigned(slots) and Assigned(slots.Card[i]) then
        Synchronize(@slots.Card[i].CardRun)
      else if Assigned(hub) and Assigned(hub.Device[i]) then
        Synchronize(@hub.Device[i].DeviceRun);
    Sleep(10);
  until not F6502.Running;
  F6502.Threaded:=False;
end;

constructor T6502Thread.Create(cpu: TMOS6502);
begin
  inherited Create(True);
  F6502:=cpu;
end;

end.

