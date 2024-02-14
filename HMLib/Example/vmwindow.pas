unit VMWindow;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, MOS6502, Dev6502,
  TerminalCard, SegmentCard, Memory6502, rom6502, Konsole, SegmentDisplay,
  thread6502, CardSlots6502, DeviceHub6502, Storage6502;

type

  { TVMForm }

  TVMForm = class(TForm)
    Konsole1: TKonsole;
    MOS6502_1: TMOS6502;
    SegmentCard1: TSegmentCard;
    SegmentDisplay1: TSegmentDisplay;
    SegmentDisplay2: TSegmentDisplay;
    SegmentDisplay3: TSegmentDisplay;
    SegmentDisplay4: TSegmentDisplay;
    T6502CardSlots1: T6502CardSlots;
    T6502DeviceHub1: T6502DeviceHub;
    T6502Memory1: T6502Memory;
    T6502ROM1: T6502ROM;
    T6502Storage1: T6502Storage;
    TerminalCard1: TTerminalCard;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FThread: T6502Thread;
  public

  end;

var
  VMForm: TVMForm;

implementation

{$R *.lfm}

{ TVMForm }

procedure TVMForm.FormCreate(Sender: TObject);
begin
  FThread:=T6502Thread.Create(MOS6502_1);
end;

procedure TVMForm.FormDestroy(Sender: TObject);
begin
  if not FThread.Finished then
  begin
    MOS6502_1.Running:=False;
    repeat
      Konsole1.Write('.');
    until FThread.Finished;
    FThread.Free;
  end;
  MOS6502_1.Active:=False;
end;

procedure TVMForm.FormShow(Sender: TObject);
begin
  MOS6502_1.Active:=True;
  FThread.Start;
end;

end.

