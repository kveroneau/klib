unit proto1main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, VMWindow;

type

  { TPrototypeMainForm }

  TPrototypeMainForm = class(TForm)
    Button1: TButton;
    VM1Enabled: TCheckBox;
    VM2Enabled: TCheckBox;
    VM3Enabled: TCheckBox;
    VM4Enabled: TCheckBox;
    VM5Enabled: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure VMClicked(Sender: TObject);
  private
    FVMs: Array[1..5] of TVMForm;
  public

  end;

var
  PrototypeMainForm: TPrototypeMainForm;

implementation

{$R *.lfm}

{ TPrototypeMainForm }

procedure TPrototypeMainForm.VMClicked(Sender: TObject);
var
  vm: Integer;
  chk: TCheckBox;
begin
  chk:=Sender as TCheckBox;
  vm:=StrToInt(RightStr(chk.Caption, 1));
  if chk.Checked then
  begin
    FVMs[vm]:=TVMForm.Create(Self);
    FVMs[vm].Show;
  end
  else
    FreeAndNil(FVMs[vm]);
end;

procedure TPrototypeMainForm.Button1Click(Sender: TObject);
var
  i: integer;
begin
  for i:=0 to 20 do
  with TVMForm.Create(Self) do
  begin
    Top:=Random(500);
    Left:=Random(800);
    Show;
  end;
end;

end.

