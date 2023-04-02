unit Konsole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  StrUtils;

type

  TKonsoleEvent = procedure(Sender: TObject; cmd, params: string) of object;

  { TKonsole }

  TKonsole = class(TMemo)
  private
    FOnCommand: TKonsoleEvent;
    procedure KonsoleKeyPress(Sender: TObject; var Key: char);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    procedure SetColor(fg, bg: TColor);
    procedure WriteLn(const s: string);
    procedure ClrScr;
  published
    property OnCommand: TKonsoleEvent read FOnCommand write FOnCommand;
  end;

procedure Register;

implementation

var
  VGAPalette: Array[0..15] of TColor;

procedure Register;
begin
  {$I Konsole_icon.lrs }
  RegisterComponents('klib',[TKonsole]);
end;

{ TKonsole }

procedure TKonsole.KonsoleKeyPress(Sender: TObject; var Key: char);
var
  cmd, params: string;
begin
  if Key <> #13 then
    Exit;
  if not Assigned(FOnCommand) then
    Exit;
  params:=Lines.Strings[Lines.Count-1];
  cmd:=Copy2SpaceDel(params);
  FOnCommand(Self, cmd, params);
  Key:=#0;
end;

constructor TKonsole.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnKeyPress:=@KonsoleKeyPress;
  Font.Name:='Monospace';
  Font.Size:=15;
  Font.Color:=clWhite;
  Color:=clBlack;
end;

procedure TKonsole.SetColor(fg, bg: TColor);
begin
  Font.Color:=VGAPalette[fg];
  Color:=VGAPalette[bg];
end;

procedure TKonsole.WriteLn(const s: string);
begin
  Lines.Add(s);
end;

procedure TKonsole.ClrScr;
begin
  Lines.Clear;
end;

initialization
  VGAPalette[0]:=clBlack;
  VGAPalette[1]:=clBlue;
  VGAPalette[2]:=clGreen;
  VGAPalette[3]:=clTeal;
  VGAPalette[4]:=clMaroon;
  VGAPalette[5]:=clFuchsia;
  VGAPalette[6]:=clNavy;
  VGAPalette[7]:=clGray;
  VGAPalette[8]:=clDkGray;
  VGAPalette[9]:=clSkyBlue;
  VGAPalette[10]:=clLime;
  VGAPalette[11]:=clAqua;
  VGAPalette[12]:=clRed;
  VGAPalette[13]:=clMoneyGreen;
  VGAPalette[14]:=clYellow;
  VGAPalette[15]:=clWhite;

end.
