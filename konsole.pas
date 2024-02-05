unit Konsole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  StrUtils;

type

  TKonsoleEvent = procedure(Sender: TObject; cmd, params: string) of object;

  TKonsoleMode = (kmCommand, kmEcho, kmRaw);

  { TKonsole }

  TKonsole = class(TMemo)
  private
    FEnforceCaps: Boolean;
    FKonsoleMode: TKonsoleMode;
    FOnCharacter: TKeyPressEvent;
    FOnCommand: TKonsoleEvent;
    FPrompt: string;
    procedure KonsoleKeyPress(Sender: TObject; var Key: char);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    procedure SetColor(fg, bg: TColor);
    procedure WriteLn(const s: string);
    procedure Write(const s: string);
    procedure ClrScr;
  published
    property OnCommand: TKonsoleEvent read FOnCommand write FOnCommand;
    property KonsoleMode: TKonsoleMode read FKonsoleMode write FKonsoleMode;
    property EnforceCaps: Boolean read FEnforceCaps write FEnforceCaps;
    property OnCharacter: TKeyPressEvent read FOnCharacter write FOnCharacter;
    property Prompt: string read FPrompt write FPrompt;
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
  if FKonsoleMode <> kmCommand then
    if Assigned(FOnCharacter) then
    begin
      if FEnforceCaps then
        if (Ord(Key) > 96) and (Ord(Key) < 123) then
          Key:=Chr(Ord(Key)-32);
      FOnCharacter(Self, Key);
      if FKonsoleMode = kmRaw then
        Key:=#0;
      Exit;
    end;
  if Key <> #13 then
    Exit;
  if not Assigned(FOnCommand) then
    Exit;
  if (Length(FPrompt) > 0) and (LeftStr(Lines.Strings[Lines.Count-1], Length(FPrompt)) = FPrompt) then
    params:=RightStr(Lines.Strings[Lines.Count-1], Length(Lines.Strings[Lines.Count-1])-Length(FPrompt))
  else
    params:=Lines.Strings[Lines.Count-1];
  cmd:=Copy2SpaceDel(params);
  FOnCommand(Self, cmd, params);
  Key:=#0;
  Lines.Strings[Lines.Count]:=FPrompt;
end;

constructor TKonsole.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnKeyPress:=@KonsoleKeyPress;
  FKonsoleMode:=kmCommand;
  FEnforceCaps:=False;
  Font.Name:='Monospace';
  Font.Size:=15;
  Font.Color:=clWhite;
  Color:=clBlack;
  FPrompt:='';
end;

procedure TKonsole.SetColor(fg, bg: TColor);
begin
  Font.Color:=VGAPalette[fg];
  Color:=VGAPalette[bg];
end;

procedure TKonsole.WriteLn(const s: string);
begin
  Lines.Add(s);
  Application.ProcessMessages;
end;

procedure TKonsole.Write(const s: string);
var
  prnt: boolean;
begin
  prnt:=True;
  if Length(s) = 1 then
  begin
    prnt:=False;
    case Ord(s[1]) of
      $a: Lines.Add('');
      $d: Lines.Add('');
      $8: Lines.Strings[Lines.Count-1]:=LeftStr(Lines.Strings[Lines.Count-1], Length(Lines.Strings[Lines.Count-1])-1);
      $ff: ClrScr;
    else
      prnt:=True;
    end;
  end;
  if prnt then
    Lines.Strings[Lines.Count-1]:=Lines.Strings[Lines.Count-1]+s;
  Application.ProcessMessages;
end;

procedure TKonsole.ClrScr;
begin
  Lines.Clear;
  Application.ProcessMessages;
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
