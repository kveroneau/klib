unit TerminalCard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Card6502, StdCtrls;

type

  { TTerminalCard }

  TTerminalCard = class(T6502Card)
  private
    FTerminal: TMemo;
    procedure SetTerminal(AValue: TMemo);
    procedure TerminalKeyPress(Sender: TObject; var Key: char);
    procedure TerminalInit(Sender: TObject);
  protected
    function GetCardType: byte; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CardRun; override;
  published
    property Memo: TMemo read FTerminal write SetTerminal;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MOS 6502',[TTerminalCard]);
end;

{ TTerminalCard }

procedure TTerminalCard.TerminalKeyPress(Sender: TObject; var Key: char);
begin
  Memory^[$02]:=ord(Key);
end;

procedure TTerminalCard.SetTerminal(AValue: TMemo);
begin
  if FTerminal=AValue then Exit;
  FTerminal:=AValue;
  FTerminal.OnKeyPress:=@TerminalKeyPress;
end;

procedure TTerminalCard.TerminalInit(Sender: TObject);
begin
  if Assigned(FTerminal) then
    FTerminal.Clear;
end;

function TTerminalCard.GetCardType: byte;
begin
  Result:=$42;
end;

constructor TTerminalCard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnInitCard:=@TerminalInit;
end;

procedure TTerminalCard.CardRun;
begin
  if Memory^[$04] > 0 then
  begin
    if Assigned(FTerminal) then
      with FTerminal.Lines do
        Strings[Count-1]:=Strings[Count-1]+chr(Memory^[$04]);
  end
  else if Memory^[$05] > 0 then
  begin
    if Assigned(FTerminal) then
      with FTerminal.Lines do
        Strings[Count-1]:=Strings[Count-1]+HexStr(Memory^[$05], 2);
  end;
  inherited CardRun;
  Memory^[$04]:=0;
  Memory^[$05]:=0;
end;

end.
