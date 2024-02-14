unit CardSlots6502;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dev6502, Card6502;

type

  PIOMemory = ^TIOMemory;
  TIOMemory = Array[0..7] of TCardMemory;
  PCardInfo = ^TCardInfo;
  TCardInfo = Array[0..7] of byte;

  { T6502CardSlots }

  T6502CardSlots = class(T6502Device)
  private
    FIOMemory: PIOMemory;
    FCards: Array[0..7] of T6502Card;
    function GetCard(i: integer): T6502Card;
    procedure SetCard(i: integer; AValue: T6502Card);
  protected
    procedure SetMemory(AValue: TBytesStream); override;
    function GetDeviceType: byte; override;
  public
    property Card[i: integer]: T6502Card read GetCard write SetCard;
    procedure DeviceRun; override;
  published
    property Card0: T6502Card read FCards[0] write FCards[0];
    property Card1: T6502Card read FCards[1] write FCards[1];
    property Card2: T6502Card read FCards[2] write FCards[2];
    property Card3: T6502Card read FCards[3] write FCards[3];
    property Card4: T6502Card read FCards[4] write FCards[4];
    property Card5: T6502Card read FCards[5] write FCards[5];
    property Card6: T6502Card read FCards[6] write FCards[6];
    property Card7: T6502Card read FCards[7] write FCards[7];
  end;

procedure Register;

implementation

const
  CARD_IO = $c000;

procedure Register;
begin
  RegisterComponents('MOS 6502',[T6502CardSlots]);
end;

{ T6502CardSlots }

function T6502CardSlots.GetCard(i: integer): T6502Card;
begin
  Result:=FCards[i];
end;

procedure T6502CardSlots.SetCard(i: integer; AValue: T6502Card);
begin
  FCards[i]:=AValue;
end;

procedure T6502CardSlots.SetMemory(AValue: TBytesStream);
var
  i: integer;
  cardio: PCardInfo;
begin
  inherited SetMemory(AValue);
  if AValue = Nil then
  begin
    for i:=0 to 7 do
      if FCards[i] <> Nil then
        FCards[i].Memory:=Nil;
  end
  else
  begin
    cardio:=@FMemory.Bytes[$c800];
    for i:=0 to 7 do
      if FCards[i] <> Nil then
      begin
        FCards[i].Memory:=@FMemory.Bytes[CARD_IO+(i*SizeOf(TCardMemory))];
        cardio^[i]:=FCards[i].CardType;
      end
      else
        cardio^[i]:=0;
  end;
end;

function T6502CardSlots.GetDeviceType: byte;
begin
  Result:=$c0;
end;

procedure T6502CardSlots.DeviceRun;
var
  i: integer;
begin
  for i:=0 to 7 do
    if FCards[i] <> Nil then
      FCards[i].CardRun;
end;

end.
