unit SegmentCard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Card6502, SegmentDisplay;

type

  { TSegmentCard }

  TSegmentCard = class(T6502Card)
  private
    FSegment1: TSegmentDisplay;
    FSegment2: TSegmentDisplay;
    FSegment3: TSegmentDisplay;
    FSegment4: TSegmentDisplay;
  protected
    function GetCardType: byte; override;
  public
    procedure CardRun; override;
  published
    property Segment1: TSegmentDisplay read FSegment1 write FSegment1;
    property Segment2: TSegmentDisplay read FSegment2 write FSegment2;
    property Segment3: TSegmentDisplay read FSegment3 write FSegment3;
    property Segment4: TSegmentDisplay read FSegment4 write FSegment4;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MOS 6502',[TSegmentCard]);
end;

{ TSegmentCard }

function TSegmentCard.GetCardType: byte;
begin
  Result:=42;
end;

procedure TSegmentCard.CardRun;
var
  d: byte;
begin
  d:=Memory^[$01];
  if d < 10 then
    FSegment1.Digit:=d;
  d:=Memory^[$02];
  if d < 10 then
    FSegment2.Digit:=d;
  d:=Memory^[$03];
  if d < 10 then
    FSegment3.Digit:=d;
  d:=Memory^[$04];
  if d < 10 then
    FSegment4.Digit:=d;
  inherited CardRun;
end;

end.
