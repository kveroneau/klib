unit SegmentDisplay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  TSegmentFlags = (sfA, sfB, sfC, sfD, sfE, sfF, sfG);
  TSegmentFlag = set of TSegmentFlags;

  { TSegmentDisplay }

  TSegmentDisplay = class(TGraphicControl)
  private
    FSegments: TSegmentFlag;
    FDigit: integer;
    FBackground, FForeground: TColor;
    procedure SetDigit(value: integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Digit: integer read FDigit write SetDigit;
    property BackgroundColor: TColor read FBackground write FBackground;
    property ForegroundColor: TColor read FForeground write FForeground;
    property OnClick;
  end;

procedure Register;

implementation

const
  DIGIT_0: TSegmentFlag = [sfA, sfC, sfD, sfE, sfF, sfG];
  DIGIT_1: TSegmentFlag = [sfD, sfE];
  DIGIT_2: TSegmentFlag = [sfA, sfB, sfC, sfE, sfF];
  DIGIT_3: TSegmentFlag = [sfA, sfB, sfC, sfF, sfG];
  DIGIT_4: TSegmentFlag = [sfB, sfD, sfF, sfG];
  DIGIT_5: TSegmentFlag = [sfA, sfB, sfC, sfD, sfG];
  DIGIT_6: TSegmentFlag = [sfA, sfB, sfC, sfD, sfE, sfG];
  DIGIT_7: TSegmentFlag = [sfA, sfF, sfG];
  DIGIT_8: TSegmentFlag = [sfA, sfB, sfC, sfD, sfE, sfF, sfG];
  DIGIT_9: TSegmentFlag = [sfA, sfB, sfC, sfD, sfF, sfG];

procedure Register;
begin
  RegisterComponents('HUD',[TSegmentDisplay]);
end;

{ TSegmentDisplay }

procedure TSegmentDisplay.SetDigit(value: integer);
begin
  FDigit:=value;
  case value of
    0: FSegments:=DIGIT_0;
    1: FSegments:=DIGIT_1;
    2: FSegments:=DIGIT_2;
    3: FSegments:=DIGIT_3;
    4: FSegments:=DIGIT_4;
    5: FSegments:=DIGIT_5;
    6: FSegments:=DIGIT_6;
    7: FSegments:=DIGIT_7;
    8: FSegments:=DIGIT_8;
    9: FSegments:=DIGIT_9;
  end;
  Invalidate;
end;

procedure TSegmentDisplay.Paint;
begin
  inherited Paint;
  with Canvas do
  begin
    Brush.Color:=FBackground;
    Clear;
    Brush.Color:=FForeground;
    Pen.Color:=clRed;
    if sfA in FSegments then
      Rectangle(10,10,40,20);
    if sfB in FSegments then
      Rectangle(10,40,40,50);
    if sfC in FSegments then
      Rectangle(10,70,40,80);
    if sfD in FSegments then
      Rectangle(0,20,10,40);
    if sfE in FSegments then
      Rectangle(0,50,10,70);
    if sfF in FSegments then
      Rectangle(40,20,50,40);
    if sfG in FSegments then
      Rectangle(40,50,50,70);
  end;
end;

constructor TSegmentDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackground:=clBlack;
  FForeground:=clWhite;
  FDigit:=0;
  Width:=50;
  Height:=90;
end;

end.
