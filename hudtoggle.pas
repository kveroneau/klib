unit HUDToggle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { THUDToggle }

  THUDToggle = class(TCustomControl)
  private
    FState: Boolean;
    FCaption: string;
    FImage: TImage;
    FToggleOn, FToggleOff: TNotifyEvent;
    procedure ClickHandler(Sender: TObject);
    procedure SetState(value: Boolean);
    procedure SetCaption(value: string);
    procedure RepaintImage;
  protected

  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property State: Boolean read FState write SetState;
    property ToggleOn: TNotifyEvent read FToggleOn write FToggleOn;
    property ToggleOff: TNotifyEvent read FToggleOff write FToggleOff;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HUD',[THUDToggle]);
end;

{ THUDToggle }

procedure THUDToggle.ClickHandler(Sender: TObject);
begin
  if FState then
  begin
    FState:=False;
    if Assigned(FToggleOff) then
      FToggleOff(Self);
  end
  else
  begin
    FState:=True;
    if Assigned(FToggleOn) then
      FToggleOn(Self);
  end;
  RepaintImage;
end;

procedure THUDToggle.SetState(value: Boolean);
begin
  FState:=value;
  RepaintImage;
end;

procedure THUDToggle.SetCaption(value: string);
begin
  FCaption:=value;
  RepaintImage;
end;

procedure THUDToggle.RepaintImage;
begin
  with FImage.Canvas do
  begin
    if FState then
      Brush.Color:=clGreen
    else
      Brush.Color:=clRed;
    Clear;
    Pen.Color:=clWhite;
    TextOut(20, Height div 2, FCaption);
  end;
end;

constructor THUDToggle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImage:=TImage.Create(Self);
  FImage.Parent:=Self;
  FImage.ControlStyle:=FImage.ControlStyle-[csNoDesignSelectable];
  FImage.Height:=40;
  FImage.Width:=200;
  FState:=False;
  FImage.OnClick:=@ClickHandler;
  Height:=40;
  Width:=200;
  FCaption:='HUDToggle';
  FImage.Canvas.Font.Color:=clWhite;
  FImage.Canvas.Font.Style:=[fsBold];
  RepaintImage;
end;

end.
