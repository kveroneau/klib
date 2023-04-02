unit HUDButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { THUDButton }

  THUDButton = class(TCustomControl)
  private
    FCaption: String;
    FImage: TImage;
    FOnClick: TNotifyEvent;
    procedure ClickHandler(Sender: TObject);
    procedure RepaintImage;
    procedure SetCaption(value: string);
  protected

  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HUD',[THUDButton]);
end;

{ THUDButton }

procedure THUDButton.ClickHandler(Sender: TObject);
begin
  if Assigned(FOnClick) then
    FOnClick(Sender);
end;

procedure THUDButton.RepaintImage;
begin
  FImage.Width:=Width;
  FImage.Height:=Height;
  with FImage.Canvas do
  begin
    Brush.Color:=clBlack;
    Clear;
    Pen.Color:=clRed;
    Pen.Width:=4;
    Line(0,0, 0, 10);
    Line(0,0, 20, 0);
    Line(Width,0,Width-20,0);
    Line(Width,0,Width,10);
    Line(0,Height,20,Height);
    Line(0,Height,0,Height-10);
    Line(Width, Height, Width-20, Height);
    Line(Width, Height, Width-20, Height);
    Line(Width, Height, Width, Height-10);
    TextOut(20, Height div 2, FCaption);
  end;
end;

procedure THUDButton.SetCaption(value: string);
begin
  FCaption:=value;
  RepaintImage;
end;

constructor THUDButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImage:=TImage.Create(Self);
  FImage.Parent:=Self;
  FImage.ControlStyle:=FImage.ControlStyle-[csNoDesignSelectable];
  FImage.Height:=40;
  FImage.Width:=150;
  FImage.AutoSize:=True;
  FImage.OnClick:=@ClickHandler;
  Height:=40;
  Width:=150;
  FCaption:='HUDButton';
  FImage.Canvas.Font.Color:=clAqua;
  FImage.Canvas.Font.Style:=[fsBold];
  RepaintImage;
end;

end.
