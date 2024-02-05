unit NetMemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LazNetcard;

type

  { TNetMemo }

  TNetMemo = class(TMemo)
  private
    FCard: TLazNetcard;
    FSubscribe: Boolean;
    function GetTitle: string;
    procedure SetTitle(AValue: string);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    procedure Pull;
    procedure Push;
  published
    property NetCard: TLazNetcard read FCard write FCard;
    property Subscribe: Boolean read FSubscribe write FSubscribe;
    property MemoTitle: string read GetTitle write SetTitle;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('klib',[TNetMemo]);
end;

{ TNetMemo }

function TNetMemo.GetTitle: string;
begin
  if Assigned(FCard) then
    Result:=FCard.BlockInfo^.title
  else
    Result:='';
end;

procedure TNetMemo.SetTitle(AValue: string);
begin
  if Assigned(FCard) then
    FCard.BlockInfo^.title:=AValue;
end;

constructor TNetMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCard:=Nil;
end;

procedure TNetMemo.Pull;
begin
  if not Assigned(FCard) then
    Exit;
  Text:=FCard.Block.ReadAnsiString;
end;

procedure TNetMemo.Push;
begin
  if not Assigned(FCard) then
    Exit;
  FCard.Block.Position:=0;
  FCard.Block.WriteAnsiString(Text);
  FCard.Write;
end;

end.
