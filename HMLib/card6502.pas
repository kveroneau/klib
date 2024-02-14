unit Card6502;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  PCardMemory = ^TCardMemory;
  TCardMemory = Array[0..255] of Byte;

  { T6502Card }

  T6502Card = class(TComponent)
  private
    FMemory: PCardMemory;
    FOnInitCard: TNotifyEvent;
    FOnRunCard, FOnInit: TNotifyEvent;
    procedure SetMemory(AValue: PCardMemory);
  protected
    function GetCardType: byte; virtual;
  public
    property Memory: PCardMemory read FMemory write SetMemory;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CardRun; virtual;
  published
    property CardType: byte read GetCardType;
    property OnRunCard: TNotifyEvent read FOnRunCard write FOnRunCard;
    property OnInitCard: TNotifyEvent read FOnInitCard write FOnInitCard;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MOS 6502',[T6502Card]);
end;

{ T6502Card }

procedure T6502Card.SetMemory(AValue: PCardMemory);
begin
  if FMemory=AValue then Exit;
  FMemory:=AValue;
  if AValue = Nil then
    Exit;
  if Assigned(FOnInitCard) then
    FOnInitCard(Self);
end;

function T6502Card.GetCardType: byte;
begin
  Result:=$99;
end;

constructor T6502Card.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor T6502Card.Destroy;
begin
  inherited Destroy;
end;

procedure T6502Card.CardRun;
begin
  if Assigned(FOnRunCard) then
    FOnRunCard(Self);
end;

end.
