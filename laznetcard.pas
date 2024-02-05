unit LazNetcard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, netcard, memcard;

type

  { TLazNetcard }

  TLazNetcard = class(TComponent)
  private
    FCard: TNetCard;
    FServer: string;
    FPort: word;
    FToken: string;
    FCardID, FBlockID: byte;
    FInfo: TBlockInfo;
    FBlock: TMemoryStream;
    FActive: Boolean;
    FOnSync: TSyncEvent;
    function GetBlockInfo: PBlockInfo;
    function GetBlockList: TStringList;
    function GetInfo(ABlock: byte): PBlockInfo;
    procedure SetActive(AValue: Boolean);
    procedure SetBlockID(AValue: byte);
    procedure SetCardID(AValue: byte);
    procedure SetInfo(ABlock: byte; AValue: PBlockInfo);
  protected

  public
    property BlockInfo: PBlockInfo read GetBlockInfo;
    property Block: TMemoryStream read FBlock write FBlock;
    property BlockList: TStringList read GetBlockList;
    property Info[ABlock: byte]: PBlockInfo read GetInfo write SetInfo;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Subscribe;
    procedure Write;
    procedure Reload;
    function FindType(typno: byte): byte;
    function FindApp(appno: byte): byte;
    function FindFree: byte;
  published
    property ServerName: string read FServer write FServer;
    property Port: word read FPort write FPort;
    property AuthToken: string read FToken write FToken;
    property CardID: byte read FCardID write SetCardID;
    property BlockID: byte read FBlockID write SetBlockID;
    property Active: Boolean read FActive write SetActive;
    property OnSync: TSyncEvent read FOnSync write FOnSync;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('klib',[TLazNetcard]);
end;

{ TLazNetcard }

procedure TLazNetcard.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  if AValue then
  begin
    if Assigned(FCard) then
      FCard.Free;
    FCard:=TNetCard.Create(FServer, FPort);
    FCard.Authenticate(FToken);
    FCard.SelectCard(FCardID);
    FCard.OnSync:=FOnSync;
    if FBlockID > 0 then
    begin
      FCard.GetInfo(FBlockID, @FInfo);
      FBlock:=FCard.ReadBlock(FBlockID);
    end;
  end
  else
  begin
    if Assigned(FCard) then
      FreeAndNil(FCard);
    if Assigned(FBlock) then
      FreeAndNil(FBlock);
  end;
  FActive:=AValue;
end;

function TLazNetcard.GetBlockList: TStringList;
begin
  Result:=FCard.BlockList;
end;

function TLazNetcard.GetInfo(ABlock: byte): PBlockInfo;
begin
  if not FActive then
    Exit;
  New(Result);
  FCard.GetInfo(ABlock, Result);
end;

function TLazNetcard.GetBlockInfo: PBlockInfo;
begin
  Result:=@FInfo;
end;

procedure TLazNetcard.SetBlockID(AValue: byte);
begin
  if FBlockID=AValue then Exit;
  if FActive then
  begin
    if Assigned(FBlock) then
      FBlock.Free;
    FCard.GetInfo(AValue, @FInfo);
    FBlock:=FCard.ReadBlock(AValue);
  end;
  FBlockID:=AValue;
end;

procedure TLazNetcard.SetCardID(AValue: byte);
begin
  if FCardID=AValue then Exit;
  if FActive then
    FCard.SelectCard(AValue);
  FCardID:=AValue;
end;

procedure TLazNetcard.SetInfo(ABlock: byte; AValue: PBlockInfo);
begin
  if not FActive then
    Exit;
  if not Assigned(FBlock) then
    Exit;
  FCard.WriteBlock(ABlock, FBlock, AValue);

end;

constructor TLazNetcard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCard:=Nil;
  FBlock:=Nil;
end;

destructor TLazNetcard.Destroy;
begin
  if Assigned(FCard) then
    FCard.Free;
  if Assigned(FBlock) then
    FBlock.Free;
  inherited Destroy;
end;

procedure TLazNetcard.Subscribe;
begin
  if FActive then
    FCard.Subscribe(FBlockID);
end;

procedure TLazNetcard.Write;
begin
  if not FActive then
    Exit;
  if not Assigned(FBlock) then
    Exit;
  FCard.WriteBlock(FBlockID, FBlock, @FInfo);
end;

procedure TLazNetcard.Reload;
begin
  if not FActive then
    Exit;
  if Assigned(FBlock) then
    FBlock.Free;
  FCard.GetInfo(FBlockID, @FInfo);
  FBlock:=FCard.ReadBlock(FBlockID);
end;

function TLazNetcard.FindType(typno: byte): byte;
var
  infox: TBlockInfo;
begin
  if FActive then
    Result:=FCard.FindType(typno, @infox);
end;

function TLazNetcard.FindApp(appno: byte): byte;
var
  infox: TBlockInfo;
begin
  if FActive then
    Result:=FCard.FindApp(appno, @infox);
end;

function TLazNetcard.FindFree: byte;
begin
  if FActive then
    Result:=FCard.FindFree;
end;

end.
