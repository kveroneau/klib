unit LazMemcard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, memcard;

type

  { TLazMemcard }

  TLazMemcard = class(TComponent)
  private
    FCard: TMemCard;
    FCardFile: string;
    FBlocksize: word;
    FBlock: TMemoryStream;
    FBlockID: byte;
    FInfo: TBlockInfo;
    FBlockList: TStringList;
    FActive: Boolean;
    function GetBlockInfo: PBlockInfo;
    procedure SetActive(value: boolean);
    function GetBlock(ABlock: byte): TMemoryStream;
    procedure SetBlock(ABlock: byte; value: TMemoryStream);
    function GetInfo(ABlock: byte): PBlockInfo;
    procedure SetBlockID(ABlock: byte);
    procedure SetInfo(ABlock: byte; AValue: PBlockInfo);
  protected

  public
    property Block[ABlock: byte]: TMemoryStream read GetBlock write SetBlock;
    property Info[ABlock: byte]: PBlockInfo read GetInfo write SetInfo;
    property Stream: TMemoryStream read FBlock;
    property BlockInfo: PBlockInfo read GetBlockInfo;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveBlock;
    procedure Format;
    function FindFree: Integer;
    function FindType(typno: Integer): Integer;
    procedure DeleteBlock(blkid: byte);
  published
    property Filename: string read FCardFile write FCardFile;
    property Blocksize: word read FBlocksize write FBlocksize;
    property Active: boolean read FActive write SetActive;
    property BlockID: byte read FBlockID write SetBlockID;
    property BlockList: TStringList read FBlockList;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I LazMemcard_icon.lrs }
  RegisterComponents('klib',[TLazMemcard]);
end;

{ TLazMemcard }

procedure TLazMemcard.SetActive(value: boolean);
begin
  if FActive and value then
    Exit;
  if value then
  begin
    FCard:=TMemCard.Create(FCardFile, FBlocksize);
    FBlocksize:=FCard.BlockSize;
    if Assigned(FBlockList) then
      FBlockList.Free;
    FBlockList:=FCard.BlockList;
    FActive:=True;
  end
  else
  begin
    FActive:=False;
    if Assigned(FCard) then
      FreeAndNil(FCard);
    if Assigned(FBlock) then
      FreeAndNil(FBlock);
  end;
end;

function TLazMemcard.GetBlockInfo: PBlockInfo;
begin
  Result:=@FInfo;
end;

function TLazMemcard.GetBlock(ABlock: byte): TMemoryStream;
begin
  if FActive then
    Result:=FCard.ReadBlock(ABlock)
  else
    Result:=Nil;
end;

procedure TLazMemcard.SetBlock(ABlock: byte; value: TMemoryStream);
begin
  if FActive then
    FCard.WriteBlock(ABlock, value);
end;

function TLazMemcard.GetInfo(ABlock: byte): PBlockInfo;
begin
  if not FActive then
    Exit;
  New(Result);
  FCard.GetInfo(ABlock, Result);
end;

procedure TLazMemcard.SetBlockID(ABlock: byte);
begin
  if not FActive then
    Exit;
  if Assigned(FBlock) then
    FBlock.Free;
  FCard.GetInfo(ABlock, @FInfo);
  FBlock:=FCard.ReadBlock(ABlock);
  FBlockID:=ABlock;
end;

procedure TLazMemcard.SetInfo(ABlock: byte; AValue: PBlockInfo);
begin
  if not FActive then
    Exit;
  if not Assigned(FBlock) then
    Exit;
  FCard.WriteBlock(ABlock, FBlock, AValue);
  if Assigned(FBlockList) then
    FBlockList.Free;
  FBlockList:=FCard.BlockList;
end;

constructor TLazMemcard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCard:=Nil;
  FBlock:=Nil;
  FBlockList:=TStringList.Create;
end;

destructor TLazMemcard.Destroy;
begin
  if Assigned(FCard) then
    FCard.Free;
  if Assigned(FBlock) then
    FBlock.Free;
  if Assigned(FBlockList) then
    FBlockList.Free;
  inherited Destroy;
end;

procedure TLazMemcard.SaveBlock;
begin
  FCard.WriteBlock(FBlockID, FBlock, @FInfo);
  if Assigned(FBlockList) then
    FBlockList.Free;
  FBlockList:=FCard.BlockList;
end;

procedure TLazMemcard.Format;
begin
  FCard.Format;
end;

function TLazMemcard.FindFree: Integer;
begin
  Result:=FCard.FindFree;
end;

function TLazMemcard.FindType(typno: Integer): Integer;
begin
  Result:=FCard.FindType(typno, @FInfo);
end;

procedure TLazMemcard.DeleteBlock(blkid: byte);
begin
  FCard.DeleteBlock(blkid);
  FBlockList:=FCard.BlockList;
end;

end.
