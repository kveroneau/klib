unit LazKClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, kclient, kapi;

type

  { TLazKClient }

  TLazKClient = class(TComponent)
  private
    FActive: Boolean;
    FClient: TKClient;
    FClientMode: TKClientMode;
    FHost: string;
    FOnConnect: TKNotifyEvent;
    FOnData: TKApiEvent;
    FOnDisconnect: TKNotifyEvent;
    FOnLine: TKStrEvent;
    FOnString: TKStrEvent;
    FPort: word;
    procedure SetActive(AValue: Boolean);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Procedure SendData(s: string);
    Procedure SendApi(api: PKApi);
    Procedure SendLine(line: string);
    Procedure SendString(s: string);
  published
    property Host: string read FHost write FHost;
    property Port: word read FPort write FPort;
    property Active: Boolean read FActive write SetActive;
    Property OnConnect: TKNotifyEvent read FOnConnect write FOnConnect;
    Property OnDisconnect: TKNotifyEvent read FOnDisconnect write FOnDisconnect;
    Property OnData: TKApiEvent read FOnData write FOnData;
    Property OnLine: TKStrEvent read FOnLine write FOnLine;
    Property OnString: TKStrEvent read FOnString write FOnString;
    Property ClientMode: TKClientMode read FClientMode write FClientMode;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('klib',[TLazKClient]);
end;

{ TLazKClient }

procedure TLazKClient.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  if AValue then
  begin
    if Assigned(FClient) then
      FClient.Free;
    FClient:=TKClient.Create(FHost, FPort);
    FClient.OnConnect:=FOnConnect;
    FClient.OnDisconnect:=FOnDisconnect;
    FClient.OnApi:=FOnData;
    FClient.OnString:=FOnString;
    FClient.OnLine:=FOnLine;
    FClient.ClientMode:=FClientMode;
    FClient.Start;
  end
  else
  begin
    if Assigned(FClient) then
    begin
      FClient.Stop;
      FClient.WaitFor;
      FreeAndNil(FClient);
    end;
  end;
  FActive:=AValue;
end;

constructor TLazKClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClient:=Nil;
end;

destructor TLazKClient.Destroy;
begin
  if Assigned(FClient) then
  begin
    FClient.Stop;
    FClient.WaitFor;
  end;
  inherited Destroy;
end;

procedure TLazKClient.SendData(s: string);
begin
  FClient.SendData(s);
end;

procedure TLazKClient.SendApi(api: PKApi);
begin
  FClient.SendApi(api);
end;

procedure TLazKClient.SendLine(line: string);
begin
  FClient.SendLine(line);
end;

procedure TLazKClient.SendString(s: string);
begin
  FClient.SendString(s);
end;

end.
