unit kevtel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ssockets, sslsockets, opensslsockets, fphttpclient, fpjson, jsonparser;

type

  PCallData = ^TCallData;
  TCallData = record
    to_: string[20];
    from: string[20];
    direction: string[20];
    status: string[20];
    stime: string[60];
  end;

  { TIVR }

  TIVR = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;
  private
    IVRResult: String;
    FConfig: TJSONObject;
    function GetConfig(calls: boolean): TJSONData;
    procedure UpdateConfig;
    procedure IVRGet(uri: string);
    function GetBusy: string;
    procedure SetBusy(AValue: string);
    function GetSleeping: boolean;
    procedure SetSleeping(value: boolean);
    function GetHome: boolean;
    procedure SetHome(value: boolean);
    function GetFrontDoor: string;
    procedure SetFrontDoor(value: string);
    procedure OpenFrontDoor(who: string);
    function GetCallID: string;
    function GetCallDataI(cid: string): TJSONData;
  public
    property Result: string read IVRResult;
    property Busy: string read GetBusy write SetBusy;
    property Sleeping: boolean read GetSleeping write SetSleeping;
    property Home: boolean read GetHome write SetHome;
    property FrontDoor: string read GetFrontDoor write SetFrontDoor;
    property CallID: string read GetCallID;
    procedure GetCallData(cid: string; cdata: PCallData);
    procedure HangUp(cid: string);
    procedure Hold(cid: string);
    procedure Conference(cid: string);
    procedure Route(cid, target: string);
    procedure Transfer(cid, sendto, from: string);
    procedure Take(cid: string);
  end;

implementation

const
  URL = '***URL_MASKED***';

{ TIVR }

constructor TIVR.Create;
begin
  FConfig:=Nil;
  UpdateConfig;
end;

destructor TIVR.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
end;

function TIVR.GetConfig(calls: boolean): TJSONData;
var
  param: string;
begin
  if calls then
    param := 'yes'
  else
    param := 'no';
  with TFPHTTPClient.Create(Nil) do
    try
      KeepConnection:=False;
      Result:=GetJSON(Get(URL+'config?calls='+param));
    finally
      Free;
    end;
end;

procedure TIVR.UpdateConfig;
begin
  if Assigned(FConfig) then
    FConfig.Free;
  try
    FConfig:=TJSONObject(GetConfig(False));
  except
    On ESocketError do FConfig:=Nil;
    On EHTTPClient do FConfig:=Nil;
    On EJSONParser do FConfig:=Nil;
  end;
  if Assigned(FConfig) then
    IVRResult:=''
  else
    IVRResult:='ESocketError';
end;

procedure TIVR.IVRGet(uri: string);
begin
  WriteLn('IVR GET: ',uri);
  with TFPHTTPClient.Create(Nil) do
    try
      KeepConnection:=False;
      IVRResult:=Get(URL+uri);
    finally
      Free;
    end;
end;

function TIVR.GetBusy: string;
begin
  Result:='';
  if FConfig = Nil then
    Exit;
  if FConfig.Types['BUSY'] = jtString then
    Result:=FConfig.Strings['BUSY']
end;

procedure TIVR.SetBusy(AValue: string);
var
  data: TStringList;
begin
  with TFPHTTPClient.Create(Nil) do
    try
      KeepConnection:=False;
      data:=TStringList.Create;
      if AValue = '' then
        data.Add('enable=no')
      else
      begin
        data.Add('enable=yes');
        data.Add('message='+AValue);
      end;
      WriteLn(data.Text);
      IVRResult:=FormPost(URL+'busy_mode', data);
    finally
      data.Free;
      Free;
    end;
  UpdateConfig;
end;

function TIVR.GetSleeping: boolean;
begin
  Result:=False;
  if FConfig = Nil then
    Exit;
  Result:=FConfig.Booleans['SLEEPING'];
end;

procedure TIVR.SetSleeping(value: boolean);
begin
  UpdateConfig;
  if value and Sleeping then
    Exit;
  if value then
    IVRGet('sleeping_mode')
  else
    IVRGet('awake_mode');
  UpdateConfig;
end;

function TIVR.GetHome: boolean;
begin
  Result:=False;
  if FConfig = Nil then
    Exit;
  Result:=FConfig.Booleans['HOME'];
end;

procedure TIVR.SetHome(value: boolean);
begin
  UpdateConfig;
  if value and Home then
    Exit;
  if value then
    IVRGet('home_mode')
  else
    IVRGet('away_mode');
  UpdateConfig;
end;

function TIVR.GetFrontDoor: string;
begin
  UpdateConfig;
  Result:='';
  if FConfig = Nil then
    Exit;
  if FConfig.Objects['FRONT_DOOR'].Booleans['open'] then
    Result:=FConfig.Objects['FRONT_DOOR'].Strings['who']
end;

procedure TIVR.SetFrontDoor(value: string);
begin
  if value = '' then
    IVRGet('close_front_door')
  else
    OpenFrontDoor(value);
end;

procedure TIVR.OpenFrontDoor(who: string);
var
  data: TStringList;
begin
  with TFPHTTPClient.Create(Nil) do
    try
      KeepConnection:=False;
      data:=TStringList.Create;
      data.Add('who='+who);
      IVRResult:=FormPost(URL+'open_front_door', data);
    finally
      data.Free;
      Free;
    end;
  UpdateConfig;
end;

function TIVR.GetCallID: string;
var
  cfg: TJSONObject;
begin
  cfg:=TJSONObject(GetConfig(True));
  GetCallID:=cfg.Strings['CALL'];
end;

function TIVR.GetCallDataI(cid: string): TJSONData;
begin
  IVRGet('/mgr/call_status/'+cid);
  Result:=GetJSON(IVRResult);
end;

procedure TIVR.GetCallData(cid: string; cdata: PCallData);
var
  d: TJSONObject;
begin
  d:=TJSONObject(GetCallDataI(cid));
  cdata^.to_:=d.Strings['to'];
  cdata^.from:=d.Strings['from'];
  cdata^.direction:=d.Strings['direction'];
  cdata^.status:=d.Strings['status'];
  cdata^.stime:=d.Strings['start_time'];
end;

procedure TIVR.HangUp(cid: string);
begin
  IVRGet('/mgr/hangup/'+cid);
end;

procedure TIVR.Hold(cid: string);
begin
  IVRGet('/mgr/hold/'+cid);
end;

procedure TIVR.Conference(cid: string);
begin
  IVRGet('/mgr/conference/'+cid);
end;

procedure TIVR.Route(cid, target: string);
begin
  IVRGet('/mgr/route/'+cid+'?action='+target);
end;

procedure TIVR.Transfer(cid, sendto, from: string);
begin
  IVRGet('/mgr/transfer/'+cid+'?cid='+from+'&number='+sendto);
end;

procedure TIVR.Take(cid: string);
begin
  IVRGet('/mgr/take/'+cid);
end;

end.

