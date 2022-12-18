unit phue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser;

type

  { THueBridge }

  THueBridge = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FConfig: TJSONObject;
    FLights: TJSONObject;
    FLightCount: Integer;
    function GetRequest(uri: string): TJSONData;
    function PutRequest(uri: string; data: string): TJSONData;
    function GetName: String;
    function GetWhitelist: TJSONObject;
    function GetUser: TJSONObject;
    function GetLight(Index: Integer): TJSONObject;
  public
    procedure Refresh;
    procedure SetState(light: Integer; lit: Boolean; bri, hue, sat: Integer);
    property Config: TJSONObject read FConfig;
    property Lights[Index: Integer]: TJSONObject read GetLight;
    property LightCount: Integer read FLightCount;
    property Name: String read GetName;
    property Whitelist: TJSONObject read GetWhitelist;
    property UserData: TJSONObject read GetUser;
  end;

implementation

const
  HUE_IP = 'hue';
  HUE_USER = '***Place Philips Hue Token here***';

{ THueBridge }

constructor THueBridge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfig:=TJSONObject(GetRequest('config'));
  FLights:=TJSONObject(GetRequest('lights'));
  FLightCount:=FLights.Count;
end;

destructor THueBridge.Destroy;
begin
  FLights.Free;
  FConfig.Free;
  inherited Destroy;
end;

function THueBridge.GetRequest(uri: string): TJSONData;
begin
  with TFPHTTPClient.Create(Nil) do
    try
      KeepConnection:=False;
      Result:=GetJSON(Get('http://'+HUE_IP+'/api/'+HUE_USER+'/'+uri));
    finally
      Free;
    end;
end;

function THueBridge.PutRequest(uri: string; data: string): TJSONData;
begin
  with TFPHTTPClient.Create(Nil) do
    try
      KeepConnection:=False;
      RequestBody:=TStringStream.Create(data);
      Result:=GetJSON(Put('http://'+HUE_IP+'/api/'+HUE_USER+'/'+uri));
    finally
      RequestBody.Free;
      Free;
    end;
end;

function THueBridge.GetName: String;
begin
  Result:=FConfig.Strings['name'];
end;

function THueBridge.GetWhitelist: TJSONObject;
begin
  Result:=FConfig.Objects['whitelist'];
end;

function THueBridge.GetUser: TJSONObject;
begin
  Result:=Whitelist.Objects[HUE_USER];
end;

function THueBridge.GetLight(Index: Integer): TJSONObject;
begin
  Result:=Nil;
  if (Index > 0) and (Index < FLights.Count+1) then
    Result:=TJSONObject(FLights.Objects[IntToStr(Index)]);
end;

procedure THueBridge.Refresh;
begin
  FLights.Free;
  FLights:=TJSONObject(GetRequest('lights'));
  FLightCount:=FLights.Count;
end;

procedure THueBridge.SetState(light: Integer; lit: Boolean; bri, hue,
  sat: Integer);
var
  state: TJSONObject;
  r: TJSONData;
  s: TStringStream;
begin
  state:=TJSONObject.Create;
  state.Add('on', lit);
  state.Add('bri', bri);
  state.Add('hue', hue);
  state.Add('sat', sat);
  s:=TStringStream.Create;
  state.DumpJSON(s);
  state.Free;
  r:=PutRequest('lights/'+IntToStr(light)+'/state', s.DataString);
  {r.DumpJSON(s);}
  {WriteLn(s.DataString);}
  s.Free;
  r.Free;
end;

end.

