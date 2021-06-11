unit homecu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser, ssockets;

type

  { THomeCU }

  THomeCU = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Strips;
    procedure AllOff;
    procedure LivingRoom;
    procedure Hue(hue: word);
    procedure Theme(name: string);
    procedure SaveTheme(name: string);
    procedure AllBright;
    procedure Relax;
    procedure Bathroom;
    procedure Bedroom;
    procedure Blacklight;
    procedure Study;
    procedure Entrance;
    procedure Firewall(ip: string);
    procedure Admin(host: string);
    procedure Lock(host: string);
    procedure Say(msg: string);
    procedure UpdateConfig;
    procedure SetTimer(minutes: integer; msg: string);
  private
    CUResult: string;
    FConfig: TJSONObject;
    procedure HomeCUGet(uri: string);
    function GetConfig: TJSONData;
    function GetRooms: boolean;
    function GetPause: boolean;
    function GetWakeme: boolean;
    function GetWeather: TJSONObject;
    function GetAvailable: TJSONObject;
    procedure SetRooms(value: Boolean);
    procedure SetPause(value: Boolean);
    procedure SetWakeme(value: Boolean);
    function GetSleeping: boolean;
    function GetDarkMode: boolean;
    function GetHomeMode: boolean;
    function GetLights: TJSONArray;
  public
    property Result: string read CUResult;
    property Rooms: boolean read GetRooms write SetRooms;
    property Pause: boolean read GetPause write SetPause;
    property Wakeme: boolean read GetWakeme write SetWakeme;
    property Weather: TJSONObject read GetWeather;
    property Available: TJSONObject read GetAvailable;
    property Sleeping: boolean read GetSleeping;
    property DarkMode: boolean read GetDarkMode;
    property HomeMode: boolean read GetHomeMode;
    property Lights: TJSONArray read GetLights;
  end;

  TSayEvent = procedure(message: String) of Object;
  TLogEvent = procedure(message: String) of Object;
  TCmdEvent = procedure(cmd: String) of Object;
  TCtlEvent = procedure(data: String) of Object;

  PCUProtocol = ^TCUProtocol;
  TCUProtocol = packed record
    id: Integer;
    op: byte;
    data: string[80];
  end;

  { THomeCUTCP }

  THomeCUTCP = class(TThread)
  private
    FOnSay: TSayEvent;
    FOnLog: TLogEvent;
    FOnCmd: TCmdEvent;
    FOnCtl: TCtlEvent;
    FMessage: String;
    FRunning: Boolean;
    FSock: TInetSocket;
    FCurId: Integer;
    procedure ProcessSay;
    procedure ProcessLog;
    procedure ProcessCmd;
    procedure ProcessCtl;
    procedure SendCommand(op: byte; data: string);
    procedure ProcessCommand(cu: PCUProtocol);
  protected
    procedure Execute; override;
  public
    constructor Create;
    procedure Stop;
    property OnSay: TSayEvent read FOnSay write FOnSay;
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property OnCmd: TCmdEvent read FOnCmd write FOnCmd;
    property OnCtl: TCtlEvent read FOnCtl write FOnCtl;
  end;

implementation

const
  URL = 'http://pi.home.lan:8080/';

{ THomeCUTCP }

procedure THomeCUTCP.ProcessSay;
begin
  if Assigned(FOnSay) then
    FOnSay(FMessage);
end;

procedure THomeCUTCP.ProcessLog;
begin
  if Assigned(FOnLog) then
    FOnLog(FMessage);
end;

procedure THomeCUTCP.ProcessCmd;
begin
  if Assigned(FOnCmd) then
    FOnCmd(FMessage);
end;

procedure THomeCUTCP.ProcessCtl;
begin
  if Assigned(FOnCtl) then
    FOnCtl(FMessage);
end;

procedure THomeCUTCP.SendCommand(op: byte; data: string);
var
  buf: TCUProtocol;
begin
  buf.id:=5;
  buf.op:=op;
  buf.data:=data;
  FSock.Write(buf, SizeOf(buf));
end;

procedure THomeCUTCP.ProcessCommand(cu: PCUProtocol);
begin
  FMessage:=cu^.data;
  case cu^.op of
    10: Synchronize(@ProcessSay);
    20: Synchronize(@ProcessLog);
    30: Synchronize(@ProcessCtl);
    50: Synchronize(@ProcessCmd);
  end;
end;

procedure THomeCUTCP.Execute;
var
  buf: TCUProtocol;
  size: Integer;
begin
  try
    FSock:=TInetSocket.Create('pi.home.lan', 5199);
  except
    On ESocketError do FSock:=Nil;
  end;
  if FSock = Nil then
  begin
    FMessage:='Connection to home control unit failed.';
    Synchronize(@ProcessSay);
    Exit;
  end;
  FSock.Read(buf, SizeOf(buf));
  FRunning:=True;
  try
    Repeat
      size:=FSock.Read(buf, SizeOf(buf));
      if size > 0 then
      begin
        if buf.op = 255 then
          FRunning:=False
        else
          ProcessCommand(@buf);
      end
      else
        FRunning:=False;
    until not FRunning;
    FMessage:='Disconnected from home control unit.';
    Synchronize(@ProcessSay);
  finally
    FSock.Free;
  end;
end;

constructor THomeCUTCP.Create;
begin
  inherited Create(True);
  FreeOnTerminate:=True;
end;

procedure THomeCUTCP.Stop;
begin
  SendCommand(255, 'exit');
end;

constructor THomeCU.Create;
begin
  FConfig:=Nil;
  UpdateConfig;
end;

destructor THomeCU.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
end;

procedure THomeCU.Strips;
begin
  HomeCUGet('kitchen');
end;

procedure THomeCU.AllOff;
begin
  HomeCUGet('hoff');
end;

procedure THomeCU.LivingRoom;
begin
  HomeCUGet('living');
end;

procedure THomeCU.Hue(hue: word);
begin
  HomeCUGet('hue?hue='+IntToStr(hue));
end;

procedure THomeCU.Theme(name: string);
begin
  HomeCUGet('theme?name='+name);
end;

procedure THomeCU.SaveTheme(name: string);
begin
  HomeCUGet('save_theme?name='+name);
end;

procedure THomeCU.AllBright;
begin
  HomeCUGet('allbright');
end;

procedure THomeCU.Relax;
begin
  HomeCUGet('relax');
end;

procedure THomeCU.Bathroom;
begin
  HomeCUGet('bathroom');
end;

procedure THomeCU.Bedroom;
begin
  HomeCUGet('bedroom');
end;

procedure THomeCU.Blacklight;
begin
  HomeCUGet('blacklight');
end;

procedure THomeCU.Study;
begin
  HomeCUGet('study');
end;

procedure THomeCU.Entrance;
begin
  HomeCUGet('entrance');
end;

procedure THomeCU.Firewall(ip: string);
begin
  HomeCUGet('fw?ip='+ip);
end;

procedure THomeCU.Admin(host: string);
begin
  HomeCUGet('adm?machine='+host);
end;

procedure THomeCU.Lock(host: string);
begin
  HomeCUGet('lck?machine='+host);
end;

procedure THomeCU.Say(msg: string);
begin
  with TFPHTTPClient.Create(Nil) do
    try
      KeepConnection:=False;
      RequestBody:=TStringStream.Create(msg);
      CUResult:=Post(URL+'say');
    finally
      Free;
    end;
end;

{ THomeCU }

procedure THomeCU.HomeCUGet(uri: string);
begin
  WriteLn('HomeCU GET: ',uri);
  with TFPHTTPClient.Create(Nil) do
    try
      {KeepConnection:=False;}
      CUResult:=Get(URL+uri);
    finally
      Free;
    end;
end;

function THomeCU.GetConfig: TJSONData;
begin
  with TFPHTTPClient.Create(Nil) do
    try
      KeepConnection:=False;
      Result:=GetJSON(Get(URL+'health'));
    finally
      Free;
    end;
end;

procedure THomeCU.UpdateConfig;
begin
  if Assigned(FConfig) then
    FConfig.Free;
  try
    FConfig:=TJSONObject(GetConfig);
  except
    On ESocketError do FConfig:=Nil;
  end;
  if Assigned(FConfig) then
    CUResult:=''
  else
    CUResult:='ESocketError';
end;

procedure THomeCU.SetTimer(minutes: integer; msg: string);
var
  data: TStringList;
begin
  with TFPHTTPClient.Create(Nil) do
    try
      KeepConnection:=False;
      data:=TStringList.Create;
      data.Add('minutes='+IntToStr(minutes));
      data.Add('text='+msg);
      CUResult:=FormPost(URL+'set_timer', data);
    finally
      data.Free;
      Free;
    end;
end;

function THomeCU.GetRooms: boolean;
begin
  Result:=FConfig.Objects['config'].Booleans['rooms'];
end;

function THomeCU.GetPause: boolean;
begin
  Result:=FConfig.Objects['config'].Booleans['paused'];
end;

function THomeCU.GetWakeme: boolean;
begin
  Result:=FConfig.Objects['config'].Booleans['wakeme'];
end;

function THomeCU.GetWeather: TJSONObject;
begin
  Result:=FConfig.Objects['weather'];
end;

function THomeCU.GetAvailable: TJSONObject;
begin
  Result:=FConfig.Objects['available'];
end;

procedure THomeCU.SetRooms(value: Boolean);
begin
  UpdateConfig;
  if value and Rooms then
    Exit;
  HomeCUGet('toggle_rooms');
  UpdateConfig;
end;

procedure THomeCU.SetPause(value: Boolean);
begin
  UpdateConfig;
  if value and Pause then
    Exit;
  HomeCUGet('toggle_pause');
  UpdateConfig;
end;

procedure THomeCU.SetWakeme(value: Boolean);
begin
  UpdateConfig;
  if value and Wakeme then
    Exit;
  HomeCUGet('toggle_wakeme');
  UpdateConfig;
end;

function THomeCU.GetSleeping: boolean;
begin
  Result:=FConfig.Objects['config'].Booleans['sleeping'];
end;

function THomeCU.GetDarkMode: boolean;
begin
  Result:=FConfig.Objects['config'].Booleans['dark'];
end;

function THomeCU.GetHomeMode: boolean;
begin
  Result:=FConfig.Objects['config'].Booleans['home'];
end;

function THomeCU.GetLights: TJSONArray;
begin
  Result:=FConfig.Arrays['lights'];
end;

end.

