unit kclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ssockets, kapi;

type

  PKBuffer = ^TKBuffer;
  TKBuffer = Array[0..1023] of byte;
  TKClient = class;
  TKNotifyEvent = Procedure(Sender: TKClient) of Object;
  TKApiEvent = Procedure(Sender: TKClient; api: TKApi) of Object;
  TKStrEvent = Procedure(Sender: TKClient; s: string) of Object;

  TKClientMode = (kcmApi, kcmString, kcmLine, kcmBlock);

  { TKClient }

  TKClient = class(TThread)
  Private
    FOnData: TKApiEvent;
    FSocket: TInetSocket;
    FHost: string;
    FPort: integer;
    FRunning: Boolean;
    FOutBuf: TMemoryStream;
    FOnConnect: TKNotifyEvent;
    FOnDisconnect: TKNotifyEvent;
    FOnApi: TKApiEvent;
    FOnLine: TKStrEvent;
    FOnString: TKStrEvent;
    FClientMode: TKClientMode;
    FCritical: TRTLCriticalSection;
    FString: string;
    FApi: TKApi;
    Procedure ProcessData(buf: PKBuffer; size: integer);
    Procedure DoApi(buf: PKBuffer; size: integer);
    Procedure DoLine(buf: PKBuffer; size: integer);
    Procedure DoString(buf: PKBuffer; size: integer);
    Procedure DoOnConnect;
    Procedure DoOnDisconnect;
    Procedure DoOnApi;
    Procedure DoOnLine;
    Procedure DoOnString;
  Protected
    Procedure Execute; override;
  Public
    Constructor Create(AHost: string; APort: integer);
    Procedure Stop;
    Procedure SendData(s: string);
    Procedure SendApi(api: PKApi);
    Procedure SendLine(line: string);
    Procedure SendString(s: string);
    Property OnConnect: TKNotifyEvent read FOnConnect write FOnConnect;
    Property OnDisconnect: TKNotifyEvent read FOnDisconnect write FOnDisconnect;
    Property OnApi: TKApiEvent read FOnApi write FOnApi;
    Property OnLine: TKStrEvent read FOnLine write FOnLine;
    Property OnString: TKStrEvent read FOnString write FOnString;
    Property ClientMode: TKClientMode read FClientMode write FClientMode;
  end;

implementation

{ TKClient }

procedure TKClient.ProcessData(buf: PKBuffer; size: integer);
begin
  case FClientMode of
    kcmApi: DoApi(buf, size);
    kcmLine: DoLine(buf, size);
    kcmString: DoString(buf, size);
  end;
end;

procedure TKClient.DoApi(buf: PKBuffer; size: integer);
begin
  if not Assigned(FOnApi) then
    Exit;
  if SizeOf(FApi) <> size then
    Exit;
  Move(buf^, FApi, size);
  Synchronize(@DoOnApi);
end;

procedure TKClient.DoLine(buf: PKBuffer; size: integer);
var
  s: string;
begin
  if not Assigned(FOnLine) then
    Exit;
  SetLength(s, size-2);
  Move(buf^, s[1], size-2);
  FString:=s;
  Synchronize(@DoOnLine);
end;

procedure TKClient.DoString(buf: PKBuffer; size: integer);
var
  s: string;
  sbuf: TMemoryStream;
begin
  if not Assigned(FOnString) then
    Exit;
  sbuf:=TMemoryStream.Create;
  try
    sbuf.Write(buf^, size);
    sbuf.SaveToFile('sbuf');
    sbuf.Position:=0;
    WriteLn(sbuf.Size);
    try
      s:=sbuf.ReadAnsiString;
    except
      s:='';
    end;
    WriteLn(s);
    FString:=s;
    Synchronize(@DoOnString);
  finally
    sbuf.Free;
  end;
end;

procedure TKClient.DoOnConnect;
begin
  FOnConnect(Self);
end;

procedure TKClient.DoOnDisconnect;
begin
  FOnDisconnect(Self);
end;

procedure TKClient.DoOnApi;
begin
  FOnApi(Self, FApi);
end;

procedure TKClient.DoOnLine;
begin
  FOnLine(Self, FString);
end;

procedure TKClient.DoOnString;
begin
  FOnString(Self, FString);
end;

procedure TKClient.Execute;
var
  buf: TKBuffer;
  size: integer;
begin
  InitCriticalSection(FCritical);
  FSocket:=TInetSocket.Create(FHost, FPort);
  FSocket.IOTimeout:=1;
  FOutBuf:=TMemoryStream.Create;
  FRunning:=True;
  try
    if Assigned(FOnConnect) then
      Synchronize(@DoOnConnect);
    repeat
      size:=FSocket.Read(buf, SizeOf(buf)); { Need a ReadAll }
      {WriteLn(size);}
      if size = 0 then
        FRunning:=False
      else if size > 0 then
        ProcessData(@buf, size);
      EnterCriticalSection(FCritical);
      try
        if FOutBuf.Size > 0 then
        begin
          FOutBuf.Position:=0;
          size:=FOutBuf.Read(buf, SizeOf(buf));
          FSocket.Write(buf, size);
          FOutBuf.Clear;
        end;
      finally
        LeaveCriticalSection(FCritical);
      end;
    until not FRunning;
  finally
    FOutBuf.Free;
    FSocket.Free;
  end;
  if Assigned(FOnDisconnect) then
    Synchronize(@DoOnDisconnect);
  DoneCriticalSection(FCritical);
end;

constructor TKClient.Create(AHost: string; APort: integer);
begin
  inherited Create(True);
  FreeOnTerminate:=False;
  FHost:=AHost;
  FPort:=APort;
end;

procedure TKClient.Stop;
begin
  FRunning:=False;
end;

procedure TKClient.SendData(s: string);
begin
  EnterCriticalSection(FCritical);
  try
    FOutBuf.Write(s[1], Length(s));
  finally
    LeaveCriticalSection(FCritical);
  end;
end;

procedure TKClient.SendApi(api: PKApi);
begin
  EnterCriticalSection(FCritical);
  try
    FOutBuf.Write(api^, SizeOf(api^));
  finally
    LeaveCriticalSection(FCritical);
  end;
end;

procedure TKClient.SendLine(line: string);
begin
  SendData(line+#10#13);
end;

procedure TKClient.SendString(s: string);
begin
  EnterCriticalSection(FCritical);
  try
    FOutBuf.WriteAnsiString(s);
  finally
    LeaveCriticalSection(FCritical);
  end;
end;

end.

