unit kserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ssockets, kapi;

type

  TExitEvent = Procedure of Object;
  PKBuffer = ^TKBuffer;
  TKBuffer = Array[0..1023] of Byte;
  TKProtocol = class;
  TKCmdEvent = Procedure(Sender: TKProtocol; api: TKApi) of Object;
  TKLineEvent = Procedure(Sender: TKProtocol; line: string) of Object;
  TKNotifyEvent = Procedure(Sender: TKProtocol) of Object;

  TKServerMode = (ksmApi, ksmString, ksmLine, ksmBlock);

  { TKProtocol }

  TKProtocol = class(TThread)
  Private
    FSocket: TSocketStream;
    FOnExit: TExitEvent;
    FOnCommand: TKCmdEvent;
    FOnLine: TKLineEvent;
    FOnString: TKLineEvent;
    FOnConnect: TKNotifyEvent;
    FRunning: Boolean;
    FServerMode: TKServerMode;
    FBlocksize: integer;
    FString: String;
    FApi: TKApi;
    {Procedure ProcessCommand(buf: PKBuffer; size: Integer);}
    Procedure DoOnApi;
    Procedure DoOnLine;
    Procedure DoOnString;
    Procedure DoApi;
    Procedure DoLine;
    Procedure DoString;
  Protected
    Procedure Execute; override;
  Public
    constructor Create(ASocket: TSocketStream);
    Procedure QuickSend(buf: PKBuffer);
    Procedure SendString(s: string);
    Procedure SendLine(line: string);
    Procedure SendApi(api: PKApi);
    procedure Stop;
    Property OnExit: TExitEvent read FOnExit write FOnExit;
    property OnCommand: TKCmdEvent read FOnCommand write FOnCommand;
    property OnLine: TKLineEvent read FOnLine write FOnLine;
    property OnString: TKLineEvent read FOnString write FOnString;
    property OnConnect: TKNotifyEvent read FOnConnect write FOnConnect;
    property ServerMode: TKServerMode read FServerMode write FServerMode;
    property Blocksize: integer read FBlocksize write FBlocksize;
  end;

  { TKTCPServer }

  TKTCPServer = class(TInetServer)
  Private
    FOnCommand: TKCmdEvent;
    FOnLine: TKLineEvent;
    FOnString: TKLineEvent;
    FOnConnect: TKNotifyEvent;
    FServerMode: TKServerMode;
    FBlocksize: integer;
    Procedure HandleExit;
    property OnCommand: TKCmdEvent read FOnCommand write FOnCommand;
    property OnLine: TKLineEvent read FOnLine write FOnLine;
    property OnString: TKLineEvent read FOnString write FOnString;
    property OnConnect: TKNotifyEvent read FOnConnect write FOnConnect;
    property ServerMode: TKServerMode read FServerMode write FServerMode;
    property Blocksize: integer read FBlocksize write FBlocksize;
  Protected
    Procedure DoConnect(ASocket: TSocketStream); override;
  end;

  { TKServer }

  TKServer = class(TObject)
  Private
    FServer: TKTCPServer;
    Procedure CheckStatus(Sender: TObject);
    Procedure SetOnCommand(Value: TKCmdEvent);
    Procedure SetOnLine(Value: TKLineEvent);
    Procedure SetOnString(Value: TKLineEvent);
    Procedure SetOnConnect(Value: TKNotifyEvent);
    Procedure SetServerMode(Value: TKServerMode);
    Procedure SetBlocksize(Value: integer);
  Public
    constructor Create(APort: integer);
    destructor Destroy; override;
    Procedure Start;
    Procedure Stop;
    property OnCommand: TKCmdEvent write SetOnCommand;
    property OnLine: TKLineEvent write SetOnLine;
    property OnString: TKLineEvent write SetOnString;
    property OnConnect: TKNotifyEvent write SetOnConnect;
    property ServerMode: TKServerMode write SetServerMode;
    property Blocksize: integer write SetBlocksize;
  end;

implementation

{ TKProtocol }

{procedure TKProtocol.ProcessCommand(buf: PKBuffer; size: Integer);
var
  strm: TMemoryStream;
begin
  if Assigned(FOnCommand) then
  begin
    strm:=TMemoryStream.Create;
    try
      strm.Write(buf^, size);
      strm.Position:=0;
      FOnCommand(Self, strm);
    finally
      strm.Free;
    end;
  end;
end;}

procedure TKProtocol.DoOnApi;
begin
  FOnCommand(Self, FApi);
end;

procedure TKProtocol.DoOnLine;
begin
  FOnLine(Self, FString);
end;

procedure TKProtocol.DoOnString;
begin
  FOnString(Self, FString);
end;

procedure TKProtocol.DoApi;
var
  size: Integer;
begin
  size:=FSocket.Read(FApi, SizeOf(FApi));
  WriteLn(size);
  if size < 1 then
    FRunning:=False
  else
  begin
    if not Assigned(FOnCommand) then
      Exit;
    Synchronize(@DoOnApi);
  end;
end;

procedure TKProtocol.DoLine;
var
  buf: TKBuffer;
  size: Integer;
  s: string;
begin
  size:=FSocket.Read(buf, SizeOf(buf));
  WriteLn(size);
  if size < 1 then
    FRunning:=False
  else
  begin
    if not Assigned(FOnLine) then
      Exit;
    SetLength(s, size-2);
    Move(buf, s[1], size-2);
    FString:=s;
    Synchronize(@DoOnLine);
  end;
end;

procedure TKProtocol.DoString;
var
  buf: TKBuffer;
  size: Integer;
  sbuf: TMemoryStream;
  s: string;
begin
  size:=FSocket.Read(buf, SizeOf(buf));
  if size < 1 then
    FRunning:=False
  else
  begin
    if not Assigned(FOnString) then
      Exit;
    sbuf:=TMemoryStream.Create;
    try
      sbuf.Write(buf, size);
      sbuf.Position:=0;
      s:=sbuf.ReadAnsiString;
      FString:=s;
      Synchronize(@DoOnString);
    finally
      sbuf.Free;
    end;
  end;
end;

procedure TKProtocol.Execute;
begin
  FRunning:=True;
  try
    if Assigned(FOnConnect) then
      FOnConnect(Self);
    repeat
      case ServerMode of
        ksmApi: DoApi;
        ksmLine: DoLine;
        ksmString: DoString;
      end;
    until not FRunning;
    WriteLn('Disconnected.');
  finally
    FSocket.Free;
  end;
end;

constructor TKProtocol.Create(ASocket: TSocketStream);
begin
  inherited Create(True);
  FreeOnTerminate:=True;
  FSocket:=ASocket;
end;

procedure TKProtocol.Stop;
begin
  FRunning:=False;
end;

procedure TKProtocol.QuickSend(buf: PKBuffer);
begin
  FSocket.Write(buf^, SizeOf(buf^));
end;

procedure TKProtocol.SendString(s: string);
begin
  if FServerMode = ksmString then
    FSocket.WriteAnsiString(s) { Use a ReadAll in kclient or make new method }
  else
    FSocket.Write(s[1], Length(s));
end;

procedure TKProtocol.SendLine(line: string);
begin
  SendString(line+#13#10);
end;

procedure TKProtocol.SendApi(api: PKApi);
begin
  FSocket.Write(api^, SizeOf(api^));
end;

{ TKServer }

procedure TKServer.CheckStatus(Sender: TObject);
begin
  CheckSynchronize();
  Sleep(500);
end;

procedure TKServer.SetOnCommand(Value: TKCmdEvent);
begin
  FServer.OnCommand:=Value;
end;

procedure TKServer.SetOnLine(Value: TKLineEvent);
begin
  FServer.OnLine:=Value;
end;

procedure TKServer.SetOnString(Value: TKLineEvent);
begin
  FServer.OnString:=Value;
end;

procedure TKServer.SetOnConnect(Value: TKNotifyEvent);
begin
  FServer.OnConnect:=Value;
end;

procedure TKServer.SetServerMode(Value: TKServerMode);
begin
  FServer.ServerMode:=Value;
end;

procedure TKServer.SetBlocksize(Value: integer);
begin
  FServer.Blocksize:=Value;
  FServer.ServerMode:=ksmBlock;
end;

constructor TKServer.Create(APort: integer);
begin
  FServer:=TKTCPServer.Create(APort);
  FServer.OnIdle:=@CheckStatus;
  FServer.SetNonBlocking;
  FServer.ServerMode:=ksmApi;
end;

destructor TKServer.Destroy;
begin
  FServer.StopAccepting(True);
  FServer.Free;
  inherited Destroy;
end;

procedure TKServer.Start;
begin
  try
    FServer.StartAccepting;
  except
    On ESocketError do WriteLn('Server Error!');
  end;
  Sleep(1000);
end;

procedure TKServer.Stop;
begin
  FServer.StopAccepting(False);
end;

{ TKTCPServer }

procedure TKTCPServer.HandleExit;
begin
  WriteLn('Server Stop Requested.');
  StopAccepting(False);
end;

procedure TKTCPServer.DoConnect(ASocket: TSocketStream);
var
  channel: TKProtocol;
begin
  WriteLn('Got connection!');
  channel:=TKProtocol.Create(ASocket);
  channel.ServerMode:=FServerMode;
  channel.OnExit:=@HandleExit;
  channel.OnCommand:=FOnCommand;
  channel.OnLine:=FOnLine;
  channel.OnString:=FOnString;
  channel.OnConnect:=FOnConnect;
  channel.Blocksize:=FBlocksize;
  channel.Start;
  WriteLn('Thread Created!');
  inherited DoConnect(ASocket);
end;

end.

