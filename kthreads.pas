unit kthreads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TThreadMsgEvent = procedure(const msg: string) of object;

  { TKThread }

  TKThread = class(TThread)
  private
    FMyMessage: string;
    procedure PostMessage;
  protected
    procedure WriteLn(const msg: string);
  end;

  { TKCmdThread }

  TKOnCommand = procedure(const cmd: string) of object;

  TKCmdThread = class(TKThread)
  private
    FCritical: TRTLCriticalSection;
    FEvent: PRTLEvent;
    FCommand: string;
    FRunning: Boolean;
    FOnCommand: TKOnCommand;
    procedure RunCommand;
  protected
    procedure Execute; override;
    procedure DoCommand(const cmd: string); virtual;
  public
    procedure GiveCommand(const cmd: string);
    property OnCommand: TKOnCommand read FOnCommand write FOnCommand;
    property Running: Boolean read FRunning write FRunning;
  end;

var
  OnThreadMsg: TThreadMsgEvent;

implementation

{ TKCmdThread }

procedure TKCmdThread.DoCommand(const cmd: string);
begin
  WriteLn('Running TKCmdThread.DoCommand');
end;

procedure TKCmdThread.RunCommand;
begin
  if Assigned(FOnCommand) then
    FOnCommand(FCommand);
end;

procedure TKCmdThread.Execute;
var
  cmd: string;
begin
  InitCriticalSection(FCritical);
  try
    try
      FEvent:=RTLEventCreate;
      FRunning:=True;
      WriteLn('TKCmdThread Ready.');
      repeat
        RTLEventWaitFor(FEvent);
        EnterCriticalSection(FCritical);
        try
          if Assigned(FOnCommand) then
            Synchronize(@RunCommand)
          else
          begin
            cmd:=FCommand;
            DoCommand(cmd);
          end;
        finally
          LeaveCriticalSection(FCritical);
        end;
      until not FRunning;
    finally
      RTLEventDestroy(FEvent);
    end;
  finally
    DoneCriticalSection(FCritical);
  end;
end;

procedure TKCmdThread.GiveCommand(const cmd: string);
begin
  EnterCriticalSection(FCritical);
  try
    FCommand:=cmd;
  finally
    LeaveCriticalSection(FCritical);
  end;
  RTLEventSetEvent(FEvent);
end;

{ TKThread }

procedure TKThread.PostMessage;
begin
  if Assigned(OnThreadMsg) then
    OnThreadMsg(FMyMessage);
end;

procedure TKThread.WriteLn(const msg: string);
begin
  FMyMessage:=msg;
  Synchronize(@PostMessage);
end;

end.

