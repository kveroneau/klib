unit pisound;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, process;

type

  { TPiCommand }

  TPiCommand = class(TObject)
  private
    FFileName, FSpeech: String;
    procedure SetFileName(AValue: String);
    procedure SetSpeech(AValue: String);
  public
    constructor Create;
    property FileName: string read FFileName write SetFileName;
    property Speech: string read FSpeech write SetSpeech;
  end;

  { TPiSound }

  TPiSound = class(TThread)
  private
    FQueue: TObjectQueue;
    FRunning: Boolean;
    FProc: TProcess;
    procedure PerformRequest(cmd: TPiCommand);
    function GetSoundPlaying: Boolean;
  protected
    procedure Execute; override;
  public
    property SoundPlaying: Boolean read GetSoundPlaying;
    constructor Create;
    destructor Destroy; override;
    procedure Say(Speech: String);
    procedure Play(FileName: String);
    procedure Stop;
    procedure StopSound;
  end;

var
  SoundSystem: TPiSound;

implementation

const
  {$IFDEF CPUARM}
  PLAY_CMD = '/usr/bin/aplay'; { Change to 'aplay' with no-desktop }
  {$ELSE}
  PLAY_CMD = '/usr/bin/paplay';
  {$ENDIF}

var
  CriticalSection: TRTLCriticalSection;

{ TPiCommand }

procedure TPiCommand.SetFileName(AValue: String);
begin
  FSpeech:='';
  FFileName:=AValue;
end;

procedure TPiCommand.SetSpeech(AValue: String);
begin
  FFileName:='';
  FSpeech:=AValue;
end;

constructor TPiCommand.Create;
begin
  inherited Create;
  FFileName:='';
  FSpeech:='';
end;

{ TPiSound }

procedure TPiSound.PerformRequest(cmd: TPiCommand);
begin
  if cmd.FileName <> '' then
  begin
    FProc:=TProcess.Create(Nil);
    FProc.Executable:=PLAY_CMD;
    FProc.Parameters.Add(cmd.FileName);
    FProc.Execute;
  end
  else if cmd.Speech <> '' then
  begin
    WriteLn('Going to try saying: ',cmd.Speech);
    FProc:=TProcess.Create(Nil);
    FProc.Executable:='/usr/bin/espeak';
    FProc.Parameters.Add(cmd.Speech);
    FProc.Execute;
    FProc.WaitOnExit;
    FreeAndNil(FProc);
    WriteLn('Execution complete.');
  end;
end;

function TPiSound.GetSoundPlaying: Boolean;
begin
  if FProc <> nil then
    Result:=True
  else
    Result:=False;
end;

procedure TPiSound.Execute;
var
  cmd: TPiCommand;
begin
  FRunning:=True;
  repeat
    EnterCriticalsection(CriticalSection);
    try
      if FProc <> nil then
      begin
        if not FProc.Running then
          FreeAndNil(FProc);
      end;
      if FProc = nil then
      begin
        cmd:=TPiCommand(FQueue.Pop);
        if cmd <> nil then
        begin
          WriteLn('We Got a request!');
          PerformRequest(cmd);
          cmd.Free;
        end;
      end;
    finally
      LeaveCriticalsection(CriticalSection);
    end;
    Sleep(1000);
  until FRunning = False;
  WriteLn('Loop ended.');
end;

constructor TPiSound.Create;
begin
  inherited Create(True);
  FreeOnTerminate:=True;
  FQueue:=TObjectQueue.Create;
  FRunning:=False;
  FProc:=nil;
end;

destructor TPiSound.Destroy;
begin
  FQueue.Free;
  inherited Destroy;
end;

procedure TPiSound.Say(Speech: String);
var
  cmd: TPiCommand;
begin
  cmd:=TPiCommand.Create;
  cmd.Speech:=Speech;
  EnterCriticalsection(CriticalSection);
  try
    FQueue.Push(cmd);
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

procedure TPiSound.Play(FileName: String);
var
  cmd: TPiCommand;
begin
  cmd:=TPiCommand.Create;
  cmd.FileName:=FileName;
  EnterCriticalsection(CriticalSection);
  try
    FQueue.Push(cmd);
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

procedure TPiSound.Stop;
begin
  FRunning:=False;
end;

procedure TPiSound.StopSound;
begin
  if FProc <> nil then
  begin
    EnterCriticalsection(CriticalSection);
    try
      FProc.Terminate(0);
    finally
      LeaveCriticalsection(CriticalSection);
    end;
  end;
end;

initialization
  InitCriticalSection(CriticalSection);
  SoundSystem:=TPiSound.Create;

finalization
  DoneCriticalsection(CriticalSection);
end.

