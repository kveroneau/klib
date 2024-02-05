unit klogger;

{$mode objfpc}{$H+}

interface

uses
  eventlog;

procedure SetupLog(const logFile: string);
procedure LogInfo(const msg: string);
procedure LogWarning(const msg: string);
procedure LogError(const msg: string);

implementation

var
  log: TEventLog;

procedure SetupLog(const logFile: string);
begin
  if Assigned(log) then
    Exit;
  log:=TEventLog.Create(Nil);
  log.LogType:=ltFile;
  log.FileName:=logFile;
  log.AppendContent:=True;
  log.Active:=True;
end;

procedure LogInfo(const msg: string);
begin
  if Assigned(log) then
    log.Info(msg);
end;

procedure LogWarning(const msg: string);
begin
  if Assigned(log) then
    log.Warning(msg);
end;

procedure LogError(const msg: string);
begin
  if Assigned(log) then
    log.Error(msg);
end;

initialization
  log:=Nil;

finalization
  if Assigned(log) then
    log.Free;

end.

