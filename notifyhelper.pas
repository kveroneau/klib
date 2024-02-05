unit notifyhelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

procedure SendNotification(title, message: string);

implementation

var
  proc: TProcess;

procedure SendNotification(title, message: string);
begin
  proc.Executable:='notify';
  proc.Parameters.Clear;
  proc.Parameters.Add(title);
  proc.Parameters.Add(message);
  proc.Execute;
  proc.WaitOnExit;
end;

initialization
  proc:=TProcess.Create(Nil);

finalization
  proc.Free;
end.

