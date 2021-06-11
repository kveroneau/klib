unit UnixSignals;

{$mode objfpc}{$H+}

interface

var
  CBreak, TermSignal, HangUp, User1Sig, User2Sig: Boolean;

implementation

uses BaseUnix;

procedure handleSignal(sig: cint); cdecl;
begin
  case sig of
    SIGINT: CBreak:=True;
    SIGTERM: TermSignal:=True;
    SIGHUP: HangUp:=True;
    SIGUSR1: User1Sig:=True;
    SIGUSR2: User2Sig:=True;
  end;
end;

procedure configureSignal(sig: cint);
var
  oa,na: psigactionrec;
begin
  New(oa);
  New(na);
  na^.sa_handler:=sigactionhandler(@handleSignal);
  FillChar(na^.sa_mask, SizeOf(na^.sa_mask), #0);
  na^.sa_flags:=0;
  {$ifdef Linux}
  na^.sa_restorer:=Nil;
  {$endif}
  if FPSigaction(sig, na, oa) <> 0 then
  begin
    WriteLn('Unable to trap signal: ',sig);
    Halt(1);
  end;
end;

initialization
  CBreak:=False;
  TermSignal:=False;
  HangUp:=False;
  User1Sig:=False;
  User2Sig:=False;
  configureSignal(SIGINT);
  configureSignal(SIGTERM);
  configureSignal(SIGHUP);
  configureSignal(SIGUSR1);
  configureSignal(SIGUSR2);
end.

