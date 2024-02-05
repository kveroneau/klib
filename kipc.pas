unit kipc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets;

type
  TKIPCSig = Array[0..2] of Char;
  PKIPCData = ^TKIPCData;
  TKIPCData = Array[0..64] of Byte;

  PKIPCPacket = ^TKIPCPacket;
  TKIPCPacket = record
    sig: TKIPCSig;
    op: byte;
    data: TKIPCData;
  end;

  EKIPCException = class(Exception);
  EKIPCMissing = class(EKIPCException);
  EKIPCBindFail = class(EKIPCException);

  TKIPCEvent = procedure(pkt: PKIPCPacket) of object;

procedure RunKIPCServer(const fname: string; callback: TKIPCEvent);
procedure SendKIPCEvent(const fname: string; const op: byte; data: PKIPCData);

implementation

const
  KIPC_SIG: TKIPCSig = ('I','P','C');

procedure RunKIPCServer(const fname: string; callback: TKIPCEvent);
var
  srv, addrLen, senderLen, sz: LongInt;
  addr, sender: TUnixSockAddr;
  Running: Boolean;
  buf: Array[0..255] of Byte;
  req: PKIPCPacket;
begin
  if FileExists(fname) then
    raise EKIPCBindFail.Create('Socket file already exists: '+fname);
  Str2UnixSockAddr(fname, addr, addrLen);
  srv:=fpsocket(AF_UNIX, SOCK_DGRAM, 0);
  if srv = -1 then
    raise Exception.Create(' ! Failed to create socket.');
  if fpbind(srv, @addr, addrLen) = -1 then
    raise EKIPCBindFail.Create('Failed to bind socket: '+fname);
  Running:=True;
  repeat
    sz:=fprecvfrom(srv, @buf[0], Length(buf), 0, @sender, @senderLen);
    if sz = SizeOf(TKIPCPacket) then
    begin
      req:=@buf[0];
      if req^.sig = KIPC_SIG then
      begin
        if req^.op = $ff then
          Running:=False
        else
          callback(req);
      end;
    end;
  until not Running;
  CloseSocket(srv);
  DeleteFile(fname);
end;

procedure SendKIPCEvent(const fname: string; const op: byte; data: PKIPCData);
var
  s, addrLen, addrLen2: LongInt;
  addr, addr2: TUnixSockAddr;
  req: TKIPCPacket;
begin
  if not FileExists(fname) then
    raise EKIPCMissing.Create('Unable to bind to socket: '+fname);
  Str2UnixSockAddr('', addr, addrLen);
  Str2UnixSockAddr(fname, addr2, addrLen2);
  s:=fpsocket(AF_UNIX, SOCK_DGRAM, 0);
  if fpbind(s, @addr, addrLen) = -1 then
    raise Exception.Create('Unable to bind socket!');
  req.sig:=KIPC_SIG;
  req.op:=op;
  if Assigned(data) then
    Move(data^[0], req.data[0], SizeOf(data^));
  fpsendto(s, @req, SizeOf(req), 0, @addr2, addrLen2);
  CloseSocket(s);
end;

end.

