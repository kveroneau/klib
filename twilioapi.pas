unit twilioapi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser;

type

  { TTwilio }

  TTwilio = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;
    function SendSMS(body, recipient: string; var status: string): Boolean;
  private
    FSID, FTOKEN, FTWILIO_NUMBER: string;
    function SanitizeNumber(ph: string): string;
    function SendRequest(body, recipient: string): TJSONData;
  end;

implementation

const
  SID = '*** Place Twilio SID here before compile ***';
  TOKEN = '*** Place Twilio TOKEN here before compile ***';
  TWILIO_NUMBER = '***NUMBER***';
  TWILIO_ENDPOINT = 'https://api.twilio.com/2010-04-01/Accounts/'+SID+'/Messages.json';
  PH_NUMBERS = ['0'..'9'];

{ TTwilio }

constructor TTwilio.Create;
begin
end;

destructor TTwilio.Destroy;
begin
  inherited Destroy;
end;

function TTwilio.SendSMS(body, recipient: string; var status: string): Boolean;
var
  json: TJSONObject;
  ph: string;
begin
  ph:=SanitizeNumber(recipient);
  if ph = '' then
  begin
    status:='Invalid number';
    Result:=False;
    Exit;
  end;
  json:=TJSONObject(SendRequest(body, ph));
  status:=json.Strings['status'];
  if status = 'queued' then
    Result:=True
  else
    Result:=False;
  json.Free;
end;

function TTwilio.SanitizeNumber(ph: string): string;
var
  buf: string;
  i: Integer;
begin
  buf:='';
  for i:=1 to Length(ph) do
    if ph[i] in PH_NUMBERS then
      buf:=buf+ph[i];
  if buf = '' then
    Result:=''
  else if buf[1] = '1' then
    Result:='+'+buf
  else
    Result:='+1'+buf;
end;

function TTwilio.SendRequest(body, recipient: string): TJSONData;
var
  form: TStringList;
begin
  with TFPHTTPClient.Create(Nil) do
    Try
      UserName:=SID;
      Password:=TOKEN;
      form:=TStringList.Create;
      form.Add('Body='+body);
      form.Add('From='+TWILIO_NUMBER);
      form.Add('To='+recipient);
      Result:=GetJSON(FormPost(TWILIO_ENDPOINT, form));
    finally
      form.Free;
      Free;
    end;
end;

end.

