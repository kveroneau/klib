unit ecweather;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, DateUtils, urllib, forecast, opensslsockets;

type

  { CalgaryWeather }

  CalgaryWeather = class(TObject)
  private
    weather_data: TWeatherData;
    last_update: TDateTime;
    procedure UpdateWeatherData;
    function GetWeatherData: PWeatherData;
  public
    constructor Create;
    destructor Destroy; override;
    property weather: PWeatherData read GetWeatherData;
  end;

var
  weather_cache: CalgaryWeather;

implementation

{ CalgaryWeather }

procedure CalgaryWeather.UpdateWeatherData;
var
  strm: TStringStream;
  city: TXMLDocument;
  current: TDOMNode;
  forecast: TDOMNodeList;
begin
  city:=Nil;
  strm:=TStringStream.Create(URLGet('https://dd.weather.gc.ca/citypage_weather/xml/AB/s0000047_e.xml'));
  try
    ReadXMLFile(city, strm);
    current:=city.GetElementsByTagName('currentConditions').Item[0];
    weather_data.conditions:=current.FindNode('condition').TextContent;
    weather_data.temperature:=current.FindNode('temperature').TextContent;
    forecast:=city.GetElementsByTagName('forecast');
    weather_data.forecast[0].title:=forecast.Item[0].FindNode('period').Attributes.GetNamedItem('textForecastName').TextContent;
    weather_data.forecast[0].temperature:=forecast.Item[0].FindNode('temperatures').FindNode('temperature').TextContent;
    weather_data.forecast[0].outlook:=forecast.Item[0].FindNode('textSummary').TextContent;
    last_update:=Now;
  finally
    if Assigned(city) then
      city.Free;
    strm.Free;
  end;
end;

function CalgaryWeather.GetWeatherData: PWeatherData;
var
  secs: Integer;
begin
  secs:=SecondsBetween(Now, last_update);
  if secs > 3600 then
    UpdateWeatherData;
  GetWeatherData:=@weather_data;
end;

constructor CalgaryWeather.Create;
begin
  UpdateWeatherData;
end;

destructor CalgaryWeather.Destroy;
begin
  inherited Destroy;
end;

initialization
  weather_cache:=CalgaryWeather.Create;

finalization
  weather_cache.Free;
end.

