unit forecast;

{$mode objfpc}{$H+}

interface

type
  TForecast = record
    title: string[30];
    temperature: string[5];
    outlook: string[80];
  end;

  PWeatherData = ^TWeatherData;
  TWeatherData = record
    conditions: string[80];
    temperature: string[5];
    forecast: Array[0..5] of TForecast;
  end;

implementation

end.

