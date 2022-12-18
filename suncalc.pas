unit suncalc;

{$mode objfpc}{$H+}

interface

uses
  Math, DateUtils, SysUtils;

procedure CalcSunriseSet(const latitude, longitude: single; const localOffset: integer; out Sunrise, Sunset: TDateTime);

implementation

const
  ZENITH = 90.83333;

function GetSunMean(value: single): single;
begin
  Result:=(0.9856 * value) - 3.289;
end;

function Sin(value: single): single;
var
  x: single;
begin
  SinCos(value, Result, x);
end;

function Cos(value: single): single;
var
  x: single;
begin
  SinCos(value, x, Result);
end;

function GetSunLong(value: single): single;
begin
  Result:=(value + (1.916 * Sin(DegToRad(value))) + (0.020 * Sin(DegToRad(2 * value))) + 282.634) mod 360;
end;

function GetSunAssent(value: single): single;
begin
  Result:=(RadToDeg(ArcTan2(0.91764 * Tan(DegToRad(value)),1))) mod 360;
end;

procedure CalcSunriseSet(const latitude, longitude: single; const localOffset: integer; out Sunrise, Sunset: TDateTime);
var
  N: integer;
  lngHour, t_rise, t_set, M_rise, M_set, L_rise, L_set, RA_rise, RA_set: single;
  Lquadrant_rise, RAqaudrant_rise, Lquadrant_set, RAqaudrant_set: single;
  sinDec_rise, cosDec_rise, sinDec_set, cosDec_set: single;
  cos_zenith, radian_lat, sin_latitude, cos_latitude, cosH_rise, cosH_set: single;
  H_rise, H_set, UT_rise, UT_set, localT_rise, localT_set: single;
  hi_rise, mi_rise, hi_set, mi_set: string;
begin
  N:=DayOfTheYear(Today);
  lngHour:=longitude / 15;
  t_rise:=N + ((6 - lngHour) / 24);
  t_set:=N + ((18 - lngHour) / 24);
  M_rise:=GetSunMean(t_rise);
  M_set:=GetSunMean(t_set);
  L_rise:=GetSunLong(M_rise);
  L_set:=GetSunLong(M_set);
  RA_rise:=GetSunAssent(L_rise);
  RA_set:=GetSunAssent(L_set);
  Lquadrant_rise:=(Floor(L_rise/90)) * 90;
  RAqaudrant_rise:=(Floor(RA_rise/90)) * 90;
  RA_rise:=RA_rise+(Lquadrant_rise - RAqaudrant_rise);
  Lquadrant_set:=(Floor(L_set/90)) * 90;
  RAqaudrant_set:=(Floor(RA_set/90)) * 90;
  RA_set:=RA_set+(Lquadrant_set - RAqaudrant_set);
  RA_rise := RA_rise / 15;
  RA_set := RA_set / 15;
  sinDec_rise:=0.39782 * Sin(DegToRad(L_rise));
  cosDec_rise:=Cos(ArcSin(sinDec_rise));
  sinDec_set:=0.39782 * Sin(DegToRad(L_set));
  cosDec_set:=Cos(ArcSin(sinDec_set));
  cos_zenith:=Cos(DegToRad(ZENITH));
  radian_lat:=DegToRad(latitude);
  SinCos(radian_lat, sin_latitude, cos_latitude);
  cosH_rise:=(cos_zenith - (sinDec_rise * sin_latitude)) / (cosDec_rise * cos_latitude);
  cosH_set:=(cos_zenith - (sinDec_set * sin_latitude)) / (cosDec_set * cos_latitude);
  H_rise:=(360 - RadToDeg(ArcCos(cosH_rise))) / 15;
  H_set:=RadToDeg(ArcCos(cosH_set)) / 15;
  t_rise:=H_rise + RA_rise - (0.06571 * t_rise) - 6.622;
  t_set:=H_set + RA_set - (0.06571 * t_set) - 6.622;
  UT_rise:=(t_rise - lngHour) mod 24;
  UT_set:=(t_set - lngHour) mod 24;
  localT_rise:=(UT_rise + localOffset) mod 24;
  localT_set:=(UT_set + localOffset) mod 24;
  if localT_set < 0 then
      localT_set:=localT_set + 24;
  hi_rise:=FloatToStrF(Floor(localT_rise), ffFixed, 0, 0);
  m_rise:=Floor(localT_rise mod 1 * 60);
  mi_rise:=FloatToStrF(m_rise, ffFixed, 0, 0);
  Sunrise:=EncodeTime(StrToInt(hi_rise), StrToInt(mi_rise), 0, 0);
  hi_set:=FloatToStrF(Floor(localT_set), ffFixed, 0, 0);
  m_set:=Floor(localT_set mod 1 * 60);
  mi_set:=FloatToStrF(m_set, ffFixed, 0, 0);
  Sunset:=EncodeTime(StrToInt(hi_set), StrToInt(mi_set), 0, 0);
end;

end.

