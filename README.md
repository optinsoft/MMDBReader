# MMDB Reader

## Description

This distribution provides a Delphi reader for the MaxMind DB file format.

## Requirements

Delphi 10.2 Tokyo or newer.

## Dependencies

BigNumbers by Rudy Velthuis (https://github.com/rvelthuis/DelphiBigNumbers).

## Usage
```pascal
uses uMMDBReader;

var
  LMMDBReader: TMMDBReader;
  LIPAddress: TMMDBIPAddress;
  LIPInfo: TMMDBIPInfo
  prefixLength: Integer;
begin
 LIPInfo := TMMDBIPInfo.Create;
 LMMDBReader := TMMDBReader.Create('C:\GeoIP2\GeoLite2-Country.mmdb'); 
 try
   LIPAddress := TMMDBIPAddress.Parse('8.8.8.8');
   if LMMDBReader.Find<TMMDBIPInfo>(LIPAddress, prefixLength, LIPInfo) then
     ShowMessage(Format('country_iso_code: "%s", country_geoname_id: %s', [LIPInfo.Country.ISOCode, IntToStr(LIPInfo.country.GeonameId)]))
   else
     ShowMessage('Not found');
 finally
   LMMDBReader.Free;
   LIPInfo.Free;
 end;
end;
```
## License

This work is licensed under the Creative Commons Attribution-ShareAlike 3.0
Unported License. To view a copy of this license, visit
http://creativecommons.org/licenses/by-sa/3.0/.

## Contributions

MMDB Reader uses modified IPTypesX module from Albert de Weerd (included in source).
