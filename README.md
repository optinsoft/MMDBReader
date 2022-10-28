# MMDB Reader

## Description

This distribution provides a Delphi reader for the MaxMind DB file format.

## Requirements

Delphi 10.2 Tokyo or newer.

## Usage

```pascal
uses uMMDBReader;

var
  LMMDBReader: TMMDBReader;
  LIPAddress: TMMDBIPAddress;
  LIPInfo: TMMDBIPCountryInfo;
  prefixLength: Integer;
begin
 LIPInfo := TMMDBIPCountryInfo.Create;
 LMMDBReader := TMMDBReader.Create('C:\GeoIP2\GeoLite2-Country.mmdb');
 try
   LIPAddress := TMMDBIPAddress.Parse('8.8.8.8');
   if LMMDBReader.Find<TMMDBIPCountryInfo>(LIPAddress, prefixLength, LIPInfo) then
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

BSD 2-Clause License.

## Dependencies

BigNumbers by Rudy Velthuis (<https://github.com/rvelthuis/DelphiBigNumbers>).

## Contributions

MMDB Reader uses modified IPTypesX module from Albert de Weerd (included in source).

## Supporting the project

* [Sign up for DigitalOcean](https://m.do.co/c/0c76ba2e7552) and get $200 in credit over 60 days (referral link)
