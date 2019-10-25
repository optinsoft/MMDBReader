{ **************************************************************************** }
{                                                                              }
{ uMMDBInfo - This module is a part of the MMDB Reader project                 }
{                                                                              }
{ Created by Vitaly Yakovlev                                                   }
{ Copyright: (c) 2019 Vitaly Yakovlev                                          }
{ Website: http://optinsoft.net/                                               }
{                                                                              }
{ License: BSD 2-Clause License.                                               }
{                                                                              }
{ Redistribution and use in source and binary forms, with or without           }
{ modification, are permitted provided that the following conditions are met:  }
{                                                                              }
{ 1. Redistributions of source code must retain the above copyright notice,    }
{    this list of conditions and the following disclaimer.                     }
{                                                                              }
{ 2. Redistributions in binary form must reproduce the above copyright notice, }
{    this list of conditions and the following disclaimer in the documentation }
{    and/or other materials provided with the distribution.                    }
{                                                                              }
{ THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  }
{ AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,        }
{ THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR       }
{ PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR            }
{ CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,        }
{ EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,          }
{ PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;  }
{ OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,     }
{ WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR      }
{ OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF       }
{ ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                                   }
{                                                                              }
{ Last edit by: Vitaly Yakovlev                                                }
{ Date: October 22, 2019                                                       }
{ Version: 1.0                                                                 }
{                                                                              }
{ **************************************************************************** }

unit uMMDBInfo;

interface

uses
  System.Generics.Collections, uMMDBReader;

type
  TMMDBContinentInfo = class
  private
    _code: String;
    _geoname_id: Int64;
  public
    [TMMDBAttribute('code')]
    property Code: String read _code write _code;
    [TMMDBAttribute('geoname_id')]
    property GeonameId: Int64 read _geoname_id write _geoname_id;
  end;

  TMMDBCountryInfo = class
  private
    _geoname_id: Int64;
    _iso_code: String;
  public
    [TMMDBAttribute('geoname_id')]
    property GeonameId: Int64 read _geoname_id write _geoname_id;
    [TMMDBAttribute('iso_code')]
    property ISOCode: String read _iso_code write _iso_code;
  end;

  TMMDBIPInfo = class
  private
    _continent: TMMDBContinentInfo;
    _country: TMMDBCountryInfo;
    _registered_country: TMMDBCountryInfo;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('continent')]
    property Continent: TMMDBContinentInfo read _continent;
    [TMMDBAttribute('country')]
    property Country: TMMDBCountryInfo read _country;
    [TMMDBAttribute('registered_country')]
    property RegisteredCountry: TMMDBCountryInfo read _registered_country;
  end;

  TMMDBContinentInfoEx = class
  private
    _code: String;
    _geoname_id: Int64;
    _names: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('code')]
    property Code: String read _code write _code;
    [TMMDBAttribute('geoname_id')]
    property GeonameId: Int64 read _geoname_id write _geoname_id;
    [TMMDBAttribute('names')]
    property Names: TDictionary<string, string> read _names;
  end;

  TMMDBCountryInfoEx = class
  private
    _geoname_id: Int64;
    _iso_code: String;
    _names: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('geoname_id')]
    property GeonameId: Int64 read _geoname_id write _geoname_id;
    [TMMDBAttribute('iso_code')]
    property ISOCode: String read _iso_code write _iso_code;
    [TMMDBAttribute('names')]
    property Names: TDictionary<string, string> read _names;
  end;

  TMMDBIPInfoEx = class
  private
    _continent: TMMDBContinentInfoEx;
    _country: TMMDBCountryInfoEx;
    _registered_country: TMMDBCountryInfoEx;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('continent')]
    property Continent: TMMDBContinentInfoEx read _continent;
    [TMMDBAttribute('country')]
    property Country: TMMDBCountryInfoEx read _country;
    [TMMDBAttribute('registered_country')]
    property RegisteredCountry: TMMDBCountryInfoEx read _registered_country;
  end;

implementation

{ TMMDBIPInfo }

constructor TMMDBIPInfo.Create;
begin
  _continent := TMMDBContinentInfo.Create;
  _country := TMMDBCountryInfo.Create;
  _registered_country := TMMDBCountryInfo.Create;
end;

destructor TMMDBIPInfo.Destroy;
begin
  _registered_country.Free;
  _country.Free;
  _continent.Free;
  inherited;
end;

{ TMMDBContinentInfoEx }

constructor TMMDBContinentInfoEx.Create;
begin
  _names := TDictionary<string, string>.Create;
end;

destructor TMMDBContinentInfoEx.Destroy;
begin
  _names.Free;
  inherited;
end;

{ TMMDBCountryInfoEx }

constructor TMMDBCountryInfoEx.Create;
begin
  _names := TDictionary<string, string>.Create;
end;

destructor TMMDBCountryInfoEx.Destroy;
begin
  _names.Free;
  inherited;
end;

{ TMMDBIPInfoEx }

constructor TMMDBIPInfoEx.Create;
begin
  _continent := TMMDBContinentInfoEx.Create;
  _country := TMMDBCountryInfoEx.Create;
  _registered_country := TMMDBCountryInfoEx.Create;
end;

destructor TMMDBIPInfoEx.Destroy;
begin
  _registered_country.Free;
  _country.Free;
  _continent.Free;
  inherited;
end;

end.
