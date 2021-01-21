{ **************************************************************************** }
{                                                                              }
{ uMMDBInfo - This module is a part of the MMDB Reader project                 }
{                                                                              }
{ Created by Vitaly Yakovlev                                                   }
{ Date: October 22, 2019                                                       }
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
{ Date: January 21, 2021                                                       }
{ Version: 1.2                                                                 }
{                                                                              }
{ Changelog:                                                                   }
{ v1.2:                                                                        }
{ - added TMMDBLocation                                                        }
{ - added TMMDBIPCountryCityInfoEx.Location                                    }
{                                                                              }
{ v1.1:                                                                        }
{ - TMMDBIPInfo renamed to TMMDBIPCountryInfo                                  }
{ - TMMDBIPInfoEx renamed to TMMDBIPCountryInfoEx                              }
{ - added TMMDBCityInfo                                                        }
{ - added TMMDBCityInfoEx                                                      }
{ - added TMMDBIPCountryCityInfo                                               }
{ - added TMMDBIPCountryCityInfoEx                                             }
{                                                                              }
{ v1.0:                                                                        }
{ - Initial release                                                            }
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

  TMMDBIPCountryInfo = class
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

  TMMDBCityInfo = class
  private
    _geoname_id: Int64;
  public
    constructor Create;
    destructor Destroy; override;
    [TMMDBAttribute('geoname_id')]
    property GeonameId: Int64 read _geoname_id write _geoname_id;
  end;

  TMMDBIPCountryCityInfo = class(TMMDBIPCountryInfo)
  private
    _city: TMMDBCityInfo;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('city')]
    property City: TMMDBCityInfo read _city;
  end;

  TMMDBContinentInfoEx = class(TMMDBContinentInfo)
  private
    _names: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('names')]
    property Names: TDictionary<string, string> read _names;
  end;

  TMMDBCountryInfoEx = class(TMMDBCountryInfo)
  private
    _names: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('names')]
    property Names: TDictionary<string, string> read _names;
  end;

  TMMDBIPCountryInfoEx = class
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

  TMMDBCityInfoEx = class(TMMDBCityInfo)
  private
    _names: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('names')]
    property Names: TDictionary<string, string> read _names;
  end;

  TMMDBLocation = class
  private
    _accuracy_radius: Integer;
    _latitude: Double;
    _longitude: Double;
    _time_zone: String;
  public
    [TMMDBAttribute('accuracy_radius')]
    property Accuracy: Integer read _accuracy_radius write _accuracy_radius;
    [TMMDBAttribute('latitude')]
    property Latitude: Double read _latitude write _latitude;
    [TMMDBAttribute('longitude')]
    property Longitude: Double read _longitude write _longitude;
    [TMMDBAttribute('time_zone')]
    property TimeZone: String read _time_zone write _time_zone;
  end;

  TMMDBIPCountryCityInfoEx = class(TMMDBIPCountryInfoEx)
  private
    _city: TMMDBCityInfoEx;
    _location: TMMDBLocation;
  public
    constructor Create;
    destructor Destroy; override;
    //
    [TMMDBAttribute('city')]
    property City: TMMDBCityInfoEx read _city;
    [TMMDBAttribute('location')]
    property Location: TMMDBLocation read _location;
  end;

implementation

{ TMMDBIPCountryInfo }

constructor TMMDBIPCountryInfo.Create;
begin
  _continent := TMMDBContinentInfo.Create;
  _country := TMMDBCountryInfo.Create;
  _registered_country := TMMDBCountryInfo.Create;
end;

destructor TMMDBIPCountryInfo.Destroy;
begin
  _registered_country.Free;
  _country.Free;
  _continent.Free;
  inherited;
end;

{ TMMDBContinentInfoEx }

constructor TMMDBContinentInfoEx.Create;
begin
  inherited;
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
  inherited;
  _names := TDictionary<string, string>.Create;
end;

destructor TMMDBCountryInfoEx.Destroy;
begin
  _names.Free;
  inherited;
end;

{ TMMDBIPCountryInfoEx }

constructor TMMDBIPCountryInfoEx.Create;
begin
  _continent := TMMDBContinentInfoEx.Create;
  _country := TMMDBCountryInfoEx.Create;
  _registered_country := TMMDBCountryInfoEx.Create;
end;

destructor TMMDBIPCountryInfoEx.Destroy;
begin
  _registered_country.Free;
  _country.Free;
  _continent.Free;
  inherited;
end;

{ TMMDBCityInfo }

constructor TMMDBCityInfo.Create;
begin

end;

destructor TMMDBCityInfo.Destroy;
begin

  inherited;
end;

{ TMMDBIPCountryCityInfo }

constructor TMMDBIPCountryCityInfo.Create;
begin
  inherited;
  _city := TMMDBCityInfo.Create;
end;

destructor TMMDBIPCountryCityInfo.Destroy;
begin
  _city.Free;
  inherited;
end;

{ TMMDBCityInfoEx }

constructor TMMDBCityInfoEx.Create;
begin
  inherited;
  _names := TDictionary<string, string>.Create;
end;

destructor TMMDBCityInfoEx.Destroy;
begin
  _names.Free;
  inherited;
end;

{ TMMDBIPCountryCityInfoEx }

constructor TMMDBIPCountryCityInfoEx.Create;
begin
  inherited;
  _city := TMMDBCityInfoEx.Create;
  _location := TMMDBLocation.Create;
end;

destructor TMMDBIPCountryCityInfoEx.Destroy;
begin
  _location.Free;
  _city.Free;
  inherited;
end;

end.
