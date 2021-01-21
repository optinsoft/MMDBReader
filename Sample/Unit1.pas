{ **************************************************************************** }
{                                                                              }
{ Sample for the MMDB Reader project                                           }
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
{ Version: 1.3                                                                 }
{                                                                              }
{ Changelog:                                                                   }
{ v1.3:                                                                        }
{ - print city location info                                                   }
{                                                                              }
{ v1.2:                                                                        }
{ -  Names['en'] has been replaced by Names.TryGetValue('en', ...) to prevent  }
{ possible 'key not found' exception                                           }
{                                                                              }
{ v1.1:                                                                        }
{ - added reading of City mmdb                                                 }
{                                                                              }
{ v1.0:                                                                        }
{ - Initial release                                                            }
{                                                                              }
{ **************************************************************************** }

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uMMDBReader;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Edit1: TEdit;
    Label1: TLabel;
    Button2: TButton;
    Edit2: TEdit;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    Button3: TButton;
    SaveDialog1: TSaveDialog;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FMMDBReader: TMMDBReader;
    FExportCSVFilename: String;
    procedure ReadMMDB(const Filename: String);
    procedure TestIP(const ipString: String; const outPrefix: String = '');
    procedure ExportToCSV;
    procedure ExportStarted(Sender: TObject);
    procedure ExportTerminated(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  uMMDBInfo, uMMDBIPAddress, System.Generics.Collections,
  System.DateUtils, System.StrUtils;

{$IFDEF DEBUG}
{.$DEFINE DEBUG_IP}
{$ENDIF}

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Caption := 'MMDB Reader Sample: '+OpenDialog1.FileName;
    ReadMMDB(OpenDialog1.FileName);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TestIP(Edit1.Text);
end;

type
  TExportThread = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

constructor TExportThread.Create;
begin
  inherited;
  FreeOnTerminate := True;
  OnTerminate := Form1.ExportTerminated;
  Form1.ExportStarted(Self);
end;

procedure TExportThread.Execute;
begin
  try
    Form1.ExportToCSV;
  except
    ShowException(ExceptObject, ExceptAddr);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    FExportCSVFilename := SaveDialog1.FileName;
    TExportThread.Create;
  end;
end;

procedure TForm1.ExportStarted(Sender: TObject);
begin
  Button1.Enabled := False;
  Button2.Enabled := False;
  Button3.Enabled := False;
  Label3.Caption := 'Exporting...';
  Label3.Visible := True;
end;

procedure TForm1.ExportTerminated(Sender: TObject);
begin
  Button1.Enabled := True;
  Button2.Enabled := True;
  Button3.Enabled := True;
  Label3.Caption := 'Export completed.';
end;

procedure TForm1.ExportToCSV;

  function FormatIPRange(rawAddress: TBytes; Prefix: Integer;
    const ISOCode: String): String;
  var
    netAddress, endNetAddress: TMMDBIPAddress;
    endRawAddress: TBytes;
    bitLength: Integer;
    i, h: Integer;
    w: Word;
  begin
    endRawAddress := Copy(rawAddress);
    bitLength := Length(rawAddress) * 8;
    i := Prefix;
    while i < bitLength do
    begin
      h := 8 - (i mod 8);
      rawAddress[i shr 3] :=
        (rawAddress[i shr 3] shr h) shl h;
      w := rawAddress[i shr 3] shr h;
      w := ((w shl 8) or $ff) shl h;
      endRawAddress[i shr 3] := w shr 8;
      i := ((i shr 3) shl 3) + 8;
    end;
    netAddress := TMMDBIPAddress.Create(rawAddress);
    endNetAddress := TMMDBIPAddress.Create(endRawAddress);
    Result := Format('%s,%s,%s',
      [netAddress.ToString, endNetAddress.ToString, ISOCode]);
  end;

  function FormatIPCountryNode(node: TMMDBIteratorNode<TMMDBIPCountryInfo>): String;
  begin
    Result := FormatIPRange(node.Start.GetAddressBytes, node.Prefix, node.Data.Country.ISOCode);
  end;

  function FormatIPCountryCityNode(node: TMMDBIteratorNode<TMMDBIPCountryCityInfoEx>): String;
  var
    cityNameEN: String;
  begin
    if not node.Data.City.Names.TryGetValue('en', cityNameEN) then cityNameEN := '';
    Result := FormatIPRange(node.Start.GetAddressBytes, node.Prefix, cityNameEN);
  end;

var
  countryIterator: IMMDBIterator<TMMDBIPCountryInfo>;
  cityIterator: IMMDBIterator<TMMDBIPCountryCityInfoEx>;
  IPv4Only: Boolean;
  LOutStream: TStream;
  S: String;
  Buffer: TBytes;
begin
  LOutStream := nil;
  IPv4Only := CheckBox1.Checked;
  try
    LOutStream := TFileStream.Create(FExportCSVFilename, fmCreate or fmShareDenyWrite);
    if EndsText('-city', FMMDBReader.Metadata.DatabaseType) then
    begin
      for cityIterator in FMMDBReader.FindAll<TMMDBIPCountryCityInfoEx>(IPv4Only) do
      begin
        S := FormatIPCountryCityNode(cityIterator.Node) + sLineBreak;
        Buffer := TEncoding.UTF8.GetBytes(S);
        LOutStream.WriteBuffer(Buffer, Length(Buffer));
      end;
    end else
    begin
      for countryIterator in FMMDBReader.FindAll<TMMDBIPCountryInfo>(IPv4Only) do
      begin
        S := FormatIPCountryNode(countryIterator.Node) + sLineBreak;
        Buffer := TEncoding.UTF8.GetBytes(S);
        LOutStream.WriteBuffer(Buffer, Length(Buffer));
      end;
    end;
  finally
    if Assigned(LOutStream) then
      LOutStream.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FMMDBReader) then
    FreeAndNil(FMMDBReader);
end;

procedure TForm1.ReadMMDB(const Filename: String);
var
  LLines: TStrings;
  LDictItem: TPair<string, string>;
  I: Integer;
  fs: TFormatSettings;

{$IFDEF DEBUG_IP}
const
  DEBUG_IPv4 = '8.8.4.4';
  DEBUG_IPv6 = '2001:4860:4860::8844';
{$ENDIF}

  function FormatIPCountryNode(node: TMMDBIteratorNode<TMMDBIPCountryInfo>): String;
  var
    netAddress: TMMDBIPAddress;
    rawAddress: TBytes;
    bitLength: Integer;
    i, h: Integer;
  begin
    rawAddress := node.Start.GetAddressBytes;
    bitLength := Length(rawAddress) * 8;
    i := node.Prefix;
    while i < bitLength do
    begin
      h := 8 - (i mod 8);
      rawAddress[i shr 3] :=
        (rawAddress[i shr 3] shr h) shl h;
      i := ((i shr 3) shl 3) + 8;
    end;
    netAddress := TMMDBIPAddress.Create(rawAddress);
    Result := Format('%s/%d,%s,%s,%s,%s,%s,%s',
      [netAddress.ToString, node.Prefix,
       node.Data.Continent.code, IntToStr(node.Data.Continent.GeonameId),
       node.Data.Country.ISOCode, IntToStr(node.Data.Country.GeonameId),
       node.Data.RegisteredCountry.ISOCode, IntToStr(node.Data.RegisteredCountry.GeonameId)]);
  end;

  function FormatIPCountryCityNode(node: TMMDBIteratorNode<TMMDBIPCountryCityInfoEx>): String;
  var
    netAddress: TMMDBIPAddress;
    rawAddress: TBytes;
    bitLength: Integer;
    i, h: Integer;
    cityNameEN: String;
  begin
    rawAddress := node.Start.GetAddressBytes;
    bitLength := Length(rawAddress) * 8;
    i := node.Prefix;
    while i < bitLength do
    begin
      h := 8 - (i mod 8);
      rawAddress[i shr 3] :=
        (rawAddress[i shr 3] shr h) shl h;
      i := ((i shr 3) shl 3) + 8;
    end;
    netAddress := TMMDBIPAddress.Create(rawAddress);
    if not node.Data.City.Names.TryGetValue('en', cityNameEN) then cityNameEN := '';
    Result := Format('%s/%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s',
      [netAddress.ToString, node.Prefix,
       node.Data.Continent.code, IntToStr(node.Data.Continent.GeonameId),
       node.Data.Country.ISOCode, IntToStr(node.Data.Country.GeonameId),
       node.Data.RegisteredCountry.ISOCode, IntToStr(node.Data.RegisteredCountry.GeonameId),
       IntToStr(node.Data.City.GeonameId), cityNameEN,
       IntToStr(node.Data.Location.Accuracy),
       FloatToStrF(node.Data.Location.Latitude, ffFixed, 4, 2, fs),
       FloatToStrF(node.Data.Location.Longitude, ffFixed, 4, 2, fs),
       node.Data.Location.TimeZone]);
  end;

var
//  ipInfo: TMMDBIPInfo;
//  enumerator: IEnumerator<TMMDBIteratorNode<TMMDBIPInfo>>;
  countryIterator: IMMDBIterator<TMMDBIPCountryInfo>;
  cityIterator: IMMDBIterator<TMMDBIPCountryCityInfoEx>;
  displayLinesCount: Integer;
  IPv4Only: Boolean;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  fs := TFormatSettings.Create(GetThreadLocale());
  {$WARN SYMBOL_PLATFORM ON}
  fs.DecimalSeparator := '.';
  displayLinesCount := StrToInt(Edit2.Text);
  IPv4Only := CheckBox1.Checked;
  LLines := Memo1.Lines;
  LLines.Clear;
  if Assigned(FMMDBReader) then FreeAndNil(FMMDBReader);
  Button2.Enabled := False;
  Button3.Enabled := False;
  FMMDBReader := TMMDBReader.Create(Filename);
  Button2.Enabled := True;
  Button3.Enabled := True;
  LLines.BeginUpdate;
  try
    LLines.Add(Format('binary_format_major_version=%d', [FMMDBReader.Metadata.BinaryFormatMajorVersion]));
    LLines.Add(Format('binary_format_minor_version=%d', [FMMDBReader.Metadata.BinaryFormatMinorVersion]));
    LLines.Add(Format('build_epoch=%s', [DateToStr(UnixToDateTime(FMMDBReader.Metadata.BuildEpoch))]));
    LLines.Add(Format('database_type=%s', [FMMDBReader.Metadata.DatabaseType]));
    for LDictItem in FMMDBReader.Metadata.Description do
      LLines.Add(Format('description.%s=%s', [LDictItem.Key, LDictItem.Value]));
    LLines.Add(Format('ip_version=%d', [FMMDBReader.Metadata.IPVersion]));
    for I := 0 to FMMDBReader.Metadata.Languages.Count-1 do
      LLines.Add(Format('language[%d]=%s', [I, FMMDBReader.Metadata.languages[I]]));
    LLines.Add(Format('node_count=%s', [IntToStr(FMMDBReader.Metadata.NodeCount)]));
    LLines.Add(Format('record_size=%d', [FMMDBReader.Metadata.RecordSize]));
{$IFDEF DEBUG_IP}
    TestIP(DEBUG_IPv4, 'DEBUG_IP_');
    TestIP(DEBUG_IPv6, 'DEBUG_IP_');
{$ENDIF}
    if EndsText('-city', FMMDBReader.Metadata.DatabaseType) then
    begin
      LLines.Add(String.Join(',',
        ['network','continent_code','continent_geoname_id','country_iso_code',
        'country_geoname_id','registered_country_iso_code','registered_country_geoname_id',
        'city_geoname_id','city_names_en','accuracy_radius','latitude',
        'longitude','time_zone'],
        0, 13));
      for cityIterator in FMMDBReader.FindAll<TMMDBIPCountryCityInfoEx>(IPv4Only) do
      begin
        LLines.Add(FormatIPCountryCityNode(cityIterator.Node));
        if LLines.Count >= displayLinesCount then Break;
      end;
    end else
    begin
      LLines.Add(String.Join(',',
        ['network','continent_code','continent_geoname_id','country_iso_code',
        'country_geoname_id','registered_country_iso_code','registered_country_geoname_id'],
        0, 7));
      for countryIterator in FMMDBReader.FindAll<TMMDBIPCountryInfo>(IPv4Only) do
      begin
        LLines.Add(FormatIPCountryNode(countryIterator.Node));
        if LLines.Count >= displayLinesCount then Break;
      end;
    end;
  finally
    LLines.EndUpdate;
  end;
end;

procedure TForm1.TestIP(const ipString, outPrefix: String);
var
  LLines: TStrings;
  ipAddress: TMMDBIPAddress;
  netAddress: TMMDBIPAddress;
  rawAddress: TBytes;
  bitLength: Integer;
  ipVersion: Integer;
  prefixLength: Integer;
  ipCountryInfo: TMMDBIPCountryInfoEx;
  ipCityInfo: TMMDBIPCountryCityInfoEx;
  ipInfo: TMMDBIPCountryInfoEx;
  ipInfoFound: Boolean;
  i, j, h: Integer;
  s: String;
  nameKey: String;
begin
  LLines := Memo1.Lines;
  ipAddress := TMMDBIPAddress.Parse(ipString);
  rawAddress := ipAddress.GetAddressBytes;
  bitLength := Length(rawAddress) * 8;
  if Length(rawAddress) > 4 then
    ipVersion := 6
  else
    ipVersion := 4;
  ipCountryInfo := TMMDBIPCountryInfoEx.Create;
  ipCityInfo := TMMDBIPCountryCityInfoEx.Create;
  try
    if EndsText('-city', FMMDBReader.Metadata.DatabaseType) then
      ipInfoFound := FMMDBReader.Find<TMMDBIPCountryCityInfoEx>(ipAddress, prefixLength, ipCityInfo)
    else
      ipInfoFound := FMMDBReader.Find<TMMDBIPCountryInfoEx>(ipAddress, prefixLength, ipCountryInfo);
    if ipInfoFound then
    begin
      i := prefixLength;
      while i < bitLength do
      begin
        h := 8 - (i mod 8);
        rawAddress[i shr 3] :=
          (rawAddress[i shr 3] shr h) shl h;
        i := ((i shr 3) shl 3) + 8;
      end;
      netAddress := TMMDBIPAddress.Create(rawAddress);
      if EndsText('-city', FMMDBReader.Metadata.DatabaseType) then
        ipInfo := ipCityInfo
      else
        ipInfo := ipCountryInfo;
      s := Format('%s%s=%s',
        [outPrefix, ipString, Format(
          'ip:{address:"%s",version:%d},network:"%s/%d",continent:{code:"%s",geoname_id:%s},country:{iso_code:"%s",geoname_id:%s},registered_country:{iso_code:"%s",geoname_id:%s}',
          [ipAddress.ToString, ipVersion, netAddress.ToString, prefixLength, ipInfo.Continent.Code, IntToStr(ipInfo.Continent.GeonameId), ipInfo.Country.ISOCode,
          IntToStr(ipInfo.country.GeonameId), ipInfo.registeredCountry.ISOCode, IntToStr(ipInfo.RegisteredCountry.GeonameId)])]);
      if EndsText('-city', FMMDBReader.Metadata.DatabaseType) then
      begin
        s := s + Format(',city:{geoname_id:%s,names:[', [IntToStr(ipCityInfo.City.GeonameId)]);
        j := 0;
        for nameKey in ipCityInfo.City.Names.Keys do
        begin
          s := s + Format('%s%s="%s"', [IfThen(j > 0, ',', ''), nameKey, ipCityInfo.City.Names[nameKey]]);
          Inc(j);
        end;
        s := s + ']}';
      end;
      LLines.Add(s);
    end else
      LLines.Add(Format('%s%s=%s', [outPrefix, ipString, 'NOT FOUND']));
  finally
    ipCityInfo.Free;
    ipCountryInfo.Free;
  end;
end;

end.
