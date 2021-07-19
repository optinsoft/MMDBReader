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
{ Date: July 19, 2021                                                          }
{ Version: 1.5                                                                 }
{                                                                              }
{ Changelog:                                                                   }
{ v1. 5:                                                                       }
{ - show mmdb formats: *-domain, *-anonymous-ip, *-isp, *-asn                  }
{                                                                              }
{ Changelog:                                                                   }
{ v1.4:                                                                        }
{ - print city location info in TestIP()                                       }
{                                                                              }
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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uMMDBReader, uMMDBInfo, uMMDBIPAddress;

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
    procedure FormCreate(Sender: TObject);
  private type
    TForEachLine = reference to procedure(const ALine: String);
    TReduceRef = reference to function (const R: String; const A: array of String; I: Integer): String;
  private
    { Private declarations }
    FMMDBReader: TMMDBReader;
    FExportCSVFilename: String;
    fs: TFormatSettings;
    procedure ReadMMDB(const Filename: String);
    procedure TestIP(const ipString: String; const outPrefix: String = '');
    procedure ExportToCSV;
    procedure ExportStarted(Sender: TObject);
    procedure ExportTerminated(Sender: TObject);
    procedure MMDBFindAll(AIPv4Only: Boolean; AForEachLine: TForEachLine;
      AMaxLinesCount: Integer = 0; AOnHeaderLine: TForEachLine = nil);
    function EscapeString(const S: String): String;
    function Reduce(const A: array of String; L: TReduceRef): String;
    function FormatRow(const columns: array of String): String;
    function NodeNetwork(rawAddress: TBytes; nodePrefix: Integer): String;
    function FormatIPCountryInfo(const network: String; info: TMMDBIPCountryInfoEx): String;
    function FormatIPCountryCityInfo(const network: String; info: TMMDBIPCountryCityInfoEx): String;
    function FormatISPInfo(const network: String; info: TMMDBISP): String;
    function FormatASNInfo(const network: String; info: TMMDBASN): String;
    function FormatAnonymousIPInfo(const network: String; info: TMMDBAnonymousIP): String;
    function FormatDomainInfo(const network: String; info: TMMDBDomain): String;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.Generics.Collections,
  System.DateUtils, System.StrUtils;

{$IFDEF DEBUG}
{.$DEFINE DEBUG_IP}
{$ENDIF}

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$WARN SYMBOL_PLATFORM OFF}
  fs := TFormatSettings.Create(GetThreadLocale());
  {$WARN SYMBOL_PLATFORM ON}
  fs.DecimalSeparator := '.';
end;

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
{
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

  function FormatIPCountryNode(node: TMMDBIteratorNode<TMMDBIPCountryInfoEx>): String;
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
}
var
  IPv4Only: Boolean;
  LOutStream: TStream;
begin
  LOutStream := nil;
  IPv4Only := CheckBox1.Checked;
  try
    LOutStream := TFileStream.Create(FExportCSVFilename, fmCreate or fmShareDenyWrite);
    MMDBFindAll(IPv4Only,
      procedure (const ALine: String)
      var Buffer: TBytes;
      begin
        Buffer := TEncoding.UTF8.GetBytes(ALine + sLineBreak);
        LOutStream.WriteBuffer(Buffer, Length(Buffer));
      end);
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

procedure TForm1.MMDBFindAll(AIPv4Only: Boolean;
  AForEachLine: TForEachLine; AMaxLinesCount: Integer;
  AOnHeaderLine: TForEachLine);
var
  countryIterator: IMMDBIterator<TMMDBIPCountryInfoEx>;
  cityIterator: IMMDBIterator<TMMDBIPCountryCityInfoEx>;
  ispIterator: IMMDBIterator<TMMDBISP>;
  asnIterator: IMMDBIterator<TMMDBASN>;
  anonymousIterator: IMMDBIterator<TMMDBAnonymousIP>;
  domainIterator: IMMDBIterator<TMMDBDomain>;
  N: Integer;
begin
  N := 0;
  if EndsText('-domain', FMMDBReader.Metadata.DatabaseType)  then
  begin
    if Assigned(AOnHeaderLine) then
      AOnHeaderLine(FormatRow([
        'network','domain'
      ]));
    for domainIterator in FMMDBReader.FindAll<TMMDBDomain>(AIPv4Only) do
    begin
      AForEachLine(FormatDomainInfo(
        NodeNetwork(domainIterator.Node.Start.GetAddressBytes, domainIterator.Node.Prefix),
        domainIterator.Node.Data));
      Inc(N);
      if (AMaxLinesCount > 0) and (N >= AMaxLinesCount) then Break;
    end;
  end else
  if EndsText('-anonymous-ip', FMMDBReader.Metadata.DatabaseType)  then
  begin
    if Assigned(AOnHeaderLine) then
      AOnHeaderLine(FormatRow([
        'network','is_anonymous','is_anonymous_vpn','is_hosting_provider',
        'is_public_proxy','is_residential_proxy','is_tor_exit_node'
      ]));
    for anonymousIterator in FMMDBReader.FindAll<TMMDBAnonymousIP>(AIPv4Only) do
    begin
      AForEachLine(FormatAnonymousIPInfo(
        NodeNetwork(anonymousIterator.Node.Start.GetAddressBytes, anonymousIterator.Node.Prefix),
        anonymousIterator.Node.Data));
      Inc(N);
      if (AMaxLinesCount > 0) and (N >= AMaxLinesCount) then Break;
    end;
  end else
  if EndsText('-isp', FMMDBReader.Metadata.DatabaseType) then
  begin
    if Assigned(AOnHeaderLine) then
      AOnHeaderLine(FormatRow([
        'network','isp','organization','autonomous_system_number','autonomous_system_organization'
      ]));
    for ispIterator in FMMDBReader.FindAll<TMMDBISP>(AIPv4Only) do
    begin
      AForEachLine(FormatISPInfo(
        NodeNetwork(ispIterator.Node.Start.GetAddressBytes, ispIterator.Node.Prefix),
        ispIterator.Node.Data));
      Inc(N);
      if (AMaxLinesCount > 0) and (N >= AMaxLinesCount) then Break;
    end;
  end else
  if EndsText('-asn', FMMDBReader.Metadata.DatabaseType) then
  begin
    if Assigned(AOnHeaderLine) then
      AOnHeaderLine(FormatRow([
        'network','autonomous_system_number','autonomous_system_organization'
      ]));
    for asnIterator in FMMDBReader.FindAll<TMMDBASN>(AIPv4Only) do
    begin
      AForEachLine(FormatASNInfo(
        NodeNetwork(asnIterator.Node.Start.GetAddressBytes, asnIterator.Node.Prefix),
        asnIterator.Node.Data));
      Inc(N);
      if (AMaxLinesCount > 0) and (N >= AMaxLinesCount) then Break;
    end;
  end else
  if EndsText('-city', FMMDBReader.Metadata.DatabaseType) then
  begin
    if Assigned(AOnHeaderLine) then
      AOnHeaderLine(FormatRow([
        'network','continent_code','continent_geoname_id','country_iso_code',
        'country_geoname_id','registered_country_iso_code','registered_country_geoname_id',
        'city_geoname_id','city_names_en','accuracy_radius','latitude',
        'longitude','time_zone'
      ]));
    for cityIterator in FMMDBReader.FindAll<TMMDBIPCountryCityInfoEx>(AIPv4Only) do
    begin
      AForEachLine(FormatIPCountryCityInfo(
        NodeNetwork(cityIterator.Node.Start.GetAddressBytes, cityIterator.Node.Prefix),
        cityIterator.Node.Data));
      Inc(N);
      if (AMaxLinesCount > 0) and (N >= AMaxLinesCount) then Break;
    end;
  end else
  begin
    if Assigned(AOnHeaderLine) then
      AOnHeaderLine(FormatRow([
        'network','continent_code','continent_geoname_id','country_iso_code',
        'country_geoname_id','registered_country_iso_code','registered_country_geoname_id'
      ]));
    for countryIterator in FMMDBReader.FindAll<TMMDBIPCountryInfoEx>(AIPv4Only) do
    begin
      AForEachLine(FormatIPCountryInfo(
        NodeNetwork(countryIterator.Node.Start.GetAddressBytes, countryIterator.Node.Prefix),
        countryIterator.Node.Data));
      Inc(N);
      if (AMaxLinesCount > 0) and (N >= AMaxLinesCount) then Break;
    end;
  end;
end;

procedure TForm1.ReadMMDB(const Filename: String);
var
  LLines: TStrings;
  LDictItem: TPair<string, string>;
  I: Integer;

{$IFDEF DEBUG_IP}
const
  DEBUG_IPv4 = '8.8.4.4';
  DEBUG_IPv6 = '2001:4860:4860::8844';
{$ENDIF}
var
//  ipInfo: TMMDBIPInfo;
//  enumerator: IEnumerator<TMMDBIteratorNode<TMMDBIPInfo>>;
  displayLinesCount: Integer;
  IPv4Only: Boolean;
begin
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
    MMDBFindAll(IPv4Only,
      procedure (const ALine: String) begin LLines.Add(ALine); end,
      displayLinesCount,
      procedure (const ALine: String) begin LLines.Add(ALine); end);
  finally
    LLines.EndUpdate;
  end;
end;

procedure TForm1.TestIP(const ipString, outPrefix: String);
var
  LLines: TStrings;
  ipAddress: TMMDBIPAddress;
  rawAddress: TBytes;
  prefixLength: Integer;
  ipCountryInfo: TMMDBIPCountryInfoEx;
  ipCityInfo: TMMDBIPCountryCityInfoEx;
  ispInfo: TMMDBISP;
  asnInfo: TMMDBASN;
  anonymousInfo: TMMDBAnonymousIP;
  domainInfo: TMMDBDomain;
  ipInfoFound: Boolean;
begin
  LLines := Memo1.Lines;
  ipAddress := TMMDBIPAddress.Parse(ipString);
  rawAddress := ipAddress.GetAddressBytes;
  ipCountryInfo := TMMDBIPCountryInfoEx.Create;
  ipCityInfo := TMMDBIPCountryCityInfoEx.Create;
  ispInfo := TMMDBISP.Create;
  asnInfo := TMMDBASN.Create;
  anonymousInfo := TMMDBAnonymousIP.Create;
  domainInfo := TMMDBDomain.Create;
  try
    if EndsText('-domain', FMMDBReader.Metadata.DatabaseType) then
    begin
      ipInfoFound := FMMDBReader.Find<TMMDBDomain>(ipAddress, prefixLength, domainInfo);
      if ipInfoFound then
        LLines.Add(Format('%s%s => ', [outPrefix, ipString]) +
          FormatDomainInfo(NodeNetwork(rawAddress, prefixLength), domainInfo));
    end else
    if EndsText('-anonymous-ip', FMMDBReader.Metadata.DatabaseType) then
    begin
      ipInfoFound := FMMDBReader.Find<TMMDBAnonymousIP>(ipAddress, prefixLength, anonymousInfo);
      if ipInfoFound then
        LLines.Add(Format('%s%s => ', [outPrefix, ipString]) +
          FormatAnonymousIPInfo(NodeNetwork(rawAddress, prefixLength), anonymousInfo));
    end else
    if EndsText('-isp', FMMDBReader.Metadata.DatabaseType) then
    begin
      ipInfoFound := FMMDBReader.Find<TMMDBISP>(ipAddress, prefixLength, ispInfo);
      if ipInfoFound then
        LLines.Add(Format('%s%s => ', [outPrefix, ipString]) +
          FormatISPInfo(NodeNetwork(rawAddress, prefixLength), ispInfo));
    end else
    if EndsText('-asn', FMMDBReader.Metadata.DatabaseType) then
    begin
      ipInfoFound := FMMDBReader.Find<TMMDBASN>(ipAddress, prefixLength, asnInfo);
      if ipInfoFound then
        LLines.Add(Format('%s%s => ', [outPrefix, ipString]) +
          FormatASNInfo(NodeNetwork(rawAddress, prefixLength), asnInfo));
    end else
    if EndsText('-city', FMMDBReader.Metadata.DatabaseType) then
    begin
      ipInfoFound := FMMDBReader.Find<TMMDBIPCountryCityInfoEx>(ipAddress, prefixLength, ipCityInfo);
      if ipInfoFound then
        LLines.Add(Format('%s%s => ', [outPrefix, ipString]) +
          FormatIPCountryCityInfo(NodeNetwork(rawAddress, prefixLength), ipCityInfo));
    end else
    begin
      ipInfoFound := FMMDBReader.Find<TMMDBIPCountryInfoEx>(ipAddress, prefixLength, ipCountryInfo);
      if ipInfoFound then
        LLines.Add(Format('%s%s => ', [outPrefix, ipString]) +
          FormatIPCountryInfo(NodeNetwork(rawAddress, prefixLength), ipCountryInfo));
    end;
    if not ipInfoFound then
      LLines.Add(Format('%s%s => %s', [outPrefix, ipString, 'NOT FOUND']));
  finally
    domainInfo.Free;
    anonymousInfo.Free;
    asnInfo.Free;
    ispInfo.Free;
    ipCityInfo.Free;
    ipCountryInfo.Free;
  end;
end;

function TForm1.EscapeString(const S: string): string;
const
  Delimiter: Char = ',';
  Enclosure: Char = '"';
begin
  Result := StringReplace(S, Enclosure, Enclosure+Enclosure, [rfReplaceAll]);
  if (Pos(Delimiter, S) > 0) or (Pos(Enclosure, S) > 0) then
      Result := Enclosure + Result + Enclosure;
end;

function TForm1.Reduce(const A: array of String; L: TReduceRef): String;
var
  I: Integer;
begin
  Result := '';
  for I := Low(A) to High(A) do
    Result := L(Result, A, I);
end;

function TForm1.FormatRow(const columns: array of String): String;
begin
  //Result := String.Join(',', columns, 0, Length(columns));
  Result := Reduce(columns,
    function (const R: String; const A: array of String; I: Integer): String
    begin
      if I > Low(A) then
        Result := Result + ',';
      Result := Result + EscapeString(A[I]);
    end);
end;

function TForm1.NodeNetwork(rawAddress: TBytes; nodePrefix: Integer): String;
var
  bitLength: Integer;
  i, h: Integer;
begin
  bitLength := Length(rawAddress) * 8;
  i := nodePrefix;
  while i < bitLength do
  begin
    h := 8 - (i mod 8);
    rawAddress[i shr 3] :=
      (rawAddress[i shr 3] shr h) shl h;
    i := ((i shr 3) shl 3) + 8;
  end;
  Result := TMMDBIPAddress.Create(rawAddress).ToString + '/' + IntToStr(nodePrefix);
end;

function TForm1.FormatIPCountryInfo(const network: String; info: TMMDBIPCountryInfoEx): String;
begin
  Result := FormatRow(
    [network,
     info.Continent.code, IntToStr(info.Continent.GeonameId),
     info.Country.ISOCode, IntToStr(info.Country.GeonameId),
     info.RegisteredCountry.ISOCode, IntToStr(info.RegisteredCountry.GeonameId)]);
end;

function TForm1.FormatIPCountryCityInfo(const network: String; info: TMMDBIPCountryCityInfoEx): String;
var
  cityNameEN: String;
begin
  if not info.City.Names.TryGetValue('en', cityNameEN) then cityNameEN := '';
  Result := FormatRow(
    [network,
     info.Continent.code, IntToStr(info.Continent.GeonameId),
     info.Country.ISOCode, IntToStr(info.Country.GeonameId),
     info.RegisteredCountry.ISOCode, IntToStr(info.RegisteredCountry.GeonameId),
     IntToStr(info.City.GeonameId), cityNameEN,
     IntToStr(info.Location.Accuracy),
     FloatToStrF(info.Location.Latitude, ffFixed, 4, 2, fs),
     FloatToStrF(info.Location.Longitude, ffFixed, 4, 2, fs),
     info.Location.TimeZone]);
end;

function TForm1.FormatISPInfo(const network: String; info: TMMDBISP): String;
begin
  Result := FormatRow(
    [network,
     info.ISP, info.Organization, IntToStr(info.AutonomousSystemNumber),
     info.AutonomousSystemOrganization]);
end;

function TForm1.FormatASNInfo(const network: String; info: TMMDBASN): String;
begin
  Result := FormatRow(
    [network,
     IntToStr(info.AutonomousSystemNumber),
     info.AutonomousSystemOrganization]);
end;

function TForm1.FormatDomainInfo(const network: String;
  info: TMMDBDomain): String;
begin
  Result := FormatRow([network, info.Domain]);
end;

function TForm1.FormatAnonymousIPInfo(const network: String; info: TMMDBAnonymousIP): String;
begin
  Result := FormatRow(
    [network,
    BoolToStr(info.IsAnonymous, True),
    BoolToStr(info.IsAnonymousVPN, True),
    BoolToStr(info.IsHostingProvider, True),
    BoolToStr(info.IsPublicProxy, True),
    BoolToStr(info.IsResidentialProxy, True),
    BoolToStr(info.IsTorExitNode, True)]);
end;


end.
