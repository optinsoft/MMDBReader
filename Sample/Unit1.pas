{ **************************************************************************** }
{                                                                              }
{ Sample for the MMDB Reader project                                           }
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
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FMMDBReader: TMMDBReader;
    procedure ReadMMDB(const Filename: String);
    procedure TestIP(const ipString: String; const outPrefix: String = '');
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  uMMDBInfo, uMMDBIPAddress, System.Generics.Collections,
  System.DateUtils;

{$IFDEF DEBUG}
{.$DEFINE DEBUG_IP}
{$ENDIF}

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    ReadMMDB(OpenDialog1.FileName);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TestIP(Edit1.Text);
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

{$IFDEF DEBUG_IP}
const
  DEBUG_IPv4 = '8.8.4.4';
  DEBUG_IPv6 = '2001:4860:4860::8844';
{$ENDIF}

  procedure PrintNode(node: TMMDBIteratorNode<TMMDBIPInfo>);
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
    LLines.Add(Format('%s/%d,%s,%s,%s,%s,%s,%s',
      [netAddress.ToString, node.Prefix,
       node.Data.Continent.code, IntToStr(node.Data.Continent.GeonameId),
       node.Data.Country.ISOCode, IntToStr(node.Data.Country.GeonameId),
       node.Data.RegisteredCountry.ISOCode, IntToStr(node.Data.RegisteredCountry.GeonameId)]));
  end;

var
//  ipInfo: TMMDBIPInfo;
//  enumerator: IEnumerator<TMMDBIteratorNode<TMMDBIPInfo>>;
  iterator: IMMDBIterator<TMMDBIPInfo>;
begin
  LLines := Memo1.Lines;
  LLines.Clear;
  if Assigned(FMMDBReader) then FreeAndNil(FMMDBReader);
  Button2.Enabled := False;
  FMMDBReader := TMMDBReader.Create(Filename);
  Button2.Enabled := True;
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
    LLines.Add('network,continent_code,continent_geoname_id,country_iso_code,country_geoname_id,registered_country_iso_code,registered_country_geoname_id');
    for iterator in FMMDBReader.FindAll<TMMDBIPInfo>(True{IPv4Only}) do
    begin
      PrintNode(iterator.Node);
      if LLines.Count >= 1000 then Break;
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
  ipInfo: TMMDBIPInfo;
  i, h: Integer;
begin
  LLines := Memo1.Lines;
  ipAddress := TMMDBIPAddress.Parse(ipString);
  rawAddress := ipAddress.GetAddressBytes;
  bitLength := Length(rawAddress) * 8;
  if Length(rawAddress) > 4 then
    ipVersion := 6
  else
    ipVersion := 4;
  ipInfo := TMMDBIPInfo.Create;
  try
    if FMMDBReader.Find<TMMDBIPInfo>(ipAddress, prefixLength, ipInfo) then
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
      LLines.Add(Format('%s%s=%s',
        [outPrefix, ipString, AnsiQuotedStr(Format(
          'ip:{address:"%s",version:%d},network:"%s/%d",continent:{code:"%s",geoname_id:%s},country:{iso_code:"%s",geoname_id:%s},registered_country:{iso_code:"%s",geoname_id:%s}',
          [ipAddress.ToString, ipVersion, netAddress.ToString, prefixLength, ipInfo.Continent.Code, IntToStr(ipInfo.Continent.GeonameId), ipInfo.Country.ISOCode,
          IntToStr(ipInfo.country.GeonameId), ipInfo.registeredCountry.ISOCode, IntToStr(ipInfo.RegisteredCountry.GeonameId)]),
          '"')]));
    end else
      LLines.Add(Format('%s%s=%s', [outPrefix, ipString, 'NOT FOUND']));
  finally
    ipInfo.Free;
  end;
end;

end.
