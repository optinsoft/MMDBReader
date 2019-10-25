{ *************************************************************************** }
{                                                                             }
{ uMMDBIPAddress - This module is a part of the MMDB Reader project           }
{                                                                             }
{ Created by Vitaly, Opt-In Software                                          }
{ Website: http://optinsoft.net/                                              }
{ License: Free to use, free to modify                                        }
{                                                                             }
{ Last edit by: Vitaly                                                        }
{ Date: October 22, 2019                                                      }
{ Version: 1.0                                                                }
{                                                                             }
{ *************************************************************************** }

unit uMMDBIPAddress;

interface

uses
  System.SysUtils, System.Classes, IPTypesX;

type
  TMMDBIPAddress = record
  private
    constructor Create(ipv4: TIPv4); overload;
    constructor Create(ipv6: TIPv6); overload;
  public
    constructor Create(const address: TBytes); overload;
    function GetAddressBytes: TBytes;
    class function TryParse(const ipString: String; out address: TMMDBIPAddress): Boolean; static;
    class function Parse(const ipString: String): TMMDBIPAddress; static;
    function ToString(): String;
  private
    case _v6: Boolean of
      False: (_ipv4: TIPv4);
      True:  (_ipv6: TIPv6);
  end;

implementation

const
  SBadAddressSize = 'Bad address size: %d.';
  SUnableConvertStringToIP = 'Unable to convert string "%s" to IP address.';

{ TMMDBIPAddress }

constructor TMMDBIPAddress.Create(ipv4: TIPv4);
begin
  _v6 := False;
  Move(ipv4, _ipv4, sizeof(TIPv4));
end;

constructor TMMDBIPAddress.Create(ipv6: TIPv6);
begin
  _v6 := True;
  Move(ipv6, _ipv6, sizeof(TIPv6));
end;

constructor TMMDBIPAddress.Create(const address: TBytes);
var
  I: Integer;
begin
  case Length(address) of
    4:
      begin
        _v6 := False;
        //convert network to host bytes order
        for I := 0 to 3 do
          _ipv4.Groups[3-I] := address[I];
      end;
    16:
      begin
        _v6 := True;
        //convert network to host bytes order
        for I := 0 to 7 do
          _ipv6.Groups[7-I] := (Word(address[I*2]) shl 8) or Word(address[I*2+1]);
      end;
  else
    raise Exception.Create(Format(SBadAddressSize, [Length(address)]));
  end;
end;

function TMMDBIPAddress.GetAddressBytes: TBytes;
var
  I: Integer;
begin
  if _v6 then
  begin
    SetLength(Result, 16);
    //convert host to network bytes order
    for I := 0 to 7 do
    begin
      Result[I*2]   := (_ipv6.Groups[7-I] and $ff00) shr 8;
      Result[I*2+1] := (_ipv6.Groups[7-I] and $ff);
    end;
  end else
  //if not _v6 then
  begin
    SetLength(Result, 4);
    //convert host to network bytes order
    for I := 0 to 3 do
      Result[I] := _ipv4.Groups[3-I];
  end;
end;

class function TMMDBIPAddress.Parse(const ipString: String): TMMDBIPAddress;
begin
  if not TryParse(ipString, Result) then
    raise Exception.Create(Format(SUnableConvertStringToIP, [ipString]));
end;

function TMMDBIPAddress.ToString: String;
begin
  if _v6 then
    Exit(LowerCase(IPv6ToStrCompr(_ipv6)));
  Result := IPv4ToStr(_ipv4);
end;

class function TMMDBIPAddress.TryParse(const ipString: String;
  out address: TMMDBIPAddress): Boolean;
var
  ipv4: TIPv4;
  ipv6: TIPv6;
begin
  ipv4 := TryStrToIPv4(ipString, False, Result);
  if Result then
  begin
    address := TMMDBIPAddress.Create(ipv4);
    Exit;
  end;
  ipv6 := TryStrToIPv6(ipString, False, Result);
  if Result then
  begin
    address := TMMDBIPAddress.Create(ipv6);
    Exit;
  end;
  Result := False;
end;

end.
