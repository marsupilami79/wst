{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit indy_http_protocol;

{.$DEFINE WST_DBG}

interface

uses
  Classes, SysUtils,
  service_intf, imp_utils, base_service_intf, wst_types, filter_intf,
  client_utils, IdHTTP;

Const
  sTRANSPORT_NAME = 'HTTP';

Type

  { THTTPTransport }
  THTTPTransport = class(TBaseTransport,ITransport)
  Private
    FFormat : string;
    FConnection : TidHttp;
    FSoapAction: string;
    FContentType: string;
  private
    function GetAddress: string;
    function GetProtocolVersion : string;
    function GetProxyPassword: string;
    function GetProxyPort: Integer;
    function GetProxyServer: string;
    function GetProxyUsername: string;
    procedure SetAddress(const AValue: string);
    procedure SetProtocolVersion(const AValue : string);
    procedure SetProxyPassword(const AValue: string);
    procedure SetProxyPort(const AValue: Integer);
    procedure SetProxyServer(const AValue: string);
    procedure SetProxyUsername(const AValue: string);
  public
    constructor Create();override;
    destructor Destroy();override;
    procedure SendAndReceive(ARequest,AResponse:TStream); override;
  published
    property ContentType : string Read FContentType Write FContentType;
    property Address : string Read GetAddress Write SetAddress;
    property ProxyServer : string Read GetProxyServer Write SetProxyServer;
    property ProxyPort : Integer Read GetProxyPort Write SetProxyPort;
    property ProxyUsername : string read GetProxyUsername write SetProxyUsername;
    property ProxyPassword : string read GetProxyPassword write SetProxyPassword;
    property SoapAction : string read FSoapAction write FSoapAction;
    property Format : string read FFormat write FFormat;
    property ProtocolVersion : string read GetProtocolVersion write SetProtocolVersion;
  End;

  procedure INDY_RegisterHTTP_Transport();

implementation
uses
  wst_consts;

const
  ProtocolVersionMAP : array[TIdHTTPProtocolVersion] of string = ('1.0', '1.1');

function TryStrToProtocolVersion(
  const AStr : string;
  out   ARes : TIdHTTPProtocolVersion
) : Boolean;
var
  i : TIdHTTPProtocolVersion;
begin
  for i := Low(TIdHTTPProtocolVersion) to High(TIdHTTPProtocolVersion) do begin
    if ( AStr = ProtocolVersionMAP[i] ) then begin
      ARes := i;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;


{ THTTPTransport }

function THTTPTransport.GetAddress: string;
begin
  Result := FConnection.Request.URL;
end;

function THTTPTransport.GetProtocolVersion : string;
begin
  Result := ProtocolVersionMAP[FConnection.ProtocolVersion];
end;

function THTTPTransport.GetProxyPassword: string;
begin
  Result := FConnection.ProxyParams.ProxyPassword;
end;

function THTTPTransport.GetProxyPort: Integer;
begin
  Result := FConnection.ProxyParams.ProxyPort;
end;

function THTTPTransport.GetProxyServer: string;
begin
  Result := FConnection.ProxyParams.ProxyServer;
end;

function THTTPTransport.GetProxyUsername: string;
begin
  Result := FConnection.ProxyParams.ProxyUsername;
end;

procedure THTTPTransport.SetAddress(const AValue: string);
begin
  FConnection.Request.URL := AValue;
end;

procedure THTTPTransport.SetProtocolVersion(const AValue : string);
var
  locValue : TIdHTTPProtocolVersion;
begin
  if not TryStrToProtocolVersion(AValue,locValue) then
    raise ETransportExecption.CreateFmt(SERR_InvalidPropertyValue,['ProtocolVersion',AValue]);
  FConnection.ProtocolVersion := locValue;
  if not ( hoKeepOrigProtocol in FConnection.HTTPOptions ) then
    FConnection.HTTPOptions := FConnection.HTTPOptions + [hoKeepOrigProtocol];
end;

procedure THTTPTransport.SetProxyPassword(const AValue: string);
begin
  FConnection.ProxyParams.ProxyPassword := AValue;
end;

procedure THTTPTransport.SetProxyPort(const AValue: Integer);
begin
  FConnection.ProxyParams.ProxyPort := AValue;
end;

procedure THTTPTransport.SetProxyServer(const AValue: string);
begin
  FConnection.ProxyParams.ProxyServer := AValue;
end;

procedure THTTPTransport.SetProxyUsername(const AValue: string);
begin
  FConnection.ProxyParams.ProxyUsername := AValue;
end;

constructor THTTPTransport.Create();
begin
  inherited;
  FConnection := TidHttp.Create(Nil);
end;

destructor THTTPTransport.Destroy();
begin
  FreeAndNil(FConnection);
  inherited Destroy();
end;

procedure THTTPTransport.SendAndReceive(ARequest, AResponse: TStream);
var
  locTempStream, locTempRes : TMemoryStream;
{$IFDEF WST_DBG}
  s : TBinaryString;
  i : Int64;
{$ENDIF WST_DBG}
begin
  if not ( IsStrEmpty(FConnection.ProxyParams.ProxyUsername) and
           IsStrEmpty(FConnection.ProxyParams.ProxyPassword)
         )
  then begin
    FConnection.ProxyParams.BasicAuthentication := True;
  end;
  FConnection.Request.CustomHeaders.Clear();
  FConnection.Request.CustomHeaders.Values['SOAPAction'] := SoapAction;
  FConnection.Request.ContentType := ContentType;
{$IFDEF WST_DBG}
  TMemoryStream(ARequest).SaveToFile('request.log');
{$ENDIF WST_DBG}
  if not HasFilter() then begin
    FConnection.Post(Address,ARequest, AResponse);
  end else begin
    locTempRes := nil;
    locTempStream := TMemoryStream.Create();
    try
      FilterInput(ARequest,locTempStream);
{$IFDEF WST_DBG}
      TMemoryStream(locTempStream).SaveToFile('request.log.wire');
{$ENDIF WST_DBG}
      locTempRes := TMemoryStream.Create();
      FConnection.Post(Address,locTempStream,locTempRes);
  {$IFDEF WST_DBG}
      TMemoryStream(locTempRes).SaveToFile('response.log.wire');
  {$ENDIF WST_DBG}
      FilterOutput(locTempRes,AResponse);
    finally
      locTempRes.Free();
      locTempStream.Free();
    end;
  end;
  {$IFDEF WST_DBG}
  if IsConsole then begin
    i := AResponse.Size;
    SetLength(s,i);
    Move(TMemoryStream(AResponse).Memory^,s[1],i);
    WriteLn('--------------------------------------------');
    WriteLn(s);
  end;
  TMemoryStream(AResponse).SaveToFile('response.log');
  {$ENDIF WST_DBG}
end;

procedure INDY_RegisterHTTP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(THTTPTransport) as IItemFactory);
end;


end.
