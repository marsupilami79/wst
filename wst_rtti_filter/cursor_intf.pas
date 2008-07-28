{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit cursor_intf;

interface

uses
  Classes, SysUtils; 

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

type
  IInterfaceCursor = interface;
  IObjectCursor  = interface;
  IDefaultTypedCursor = {$IFDEF WST_INTF_DOM}IInterfaceCursor{$ELSE}IObjectCursor{$ENDIF};
  IDefaultItemType = {$IFDEF WST_INTF_DOM}IInterface{$ELSE}TObject{$ENDIF};

  ECursorException = class(Exception)
  end;

  ICursor = interface
    ['{2B7756B1-E239-4B6F-A7A3-4B57B98FAD4F}']
    procedure Reset();
    function MoveNext() : Boolean;
    //It is just the cursor that is cloned, the underliying datas are shared
    function Clone():ICursor;
    function GetCount() : PtrInt;
  end;
  
  IObjectFilter = interface
    ['{3DFB1A26-ED2D-428A-9F62-2091A076D97B}']
    function Evaluate(const AObject : TObject) : Boolean;
  end;

  IObjectCursor = interface(ICursor)
    ['{13E9C22D-0508-4D7A-A969-96E2291B4FE8}']
    function GetCurrent() : TObject;
  end;

  IInterfaceCursor = interface(ICursor)
    ['{82FCF6F3-8008-4CCA-99DA-88945B250B92}']
    function GetCurrent() : IInterface;
  end;

  IFilterableObjectCursor = interface(IObjectCursor)
    ['{F11B588A-E8CF-45D3-98D2-B49755FFC22D}']
    function GetFilter() : IObjectFilter;
    function SetFilter(const AFilter : IObjectFilter) : IObjectFilter;
  end;

  function CreateCursorOn(
    AInputCursor : IObjectCursor;
    AFilter      : IObjectFilter
  ) : IFilterableObjectCursor ;
  
implementation

type

  { TSimpleObjectFilterableCursor }

  TSimpleObjectFilterableCursor = class(
    TInterfacedObject,
    ICursor,IObjectCursor,IFilterableObjectCursor
  )
  private
    FBaseCursor : IObjectCursor;
    FFilter : IObjectFilter;
  protected
    procedure Reset();
    function MoveNext() : Boolean;
    function Clone():ICursor;
    function GetCurrent() : TObject;
    function GetCount() : PtrInt;
    function GetFilter() : IObjectFilter;
    function SetFilter(const AFilter : IObjectFilter) : IObjectFilter;
  public
    constructor Create(
      AInputCursor : IObjectCursor;
      AFilter      : IObjectFilter
    );
  end;

function CreateCursorOn(
  AInputCursor : IObjectCursor;
  AFilter      : IObjectFilter
) : IFilterableObjectCursor ;
begin
  Result := TSimpleObjectFilterableCursor.Create(AInputCursor,AFilter);
end;


{ TSimpleObjectFilterableCursor }

procedure TSimpleObjectFilterableCursor.Reset();
begin
  FBaseCursor.Reset();
end;

function TSimpleObjectFilterableCursor.MoveNext(): Boolean;
begin
  if ( FFilter = nil ) then begin
    Result := FBaseCursor.MoveNext();
  end else begin
    while FBaseCursor.MoveNext() do begin
      if FFilter.Evaluate(FBaseCursor.GetCurrent()) then begin
        Result := True;
        exit;
      end;
    end;
    Result := False;
  end;
end;

function TSimpleObjectFilterableCursor.Clone(): ICursor;
var
  baseClone : ICursor;
begin
  Result := nil;
  baseClone := FBaseCursor.Clone();
  if ( baseClone <> nil ) then
    Result := TSimpleObjectFilterableCursor.Create(baseClone as IObjectCursor,FFilter);
end;

function TSimpleObjectFilterableCursor.GetCurrent(): TObject;
begin
  Result := FBaseCursor.GetCurrent();
end;

function TSimpleObjectFilterableCursor.GetCount() : PtrInt;
var
  crs : ICursor;
begin
  if ( FFilter = nil ) then begin
    Result := FBaseCursor.GetCount();
  end else begin
    crs := Self.Clone();
    crs.Reset();
    Result := 0;
    while crs.MoveNext() do begin
      Result := Result + 1;
    end;
  end;
end;

function TSimpleObjectFilterableCursor.GetFilter(): IObjectFilter;
begin
  Result := FFilter;
end;

function TSimpleObjectFilterableCursor.SetFilter(const AFilter: IObjectFilter): IObjectFilter;
begin
  FFilter := AFilter;
  Result := FFilter;
end;

constructor TSimpleObjectFilterableCursor.Create(
  AInputCursor : IObjectCursor;
  AFilter      : IObjectFilter
);
begin
  Assert(Assigned(AInputCursor));
  inherited Create();
  FBaseCursor := AInputCursor;
  FFilter := AFilter;
end;


end.
