{
    This file is part of the Web Service Toolkit
    Copyright (c) 2020 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$INCLUDE wst_global.inc}
unit generatorts;

interface

uses
  Classes, SysUtils,
  PasTree,
  pascal_parser_intf, source_utils, wst_types, generatorbase;      

type

{$SCOPEDENUMS ON}
  TSimpleTypeItem = (Simple, Extendable);
  TTypeScriptOption = (GenerateProxy);
  TTypeScriptOptions = set of TTypeScriptOption;

  { TInftGenerator }

  TInftGenerator = class(TBaseGenerator)
  private
    FStream : ISourceStream;      
    FOptionsEx : TTypeScriptOptions;
  private
    function GenerateIntfName(AIntf : TPasElement):string;
    function GenerateTypeText(AType : TPasType; const AItem : TSimpleTypeItem = TSimpleTypeItem.Simple) : string;

    procedure GenerateIntf(AIntf : TPasClassType);
    procedure GenerateIntfProxy(AIntf : TPasClassType);
    procedure GenerateClass(ASymbol : TPasClassType);
    procedure GenerateEnum(ASymbol : TPasEnumType);
    function GetDestUnitName():string;

    procedure GenerateEnums();
    procedure PrepareModule();
    procedure InternalExecute();
  public
    procedure Execute();override;
    property OptionsEx : TTypeScriptOptions read FOptionsEx write FOptionsEx;
  end;

implementation
uses
  contnrs,
  parserutils, logger_intf;

const
  TS_PROXY_BASE_CODE =
    'type ReadOnlyUnknownArray = readonly unknown[];' + sLineBreak + 
    '' + sLineBreak +
    'function buildCallData<T extends ReadOnlyUnknownArray>(aMethod : string, aArgs : T) : string {' + sLineBreak +
    '  return JSON.stringify({version: "1.1", method : aMethod, params : aArgs});' + sLineBreak +
    '}' + sLineBreak +
    '' + sLineBreak +
    'export interface DataTransporter {' + sLineBreak +
    '  execute(' + sLineBreak +
    '    aUrl  : string,' + sLineBreak +
    '    aData : string,' + sLineBreak +
    '    aCallback : (e? : Error, result? : string) => void' + sLineBreak +
    '  ) : void;' + sLineBreak +
    '}' + sLineBreak +
    '' + sLineBreak +
    'export interface JsonRpcError {' + sLineBreak +
    '  name    : string;' + sLineBreak +
    '  code    : number;' + sLineBreak +
    '  message : string;' + sLineBreak +
    '}' + sLineBreak +
    '' + sLineBreak +
    'interface JsonRpcEnv<T> {' + sLineBreak +
    '  version : string;' + sLineBreak +
    '  error   : JsonRpcError;' + sLineBreak +
    '  result  : T;' + sLineBreak +
    '}' + sLineBreak +
    '' + sLineBreak +
    'function toJsonRpcError(e : Error) : JsonRpcError {' + sLineBreak +
    '  return {name: e.name, message: (e.message+(e.stack? ''\n''+e.stack:"")), code: 0};' + sLineBreak +
    '}' + sLineBreak +
    '' + sLineBreak +
    'type JsonRpcProxyCallback<T> = (e? : JsonRpcError, r? : T) => void;' + sLineBreak +
    '' + sLineBreak +
    'export class JsonRpcProxyBase {' + sLineBreak +
    '  serviceUrl : string;' + sLineBreak +
    '  transporter : DataTransporter;' + sLineBreak +
    '' + sLineBreak +
    '  constructor(aServiceUrl : string, aTransporter : DataTransporter){' + sLineBreak +
    '    this.serviceUrl = aServiceUrl;' + sLineBreak +
    '    this.transporter = aTransporter;' + sLineBreak +
    '  }' + sLineBreak +
    '' + sLineBreak +
    '  executeRPC<T>(' + sLineBreak +
    '    aMethod   : string,' + sLineBreak +
    '    aParams   : ReadOnlyUnknownArray,' + sLineBreak +
    '    aCallback : JsonRpcProxyCallback<T>' + sLineBreak +
    '  ) : void' + sLineBreak +
    '  {' + sLineBreak +
    '    const requestBuffer = buildCallData(aMethod,aParams);' + sLineBreak +
    '    this.transporter.execute(this.serviceUrl, requestBuffer, (e,r) => {' + sLineBreak +
    '      if(e){' + sLineBreak +
    '        aCallback(toJsonRpcError(e));' + sLineBreak +
    '        return;' + sLineBreak +
    '      }' + sLineBreak +
    '      if(!r){' + sLineBreak +
    '        aCallback(toJsonRpcError(new Error("Empty response from network.")));' + sLineBreak +
    '        return;' + sLineBreak +
    '      }' + sLineBreak +
    '      let rt = JSON.parse(r!) as JsonRpcEnv<T>;' + sLineBreak +
    '      if(rt?.error?.message || rt?.error?.code){' + sLineBreak +
    '        aCallback(rt.error);' + sLineBreak +
    '        return;' + sLineBreak +
    '      }' + sLineBreak +
    '      aCallback(undefined,rt.result)' + sLineBreak +
    '    });' + sLineBreak +
    '  }' + sLineBreak +
    '}' + sLineBreak +
    '' + sLineBreak +
    'export class XhrDataTransporter implements DataTransporter {' + sLineBreak +
    '  execute(' + sLineBreak +
    '    aUrl  : string,' + sLineBreak +
    '    aData : string,' + sLineBreak +
    '    aCallback : (e? : Error, result? : string) => void' + sLineBreak +
    '  ) : void' + sLineBreak +
    '  {' + sLineBreak +
    '    let transport = new XMLHttpRequest();' + sLineBreak +
    '    transport.onreadystatechange = () => {' + sLineBreak +
    '      if(transport.readyState === XMLHttpRequest.DONE){' + sLineBreak +
    '        const status = transport.status;' + sLineBreak +
    '        if( (status === 0) || (status >= 200 && status <= 400) ){' + sLineBreak +
    '          aCallback(undefined,transport.responseText);' + sLineBreak +
    '        } else {' + sLineBreak +
    '          let e = new Error(transport.statusText);' + sLineBreak +
    '          e.name = status.toString();' + sLineBreak +
    '          aCallback(e)' + sLineBreak +
    '        }' + sLineBreak +
    '      };' + sLineBreak +
    '    }' + sLineBreak +
    '    transport.open("post",aUrl,true);' + sLineBreak +  
    '    transport.setRequestHeader(''Content-Type'', ''application/json'');' + sLineBreak +
    '    transport.send(aData);' + sLineBreak +
    '  }' + sLineBreak +
    '}';

{ TInftGenerator }

function TInftGenerator.GenerateIntfName(AIntf : TPasElement) : string;
begin
  Result := AIntf.Name;
end;

function TInftGenerator.GenerateTypeText(AType : TPasType; const AItem : TSimpleTypeItem) : string;
var
  t : TPasType;
begin
  t := GetUltimeType(AType);
  if not t.InheritsFrom(TPasArrayType) then begin
    if (AItem = TSimpleTypeItem.Extendable) and
       t.InheritsFrom(TPasNativeSimpleType) and
       (TPasNativeSimpleType(t).ExtendableType <> nil)
    then begin
      t := TPasNativeSimpleType(t).ExtendableType;
    end;
    Result := SymbolTable.GetExternalName(t);
  end else begin
    t := GetUltimeType(TPasArrayType(t).ElType);
    if t.InheritsFrom(TPasNativeSimpleType) and
       Assigned(TPasNativeSimpleType(t).ExtendableType)
    then begin
      t := TPasNativeSimpleType(t).ExtendableType;
    end;
    Result := Format('Array<%s>',[SymbolTable.GetExternalName(t)]);
  end;
end;

procedure TInftGenerator.GenerateIntf(AIntf : TPasClassType);
var
  locName : string;

  procedure WriteMethod(AMthd : TPasProcedure);
  var
    prmCnt,k : Integer;
    prm : TPasArgument;
    prms : TList2;
    pt : TPasType;
    s : string;
  begin
    Indent();
    prms := AMthd.ProcType.Args;
    prmCnt := prms.Count; 
    Write('%s(',[SymbolTable.GetExternalName(AMthd)]);
       
    IncIndent();
    if ( prmCnt > 0 ) then begin
      for k := 0 to Pred(prmCnt) do begin
        prm := TPasArgument(prms[k]);
        NewLine();
        Indent();
        Write('%s : %s,',[prm.Name,GenerateTypeText(prm.ArgType)]);
      end;
    end;
    if AMthd.InheritsFrom(TPasFunction) then begin
      pt := TPasFunctionType(AMthd.ProcType).ResultEl.ResultType;
      s := GenerateTypeText(pt);
    end else begin
      s := 'void';
    end;
    NewLine();
    Indent();
    WriteLn('aCallback : JsonRpcProxyCallback<%s>',[s]);
    DecIndent();
    Indent();
    WriteLn(') : void;',[s]);
  end;

  procedure WriteMethods();
  var
    k, kc : Integer;
    mbrs : TList2;
    elt : TPasElement;
  begin
    IncIndent();
      mbrs := AIntf.Members;
      kc := 0;
      for k := 0 to Pred(mbrs.Count) do begin
        elt := TPasElement(mbrs[k]);
        if elt.InheritsFrom(TPasProcedure) then begin
          if (kc > 0) then
            NewLine();
          kc := kc+1;
          WriteMethod(TPasProcedure(elt));
        end;
      end;
    DecIndent();
  end;

begin
  NewLine();
  locName := GenerateIntfName(AIntf);
  WriteLn('export interface %s {',[locName]);
  IncIndent();
    WriteMethods();
  DecIndent();
  WriteLn('}');
end;

procedure TInftGenerator.GenerateIntfProxy(AIntf : TPasClassType);
var
  locProxyName : string;

  procedure WriteMethod(AMthd : TPasProcedure);
  var
    prmCnt,k : Integer;
    prm : TPasArgument;
    prms : TList2;
    pt : TPasType;
    callParams, s : string;
  begin
    Indent();
    prms := AMthd.ProcType.Args;
    prmCnt := prms.Count;
    Write('%s(',[SymbolTable.GetExternalName(AMthd)]);

    callParams := '';
    IncIndent();
    if ( prmCnt > 0 ) then begin
      for k := 0 to Pred(prmCnt) do begin
        prm := TPasArgument(prms[k]);
        NewLine();
        Indent();
        Write('%s : %s,',[prm.Name,GenerateTypeText(prm.ArgType)]);
        callParams := callParams + prm.Name + ',';
      end;
      Delete(callParams,Length(callParams),1);
    end;
    if AMthd.InheritsFrom(TPasFunction) then begin
      pt := TPasFunctionType(AMthd.ProcType).ResultEl.ResultType;
      s := GenerateTypeText(pt);
    end else begin
      s := 'void';
    end;
    NewLine();
    Indent();
    WriteLn('aCallback : JsonRpcProxyCallback<%s>',[s]);
    DecIndent();
    Indent();
    WriteLn(') : void {',[s]);
    IncIndent();
      Indent();
      WriteLn('this.executeRPC("%s",[%s],aCallback);',[SymbolTable.GetExternalName(AMthd),callParams]);
    DecIndent();
    Indent();
    WriteLn('}',[s]);
  end;

  procedure WriteMethods();
  var
    k : Integer;
    mbrs : TList2;
    elt : TPasElement;
  begin
    IncIndent();
      mbrs := AIntf.Members;
      for k := 0 to Pred(mbrs.Count) do begin
        elt := TPasElement(mbrs[k]);
        if elt.InheritsFrom(TPasProcedure) then begin
          NewLine();
          WriteMethod(TPasProcedure(elt));
        end;
      end;
    DecIndent();
  end;

begin
  locProxyName := ExtractserviceName(AIntf);
  locProxyName := Format('%sProxy',[locProxyName]);
  NewLine();
  WriteLn('export class %s extends JsonRpcProxyBase implements %s {',[locProxyName,AIntf.Name]);
  WriteMethods();
  WriteLn('}');
end;

procedure TInftGenerator.GenerateClass(ASymbol : TPasClassType);

  procedure WriteDec();
  var
    decBuffer, s : string;
    elt : TPasElement;
    trueAncestor : TPasType;
  begin
    s := '';
    if Assigned(ASymbol.AncestorType) then begin
      trueAncestor := ASymbol.AncestorType;
      if trueAncestor.InheritsFrom(TPasUnresolvedTypeRef) then begin
        elt := SymbolTable.FindElement(SymbolTable.GetExternalName(trueAncestor));
        if (elt = nil) or (not elt.InheritsFrom(TPasType)) then
          trueAncestor := nil
        else
          trueAncestor := TPasType(elt);
      end;
      if (trueAncestor <> nil) then begin
        trueAncestor := GetUltimeType(trueAncestor);
        if trueAncestor.InheritsFrom(TPasNativeSimpleType) and
           Assigned(TPasNativeSimpleType(trueAncestor).ExtendableType)
        then begin
          trueAncestor := TPasNativeSimpleType(trueAncestor).ExtendableType;
        end;
        if not(trueAncestor.InheritsFrom(TPasNativeClassType)) or (trueAncestor.Name <> 'Object') then
          s := Format('%s',[trueAncestor.Name]);
      end;
    end;
    if IsStrEmpty(s) then
      decBuffer := ''
    else
      decBuffer := Format(' extends %s',[s]);
    Indent();
    Write('export class %s%s {',[SymbolTable.GetExternalName(ASymbol),decBuffer]);
  end;

  procedure WriteProperty(AProp : TPasProperty; AActualPropType : TPasType);
  var
    locLine, locTypeStr, ks : string;
    locType : TPasType;
    locIsArray, locOptional : Boolean;
  begin
    locType := GetUltimeType(AActualPropType);
    locTypeStr := GenerateTypeText(locType);
    locIsArray := SymbolTable.IsOfType(locType,TPasArrayType); 
    if locIsArray then begin
      locLine := Format('%s : %s = [];',[SymbolTable.GetExternalName(AProp),locTypeStr]);
    end else begin
      locLine := SymbolTable.GetExternalName(AProp);
      locOptional := AnsiSameText(sWST_PROP_STORE_PREFIX,Copy(AProp.StoredAccessorName,1,Length(sWST_PROP_STORE_PREFIX)));
      if locOptional then
        locLine := locLine + '?';
      locLine := locLine + ' : ' + locTypeStr;
      if not locOptional then begin
        if locType.InheritsFrom(TPasClassType) then
          locLine := Format('%s = new %s()',[locLine,locTypeStr])
        else if locType.InheritsFrom(TPasEnumType) then begin
          if (TPasEnumType(locType).Values.Count > 0) then
            ks := TPasEnumValue(TPasEnumType(locType).Values[0]).Name
          else
            ks := 'This Enum does not contain elements.';
          locLine := Format('%s = %s.%s',[locLine,locTypeStr,ks]);
        end else begin
          locTypeStr := LowerCase(locTypeStr);
          if (locTypeStr = 'boolean') then
            locLine := Format('%s = false',[locLine])
          else if (locTypeStr = 'number') then
            locLine := Format('%s = 0',[locLine])
          else if (locTypeStr = 'string') then
            locLine := Format('%s = ""',[locLine])
        end;
      end;  
      locLine := locLine + ';'
    end;
    Indent(); WriteLn(locLine);
  end;

  procedure WriteProperties();
  var
    k : Integer;
    p : TPasProperty;
    elt : TPasElement;
  begin
    if (ASymbol.Members.Count > 0) then
      WriteLn('');
    Indent();
    IncIndent();
    for k := 0 to Pred(ASymbol.Members.Count) do begin
      elt := TPasElement(ASymbol.Members[k]);
      if elt.InheritsFrom(TPasProperty) then begin
        p := TPasProperty(elt);
        WriteProperty(p,FindActualType(p.VarType,SymbolTable));
      end;
    end;
    DecIndent();
  end;

begin
  try
    NewLine();
    WriteDec();
    WriteProperties();
    WriteLn('}');
  except
    on e : Exception do begin
      GetLogger.Log(mtError,'TInftGenerator.GenerateClass()=',[ASymbol.Name, ' ;; ', e.Message]);
      raise;
    end;
  end;
end;

procedure TInftGenerator.GenerateEnum(ASymbol : TPasEnumType);
var
  i : Integer;
  itm : TPasEnumValue;
  extName, s : string;
begin
  NewLine();
  extName := SymbolTable.GetExternalName(ASymbol);
  WriteLn('export enum %s {',[extName]);
  IncIndent();
    for i := 0 to ASymbol.Values.Count-2 do begin
      itm := TPasEnumValue(ASymbol.Values[i]);
      s := SymbolTable.GetExternalName(itm);  
      Indent();
      WriteLn('%s = "%s",',[s,s])
    end;
    i := ASymbol.Values.Count-1;
    itm := TPasEnumValue(ASymbol.Values[i]);
    s := SymbolTable.GetExternalName(itm);
    Indent();
    WriteLn('%s = "%s"',[s,s]);
  DecIndent();
  WriteLn('}');  ///const SexeOrdinals : Sexe[] = [Sexe.Inconnue, Sexe.Femme, Sexe.Homme];

  WriteLn('export const %s_VALUES : %s[] = [',[extName,extName]);
  IncIndent();
    for i := 0 to ASymbol.Values.Count-2 do begin
      itm := TPasEnumValue(ASymbol.Values[i]);
      s := SymbolTable.GetExternalName(itm);
      Indent();
      WriteLn('%s.%s,',[extName,s]);
    end;
    i := ASymbol.Values.Count-1;
    itm := TPasEnumValue(ASymbol.Values[i]);
    s := SymbolTable.GetExternalName(itm);
    Indent();
    WriteLn('%s.%s,',[extName,s]);
  DecIndent();
  WriteLn(']');
end;

function TInftGenerator.GetDestUnitName(): string;
begin
  Result := SymbolTable.CurrentModule.Name;
end;

procedure TInftGenerator.GenerateEnums();
var
  typeList : TList2;
  i, c : Integer;
  elt : TPasElement;
begin
  typeList := SymbolTable.CurrentModule.InterfaceSection.Declarations;
  c := typeList.Count;
  for i := 0 to c-1 do begin
    elt := TPasElement(typeList[i]);
    if elt.InheritsFrom(TPasEnumType) then
      GenerateEnum(TPasEnumType(elt));
  end;
end;

procedure TInftGenerator.PrepareModule();
var
  s : string;
begin
  s := GetDestUnitName() + '.ts';
  FStream := SrcMngr.CreateItem(s);
  SetCurrentStream(FStream);   
  WriteLn('/*');
  WriteLn('This file has been produced by ws_helper.');
  WriteLn('  Input unit name : "%s".',[SymbolTable.CurrentModule.Name]);
  WriteLn('  This unit name  : "%s".',[s]);
  WriteLn('  Date            : "%s".',[DateTimeToStr(Now())]);
  WriteLn('*/');
end;

procedure TInftGenerator.InternalExecute();
var
  i, c, j, k : Integer;
  clssTyp : TPasClassType;
  gnrClssLst : TObjectList;
  objLst : TObjectList;
  typeList : TList2;
  elt : TPasElement;
  classAncestor : TPasElement;
begin
  GenerateEnums();

  objLst := nil;
  gnrClssLst := TObjectList.Create(False);
  try
    typeList := SymbolTable.CurrentModule.InterfaceSection.Declarations;
    c := Pred(typeList.Count);

    objLst := TObjectList.Create();
    objLst.OwnsObjects := False;
    for i := 0 to c do begin
      elt := TPasElement(typeList[i]);
      if elt.InheritsFrom(TPasClassType) and ( TPasClassType(elt).ObjKind = okClass ) then begin
        clssTyp := TPasClassType(elt);
        if ( gnrClssLst.IndexOf(clssTyp) = -1 ) then begin
          objLst.Clear();
          while Assigned(clssTyp) and ( objLst.IndexOf(clssTyp) = -1 ) do begin
            objLst.Add(clssTyp);
            classAncestor := clssTyp.AncestorType;
            if Assigned(classAncestor) and classAncestor.InheritsFrom(TPasUnresolvedTypeRef) then begin
              classAncestor := SymbolTable.FindElement(SymbolTable.GetExternalName(classAncestor));
            end;
            if Assigned(classAncestor) and classAncestor.InheritsFrom(TPasClassType) then begin
              clssTyp := classAncestor as TPasClassType;
            end else begin
              clssTyp := nil;
            end;
          end;

          k := Pred(objLst.Count);
          for j := 0 to k do begin
            clssTyp := objLst[k-j] as TPasClassType;
            if (gnrClssLst.IndexOf(clssTyp) = -1) then begin
              if (SymbolTable.CurrentModule.InterfaceSection.Declarations.IndexOf(clssTyp) <> -1) then begin
                GenerateClass(clssTyp);
                gnrClssLst.Add(clssTyp);
              end;
            end;
          end;
        end;
      end;
    end;

    if (TTypeScriptOption.GenerateProxy in Self.OptionsEx) then begin
      NewLine();
      WriteLn(TS_PROXY_BASE_CODE);
    end;
    for i := 0 to c do begin
      elt := TPasElement(typeList[i]);
      if elt.InheritsFrom(TPasClassType) and ( TPasClassType(elt).ObjKind = okInterface ) then begin
        GenerateIntf(TPasClassType(elt));
        if (TTypeScriptOption.GenerateProxy in Self.OptionsEx) then
          GenerateIntfProxy(TPasClassType(elt));
      end;
    end;
  finally
    FreeAndNil(objLst);
    FreeAndNil(gnrClssLst);
  end;
end;

procedure TInftGenerator.Execute();
var
  oldCurrent, mdl : TPasModule;
  i : Integer;
  mdlList : TList2;
  oldCS : Boolean;
  oldNamesKinds : TElementNameKinds;
begin
  oldCS := SymbolTable.CaseSensitive;
  oldNamesKinds := SymbolTable.DefaultSearchNameKinds;
  oldCurrent := SymbolTable.CurrentModule;
  try
    SymbolTable.CaseSensitive := True;
    SymbolTable.DefaultSearchNameKinds := [elkDeclaredName];
    mdlList := SymbolTable.Package.Modules;
    for i := 0 to Pred(mdlList.Count) do begin
      mdl := TPasModule(mdlList[i]);
      if not mdl.InheritsFrom(TPasNativeModule) then begin
        SymbolTable.SetCurrentModule(mdl);
        PrepareModule();
        InternalExecute();
      end;
    end;
  finally
    SymbolTable.SetCurrentModule(oldCurrent);
    SymbolTable.CaseSensitive := oldCS;
    SymbolTable.DefaultSearchNameKinds := oldNamesKinds;
  end;
end;

end.

