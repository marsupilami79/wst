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
unit xsd_generator;

interface

uses
  Classes, SysUtils, TypInfo,
  {$IFNDEF FPC}xmldom, wst_delphi_xml{$ELSE}DOM, wst_fpc_xml{$ENDIF},
  pastree, pascal_parser_intf;
  
type

  TGeneratorOption = ( xgoIgnorembeddedArray );
  TGeneratorOptions = set of TGeneratorOption;

  EXsdGeneratorException = class(Exception) end;
  TBaseTypeHandler = class;
  TBaseTypeHandlerClass = class of TBaseTypeHandler;

  IGenerator = interface
    ['{F69523B3-A6FF-4BFB-9ACB-D4B9F32DBCA9}']
    procedure Execute(
      ASymTable   : TwstPasTreeContainer;
      AModuleName : string
    );
  end;

  IXsdGenerator = interface(IGenerator)
    ['{FBFF92BC-B72B-4B85-8D16-379F9E548DDB}']
    function GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;
    procedure SetPreferedShortNames(const ALongName, AShortName : string);
    function GetPreferedShortNames() : TStrings;
  end;
  
  IXsdTypeHandler = interface
    ['{541EA377-4F70-49B1-AFB4-FC62B24F567B}']
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument : TDOMDocument
    );
    function GetOwner() : IXsdGenerator;
  end;

  IXsdSpecialTypeHelper = interface
    ['{1F4115E8-2B82-4E63-844B-36EB5911172F}']
    procedure HandleTypeUsage(
      ATargetNode,
      ASchemaNode  : TDOMElement
    );
  end;

  IXsdTypeHandlerRegistry = interface
    ['{C5666646-3426-4696-93EE-AFA8EE7CAE53}']
    function Find(
          ASymbol  : TPasElement;
          Aowner   : IGenerator;
      out AHandler : IXsdTypeHandler
    ) : Boolean;
    function FindHelper(
          ASymbol  : TPasElement;
      out AHelper  : IXsdSpecialTypeHelper
    ) : Boolean;
    procedure Register(AFactory : TBaseTypeHandlerClass);
  end;

  { TCustomXsdGenerator }

  TCustomXsdGenerator = class(
    TInterfacedObject,
    IInterface,
    IGenerator,
    IXsdGenerator
  )
  private
    FDocument : TDOMDocument;
    FOptions: TGeneratorOptions;
    FShortNames : TStrings;
  protected
    function GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;virtual;abstract;
    procedure SetPreferedShortNames(const ALongName, AShortName : string);
    function GetPreferedShortNames() : TStrings;
    procedure Execute(
      ASymTable   : TwstPasTreeContainer;
      AModuleName : string
    );

    procedure Prepare(
      ASymTable : TwstPasTreeContainer;
      AModule   : TPasModule
    );virtual;
    property Document : TDOMDocument read FDocument;
    property Options : TGeneratorOptions read FOptions;
  public
    constructor Create(const ADocument : TDOMDocument);overload;
    constructor Create(
      const ADocument : TDOMDocument;
      const AOptions : TGeneratorOptions
    );overload;
    destructor Destroy();override;
  end;

  { TXsdGenerator }

  TXsdGenerator = class(TCustomXsdGenerator)
  private
    FSchemaNode : TDOMElement;
  protected
    function GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;override;
    procedure Prepare(ASymTable : TwstPasTreeContainer; AModule : TPasModule);override;
  end;
  
  { TBaseTypeHandler }

  TBaseTypeHandler = class(TInterfacedObject,IXsdTypeHandler)
  private
    FOwner : Pointer;
    FRegistry : IXsdTypeHandlerRegistry;
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument     : TDOMDocument
    );virtual;abstract;
    function GetOwner() : IXsdGenerator;
    class function CanHandle(ASymbol : TObject) : Boolean;virtual;abstract;
    function GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;
    procedure DeclareNameSpaceOf_WST(ADocument : TDOMDocument);
    procedure DeclareAttributeOf_WST(AElement : TDOMElement; const AAttName, AAttValue : DOMString);
    function GetRegistry() : IXsdTypeHandlerRegistry;{$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    constructor Create(
      AOwner : IGenerator;
      ARegistry : IXsdTypeHandlerRegistry
    );virtual;
  end;

  function GetNameSpaceShortName(
    const ANameSpace    : string;
          ADocument : TDOMDocument;
    const APreferedList : TStrings
  ):string;

  function GetXsdTypeHandlerRegistry():IXsdTypeHandlerRegistry;
  function CreateElement(const ANodeName : DOMString; AParent : TDOMNode; ADoc : TDOMDocument):TDOMElement;{$IFDEF USE_INLINE}inline;{$ENDIF}

implementation
uses
  xsd_consts, Contnrs, StrUtils, wst_types, parserutils;

type

  { TAbstractSpecialTypeHelper }

  TAbstractSpecialTypeHelper = class(TInterfacedObject,IXsdSpecialTypeHelper)
  protected
    procedure HandleTypeUsage(
      ATargetNode,
      ASchemaNode  : TDOMElement
    );virtual;abstract;
  public
    constructor Create();virtual;
  end;

  TAbstractSpecialTypeHelperClass = class of TAbstractSpecialTypeHelper;

  { TWideStringHelper }

  TWideStringHelper = class(TAbstractSpecialTypeHelper,IXsdSpecialTypeHelper)
  protected
    procedure HandleTypeUsage(
      ATargetNode,
      ASchemaNode  : TDOMElement
    );override;
  end;

  TAnsiCharHelper = class(TAbstractSpecialTypeHelper,IXsdSpecialTypeHelper)
  protected
    procedure HandleTypeUsage(
      ATargetNode,
      ASchemaNode  : TDOMElement
    );override;
  end;

  TWideCharHelper = class(TAbstractSpecialTypeHelper,IXsdSpecialTypeHelper)
  protected
    procedure HandleTypeUsage(
      ATargetNode,
      ASchemaNode  : TDOMElement
    );override;
  end;

{$IFDEF WST_UNICODESTRING}
  { TUnicodeStringHelper }

  TUnicodeStringHelper = class(TAbstractSpecialTypeHelper,IXsdSpecialTypeHelper)
  protected
    procedure HandleTypeUsage(
      ATargetNode,
      ASchemaNode  : TDOMElement
    );override;
  end;
{$ENDIF WST_UNICODESTRING}

  { TXsdTypeHandlerRegistry }

  TXsdTypeHandlerRegistry = class(TInterfacedObject,IInterface,IXsdTypeHandlerRegistry)
  private
    FList : TClassList;
  private
    function FindIndexOfHandler(ASymbol : TPasElement) : Integer;
  protected
    function Find(
          ASymbol  : TPasElement;
          Aowner   : IGenerator;
      out AHandler : IXsdTypeHandler
    ) : Boolean;
    function FindHelper(
          ASymbol  : TPasElement;
      out AHelper  : IXsdSpecialTypeHelper
    ) : Boolean;
    procedure Register(AFactory : TBaseTypeHandlerClass);
  public
    constructor Create();
    destructor Destroy();override;
  end;
  
  { TTypeDefinition_TypeHandler }

  TTypeDefinition_TypeHandler = class(TBaseTypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument     : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
{$IFDEF WST_HANDLE_DOC}
    procedure GenerateDocumentation(
            AContainerNode : TDOMElement;
      const ADocString : string;
            ADocument  : TDOMDocument
    );
{$ENDIF WST_HANDLE_DOC}
  end;
  
  { TTypeAliasDefinition_TypeHandler }

  TTypeAliasDefinition_TypeHandler = class(TTypeDefinition_TypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
  end;
  
  { TEnumTypeHandler }

  TEnumTypeHandler = class(TTypeDefinition_TypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument     : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
  end;
  
  { TClassTypeDefinition_TypeHandler }

  TClassTypeDefinition_TypeHandler = class(TTypeDefinition_TypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument     : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
  end;

  { TPasRecordType_TypeHandler }

  TPasRecordType_TypeHandler = class(TTypeDefinition_TypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument     : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
  end;
  
  { TBaseArrayRemotable_TypeHandler }

  TBaseArrayRemotable_TypeHandler = class(TTypeDefinition_TypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            ADocument     : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
  end;

{ TAbstractSpecialTypeHelper }

constructor TAbstractSpecialTypeHelper.Create();
begin
  inherited Create();
end;


function GetTypeNameSpace(
  AContainer : TwstPasTreeContainer;
  AType      : TPasElement
) : string;
var
  locElt : TPasElement;
begin
  Result := '';
  locElt := AType;
  if ( locElt <> nil ) then begin
    if locElt.InheritsFrom(TPasUnresolvedTypeRef) then
      locElt := AContainer.FindElement(AContainer.GetExternalName(locElt));
    if ( locElt <> nil ) and
       ( not locElt.InheritsFrom(TPasUnresolvedTypeRef) ) and
       //locElt.InheritsFrom(TPasType) and
       ( locElt.Parent <> nil ) and
       ( locElt.Parent.Parent <> nil )
    then begin
      Result := AContainer.GetExternalName(locElt.Parent.Parent);
    end;
  end;
  Result := Trim(Result);
  if ( Length(Result) = 0 ) then
    Result := AContainer.GetExternalName(AContainer.CurrentModule);
end;

function FindAttributeByValueInNode(
  const AAttValue        : string;
  const ANode            : TDOMNode;
  out   AResAtt          : string;
  const AStartIndex      : Integer = 0;
  const AStartingWith    : string = ''
):boolean;
var
  i,c : Integer;
  b : Boolean;
begin
  AResAtt := '';
  if Assigned(ANode) and Assigned(ANode.Attributes) then begin
    b := ( Length(AStartingWith) = 0);
    c := Pred(ANode.Attributes.Length);
//    if ( AStartIndex >= 0 ) then
  //    i := AStartIndex;
    for i := AStartIndex to c do begin
      if AnsiSameText(AAttValue,ANode.Attributes.Item[i].NodeValue) and
         ( b or ( Pos(AStartingWith,ANode.Attributes.Item[i].NodeName) = 1 ))
      then begin
        AResAtt := ANode.Attributes.Item[i].NodeName;
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

function GetNameSpaceShortName(
  const ANameSpace    : string;
        ADocument : TDOMDocument;
  const APreferedList : TStrings
):string;
begin
  if FindAttributeByValueInNode(ANameSpace,ADocument.DocumentElement,Result,0, s_xmlns) then begin
    Result := Copy(Result,Length(s_xmlns+':')+1,MaxInt);
  end else begin
    if ( APreferedList <> nil ) then
      Result := Trim(APreferedList.Values[ANameSpace]);
    if ( Result = '' ) then
      Result := Format('ns%d',[GetNodeListCount(ADocument.DocumentElement.Attributes)]) ;
    ADocument.DocumentElement.SetAttribute(Format('%s:%s',[s_xmlns,Result]),ANameSpace);
  end;
end;

function CreateElement(const ANodeName : DOMString; AParent : TDOMNode; ADoc : TDOMDocument):TDOMElement;//inline;
begin
  Result := ADoc.CreateElement(ANodeName);
  AParent.AppendChild(Result);
end;

{ TWideStringHelper }

procedure TWideStringHelper.HandleTypeUsage(
  ATargetNode,
  ASchemaNode  : TDOMElement
);
var
  strBuffer : string;
begin
  if not FindAttributeByValueInNode(s_WST_base_namespace,ASchemaNode,strBuffer) then
    ASchemaNode.SetAttribute(Format('%s:%s',[s_xmlns,s_WST]),s_WST_base_namespace);
  ATargetNode.SetAttribute(Format('%s:%s',[s_WST,s_WST_typeHint]),'WideString');
end;

{ TAnsiCharHelper }

procedure TAnsiCharHelper.HandleTypeUsage(ATargetNode, ASchemaNode: TDOMElement);
var
  strBuffer : string;
begin
  if not FindAttributeByValueInNode(s_WST_base_namespace,ASchemaNode,strBuffer) then
    ASchemaNode.SetAttribute(Format('%s:%s',[s_xmlns,s_WST]),s_WST_base_namespace);
  ATargetNode.SetAttribute(Format('%s:%s',[s_WST,s_WST_typeHint]),'AnsiChar');
end;

{ TWideCharHelper }

procedure TWideCharHelper.HandleTypeUsage(ATargetNode, ASchemaNode: TDOMElement);
var
  strBuffer : string;
begin
  if not FindAttributeByValueInNode(s_WST_base_namespace,ASchemaNode,strBuffer) then
    ASchemaNode.SetAttribute(Format('%s:%s',[s_xmlns,s_WST]),s_WST_base_namespace);
  ATargetNode.SetAttribute(Format('%s:%s',[s_WST,s_WST_typeHint]),'WideChar');
end;

{$IFDEF WST_UNICODESTRING}
{ TUnicodeStringHelper }

procedure TUnicodeStringHelper.HandleTypeUsage(
  ATargetNode,
  ASchemaNode  : TDOMElement
);
var
  strBuffer : string;
begin
  if not FindAttributeByValueInNode(s_WST_base_namespace,ASchemaNode,strBuffer) then
    ASchemaNode.SetAttribute(Format('%s:%s',[s_xmlns,s_WST]),s_WST_base_namespace);
  ATargetNode.SetAttribute(Format('%s:%s',[s_WST,s_WST_typeHint]),'UnicodeString');
end;
{$ENDIF WST_UNICODESTRING}

var
  XsdTypeHandlerRegistryInst : IXsdTypeHandlerRegistry = nil;
function GetXsdTypeHandlerRegistry():IXsdTypeHandlerRegistry;
begin
  Result := XsdTypeHandlerRegistryInst;
end;

procedure RegisterFondamentalTypes();
var
  r : IXsdTypeHandlerRegistry;
begin
  r := GetXsdTypeHandlerRegistry();
  r.Register(TEnumTypeHandler);
  r.Register(TClassTypeDefinition_TypeHandler);
  r.Register(TPasRecordType_TypeHandler);
  r.Register(TBaseArrayRemotable_TypeHandler);
  r.Register(TTypeAliasDefinition_TypeHandler);
end;



{ TWsdlTypeHandlerRegistry }

function TXsdTypeHandlerRegistry.FindIndexOfHandler(ASymbol: TPasElement): Integer;
Var
  i, c : Integer;
begin
  Result := -1;
  c := FList.Count;
  for i := 0 to Pred(c) do begin
    if TBaseTypeHandlerClass(FList[i]).CanHandle(ASymbol) then begin
      Result := i;
      Break;
    end;
  end;
end;

function TXsdTypeHandlerRegistry.Find(
          ASymbol  : TPasElement;
          Aowner   : IGenerator;
      out AHandler : IXsdTypeHandler
) : Boolean;
var
  fct : TBaseTypeHandlerClass;
  i : Integer;
begin
  i := FindIndexOfHandler(ASymbol);
  Result := ( i >= 0 );
  if Result then begin
    fct := TBaseTypeHandlerClass(FList[i]);
    AHandler := fct.Create(Aowner,Self) as IXsdTypeHandler;
  end;
end;

type
  TSpecialTypeHelperRecord = record
    Name : string;
    HelperClass : TAbstractSpecialTypeHelperClass;
  end;
function TXsdTypeHandlerRegistry.FindHelper(
      ASymbol : TPasElement;
  out AHelper: IXsdSpecialTypeHelper
) : Boolean;
const
   HELPER_COUNT = 3 {$IFDEF WST_UNICODESTRING} + 1 {$ENDIF WST_UNICODESTRING};
   HELPER_MAP : array[0..Pred(HELPER_COUNT)] of TSpecialTypeHelperRecord = (
     ( Name : 'widestring'; HelperClass : TWideStringHelper;),
     ( Name : 'ansichar'; HelperClass : TAnsiCharHelper;),
     ( Name : 'widechar'; HelperClass : TWideCharHelper;)
{$IFDEF WST_UNICODESTRING}
    ,( Name : 'unicodestring'; HelperClass : TUnicodeStringHelper;)
{$ENDIF WST_UNICODESTRING}
   );
var
  i : PtrInt;
  locName : string;
begin
  AHelper := nil;
  if ( ASymbol <> nil ) and ASymbol.InheritsFrom(TPasNativeSpecialSimpleType) then begin
    locName := LowerCase(ASymbol.Name);
    for i := Low(HELPER_MAP) to High(HELPER_MAP) do begin
      if ( locName = HELPER_MAP[i].Name ) then begin
        AHelper := HELPER_MAP[i].HelperClass.Create() as IXsdSpecialTypeHelper;
        Break;
      end;
    end;
  end;
  Result := ( AHelper <> nil );
end;

procedure TXsdTypeHandlerRegistry.Register(AFactory: TBaseTypeHandlerClass);
begin
  if ( FList.IndexOf(AFactory) = -1 ) then begin
    FList.Add(AFactory);
  end;
end;

constructor TXsdTypeHandlerRegistry.Create();
begin
  FList := TClassList.Create();
end;

destructor TXsdTypeHandlerRegistry.Destroy();
begin
  FreeAndNil(FList);
  inherited Destroy();
end;

{ TBaseTypeHandler }

function TBaseTypeHandler.GetOwner(): IXsdGenerator;
begin
  Result := IXsdGenerator(FOwner);
end;

function TBaseTypeHandler.GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;
begin
  Result := GetOwner().GetSchemaNode(ADocument);
end;

procedure TBaseTypeHandler.DeclareNameSpaceOf_WST(ADocument : TDOMDocument);
var
  defSchemaNode : TDOMElement;
  strBuffer : string;
begin
  defSchemaNode := GetSchemaNode(ADocument) as TDOMElement;
  if not FindAttributeByValueInNode(s_WST_base_namespace,defSchemaNode,strBuffer) then
    defSchemaNode.SetAttribute(Format('%s:%s',[s_xmlns,s_WST]),s_WST_base_namespace);
end;

procedure TBaseTypeHandler.DeclareAttributeOf_WST(
        AElement : TDOMElement;
  const AAttName, AAttValue : DOMString
);
begin
  AElement.SetAttribute(Format('%s:%s',[s_WST,AAttName]),AAttvalue);
end;

function TBaseTypeHandler.GetRegistry(): IXsdTypeHandlerRegistry;
begin
  Result := FRegistry;
end;

constructor TBaseTypeHandler.Create(
  AOwner: IGenerator;
  ARegistry : IXsdTypeHandlerRegistry
);
begin
  Assert(Assigned(AOwner));
  FOwner := Pointer(AOwner);
  FRegistry := ARegistry;
end;

{ TTypeDefinition_TypeHandler }

procedure TTypeDefinition_TypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol: TPasElement;
        ADocument: TDOMDocument
);
begin
  Assert(ASymbol.InheritsFrom(TPasType));
end;

class function TTypeDefinition_TypeHandler.CanHandle(ASymbol: TObject): Boolean;
begin
  Result := Assigned(ASymbol) and ASymbol.InheritsFrom(TPasType);
end;

{$IFDEF WST_HANDLE_DOC}
procedure TTypeDefinition_TypeHandler.GenerateDocumentation(
        AContainerNode : TDOMElement;
  const ADocString : string;
        ADocument  : TDOMDocument
);
var
  tmpNode : TDOMElement;
begin
  if ( Length(Trim(ADocString)) > 0 ) then begin
    tmpNode := CreateElement(Format('%s:%s',[s_xs_short,s_annotation]),AContainerNode,ADocument);
    tmpNode := CreateElement(Format('%s:%s',[s_xs_short,s_documentation]),tmpNode,ADocument);
    tmpNode.AppendChild(ADocument.CreateTextNode(ADocString));
  end;
end;
{$ENDIF WST_HANDLE_DOC}

{ TTypeAliasDefinition_TypeHandler }

procedure TTypeAliasDefinition_TypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol: TPasElement;
        ADocument: TDOMDocument
);
var
  typItm : TPasAliasType;
  ns_shortName, s : string;
  defSchemaNode, resNode : TDOMElement;
  unitExternalName, baseUnitExternalName : string;
  trueDestType : TPasType;
{$IFDEF WST_HANDLE_DOC}
  i : PtrInt;
  ls : TStrings;
{$ENDIF WST_HANDLE_DOC}
begin
  inherited;
  typItm := ASymbol as TPasAliasType;
  if Assigned(typItm) then begin
    unitExternalName := GetTypeNameSpace(AContainer,ASymbol);
    ns_shortName := GetNameSpaceShortName(unitExternalName,ADocument,GetOwner().GetPreferedShortNames());
    defSchemaNode := GetSchemaNode(ADocument) as TDOMElement;

    s := Format('%s:%s',[s_xs_short,s_element]);
    resNode := CreateElement(s,defSchemaNode,ADocument);
    resNode.SetAttribute(s_name, AContainer.GetExternalName(typItm)) ;

{$IFDEF WST_HANDLE_DOC}
    ls := AContainer.Properties.FindList(typItm);
    if ( ls <> nil ) then begin
      i := ls.IndexOfName(s_documentation);
      if ( i >= 0 ) then
        GenerateDocumentation(resNode,DecodeLineBreak(ls.ValueFromIndex[i]),ADocument);
    end;
{$ENDIF WST_HANDLE_DOC}

    trueDestType := typItm.DestType;
    if trueDestType.InheritsFrom(TPasUnresolvedTypeRef) then
      trueDestType := AContainer.FindElement(AContainer.GetExternalName(typItm.DestType)) as TPasType;
    baseUnitExternalName := GetTypeNameSpace(AContainer,trueDestType);
    s := GetNameSpaceShortName(baseUnitExternalName,ADocument,GetOwner().GetPreferedShortNames());
    s := Format('%s:%s',[s,AContainer.GetExternalName(trueDestType)]);
    resNode.SetAttribute(s_type,s) ;
  end;
end;

class function TTypeAliasDefinition_TypeHandler.CanHandle(ASymbol: TObject): Boolean;
begin
  Result := Assigned(ASymbol) and ASymbol.InheritsFrom(TPasAliasType);
end;

{ TEnumTypeHandler }

procedure TEnumTypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol : TPasElement;
        ADocument : TDOMDocument
);
var
  typItm : TPasEnumType;
  ns_shortName, s : string;
  defSchemaNode, resNode, restrictNode : TDOMElement;
  i, c : Integer;
  unitExternalName : string;
{$IFDEF WST_HANDLE_DOC}
  ls : TStrings;
{$ENDIF WST_HANDLE_DOC}
begin
  typItm := ASymbol as TPasEnumType;
  if Assigned(typItm) then begin
    unitExternalName := GetTypeNameSpace(AContainer,ASymbol);
    if FindAttributeByValueInNode(unitExternalName,ADocument.DocumentElement,ns_shortName) then begin
      ns_shortName := Copy(ns_shortName,Length(s_xmlns+':')+1,MaxInt);
    end else begin
      ns_shortName := Format('ns%d',[GetNodeListCount(ADocument.DocumentElement.Attributes)]) ;
      ADocument.DocumentElement.SetAttribute(Format('%s:%s',[s_xmlns,ns_shortName]),unitExternalName);
    end;
    defSchemaNode := GetSchemaNode(ADocument) as TDOMElement;

    s := Format('%s:%s',[s_xs_short,s_simpleType]);
    resNode := CreateElement(s,defSchemaNode,ADocument);
    resNode.SetAttribute(s_name, AContainer.GetExternalName(typItm)) ;
{$IFDEF WST_HANDLE_DOC}
    ls := AContainer.Properties.FindList(typItm);
    if ( ls <> nil ) then begin
      i := ls.IndexOfName(s_documentation);
      if ( i >= 0 ) then
        GenerateDocumentation(resNode,DecodeLineBreak(ls.ValueFromIndex[i]),ADocument);
    end;
{$ENDIF WST_HANDLE_DOC}

      s := Format('%s:%s',[s_xs_short,s_restriction]);
      restrictNode := CreateElement(s,resNode,ADocument);
      restrictNode.SetAttribute(s_base,Format('%s:%s',[s_xs_short,'string'])) ;
      c := typItm.Values.Count;
      for i := 0 to pred(c) do begin
        s := Format('%s:%s',[s_xs_short,s_enumeration]);
        CreateElement(s,restrictNode,ADocument).SetAttribute(
          s_value,
          AContainer.GetExternalName(TPasEnumValue(typItm.Values[i]))
        );
      end;
  end;
end;

class function TEnumTypeHandler.CanHandle(ASymbol: TObject): Boolean;
begin
  Result := inherited CanHandle(ASymbol) and ASymbol.InheritsFrom(TPasEnumType);
end;

{ TClassTypeDefinition_TypeHandler }
type TTypeCategory = ( tcComplexContent, tcSimpleContent );
procedure TClassTypeDefinition_TypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol       : TPasElement;
        ADocument     : TDOMDocument
);

  function TypeHasSequence(const AClassType : TPasClassType; const ACategory : TTypeCategory) : Boolean;
  var
    k : PtrInt;
    p : TPasProperty;
  begin
    Result := False;
    if ( AClassType.Members.Count > 0 ) then begin
      for k := 0 to Pred(AClassType.Members.Count) do begin
        if TPasElement(AClassType.Members[k]).InheritsFrom(TPasProperty) then begin
          p := TPasProperty(AClassType.Members[k]);
          if not AContainer.IsAttributeProperty(p) then begin
            if ( ACategory = tcSimpleContent ) then begin
              raise EXsdGeneratorException.CreateFmt('Invalid type definition, a simple type cannot have "not attribute" properties : "%s"',[AContainer.GetExternalName(AClassType)]);
            end;
            Result := True;
          end;
        end;
      end;
    end;
  end;

  procedure ProcessPropertyExtendedMetadata(const AProp : TPasProperty; const APropNode : TDOMElement);
  var
    ls : TStrings;
    line, ns, ns_short, localName, attName, attValue : string;
    k, q : PtrInt;
  begin
    ls := AContainer.Properties.GetList(AProp);
    if ( ls <> nil ) and ( ls.Count > 0 ) then begin
      for k := 0 to Pred(ls.Count) do begin
        line := ls.Names[k];
        q := Pos('#',line);
        if ( q > 0 ) then begin
          ns := Copy(line,1,Pred(q));
          localName := Copy(line,Succ(q),MaxInt);
          ns_short := GetNameSpaceShortName(ns,ADocument,GetOwner().GetPreferedShortNames());
          attName := Format('%s:%s',[ns_short,localName]);
          line := ls.Values[line];
          q := Pos('#',line);
          if ( q > 0 ) then begin
            ns := Copy(line,1,Pred(q));
            localName := Copy(line,Succ(q),MaxInt);
            ns_short := GetNameSpaceShortName(ns,ADocument,GetOwner().GetPreferedShortNames());
            attValue := Format('%s:%s',[ns_short,localName]);
          end else begin
            attValue := line;
          end;
          APropNode.SetAttribute(attName,attValue);
        end;
      end;
    end;
  end;

  procedure ProcessXsdAny(const AContentNode : TDOMElement; const AXsdInfo : string);
  var
    xsdAnyNode : TDOMElement;
    ss : string;
    locLS : TStringList;
  begin
    xsdAnyNode := CreateElement(Format('%s:%s',[s_xs_short,s_any]),AContentNode,ADocument);
    locLS := TStringList.Create();
    try
      locLS.Delimiter := ';';
      locLS.DelimitedText := AXsdInfo;
      ss := locLS.Values[s_processContents];
      if not IsStrEmpty(ss) then
        xsdAnyNode.SetAttribute(s_processContents,ss);
      ss := locLS.Values[s_minOccurs];
      if not IsStrEmpty(ss) then
        xsdAnyNode.SetAttribute(s_minOccurs,ss);
      ss := locLS.Values[s_maxOccurs];
      if not IsStrEmpty(ss) then
        xsdAnyNode.SetAttribute(s_maxOccurs,ss);
    finally
      locLS.Free();
    end;
  end;

  procedure ProcessXsdAnyAttribute(const AContentNode : TDOMElement; const AXsdInfo : string);
  var
    xsdAnyNode : TDOMElement;
    ss : string;
    locLS : TStringList;
  begin
    xsdAnyNode := CreateElement(Format('%s:%s',[s_xs_short,s_anyAttribute]),AContentNode,ADocument);
    locLS := TStringList.Create();
    try
      locLS.Delimiter := ';';
      locLS.DelimitedText := AXsdInfo;
      ss := locLS.Values[s_processContents];
      if not IsStrEmpty(ss) then
        xsdAnyNode.SetAttribute(s_processContents,ss);
    finally
      locLS.Free();
    end;
  end;

var
  cplxNode, sqcNode, derivationNode, defSchemaNode : TDOMElement;

  procedure ProcessProperty(const AProp : TPasProperty);
  var
    p : TPasProperty;
    s : string;
    propNode : TDOMElement;
    propTypItm, propItmUltimeType : TPasType;
    prop_ns_shortName : string;
    isEmbeddedArray : Boolean;
    typeHelper : IXsdSpecialTypeHelper;
  begin
    p := AProp;
    if AnsiSameText('Has',Copy(p.StoredAccessorName,1,3)) or AnsiSameText('True',p.StoredAccessorName) then begin
      if AContainer.IsAttributeProperty(p) then begin
        s := Format('%s:%s',[s_xs_short,s_attribute]);
        if Assigned(derivationNode) then
          propNode := CreateElement(s,derivationNode,ADocument)
        else
          propNode := CreateElement(s,cplxNode,ADocument);
      end else begin
        s := Format('%s:%s',[s_xs_short,s_element]);
        propNode := CreateElement(s,sqcNode,ADocument);
      end;
      propNode.SetAttribute(s_name,AContainer.GetExternalName(p));
      propTypItm := p.VarType;
      if Assigned(propTypItm) then begin
        if propTypItm.InheritsFrom(TPasUnresolvedTypeRef) then
          propTypItm := AContainer.FindElement(AContainer.GetExternalName(propTypItm)) as TPasType;
        //prop_ns_shortName := GetNameSpaceShortName(GetTypeNameSpace(AContainer,propTypItm),ADocument,GetOwner().GetPreferedShortNames());
        propItmUltimeType := GetUltimeType(propTypItm);
        isEmbeddedArray := propItmUltimeType.InheritsFrom(TPasArrayType) and
                           ( AContainer.GetArrayStyle(TPasArrayType(propItmUltimeType)) = asEmbeded );
        if isEmbeddedArray then begin
          s := AContainer.GetExternalName(TPasArrayType(propItmUltimeType).ElType);
          prop_ns_shortName := GetNameSpaceShortName(GetTypeNameSpace(AContainer,TPasArrayType(propItmUltimeType).ElType),ADocument,GetOwner().GetPreferedShortNames());
        end else begin
          s := AContainer.GetExternalName(propTypItm);
          prop_ns_shortName := GetNameSpaceShortName(GetTypeNameSpace(AContainer,propTypItm),ADocument,GetOwner().GetPreferedShortNames());
        end;
        propNode.SetAttribute(s_type,Format('%s:%s',[prop_ns_shortName,s]));
        if propTypItm.InheritsFrom(TPasNativeSpecialSimpleType) then begin
          if GetRegistry().FindHelper(propTypItm,typeHelper) then
            typeHelper.HandleTypeUsage(propNode,defSchemaNode);
        end;
        if ( Length(p.DefaultValue) > 0 ) then
          propNode.SetAttribute(s_default,p.DefaultValue);
        if AContainer.IsAttributeProperty(p) then begin
          if AnsiSameText('Has',Copy(p.StoredAccessorName,1,3)) then begin
            {propNode.SetAttribute(s_use,'optional')}
          end else begin
            propNode.SetAttribute(s_use,'required');
          end;
        end else begin
          if AnsiSameText('Has',Copy(p.StoredAccessorName,1,3)) then
            propNode.SetAttribute(s_minOccurs,'0');
          if isEmbeddedArray then begin
            propNode.SetAttribute(s_maxOccurs,s_unbounded);
            if AContainer.IsCollection(TPasArrayType(propItmUltimeType)) then begin
              DeclareNameSpaceOf_WST(ADocument);
              DeclareAttributeOf_WST(propNode,s_WST_collection,'true');
            end;
          end;
        end;
      end;
      ProcessPropertyExtendedMetadata(p,propNode);
    end;
  end;

var
  typItm : TPasClassType;
  s : string;
  i : Integer;
  typeCategory : TTypeCategory;
  hasSequence : Boolean;
  trueParent : TPasType;
  hasXsdAny, hasXsdAnyAtt : Boolean;
  xsdAnyString, xsdAnyAttString : string;
  ls : TStrings;
begin
  inherited;
  typItm := ASymbol as TPasClassType;
  if Assigned(typItm) then begin
    GetNameSpaceShortName(AContainer.GetExternalName(AContainer.CurrentModule) ,ADocument,GetOwner().GetPreferedShortNames());
    defSchemaNode := GetSchemaNode(ADocument) as TDOMElement;

    s := Format('%s:%s',[s_xs_short,s_complexType]);
    cplxNode := CreateElement(s,defSchemaNode,ADocument);
    cplxNode.SetAttribute(s_name, AContainer.GetExternalName(typItm)) ;

{$IFDEF WST_HANDLE_DOC}
    ls := AContainer.Properties.FindList(typItm);
    if ( ls <> nil ) then begin
      i := ls.IndexOfName(s_documentation);
      if ( i >= 0 ) then
        GenerateDocumentation(cplxNode,DecodeLineBreak(ls.ValueFromIndex[i]),ADocument);
    end;
{$ENDIF WST_HANDLE_DOC}

    typeCategory := tcComplexContent;
    derivationNode := nil;
    hasSequence := True;
    if Assigned(typItm.AncestorType) then begin
      trueParent := typItm.AncestorType;
      if trueParent.InheritsFrom(TPasUnresolvedTypeRef) then
        trueParent := AContainer.FindElement(AContainer.GetExternalName(trueParent)) as TPasType;
      if trueParent.InheritsFrom(TPasNativeClassType) and AnsiSameText('THeaderBlock',trueParent.Name) then begin
        DeclareNameSpaceOf_WST(ADocument);
        DeclareAttributeOf_WST(cplxNode,s_WST_headerBlock,'true');
      end else if trueParent.InheritsFrom(TPasNativeClassType) and AnsiSameText('TSimpleContentHeaderBlock',trueParent.Name) then begin
        DeclareNameSpaceOf_WST(ADocument);
        DeclareAttributeOf_WST(cplxNode,s_WST_headerBlockSimpleContent,'true');
      end;

      if trueParent.InheritsFrom(TPasAliasType) then
        trueParent := GetUltimeType(trueParent);
      if trueParent.InheritsFrom(TPasNativeSimpleContentClassType) or
         trueParent.InheritsFrom(TPasNativeSimpleType)
      then begin
        typeCategory := tcSimpleContent;
      end;
      if trueParent.InheritsFrom(TPasNativeSimpleContentClassType) or
         ( not trueParent.InheritsFrom(TPasNativeClassType) )
      then begin
        if ( typeCategory = tcSimpleContent ) then begin
          derivationNode := CreateElement(Format('%s:%s',[s_xs_short,s_simpleContent]),cplxNode,ADocument);
          derivationNode := CreateElement(Format('%s:%s',[s_xs_short,s_extension]),derivationNode,ADocument);
        end else begin
          derivationNode := CreateElement(Format('%s:%s',[s_xs_short,s_extension]),cplxNode,ADocument);
        end;
        s := Trim(GetNameSpaceShortName(GetTypeNameSpace(AContainer,trueParent),ADocument,GetOwner().GetPreferedShortNames()));
        if ( Length(s) > 0 ) then
          s := s + ':';
        s := s + AContainer.GetExternalName(trueParent);
        derivationNode.SetAttribute(s_base,s);
      end;
      hasSequence := False;
    end;
    if ( typItm.Members.Count > 0 ) then
      hasSequence := TypeHasSequence(typItm,typeCategory);
    hasXsdAny := False;
    hasXsdAnyAtt := False;
    if ( typeCategory = tcComplexContent ) then begin
      ls := AContainer.Properties.FindList(typItm);
      i := ls.IndexOfName(Format('%s#%s',[s_xs,s_any]));
      hasXsdAny := ( i > 0 );
      if hasXsdAny then begin
        xsdAnyString := ls.ValueFromIndex[i];
        if not hasSequence then
          hasSequence := True;
      end;
      i := ls.IndexOfName(Format('%s#%s',[s_xs,s_anyAttribute]));
      hasXsdAnyAtt := ( i > 0 );
      if hasXsdAnyAtt then
        xsdAnyAttString := ls.ValueFromIndex[i];
    end;
    if hasSequence then begin
      s := Format('%s:%s',[s_xs_short,s_sequence]);
      if Assigned(derivationNode) then
        sqcNode := CreateElement(s,derivationNode,ADocument)
      else
        sqcNode := CreateElement(s,cplxNode,ADocument);
    end else begin
      sqcNode := nil;
    end;

    for i := 0 to Pred(typItm.Members.Count) do begin
      if TPasElement(typItm.Members[i]).InheritsFrom(TPasProperty) then
        ProcessProperty(TPasProperty(typItm.Members[i]));
    end;
    if hasXsdAny then
      ProcessXsdAny(sqcNode,xsdAnyString);
    if hasXsdAnyAtt then
      ProcessXsdAnyAttribute(cplxNode,xsdAnyAttString);
  end;
end;

class function TClassTypeDefinition_TypeHandler.CanHandle(ASymbol: TObject): Boolean;
begin
  Result := inherited CanHandle(ASymbol) and
            ( ASymbol.InheritsFrom(TPasClassType) and ( TPasClassType(ASymbol).ObjKind = okClass ));
end;

{ TPasRecordType_TypeHandler }

procedure TPasRecordType_TypeHandler.Generate(
        AContainer : TwstPasTreeContainer;
  const ASymbol : TPasElement;
        ADocument : TDOMDocument
);
var
  cplxNode : TDOMElement;
  typItm : TPasRecordType;
  propTypItm : TPasType;
  s, prop_ns_shortName : string;
  defSchemaNode, sqcNode, propNode : TDOMElement;
  i : Integer;
  p : TPasVariable;
  hasSequence : Boolean;
  storeOption : string;
{$IFDEF WST_HANDLE_DOC}
  ls : TStrings;
{$ENDIF WST_HANDLE_DOC}
begin
  inherited;
  typItm := ASymbol as TPasRecordType;
  if Assigned(typItm) then begin
    GetNameSpaceShortName(AContainer.GetExternalName(AContainer.CurrentModule) ,ADocument,GetOwner().GetPreferedShortNames());
    defSchemaNode := GetSchemaNode(ADocument) as TDOMElement;

    s := Format('%s:%s',[s_xs_short,s_complexType]);
    cplxNode := CreateElement(s,defSchemaNode,ADocument);
    cplxNode.SetAttribute(s_name, AContainer.GetExternalName(typItm)) ;

    DeclareNameSpaceOf_WST(ADocument);
    DeclareAttributeOf_WST(cplxNode,s_WST_record,'true');
{$IFDEF WST_HANDLE_DOC}
    ls := AContainer.Properties.FindList(typItm);
    if ( ls <> nil ) then begin
      i := ls.IndexOfName(s_documentation);
      if ( i >= 0 ) then
        GenerateDocumentation(cplxNode,DecodeLineBreak(ls.ValueFromIndex[i]),ADocument);
    end;
{$ENDIF WST_HANDLE_DOC}

    hasSequence := False;
    for i := 0 to Pred(typItm.Members.Count) do begin
      if TPasElement(typItm.Members[i]).InheritsFrom(TPasVariable) then begin
        p := TPasVariable(typItm.Members[i]);
        if not AContainer.IsAttributeProperty(p) then begin
          hasSequence := True;
          Break;
        end;
      end;
    end;
    if hasSequence then begin
      s := Format('%s:%s',[s_xs_short,s_sequence]);
      sqcNode := CreateElement(s,cplxNode,ADocument);
    end else begin
      sqcNode := nil;
    end;

    for i := 0 to Pred(typItm.Members.Count) do begin
      if TPasElement(typItm.Members[i]).InheritsFrom(TPasVariable) then begin
        p := TPasVariable(typItm.Members[i]);
        if AContainer.IsAttributeProperty(p) then begin
          s := Format('%s:%s',[s_xs_short,s_attribute]);
          propNode := CreateElement(s,cplxNode,ADocument);
        end else begin
          s := Format('%s:%s',[s_xs_short,s_element]);
          propNode := CreateElement(s,sqcNode,ADocument);
        end;
        propNode.SetAttribute(s_name,AContainer.GetExternalName(p));
        propTypItm := p.VarType;
        if Assigned(propTypItm) then begin
          if propTypItm.InheritsFrom(TPasUnresolvedTypeRef) then
            propTypItm := AContainer.FindElement(AContainer.GetExternalName(propTypItm)) as TPasType;
          prop_ns_shortName := GetNameSpaceShortName(GetTypeNameSpace(AContainer,propTypItm),ADocument,GetOwner().GetPreferedShortNames());
          propNode.SetAttribute(s_type,Format('%s:%s',[prop_ns_shortName,AContainer.GetExternalName(propTypItm)]));
          storeOption := Trim(AContainer.Properties.GetValue(p,s_WST_storeType));
          if AContainer.IsAttributeProperty(p) then begin
            if ( Length(storeOption) > 0 ) then begin
              case AnsiIndexText(storeOption,[s_required,s_optional,s_prohibited]) of
                0 : propNode.SetAttribute(s_use,storeOption);
                1 : ;
                2 : propNode.SetAttribute(s_use,storeOption);
                else
                  raise EXsdGeneratorException.CreateFmt('Invalid attribute "%s" value : "%s".',[s_use,storeOption]);
              end;
            end;
          end else begin
            case AnsiIndexText(storeOption,[s_required,s_optional,s_prohibited]) of
              0 : ;//propNode.SetAttribute(s_minOccurs,'1');
              1 : propNode.SetAttribute(s_minOccurs,'0');
            end;
            //propNode.SetAttribute(s_maxOccurs,'1');
          end;
        end;
      end;
    end;
  end;
end;

class function TPasRecordType_TypeHandler.CanHandle(ASymbol : TObject) : Boolean;
begin
  Result := inherited CanHandle(ASymbol) and ASymbol.InheritsFrom(TPasRecordType);
end;

{ TBaseArrayRemotable_TypeHandler }

procedure TBaseArrayRemotable_TypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol       : TPasElement;
        ADocument : TDOMDocument
);

  function GetNameSpaceShortName(const ANameSpace : string):string;
  begin
    if FindAttributeByValueInNode(ANameSpace,ADocument.DocumentElement,Result,0,s_xmlns) then begin
      Result := Copy(Result,Length(s_xmlns+':')+1,MaxInt);
    end else begin
      Result := Format('ns%d',[GetNodeListCount(ADocument.DocumentElement.Attributes)]) ;
      ADocument.DocumentElement.SetAttribute(Format('%s:%s',[s_xmlns,Result]),ANameSpace);
    end;
  end;

var
  typItm : TPasArrayType;
  propTypItm : TPasType;
  s, prop_ns_shortName : string;
  defSchemaNode, cplxNode, sqcNode, propNode : TDOMElement;
  unitExternalName : string;
{$IFDEF WST_HANDLE_DOC}
  i : PtrInt;
  ls : TStrings;
{$ENDIF WST_HANDLE_DOC}
begin
  inherited;
  typItm := ASymbol as TPasArrayType;
  if not Assigned(typItm) then
    Exit;
  if Assigned(typItm) then begin
    unitExternalName := GetTypeNameSpace(AContainer,typItm);
    GetNameSpaceShortName(unitExternalName);
    defSchemaNode := GetSchemaNode(ADocument) as TDOMElement;

    s := Format('%s:%s',[s_xs_short,s_complexType]);
    cplxNode := CreateElement(s,defSchemaNode,ADocument);
    cplxNode.SetAttribute(s_name, AContainer.GetExternalName(typItm)) ;
{$IFDEF WST_HANDLE_DOC}
    ls := AContainer.Properties.FindList(typItm);
    if ( ls <> nil ) then begin
      i := ls.IndexOfName(s_documentation);
      if ( i >= 0 ) then
        GenerateDocumentation(cplxNode,DecodeLineBreak(ls.ValueFromIndex[i]),ADocument);
    end;
{$ENDIF WST_HANDLE_DOC}

      s := Format('%s:%s',[s_xs_short,s_sequence]);
      sqcNode := CreateElement(s,cplxNode,ADocument);
      propTypItm := typItm.ElType;
      s := Format('%s:%s',[s_xs_short,s_element]);
      propNode := CreateElement(s,sqcNode,ADocument);
      propNode.SetAttribute(s_name,s_item);
      if AContainer.IsCollection(typItm) then begin
        DeclareNameSpaceOf_WST(ADocument);
        DeclareAttributeOf_WST(propNode,s_WST_collection,'true');
      end;
      if Assigned(propTypItm) then begin
        prop_ns_shortName := GetNameSpaceShortName(GetTypeNameSpace(AContainer,propTypItm));//  AContainer.GetExternalName(propTypItm.Parent.Parent));
        propNode.SetAttribute(s_type,Format('%s:%s',[prop_ns_shortName,AContainer.GetExternalName(propTypItm)]));
        propNode.SetAttribute(s_minOccurs,'0');
        propNode.SetAttribute(s_maxOccurs,s_unbounded);
      end;
  end;
end;

class function TBaseArrayRemotable_TypeHandler.CanHandle(ASymbol: TObject): Boolean;
begin
  Result := inherited CanHandle(ASymbol) and ASymbol.InheritsFrom(TPasArrayType);
end;

{ TCustomXsdGenerator }

procedure TCustomXsdGenerator.Execute(
  ASymTable   : TwstPasTreeContainer;
  AModuleName : string
);
var
  j, k : Integer;
  tri : TPasElement;
  g : IXsdTypeHandler;
  gr : IXsdTypeHandlerRegistry;
  typeList : TList;
  mdl : TPasModule;
begin
  if ( ASymTable = nil ) then
    raise EXsdGeneratorException.Create('Invalid symbol table.');
  mdl := ASymTable.FindModule(AModuleName);
  if ( mdl = nil ) then
    raise EXsdGeneratorException.CreateFmt('Unable to find module : "%s".',[AModuleName]);
  Prepare(ASymTable,mdl);
  gr := GetXsdTypeHandlerRegistry();
  typeList := mdl.InterfaceSection.Declarations;
  k := typeList.Count;
  if ( xgoIgnorembeddedArray in Options ) then begin
    for j := 0 to Pred(k) do begin
      tri := TPasElement(typeList[j]);
      if tri.InheritsFrom(TPasType) and
         ( not tri.InheritsFrom(TPasNativeClassType) ) and
         ( not tri.InheritsFrom(TPasNativeSimpleType) ) and
         ( ( not tri.InheritsFrom(TPasArrayType) ) or
           ( ASymTable.GetArrayStyle(TPasArrayType(tri)) <> asEmbeded )
         )
      then begin
        if gr.Find(tri,Self,g) then
          g.Generate(ASymTable,tri,Self.Document);
      end;
    end;
  end else begin
    for j := 0 to Pred(k) do begin
      tri := TPasElement(typeList[j]);
      if tri.InheritsFrom(TPasType) and
         ( not tri.InheritsFrom(TPasNativeClassType) ) and
         ( not tri.InheritsFrom(TPasNativeSimpleType) )
      then begin
        if gr.Find(tri,Self,g) then
          g.Generate(ASymTable,tri,Self.Document);
      end;
    end;
  end;
end;

procedure TCustomXsdGenerator.Prepare(ASymTable : TwstPasTreeContainer; AModule : TPasModule);
begin

end;

constructor TCustomXsdGenerator.Create(const ADocument : TDOMDocument);
begin
  Create(ADocument,[]);
end;

constructor TCustomXsdGenerator.Create(
  const ADocument: TDOMDocument;
  const AOptions: TGeneratorOptions
);
var
  sl : TStringList;
begin
  if ( ADocument = nil ) then
    raise EXsdGeneratorException.Create('Invalid document.');
  FDocument := ADocument;
  FOptions := AOptions;
  FShortNames := TStringList.Create();
  sl := TStringList(FShortNames);
  //sl.Sorted := True;
  sl.Duplicates := dupIgnore;
end;

procedure TCustomXsdGenerator.SetPreferedShortNames(const ALongName, AShortName: string);
begin
  FShortNames.Values[ALongName] := AShortName;
end;

function TCustomXsdGenerator.GetPreferedShortNames() : TStrings;
begin
  Result := FShortNames;
end;

destructor TCustomXsdGenerator.Destroy();
begin
  FreeAndNil(FShortNames);
  inherited;
end;

{ TXsdGenerator }

function TXsdGenerator.GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;
begin
  Result := FSchemaNode;
end;

procedure TXsdGenerator.Prepare(ASymTable : TwstPasTreeContainer; AModule : TPasModule);
var
  unitExternalName : string;
begin
  inherited Prepare(ASymTable, AModule);
  unitExternalName := ASymTable.GetExternalName(AModule);
  FSchemaNode := CreateElement(s_schema,Document,Document);
  FSchemaNode.SetAttribute(s_targetNamespace,unitExternalName);
  FSchemaNode.SetAttribute(Format('%s:%s',[s_xmlns,s_xs_short]),s_xs);
  FSchemaNode.SetAttribute(Format('%s:%s',[s_xmlns,s_tns]),unitExternalName);
end;


initialization
  XsdTypeHandlerRegistryInst := TXsdTypeHandlerRegistry.Create() as IXsdTypeHandlerRegistry;
  RegisterFondamentalTypes();

finalization
  XsdTypeHandlerRegistryInst := nil;
  
end.
