{   This file is part of the Web Service Toolkit
    Copyright (c) 2006, 2007, 2008 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit test_generators;

interface
uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testutils, testregistry, DOM, XmlRead, XmlWrite, wst_fpc_xml,
{$ELSE}
  TestFrameWork, xmldom, wst_delphi_xml,
{$ENDIF}
  pastree, pascal_parser_intf, xsd_generator;

type

  TPropertyType = ( ptField, ptAttribute );

  TTest_CustomXsdGenerator = class(TTestCase)
  protected
    function CreateGenerator(const ADoc : TXMLDocument) : IXsdGenerator;virtual;abstract;
    function LoadXmlFromFilesList(const AFileName : string) : TXMLDocument;
  published
    procedure class_properties_default();
    procedure class_properties_extended_metadata();
  end;

  TTest_XsdGenerator = class(TTest_CustomXsdGenerator)
  protected
    function CreateGenerator(const ADoc : TXMLDocument) : IXsdGenerator;override;
  end;

implementation

uses test_suite_utils;

{ TTest_CustomXsdGenerator }

procedure TTest_CustomXsdGenerator.class_properties_default();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  cltyp : TPasClassType;

  procedure AddProperty(
    const AName,
          ATypeName,
          ADefault   : string;
    const AKind      : TPropertyType
  );
  var
    p : TPasProperty;
  begin
    p := TPasProperty(tr.CreateElement(TPasProperty,AName,cltyp,visDefault,'',0));
    cltyp.Members.Add(p);
    p.ReadAccessorName := 'F' + AName;
    p.WriteAccessorName := 'F' + AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
    p.DefaultValue := ADefault;
    p.Visibility := visPublished;
    p.StoredAccessorName := 'True';
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
  end;

var
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'class_properties_default',tr.Package,visDefault,'',0));
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TPasSection(tr.CreateElement(TPasSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TClassSampleType',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      AddProperty('intField','integer','1210',ptField);
      AddProperty('strField','string','azerty',ptField);
      AddProperty('floatField','float','1234',ptField);
      AddProperty('strAtt','string','attribute azerty',ptAttribute);
      AddProperty('intAtt','integer','789',ptAttribute);

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    WriteXMLFile(locDoc,'.\class_properties_default.xsd');
    locExistDoc := LoadXmlFromFilesList('class_properties_default.xsd');
    Check(CompareNodes(locExistDoc,locDoc),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdGenerator.class_properties_extended_metadata();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  cltyp : TPasClassType;

  function AddProperty(
    const AName,
          ATypeName,
          ADefault   : string;
    const AKind      : TPropertyType;
    const AExtMetadataName,
          AExtMetadataValue : string
  ) : TPasProperty;
  var
    p : TPasProperty;
  begin
    p := TPasProperty(tr.CreateElement(TPasProperty,AName,cltyp,visDefault,'',0));
    cltyp.Members.Add(p);
    p.ReadAccessorName := 'F' + AName;
    p.WriteAccessorName := 'F' + AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
    p.DefaultValue := ADefault;
    p.Visibility := visPublished;
    p.StoredAccessorName := 'True';
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
    if ( Length(AExtMetadataName) > 0 ) then
      tr.Properties.SetValue(p,AExtMetadataName,AExtMetadataValue);
    Result := p;
  end;

var
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
  p : TPasProperty;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'urn:wst-test',tr.Package,visDefault,'',0));
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TPasSection(tr.CreateElement(TPasSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TClassSampleType',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      p := AddProperty('intField','integer','',ptField,'uri-4#a','1210');
        tr.Properties.SetValue(p,'uri-4#b','uri-5#xx');
      AddProperty('strField','string','azerty',ptField,'uri-4#a','http://www.w3.org/2001/XMLSchema#int');
      AddProperty('floatField','float','',ptField,'','');
      AddProperty('strAtt','string','attribute azerty',ptAttribute,'uri-4#a','optional');
      AddProperty('intAtt','integer','',ptAttribute,'','');

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    WriteXMLFile(locDoc,'.\class_properties_extended_metadata.xsd');
    locExistDoc := LoadXmlFromFilesList('class_properties_extended_metadata.xsd');
    Check(CompareNodes(locExistDoc,locDoc),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

function TTest_CustomXsdGenerator.LoadXmlFromFilesList(const AFileName: string): TXMLDocument;
var
  locFileName : string;
begin
{$IFDEF FPC}
  locFileName := Format('.%sfiles%s%s',[PathDelim,PathDelim,AFileName]);
{$ENDIF}
{$IFDEF DELPHI}
  locFileName := Format('..%sfiles%s%s',[PathDelim,PathDelim,AFileName]);
{$ENDIF}
  ReadXMLFile(Result,locFileName);
end;

{ TTest_XsdGenerator }

function TTest_XsdGenerator.CreateGenerator(const ADoc: TXMLDocument): IXsdGenerator;
begin
  Result := TXsdGenerator.Create(ADoc) as IXsdGenerator;
end;

initialization
  RegisterTest('XSD generator',TTest_XsdGenerator.Suite);

end.