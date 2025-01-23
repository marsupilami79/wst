{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006-2017 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
resourcestring
  sUSAGE = 'ws_helper [-uMODE] [-gOPTION] [-cOPTION] [-p] [-b] [-i] [-w] [-x] [-y] [-d] [-j] -[fSPECIFACTIONS] [-oPATH] [-aPATH] inputFilename' + sNEW_LINE +
           '  -u MODE Generate the pascal translation of the WSDL input file ' + sNEW_LINE +
           '        MODE value may be U for used types or A for all types' + sNEW_LINE +
           '  -g  Code generation option, with the following options : ' + sNEW_LINE +
           '        A  : object arrays are generated as "array" derived from TBaseObjectArrayRemotable' + sNEW_LINE +
           '        C  : object arrays are generated as "collection" derived from TObjectCollectionRemotable' + sNEW_LINE +
           '        EP : enum type''s items are prefixed with the enum name' + sNEW_LINE +
           '        EN : enum type''s items are not prefixed with the enum name, the default' + sNEW_LINE +
           '        FN : do not create fields for "choice"''s items in constructor' + sNEW_LINE +
           '        FO : create fields for "choice"''s items in constructor, the default' + sNEW_LINE +
           '        SS : XSD''string type is mapped to Object Pascal'' String' + sNEW_LINE +
           '        SU : XSD''string type is mapped to Object Pascal'' UnicodeString' + sNEW_LINE +
           '  -p  Generate service proxy' + sNEW_LINE +
           '  -b  Generate service binder' + sNEW_LINE +
           '  -i  Generate service minimal implementation. This will erase any existing implementation file!' + sNEW_LINE +
           '  -o  PATH  Relative output directory' + sNEW_LINE +
           '  -a  PATH  Absolute output directory' + sNEW_LINE +
           '  -w  Generate WSDL file; Can be used to get wsdl from pascal' + sNEW_LINE +
           '  -x  Generate XSD file; Can be used to get xsd from pascal' + sNEW_LINE +
           '  -y  Generate easy access interface for wrapped parameters' + sNEW_LINE +
           '  -d  Generate documentation as comment in the interface file' + sNEW_LINE +
           '  -c  Indicate the parser''s case sensitivity : ' + sNEW_LINE +
           '        S  : the paser is case sensitive' + sNEW_LINE +
           '        I  : the paser is not case sensitive' + sNEW_LINE +
           '  -j  Generate Java Language interface files for' + sNEW_LINE +  
           '  -t  Generate TypeScript Language interface files ' + sNEW_LINE +
           '  -f  Specify unit(s) renaming option : oldName= NewName(;oldName= NewName)* ';
  sCOPYRIGHT = 'ws_helper, Web Service Toolkit 0.7 Copyright (c) 2006-2017 by Inoussa OUEDRAOGO';


type
  TSourceFileType = ( sftPascal, sftWSDL, sftXsd );
  TExportLanguage = (exlgPascal, exlgWSDL, exlgXSD, exlgJava, exlgTypeScript);
  TExportLanguages = set of TExportLanguage;
  
var
  inFileName,outPath,errStr : string;
  AppOptions : TComandLineOptions;
  exportLanguages : TExportLanguages;
  NextParam : Integer;
  sourceType : TSourceFileType;
  parserMode : TParserMode;
  
  osParam, targetParam : string;

  function DetermineExportedLanguages(AAppOptions : TComandLineOptions) : TExportLanguages;
  var
    r : TExportLanguages;
  begin
    r := [];
    if ([cloInterface, cloProxy, cloImp, cloBinder]*AAppOptions <> []) then
      r := r + [exlgPascal];    
    if (cloWsdl in AAppOptions) then
      r := r + [exlgWSDL];
    if (cloXsd in AAppOptions) then
      r := r + [exlgXSD];
    if (cloJava in AAppOptions) then
      r := r + [exlgJava];
    if (cloTypeScript in AAppOptions) then
      r := r + [exlgTypeScript];
    if (r = []) then      
      r := r + [exlgPascal];
    Result := r;
  end;

  function ProcessCmdLine():boolean;
  begin
    NextParam := ParseCmdLineOptions(AppOptions);
    if ( NextParam <= Paramcount ) then begin
      inFileName := ParamStr(NextParam);
    end;
    Result := FileExists(ExpandFileName(inFileName));
    if AnsiSameText(ExtractFileExt(inFileName),'.PAS') or
       AnsiSameText(ExtractFileExt(inFileName),'.PP')
    then begin
      sourceType := sftPascal;
    end else if AnsiSameText(ExtractFileExt(inFileName),'.WSDL') then begin
      sourceType := sftWSDL;
    end else if AnsiSameText(ExtractFileExt(inFileName),'.XSD') then begin
      sourceType := sftXsd;
    end;
    if Result then begin
      if ( AppOptions = [] ) then begin
        Include(AppOptions,cloProxy);
      end;
    end else begin
      errStr := Format('File not Found : "%s"',[inFileName]);
    end;
    if ( cloOutPutDirAbsolute in AppOptions ) then begin
      outPath := Trim(GetOptionArg(cloOutPutDirAbsolute));
    end else begin
      outPath := ExtractFilePath(inFileName);
      if ( cloOutPutDirRelative in AppOptions ) then begin
        outPath := outPath + Trim(GetOptionArg(cloOutPutDirRelative));
      end;
    end;
    outPath := IncludeTrailingPathDelimiter(outPath);
    parserMode := pmUsedTypes;
    if AnsiSameText('A',Trim(GetOptionArg(cloInterface))) then begin
      parserMode := pmAllTypes;
    end;
    if AnsiSameText('C',Trim(GetOptionArg(cloGenerateObjectCollection))) then begin
      Include(AppOptions,cloGenerateObjectCollection);
    end;    
    if ( sourceType = sftXsd ) then begin
      AppOptions := AppOptions - [ cloProxy, cloImp, cloBinder, cloWsdl ];
    end;
    if (cloParserCaseSensitive in AppOptions) then begin
      if AnsiSameText('S',Trim(GetOptionArg(cloParserCaseSensitive))) then
        Include(AppOptions,cloParserCaseSensitive);
      if AnsiSameText('I',Trim(GetOptionArg(cloParserCaseSensitive))) then
        Exclude(AppOptions,cloParserCaseSensitive);
    end else begin
      Include(AppOptions,cloParserCaseSensitive);
    end;
    if not(cloCreateChoiceFields in AppOptions) then begin
      Include(AppOptions,cloCreateChoiceFields);
    end else begin
      if AnsiSameText('FN',Trim(GetOptionArg(cloCreateChoiceFields))) then
        Exclude(AppOptions,cloCreateChoiceFields);
    end;
    exportLanguages := DetermineExportedLanguages(AppOptions);
  end;     

  procedure Error(const AMsg : string);
  begin
    raise Exception.Create(AMsg);
  end;

  function CreateSymboleTable(const ALangage : TExportLanguage) : TwstPasTreeContainer;
  var
    tmpString : string;
  begin
    Result := TwstPasTreeContainer.Create();
    try
      Result.CaseSensitive :=
        (ALangage in [exlgJava,exlgTypeScript]) or
        (cloParserCaseSensitive in AppOptions);
      if (ALangage = exlgJava) then
        JavaCreateWstInterfaceSymbolTable(Result)
      else if (ALangage = exlgTypeScript) then
        TypeScriptCreateWstInterfaceSymbolTable(Result)
      else if(ALangage = exlgPascal) then begin
        if (cloStringMaping in AppOptions) then begin
          tmpString := Trim(GetOptionArg(cloStringMaping));
          tmpString := UpperCase(Copy(tmpString,2,Length(tmpString)));
          if (tmpString = 'S') then
            Result.XsdStringMaping := xsmString
          else if (tmpString = 'U') then
            Result.XsdStringMaping := xsmUnicodeString
          else
            Error('Invalid argument for "-gS" option: "' + tmpString + '".');
        end;
      end;
    except
      Result.Free();
      raise;
    end;
  end;

  function GenerateSymbolTable(ASymtable : TwstPasTreeContainer) : Boolean ;

    procedure ParsePascalFile();
    begin
      ParseSource(ASymtable,inFileName,osParam,targetParam);
      if ( ASymtable <> nil ) then
        CreateDefaultBindingForIntf(ASymtable);
    end;

    function GetParserSimpleOptions( ) : TParserOptions;
    begin
      Result := [];
      if ( cloPrefixEnum in AppOptions ) then begin
        if ( Pos('P',GetOptionArg(cloPrefixEnum)) = 2 ) then
          Result := Result + [poEnumAlwaysPrefix];
      end;
    end;

    procedure ParseWsdlFile();
    var
      locDoc : TXMLDocument;
      prsrW : IParser;
      prsrCtx : IParserContext;
    begin
      locDoc := ReadXMLFile(inFileName);
{$IFNDEF WST_INTF_DOM}
      try
{$ENDIF}
        prsrW := TWsdlParser.Create(locDoc,ASymtable);// as IParser;
        prsrCtx := prsrW as IParserContext;
        prsrCtx.SetDocumentLocator(TFileDocumentLocator.Create(ExtractFilePath(ExpandFileName(inFileName))));
        prsrCtx.SetSimpleOptions(GetParserSimpleOptions());
        prsrW.Execute(parserMode,ChangeFileExt(ExtractFileName(inFileName),''));
{$IFNDEF WST_INTF_DOM}
      finally
        prsrW := nil;
        ReleaseDomNode(locDoc);
      end;
{$ENDIF}
    end;

    procedure ParseXsdFile();
    var
      locDoc : TXMLDocument;
      prsr : IXsdPaser;
      prsrCtx : IParserContext;
    begin
      prsr := nil;
      locDoc := ReadXMLFile(inFileName);
{$IFNDEF WST_INTF_DOM}
      try
{$ENDIF}
        prsr := TXsdParser.Create(locDoc,ASymtable,ChangeFileExt(ExtractFileName(inFileName),'')) as IXsdPaser;
        prsrCtx := prsr as IParserContext;
        prsrCtx.SetDocumentLocator(TFileDocumentLocator.Create(ExtractFilePath(ExpandFileName(inFileName))));
        prsrCtx.SetSimpleOptions(GetParserSimpleOptions());
        prsr.ParseTypes();
{$IFNDEF WST_INTF_DOM}
      finally
        ReleaseDomNode(locDoc);
      end;
{$ENDIF}
    end;

  begin
    try
      WriteLn('Parsing the file : ', inFileName);
      if ASymtable.CaseSensitive then
        WriteLn('The parser is case sensitive');
      case sourceType of
        sftPascal : ParsePascalFile();
        sftWSDL   : ParseWsdlFile();
        sftXsd    : ParseXsdFile();
      end;
      Result := True;
    except
      on e : Exception do begin
        Result := False;
        errStr := e.Message;
      end;
    end;
  end;

  procedure GenerateWSDLFromTree(ASymbol : TwstPasTreeContainer; ADest : TStream);
  var
    doc : TXMLDocument;
    g : IGenerator;
  begin
    doc := CreateDoc();
    try
      g := TWsdlGenerator.Create(doc);
      g.Execute(ASymbol,ASymbol.CurrentModule.Name);
      WriteXML(doc,ADest);
    finally
      ReleaseDomNode(doc);
    end;
  end;

  procedure GenerateXsdFromTree(ASymbol : TwstPasTreeContainer; ADest : TStream);
  var
    doc : TXMLDocument;
    gnrtr : IGenerator;
  begin
    doc := CreateDoc();
    try
      gnrtr := TXsdGenerator.Create(doc);
      gnrtr.Execute(ASymbol,ASymbol.CurrentModule.Name);
      WriteXML(doc,ADest);
    finally
      ReleaseDomNode(doc);
    end;
  end;
  
  procedure HandleUnitRenaming(ASymtable : TwstPasTreeContainer);
  var
    namesSpecif, strBuffer, oldName, newName : string;
    k : Integer;
    mdlList : TList2;
    mdl : TPasModule;
    found : Boolean;
  begin
    namesSpecif := Trim(GetOptionArg(cloFileRenaming));             
    Write('Handling unit renaming ... ');
    WriteLn(namesSpecif);
    mdlList := ASymtable.Package.Modules;   
    strBuffer := namesSpecif;
    while True do begin
      oldName := GetToken(strBuffer,'=');
      if IsStrEmpty(oldName) then
        Break;
      newName := GetToken(strBuffer,';');
      if IsStrEmpty(newName) then
        raise Exception.CreateFmt('Invalid option unit renaming specification : "%s".',[namesSpecif]);
      found := False;
      for k := 0 to Pred(mdlList.Count) do begin
        mdl := TPasModule(mdlList[k]);
        if SameText(oldName,mdl.Name) then begin
          mdl.Name := newName;
          WriteLn(Format('Unit renamed, old name = "%s", new name = "%s".',[oldName,newName]));
          found := True;
          Break;
        end;  
      end;
      if not found then
        WriteLn(Format('Unit not found : "%s".',[oldName]));
    end;
    for k := 0 to Pred(mdlList.Count) do begin
      mdl := TPasModule(mdlList[k]);
      if mdl.InheritsFrom(TPasNativeModule) then
        Continue;
      newName := ExtractIdentifier(mdl.Name);
      if (newName <> mdl.Name) then begin
        oldName := mdl.Name;
        mdl.Name := newName;
        WriteLn(Format('Unit renamed, old name = "%s", new name = "%s".',[oldName,newName]));
      end;
    end;
  end;
  
  function GenerateForLangage(ASymtable : TwstPasTreeContainer; const ALanguage : TExportLanguage) : Boolean;
  Var      
    srcMngr : ISourceManager;
    mtdaFS: TMemoryStream;
    g : TBaseGenerator;
    mg : TMetadataGenerator;
    rsrcStrm : TMemoryStream;
    strStream : TStringStream;
    wrappedParams : Boolean;
  begin
    Result := False;  
    srcMngr := CreateSourceManager();
    HandleUnitRenaming(ASymtable);    
    wrappedParams := ( cloHandleWrappedParameters in AppOptions );
    strStream := nil;
    rsrcStrm := nil;
    mtdaFS := nil;
    mg := nil;
    g := Nil;
    try
      try
        if (ALanguage = exlgPascal) then begin
          if ( cloInterface in AppOptions ) then begin
            WriteLn('Interface file generation...');
            g := generator.TInftGenerator.Create(ASymtable,srcMngr);
            if wrappedParams then
              g.Options := g.Options + [goDocumentWrappedParameter];
            if ( cloGenerateObjectCollection in AppOptions ) then
              g.Options := g.Options + [goGenerateObjectCollection];
            if ( cloCreateChoiceFields in AppOptions ) then
              g.Options := g.Options + [goCreateChoiceFieldsInConstructor];
            g.Execute();
            FreeAndNil(g);
          end;

          If ( cloProxy in AppOptions ) Then Begin
            WriteLn('Proxy file generation...');
            g := generator.TProxyGenerator.Create(ASymtable,srcMngr);
            if wrappedParams then
              g.Options := g.Options + [goDocumentWrappedParameter];
            g.Execute();
            FreeAndNil(g);
          End;

          If ( cloBinder in AppOptions ) Then Begin
            WriteLn('Binder file generation...');
            g := generator.TBinderGenerator.Create(ASymtable,srcMngr);
            if wrappedParams then
              g.Options := g.Options + [goDocumentWrappedParameter];
            g.Execute();
            FreeAndNil(g);
          End;

          If ( cloImp in AppOptions ) Then Begin
            WriteLn('Implementation file generation...');
            g := generator.TImplementationGenerator.Create(ASymtable,srcMngr);
            if wrappedParams then
              g.Options := g.Options + [goDocumentWrappedParameter];
            g.Execute();
            FreeAndNil(g);
          End;

          if ( [cloBinder,cloProxy]*AppOptions  <> [] ) then begin
            WriteLn('Metadata file generation...');
            mtdaFS := TMemoryStream.Create();
            mg := TMetadataGenerator.Create(ASymtable,CreateBinaryWriter(mtdaFS));
            mg.Execute();
            //mtdaFS.SaveToFile(ChangeFileExt(inFileName,'.' + sWST_META));
            rsrcStrm := TMemoryStream.Create();
            mtdaFS.Position := 0;
            BinToWstRessource(UpperCase(ASymtable.CurrentModule.Name),mtdaFS,rsrcStrm);
            rsrcStrm.SaveToFile(outPath + ChangeFileExt(ExtractFileName(ASymtable.CurrentModule.Name),'.' + sWST_EXTENSION));
          end;
        end;

        if (ALanguage = exlgWSDL) then begin
          strStream := TStringStream.Create('');
          GenerateWSDLFromTree(ASymtable,strStream);
          if not IsStrEmpty(strStream.DataString) then begin
            strStream.Position := 0;
            srcMngr.CreateItem(ChangeFileExt(ExtractFileName(inFileName),'.wsdl')).Write(strStream.DataString);
          end;
        end;

        if (ALanguage = exlgXSD) then begin
          strStream := TStringStream.Create('');
          GenerateXsdFromTree(ASymtable,strStream);
          if not IsStrEmpty(strStream.DataString) then begin
            strStream.Position := 0;
            srcMngr.CreateItem(ChangeFileExt(ExtractFileName(inFileName),'.xsd')).Write(strStream.DataString);
          end;
        end;

        if (ALanguage = exlgJava) then begin
          WriteLn('Java file generation...');
          g := generatorj.TInftGenerator.Create(ASymtable,srcMngr);
          //if wrappedParams then
            //g.Options := g.Options + [goDocumentWrappedParameter];
          g.Execute();
          FreeAndNil(g);
        end;

        if (ALanguage = exlgTypeScript) then begin
          WriteLn('TypeScript file generation...');
          g := generatorts.TInftGenerator.Create(ASymtable,srcMngr);
          if (cloProxy in AppOptions) then begin
            generatorts.TInftGenerator(g).OptionsEx :=
              generatorts.TInftGenerator(g).OptionsEx +
              [TTypeScriptOption.GenerateProxy];
          end;
          g.Execute();
          FreeAndNil(g);
        end;

        srcMngr.SaveToFile(outPath);
        if ( GetLogger().GetMessageCount(mtError) = 0 ) then begin
          WriteLn(Format('File "%s" parsed succesfully.',[inFileName]));
        end else begin
          WriteLn(Format('Parsing complete with %d error(s).',[GetLogger().GetMessageCount(mtError)]));
        end;

        Result := True;
      except
        on E : Exception do begin
          Result := False;
          errStr := E.Message;
        end;
      end;
    finally
      strStream.Free();
      rsrcStrm.Free();
      mg.Free();
      mtdaFS.Free();
      g.Free();
    end;
  end;

var
  symtable : TwstPasTreeContainer;
  exportlanguage : TExportLanguage;      
  parserModeKeep : TParserMode;
begin
{$IFDEF FPC}
  {$IF Declared(SetHeapTraceOutput) }
    SetHeapTraceOutput('heapOut.txt');
  {$IFEND}
{$ENDIF}
  osParam := 'windows';
  targetParam := 'x86';
  
  SetLogger(TSimpleConsoleLogger.Create());
  try
    try
      Writeln(sCOPYRIGHT);
      If ( ParamCount = 0 ) Then Begin
        WriteLn(sUSAGE);
        Exit;
      End;

      if not ProcessCmdLine() then begin
        Error(errStr);
      end;           

      for exportlanguage := Low(TExportLanguage) to High(TExportLanguage) do begin
        if (exportlanguage in exportLanguages) then begin
          parserModeKeep := parserMode;
          symtable := CreateSymboleTable(exportlanguage);
          try
            parserMode := pmAllTypes;
            if not GenerateSymbolTable(symtable) then
              Error(errStr);
            if not GenerateForLangage(symtable,exportlanguage) then
              Error(errStr);
          finally
            FreeAndNil(symtable);
            parserMode := parserModeKeep;
          end;
        end;
      end;

    finally
      SetLogger(nil);
    end;
  except
    on e:exception Do
      Writeln('Exception : ' + e.Message)
  end;
end.
