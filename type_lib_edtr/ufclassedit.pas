{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit ufclassedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ActnList,
  ExtCtrls, ComCtrls, Buttons, StdCtrls, Contnrs,
  pastree, pascal_parser_intf,
  edit_helper, Menus, SynEdit, SynHighlighterXML, view_helper;

type

  { TfClassEdit }

  TfClassEdit = class(TForm)
    actApply : TAction;
    actMoveDown: TAction;
    actMoveUp: TAction;
    actPropDelete: TAction;
    actPropEdit: TAction;
    actPropAdd: TAction;
    ActionList1: TActionList;
    actOK: TAction;
    actOK1: TAction;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6 : TButton;
    Button7: TButton;
    Button8: TButton;
    edtParent: TComboBox;
    edtName: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edtProp: TListView;
    Label2: TLabel;
    edtDocumentation : TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Panel1: TPanel;
    PC: TPageControl;
    PopupMenu1: TPopupMenu;
    edtSourceXSD : TSynEdit;
    SynXMLSyn1 : TSynXMLSyn;
    TabSheet1: TTabSheet;
    tsDocumentation : TTabSheet;
    tsDependencies : TTabSheet;
    tvDependency : TTreeView;
    tsSourceXSD : TTabSheet;
    procedure actApplyExecute(Sender : TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure actMoveDownUpdate(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveUpUpdate(Sender: TObject);
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
    procedure actPropAddExecute(Sender: TObject);
    procedure actPropDeleteExecute(Sender: TObject);
    procedure actPropEditExecute(Sender: TObject);
    procedure actPropEditUpdate(Sender: TObject);
    procedure edtPropDblClick(Sender: TObject);
    procedure PCChange(Sender : TObject);
  private
    FUpdateType : TEditType;
    FObject : TPasClassType;
    FSymbolTable : TwstPasTreeContainer;
    FOldAncestor : TPasType;
    FApplied : Boolean;
    FDependencyList : TObjectList;
  private
    property UpdateType : TEditType read FUpdateType;
  private
    procedure MovePropertyItem(AItem : TPasProperty; const ANewIndex : Integer);
    procedure PrepareParentCombo();
    function LoadProperty(APropDef : TPasProperty; const AIndex : Integer) : TListItem;
    procedure LoadFromObject();
    procedure SaveToObject();

    procedure ShowSourceXSD();
    procedure ShowDependencies();
    procedure ShowDocumentation();
  public
    destructor Destroy();override;
    function UpdateObject(
      var   AObject     : TPasClassType;
      const AUpdateType : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ):Boolean;
  end;

var
  fClassEdit: TfClassEdit;

implementation
uses
  parserutils, ufpropedit, common_gui_utils, xsd_consts;


{ TfClassEdit }

procedure TfClassEdit.actPropAddExecute(Sender: TObject);
var
  prp : TPasProperty;
begin
  prp := CreateProperty(FObject,FSymbolTable) as TPasProperty;
  if Assigned(prp) then begin
    LoadProperty(prp,-1);
  end;
end;

procedure TfClassEdit.actOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not IsStrEmpty(ExtractIdentifier(edtName.Text));
end;

procedure TfClassEdit.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfClassEdit.actApplyExecute(Sender : TObject);
begin
  SaveToObject();
  if ( PC.ActivePage = tsSourceXSD ) then
    ShowSourceXSD();
end;

procedure TfClassEdit.actMoveDownExecute(Sender: TObject);
begin
  MovePropertyItem(TPasProperty(edtProp.ItemFocused.Data),(edtProp.ItemFocused.Index + 1));
end;

procedure TfClassEdit.actMoveDownUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(edtProp.ItemFocused) and ( edtProp.ItemFocused.Index < Pred(edtProp.Items.Count) );
end;

procedure TfClassEdit.actMoveUpExecute(Sender: TObject);
begin
  MovePropertyItem(TPasProperty(edtProp.ItemFocused.Data),(edtProp.ItemFocused.Index - 1));
end;

procedure TfClassEdit.actMoveUpUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(edtProp.ItemFocused) and ( edtProp.ItemFocused.Index > 0 );
end;

procedure TfClassEdit.actPropDeleteExecute(Sender: TObject);
var
  prop : TPasProperty;
begin
  prop := TPasProperty(edtProp.ItemFocused.Data);
  FObject.Members.Extract(prop);
  prop.Release();
  edtProp.ItemFocused.Free();
end;

procedure TfClassEdit.actPropEditExecute(Sender: TObject);
var
  prp : TPasProperty;
  itm : TListItem;
  oldPos : Integer;
begin
  itm := edtProp.ItemFocused;
  if Assigned(itm) then begin
    prp := TPasProperty(itm.Data);
    if UpdateProperty(prp,FSymbolTable) then begin
      oldPos := itm.Index;
      itm.Free();
      LoadProperty(prp,oldPos);
    end;
  end;
end;

procedure TfClassEdit.actPropEditUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(edtProp.ItemFocused);
end;

procedure TfClassEdit.edtPropDblClick(Sender: TObject);
begin
  if actPropEdit.Enabled then begin
    actPropEdit.Execute();
  end else if actPropAdd.Enabled then begin
    actPropAdd.Execute();
  end;
end;

procedure TfClassEdit.PCChange(Sender : TObject);
begin
  if ( PC.ActivePage = tsSourceXSD ) then begin
    if actApply.Enabled then
      actApply.Execute();
    ShowSourceXSD();
  end else if ( PC.ActivePage = tsDependencies ) then begin
    ShowDependencies();
  end else if ( PC.ActivePage = tsDocumentation ) then begin
    ShowDocumentation();
  end;
end;

procedure TfClassEdit.MovePropertyItem(AItem: TPasProperty; const ANewIndex: Integer);

  function FindNewMemberPosition() : Integer;
  var
    k, kcounter : Integer;
    mlist : TList;
  begin
    Result := 0;
    kcounter := 0;
    mlist := FObject.Members;
    for k := 0 to Pred(mlist.Count) do begin
      if TPasElement(mlist[k]).InheritsFrom(TPasProperty) then begin
        Inc(kcounter);
        if ( kcounter = ANewIndex ) then begin
          Result := k;
          Break;
        end;
      end;
    end;
  end;

var
  locItem : TListItem;
begin
  if ( AItem <> nil ) and
     ( ( ANewIndex >= 0 ) and ( ANewIndex < edtProp.Items.Count ) )
  then begin
    locItem := FindItem(FSymbolTable.GetExternalName(AItem),edtProp.Items);
    if ( locItem <> nil ) then
      locItem.Free();
    FObject.Members.Exchange(FObject.Members.IndexOf(AItem),FindNewMemberPosition());
    locItem := LoadProperty(AItem,ANewIndex);
    edtProp.ItemFocused := locItem;
    edtProp.Selected := locItem;
    FApplied := True;
  end;
end;

procedure InternalFillList(
  ALs : TStrings;
  AContainer : TwstPasTreeContainer
);
var
  i, j : Integer;
  sym : TPasElement;
  modulList, decList : TList;
  mdl : TPasModule;
  ok : Boolean;
  locExtName : string;   
begin
  modulList := AContainer.Package.Modules;
  for i := 0 to Pred(modulList.Count) do begin
    mdl := TPasModule(modulList[i]);
    decList := mdl.InterfaceSection.Declarations;
    for j := 0 to Pred(decList.Count) do begin
      sym := TPasElement(decList[j]);
      if sym.InheritsFrom(TPasType) and
         ( sym.InheritsFrom(TPasClassType) or
           sym.InheritsFrom(TPasNativeSimpleContentClassType) or
           ( sym.InheritsFrom(TPasAliasType) and
             Assigned(TPasAliasType(sym).DestType) and
             ( TPasAliasType(sym).DestType.InheritsFrom(TPasClassType) or
               TPasAliasType(sym).DestType.InheritsFrom(TPasNativeSimpleType)
             )
           )
         )  and
        ( not sym.InheritsFrom(TPasNativeSimpleType) )
      then begin
        ok := ( not sym.InheritsFrom(TPasNativeClassType) ) or
              ( sym.InheritsFrom(TPasNativeSimpleContentClassType) or
                ( TPasNativeClassType(sym).ExtendableType = nil )
              );
        if ok and ( ALs.IndexOfObject(sym) = -1 ) then begin
          locExtName := AContainer.GetExternalName(sym);
          if ( locExtName <> sym.Name ) then
            locExtName := Format('%s - ( %s )',[AContainer.GetExternalName(sym), sym.Name]);
          if sym.InheritsFrom(TPasNativeSpecialSimpleType) or
             sym.InheritsFrom(TPasNativeSpecialSimpleContentClassType)
          then begin
            ALs.AddObject(locExtName,sym);
          end else begin
            ALs.AddObject(locExtName,sym);
          end;
        end;
      end;
    end;
  end;
end;

procedure FillList(
  ALs : TStrings;
  ASymbol : TwstPasTreeContainer
);
var
  locLST : TStringList;
begin
  locLST := TStringList.Create();
  try
    locLST.Assign(ALs);
    locLST.Duplicates := dupAccept;
    InternalFillList(locLST,ASymbol);
    locLST.Sort();
    ALs.Assign(locLST);
  finally
    FreeAndNil(locLST);
  end;
end;

procedure TfClassEdit.PrepareParentCombo();
var
  i : PtrInt;
begin
  edtParent.Items.BeginUpdate();
  try
    FillList(edtParent.Items,FSymbolTable);
    i := edtParent.Items.IndexOfObject(FObject);
    if ( i >= 0 ) then
      edtParent.Items.Delete(i);
  finally
    edtParent.Items.EndUpdate();
  end;
end;

function TfClassEdit.LoadProperty(APropDef: TPasProperty; const AIndex : Integer) : TListItem;
var
  itm : TListItem;
  s, extName : string;
begin
  extName := FSymbolTable.GetExternalName(APropDef);
  itm := FindItem(extName,edtProp.Items);
  if ( itm = nil ) then begin
    if ( AIndex >= 0 ) and ( AIndex < edtProp.Items.Count ) then
      itm := edtProp.Items.Insert(AIndex)
    else
      itm := edtProp.Items.Add();
  end;
  itm.Caption := extName;
  itm.SubItems.Add(FSymbolTable.GetExternalName(APropDef.VarType));
  if FSymbolTable.IsAttributeProperty(APropDef) then begin
    s := 'Y';
  end else begin
    s := 'N';
  end;
  itm.SubItems.Add(s);
  itm.Data := APropDef;
  Result := itm;
end;

procedure TfClassEdit.LoadFromObject();
var
  i : Integer;
  prp : TPasProperty;
  extName : string;
  ancestorType : TPasElement;
begin
  edtName.Text := '';
  edtProp.Clear();
  if Assigned(FObject) then begin
    extName := FSymbolTable.GetExternalName(FObject);
    Self.Caption := extName;
    edtName.Text := extName;
    for i := 0 to Pred(FObject.Members.Count) do begin
      if TPasElement(FObject.Members[i]).InheritsFrom(TPasProperty) then begin
        prp := TPasProperty(FObject.Members[i]);
        LoadProperty(prp,-1);
      end;
    end;
    if Assigned(FObject.AncestorType) then begin
      //edtParent.ItemIndex := edtParent.Items.IndexOfObject(FObject.AncestorType);
      ancestorType := FObject.AncestorType;
      if ancestorType.InheritsFrom(TPasUnresolvedTypeRef) then begin
        ancestorType := FSymbolTable.FindElement(FSymbolTable.GetExternalName(ancestorType));
      end;
      if Assigned(ancestorType) then begin
        if ancestorType.InheritsFrom(TPasNativeSimpleType) then
          ancestorType := TPasNativeSimpleType(ancestorType).ExtendableType
        else if ancestorType.InheritsFrom(TPasNativeClassType) and Assigned(TPasNativeClassType(ancestorType).ExtendableType) then
          ancestorType := TPasNativeClassType(ancestorType).ExtendableType;
      end;
      edtParent.ItemIndex := edtParent.Items.IndexOfObject(ancestorType);
    end;
  end else begin
    Self.Caption := 'New';
  end;
end;

procedure TfClassEdit.SaveToObject();
var
  typExtName, typIntName : string;
  locObj : TPasClassType;
  trueParent : TPasType;
begin
  locObj := nil;
  typExtName := ExtractIdentifier(edtName.Text);
  typIntName := MakeInternalSymbolNameFrom(typExtName);
  locObj := FObject;
  locObj.Name := typIntName;
  FSymbolTable.RegisterExternalAlias(locObj,typExtName);
  if ( edtParent.ItemIndex >= 0 ) then begin
    trueParent := edtParent.Items.Objects[edtParent.ItemIndex] as TPasType;
    if trueParent.InheritsFrom(TPasAliasType) then begin
      trueParent := GetUltimeType(trueParent);
    end;
    if trueParent.InheritsFrom(TPasNativeSimpleType) and
       Assigned(TPasNativeSimpleType(trueParent).ExtendableType)
    then begin
      trueParent := TPasNativeSimpleType(trueParent).ExtendableType;
    end;
  end else begin
    //trueParent := nil;
    trueParent := FSymbolTable.FindElementNS('TBaseComplexRemotable',sXSD_NS) as TPasType;
  end;
  if ( trueParent <> FOldAncestor ) then begin
    if ( FOldAncestor <> nil ) then
      FOldAncestor.Release();
    locObj.AncestorType := trueParent;
    if Assigned(locObj.AncestorType) then
      locObj.AncestorType.AddRef();
  end;
  FSymbolTable.Properties.GetList(FObject).Values[s_documentation] :=
    EncodeLineBreak(StringReplace(edtDocumentation.Lines.Text,sLineBreak,#10,[rfReplaceAll]));
  FOldAncestor := locObj.AncestorType;
  FApplied := True;
end;

procedure TfClassEdit.ShowSourceXSD();
begin
  edtSourceXSD.Lines.Text := XsdGenerateSourceForObject(FObject,FSymbolTable);
end;

procedure TfClassEdit.ShowDependencies();
begin
  if ( FDependencyList = nil ) then
    FDependencyList := TObjectList.Create(True);
  FDependencyList.Clear();
  FindDependencies(FObject,FSymbolTable,FDependencyList);
  DrawDependencies(tvDependency,FDependencyList);
end;

procedure TfClassEdit.ShowDocumentation();
var
  props : TStrings;
begin
  props := FSymbolTable.Properties.FindList(FObject);
  if ( props <> nil ) then
    edtDocumentation.Lines.Text :=
      StringReplace(DecodeLineBreak(props.Values[s_documentation]),#10,sLineBreak,[rfReplaceAll]);
end;

destructor TfClassEdit.Destroy();
begin
  FDependencyList.Free();
  inherited Destroy();
end;

function TfClassEdit.UpdateObject(
  var   AObject     : TPasClassType;
  const AUpdateType : TEditType;
        ASymbolTable : TwstPasTreeContainer
): Boolean;
begin
  Assert(Assigned(ASymbolTable));
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  if ( UpdateType = etCreate ) and ( FObject = nil ) then begin
    FObject := TPasClassType(FSymbolTable.CreateElement(TPasClassType,'new_class',FSymbolTable.CurrentModule.InterfaceSection,visDefault,'',0));
    FObject.ObjKind := okClass;
    FSymbolTable.CurrentModule.InterfaceSection.Declarations.Add(FObject);
    FSymbolTable.CurrentModule.InterfaceSection.Types.Add(FObject);
    FSymbolTable.CurrentModule.InterfaceSection.Classes.Add(FObject);
  end;
  FOldAncestor := FObject.AncestorType;
  try
    PrepareParentCombo();
    LoadFromObject();
    Result := FApplied or ( ShowModal() = mrOK );
    if Result then begin
      try
        SaveToObject();
        if ( AUpdateType = etCreate ) then begin
          AObject := FObject;
        end;
      except
        Result := False;
        raise;
      end;
    end;
  finally
    if ( not Result ) and ( UpdateType = etCreate ) and ( AObject = nil ) then begin
      FSymbolTable.CurrentModule.InterfaceSection.Declarations.Extract(FObject);
      FSymbolTable.CurrentModule.InterfaceSection.Types.Extract(FObject);
      FSymbolTable.CurrentModule.InterfaceSection.Classes.Extract(FObject);
      FObject.Release();
      FObject := nil;
    end;
  end;
end;

initialization
  {$I ufclassedit.lrs}

end.

