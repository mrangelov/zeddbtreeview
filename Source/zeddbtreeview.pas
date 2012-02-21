(******************************************************************************
 * ZedDBTreeView                                                              *
 *                                                                            *
 * Author: Momchil Rangelov                                                   *
 *                                                                            *
 * You can report bugs at ms_rangelov@hotmail.com                             *
 *                                                                            *
 * This source is free software; you can redistribute it and/or modify it     *
 * under the terms of the GNU General Public License as published by the      *
 * Free Software Foundation at <http://www.gnu.org/copyleft/gpl.html>         *
 *                                                                            *
 * This code is distributed in the hope that it will be useful, but           *
 * WITHOUT ANY WARRANTY; without even the implied warranty of                 *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                       *
 *                                                                            *
 * Abstract:                                                                  *
 * TZedDBTreeView is Data Aware TreeView. It is designed to load              *
 * self-referenced hierarchical data sets. It is fast and loads all the data  *
 * when it is built. There's no need to rebuild the tree for every            *
 * modification. Data's nature independent, so you can connect it to database *
 * on your choice. Supports browsing and modifications both ways              *
 * (Tree -> Dataset, dataset -> Tree).                                        *
 ******************************************************************************)

unit ZedDBTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZedTreeView, DB, DBGrids, DBCtrls, LResources, PropEdits,
  DBPropEdits, Controls;
  
type
  TTreeState = set of (tsSelecting, tsDeleting, tsBrowse, tsMovingNode, tsChangingNode,
                       tsRefreshNodes, tsChangingParent, tsDestroying, tsInternalAdd);
  
type

  { TZedDBTreeView }

  TZedDBTreeView = class(TZedTreeView)

    private
      FIDField: string;
      FTextDataLink : TFieldDataLink;
      FParentDataLink : TFieldDataLink;
      FUpdating : Boolean;
      FDeleting : Boolean;
      FNodes : TList;
      FState : TTreeState;
      FPreviousDataSetState : TDataSetState;
      FPrevSelectedID : Integer;
      FHasBeenDrawn: Boolean;
      FGridSelectedRows: TBookmarkList;
      function GetDataSource: TDataSource;
      function GetTextField: string;
      function GetParentField: string;
      procedure SetDataSource(const AValue: TDataSource);
      procedure SetGridSelectedRows(const AValue: TBookmarkList);
      procedure SetIDField(const AValue: string);
      procedure SetTextField(const AValue: string);
      procedure SetParentField(const AValue: string);
      procedure BuildTree;
      procedure DataChange(Sender: TObject);
      procedure EditingChange(Sender: TObject);
      procedure UpdateData(Sender: TObject);
      procedure ParentDataChange(Sender: TObject);
      procedure DeleteFromDB(Node : TZedTreeNode);
      procedure ActiveChange(Sender: TObject);
      procedure RefreshNodes;
      function HasToProceedTree : Boolean;
      
    protected
      function GetParentIndex(aParentID : Integer) : Integer;
      procedure Loaded; override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure Added(Node: TZedTreeNode); override;
      procedure Edit(aText: String); override;
      procedure Change(Node: TZedTreeNode); override;
      procedure Delete(Node: TZedTreeNode); override;
      procedure Moved(aDestNode, aSourceNode : TZedTreeNode); override;
      procedure StartMoving(aDestNode, aSourceNode : TZedTreeNode); override;
      procedure NodeAfterPost;
      procedure CheckDelete;
      procedure CheckAppend;
      function DataSetAvailable : Boolean;
      procedure SetParent(AParent: TWinControl); override;
      procedure DoPaint; override;
      procedure SetDestroyingState; override;
      procedure ClearDestroyingState; override;
      procedure SetInternalAdd; override;
      procedure ClearInternalAdd; override;
      procedure BeginReload;
      procedure EndReload;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function GetNodeByID(ID : Integer) : TZedTreeNode;
      procedure SelectNodeByID(ID : Integer);
      procedure Rebuild;
      procedure BeginUpdate;
      procedure EndUpdate;
      procedure RemoveBookmark(aNode : TZedTreeNode);
      procedure SetBookmark(aNode : TZedTreeNode);
      property GridSelectedRows : TBookmarkList read FGridSelectedRows write SetGridSelectedRows;
      
    published
      property DataSource : TDataSource read GetDataSource write SetDataSource;
      property DBKeyField : string read FIDField write SetIDField;
      property DBTextField : string read GetTextField write SetTextField;
      property DBParentField : string read GetParentField write SetParentField;
      
  end;

  procedure Register;
  
implementation

uses ComCtrls, Dialogs;

(******************************************************************************
* Registers the component itself.
*******************************************************************************)
procedure Register;
begin
  RegisterComponents('ZedLib', [TZedDBTreeView]);

  RegisterPropertyEditor(TypeInfo(string), TZedDBTreeView, 'DBTextField', TFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TZedDBTreeView, 'DBKeyField', TFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TZedDBTreeView, 'DBParentField', TFieldProperty);
end;

{ TZedDBTreeView }

(******************************************************************************
* Builds the tree by iterating through all dataset records.
*******************************************************************************)
procedure TZedDBTreeView.BuildTree;
var
  ParentIndex, i : Integer;
  Node, NewNode : TZedTreeNode;
begin
  if HasToProceedTree then
    begin
      Self.BeginUpdate;
      try
        Self.Items.Clear;
        FTextDataLink.DataSet.Close;
        FTextDataLink.DataSet.Open;
        FTextDataLink.DataSet.First;
        FNodes := TList.Create;
        for i:= 1 to FTextDataLink.DataSet.RecordCount do
         begin
           if ( FTextDataLink.DataSet.FieldByName(FParentDataLink.FieldName).IsNull ) then
             begin
               NewNode := Self.Items.AddNewChild(nil, FTextDataLink.Field.AsString);
               NewNode.ID := FTextDataLink.DataSet.FieldByName(FIDField).AsInteger;
               NewNode.ParentID := FParentDataLink.DataSet.FieldByName(GetParentField).AsInteger;
             end
          else
            begin
              ParentIndex:= GetParentIndex(FTextDataLink.DataSet.FieldByName(FParentDataLink.FieldName).AsInteger);
              if ParentIndex > -1 then
                begin
                  Node := Self.Items[ParentIndex];
                  NewNode := Self.Items.AddNewChild(Node, FTextDataLink.Field.AsString);
                  NewNode.ID := FTextDataLink.DataSet.FieldByName(FIDField).AsInteger;
                  NewNode.ParentID := FParentDataLink.DataSet.FieldByName(GetParentField).AsInteger;
                end
              else
                begin
                  NewNode := Self.Items.AddNewChild(nil, FTextDataLink.Field.AsString);
                  NewNode.ID := FTextDataLink.DataSet.FieldByName(FIDField).AsInteger;
                  NewNode.ParentID := FParentDataLink.DataSet.FieldByName(GetParentField).AsInteger;
                  FNodes.Add(NewNode);
                end;
            end;
          FTextDataLink.DataSet.Next;
        end;
        RefreshNodes;
        FNodes.Free;
      finally
        FTextDataLink.DataSet.First;
        Self.EndUpdate;
      end;
    end;
end;

(******************************************************************************
* Responds on data changes and if necessary rebuilds the tree.
*******************************************************************************)
procedure TZedDBTreeView.DataChange(Sender: TObject);
begin
  if (DatasetAvailable) and (not Self.FUpdating) then
    begin
      CheckDelete;
      NodeAfterPost;
      CheckAppend;
      if (Self.FIDField <> EmptyStr) and (not FDeleting) and
         (not (tsMovingNode in FState)) then
        SelectNodeByID(Self.FTextDataLink.DataSet.FieldByName(Self.FIDField).AsInteger);
      if (Self.FTextDataLink.BOF) and  (not (csDesigning in self.ComponentState)) then
        BuildTree;
  end;
end;

(******************************************************************************
* Returns the DataSource of the tree.
*******************************************************************************)
function TZedDBTreeView.GetDataSource: TDataSource;
begin
  Result:= FTextDataLink.DataSource;
end;

(******************************************************************************
* Returns the parent field name for the tree which contains key column for
* parent nodes.
*******************************************************************************)
function TZedDBTreeView.GetParentField: string;
begin
  Result:= FParentDataLink.FieldName;
end;

(******************************************************************************
* Returns the text field name for the tree which contains column used to show
* nodes captions.
*******************************************************************************)
function TZedDBTreeView.GetTextField: string;
begin
  Result:= FTextDataLink.FieldName;
end;

(******************************************************************************
* Sets a DataSource for the tree and builds it if DataSource is active.
*******************************************************************************)
procedure TZedDBTreeView.SetDataSource(const AValue: TDataSource);
begin
  if AValue = FTextDataLink.Datasource then Exit;
  if not (FTextDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    begin
      FTextDataLink.DataSource := AValue;
      FParentDataLink.DataSource := AValue;
    end;
  BuildTree;
  if AValue <> nil then AValue.FreeNotification(Self);
end;

(******************************************************************************
* Saves a bookmark.
*******************************************************************************)
procedure TZedDBTreeView.SetGridSelectedRows(const AValue: TBookmarkList);
begin
  if FGridSelectedRows=AValue then exit;
  FGridSelectedRows:=AValue;
end;

(******************************************************************************
* Sets a name for DBIDField property of the tree which is used to identify
* the record attached to the node.
*******************************************************************************)
procedure TZedDBTreeView.SetIDField(const AValue: string);
begin
  if FIDField <> AValue then FIDField := AValue;
  BuildTree;
end;

(******************************************************************************
* Sets a name for DBParentField property of the tree which is used for key of
* the parent node.
*******************************************************************************)
procedure TZedDBTreeView.SetParentField(const AValue: string);
begin
  if AValue <> FParentDataLink.FieldName then
     FParentDataLink.FieldName := AValue;
  BuildTree;
end;

(******************************************************************************
* Sets a name for DBTextField property of the tree which is used for the
* caption of the node.
*******************************************************************************)
procedure TZedDBTreeView.SetTextField(const AValue: string);
begin
  if AValue <> FTextDataLink.FieldName then
     FTextDataLink.FieldName:= AValue;
  BuildTree;
end;

(******************************************************************************
* Refreshes the nodes if there are changes of the keys.
*******************************************************************************)
procedure TZedDBTreeView.RefreshNodes;
var
  i : Integer;
  ParentNode : TZedTreeNode;
begin
  for i := 0 to FNodes.Count - 1 do
    begin
      ParentNode := GetNodeByID(TZedTreeNode(FNodes[i]).ParentID);
      if Assigned(ParentNode) and (ParentNode <> TZedTreeNode(FNodes[i])) then
        begin
          FState := FState + [tsRefreshNodes];
          TZedTreeNode(FNodes[i]).MoveTo(ParentNode, naAddChild);
          FState := FState - [tsRefreshNodes];
        end
    end;
end;

(******************************************************************************
* Processes editing event.
*******************************************************************************)
procedure TZedDBTreeView.EditingChange(Sender: TObject);
begin
  if DatasetAvailable and (FTextDataLink.DataSet.State in [dsBrowse]) and (not Self.FUpdating) then
    begin
      if FTextDataLink.Field.IsNull and (FPreviousDataSetState in [dsEdit, dsInsert]) then
           FPreviousDataSetState:= dsBrowse
    end;
end;

(******************************************************************************
* Processes update event.
*******************************************************************************)
procedure TZedDBTreeView.UpdateData(Sender: TObject);
begin
  Self.Selected.Text := Self.FTextDataLink.Field.AsString;
end;

(******************************************************************************
* Puts the node at proper place if parent key is changed.
*******************************************************************************)
procedure TZedDBTreeView.ParentDataChange(Sender: TObject);
var
  Node, ParentNode : TZedTreeNode;
  ParentID : Integer;
begin
  if DatasetAvailable and not (tsMovingNode in FState) then
    if Assigned(FParentDataLink.DataSet) and (FParentDataLink.DataSet.Modified) and (not Self.FUpdating) then
      begin
        if not FParentDataLink.Field.IsNull then
          begin
            Node := GetNodeByID(FTextDataLink.DataSet.FieldByName(FIDField).AsInteger);
            if Assigned(Node) then
              begin
                ParentNode := GetNodeByID(FParentDataLink.DataSet.FieldByName(GetParentField).AsInteger);
                ParentID := -1;
                if Assigned(ParentNode) then ParentID := ParentNode.ID;
                if  Node.ParentID <> ParentID then
                  if Assigned(ParentNode) then Node.MoveTo(ParentNode, naAddChild)
                  else Node.MoveTo(nil, naAdd);
              end;
          end;
      end;
end;

(******************************************************************************
* Removes record from dataset which is deleted from database.
*******************************************************************************)
procedure TZedDBTreeView.DeleteFromDB(Node: TZedTreeNode);
begin
  if DatasetAvailable and Node.Deleting and (not Node.DeletedFromDB) then
    begin
      if FTextDataLink.DataSet.Locate(FIDField, Node.ID, []) then
        FTextDataLink.DataSet.Delete;
    end;
end;

(******************************************************************************
* Processes changes of DataSet status, if it is not active clears the tree,
* when it becomes active rebuilds the tree.
*******************************************************************************)
procedure TZedDBTreeView.ActiveChange(Sender: TObject);
begin
  if DataSetAvailable then BuildTree
  else Self.Items.Clear;
end;

(******************************************************************************
* Checks status of DataSet and returns true if it is present and active.
*******************************************************************************)
function TZedDBTreeView.DataSetAvailable: Boolean;
begin
  Result:= Assigned(Self.FTextDataLink.DataSet) and Assigned(Self.FParentDataLink.DataSet) and
           Self.FTextDataLink.DataSet.Active and (FIDField <> EmptyStr);
end;

(******************************************************************************
* Changes control parent.
*******************************************************************************)
procedure TZedDBTreeView.SetParent(AParent: TWinControl);
begin
  FState := FState + [tsChangingParent];
  inherited SetParent(AParent);
  FState := FState - [tsChangingParent];
end;

(******************************************************************************
* Returns index of parent node by given parent ID.
*******************************************************************************)
function TZedDBTreeView.GetParentIndex(aParentID: Integer): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to Items.Count - 1 do
    if Items[i].ID = aParentID then
      begin
        Result := i;
        Break;
      end;
end;

(******************************************************************************
* Builds the tree when component is created in design mode.
*******************************************************************************)
procedure TZedDBTreeView.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

(******************************************************************************
* Processes removing of DataSource.
*******************************************************************************)
procedure TZedDBTreeView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FTextDataLink <> nil)
     and (AComponent = DataSource) then DataSource := nil;
end;

(******************************************************************************
* Processes adding of a new node to the tree.
*******************************************************************************)
procedure TZedDBTreeView.Added(Node: TZedTreeNode);
begin
  if DatasetAvailable and (not Self.FUpdating) and (not (tsChangingParent in FState))
    and (not (tsInternalAdd in FState)) then
    begin
      Self.BeginUpdate;
      try
        Self.FTextDataLink.DataSet.Insert;
        Self.FTextDataLink.Field.AsString:= Node.Text;
        if Assigned(Node.Parent) then
          begin
            Self.FTextDataLink.DataSet.FieldByName(FParentDataLink.FieldName).AsInteger := Node.Parent.ID;
            Node.ParentID := Node.Parent.ID;
          end
        else
          begin
            Self.FTextDataLink.DataSet.FieldByName(FParentDataLink.FieldName).AsInteger := -1;
            Node.ParentID := -1;
          end;
        Self.FTextDataLink.DataSet.Post;
        Node.ID := Self.FTextDataLink.DataSet.FieldByName(FIDField).AsInteger;
        Self.Selected:= Node;
      finally
        Self.EndUpdate;
      end;
    end;
  inherited Added(Node);
end;

(******************************************************************************
* Processes changing of selected node.
*******************************************************************************)
procedure TZedDBTreeView.Change(Node: TZedTreeNode);
begin
  if DatasetAvailable and (not (tsSelecting in FState)) and (not (tsMovingNode in FState)) then
    begin
      if Assigned(Node) then
        begin
          FState := FState + [tsChangingNode];
          if (FTextDataLink.DataSet.RecordCount > 0) and FTextDataLink.DataSet.Locate(FIDField, Node.ID, []) then
            begin
              if self.RowSelect then Node.DBBookmark := FTextDataLink.DataSet.GetBookmark;
              if Assigned(GridSelectedRows) then GridSelectedRows.CurrentRowSelected := true;
            end;
          FState := FState - [tsChangingNode];
        end
    end;
  inherited Change(Node);
end;

(******************************************************************************
* Processes deleting of given node.
*******************************************************************************)
procedure TZedDBTreeView.Delete(Node: TZedTreeNode);
begin
  FDeleting := Node.Deleting;
  if not ((csDestroying in Node.TreeView.ComponentState) or
          (csDesigning in Node.TreeView.ComponentState) or
          (tsDestroying in FState) or
          Self.FUpdating or (tsChangingParent in FState)) then begin
    DeleteFromDB(Node);
  end;
  FDeleting := false;
  inherited Delete(Node);
end;

(******************************************************************************
* After node is moved DataSet needs to be updated.
*******************************************************************************)
procedure TZedDBTreeView.Moved(aDestNode, aSourceNode: TZedTreeNode);
begin
  if DatasetAvailable and (not (tsRefreshNodes in FState)) then begin
    if (not Assigned(aDestNode)) or (not aDestNode.HasAsParent(aSourceNode)) then begin
     Self.FParentDataLink.DataSet.Edit;
     if Assigned(aSourceNode.Parent) then
       begin
         Self.FParentDataLink.Field.AsInteger := aSourceNode.Parent.ID;
         aSourceNode.ParentID := aSourceNode.Parent.ID;
       end
     else
       begin
         Self.FParentDataLink.Field.AsVariant := -1;
         aSourceNode.ParentID := -1;
       end;
    end
    else Self.FParentDataLink.DataSet.Cancel;
  end;
  FState := FState - [tsMovingNode];
  inherited Moved(aDestNode, aSourceNode);
  if not (tsRefreshNodes in FState) then
    Self.EndUpdate;
end;

(******************************************************************************
* Does some preparations for moving a node.
*******************************************************************************)
procedure TZedDBTreeView.StartMoving(aDestNode, aSourceNode: TZedTreeNode);
begin
  FState := FState + [tsMovingNode];
  if not (tsRefreshNodes in FState) then Self.BeginUpdate;
  inherited StartMoving(aDestNode, aSourceNode);
end;

(******************************************************************************
* Synchronize the tree with database after deleting a row directly from DB.
*******************************************************************************)
procedure TZedDBTreeView.CheckDelete;
var
  Node : TZedTreeNode;
begin
  if (DatasetAvailable) and ((Self.Items.Count - FTextDataLink.DataSet.RecordCount) = 1) and (not FDeleting) then
    begin
      Node:= GetNodeByID(FPrevSelectedID);
      if Assigned(Node) then
        begin
          Node.DeletedFromDB:= true;
          Node.Delete;
        end;
    end;
  if DatasetAvailable and not(tsMovingNode in FState) then
    FPrevSelectedID := FTextDataLink.DataSet.FieldByName(FIDField).AsInteger;
end;

(******************************************************************************
* Synchronize the tree with database after adding a row directly into DB.
*******************************************************************************)
procedure TZedDBTreeView.CheckAppend;
var
  Node : TZedTreeNode;
begin
  if (DatasetAvailable) and ((FTextDataLink.DataSet.RecordCount - Self.Items.Count) = 1) then
    begin
      Self.BeginUpdate;
      try
        Node := Self.Items.AddNewChild(GetNodeByID(FParentDataLink.Field.AsInteger), FTextDataLink.Field.AsString);
        Node.ID := FTextDataLink.DataSet.FieldByName(FIDField).AsInteger;
        Node.ParentID := FParentDataLink.Field.AsInteger;
      finally
        Self.EndUpdate;
      end;
    end;
end;

(******************************************************************************
* Processes after post event and if new record is inserted adds a new node to
* the tree or if value of text field is changed refreshes selected node.
*******************************************************************************)
procedure TZedDBTreeView.NodeAfterPost;
var
  Node: TZedTreeNode;
begin
  if DatasetAvailable then begin
     if (FTextDataLink.DataSet.State in [dsBrowse]) and (not Self.FUpdating)
        and ( not (tsMovingNode in FState) ) then
       begin
         if (FPreviousDataSetState in [dsEdit]) and not FTextDataLink.Field.IsNull then
           begin
             Self.Selected.Text := FTextDataLink.Field.AsString
           end
         else
           if (FPreviousDataSetState in [dsInsert])and not FTextDataLink.Field.IsNull then
             begin
               Self.BeginUpdate;
               try
                  Node := Self.Items.AddNewChild(GetNodeByID(FParentDataLink.Field.AsInteger), FTextDataLink.Field.AsString);
                  Node.ID := FTextDataLink.DataSet.FieldByName(FIDField).AsInteger;
                  Node.ParentID := FParentDataLink.Field.AsInteger;
               finally Self.EndUpdate; end;
             end
       end;
     FPreviousDataSetState:= FTextDataLink.DataSet.State;
  end;
end;

procedure TZedDBTreeView.DoPaint;
begin
  FHasBeenDrawn:= True;
  inherited DoPaint;
end;

procedure TZedDBTreeView.SetDestroyingState;
begin
  Include(FState, tsDestroying);
end;

procedure TZedDBTreeView.ClearDestroyingState;
begin
  Exclude(FState, tsDestroying);
end;

procedure TZedDBTreeView.SetInternalAdd;
begin
  Include(FState, tsInternalAdd);
end;

procedure TZedDBTreeView.ClearInternalAdd;
begin
  Exclude(FState, tsInternalAdd);
end;

(******************************************************************************
* Disables showing of data till the tree is being reloaded.
*******************************************************************************)
procedure TZedDBTreeView.BeginReload;
begin
  if Self.FHasBeenDrawn then
    FTextDataLink.DataSet.DisableControls;
  if Self.FHasBeenDrawn then
    FParentDataLink.DataSet.DisableControls;
  Self.Items.BeginUpdate;
end;

(******************************************************************************
* Enables showing of data after the tree is reloaded.
*******************************************************************************)
procedure TZedDBTreeView.EndReload;
begin
  if FTextDataLink.DataSet.ControlsDisabled then
    FTextDataLink.DataSet.EnableControls;
  if FParentDataLink.DataSet.ControlsDisabled then
    FParentDataLink.DataSet.EnableControls;
  Self.Items.EndUpdate;
end;

(******************************************************************************
* Checks if the tree is initialized and data manipulations can be made on it.
*******************************************************************************)
function TZedDBTreeView.HasToProceedTree: Boolean;
begin
  Result := true;
  if FTextDataLink.DataSource = nil then Result := false;
  if FParentDataLink.DataSource = nil then Result := false;
  if FParentDataLink.FieldName = EmptyStr then Result := false;
  if FIDField = EmptyStr then Result := false;
  if FTextDataLink.FieldName = EmptyStr then Result := false;

  if Self.FUpdating then Result := false;
  if not DatasetAvailable then Result := false;
end;

(******************************************************************************
* Constructor: initializes the tree and required events.
*******************************************************************************)
constructor TZedDBTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HideSelection:= False;
  FHasBeenDrawn:= False;
  FTextDataLink := TFieldDataLink.Create;
  FTextDataLink.Control := Self;
  FTextDataLink.OnDataChange := @DataChange;
  FTextDataLink.OnEditingChange := @EditingChange;
  FTextDataLink.OnUpdateData := @UpdateData;
  FTextDataLink.OnActiveChange := @ActiveChange;
  FParentDataLink := TFieldDataLink.Create;
  FParentDataLink.Control := Self;
  FParentDataLink.OnDataChange := @ParentDataChange;
  FState := [tsBrowse];
end;

(******************************************************************************
* Destructor: Frees data resources.
*******************************************************************************)
destructor TZedDBTreeView.Destroy;
begin
  DataSource:= nil;
  FTextDataLink.Free;
  FTextDataLink := nil;
  FParentDataLink.Free;
  FParentDataLink := nil;
  inherited Destroy;
end;

(******************************************************************************
* Finds and returns a node by given ID.
*******************************************************************************)
function TZedDBTreeView.GetNodeByID(ID: Integer): TZedTreeNode;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to Items.Count - 1 do
    if Items[i].ID = ID then
      begin
        Result := Items[i];
        Break;
      end;
end;

(******************************************************************************
* Selects a node in the tree by given ID.
*******************************************************************************)
procedure TZedDBTreeView.SelectNodeByID(ID: Integer);
var
  Index : Integer;
begin
  if (not (tsMovingNode in FState)) and (not (tsChangingNode in FState)) then
    begin
      Index := Self.Items.IndexOf(ID);
      if Index > -1 then
        begin
          FState := FState + [tsSelecting];
          Self.Items[Index].Selected := true;
          FState := FState - [tsSelecting];
        end;
    end;
end;

(******************************************************************************
* Rebuilds the tree.
*******************************************************************************)
procedure TZedDBTreeView.Rebuild;
begin
  Self.BeginReload;
  BuildTree;
  Self.EndReload;
end;

(******************************************************************************
* Marks beginning of updating the tree and disables showing of data.
*******************************************************************************)
procedure TZedDBTreeView.BeginUpdate;
begin
  if Self.FHasBeenDrawn then
    FTextDataLink.DataSet.DisableControls;
  if Self.FHasBeenDrawn then
    FParentDataLink.DataSet.DisableControls;
  Self.Items.BeginUpdate;
  FUpdating:= True;
end;

(******************************************************************************
* Marks ending of updating the tree and enables showing of data.
*******************************************************************************)
procedure TZedDBTreeView.EndUpdate;
begin
  if FTextDataLink.DataSet.ControlsDisabled then
    FTextDataLink.DataSet.EnableControls;
  if FParentDataLink.DataSet.ControlsDisabled then
    FParentDataLink.DataSet.EnableControls;
  Self.Items.EndUpdate;
  FUpdating:= False;
end;

(******************************************************************************
* Sets new value of data text field for given node when its caption is edited.
*******************************************************************************)
procedure TZedDBTreeView.Edit(aText: String);
var
  tmpStr : String;
begin
  if DataSetAvailable then
    begin
      Self.FTextDataLink.DataSet.Edit;
      Self.FTextDataLink.Field.AsString:= aText;
      if Self.FTextDataLink.DataSet.State in [dsEdit] then
        Self.FTextDataLink.DataSet.Post;
    end;
end;

(******************************************************************************
* Deletes a bookmark for given node.
*******************************************************************************)
procedure TZedDBTreeView.RemoveBookmark(aNode: TZedTreeNode);
begin
  if DatasetAvailable then
    begin
      FTextDataLink.DataSet.FreeBookmark(aNode.DBBookmark);
      aNode.DBBookmark := nil;
    end;
end;

(******************************************************************************
* Sets a bookmark for given node.
*******************************************************************************)
procedure TZedDBTreeView.SetBookmark(aNode: TZedTreeNode);
begin
  if DatasetAvailable then
    begin
      aNode.DBBookmark := FTextDataLink.DataSet.GetBookmark;
    end;
end;

initialization
  {$I zeddbtreeview.lrs}

end.

