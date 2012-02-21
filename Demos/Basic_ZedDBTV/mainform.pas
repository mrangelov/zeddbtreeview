unit mainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DBGrids, StdCtrls, ZedDBTreeView, db, Sqlite3DS;

type

  TButtonType = (btAdd, btEdit, btDelete, btMove);

  { TfrmMain }

  TfrmMain = class(TForm)
    btnAdd: TButton;
    btnDelete: TButton;
    btnMove: TButton;
    btnEdit: TButton;
    btnApply: TButton;
    dscContacts: TDatasource;
    dbgrContacts: TDBGrid;
    pnlDBTree: TPanel;
    pnlDBGrid: TPanel;
    pnlFuncButtons: TPanel;
    pnlDescription: TPanel;
    sldsContacts: TSqlite3Dataset;
    sldsContactsID: TAutoIncField;
    sldsContactsName: TStringField;
    sldsContactsParentID: TLongintField;
    Splitter1: TSplitter;
    stxtDescription: TStaticText;
    zdbtvContacts: TZedDBTreeView;
    procedure btnAddClick(Sender: TObject);
    procedure btnAddMouseEnter(Sender: TObject);
    procedure btnAddMouseLeave(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDeleteMouseEnter(Sender: TObject);
    procedure btnDeleteMouseLeave(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnEditMouseEnter(Sender: TObject);
    procedure btnEditMouseLeave(Sender: TObject);
    procedure btnMoveClick(Sender: TObject);
    procedure btnMoveMouseEnter(Sender: TObject);
    procedure btnMoveMouseLeave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure zdbtvContactsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure zdbtvContactsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure zdbtvContactsMouseEnter(Sender: TObject);
    procedure zdbtvContactsMouseLeave(Sender: TObject);
  private
    { private declarations }
    procedure DataInitialize;
    procedure ShowHint(aType: TButtonType);
    procedure ClearHint;

  public
    { public declarations }
  end; 

const
  DBFileName = 'contacts.sqb';

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  formdlgcaption, ZedTreeView, ComCtrls;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DataInitialize;
  ShowHint(TButtonType(-1));
end;

// Shows how to move a node when it is draged and droped onto another node
procedure TfrmMain.zdbtvContactsDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  TargetNode, SourceNode : TZedTreeNode;
begin
  with zdbtvContacts do
  begin
    TargetNode := GetNodeAt(X,Y) as TZedTreeNode;
    SourceNode := Selected;
    if (TargetNode = nil) then Exit;
    SourceNode.MoveTo(TargetNode,naAddChild);
  end;
end;

procedure TfrmMain.zdbtvContactsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (Sender = zdbtvContacts) then
     begin
       Accept := True;
     end;
end;

procedure TfrmMain.zdbtvContactsMouseEnter(Sender: TObject);
begin
  ShowHint(TButtonType(-1));
end;

procedure TfrmMain.zdbtvContactsMouseLeave(Sender: TObject);
begin
  ClearHint;
end;

// Shows how to add a new node, if there is selected node, it is added as child
// otherwise it is added as root node.
procedure TfrmMain.btnAddClick(Sender: TObject);
var
  nodeCaption : String;
begin
  nodeCaption := '';
  frmDlgCaption.Caption := 'Enter a name';
  frmDlgCaption.edtCaption.Text := '';
  frmDlgCaption.ShowModal;
  if frmDlgCaption.ModalResult = mrOK then
     begin
       nodeCaption := frmDlgCaption.edtCaption.Text;

       if Assigned(zdbtvContacts.Selected) then
          begin
            zdbtvContacts.Items.AddNewChild(zdbtvContacts.Selected,nodeCaption);
          end
       else zdbtvContacts.Items.AddNewChild(nil,nodeCaption);
     end;
end;

procedure TfrmMain.btnAddMouseEnter(Sender: TObject);
begin
  ShowHint(btAdd);
end;

procedure TfrmMain.btnAddMouseLeave(Sender: TObject);
begin
  ClearHint;
end;

procedure TfrmMain.btnApplyClick(Sender: TObject);
begin
  sldsContacts.ApplyUpdates;
end;

// Deletes selected node and its child nodes.
procedure TfrmMain.btnDeleteClick(Sender: TObject);
begin
  if Assigned(zdbtvContacts.Selected) then
     begin
       zdbtvContacts.Selected.Delete;
     end;
end;

procedure TfrmMain.btnDeleteMouseEnter(Sender: TObject);
begin
  ShowHint(btDelete);
end;

procedure TfrmMain.btnDeleteMouseLeave(Sender: TObject);
begin
  ClearHint;
end;

// Edits the text of slelected node.
procedure TfrmMain.btnEditClick(Sender: TObject);
begin
  if Assigned(zdbtvContacts.Selected) then
     begin
       frmDlgCaption.Caption := 'Edit the name';
       frmDlgCaption.edtCaption.Text := zdbtvContacts.Selected.Text;

       frmDlgCaption.ShowModal;
       if frmDlgCaption.ModalResult = mrOK then
          begin
            zdbtvContacts.DataSource.DataSet.Edit;
            zdbtvContacts.DataSource.DataSet.FieldByName('Name').AsString :=
              frmDlgCaption.edtCaption.Text;
            zdbtvContacts.DataSource.DataSet.Post;
          end;
     end
  else ShowMessage('You should select a node first!');
end;

procedure TfrmMain.btnEditMouseEnter(Sender: TObject);
begin
  ShowHint(btEdit);
end;

procedure TfrmMain.btnEditMouseLeave(Sender: TObject);
begin
  ClearHint;
end;

// Shows how to move a node by changing its parent ID key.
procedure TfrmMain.btnMoveClick(Sender: TObject);
var
  ParentID, Code : Integer;
begin
  if Assigned(zdbtvContacts.Selected) then
     begin
       frmDlgCaption.Caption := 'Enter Parent ID';
       frmDlgCaption.edtCaption.Text := '';

       frmDlgCaption.ShowModal;
       if frmDlgCaption.ModalResult = mrOK then
          begin
            val(frmDlgCaption.edtCaption.Text, ParentID, Code);
            if Code = 0 then
               begin
                 zdbtvContacts.DataSource.DataSet.Edit;
                 zdbtvContacts.DataSource.DataSet.FieldByName('ParentID').AsInteger := ParentID;
               end
            else ShowMessage('Invalid number!');
          end;
     end;
end;

procedure TfrmMain.btnMoveMouseEnter(Sender: TObject);
begin
  ShowHint(btMove);
end;

procedure TfrmMain.btnMoveMouseLeave(Sender: TObject);
begin
  ClearHint;
end;

procedure TfrmMain.DataInitialize;
var
  ExePath : String;
begin
  ExePath := ExtractFilePath(Application.ExeName);
  sldsContacts.FileName := ExePath + DBFileName;
  sldsContacts.Active := true;
end;

procedure TfrmMain.ShowHint(aType: TButtonType);
begin
  case aType of
    btAdd : stxtDescription.Caption := 'You can add a new node by: adding it to ' +
                                       ' the tree specifying its caption or adding a new record in dataset';
    btEdit : stxtDescription.Caption := 'You can edit caption of node by directly doble clicking on it ' +
                                        'or edit corresponding data field';
    btDelete : stxtDescription.Caption := 'You can delete a node by removing its record or by calling its delete method';
    btMove : stxtDescription.Caption := 'You can move node by drag and drop it or change ' +
                                        'its parent ID key into corresponding data field';
    else stxtDescription.Caption := 'TZedDBTreeView is Data Aware TreeView. It is designed to load ' +
                                      'self-referenced hierarchical data sets. It is fast and loads all the data ' +
                                      'when it is built. There''s no need to rebuild the tree for every ' +
                                      'modification. Data''s nature independent, so you can connect it to database ' +
                                      'on your choice. Supports browsing and modifications both ways ' +
                                      '(Tree -> Dataset, dataset -> Tree).'
  end;
end;

procedure TfrmMain.ClearHint;
begin
  stxtDescription.Caption := '';
end;

end.

