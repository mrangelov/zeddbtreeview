unit formdlgcaption; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { TfrmDlgCaption }

  TfrmDlgCaption = class(TForm)
    bitbtnOK: TBitBtn;
    bitbtnCancel: TBitBtn;
    edtCaption: TEdit;
    pnlButtons: TPanel;
  private
    { private declarations }
  public
    { public declarations }

  end; 

var
  frmDlgCaption: TfrmDlgCaption;

implementation

{$R *.lfm}

end.

