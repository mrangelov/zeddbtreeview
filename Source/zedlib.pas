{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit zedlib; 

interface

uses
  ZedDBTreeView, ZedTreeView, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ZedDBTreeView', @ZedDBTreeView.Register); 
end; 

initialization
  RegisterPackage('zedlib', @Register); 
end.
