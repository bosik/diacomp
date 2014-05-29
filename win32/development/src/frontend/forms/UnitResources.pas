unit UnitResources;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls;

type
  TFormResources = class(TForm)
    ListRes: TListView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormResources: TFormResources;

implementation

{$R *.dfm}

end.
