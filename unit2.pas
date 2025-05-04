unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    Memo1: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  Form2: TForm2;

implementation
USES UNIT1,UNIT3;
{$R *.lfm}

{ TForm2 }


procedure TForm2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
form2.hide;
end;


end.

