unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation
USES UNIT2,UNIT3;
{$R *.lfm}

{ TForm1 }

procedure TForm1.Button4Click(Sender: TObject);     //renvoie au formulaire 2, les règles
begin
form2.show;
end;

procedure TForm1.Button1Click(Sender: TObject);      // renvoie au formulaire 1, la partie
begin
  form1.hide;
  form3.show;
end;

procedure TForm1.Button5Click(Sender: TObject);    // ferme l'application
begin
 application.terminate;
end;

end.

