program DDA_Demo1;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  Execute.DesktopDuplicationAPI in '..\lib\Execute.DesktopDuplicationAPI.pas',
  DX12.D3D11 in '..\DXHeaders\DX12.D3D11.pas',
  DX12.D3DCommon in '..\DXHeaders\DX12.D3DCommon.pas',
  DX12.DXGI in '..\DXHeaders\DX12.DXGI.pas',
  DX12.DXGI1_2 in '..\DXHeaders\DX12.DXGI1_2.pas',
  CommonTypes in '..\lib\CommonTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
