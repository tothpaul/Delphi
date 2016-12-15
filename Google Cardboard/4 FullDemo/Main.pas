unit Main;

{
   Let's have Real 3D with Delphi Berlin

   (c)2016 Paul TOTH, Execute SARL
   http://www.execute.fr

   https://www.barnsten.com/fr/blog/firemonkey.html

}
interface

uses
{$IFDEF ANDROID}
  Androidapi.Sensor,
  Androidapi.Looper,
  Androidapi.AppGlue,
{$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Sensors,
  System.Sensors.Components, System.Math.Vectors, FMX.Types3D, System.Math,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects3D, FMX.Controls3D,
  FMX.MaterialSources, FMX.Viewport3D,

  Execute.StereoViewPort;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    Cube1: TCube;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    Grid3D1: TGrid3D;
    Timer1: TTimer;
    cbCamera: TCheckBox;
    Pivot: TDummy;
    DepthPos: TDummy;
    CameraUI: TDummy;
    Disk1: TDisk;
    CameraSensor: TDummy;
    Text3D1: TText3D;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure cbCameraChange(Sender: TObject);
  private
    { Déclarations privées }
    FHasSensor: Boolean;
  {$IFDEF ANDROID}
    FSensorManager: PASensorManager;
    FNativeSensor: PASensor;
    FNativeEventQueue: PASensorEventQueue;
    FSensorEvent: ASensorEvent;
  {$ENDIF}
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

type
  TControl3DHelper = class helper for TControl3D
    procedure SetMatrix(const M: TMatrix3D);
  end;

procedure TControl3DHelper.SetMatrix(const M: TMatrix3D);
begin
  FLocalMatrix := M;
  RecalcAbsolute;
  RebuildRenderingList;
  Repaint;
end;

{$IFDEF ANDROID}

(*** Helper function to convert a rotation vector to a normalized quaternion.
  *  Given a rotation vector (presumably from a ROTATION_VECTOR sensor), returns a normalized
  *  quaternion in the array Q.  The quaternion is stored as [w, x, y, z]
  *  @param rv the rotation vector to convert
  *  @param Q an array of floats in which to store the computed quaternion
  *)
procedure getQuaternionFromVector(var Q: TQuaternion3D; const rv: ASensorVector);
begin
  Q.RealPart := 1 - rv.x * rv.x - rv.y * rv.y - rv.z * rv.z;
  if Q.RealPart > 0 then
  begin
    Q.RealPart := Sqrt(Q.RealPart);
  end else begin
    Q.RealPart := 0;
  end;
  Q.ImagPart.x := rv.x;
  Q.ImagPart.y := rv.y;
  Q.ImagPart.z := rv.z;
end;
{$ENDIF}


procedure TForm1.FormCreate(Sender: TObject);
begin
  Viewport3D1.Expand := 0.25;
  Viewport3D1.Stereo := True;
  Viewport3D1.Distortion := True;
{$IFDEF ANDROID}
  FSensorManager := ASensorManager_getInstance;
  FNativeSensor := ASensorManager_getDefaultSensor(FSensorManager, ASENSOR_TYPE_ROTATION_VECTOR);
  if FNativeSensor <> nil then
  begin
    FNativeEventQueue := ASensorManager_createEventQueue(FSensorManager, ALooper_forThread, LOOPER_ID_USER,
      nil, nil);
    ASensorEventQueue_setEventRate(FNativeEventQueue,FNativeSensor, 1000);
    ASensorEventQueue_enableSensor(FNativeEventQueue,FNativeSensor);
    Timer1.Enabled := True;
    FHasSensor := True;
  end else begin
    // pas de capteur de rotation, pas de gyroscope ?
    ShowMessage('No Rotation Sensor on this mobile...lack of a Gyroscope perhaps ?');
  end;
{$ELSE}
  // OS non supporté
  ShowMessage('Unsupported OS');
{$ENDIF}
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Q: TQuaternion3D;
begin
  if FHasSensor then
  begin
  {$IFDEF ANDROID}
    while ASensorEventQueue_getEvents(FNativeEventQueue, @FSensorEvent,1) > 0 do { loop };
    getQuaternionFromVector(Q, FSensorEvent.vector);
  {$ENDIF}
    if cbCamera.IsChecked then
    begin
      CameraSensor.SetMatrix(Q);
    end else begin
      Pivot.SetMatrix(Q);
    end;
  end;
end;

procedure TForm1.cbCameraChange(Sender: TObject);
begin
  if cbCamera.IsChecked then
  begin
    Pivot.SetMatrix(TMatrix3D.Identity);
    Cube1.RotationAngle.X := 0;
    Cube1.RotationAngle.Y := 0;
    Cube1.RotationAngle.Z := 0;
  end else begin
    CameraSensor.SetMatrix(TMatrix3D.Identity);
    CameraUI.RotationAngle.X := 0;
    CameraUI.RotationAngle.Y := 0;
    CameraUI.RotationAngle.Z := 0;
  end;
end;

end.
