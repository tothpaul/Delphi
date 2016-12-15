unit Execute.StereoViewPort;

{*************************************************}
{                                                 }
{ Copyright(c) Execute SARL <contact@execute.fr>  }
{             All rights reserved                 }
{                                                 }
{*************************************************}

// inspired by FMX.Viewport3D.pas

// NB: this component do supports mouse intercation and can not be registered at DesignTime
//     compare the code with FMX.Viewport3D.pas to add required functions.

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils,
  System.Classes,
  System.Math.Vectors,
  System.Generics.Collections,
  System.Types,
  System.UITypes,
  System.Messaging,
  FMX.Types3D,
  FMX.Types,
  FMX.Forms,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Controls,
  FMX.Graphics,
  FMX.Platform,
  FMX.Materials,
  Execute.LensEffect;

type
  TViewport3D = class(TControl, IViewport3D)
  private
    FTexture : TTexture;
    FContext : TContext3D;
    FBitmap  : TBitmap;
    FCamera  : TCamera;

    FDesignCamera: TCamera;

    FFill         : TAlphaColor;
    FMultisample  : TMultisample;

    FUsingDesignCamera: Boolean;
    FDrawing: Boolean;

    FScaleChangedId: Integer;

    FLights       : TList;
    FLightsChanged: Boolean;
    FRenderingList: TList;
    FNeedRebuild  : Boolean;
    FResize       : Boolean;

    FStereo       : Boolean;
    FStereoBitmap : TBitmap;
    FEyeDistance  : Single;
    FDistortion   : Boolean;
    FLensEffect   : TLensEffect;
    FExpand       : Single;
    procedure SetStereo(Value: Boolean);
    procedure SetDistortion(Value: Boolean);
    procedure SetExpand(Value: Single);
    procedure RenderViewToContext(Distance: Single);
    procedure RenderView(Distance: Single; Bitmap: TBitmap; const Rect: TRectF);
    procedure SetFill(const Value: TAlphaColor);
    procedure SetMultisample(const Value: TMultisample);
    procedure SetUsingDesignCamera(const Value: Boolean);
    procedure RebuildRenderingList;
    procedure ScaleChangedHandler(const Sender: TObject; const Msg : TMessage);
    function GetCurrentCamera: TCamera;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure ResizeTexture;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // You can take the code from TViewport3D if you need it
//    function FindTarget(P: TPointF; const Data: TDragObject): IControl; override;
//    function ObjectAtPoint(P: TPointF): IControl; override;

    { children }
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    { IViewport3D }
    function GetObject: TFmxObject;
    function GetContext: TContext3D;
    function GetCamera: TCamera;
    function GetLightCount: Integer;
    function GetLight(Index: Integer): TLight;
    procedure AddLight(const ALight: TLight);
    procedure RemoveLight(const ALight: TLight);
    function GetUsingDesignCamera: Boolean;
    function GetViewportScale: Single;
    procedure SetCamera(const ACamera: TCamera);
    procedure NeedRender;
    { Events }
    procedure DoScaleChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Context: TContext3D read FContext write FContext;
    property Stereo: Boolean read FStereo write SetStereo;
  published
    property Align;
    property Anchors;
    property Camera: TCamera read FCamera write SetCamera;
    property ClipChildren;
    property ClipParent;
    property Color: TAlphaColor read FFill write SetFill default TAlphaColors.White;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Height;
    property HitTest;
    property Locked;
    property Padding;
    property Multisample: TMultisample read FMultisample write SetMultisample default TMultisample.FourSamples;
    property Margins;
    property Opacity;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property UsingDesignCamera: Boolean read FUsingDesignCamera write SetUsingDesignCamera default True;
    property Visible;
    property Width;
    {events}
    property OnDragDrop;
    property OnDragEnd;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnKeyDown;
    property OnKeyUp;
    property OnClick;
    property OnDblClick;
    property OnCanFocus;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    // Execute
    property EyeDistance: Single read FEyeDistance write FEyeDistance;
    property Distortion: Boolean read FDistortion write SetDistortion;
    property Expand: Single read FExpand write SetExpand;
  end;

  TControl3DHelper = class helper for TControl3D
    procedure SetMatrix(const M: TMatrix3D);
  end;

implementation

type
  TOpenControl3D = class(TControl3D);


{ TControl3DHelper}

procedure TControl3DHelper.SetMatrix(const M: TMatrix3D);
begin
  FLocalMatrix := M;
  RecalcAbsolute;
  RebuildRenderingList;
  Repaint;
end;

{ TViewport3D }

constructor TViewport3D.Create(AOwner: TComponent);
var
  IDeviceMetrics: IFMXDeviceMetricsService;
  DisplayMetrics: TDeviceDisplayMetrics;
begin
  inherited;

  // Compute Eye distance in Pixels
  if TPlatformServices.Current.SupportsPlatformService(IFMXDeviceMetricsService, IInterface(IDeviceMetrics)) then
  begin
    DisplayMetrics := IDeviceMetrics.GetDisplayMetrics;
  end else begin
    DisplayMetrics.PixelsPerInch := 96;
  end;
  // 63,73mm distance interoculaire moyenne
  // 25,4 mm/inch
  FEyeDistance := 63.73 / 25.4 * DisplayMetrics.PixelsPerInch;

  // Pre create texture and bitmaps
  FTexture := TTexture.Create;
  FTexture.Style := [TTextureStyle.RenderTarget];
  FBitmap := TBitmap.Create;
  FStereoBitmap := TBitmap.Create;

  // nothing special
  AutoCapture := True;
  ShowHint := True;
  Width := 100;
  Height := 100;
  FMultisample := TMultisample.FourSamples;

  // this component use simple TList
  FLights := TList.Create;
  FRenderingList := TList.Create;

  FUsingDesignCamera := True;
  FFill := TAlphaColors.White;

  // The Design camera (as a single Object with the SetMatrix() hack)
  FDesignCamera := TCamera.Create(Self);
  FDesignCamera.Tag := $FFFE;
  FDesignCamera.Locked := True;
  FDesignCamera.Stored := False;
  FDesignCamera.SetMatrix(
    TMatrix3D.CreateTranslation(TPoint3D.Create(0, 0, -20))
  *
    TMatrix3D.CreateRotationX(-20 * PI / 180)
  );

  FScaleChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TScaleChangedMessage, ScaleChangedHandler);
end;

destructor TViewport3D.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TScaleChangedMessage, FScaleChangedId);

  DeleteChildren;

  FContext.Free;
  FTexture.Free;
  FBitmap.Free;
  FLights.Free;
  FRenderingList.Free;
  inherited;
end;

procedure TViewport3D.SetFill(const Value: TAlphaColor);
begin
  if FFill <> Value then
  begin
    FFill := Value;
    Repaint;
  end;
end;

procedure TViewport3D.SetMultisample(const Value: TMultisample);
begin
  if FMultisample <> Value then
  begin
    FMultisample := Value;
    if Context <> nil then
      Context.SetMultisample(FMultisample);
  end;
end;

procedure TViewport3D.SetUsingDesignCamera(const Value: Boolean);
begin
  if FUsingDesignCamera <> Value then
  begin
    FUsingDesignCamera := Value;
    NeedRender;
  end;
end;

procedure TViewport3D.AddLight(const ALight: TLight);
begin
  FLights.Add(ALight);
  FLightsChanged := True;
end;

procedure TViewport3D.RemoveLight(const ALight: TLight);
begin
  FLights.Remove(ALight);
  FLightsChanged := True;
end;

procedure TViewport3D.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  if AObject is TControl3D then
  begin
    TOpenControl3D(AObject).SetNewViewport(Self);
    FNeedRebuild := True;
  end;
end;

procedure TViewport3D.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  if AObject is TControl3D then
  begin
    TOpenControl3D(AObject).SetNewViewport(nil);
    FNeedRebuild := True;
  end;
end;

procedure TViewport3D.DoScaleChanged;
begin
  Resize;
end;

procedure TViewport3D.SetStereo(Value: Boolean);
begin
  if Value <> FStereo then
  begin
    FStereo := Value;
    FResize := True;
    Repaint;
  end;
end;

procedure TViewport3D.Paint;
var
  R: TRectF;
  D: Single;
begin
  if FDrawing then Exit;

  FDrawing := True;
  try
    // only when requiered
    if FNeedRebuild then
      RebuildRenderingList;

    if FResize then
      ResizeTexture;

    if Context <> nil then
    begin
      Canvas.Flush;

      R := LocalRect;

      // draw once or twice
      if FStereo then
      begin

        // half view
        R.Width := R.Width / 2;

        RenderView(+ FEyeDistance, FStereoBitmap, R);

        // second portion of the view
        R.Left := R.Right;
        R.Right := LocalRect.Right;

        D := - FEyeDistance;

      end else begin

        // no translation
        D := 0;

      end;

      // single or second view
      RenderView(D, FBitmap, R);

    end;
  finally
    FDrawing := False;
  end;
end;

// this method is called once or twice according to the Stereo mode
procedure TViewport3D.RenderView(Distance: Single; Bitmap: TBitmap; const Rect: TRectF);
var
  Ctx: TContext3D;
begin
  RenderViewToContext(Distance);

  // Is the distorion actived ?
  if FDistortion then
  begin
    // Render the texture ...
    FLensEffect.Render(FTexture);
    // ... and use the Lens context as Source image
    Ctx := FLensEffect.Context;
  end else begin
    // use the Viewport context as Source image
    Ctx := Context;
  end;

  // Canvas do not know Textures, we need to copy the texture to a bitmap
  // this is probably a bad idea on OpenGL
  Ctx.CopyToBitmap(Bitmap, Bitmap.Bounds);

  // Now draw the Bitmap on the Canvas
  inherited Canvas.DrawBitmap(Bitmap, Bitmap.BoundsF, Rect, AbsoluteOpacity, True);
end;

// Render the view to a context with a translation (or not)
procedure TViewport3D.RenderViewToContext(Distance: Single);
var
  C: TCamera;
  I: Integer;
begin
  if Context.BeginScene then
  try
    C := GetCurrentCamera;
    Context.SetContextState(TContextState.csScissorOff);
    Context.Clear([TClearTarget.Color, TClearTarget.Depth], FFill, 1.0, 0);

    // Set the translaction
    Context.SetCameraMatrix(TMatrix3D.CreateTranslation(TPoint3D.Create(Distance / (4 * Width), 0, 0)) * C.CameraMatrix);

    Context.SetCameraAngleOfView(C.AngleOfView);

    // Add lights
    if FLightsChanged then
    begin
      FlightsChanged := False;
      Context.Lights.Clear;
      for I := 0 to FLights.Count - 1 do
        Context.Lights.Add(GetLight(I).LightDescription);
    end;

    // Render objects
    for I := 0 to FRenderingList.Count - 1 do
      with TOpenControl3D(FRenderingList[i]) do
      begin
        if Visible and not Locked then
          RenderInternal;
      end;

  finally
    Context.EndScene;
  end;
end;

procedure TViewport3D.RebuildRenderingList;
var
  I: Integer;
  CompareFunc: TRenderingCompare;
begin
  FNeedRebuild := False;
  FRenderingList.Clear;
  if (Children <> nil) and (Children.Count > 0) and (FUpdating = 0) then
  begin
    for I := 0 to Children.Count - 1 do
      if (Children[i] is TControl3D) then
        FRenderingList.Add(Children[I]);
    CompareFunc := TRenderingCompare.Create;
    try
      FRenderingList.SortList(
        function(Item1, Item2: Pointer): Integer
        begin
          Result := CompareFunc.Compare(Item1, Item2);
        end
      );
    finally
      CompareFunc.Free;
    end;
  end;
end;

procedure TViewport3D.Resize;
begin
  inherited;
  ResizeTexture;
  Repaint;
end;

procedure TViewPort3D.ResizeTexture;
var
  S, W, H: Single;
begin
  FResize := False;
  FreeAndNil(FContext);

  S := GetViewportScale;
  ITextureAccess(FTexture).TextureScale := S;

  W := Width * S;
  H := Height * S;
  if FStereo then
    W := W / 2;

  FTexture.SetSize(Round(W * (1 + FExpand)), Round(H * (1 + FExpand)));
  FContext := TContextManager.CreateFromTexture(FTexture, FMultisample, True);
  FLightsChanged := True;

  FBitmap.SetSize(Round(W), Round(H));

  if FStereo then
    FStereoBitmap.Resize(Round(W), Round(H))
  else
    FStereoBitmap.Resize(0, 0);

  if FDistortion then
  begin
    if FLensEffect = nil then
    begin
      FLensEffect := TLensEffect.Create(40);
    end;
    FLensEffect.SetSize(Round(W), Round(H), FExpand);
  end;
end;

procedure TViewport3D.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCamera) then
    FCamera := nil;
end;

function TViewport3D.GetLightCount: Integer;
begin
  Result := FLights.Count;
end;

function TViewport3D.GetLight(Index: Integer): TLight;
begin
  Result := FLights[Index];
end;

function TViewport3D.GetObject: TFmxObject;
begin
  Result := Self;
end;

function TViewport3D.GetCamera: TCamera;
begin
  Result := FCamera;
end;

function TViewport3D.GetContext: TContext3D;
begin
  Result := FContext;
end;

function TViewport3D.GetCurrentCamera: TCamera;
begin
  if (FCamera <> nil) and not (FUsingDesignCamera) then
    Result := FCamera
  else
    Result := FDesignCamera;
end;

function TViewport3D.GetUsingDesignCamera: Boolean;
begin
  Result := FUsingDesignCamera;
end;

function TViewport3D.GetViewportScale: Single;
begin
  if Scene <> nil then
    Result := Scene.GetSceneScale
  else
    Result := 1.0;
end;

procedure TViewport3D.ScaleChangedHandler(const Sender: TObject; const Msg: TMessage);
begin
  DoScaleChanged;
end;

procedure TViewport3D.SetCamera(const ACamera: TCamera);
begin
  if FCamera <> ACamera then
  begin
    FCamera := ACamera;
    if not FUsingDesignCamera then
    begin
      NeedRender;
    end;
  end;
end;

procedure TViewport3D.SetDistortion(Value: Boolean);
begin
  if Value <> FDistortion then
  begin
    FDistortion := Value;
    FResize := True;
    Repaint;
  end;
end;

procedure TViewport3D.SetExpand(Value: Single);
begin
  if Value <> FExpand then
  begin
    FExpand := Value;
    FResize := True;
    Repaint;
  end;
end;

procedure TViewport3D.NeedRender;
begin
  if (FContext <> nil) then
  begin
    Repaint;
    UpdateEffects;
  end;
end;


end.
