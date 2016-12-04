unit Execute.GraphicPanels.Register;
{
  (c)2016 By Execute SARL, Paul TOTH <contact@execute.fr>
  http://www.execute.fr
}
interface

uses
  System.Classes,
  Vcl.Controls,
  TreeIntf, VCLSprigs, DesignIntf, DesignEditors,
  Execute.GraphicPanels;

procedure Register;

implementation

type
  TGraphicPanelSprig = class(TControlSprig)
  public
    function DragOver(AItem: TSprig): Boolean; override;
    function DragDrop(AItem: TSprig): Boolean; override;
  end;

function GraphicDragOver(Sender, AItem: TSprig): Boolean;
begin
  Result := (AItem is TGraphicPanelSprig) and
            (csAcceptsControls in TControl(Sender.Item).ControlStyle) and
            AItem.DragOverTo(Sender);
end;

function GraphicDragDrop(Sender, AItem: TSprig): Boolean;
var
  LLeft, LTop: Integer;
begin
  Result := TControlSprig(AItem).Parent <> Sender;
  if Result then
    if AItem.DragDropTo(Sender) then
    begin
      if AItem.Item is TGraphicPanel then
      begin
        with TGraphicPanel(AItem.Item) do
        begin
          GraphicParent := TGraphicPanel(Sender.Item);
          LLeft := Left;
          LTop := Top;
          if LLeft + Width > GraphicParent.ClientWidth then
            LLeft := GraphicParent.ClientWidth - Width;
          if LTop + Height > GraphicParent.ClientHeight then
            LTop := GraphicParent.ClientHeight - Height;
          SetBounds(LLeft, LTop, Width, Height);
        end;
      end else begin
        with TGraphicPanel(AItem.Item) do
        begin
          Parent := TWinControl(Sender.Item);
          LLeft := Left;
          LTop := Top;
          if LLeft + Width > Parent.ClientWidth then
            LLeft := Parent.ClientWidth - Width;
          if LTop + Height > Parent.ClientHeight then
            LTop := Parent.ClientHeight - Height;
          SetBounds(LLeft, LTop, Width, Height);
        end;
      end;
    end;
end;

function TGraphicPanelSprig.DragDrop(AItem: TSprig): Boolean;
begin
  Result := (AItem.Owner = Owner) and GraphicDragDrop(Self, AItem);
end;

function TGraphicPanelSprig.DragOver(AItem: TSprig): Boolean;
begin
  Result := (AItem.Owner = Owner) and GraphicDragOver(Self, AItem);
end;

procedure Register;
begin
// Register the component
  RegisterComponents('Execute SARL', [TGraphicPanel]);
// Allow Drag&Drop of a GraphicPanel over a GraphicPanel in the Struture view
  RegisterSprigType(TGraphicPanel, TGraphicPanelSprig);
// TControlGuideLines need a Parent component, so we can't use it
// => VCLEditors.TControlGuidelines.CalcHorzPos
  RegisterComponentGuidelines(TGraphicPanel, TComponentGuidelines);
end;

end.
