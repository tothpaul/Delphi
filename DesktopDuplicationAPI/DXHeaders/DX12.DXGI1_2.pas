unit DX12.DXGI1_2;

{$IFDEF FPC}
{$MODE delphi}{$H+}
{$ENDIF}


interface
{$Z4}

uses
    Windows, Classes, SysUtils, DX12.DXGI;

const
    IID_IDXGIDisplayControl: TGUID = '{ea9dbf1a-c88e-4486-854a-98aa0138f30c}';
    IID_IDXGIOutputDuplication: TGUID = '{191cfac3-a341-470d-b26e-a864f428319c}';
    IID_IDXGISurface2: TGUID = '{aba496dd-b617-4cb8-a866-bc44d7eb1fa2}';
    IID_IDXGIResource1: TGUID = '{30961379-4609-4a41-998e-54fe567ee0c1}';
    IID_IDXGIDevice2: TGUID = '{05008617-fbfd-4051-a790-144884b4f6a9}';
    IID_IDXGISwapChain1: TGUID = '{790a45f7-0d42-4876-983a-0a55cfe6f4aa}';
    IID_IDXGIFactory2: TGUID = '{50c83a1c-e072-4c48-87b0-3630fa36a6d0}';
    IID_IDXGIAdapter2: TGUID = '{0AA1AE0A-FA0E-4B84-8644-E05FF8E5ACB5}';
    IID_IDXGIOutput1: TGUID = '{00cddea8-939b-4b83-a340-a685226666cc}';


    DXGI_ENUM_MODES_STEREO = (4);
    DXGI_ENUM_MODES_DISABLED_STEREO = (8);
    DXGI_SHARED_RESOURCE_READ = ($80000000);
    DXGI_SHARED_RESOURCE_WRITE = (1);

type
    PSECURITY_ATTRIBUTES = ^SECURITY_ATTRIBUTES; // Missing in Winapi.Windows or Windows;

    IDXGIDisplayControl = interface(IUnknown)
        ['{ea9dbf1a-c88e-4486-854a-98aa0138f30c}']
        function IsStereoEnabled(): longbool; stdcall;
        procedure SetStereoEnabled(Enabled: longbool); stdcall;
    end;


    TDXGI_OUTDUPL_MOVE_RECT = record
        SourcePoint: TPOINT;
        DestinationRect: TRECT;
    end;

    PDXGI_OUTDUPL_MOVE_RECT = ^TDXGI_OUTDUPL_MOVE_RECT;

    TDXGI_OUTDUPL_DESC = record
        ModeDesc: TDXGI_MODE_DESC;
        Rotation: TDXGI_MODE_ROTATION;
        DesktopImageInSystemMemory: longbool;
    end;

    TDXGI_OUTDUPL_POINTER_POSITION = record
        Position: TPOINT;
        Visible: longbool;
    end;

    TDXGI_OUTDUPL_POINTER_SHAPE_TYPE = (
        DXGI_OUTDUPL_POINTER_SHAPE_TYPE_MONOCHROME = $1,
        DXGI_OUTDUPL_POINTER_SHAPE_TYPE_COLOR = $2,
        DXGI_OUTDUPL_POINTER_SHAPE_TYPE_MASKED_COLOR = $4
        );

    TDXGI_OUTDUPL_POINTER_SHAPE_INFO = record
        _Type: UINT;
        Width: UINT;
        Height: UINT;
        Pitch: UINT;
        HotSpot: TPOINT;
    end;

    PDXGI_OUTDUPL_FRAME_INFO = ^TDXGI_OUTDUPL_FRAME_INFO;
    TDXGI_OUTDUPL_FRAME_INFO = record
        LastPresentTime: LARGE_INTEGER;
        LastMouseUpdateTime: LARGE_INTEGER;
        AccumulatedFrames: UINT;
        RectsCoalesced: longbool;
        ProtectedContentMaskedOut: longbool;
        PointerPosition: TDXGI_OUTDUPL_POINTER_POSITION;
        TotalMetadataBufferSize: UINT;
        PointerShapeBufferSize: UINT;
    end;


    IDXGIOutputDuplication = interface(IDXGIObject)
        ['{191cfac3-a341-470d-b26e-a864f428319c}']
        procedure GetDesc(out pDesc: TDXGI_OUTDUPL_DESC); stdcall;
        function AcquireNextFrame(TimeoutInMilliseconds: UINT; out pFrameInfo: TDXGI_OUTDUPL_FRAME_INFO;
            out ppDesktopResource: IDXGIResource): HResult; stdcall;
        function GetFrameDirtyRects(DirtyRectsBufferSize: UINT; pDirtyRectsBuffer: PRECT;
            out pDirtyRectsBufferSizeRequired: UINT): HResult; stdcall;
        function GetFrameMoveRects(MoveRectsBufferSize: UINT; pMoveRectBuffer: PDXGI_OUTDUPL_MOVE_RECT;
            out pMoveRectsBufferSizeRequired: UINT): HResult; stdcall;
        function GetFramePointerShape(PointerShapeBufferSize: UINT; pPointerShapeBuffer: Pointer;
            out pPointerShapeBufferSizeRequired: UINT; out pPointerShapeInfo: TDXGI_OUTDUPL_POINTER_SHAPE_INFO): HResult;
            stdcall;
        function MapDesktopSurface(out pLockedRect: TDXGI_MAPPED_RECT): HResult; stdcall;
        function UnMapDesktopSurface(): HResult; stdcall;
        function ReleaseFrame(): HResult; stdcall;
    end;

    TDXGI_ALPHA_MODE = (
        DXGI_ALPHA_MODE_UNSPECIFIED = 0,
        DXGI_ALPHA_MODE_PREMULTIPLIED = 1,
        DXGI_ALPHA_MODE_STRAIGHT = 2,
        DXGI_ALPHA_MODE_IGNORE = 3,
        DXGI_ALPHA_MODE_FORCE_DWORD = integer($ffffffff)
        );

    IDXGISurface2 = interface(IDXGISurface1)
        ['{aba496dd-b617-4cb8-a866-bc44d7eb1fa2}']
        function GetResource(riid: TGUID; out ppParentResource: Pointer; out pSubresourceIndex: UINT): HResult; stdcall;
    end;


    IDXGIResource1 = interface(IDXGIResource)
        ['{30961379-4609-4a41-998e-54fe567ee0c1}']
        function CreateSubresourceSurface(index: UINT; out ppSurface: IDXGISurface2): HResult; stdcall;
        function CreateSharedHandle(pAttributes: PSECURITY_ATTRIBUTES; dwAccess: DWORD; lpName: PWideChar;
            out pHandle: THANDLE): HResult; stdcall;
    end;

    TDXGI_OFFER_RESOURCE_PRIORITY = (
        DXGI_OFFER_RESOURCE_PRIORITY_LOW = 1,
        DXGI_OFFER_RESOURCE_PRIORITY_NORMAL = (DXGI_OFFER_RESOURCE_PRIORITY_LOW + 1),
        DXGI_OFFER_RESOURCE_PRIORITY_HIGH = (DXGI_OFFER_RESOURCE_PRIORITY_NORMAL + 1)
        );


    IDXGIDevice2 = interface(IDXGIDevice1)
        ['{05008617-fbfd-4051-a790-144884b4f6a9}']
        function OfferResources(NumResources: UINT; ppResources: PIDXGIResource; Priority: TDXGI_OFFER_RESOURCE_PRIORITY): HResult;
            stdcall;
        function ReclaimResources(NumResources: UINT; ppResources: PIDXGIResource; out pDiscarded: PBoolean): HResult; stdcall;
        function EnqueueSetEvent(hEvent: THANDLE): HResult; stdcall;
    end;


    TDXGI_MODE_DESC1 = record
        Width: UINT;
        Height: UINT;
        RefreshRate: TDXGI_RATIONAL;
        Format: TDXGI_FORMAT;
        ScanlineOrdering: TDXGI_MODE_SCANLINE_ORDER;
        Scaling: TDXGI_MODE_SCALING;
        Stereo: longbool;
    end;
    PDXGI_MODE_DESC1 = ^TDXGI_MODE_DESC1;

    TDXGI_SCALING = (
        DXGI_SCALING_STRETCH = 0,
        DXGI_SCALING_NONE = 1,
        DXGI_SCALING_ASPECT_RATIO_STRETCH = 2
        );

    TDXGI_SWAP_CHAIN_DESC1 = record
        Width: UINT;
        Height: UINT;
        Format: TDXGI_FORMAT;
        Stereo: longbool;
        SampleDesc: TDXGI_SAMPLE_DESC;
        BufferUsage: TDXGI_USAGE;
        BufferCount: UINT;
        Scaling: TDXGI_SCALING;
        SwapEffect: TDXGI_SWAP_EFFECT;
        AlphaMode: TDXGI_ALPHA_MODE;
        Flags: UINT;
    end;

    PDXGI_SWAP_CHAIN_DESC1 = ^TDXGI_SWAP_CHAIN_DESC1;

    TDXGI_SWAP_CHAIN_FULLSCREEN_DESC = record
        RefreshRate: TDXGI_RATIONAL;
        ScanlineOrdering: TDXGI_MODE_SCANLINE_ORDER;
        Scaling: TDXGI_MODE_SCALING;
        Windowed: longbool;
    end;

    PDXGI_SWAP_CHAIN_FULLSCREEN_DESC = ^TDXGI_SWAP_CHAIN_FULLSCREEN_DESC;

    TDXGI_PRESENT_PARAMETERS = record
        DirtyRectsCount: UINT;
        pDirtyRects: PRECT;
        pScrollRect: PRECT;
        pScrollOffset: PPOINT;
    end;
    PDXGI_PRESENT_PARAMETERS = ^  TDXGI_PRESENT_PARAMETERS;

    IDXGISwapChain1 = interface(IDXGISwapChain)
        ['{790a45f7-0d42-4876-983a-0a55cfe6f4aa}']
        function GetDesc1(out pDesc: TDXGI_SWAP_CHAIN_DESC1): HResult; stdcall;
        function GetFullscreenDesc(out pDesc: TDXGI_SWAP_CHAIN_FULLSCREEN_DESC): HResult; stdcall;
        function GetHwnd(out pHwnd: HWND): HResult; stdcall;
        function GetCoreWindow(refiid: TGUID; out ppUnk: pointer): HResult; stdcall;
        function Present1(SyncInterval: UINT; PresentFlags: UINT; pPresentParameters: PDXGI_PRESENT_PARAMETERS): HResult; stdcall;
        function IsTemporaryMonoSupported(): longbool; stdcall;
        function GetRestrictToOutput(out ppRestrictToOutput: IDXGIOutput): HResult; stdcall;
        function SetBackgroundColor(pColor: PDXGI_RGBA): HResult; stdcall;
        function GetBackgroundColor(out pColor: TDXGI_RGBA): HResult; stdcall;
        function SetRotation(Rotation: TDXGI_MODE_ROTATION): HResult; stdcall;
        function GetRotation(out pRotation: TDXGI_MODE_ROTATION): HResult; stdcall;
    end;


    IDXGIFactory2 = interface(IDXGIFactory1)
        ['{50c83a1c-e072-4c48-87b0-3630fa36a6d0}']
        function IsWindowedStereoEnabled(): longbool; stdcall;
        function CreateSwapChainForHwnd(pDevice: IUnknown; hWnd: HWND; pDesc: PDXGI_SWAP_CHAIN_DESC1;
            pFullscreenDesc: PDXGI_SWAP_CHAIN_FULLSCREEN_DESC; pRestrictToOutput: IDXGIOutput; out ppSwapChain: IDXGISwapChain1): HResult; stdcall;
        function CreateSwapChainForCoreWindow(pDevice: IUnknown; pWindow: IUnknown; pDesc: PDXGI_SWAP_CHAIN_DESC1;
            pRestrictToOutput: IDXGIOutput; out ppSwapChain: IDXGISwapChain1): HResult; stdcall;
        function GetSharedResourceAdapterLuid(hResource: THANDLE; out pLuid: TLUID): HResult; stdcall;

        function RegisterStereoStatusWindow(WindowHandle: HWND; wMsg: UINT; out pdwCookie: DWORD): HResult; stdcall;
        function RegisterStereoStatusEvent(hEvent: THANDLE; out pdwCookie: DWORD): HResult; stdcall;
        procedure UnregisterStereoStatus(dwCookie: DWORD); stdcall;
        function RegisterOcclusionStatusWindow(WindowHandle: HWND; wMsg: UINT; out pdwCookie: DWORD): HResult; stdcall;
        function RegisterOcclusionStatusEvent(hEvent: THANDLE; out pdwCookie: DWORD): HResult; stdcall;
        procedure UnregisterOcclusionStatus(dwCookie: DWORD); stdcall;
        function CreateSwapChainForComposition(pDevice: IUnknown; pDesc: PDXGI_SWAP_CHAIN_DESC1;
            pRestrictToOutput: IDXGIOutput; out ppSwapChain: IDXGISwapChain1): HResult; stdcall;
    end;

    TDXGI_GRAPHICS_PREEMPTION_GRANULARITY = (
        DXGI_GRAPHICS_PREEMPTION_DMA_BUFFER_BOUNDARY = 0,
        DXGI_GRAPHICS_PREEMPTION_PRIMITIVE_BOUNDARY = 1,
        DXGI_GRAPHICS_PREEMPTION_TRIANGLE_BOUNDARY = 2,
        DXGI_GRAPHICS_PREEMPTION_PIXEL_BOUNDARY = 3,
        DXGI_GRAPHICS_PREEMPTION_INSTRUCTION_BOUNDARY = 4
        );
    TDXGI_COMPUTE_PREEMPTION_GRANULARITY = (
        DXGI_COMPUTE_PREEMPTION_DMA_BUFFER_BOUNDARY = 0,
        DXGI_COMPUTE_PREEMPTION_DISPATCH_BOUNDARY = 1,
        DXGI_COMPUTE_PREEMPTION_THREAD_GROUP_BOUNDARY = 2,
        DXGI_COMPUTE_PREEMPTION_THREAD_BOUNDARY = 3,
        DXGI_COMPUTE_PREEMPTION_INSTRUCTION_BOUNDARY = 4
        );

    TDXGI_ADAPTER_DESC2 = record
        Description: array [0.. 127] of WCHAR;
        VendorId: UINT;
        DeviceId: UINT;
        SubSysId: UINT;
        Revision: UINT;
        DedicatedVideoMemory: SIZE_T;
        DedicatedSystemMemory: SIZE_T;
        SharedSystemMemory: SIZE_T;
        AdapterLuid: LUID;
        Flags: UINT;
        GraphicsPreemptionGranularity: TDXGI_GRAPHICS_PREEMPTION_GRANULARITY;
        ComputePreemptionGranularity: TDXGI_COMPUTE_PREEMPTION_GRANULARITY;
    end;

    PDXGI_ADAPTER_DESC2 = ^TDXGI_ADAPTER_DESC2;


    IDXGIAdapter2 = interface(IDXGIAdapter1)
        ['{0AA1AE0A-FA0E-4B84-8644-E05FF8E5ACB5}']
        function GetDesc2(out pDesc: PDXGI_ADAPTER_DESC2): HResult; stdcall;
    end;


    IDXGIOutput1 = interface(IDXGIOutput)
        ['{00cddea8-939b-4b83-a340-a685226666cc}']
        function GetDisplayModeList1(EnumFormat: TDXGI_FORMAT; Flags: UINT; var pNumModes: UINT;
            out pDesc: PDXGI_MODE_DESC1): HResult; stdcall;
        function FindClosestMatchingMode1(pModeToMatch: PDXGI_MODE_DESC1; out pClosestMatch: TDXGI_MODE_DESC1;
            pConcernedDevice: IUnknown): HResult; stdcall;
        function GetDisplaySurfaceData1(pDestination: IDXGIResource): HResult; stdcall;
        function DuplicateOutput(pDevice: IUnknown; out ppOutputDuplication: IDXGIOutputDuplication): HResult; stdcall;
    end;



implementation

end.
