-------------------------------------------------------------------------------
-- NOM DU CSU (spécification)       : tp7-graph.ads
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 2.1a
-- DATE DE LA DERNIERE MISE A JOUR  : 23 novembre 2012
-- ROLE DU CSU                      : Unité d'émulation Turbo Pascal 7.0.
--
--
-- FONCTIONS EXPORTEES DU CSU       :
--
-- FONCTIONS LOCALES DU CSU         :
--
--
-- NOTES                            :
--
-- COPYRIGHT                        : (c) Pascal Pignard 2002-2012
-- LICENCE                          : CeCILL V2 (http://www.cecill.info)
-- CONTACT                          : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------
-- Based on:
--*******************************************************
--
--       Turbo Pascal Version 7.0
--       Graph Interface Unit
--
--       Copyright (C) 1987,92 Borland International
--
--*******************************************************

-- Pascal to Ada translation by Pascal Pignard August 2002

package TP7.Graph is
   pragma Elaborate_Body;

   subtype Integer is TPInteger;

   -- GraphResult error return codes:
   grOk             : constant := 0;
   grNoInitGraph    : constant := -1;
   grNotDetected    : constant := -2;
   grFileNotFound   : constant := -3;
   grInvalidDriver  : constant := -4;
   grNoLoadMem      : constant := -5;
   grNoScanMem      : constant := -6;
   grNoFloodMem     : constant := -7;
   grFontNotFound   : constant := -8;
   grNoFontMem      : constant := -9;
   grInvalidMode    : constant := -10;
   grError          : constant := -11;   -- generic error
   grIOerror        : constant := -12;
   grInvalidFont    : constant := -13;
   grInvalidFontNum : constant := -14;
   grInvalidVersion : constant := -18;

   -- define graphics drivers
   CurrentDriver : constant := -128; -- passed to GetModeRange
   Detect        : constant := 0;    -- requests autodetection
   CGA           : constant := 1;
   MCGA          : constant := 2;
   EGA           : constant := 3;
   EGA64         : constant := 4;
   EGAMono       : constant := 5;
   IBM8514       : constant := 6;
   HercMono      : constant := 7;
   ATT400        : constant := 8;
   VGA           : constant := 9;
   PC3270        : constant := 10;

   -- graphics modes for each driver
   CGAC0      : constant := 0;  -- 320x200 palette 0: LightGreen, LightRed, Yellow; 1 page
   CGAC1      : constant := 1;  -- 320x200 palette 1: LightCyan, LightMagenta, White; 1 page
   CGAC2      : constant := 2;  -- 320x200 palette 2: Green, Red, Brown; 1 page
   CGAC3      : constant := 3;  -- 320x200 palette 3: Cyan, Magenta, LightGray; 1 page
   CGAHi      : constant := 4;  -- 640x200 1 page
   MCGAC0     : constant := 0;  -- 320x200 palette 0: LightGreen, LightRed, Yellow; 1 page
   MCGAC1     : constant := 1;  -- 320x200 palette 1: LightCyan, LightMagenta, White; 1 page
   MCGAC2     : constant := 2;  -- 320x200 palette 2: Green, Red, Brown; 1 page
   MCGAC3     : constant := 3;  -- 320x200 palette 3: Cyan, Magenta, LightGray; 1 page
   MCGAMed    : constant := 4;  -- 640x200 1 page
   MCGAHi     : constant := 5;  -- 640x480 1 page
   EGALo      : constant := 0;  -- 640x200 16 color 4 page
   EGAHi      : constant := 1;  -- 640x350 16 color 2 page
   EGA64Lo    : constant := 0;  -- 640x200 16 color 1 page
   EGA64Hi    : constant := 1;  -- 640x350 4 color  1 page
   EGAMonoHi  : constant := 3;  -- 640x350 64K on card, 1 page; 256K on card, 2 page
   HercMonoHi : constant := 0;  -- 720x348 2 page
   ATT400C0   : constant := 0;  -- 320x200 palette 0: LightGreen, LightRed, Yellow; 1 page
   ATT400C1   : constant := 1;  -- 320x200 palette 1: LightCyan, LightMagenta, White; 1 page
   ATT400C2   : constant := 2;  -- 320x200 palette 2: Green, Red, Brown; 1 page
   ATT400C3   : constant := 3;  -- 320x200 palette 3: Cyan, Magenta, LightGray; 1 page
   ATT400Med  : constant := 4;  -- 640x200 1 page
   ATT400Hi   : constant := 5;  -- 640x400 1 page
   VGALo      : constant := 0;  -- 640x200 16 color 4 page
   VGAMed     : constant := 1;  -- 640x350 16 color 2 page
   VGAHi      : constant := 2;  -- 640x480 16 color 1 page
   PC3270Hi   : constant := 0;  -- 720x350 1 page
   IBM8514LO  : constant := 0;  -- 640x480 256 colors
   IBM8514HI  : constant := 1;  -- 1024x768 256 colors

   -- Colors for SetPalette and SetAllPalette:
   Black        : constant := 0;
   Blue         : constant := 1;
   Green        : constant := 2;
   Cyan         : constant := 3;
   Red          : constant := 4;
   Magenta      : constant := 5;
   Brown        : constant := 6;
   LightGray    : constant := 7;
   DarkGray     : constant := 8;
   LightBlue    : constant := 9;
   LightGreen   : constant := 10;
   LightCyan    : constant := 11;
   LightRed     : constant := 12;
   LightMagenta : constant := 13;
   Yellow       : constant := 14;
   White        : constant := 15;

   -- colors for 8514 to set standard EGA colors w/o knowing their values
   EGABlack        : constant := 0;       -- dark colors
   EGABlue         : constant := 1;
   EGAGreen        : constant := 2;
   EGACyan         : constant := 3;
   EGARed          : constant := 4;
   EGAMagenta      : constant := 5;
   EGABrown        : constant := 20;
   EGALightgray    : constant := 7;
   EGADarkgray     : constant := 56;      -- light colors
   EGALightblue    : constant := 57;
   EGALightgreen   : constant := 58;
   EGALightcyan    : constant := 59;
   EGALightred     : constant := 60;
   EGALightmagenta : constant := 61;
   EGAYellow       : constant := 62;
   EGAWhite        : constant := 63;

   -- Line styles and widths for Get/SetLineStyle:
   SolidLn   : constant := 0;
   DottedLn  : constant := 1;
   CenterLn  : constant := 2;
   DashedLn  : constant := 3;
   UserBitLn : constant := 4;       -- User-defined line style

   NormWidth  : constant := 1;
   ThickWidth : constant := 3;

   -- Set/GetTextStyle constants:
   DefaultFont       : constant := 0;    -- 8x8 bit mapped font
   TriplexFont       : constant := 1;    -- "Stroked" fonts
   SmallFont         : constant := 2;
   SansSerifFont     : constant := 3;
   GothicFont        : constant := 4;
   ScriptFont        : constant := 5;
   SimplexFont       : constant := 6;
   TriplexScriptFont : constant := 7;
   ComplexFont       : constant := 8;
   EuropeanFont      : constant := 9;
   BoldFont          : constant := 10;

   HorizDir : constant := 0;       -- left to right
   VertDir  : constant := 1;       -- bottom to top

   UserCharSize : constant := 0;   -- user-defined char size

   -- Clipping constants:
   ClipOn  : constant Boolean := True;
   ClipOff : constant Boolean := False;

   -- Bar3D constants:
   TopOn  : constant Boolean := True;
   TopOff : constant Boolean := False;

   -- Fill patterns for Get/SetFillStyle:
   EmptyFill      : constant := 0;  -- fills area in background color
   SolidFill      : constant := 1;  -- fills area in solid fill color
   LineFill       : constant := 2;  -- --- fill
   LtSlashFill    : constant := 3;  -- /// fill
   SlashFill      : constant := 4;  -- /// fill with thick lines
   BkSlashFill    : constant := 5;  -- \\\ fill with thick lines
   LtBkSlashFill  : constant := 6;  -- \\\ fill
   HatchFill      : constant := 7;  -- light hatch fill
   XHatchFill     : constant := 8;  -- heavy cross hatch fill
   InterleaveFill : constant := 9;  -- interleaving line fill
   WideDotFill    : constant := 10; -- Widely spaced dot fill
   CloseDotFill   : constant := 11; -- Closely spaced dot fill
   UserFill       : constant := 12; -- user defined fill

   -- BitBlt operators for PutImage:
   NormalPut : constant := 0;    -- MOV        -- left for 1.0 compatibility
   CopyPut   : constant := 0;    -- MOV
   XORPut    : constant := 1;    -- XOR
   OrPut     : constant := 2;    -- OR
   AndPut    : constant := 3;    -- AND
   NotPut    : constant := 4;    -- NOT

   -- Horizontal and vertical justification for SetTextJustify:
   LeftText   : constant := 0;
   CenterText : constant := 1;
   RightText  : constant := 2;

   BottomText : constant := 0;
   -- CenterText = 1; already defined above
   TopText : constant := 2;

   MaxColors : constant Byte := 15;
   type TTabColor is array (0 .. MaxColors) of Shortint;
   type PaletteType is record
      Size   : Byte;
      Colors : TTabColor; -- array (0..MaxColors ) of  shortint;
   end record;

   type LineSettingsType is record
      LineStyle : Word;
      Pattern   : Word;
      Thickness : Word;
   end record;

   type TextSettingsType is record
      Font      : Word;
      Direction : Word;
      CharSize  : Word;
      Horiz     : Word;
      Vert      : Word;
   end record;

   type FillSettingsType is record                  -- Pre-defined fill style
      Pattern : Word;
      Color   : Word;
   end record;

   type FillPatternType is array (1 .. 8) of Byte;  -- User defined fill style

   type PointType is record
      X, Y : Integer;
   end record;

   type PolygonType is array (Positive range <>) of PointType;

   type ViewPortType is record
      X1, Y1, X2, Y2 : Integer;
      Clip           : Boolean;
   end record;

   type ArcCoordsType is record
      X, Y           : Integer;
      XStart, YStart : Integer;
      XEnd, YEnd     : Integer;
   end record;

   GraphGetMemPtr  : Pointer := nil;   -- allows user to steal heap allocation
   GraphFreeMemPtr : Pointer := nil;   -- allows user to steal heap de-allocation

   -- *** high-level error handling ***
   function GraphErrorMsg (ErrorCode : Integer) return String;
   function GraphResult return Integer;

   -- *** detection, initialization and crt mode routines ***
   procedure DetectGraph (GraphDriver, GraphMode : out Integer);
   function GetDriverName return String;

   procedure InitGraph
     (GraphDriver  : in out Integer;
      GraphMode    : in out Integer;
      PathToDriver : String);

   function RegisterBGIfont (Font : Pointer) return Integer;
   function RegisterBGIdriver (Driver : Pointer) return Integer;
   function InstallUserDriver (DriverFileName : String; AutoDetectPtr : Pointer) return Integer;
   function InstallUserFont (FontFileName : String) return Integer;
   procedure SetGraphBufSize (BufSize : Word);
   function GetMaxMode return Integer;
   procedure GetModeRange (GraphDriver : Integer; LoMode, HiMode : out Integer);
   function GetModeName (GraphMode : Integer) return String;
   procedure SetGraphMode (Mode : Integer);
   function GetGraphMode return Integer;
   procedure GraphDefaults;
   procedure RestoreCrtMode;
   procedure CloseGraph;

   function GetX return Integer;
   function GetY return Integer;
   function GetMaxX return Integer;
   function GetMaxY return Integer;

   -- *** Screen, viewport, page routines ***
   procedure ClearDevice;
   procedure SetViewPort (X1, Y1, X2, Y2 : Integer; Clip : Boolean);
   procedure GetViewSettings (ViewPort : out ViewPortType);
   procedure ClearViewPort;
   procedure SetVisualPage (Page : Word);
   procedure SetActivePage (Page : Word);

   -- *** point-oriented routines ***
   procedure PutPixel (X, Y : Integer; Pixel : Word);
   function GetPixel (X, Y : Integer) return Word;

   -- *** line-oriented routines ***
   procedure SetWriteMode (WriteMode : Integer);
   procedure LineTo (X, Y : Integer);
   procedure LineRel (Dx, Dy : Integer);
   procedure MoveTo (X, Y : Integer);
   procedure MoveRel (Dx, Dy : Integer);
   procedure Line (X1, Y1, X2, Y2 : Integer);
   procedure GetLineSettings (LineInfo : out LineSettingsType);
   procedure SetLineStyle (LineStyle : Word; Pattern : Word; Thickness : Word);

   -- *** polygon, fills and figures ***
   procedure Rectangle (X1, Y1, X2, Y2 : Integer);
   procedure Bar (X1, Y1, X2, Y2 : Integer);
   procedure Bar3D (X1, Y1, X2, Y2 : Integer; Depth : Word; Top : Boolean);
   procedure DrawPoly (NumPoints : Word; PolyPoints : PolygonType);
   procedure FillPoly (NumPoints : Word; PolyPoints : PolygonType);
   procedure GetFillSettings (FillInfo : out FillSettingsType);
   procedure GetFillPattern (FillPattern : out FillPatternType);
   procedure SetFillStyle (Pattern : Word; Color : Word);
   procedure SetFillPattern (Pattern : FillPatternType; Color : Word);
   procedure FloodFill (X, Y : Integer; Border : Word);

   -- *** arc, circle, and other curves ***
   procedure Arc (X, Y : Integer; StAngle, EndAngle, Radius : Word);
   procedure GetArcCoords (ArcCoords : out ArcCoordsType);
   procedure Circle (X, Y : Integer; Radius : Word);
   procedure Ellipse (X, Y : Integer; StAngle, EndAngle : Word; XRadius, YRadius : Word);
   procedure FillEllipse (X, Y : Integer; XRadius, YRadius : Word);
   procedure GetAspectRatio (XAsp, YAsp : out Word);
   procedure SetAspectRatio (XAsp, YAsp : Word);
   procedure PieSlice (X, Y : Integer; StAngle, EndAngle, Radius : Word);
   procedure Sector (X, Y : Integer; StAngle, EndAngle, XRadius, YRadius : Word);

   -- *** color and palette routines ***
   procedure SetBkColor (ColorNum : Word);
   procedure SetColor (Color : Word);
   function GetBkColor return Word;
   function GetColor return Word;
   procedure SetAllPalette (Palette : PaletteType);
   procedure SetPalette (ColorNum : Word; Color : Shortint);
   procedure GetPalette (Palette : out PaletteType);
   function GetPaletteSize return Integer;
   procedure GetDefaultPalette (Palette : out PaletteType);
   function GetMaxColor return Word;
   procedure SetRGBPalette (ColorNum, RedValue, GreenValue, BlueValue : Integer);

   -- *** bit-image routines ***
   function ImageSize (X1, Y1, X2, Y2 : Integer) return Longint;
   procedure GetImage (X1, Y1, X2, Y2 : Integer; BitMap : out Pointer);
   procedure PutImage (X, Y : Integer; BitMap : Pointer; BitBlt : Word);

   -- *** text routines ***
   procedure GetTextSettings (TextInfo : out TextSettingsType);
   procedure OutText (TextString : String);
   procedure OutTextXY (X, Y : Integer; TextString : String);
   procedure SetTextJustify (Horiz, Vert : Word);
   procedure SetTextStyle (Font, Direction : Word; CharSize : Word);
   procedure SetUserCharSize (MultX, DivX, MultY, DivY : Word);
   function TextHeight (TextString : String) return Word;
   function TextWidth (TextString : String) return Word;

end TP7.Graph;
