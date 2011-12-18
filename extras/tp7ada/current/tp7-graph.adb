-------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : tp7-graph.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 2.2a
-- DATE DE LA DERNIERE MISE A JOUR  : 16 décembre 2011
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
-- COPYRIGHT                        : (c) Pascal Pignard 2002-2011
-- LICENCE                          : CeCILL V2 (http://www.cecill.info)
-- CONTACT                          : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Ada.Numerics.Elementary_Functions;
with Ada.Numerics;
with Ada.Unchecked_Deallocation;
with Cairo;
with Glib;
with Glib.Convert;
with Gdk.Color;
with Gdk.Cairo;
with Gtk.Handlers;
with Gdk.Event;
with Gtk.Drawing_Area;
with Gdk.Threads;
with Gtk.Window;
with Cairo.Image_Surface;
with TP7.System;
with Pango.Layout;
with Pango.Font;
--  with Pango.Enums;
with Pango.Cairo;
with Gdk.Window;
--  with Gtk.Enums;
--  with Gtk.Style;
pragma Elaborate_All (Gdk.Color);

package body TP7.Graph is

   CCharSize : constant := 8;
   Pi        : constant := Ada.Numerics.Pi;
   subtype GDouble is Glib.Gdouble;
   use type Glib.Gdouble;
   subtype GUInt16 is Glib.Guint16;

   function Sin (X : Float) return Float renames Ada.Numerics.Elementary_Functions.Sin;
   function Cos (X : Float) return Float renames Ada.Numerics.Elementary_Functions.Cos;

   Win_Draw  : Gtk.Window.Gtk_Window := null;
   Area_Draw : Gtk.Drawing_Area.Gtk_Drawing_Area;
   --     IntSurface : Cairo_Surface;
   Cr : Cairo.Cairo_Context;
   --     IntPixmap  : Gdk.Pixmap.Gdk_Pixmap;
   IntSurface  : Cairo.Cairo_Surface;
   IntLayout   : Pango.Layout.Pango_Layout;
   IntFontDesc : Pango.Font.Pango_Font_Description;
   IntX, IntY  : aliased GDouble := 0.0;

   package Event_Cb is new Gtk.Handlers.Return_Callback (
      Gtk.Drawing_Area.Gtk_Drawing_Area_Record,
      Boolean);

   function Expose_Cb
     (Area  : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event : Gdk.Event.Gdk_Event)
      return  Boolean
   is
      pragma Unreferenced (Event);
      LCr : Cairo.Cairo_Context;
   begin
      LCr := Gdk.Cairo.Create (Gtk.Drawing_Area.Get_Window (Area));
      Cairo.Set_Source_Surface (LCr, IntSurface, 0.0, 0.0);
      Cairo.Paint (LCr);
      Cairo.Destroy (LCr);
      return False;
   end Expose_Cb;

   type IntTabColors is array (0 .. Word (MaxColors)) of Gdk.Color.Gdk_Color;
   type IntColorPaletteType is record
      Size   : Word;
      Colors : IntTabColors;
   end record;

   IntColorPalette : IntColorPaletteType :=
     (Size   => 16,
      Colors => (Black        => Gdk.Color.Parse ("black"),
                 Blue         => Gdk.Color.Parse ("blue"),
                 Green        => Gdk.Color.Parse ("green"),
                 Cyan         => Gdk.Color.Parse ("cyan"),
                 Red          => Gdk.Color.Parse ("red"),
                 Magenta      => Gdk.Color.Parse ("magenta"),
                 Brown        => Gdk.Color.Parse ("brown"),
                 LightGray    => Gdk.Color.Parse ("LightGray"),
                 DarkGray     => Gdk.Color.Parse ("DarkGray"),
                 LightBlue    => Gdk.Color.Parse ("LightBlue"),
                 LightGreen   => Gdk.Color.Parse ("LightGreen"),
                 LightCyan    => Gdk.Color.Parse ("LightCyan"),
                 LightRed     => Gdk.Color.Parse ("LightCoral"),
                 LightMagenta => Gdk.Color.Parse ("LightPink"),
                 Yellow       => Gdk.Color.Parse ("yellow"),
                 White        => Gdk.Color.Parse ("white")));

   type Rect is record
      Left, Right, Top, Bottom : TPInteger;
   end record;
   type Point is record
      H, V : TPInteger;
   end record;

   type TPixMapList;
   type PPixMapList is access TPixMapList;   -- liste chaînée des PixMap
                                             --créés pour GetImage et
                                             --PutImage
   type TPixMapList is record
      Next : PPixMapList;
      PM   : Word; --PixMapHandle;
   end record;

   type TIntPixMap is record
      PM    : Word; --PixMapHandle;
      Image : aliased Word; --SInt8;
   end record;
   type PIntPixMap is access TIntPixMap;     -- types interne pour GetImage et
                                             --PutImage

   type BitMapPtr is access Word; --BitMap;
   type BitMapHdl is access BitMapPtr;

   generic
      Size : Natural;
   package PH is
      type Pm_Data_Fields is array (0 .. 6) of Short_Integer;
      type Pm_Info is array (0 .. Size - 1) of Word; --ColorInfo;
      type ColorInfoHandle is access Word; --ColorInfoPtr;
      type Palette;
      type PalettePtr is access Palette;
      type Palette is record
         PmEntries    : Short_Integer;  -- entries in pmTable
         PmDataFields : Pm_Data_Fields; -- private fields
         PmInfo       : Pm_Info;
      end record;
      type PaletteHandle is access PalettePtr;
      --      function Convert is
      --      new
      --  --        Unchecked_Conversion(ApplicationServices.QD.
      --Palettes.PaletteHandle,
      --        PaletteHandle);
   end PH;

   generic
      PixelDepth : Natural;
   package AdaColorTable is
      type CSpecArray is array (0 .. 2 ** PixelDepth - 1) of Word;   --ColorSpe
                                                                     --c;
      type ColorTable is record
         CtSeed  : Long_Integer;  -- unique identifier for table
         CtFlags : Short_Integer;
         -- high bit: 0 = PixMap; 1 = device
         CtSize  : Short_Integer; -- number of entries in CTTable
         CtTable : CSpecArray;    -- array [0..0] of ColorSpec
      end record;
      type ColorTablePtr is access ColorTable;

      type CTabPtr is access ColorTable;
      --    type CTabHandle is access all CTabPtr;
      type CTabHandle is access CTabPtr;
      --      function Convert is new
      --        Unchecked_Conversion(ApplicationServices.QD.Quickdraw.CTabHandl
      --e, CTabHandle);
      --      function Convert is new
      --        Unchecked_Conversion(Handle, CTabHandle);
      --      function Convert is new
      --        Unchecked_Conversion(CTabHandle,
      --ApplicationServices.QD.Quickdraw.CTabHandle);
   end AdaColorTable;

   package CT4 is new AdaColorTable (4);

   -- sauvegarde des paramétres courants
   IntArcCoords : ArcCoordsType;
   IntFillInfo  : FillSettingsType;
   IntFillPat   : FillPatternType;
   IntLineInfo  : LineSettingsType;
   IntTextInfo  : TextSettingsType;
   IntViewPort  : ViewPortType;
   IntPalette   : PaletteType;

   IntColor       : Word;
   IntBkColor     : Word;
   IntTextsize    : Integer;
   IntMaxRect     : Rect;
   IntPixMapList  : PPixMapList;
   IntGraphResult : Integer := grError;

   -- Procedure interne renvoyant le motif de remplissage courant
   subtype Pattern is Word;
   function GetIntPat return Pattern is
      ResultGetIntPat : aliased Pattern;
      Ind             : Short_Integer;
   begin
      if IntFillInfo.Pattern = UserFill then
         --        for Ind in Patternarray'range loop
         --          ResultGetIntPat.Pat(Ind) := IntFillPat(Ind + 1);
         --        end loop;
         null;
      else
         case IntFillInfo.Pattern is
         when EmptyFill =>
            Ind := 20;  -- fills area in background color
         when SolidFill =>
            Ind := 1;  -- fills area in solid fill color
         when LineFill =>
            Ind := 27;  -- --- fill
         when LtSlashFill =>
            Ind := 28;  -- /// fill
         when SlashFill =>
            Ind := 26;  -- /// fill with thick lines
         when BkSlashFill =>
            Ind := 16;  -- \\\ fill with thick lines
         when LtBkSlashFill =>
            Ind := 34;  -- \\\ fill
         when HatchFill =>
            Ind := 14;  -- light hatch fill
         when XHatchFill =>
            Ind := 16;  -- heavy cross hatch fill
         when InterleaveFill =>
            Ind := 30;  -- interleaving line fill
         when WideDotFill =>
            Ind := 13; -- Widely spaced dot fill
         when CloseDotFill =>
            Ind := 22; -- Closely spaced dot fill
         when others =>
            null;
         end case;
         --      GetIndPattern(ResultGetIntPat'access, SysPatListId, Ind);
      end if;
      return ResultGetIntPat;
   end GetIntPat;

   -- procedure interne recherchant la couleur PC correspondant à celle du MAC
   subtype rgbcolor is Word;
   function GetIndColor (MacColor : rgbcolor) return Integer is
      ResultGetIndColor : Integer;
      CRGB              : aliased rgbcolor;
   begin
      ResultGetIndColor := Integer (IntPalette.Size);
      for Ind in reverse 0 .. IntPalette.Size - 1 loop
         --      GetEntryColor(IntPaletteHdl, Short_Integer(Ind), CRGB'access);
         if CRGB = MacColor then
            ResultGetIndColor := Integer (Ind);
         end if;
      end loop;
      return ResultGetIndColor;
   end GetIndColor;

   -- procedure interne initialisant la palette graphique
   procedure CreatePalette is
   --        DumCT4    : CT4.CTabHandle;
   --        DumCTable : Word; --CTabHandle;
   --        DumRGB    : aliased rgbcolor;
   begin
      -- Palette graphique :  MaxColors + 1
      --    DumCTable := GetCTable(68);
      --    IntPaletteHdl := NewPalette(MaxColors + 1, DumCTable, pmAnimated,
      --0);
      --SetPalette(IntCWind, IntPaletteHdl, True);
      --    NSetPalette(IntCWind, IntPaletteHdl, pmAllUpdates - 65536);
      -- Réordonnement des couleurs
      --      if Standard.True then
      --      DumCT4 := CT4.Convert(DumCTable);
      --        DumRGB := DumCT4.all.all.CtTable(15).RGB;
      --  --      AnimateEntry(IntCWind, Black, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(6).RGB;
      --        AnimateEntry(IntCWind, Blue, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(9).RGB;
      --        AnimateEntry(IntCWind, Green, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(7).RGB;
      --        AnimateEntry(IntCWind, Cyan, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(3).RGB;
      --        AnimateEntry(IntCWind, Red, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(5).RGB;
      --        AnimateEntry(IntCWind, Magenta, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(10).RGB;
      --        AnimateEntry(IntCWind, Brown, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(13).RGB;
      --        AnimateEntry(IntCWind, LightGray, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(14).RGB;
      --        AnimateEntry(IntCWind, DarkGray, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(6).RGB;
      --        AnimateEntry(IntCWind, LightBlue, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(8).RGB;
      --        AnimateEntry(IntCWind, LightGreen, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(7).RGB;
      --        AnimateEntry(IntCWind, LightCyan, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(2).RGB;
      --        AnimateEntry(IntCWind, LightRed, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(4).RGB;
      --        AnimateEntry(IntCWind, LightMagenta, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(1).RGB;
      --        AnimateEntry(IntCWind, Yellow, DumRGB'access);
      --        DumRGB := DumCT4.all.all.CtTable(0).RGB;
      --        AnimateEntry(IntCWind, White, DumRGB'access);
      --        ActivatePalette(IntCWind);
      --      end if;
      --      DisposeCTable(DumCTable);
      --      IntCT4 := CT4.Convert(NewHandle(ColorTable'Size / 8 +
      --(Long_Integer(
      --              MaxColors) * ColorSpec'Size) / 8));
      --      Palette2CTab(IntPaletteHdl, CT4.Convert(IntCT4));
      null;
   end CreatePalette;
   subtype PixMapHandle is Word;
   -- procedures internes de gestion des PixMap
   procedure CreatePixMap (PM : out PixMapHandle) is
      DumMaillon : PPixMapList;
   begin
      --      PM := NewPixMap;
      --      CopyPixMap(GetPortPixMap(GetWindowPort(IntCWind)), PM);
      DumMaillon      := new TPixMapList;
      DumMaillon.Next := IntPixMapList;
      DumMaillon.PM   := PM;
      IntPixMapList   := DumMaillon;
   end CreatePixMap;

   procedure DeletePixMap is
      procedure FreePixMap is new Ada.Unchecked_Deallocation (TPixMapList, PPixMapList);
      DumMaillon : PPixMapList;
   begin
      while IntPixMapList /= null loop
         --        DisposePixMap(IntPixMapList.PM);
         DumMaillon    := IntPixMapList;
         IntPixMapList := IntPixMapList.Next;
         FreePixMap (DumMaillon);
      end loop;
   end DeletePixMap;

   -- procedure interne pour normaliser les coordonnées (x1,y1) - (x2,y2)
   procedure NormRect (X1, Y1, X2, Y2 : in out Integer) is
      procedure SwapI (A, B : in out Integer) is

         Dum : Integer;
      begin
         Dum := A;
         A   := B;
         B   := Dum;
      end SwapI;

   begin
      if X1 > X2 then
         SwapI (X1, X2);
      end if;
      if Y1 > Y2 then
         SwapI (Y1, Y2);
      end if;
   end NormRect;

   procedure NormRect (R : in out Rect) is
      procedure SwapI (A, B : in out Integer) is
         Dum : Integer;
      begin
         Dum := A;
         A   := B;
         B   := Dum;
      end SwapI;
   begin
      if R.Left > R.Right then
         SwapI (R.Left, R.Right);
      end if;
      if R.Top > R.Bottom then
         SwapI (R.Top, R.Bottom);
      end if;
   end NormRect;

   procedure SetRect (R : out Rect; X1, Y1, X2, Y2 : Integer) is
   begin
      R := (Left => X1, Top => Y1, Right => X2, Bottom => Y2);
   end SetRect;

   -- *** high-level error handling ***
   function GraphErrorMsg (ErrorCode : Integer) return String is
   begin
      case ErrorCode is
      when grOk =>
         return To_TPString ("No error");
      when grNoInitGraph =>
         return To_TPString ("(BGI) graphics not installed");
      when grNotDetected =>
         return To_TPString ("Graphics hardware not detected");
      when grFileNotFound =>
         return To_TPString ("device driver file not found ()");
      when grInvalidDriver =>
         return To_TPString ("Invalid device driver file ()");
      when grNoLoadMem =>
         return To_TPString ("Not enough memory to load driver");
      when grNoScanMem =>
         return To_TPString ("Out of memory in scan fill");
      when grNoFloodMem =>
         return To_TPString ("Out of memory in flood fill");
      when grFontNotFound =>
         return To_TPString ("Font file not found ()");
      when grNoFontMem =>
         return To_TPString ("Not enough memory to load font");
      when grInvalidMode =>
         return To_TPString ("Invalid graphics mode for selected driver");
      when grError =>
         return To_TPString ("Graphics error");   -- generic error
      when grIOerror =>
         return To_TPString ("Graphics I/O error");
      when grInvalidFont =>
         return To_TPString ("Invalid font file ()");
      when grInvalidFontNum =>
         return To_TPString ("Invalid font number");
      when grInvalidVersion =>
         return To_TPString ("Invalid version");
      when others =>
         return To_TPString ("Graphics error (") & ErrorCode'Img & ')';
      end case;
   end GraphErrorMsg;

   function GraphResult return Integer is
      LGraphResult : constant Integer := IntGraphResult;
   begin
      IntGraphResult := grOk;
      return LGraphResult;
   end GraphResult;

   -- *** detection, initialization and crt mode routines ***
   procedure DetectGraph (GraphDriver, GraphMode : out Integer) is
   begin
      GraphDriver    := VGA;
      GraphMode      := VGAHi;
      IntGraphResult := grOk;
   end DetectGraph;

   function GetDriverName return String is
   begin
      return To_TPString ("EGAVGA");
   end GetDriverName;

   procedure InitGraph
     (GraphDriver  : in out Integer;
      GraphMode    : in out Integer;
      PathToDriver : String)
   is
      pragma Unreferenced (PathToDriver);
      use type Gtk.Window.Gtk_Window;
   -- Options choisies : . driver VGA
   --                            . mode VGAhi
   --                            . pas de gestion d'erreur
   --                            . ratio 1: 1
   --                            . polices et motifs du Macintosh
   begin
      GraphGetMemPtr  := nil;
      GraphFreeMemPtr := nil;
      GraphDriver     := VGA;
      GraphMode       := VGAHi;
      IntGraphResult  := grError;
      if Win_Draw = null then
         Gdk.Threads.Enter;
         Gtk.Window.Gtk_New (Win_Draw);
         Gtk.Window.Set_Title (Win_Draw, "Win Graph");
         Win_Draw.Set_Default_Size (680, 400);
         Gtk.Drawing_Area.Gtk_New (Area_Draw);
         Win_Draw.Add (Area_Draw);
         Event_Cb.Connect (Area_Draw, "expose_event", Event_Cb.To_Marshaller (Expose_Cb'Access));
         Win_Draw.Show_All;
         IntSurface :=
            Cairo.Image_Surface.Create (Cairo.Image_Surface.Cairo_Format_ARGB32, 680, 400);
         Cr         := Cairo.Create (IntSurface);
         IntLayout  := Gtk.Drawing_Area.Create_Pango_Layout (Area_Draw);
         --           Cairo.Save (CR);
         Gdk.Threads.Leave;
      end if;
      --CreatePalette;
      GraphDefaults;
      ClearDevice;
      IntGraphResult := grOk;
      --end if;
   end InitGraph;

   function RegisterBGIfont (Font : Pointer) return Integer is
      pragma Unreferenced (Font);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction RegisterBGIfont n'est pas définie !");
      end if;
      IntGraphResult := grOk;
      return 0;
   end RegisterBGIfont;

   function RegisterBGIdriver (Driver : Pointer) return Integer is
      pragma Unreferenced (Driver);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction RegisterBGIdriver n'est pas définie !");
      end if;
      IntGraphResult := grOk;
      return 0;
   end RegisterBGIdriver;

   function InstallUserDriver (DriverFileName : String; AutoDetectPtr : Pointer) return Integer is
      pragma Unreferenced (AutoDetectPtr, DriverFileName);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction InstallUserDriver n'est pas définie !");
      end if;
      IntGraphResult := grOk;
      return VGA;
   end InstallUserDriver;

   function InstallUserFont (FontFileName : String) return Integer is
      pragma Unreferenced (FontFileName);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction InstallUserFont n'est pas définie !");
      end if;
      IntGraphResult := grOk;
      return DefaultFont;
   end InstallUserFont;

   procedure SetGraphBufSize (BufSize : Word) is
      pragma Unreferenced (BufSize);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetGraphBufSize n'est pas définie !");
      end if;
      IntGraphResult := grOk;
   end SetGraphBufSize;

   function GetMaxMode return Integer is
   begin
      return VGAHi;
   end GetMaxMode;

   procedure GetModeRange (GraphDriver : Integer; LoMode, HiMode : out Integer) is
   begin
      if (GraphDriver = VGA) or (GraphDriver = CurrentDriver) then
         LoMode := VGAHi;
         HiMode := VGAHi;
      else
         LoMode := -1;
         HiMode := -1;
      end if;
   end GetModeRange;

   function GetModeName (GraphMode : Integer) return String is
   begin
      case GraphMode is
         when VGAHi =>
            return To_TPString ("640 x 480 VGA");
         when others =>
            return To_TPString ("Mode error");
      end case;
   end GetModeName;

   procedure SetGraphMode (Mode : Integer) is
      pragma Unreferenced (Mode);
   begin
      GraphDefaults;
      ClearDevice;
      IntGraphResult := grOk;
   end SetGraphMode;

   function GetGraphMode return Integer is
   begin
      IntGraphResult := grOk;
      return VGAHi;
   end GetGraphMode;

   procedure GraphDefaults is
   begin
      IntX := 0.0;
      IntY := 0.0;
      SetRect (IntMaxRect, 0, 0, 679, 399);
      SetViewPort (0, 0, GetMaxX, GetMaxY, ClipOn);
      GetDefaultPalette (IntPalette);
      SetColor (White);
      SetBkColor (Black);
      SetLineStyle (SolidLn, 0, NormWidth);
      SetFillStyle (SolidFill, GetMaxColor);
      for Ind in 1 .. 8 loop
         IntFillPat (Ind) := 16#FF#;
      end loop;
      SetTextStyle (DefaultFont, HorizDir, 1);
      SetTextJustify (LeftText, TopText);
      SetWriteMode (CopyPut);
      IntArcCoords.X      := 0;
      IntArcCoords.Y      := 0;
      IntArcCoords.Xstart := 0;
      IntArcCoords.Ystart := 0;
      IntArcCoords.Xend   := 0;
      IntArcCoords.Yend   := 0;
      IntPixMapList       := null;
   end GraphDefaults;

   procedure RestoreCrtMode is
   begin
      null;
   end RestoreCrtMode;

   procedure CloseGraph is
   begin
      DeletePixMap;
      --DisposeHandle(Handle(IntCTable));
      --      DisposePalette(IntPaletteHdl);
      --      DisposeWindow(IntCWind);
      --ShowText;
      IntGraphResult := grOk;
   end CloseGraph;

   function GetX return Integer is
   begin
      return Integer (IntX);
   end GetX;

   function GetY return Integer is
   begin
      return Integer (IntY);
   end GetY;

   function GetMaxX return Integer is
   begin
      return IntMaxRect.Right - IntMaxRect.Left;
   end GetMaxX;

   function GetMaxY return Integer is
   begin
      return IntMaxRect.Bottom - IntMaxRect.Top;
   end GetMaxY;

   -- *** Screen, viewport, page routines ***
   procedure ClearDevice is
   begin
      Gdk.Threads.Enter;
      Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Clear);
      Cairo.Rectangle (Cr, 0.0, 0.0, 680.0, 400.0);
      Cairo.Fill (Cr);
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Source);
      Gdk.Threads.Leave;
   end ClearDevice;

   procedure SetViewPort (X1, Y1, X2, Y2 : Integer; Clip : Boolean) is
   begin
      Gdk.Threads.Enter;
      -- Get back to the origin
      Cairo.Translate (Cr, Glib.Gdouble (-IntViewPort.x1), Glib.Gdouble (-IntViewPort.y1));
      IntViewPort.x1   := x1;
      IntViewPort.y1   := y1;
      IntViewPort.x2   := x2;
      IntViewPort.y2   := y2;
      IntViewPort.Clip := Clip;
      Cairo.Translate (Cr, Glib.Gdouble (x1), Glib.Gdouble (y1));
      IntX := 0.0;
      IntY := 0.0;
      Cairo.Reset_Clip (Cr);
      if Clip = ClipOn then
         Cairo.Rectangle (Cr, 0.0, 0.0, Glib.Gdouble (x2 - x1), Glib.Gdouble (y2 - y1));
         Cairo.Clip (Cr);
      end if;
      Gdk.Threads.Leave;
   end SetViewPort;

   procedure GetViewSettings (ViewPort : out ViewPortType) is
   begin
      ViewPort := IntViewPort;
   end GetViewSettings;

   procedure ClearViewPort is
   begin
      Gdk.Threads.Enter;
      Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Clear);
      Cairo.Rectangle
        (Cr,
         0.0,
         0.0,
         Glib.Gdouble (IntViewPort.x2 - IntViewPort.x1),
         Glib.Gdouble (IntViewPort.y2 - IntViewPort.y1));
      Cairo.Fill (Cr);
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Source);
      Gdk.Threads.Leave;
      IntGraphResult := grOk;
   end ClearViewPort;

   procedure SetVisualPage (Page : Word) is
      pragma Unreferenced (Page);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetVisualPage n'est pas définie !");
      end if;
   end SetVisualPage;

   procedure SetActivePage (Page : Word) is
      pragma Unreferenced (Page);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetActivePage n'est pas définie !");
      end if;
   end SetActivePage;

   -- *** point-oriented routines ***
   procedure PutPixel (X, Y : Integer; Pixel : Word) is
      pragma Unreferenced (Y, X);
   --        CRGB : aliased rgbcolor;
   begin
      if Pixel <= Word (IntPalette.Size - 1) then
         --        GetEntryColor(IntPaletteHdl, Short_Integer(Pixel),
         --CRGB'access);
         --        SetCPIxel(X, Y, CRGB'access);
         null;
      end if;
      if Debug then
         TP7.System.Writeln ("La fonction PutPixel n'est encore pas définie !");
      end if;
   end PutPixel;

   function GetPixel (X, Y : Integer) return Word is
      pragma Unreferenced (Y, X);
   --        CPix : aliased rgbcolor;
   begin
      --      GetCPIxel(X, Y, CPix'access);
      if Debug then
         TP7.System.Writeln ("La fonction GetPixel n'est encore pas définie !");
      end if;
      --        return Word (GetIndColor (CPix));
      return 0;
   end GetPixel;

   -- *** line-oriented routines ***
   procedure SetWriteMode (WriteMode : Integer) is
   begin
      Gdk.Threads.Enter;
      case WriteMode is
         when CopyPut =>
            Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Source);
         when XORPut =>
            Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Xor);
         when others =>
            null;
      end case;
      Gdk.Threads.Leave;
   end SetWriteMode;

   procedure LineTo (X, Y : Integer) is
   begin
      Gdk.Threads.Enter;
      Cairo.Move_To (Cr, IntX, IntY);
      Cairo.Line_To (Cr, GDouble (X), GDouble (Y));
      Cairo.Get_Current_Point (Cr, IntX'Access, IntY'Access);
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
   end LineTo;

   procedure LineRel (Dx, Dy : Integer) is
   begin
      Gdk.Threads.Enter;
      Cairo.Move_To (Cr, IntX, IntY);
      Cairo.Rel_Line_To (Cr, GDouble (Dx), GDouble (Dy));
      Cairo.Get_Current_Point (Cr, IntX'Access, IntY'Access);
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
   end LineRel;

   procedure MoveTo (X, Y : Integer) is
   begin
      Gdk.Threads.Enter;
      Cairo.Move_To (Cr, GDouble (X), GDouble (Y));
      Cairo.Get_Current_Point (Cr, IntX'Access, IntY'Access);
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
   end MoveTo;

   procedure MoveRel (Dx, Dy : Integer) is
   begin
      Gdk.Threads.Enter;
      Cairo.Move_To (Cr, IntX, IntY);
      Cairo.Rel_Move_To (Cr, GDouble (Dx), GDouble (Dy));
      Cairo.Get_Current_Point (Cr, IntX'Access, IntY'Access);
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
   end MoveRel;

   procedure Line (X1, Y1, X2, Y2 : Integer) is
   begin
      Gdk.Threads.Enter;
      Cairo.Move_To (Cr, GDouble (x1), GDouble (y1));
      Cairo.Line_To (Cr, GDouble (x2), GDouble (y2));
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
   end Line;

   procedure GetLineSettings (LineInfo : out LineSettingsType) is
   begin
      LineInfo := IntLineInfo;
   end GetLineSettings;

   procedure SetLineStyle (LineStyle : Word; Pattern : Word; Thickness : Word) is
      Dotted_Array : constant Cairo.Dash_Array := (2.0, 8.0);
      Center_Array : constant Cairo.Dash_Array := (6.0, 6.0, 2.0, 6.0);
      Dashed_Array : constant Cairo.Dash_Array := (10.0, 8.0);
      Ind          : Integer                   := 15;
      function Length (Val : Boolean) return Cairo.Dash_Array is
         Len : Natural := 0;
         use type Cairo.Dash_Array;
      begin
         while Ind >= 0 and then (Pattern / 2 ** Ind) mod 2 = Boolean'Pos (Val) loop
            Len := Len + 1;
            Ind := Ind - 1;
         end loop;
         if Ind < 0 then
            return (1 => GDouble (Len));
         else
            return GDouble (Len) & Length (not Val);
         end if;
      end Length;
      function Normalize (Line : Cairo.Dash_Array) return Cairo.Dash_Array is
         -- we avoid beginning with a null first On value
         use type Cairo.Dash_Array;
      begin
         -- if fisrt bit is 0 we have a first On null value
         if (Pattern / 2 ** 15) mod 2 = 0 then
            -- if last bit is 0 we move and add the first value with the last
            if Pattern mod 2 = 0 then
               return Line (Line'First + 2 .. Line'Last - 1) &
                      (Line (Line'Last) + Line (Line'First + 1));
            -- otherwise we move the second value at the end
            else
               return Line (Line'First + 2 .. Line'Last) & Line (Line'First + 1);
            end if;
         end if;
         return Line;
      end Normalize;
   begin
      IntLineInfo.LineStyle := LineStyle;
      IntLineInfo.Pattern   := Pattern;
      IntLineInfo.Thickness := Thickness;
      Cairo.Set_Line_Width (Cr, GDouble (Thickness));
      case LineStyle is
         when SolidLn =>
            Cairo.Set_Dash (Cr, Cairo.No_Dashes, 0.0);
         when DottedLn =>
            Cairo.Set_Dash (Cr, Dotted_Array, 0.0);
         when CenterLn =>
            Cairo.Set_Dash (Cr, Center_Array, 0.0);
         when DashedLn =>
            Cairo.Set_Dash (Cr, Dashed_Array, 0.0);
         when UserBitLn =>
            if Pattern /= 0 then
               Cairo.Set_Dash (Cr, Normalize (Length (True)), 0.0);
            else
               IntGraphResult := grError;
               return;
            end if;
         when others =>
            IntGraphResult := grError;
            return;
      end case;
      IntGraphResult := grOk;
   end SetLineStyle;

   -- *** polygon, fills and figures ***
   procedure Rectangle (X1, Y1, X2, Y2 : Integer) is
   begin
      Gdk.Threads.Enter;
      Cairo.Rectangle (Cr, GDouble (x1), GDouble (y1), GDouble (x2 - x1), GDouble (y2 - y1));
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
   end Rectangle;

   procedure Bar (X1, Y1, X2, Y2 : Integer) is
   begin
      Gdk.Threads.Enter;
      Cairo.Rectangle (Cr, GDouble (x1), GDouble (y1), GDouble (x2 - x1), GDouble (y2 - y1));
      Cairo.Fill (Cr);
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
      IntGraphResult := grOk;
   end Bar;

   procedure Bar3D (X1, Y1, X2, Y2 : Integer; Depth : Word; Top : Boolean) is
      X : constant Integer := GetX;
      Y : constant Integer := GetY;
   begin
      Gdk.Threads.Enter;
      Cairo.Rectangle (Cr, GDouble (x1), GDouble (y1), GDouble (x2 - x1), GDouble (y2 - y1));
      Cairo.Fill (Cr);
      Cairo.Move_To (Cr, GDouble (x2), GDouble (y2));
      Cairo.Line_To (Cr, GDouble (x1), GDouble (y2));
      Cairo.Line_To (Cr, GDouble (x1), GDouble (y2));
      Cairo.Line_To (Cr, GDouble (x1), GDouble (y1));
      Cairo.Line_To (Cr, GDouble (x2), GDouble (y1));
      Cairo.Line_To (Cr, GDouble (x2), GDouble (y2));
      Cairo.Line_To (Cr, GDouble (x2 + Depth), GDouble (y2 - Depth));
      Cairo.Line_To (Cr, GDouble (x2 + Depth), GDouble (y1 - Depth));
      if Top = TopOn then
         Cairo.Line_To (Cr, GDouble (x1 + Depth), GDouble (y1 - Depth));
         Cairo.Line_To (Cr, GDouble (x1), GDouble (y1));
         Cairo.Line_To (Cr, GDouble (x2), GDouble (y1));
         Cairo.Line_To (Cr, GDouble (x2 + Depth), GDouble (y1 - Depth));
      end if;
      Cairo.Move_To (Cr, GDouble (X), GDouble (Y));
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
      IntGraphResult := grOk;
   end Bar3D;

   procedure DrawPoly (NumPoints : Word; PolyPoints : PolygonType) is
   begin
      Gdk.Threads.Enter;
      Cairo.Move_To (Cr, GDouble (PolyPoints (1).X), GDouble (PolyPoints (1).Y));
      for Ind in 2 .. Positive (NumPoints) loop
         Cairo.Line_To (Cr, GDouble (PolyPoints (Ind).X), GDouble (PolyPoints (Ind).Y));
      end loop;
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
      IntGraphResult := grOk;
   end DrawPoly;

   procedure FillPoly (NumPoints : Word; PolyPoints : PolygonType) is
   --      PH  : PolyHandle;
   --        Pat : aliased Pattern := GetIntPat;
   begin
      Gdk.Threads.Enter;
      Cairo.Move_To (Cr, GDouble (PolyPoints (1).X), GDouble (PolyPoints (1).Y));
      for Ind in 2 .. Positive (NumPoints) loop
         Cairo.Line_To (Cr, GDouble (PolyPoints (Ind).X), GDouble (PolyPoints (Ind).Y));
      end loop;
      Cairo.Fill (Cr);
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
      IntGraphResult := grOk;
   end FillPoly;

   procedure GetFillSettings (FillInfo : out FillSettingsType) is
   begin
      FillInfo := IntFillInfo;
   end GetFillSettings;

   procedure GetFillPattern (FillPattern : out FillPatternType) is
   begin
      FillPattern := IntFillPat;
   end GetFillPattern;

   procedure SetFillStyle (Pattern : Word; Color : Word) is
   begin
      IntFillInfo.Pattern := Pattern;
      IntFillInfo.Color   := Color;
      IntGraphResult      := grOk;
      if Debug then
         TP7.System.Writeln ("La fonction SetFillStyle n'est encore pas définie !");
      end if;
   end SetFillStyle;

   procedure SetFillPattern (Pattern : FillPatternType; Color : Word) is
   begin
      IntFillPat          := Pattern;
      IntFillInfo.Pattern := UserFill;
      IntFillInfo.Color   := Color;
      IntGraphResult      := grOk;
      if Debug then
         TP7.System.Writeln ("La fonction SetFillPattern n'est encore pas définie !");
      end if;
   end SetFillPattern;

   procedure FloodFill (X, Y : Integer; Border : Word) is
      pragma Unreferenced (Border, Y, X);
   -- /!\ vérifier que la taille du tas est suffisante ...
   --        R : aliased Rect;
   --      PR : RectPtr;
   --        Words, Height : Integer;
   --      BM,  Mask   : aliased BitMap;
   --      DrawPort,  IntPort  : aliased CGrafPtr;
   --        Pat : aliased Pattern := GetIntPat;
   begin
      --      DrawPort := GetWindowPort(IntCWind);
      --      HideCursor;
      --
      --      PR := GetPortBounds(DrawPort, R'access);
      --        Words  := ((R.Right - R.Left) + 15) / 16;
      --        Height := R.Bottom - R.Top;
      --        SetRect(R'access, 0, 0, words * 16, height);
      --        Mask.bounds := R;
      --        Mask.rowBytes := (((Mask.bounds.right - Mask.bounds.left) +
      --15)   /   16) * 2;
      --        Mask.baseAddr := NewPtr(Long_Integer(Mask.bounds.bottom -
      --Mask.bounds.top) * Long_Integer(Mask.rowBytes) * 24);
      --        if  Mask.BaseAddr = null then
      --        	return;
      --          end if;

      --      IntPort := CreateNewPort;
      --      BM := Mask;
      --      BM.BaseAddr := NewPtr(GetPtrSize(Mask.BaseAddr));
      --      if  BM.BaseAddr = null then
      --        return;
      --      end if;
      --SetPortBits(BM'access);
      --      PR := GetPortBounds(IntPort, R'access);
      --FillRect(R'access, Pat'access);
      --      SetPort(DrawPort);
      --      DisposePort(IntPort);

      --PmForeColor(Short_Integer(Border));
      --      PR := GetPortBounds(DrawPort, R'access);
      --SeedCFill(GetPortBitMapForCopyBits(DrawPort), Mask'access, R'access,
      --R'access, X, Y, null, 0);
      --CopyMask(BM'access, mask'access, GetPortBitMapForCopyBits(DrawPort),
      --R'access, R'access, R'access);
      --PmForeColor(Short_Integer(IntColor));
      --      DisposePtr(Mask.BaseAddr);
      --      DisposePtr(BM.BaseAddr);
      --      ShowCursor;
      if Debug then
         TP7.System.Writeln ("La fonction FloodFill n'est pas définie !");
      end if;
   end FloodFill;

   -- *** arc, circle, and other curves ***
   procedure Arc (X, Y : Integer; StAngle, EndAngle, Radius : Word) is
   begin
      -- Save values for GetArcCoords
      IntArcCoords.X      := X;
      IntArcCoords.Y      := Y;
      IntArcCoords.Xstart := X +
                             Integer (Float (Radius) * Cos (Float (StAngle) * Pi / 180.0));
      IntArcCoords.Ystart := Y -
                             Integer (Float (Radius) * Sin (Float (StAngle) * Pi / 180.0));
      IntArcCoords.Xend   := X +
                             Integer (Float (Radius) * Cos (Float (EndAngle) * Pi / 180.0));
      IntArcCoords.Yend   := Y -
                             Integer (Float (Radius) * Sin (Float (EndAngle) * Pi / 180.0));
      Gdk.Threads.Enter;
      Cairo.Arc
        (Cr,
         GDouble (X),
         GDouble (Y),
         GDouble (Radius),
         GDouble (360 - EndAngle) * Pi / 180.0,
         GDouble (360 - StAngle) * Pi / 180.0);
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
   end Arc;

   procedure GetArcCoords (ArcCoords : out ArcCoordsType) is
   begin
      ArcCoords := IntArcCoords;
   end GetArcCoords;

   procedure Circle (X, Y : Integer; Radius : Word) is
   begin
      Gdk.Threads.Enter;
      Cairo.Arc (Cr, GDouble (X), GDouble (Y), GDouble (Radius), 0.0, GDouble (2 * Pi));
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
   end Circle;

   procedure Ellipse (X, Y : Integer; StAngle, EndAngle : Word; XRadius, YRadius : Word) is
   begin
      -- Save values for GetArcCoords
      IntArcCoords.X      := X;
      IntArcCoords.Y      := Y;
      IntArcCoords.Xstart := X +
                             Integer (Float (XRadius) * Cos (Float (StAngle) * Pi / 180.0));
      IntArcCoords.Ystart := Y -
                             Integer (Float (YRadius) * Sin (Float (StAngle) * Pi / 180.0));
      IntArcCoords.Xend   := X +
                             Integer (Float (XRadius) * Cos (Float (EndAngle) * Pi / 180.0));
      IntArcCoords.Yend   := Y -
                             Integer (Float (YRadius) * Sin (Float (EndAngle) * Pi / 180.0));
      Gdk.Threads.Enter;
      if XRadius = 0 or else YRadius = 0 then
         Cairo.Rectangle
           (Cr,
            GDouble (X - XRadius),
            GDouble (Y - YRadius),
            GDouble (XRadius) * 2.0,
            GDouble (YRadius) * 2.0);
      else
         Cairo.Save (Cr);
         Cairo.Translate (Cr, GDouble (X), GDouble (Y));
         Cairo.Scale (Cr, GDouble (XRadius), GDouble (YRadius));
         Cairo.Arc
           (Cr,
            0.0,
            0.0,
            1.0,
            GDouble (360 - EndAngle) * Pi / 180.0,
            GDouble (360 - StAngle) * Pi / 180.0);
         Cairo.Restore (Cr);
      end if;
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
   end Ellipse;

   procedure FillEllipse (X, Y : Integer; XRadius, YRadius : Word) is
   begin
      -- Save values for GetArcCoords
      IntArcCoords.X      := X;
      IntArcCoords.Y      := Y;
      IntArcCoords.Xstart := X + Integer (XRadius);
      IntArcCoords.Ystart := Y;
      IntArcCoords.Xend   := X + Integer (XRadius);
      IntArcCoords.Yend   := Y;
      Gdk.Threads.Enter;
      if XRadius = 0 or else YRadius = 0 then
         Cairo.Rectangle
           (Cr,
            GDouble (X - XRadius),
            GDouble (Y - YRadius),
            GDouble (XRadius) * 2.0,
            GDouble (YRadius) * 2.0);
      else
         Cairo.Save (Cr);
         Cairo.Translate (Cr, GDouble (X), GDouble (Y));
         Cairo.Scale (Cr, GDouble (XRadius), GDouble (YRadius));
         Cairo.Arc (Cr, 0.0, 0.0, 1.0, 0.0, GDouble (2 * Pi));
         Cairo.Fill (Cr);
         Cairo.Restore (Cr);
      end if;
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
   end FillEllipse;

   procedure GetAspectRatio (Xasp, Yasp : out Word) is
   begin
      Xasp := 1000;
      Yasp := 1000;
   end GetAspectRatio;

   procedure SetAspectRatio (Xasp, Yasp : Word) is
      pragma Unreferenced (Yasp, Xasp);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetAspectRatio n'est pas encore définie !");
      end if;
   end SetAspectRatio;

   procedure PieSlice (X, Y : Integer; StAngle, EndAngle, Radius : Word) is
   begin
      -- Save values for GetArcCoords
      IntArcCoords.X      := X;
      IntArcCoords.Y      := Y;
      IntArcCoords.Xstart := X +
                             Integer (Float (Radius) * Cos (Float (StAngle) * Pi / 180.0));
      IntArcCoords.Ystart := Y -
                             Integer (Float (Radius) * Sin (Float (StAngle) * Pi / 180.0));
      IntArcCoords.Xend   := X +
                             Integer (Float (Radius) * Cos (Float (EndAngle) * Pi / 180.0));
      IntArcCoords.Yend   := Y -
                             Integer (Float (Radius) * Sin (Float (EndAngle) * Pi / 180.0));
      Gdk.Threads.Enter;
      Cairo.Move_To (Cr, GDouble (X), GDouble (Y));
      Cairo.Arc
        (Cr,
         GDouble (X),
         GDouble (Y),
         GDouble (Radius),
         GDouble (360 - EndAngle) * Pi / 180.0,
         GDouble (360 - StAngle) * Pi / 180.0);
      Cairo.Line_To (Cr, GDouble (X), GDouble (Y));
      Cairo.Fill (Cr);
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
      IntGraphResult := grOk;
   end PieSlice;

   procedure Sector (X, Y : Integer; StAngle, EndAngle, XRadius, YRadius : Word) is
   begin
      -- Save values for GetArcCoords
      IntArcCoords.X      := X;
      IntArcCoords.Y      := Y;
      IntArcCoords.Xstart := X +
                             Integer (Float (XRadius) * Cos (Float (StAngle) * Pi / 180.0));
      IntArcCoords.Ystart := Y -
                             Integer (Float (YRadius) * Sin (Float (StAngle) * Pi / 180.0));
      IntArcCoords.Xend   := X +
                             Integer (Float (XRadius) * Cos (Float (EndAngle) * Pi / 180.0));
      IntArcCoords.Yend   := Y -
                             Integer (Float (YRadius) * Sin (Float (EndAngle) * Pi / 180.0));
      Gdk.Threads.Enter;
      if XRadius = 0 or else YRadius = 0 then
         Cairo.Rectangle
           (Cr,
            GDouble (X - XRadius),
            GDouble (Y - YRadius),
            GDouble (XRadius) * 2.0,
            GDouble (YRadius) * 2.0);
      else
         Cairo.Save (Cr);
         Cairo.Translate (Cr, GDouble (X), GDouble (Y));
         Cairo.Scale (Cr, GDouble (XRadius), GDouble (YRadius));
         Cairo.Move_To (Cr, 0.0, 0.0);
         Cairo.Arc
           (Cr,
            0.0,
            0.0,
            1.0,
            GDouble (360 - EndAngle) * Pi / 180.0,
            GDouble (360 - StAngle) * Pi / 180.0);
         Cairo.Line_To (Cr, 0.0, 0.0);
         Cairo.Fill (Cr);
         Cairo.Restore (Cr);
      end if;
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
   end Sector;

   -- *** color and palette routines ***
   procedure SetBkColor (ColorNum : Word) is
   -- TBF
   --  DumCT4 : CT4.CTabHandle;
   --DumRGB : aliased rgbcolor;
   --     IntStyle : Gtk.Style.Gtk_Style;
   begin
      if ColorNum <= Word (IntPalette.Size - 1) then
         IntBkColor := ColorNum;
         Gdk.Window.Set_Background
           (Gtk.Drawing_Area.Get_Window (Area_Draw),
            IntColorPalette.Colors (IntBkColor));
         --           gtk.Widget.Modify_Bg(gtk.Widget.Gtk_Widget(Win_Draw),
         --gtk.Enums.State_Normal, IntColorPalette.Colors(IntBkColor));
         --           gtk.Style.Gtk_New(IntStyle);
         --           gtk.Style.Set_Background(IntStyle, gtk.Enums.State_Normal,
         --IntColorPalette.Colors(IntBkColor));
         --           gtk.Style.Set_Background(IntStyle, get_window(area_Draw),
         --gtk.Enums.State_Normal);
         Win_Draw.Show_All;
         --DumCT4 := CT4.Convert(IntCTable);
         --        DumRGB :=
         --IntCT4.all.all.CtTable(Standard.Integer(ColorNum)).RGB;
         --AnimateEntry(IntCWind, 0, DumRGB'access);		-- Fond de la fenêtre
      end if;
   end SetBkColor;

   procedure SetColor (Color : Word) is
   begin
      if Color <= IntColorPalette.Size - 1 then
         IntColor := Color;
         Gdk.Threads.Enter;
         Gdk.Cairo.Set_Source_Color (Cr, IntColorPalette.Colors (IntColor));
         Gdk.Threads.Leave;
      end if;
   end SetColor;

   function GetBkColor return Word is
   begin
      return IntBkColor;
   end GetBkColor;

   function GetColor return Word is
   begin
      return IntColor;
   end GetColor;

   procedure SetAllPalette (Palette : in PaletteType) is
   --DumRGB : aliased rgbcolor;
   begin
      -- Check consitency of incoming palette
      if Palette.Size > MaxColors + 1 then
         IntGraphResult := grError;
         return;
      end if;
      for Ind in 0 .. Palette.Size - 1 loop
         if Palette.Colors (Ind) < -1
           or else Palette.Colors (Ind) > Shortint (MaxColors)
         then
            IntGraphResult := grError;
            return;
         end if;
      end loop;
      -- Modify internal palette
      for Ind in 0 .. Palette.Size - 1 loop
         if Palette.Colors (Ind) /= -1 then
            IntPalette.Colors (Ind) := Palette.Colors (Ind);
            --        DumRGB :=
            --IntCT4.all.all.CtTable(Standard.Integer(Palette.Colors(
            --              Standard.Integer(Ind)))).RGB;
            --        AnimateEntry(IntCWind, Ind, DumRGB'access);
            -- TBF activation
         end if;
      end loop;
      --  ActivatePalette(IntCWind);
      IntGraphResult := grOk;
      if Debug then
         TP7.System.Writeln ("La fonction SetAllPalette n'est encore pas définie !");
      end if;
   end SetAllPalette;

   procedure SetPalette (ColorNum : Word; Color : Shortint) is
   --DumRGB : aliased rgbcolor;
   begin
      if ColorNum <= Word (IntPalette.Size - 1)
        and then Color <= Shortint (IntPalette.Size - 1)
        and then Color >= 0
      then
         IntPalette.Colors (Byte (ColorNum))  := Color;
      --        DumRGB :=
      --IntCT4.all.all.CtTable(Standard.Integer(Color)).RGB;
      --        AnimateEntry(IntCWind, Integer(ColorNum), DumRGB'access);
      --        ActivatePalette(IntCWind);
      -- TBF activation
      else
         IntGraphResult := grError;
      end if;
      IntGraphResult := grOk;
      if Debug then
         TP7.System.Writeln ("La fonction SetPalette n'est encore pas définie !");
      end if;
   end SetPalette;

   procedure GetPalette (Palette : out PaletteType) is
   begin
      Palette := IntPalette;
   end GetPalette;

   function GetPaletteSize return Integer is
   begin
      return Integer (IntPalette.Size);
   end GetPaletteSize;

   procedure GetDefaultPalette (Palette : out PaletteType) is
   begin
      Palette.Size := MaxColors + 1;
      for Ind in 0 .. MaxColors loop
         Palette.Colors (Ind) := Ind;
      end loop;
   end GetDefaultPalette;

   function GetMaxColor return Word is
   begin
      return Word (IntPalette.Size - 1);
   end GetMaxColor;

   procedure SetRGBPalette (ColorNum, RedValue, GreenValue, BlueValue : Integer) is
      --CRGB : aliased rgbcolor;
      aColor : Gdk.Color.Gdk_Color;
   begin
      if ColorNum <= Integer (IntPalette.Size - 1) then
         --        CRGB.Red := Word(RedValue);
         --        CRGB.Green := Word(GreenValue);
         --        CRGB.Blue := Word(BlueValue);
         --        AnimateEntry(IntCWind, ColorNum, CRGB'access);
         --        ActivatePalette(IntCWind);
         Gdk.Threads.Enter;
         Gdk.Color.Set_Rgb
           (aColor,
            GUInt16 (RedValue),
            GUInt16 (GreenValue),
            GUInt16 (BlueValue));
         IntColorPalette.Colors (Word (ColorNum))  := aColor;
         -- TBF activation
         Gdk.Threads.Leave;
      end if;
      if Debug then
         TP7.System.Writeln ("La fonction SetRGBPalette n'est encore pas définie !");
      end if;
   end SetRGBPalette;

   -- *** bit-image routines ***
   function ImageSize (X1, Y1, X2, Y2 : Integer) return Longint is
      pragma Unreferenced (y2, x2, y1, x1);
   begin
      --NormRect(x1, y1, x2, y2);
      --      return
      --(((LongInt(GetPortPixMap(GetWindowPort(IntCWind)).all.PixelSize) *
      --LongInt(x2 - x1) + 15) / 16) * 2) * LongInt(y2 - y1) +
      --PixMapHandle'Size / 8;
      IntGraphResult := grOk;
      if Debug then
         TP7.System.Writeln ("La fonction ImageSize n'est encore pas définie !");
      end if;
      return 0;
   end ImageSize;

   procedure GetImage (X1, Y1, X2, Y2 : Integer; BitMap : in out Pointer) is
      pragma Unreferenced (BitMap, y2, x2, y1, x1);
   --      function Convert is new Unchecked_Conversion(Pointer,
   --PIntPixMap);
   --      function Convert is new Unchecked_Conversion(PixMapHandle,
   --BitMapHdl);
   --      function Convert is new Unchecked_Conversion(System.Address,
   --CoreServices.CarbonCore.MacTypes.Ptr);
   --        R1, R2 : aliased Rect;
   --      PPM : PIntPixMap := Convert(Bitmap);
   begin
      --NormRect(x1, y1, x2, y2);
      --      SetRect(R1'access, X1, Y1, X2, Y2);
      --        CreatePixMap(PPM.PM);
      --          SetRect(R2'access, 0, 0, x2 - x1, y2 - y1);
      --          PPM.PM.all.Bounds := R2;
      --          PPM.PM.all.RowBytes :=
      --Short_Integer(((LongInt(GetPortPixMap(GetWindowPort(IntCWind)).all.Pixe
      --lSize) * LongInt(x2 - x1) + 15) / 16) * 2 - 16#8000#);
      --          PPM.PM.all.BaseAddr := Convert(PPM.Image'address);
      --          CopyBits(GetPortBitMapForCopyBits(GetWindowPort(IntCWind)),
      --Convert(PPM.PM).all, R1'access, R2'access, srcCopy, null);
      if Debug then
         TP7.System.Writeln ("La fonction GetImage n'est encore pas définie !");
      end if;
   end GetImage;

   procedure PutImage (X, Y : Integer; BitMap : Pointer; BitBlt : Word) is
      pragma Unreferenced (BitMap, Y, X, BitBlt);
   --      function Convert is new Unchecked_Conversion(Pointer,
   --PIntPixMap);
   --      function Convert is new Unchecked_Conversion(PixMapHandle,
   --BitMapHdl);
   --        R1, R2   : aliased Rect;
   --        MyBitBlt : Short_Integer;
   --      PPM : PIntPixMap := Convert(Bitmap);
   begin
      --        case BitBlt is
      --           when CopyPut =>
      --              MyBitBlt := 0; --SrcCopy;
      --           when XORPut =>
      --              MyBitBlt := 0; --SrcXor;
      --           when OrPut =>
      --              MyBitBlt := 0; --SrcOr;
      --           when AndPut =>
      --              MyBitBlt := 0; --SrcBic;
      --           when NotPut =>
      --              MyBitBlt := 0; --NotSrcCopy;
      --           when others =>
      --              null;
      --        end case;
      --          R2 := PPM.PM.all.Bounds;
      --          SetRect(R1'access, X, Y, X + R2.Right, Y + R2.Bottom);
      --          CopyBits(Convert(PPM.PM).all,
      --GetPortBitMapForCopyBits(GetWindowPort(IntCWind)), R2'access,
      --R1'access, MyBitBlt, null);
      if Debug then
         TP7.System.Writeln ("La fonction PutImage n'est encore pas définie !");
      end if;
   end PutImage;

   -- *** text routines ***
   procedure GetTextSettings (TextInfo : out TextSettingsType) is
   begin
      TextInfo := IntTextInfo;
   end GetTextSettings;

   procedure DrawHText (X, Y : Integer; TextString : String) is
      X1 : Integer := X;
      Y1 : Integer := Y;
   begin
      case IntTextInfo.Horiz is
         when LeftText =>
            null;
         when CenterText =>
            X1 := X1 - TextWidth (TextString) / 2;
         when RightText =>
            X1 := X1 - TextWidth (TextString);
         when others =>
            null;
      end case;
      case IntTextInfo.Vert is
         when BottomText =>
            Y1 := Y1 - TextHeight (TextString);
         when CenterText =>
            Y1 := Y1 - TextHeight (TextString) / 2;
         when TopText =>
            null;
         when others =>
            null;
      end case;
      Gdk.Threads.Enter;
      Pango.Layout.Set_Text (IntLayout, Glib.Convert.Locale_To_UTF8 (To_String (TextString)));
      Cairo.Move_To (Cr, Glib.Gdouble (X1), Glib.Gdouble (Y1));
      Pango.Cairo.Show_Layout (Cr, IntLayout);
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
   end DrawHText;

   procedure DrawVText (X, Y : Integer; TextString : String) is
      X1 : Integer := X;
      Y1 : Integer := Y;
   begin
      case IntTextInfo.Horiz is
         when LeftText =>
            null;
         when CenterText =>
            X1 := X1 - TextWidth (+'M') / 2;
         when RightText =>
            X1 := X1 - TextWidth (+'M');
         when others =>
            null;
      end case;
      case IntTextInfo.Vert is
         when BottomText =>
            Y1 := Y1 - TextHeight (+'M') * TextString'Length;
         when CenterText =>
            Y1 := Y1 - (TextHeight (+'M') * TextString'Length) / 2;
         when TopText =>
            null;
         when others =>
            null;
      end case;
      Gdk.Threads.Enter;
      for Ind in 1 .. TextString'Length loop
         Pango.Layout.Set_Text (IntLayout, Glib.Convert.Locale_To_UTF8 (+TextString (Ind)));
         Cairo.Move_To (Cr, Glib.Gdouble (X1), Glib.Gdouble (Y1));
         Pango.Cairo.Show_Layout (Cr, IntLayout);
         Y1 := Y1 + TextHeight (+TextString (Ind));
      end loop;
      Cairo.Stroke (Cr);
      Win_Draw.Queue_Draw;
      Gdk.Threads.Leave;
   end DrawVText;

   procedure OutText (TextString : String) is
   begin
      case IntTextInfo.Direction is
         when HorizDir =>
            DrawHText (Integer (IntX), Integer (IntY), TextString);
         when VertDir =>
            DrawVText (Integer (IntX), Integer (IntY), TextString);
         when others =>
            null;
      end case;
      if (IntTextInfo.Horiz = CenterText) or (IntTextInfo.Horiz = RightText) then
         IntY := IntY + Glib.Gdouble (TextHeight (TextString));
      end if;
      if IntTextInfo.Horiz = LeftText then
         IntX := IntX + Glib.Gdouble (TextWidth (TextString));
      end if;
   end OutText;

   procedure OutTextXY (X, Y : Integer; TextString : String) is
   begin
      case IntTextInfo.Direction is
         when HorizDir =>
            DrawHText (X, Y, TextString);
         when VertDir =>
            DrawVText (X, Y, TextString);
         when others =>
            null;
      end case;
   end OutTextXY;

   procedure SetTextJustify (Horiz, Vert : Word) is
   begin
      IntTextInfo.Horiz := Horiz;
      IntTextInfo.Vert  := Vert;
      IntGraphResult    := grOk;
   end SetTextJustify;

   procedure SetTextStyle (Font, Direction : Word; CharSize : Word) is
   begin
      IntTextInfo.Font      := Font;
      IntTextInfo.Direction := Direction;
      IntTextInfo.CharSize  := CharSize;
      Pango.Font.Free (IntFontDesc);
      if CharSize = UserCharSize then
         IntTextsize := CCharSize;
      else
         IntTextsize := CharSize * CCharSize;
      end if;
      case Font is
         when DefaultFont =>
            IntFontDesc := Pango.Font.From_String ("Courier" & IntTextsize'Img);
         when TriplexFont =>
            IntFontDesc := Pango.Font.From_String ("Verdana" & IntTextsize'Img);
         when SmallFont =>
            IntFontDesc := Pango.Font.From_String ("Monaco" & IntTextsize'Img);
         when SansSerifFont =>
            IntFontDesc := Pango.Font.From_String ("Helvetica" & IntTextsize'Img);
         when GothicFont =>
            IntFontDesc := Pango.Font.From_String ("Zapfino" & IntTextsize'Img);
         when others =>
            null;
      end case;
      Pango.Layout.Set_Font_Description (IntLayout, IntFontDesc);
      IntGraphResult := grOk;
   end SetTextStyle;

   procedure SetUserCharSize (MultX, DivX, MultY, DivY : Word) is
      pragma Unreferenced (DivY, MultY, DivX, MultX);
   --        aMatrix : aliased Cairo_Matrix :=
   --          (Gdouble (MultX) / Gdouble (DivX),
   --           0.0,
   --           0.0,
   --           Gdouble (MultY) / Gdouble (DivY),
   --           0.0,
   --           0.0);
   begin
      --        IntTextsize          := (Integer (MultY) * IntTextsize) / Integer (DivY);
      IntTextInfo.CharSize := UserCharSize;
      --      TextSize(IntTextSize);
      --        Set_Font_Matrix (Cr, aMatrix'Access);
      if Debug then
         TP7.System.Writeln ("La fonction SetUserCharSize n'est encore pas définie !");
      end if;
   end SetUserCharSize;

   function TextHeight (TextString : String) return Word is
      Width, Height : Glib.Gint;
   begin
      Pango.Layout.Set_Text (IntLayout, Glib.Convert.Locale_To_UTF8 (To_String (TextString)));
      Pango.Layout.Get_Pixel_Size (IntLayout, Width, Height);
      return Word (Height);
   end TextHeight;

   function TextWidth (TextString : String) return Word is
      Width, Height : Glib.Gint;
   begin
      Pango.Layout.Set_Text (IntLayout, Glib.Convert.Locale_To_UTF8 (To_String (TextString)));
      Pango.Layout.Get_Pixel_Size (IntLayout, Width, Height);
      return Word (Width);
   end TextWidth;

end TP7.Graph;
