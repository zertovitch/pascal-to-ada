--************************************************
--
--   BGI Demo Program
--   Copyright (c) 1992 by Borland International
--
--************************************************

with TP7, TP7.Crt, TP7.Graph, TP7.System;
use  TP7, TP7.Crt, TP7.Graph, TP7.System;

-- Pascal to Ada translation by Pascal Pignard September 2002
procedure BGIDemo is

   --
   --  Borland Graphics Interface (BGI) demonstration
   --  program. This program shows how to use many features of
   --  the Graph unit.
   --
   --  NOTE: to have this demo use the IBM8514 driver, specify a
   --  conditional define constant "Use8514" (using the {$DEFINE}
   --  directive or Options\Compiler\Conditional defines) and then
   --  re-compile.
   --

   --subtype String is TPString;
   subtype Integer is TPInteger;

   RepertoireBGI : constant String := "C:\TP\BGI";

   -- The ten fonts available
   Fonts : constant array (0 .. Word (10)) of TPString (1 .. 17 + 1) :=
     (To_TPString (17, "DefaultFont"),
      To_TPString (17, "TriplexFont"),
      To_TPString (17, "SmallFont"),
      To_TPString (17, "SansSerifFont"),
      To_TPString (17, "GothicFont"),
      To_TPString (17, "ScriptFont"),
      To_TPString (17, "SimplexFont"),
      To_TPString (17, "TriplexScriptFont"),
      To_TPString (17, "ComplexFont"),
      To_TPString (17, "EuropeanFont"),
      To_TPString (17, "BoldFont"));

   -- The five predefined line styles supported
   LineStyles : constant array (0 .. Word (4)) of TPString (1 .. 9 + 1) :=
     (To_TPString (9, "SolidLn"),
      To_TPString (9, "DottedLn"),
      To_TPString (9, "CenterLn"),
      To_TPString (9, "DashedLn"),
      To_TPString (9, "UserBitLn"));

   -- The twelve predefined fill styles supported
   FillStyles : constant array (0 .. Word (11)) of TPString (1 .. 14 + 1) :=
     (To_TPString (14, "EmptyFill"),
      To_TPString (14, "SolidFill"),
      To_TPString (14, "LineFill"),
      To_TPString (14, "LtSlashFill"),
      To_TPString (14, "SlashFill"),
      To_TPString (14, "BkSlashFill"),
      To_TPString (14, "LtBkSlashFill"),
      To_TPString (14, "HatchFill"),
      To_TPString (14, "XHatchFill"),
      To_TPString (14, "InterleaveFill"),
      To_TPString (14, "WideDotFill"),
      To_TPString (14, "CloseDotFill"));

   -- The two text directions available
   TextDirect : constant array (0 .. Word (1)) of TPString (1 .. 8 + 1) :=
     (To_TPString (8, "HorizDir"),
      To_TPString (8, "VertDir"));

   -- The Horizontal text justifications available
   HorizJust : constant array (0 .. Word (2)) of TPString (1 .. 10 + 1) :=
     (To_TPString (10, "LeftText"),
      To_TPString (10, "CenterText"),
      To_TPString (10, "RightText"));

   -- The vertical text justifications available
   VertJust : constant array (0 .. Word (2)) of TPString (1 .. 10 + 1) :=
     (To_TPString (10, "BottomText"),
      To_TPString (10, "CenterText"),
      To_TPString (10, "TopText"));

   GraphDriver : Integer;  -- The Graphics device driver
   GraphMode   : Integer;  -- The Graphics mode value
   MaxX, MaxY  : Integer;     -- The maximum resolution of the screen
   ErrorCode   : Integer;  -- Reports any graphics errors
   MaxColor    : Word;     -- The maximum color value available
   OldExitProc : TPProc;  -- Saves exit procedure address
   VESA16      : Integer;  -- Driver number of 16 color driver

   type Tab3Byte is array (0 .. 3) of Byte;

   type VgaInfoBlock is record
      VESASignature : Tab3Byte;
      VESAVersion   : Word;
      OEMStringPtr  : Pointer;
      Capabilities  : Tab3Byte;
      VideoModePtr  : Pointer;
   end record;

   VESA16Modes : constant array (0 .. 2) of Word := (16#0102#, 16#0104#, 16#0106#);

   -- Scan the supported mode table for the highest mode this card
   --  will provide
   --

   function GetHighestCap
     (Table : Pointer;
      Modes : Word;
      Size  : Integer)
      return  Integer
   is
      pragma Unreferenced (Table, Modes, Size);
      ResultGetHighestCap : Integer;
   begin
      declare
      begin
         ResultGetHighestCap := 0;
         --
         --  near; assembler;
         --asm
         --        XOR     AX,AX
         --        LES     DI, Table
         --@@1:
         --        MOV     SI, Modes
         --        ADD     SI, Size
         --        ADD     SI, Size
         --        MOV     BX, ES:[DI]
         --        CMP     BX, 0FFFFH
         --        JE      @@4
         --        INC     DI
         --        INC     DI
         --        MOV     CX,Size
         --@@2:
         --        CMP     BX,[SI]
         --        JZ      @@3
         --        DEC     SI
         --        DEC     SI
         --        LOOP    @@2
         --@@3:
         --        CMP     AX,CX
         --        JA      @@1
         --        MOV     AX,CX
         --        JMP     @@1
         --@@4:
         --
         null;
      end;
      return ResultGetHighestCap;
   end GetHighestCap;

   --$IFDEF DPMI
   type TRealRegs is record
      RealEDI   : Longint;
      RealESI   : Longint;
      RealEBP   : Longint;
      Reserved  : Longint;
      RealEBX   : Longint;
      RealEDX   : Longint;
      RealECX   : Longint;
      RealEAX   : Longint;
      RealFlags : Word;
      RealES    : Word;
      RealDS    : Word;
      RealFS    : Word;
      RealGS    : Word;
      RealIP    : Word;
      RealCS    : Word;
      RealSP    : Word;
      RealSS    : Word;
   end record;

   function DetectVesa16 return Integer --far; assembler;
        is
      ResultDetectVesa16 : Integer;
   begin
      declare
   --           Segment, Selector, VesaCap : Word;
      begin
         ResultDetectVesa16 := 0;
         null;
      end;
      return ResultDetectVesa16;
   end DetectVesa16;

   --$F+
   procedure MyExitProc is
   begin
      ExitProc := OldExitProc; -- Restore exit procedure address
      CloseGraph;              -- Shut down the graphics system
      null;
   end MyExitProc; -- MyExitProc
   --$F-

   procedure Initialize
      -- Initialize graphics and report any errors that may occur
   is

   --        InGraphicsMode : Boolean; -- Flags initialization of graphics mode
   --        PathToDriver   : TPString (1 .. 256);  -- Stores the DOS path to *.BGI & *.CHR
   begin
      -- when using Crt and graphics, turn off Crt's memory-mapped writes
      DirectVideo := False;
      OldExitProc := ExitProc;                -- save previous exit proc
      --ExitProc :=
      --System.Address_To_Access_Conversions.To_Address(MyExitProc);
      ---- insert our exit proc in chain

      --VESA16 := InstallUserDriver("VESA16",
      --System.Address_To_Access_Conversions.To_Address(DetectVESA16));

      --$IFDEF Use8514                          -- check for Use8514 $DEFINE
      --GraphDriver := IBM8514;
      --GraphMode := IBM8514Hi;
      --$ELSE
      GraphDriver := Detect;                -- use autodetection
      --$ENDIF

      InitGraph (GraphDriver, GraphMode, RepertoireBGI);
      ErrorCode := GraphResult;             -- preserve error return
      if ErrorCode /= grOk then             -- error?
         Writeln ("Graphics error: " + GraphErrorMsg (ErrorCode));
         Halt (1);
      end if;
      Randomize;                -- init random number generator
      MaxColor := GetMaxColor;  -- Get the maximum allowable drawing color
      MaxX     := GetMaxX;          -- Get screen resolution values
      MaxY     := GetMaxY;
      null;
   end Initialize; -- Initialize

   --     function Int2Str (L : Byte) return String
   --        -- Converts an integer to a string for use with OutText, OutTextXY
   --          is
   --     begin
   --        return To_TPString (L'Img);
   --     end Int2Str; -- Int2Str
   --
   --     function Int2Str (L : Integer) return String
   --        -- Converts an integer to a string for use with OutText, OutTextXY
   --          is
   --     begin
   --        return To_TPString (L'Img);
   --     end Int2Str; -- Int2Str
   --
   --     function Int2Str (L : Word) return String
   --        -- Converts an integer to a string for use with OutText, OutTextXY
   --          is
   --     begin
   --        return To_TPString (L'Img);
   --     end Int2Str; -- Int2Str

   function Int2Str (L : Longint) return String
      -- Converts an integer to a string for use with OutText, OutTextXY
        is
   begin
      return To_TPString (L'Img);
   end Int2Str; -- Int2Str

   function RandColor return Word
      -- Returns a Random non-zero color value that is within the legal
      --  color range for the selected device driver and graphics mode.
      --  MaxColor is set to GetMaxColor by Initialize
        is
      ResultRandColor : Word;
   begin
      declare
      begin
         ResultRandColor := Random (MaxColor) + 1;
         null;
      end;
      return ResultRandColor;
   end RandColor; -- RandColor

   procedure DefaultColors
      -- Select the maximum color in the Palette for the drawing color
   is
   begin
      SetColor (MaxColor);
      null;
   end DefaultColors; -- DefaultColors

   procedure DrawBorder
      -- Draw a border around the current view port
   is

      ViewPort : ViewPortType;
   begin
      DefaultColors;
      SetLineStyle (SolidLn, 0, NormWidth);
      GetViewSettings (ViewPort);
      Rectangle (0, 0, ViewPort.x2 - ViewPort.x1, ViewPort.y2 - ViewPort.y1);
   end DrawBorder; -- DrawBorder

   procedure FullPort
      -- Set the view port to the entire screen
   is
   begin
      SetViewPort (0, 0, MaxX, MaxY, ClipOn);
      null;
   end FullPort; -- FullPort

   procedure MainWindow (Header : String)
      -- Make a default window and view port for demos
   is
   begin
      DefaultColors;                           -- Reset the colors
      ClearDevice;                             -- Clear the screen
      SetTextStyle (DefaultFont, HorizDir, 1);  -- Default text font
      SetTextJustify (CenterText, TopText);     -- Left justify text
      FullPort;                                -- Full screen view port
      OutTextXY (MaxX / 2, 2, Header);        -- Draw the header
      -- Draw main window
      SetViewPort
        (0,
         Integer (TextHeight (To_TPString (+'M'))) + 4,
         MaxX,
         MaxY - Integer (Integer (TextHeight (To_TPString (+'M'))) + 4),
         ClipOn);
      DrawBorder;                              -- Put a border around it
      -- Move the edges in 1 pixel on all sides so border isn't in the view
      --port
      SetViewPort
        (1,
         Integer (TextHeight (To_TPString (+'M'))) + 5,
         MaxX - 1,
         MaxY - Integer (Integer (TextHeight (To_TPString (+'M'))) + 5),
         ClipOn);
      null;
   end MainWindow; -- MainWindow

   procedure StatusLine (Msg : String)
      -- Display a status line at the bottom of the screen
   is
   begin
      FullPort;
      DefaultColors;
      SetTextStyle (DefaultFont, HorizDir, 1);
      SetTextJustify (CenterText, TopText);
      SetLineStyle (SolidLn, 0, NormWidth);
      SetFillStyle (EmptyFill, 0);
      Bar (0, MaxY - Integer (Integer (TextHeight (To_TPString (+'M'))) + 4), MaxX, MaxY);   -- Era
                                                                                             --se
                                                                                             --old
                                                                                             --stat
                                                                                             --us
                                                                                             --line
      Rectangle
        (0,
         MaxY - Integer (Integer (TextHeight (To_TPString (+'M'))) + 4),
         MaxX,
         MaxY);
      OutTextXY
        (MaxX / 2,
         MaxY - Integer (Integer (TextHeight (To_TPString (+'M'))) + 2),
         Msg);
      -- Go back to the main window
      SetViewPort
        (1,
         Integer (TextHeight (To_TPString (+'M'))) + 5,
         MaxX - 1,
         MaxY - Integer (Integer (TextHeight (To_TPString (+'M'))) + 5),
         ClipOn);
      null;
   end StatusLine; -- StatusLine

   procedure WaitToGo
      -- Wait for the user to abort the program or continue
   is

      Esc : constant Char := Char'Val (27);

      Ch : Char;
   begin
      StatusLine (To_TPString ("Esc aborts or press a key..."));
      loop
         null;
         Delay1 (500);
         exit when KeyPressed;
      end loop;

      Ch := ReadKey;
      if Ch = Char'Val (0) then
         Ch := ReadKey;
      end if;      -- trap function keys
      if Ch = Esc then
         Halt (0)                           -- terminate program
;
      else
         ClearDevice;
      end if;                      -- clear screen, go on with demo
      null;
   end WaitToGo; -- WaitToGo

   procedure GetDriverAndMode (DriveStr, ModeStr : in out String)
      -- Return strings describing the current device driver and graphics mode
      --  for display of status report
   is
   begin
      Assign_String (DriveStr, GetDriverName);
      Assign_String (ModeStr, GetModeName (GetGraphMode));
      null;
   end GetDriverAndMode; -- GetDriverAndMode

   procedure ReportStatus
      -- Display the status of all query functions after InitGraph
   is

      X : constant := 10;

      ViewInfo  : ViewPortType;     -- Parameters for inquiry procedures
      LineInfo  : LineSettingsType;
      FillInfo  : FillSettingsType;
      TextInfo  : TextSettingsType;
      Palette   : PaletteType;
      DriverStr : TPString (1 .. 256);           -- Driver and mode strings
      ModeStr   : TPString (1 .. 256);
      Y         : Integer;

      procedure WriteOut (S : String)
         -- Write out a string and increment to next line
      is
      begin
         OutTextXY (X, Y, S);
         Inc (Y, Longint (Integer (TextHeight (To_TPString (+'M')))) + 2);
         null;
      end WriteOut; -- WriteOut

   begin -- ReportStatus
      GetDriverAndMode (DriverStr, ModeStr);   -- Get current settings
      GetViewSettings (ViewInfo);
      GetLineSettings (LineInfo);
      GetFillSettings (FillInfo);
      GetTextSettings (TextInfo);
      GetPalette (Palette);

      Y := 4;
      MainWindow (To_TPString ("Status report after InitGraph"));
      SetTextJustify (LeftText, TopText);
      WriteOut (To_TPString ("Graphics device    : ") + DriverStr);
      WriteOut (To_TPString ("Graphics mode      : ") + ModeStr);
      WriteOut
        (To_TPString ("Screen resolution  : (0, 0, ") +
         Int2Str (GetMaxX) +
         To_TPString (", ") +
         Int2Str (GetMaxY) +
         ')');
      WriteOut
        (To_TPString ("Current view port  : (") +
         Int2Str (ViewInfo.x1) +
         To_TPString (", ") +
         Int2Str (ViewInfo.y1) +
         To_TPString (", ") +
         Int2Str (ViewInfo.x2) +
         To_TPString (", ") +
         Int2Str (ViewInfo.y2) +
         ')');
      if ViewInfo.Clip = ClipOn then
         WriteOut (To_TPString ("Clipping           : ON"));
      else
         WriteOut (To_TPString ("Clipping           : OFF"));
      end if;
      WriteOut
        (To_TPString ("Current position   : (") +
         Int2Str (GetX) +
         To_TPString (", ") +
         Int2Str (GetY) +
         ')');
      WriteOut (To_TPString ("Palette entries    : ") + Int2Str (Palette.Size));
      WriteOut (To_TPString ("GetMaxColor        : ") + Int2Str (GetMaxColor));
      WriteOut (To_TPString ("Current color      : ") + Int2Str (GetColor));
      WriteOut (To_TPString ("Line style         : ") + LineStyles (LineInfo.LineStyle));
      WriteOut (To_TPString ("Line thickness     : ") + Int2Str (LineInfo.Thickness));
      WriteOut (To_TPString ("Current fill style : ") + FillStyles (FillInfo.Pattern));
      WriteOut (To_TPString ("Current fill color : ") + Int2Str (FillInfo.Color));
      WriteOut (To_TPString ("Current font       : ") + Fonts (TextInfo.Font));
      WriteOut (To_TPString ("Text direction     : ") + TextDirect (TextInfo.Direction));
      WriteOut (To_TPString ("Character size     : ") + Int2Str (TextInfo.CharSize));
      WriteOut (To_TPString ("Horizontal justify : ") + HorizJust (TextInfo.Horiz));
      WriteOut (To_TPString ("Vertical justify   : ") + VertJust (TextInfo.Vert));
      WaitToGo;
      null;
   end ReportStatus; -- ReportStatus

   procedure FillEllipsePlay
      -- Random filled ellipse demonstration
   is

      MaxFillStyles : constant := 12; -- patterns 0..11

      MaxRadius : Word;
      FillColor : Word;
   begin
      MainWindow (To_TPString ("FillEllipse demonstration"));
      StatusLine (To_TPString ("Esc aborts or press a key"));
      MaxRadius := Word (MaxY) / 10;
      SetLineStyle (SolidLn, 0, NormWidth);
      loop

         FillColor := RandColor;
         SetColor (FillColor);
         SetFillStyle (Random (MaxFillStyles), FillColor);
         FillEllipse
           (Integer (Random (Word (MaxX))),
            Integer (Random (Word (MaxY))),
            Random (MaxRadius),
            Random (MaxRadius));
         null;
         Delay1 (500);
         exit when KeyPressed;
      end loop;

      WaitToGo;
      null;
   end FillEllipsePlay; -- FillEllipsePlay

   procedure SectorPlay
      -- Draw random sectors on the screen
   is

      MaxFillStyles : constant := 12; -- patterns 0..11

      MaxRadius : Word;
      FillColor : Word;
      EndAngle  : Word;
   begin
      MainWindow (To_TPString ("Sector demonstration"));
      StatusLine (To_TPString ("Esc aborts or press a key"));
      MaxRadius := Word (MaxY) / 10;
      SetLineStyle (SolidLn, 0, NormWidth);
      loop

         FillColor := RandColor;
         SetColor (FillColor);
         SetFillStyle (Random (MaxFillStyles), FillColor);
         EndAngle := Random (360);
         Sector
           (Integer (Random (Word (MaxX))),
            Integer (Random (Word (MaxY))),
            Random (EndAngle),
            EndAngle,
            Random (MaxRadius),
            Random (MaxRadius));
         null;
         Delay1 (500);
         exit when KeyPressed;
      end loop;

      WaitToGo;
      null;
   end SectorPlay; -- SectorPlay

   procedure WriteModePlay
      -- Demonstrate the SetWriteMode procedure for XOR lines
   is

      DelayValue : constant := 50;  -- milliseconds to delay

      ViewInfo      : ViewPortType;
      Color         : Word;
      Left, Top     : Integer;
      Right, Bottom : Integer;
      Step          : Longint; -- step for rectangle shrinking
   begin
      MainWindow (To_TPString ("SetWriteMode demonstration"));
      StatusLine (To_TPString ("Esc aborts or press a key"));
      GetViewSettings (ViewInfo);
      Left   := 0;
      Top    := 0;
      Right  := ViewInfo.x2 - ViewInfo.x1;
      Bottom := ViewInfo.y2 - ViewInfo.y1;
      Step   := Longint (Bottom) / 50;
      SetColor (GetMaxColor);
      Line (Left, Top, Right, Bottom);
      Line (Left, Bottom, Right, Top);
      SetWriteMode (XORPut);                    -- Set XOR write mode
      loop

         Line (Left, Top, Right, Bottom);        -- Draw XOR lines
         Line (Left, Bottom, Right, Top);
         Rectangle (Left, Top, Right, Bottom);   -- Draw XOR rectangle
         Delay1 (DelayValue);                     -- Wait
         Line (Left, Top, Right, Bottom);        -- Erase lines
         Line (Left, Bottom, Right, Top);
         Rectangle (Left, Top, Right, Bottom);   -- Erase rectangle
         if (Left + Integer (Step) < Right) and (Top + Integer (Step) < Bottom) then

            Inc (Left, Step);                  -- Shrink rectangle
            Inc (Top, Step);
            Dec (Right, Step);
            Dec (Bottom, Step);
            null;
         else
            Color := RandColor;                -- New color
            SetColor (Color);
            Left   := 0;                         -- Original large rectangle
            Top    := 0;
            Right  := ViewInfo.x2 - ViewInfo.x1;
            Bottom := ViewInfo.y2 - ViewInfo.y1;
            null;
         end if;
         null;
         Delay1 (500);
         exit when KeyPressed;
      end loop;

      SetWriteMode (CopyPut);                   -- back to overwrite mode
      WaitToGo;
      null;
   end WriteModePlay; -- WriteModePlay

   procedure AspectRatioPlay
      -- Demonstrate  SetAspectRatio command
   is

      ViewInfo   : ViewPortType;
      CenterX    : Integer;
      CenterY    : Integer;
      Radius     : Word;
      Xasp, Yasp : Word;
      RadiusStep : Longint;
   begin
      MainWindow (To_TPString ("SetAspectRatio demonstration"));
      GetViewSettings (ViewInfo);
      CenterX    := (ViewInfo.x2 - ViewInfo.x1) / 2;
      CenterY    := (ViewInfo.y2 - ViewInfo.y1) / 2;
      Radius     := 3 * (Word (ViewInfo.y2 - ViewInfo.y1) / 5);
      RadiusStep := (Longint (Radius) / 30);
      Circle (CenterX, CenterY, Radius);
      GetAspectRatio (Xasp, Yasp);
      for i in 1 .. Word (30) loop

         SetAspectRatio (Xasp, Yasp + (i * Word (GetMaxX)));      -- Increase
                                                                  --Y aspect
                                                                  --factor
         Circle (CenterX, CenterY, Radius);
         Dec (Radius, RadiusStep);                   -- Shrink radius
         null;
      end loop;

      Inc (Radius, RadiusStep * 30);
      for i in 1 .. Word (30) loop

         SetAspectRatio (Xasp + (i * Word (GetMaxX)), Yasp);      -- Increase
                                                                  --X aspect
                                                                  --factor
         if Radius > Word (RadiusStep) then
            Dec (Radius, RadiusStep);
         end if;                 -- Shrink radius
         Circle (CenterX, CenterY, Radius);
         null;
      end loop;

      SetAspectRatio (Xasp, Yasp);                  -- back to original aspect
      WaitToGo;
      null;
   end AspectRatioPlay; -- AspectRatioPlay

   procedure TextPlay
      -- Demonstrate text justifications and text sizing
   is

      --        Size       : Word;
      W, H, X, Y : Integer;
      ViewInfo   : ViewPortType;
   begin
      MainWindow (To_TPString ("SetTextJustify / SetUserCharSize demo"));
      GetViewSettings (ViewInfo);
      SetTextStyle (TriplexFont, VertDir, 4);
      Y := (ViewInfo.y2 - ViewInfo.y1) - 2;
      SetTextJustify (CenterText, BottomText);
      OutTextXY (2 * Integer (TextWidth (To_TPString (+'M'))), Y, To_TPString ("Vertical"));
      SetTextStyle (TriplexFont, HorizDir, 4);
      SetTextJustify (LeftText, TopText);
      OutTextXY (2 * Integer (TextWidth (To_TPString (+'M'))), 2, To_TPString ("Horizontal"));
      Rectangle
        (2 * Integer (TextWidth (To_TPString (+'M'))),
         2,
         2 * Integer (TextWidth (To_TPString (+'M'))) +
         TextWidth (To_TPString ("Horizontal")),
         2 + TextHeight (To_TPString ("Horizontal")));
      SetTextJustify (CenterText, CenterText);
      X := (ViewInfo.x2 - ViewInfo.x1) / 2;
      Y := Integer (TextHeight (To_TPString (+'H')));
      for Size in 1 .. Word (4) loop

         SetTextStyle (TriplexFont, HorizDir, Size);
         H := Integer (TextHeight (To_TPString (+'M')));
         W := Integer (TextWidth (To_TPString (+'M')));
         Inc (Y, Longint (H));
         OutTextXY (X, Y, To_TPString ("Size ") + Int2Str (Size));
         null;
      end loop;

      Inc (Y, Longint (H) / 2);
      SetTextJustify (CenterText, TopText);
      SetUserCharSize (5, 6, 3, 2);
      SetTextStyle (TriplexFont, HorizDir, UserCharSize);
      OutTextXY ((ViewInfo.x2 - ViewInfo.x1) / 2, Y, To_TPString ("User defined size!"));
      WaitToGo;
      null;
   end TextPlay; -- TextPlay

   procedure TextDump
      -- Dump the complete character sets to the screen
   is

      CGASizes  : constant array (0 .. Word (10)) of Word  := (1, 3, 7, 3, 3, 3, 3, 3, 3, 1, 1);
      NormSizes : constant array (0 .. Word (10)) of Word  := (1, 4, 7, 4, 4, 4, 4, 4, 4, 2, 2);

      --        Font     : Word;
      ViewInfo : ViewPortType;
      Ch       : Char;
   begin
      for Font in 0 .. Word (10) loop

         MainWindow (Fonts (Font) + To_TPString (" character set"));
         GetViewSettings (ViewInfo);
         SetTextJustify (LeftText, TopText);
         MoveTo (2, 3);
         if Font = DefaultFont then

            SetTextStyle (Font, HorizDir, 1);
            Ch := Char'Val (0);
            loop

               OutText (To_TPString (+Ch));
               if (GetX + Integer (TextWidth (To_TPString (+'M')))) >
                  (ViewInfo.x2 - ViewInfo.x1)
               then
                  MoveTo (2, GetY + Integer (TextHeight (To_TPString (+'M'))) + 3);
               end if;
               Ch := Succ (Ch);
               null;
               exit when (Ch >= Char'Val (255));
            end loop;

            null;
         else
            if MaxY < 200 then
               SetTextStyle (Font, HorizDir, CGASizes (Font));
            else
               SetTextStyle (Font, HorizDir, NormSizes (Font));
            end if;
            Ch := '!';
            loop

               OutText (To_TPString (+Ch));
               if (GetX + Integer (TextWidth (To_TPString (+'M')))) >
                  (ViewInfo.x2 - ViewInfo.x1)
               then
                  MoveTo (2, GetY + Integer (TextHeight (To_TPString (+'M'))) + 3);
               end if;
               Ch := Succ (Ch);
               null;
               exit when (Ch >= Char'Val (255));
            end loop;

            null;
         end if;
         WaitToGo;
         null;
      end loop;
      -- for loop
      null;
   end TextDump; -- TextDump

   procedure LineToPlay
      -- Demonstrate MoveTo and LineTo commands
   is

      MaxPoints : constant := 15;

      Points   : array (0 .. MaxPoints) of PointType;
      ViewInfo : ViewPortType;
      --        I, J       : Integer;
      CenterX    : Integer;   -- The center point of the circle
      CenterY    : Integer;
      Radius     : Word;
      StepAngle  : Word;
      Xasp, Yasp : Word;
      Radians    : Real;

      function AdjAsp (Value : Integer) return Integer
         -- Adjust a value for the aspect ratio of the device
           is
         ResultAdjAsp : Integer;
      begin
         declare
         begin
            ResultAdjAsp := Integer ((Longint (Value) * Longint (Xasp)) / Longint (Yasp));
            null;
         end;
         return ResultAdjAsp;
      end AdjAsp; -- AdjAsp

   begin
      MainWindow (To_TPString ("MoveTo, LineTo demonstration"));
      GetAspectRatio (Xasp, Yasp);
      GetViewSettings (ViewInfo);
      CenterX := (ViewInfo.x2 - ViewInfo.x1) / 2;
      CenterY := (ViewInfo.y2 - ViewInfo.y1) / 2;
      Radius  := Word (CenterY);
      while (CenterY + AdjAsp (Integer (Radius))) < (ViewInfo.y2 - ViewInfo.y1) - 20 loop

         Inc (Radius);
      end loop;

      StepAngle := 360 / MaxPoints;
      for I in 0 .. MaxPoints - 1 loop

         Radians      := Real (StepAngle * I) * Pi / 180.0;
         Points (I).X := CenterX + Integer (Round (Cos (Radians) * Real (Radius)));
         Points (I).Y := CenterY - AdjAsp (Integer (Round (Sin (Radians) * Real (Radius))));
         null;
      end loop;

      Circle (CenterX, CenterY, Radius);
      for I in 0 .. MaxPoints - 1 loop

         for J in I .. MaxPoints - 1 loop

            MoveTo (Points (I).X, Points (I).Y);
            LineTo (Points (J).X, Points (J).Y);
            null;
         end loop;

         null;
      end loop;

      WaitToGo;
      null;
   end LineToPlay; -- LineToPlay

   procedure LineRelPlay
      -- Demonstrate MoveRel and LineRel commands
   is

      MaxPoints : constant := 12;

      Poly     : PolygonType (1 .. MaxPoints); -- Stores a polygon for filling
      CurrPort : ViewPortType;

      procedure DrawTesseract
         -- Draw a Tesseract on the screen with relative move and
         --  line drawing commands, also create a polygon for filling
      is

         CheckerBoard : constant FillPatternType :=
           (0,
            16#10#,
            16#28#,
            16#44#,
            16#28#,
            16#10#,
            0,
            0);

         X, Y, W, H : Integer;

      begin
         GetViewSettings (CurrPort);
         W := (CurrPort.x2 - CurrPort.x1) / 9;
         H := (CurrPort.y2 - CurrPort.y1) / 8;
         X := ((CurrPort.x2 - CurrPort.x1) / 2) - Integer (Round (2.5 * Real (W)));
         Y := ((CurrPort.y2 - CurrPort.y1) / 2) - (3 * H);

         -- Border around viewport is outer part of polygon
         Poly (1).X := 0;
         Poly (1).Y := 0;
         Poly (2).X := CurrPort.x2 - CurrPort.x1;
         Poly (2).Y := 0;
         Poly (3).X := CurrPort.x2 - CurrPort.x1;
         Poly (3).Y := CurrPort.y2 - CurrPort.y1;
         Poly (4).X := 0;
         Poly (4).Y := CurrPort.y2 - CurrPort.y1;
         Poly (5).X := 0;
         Poly (5).Y := 0;
         MoveTo (X, Y);

         -- Grab the whole in the polygon as we draw
         MoveRel (0, H);
         Poly (6).X := GetX;
         Poly (6).Y := GetY;
         MoveRel (W, -H);
         Poly (7).X := GetX;
         Poly (7).Y := GetY;
         MoveRel (4 * W, 0);
         Poly (8).X := GetX;
         Poly (8).Y := GetY;
         MoveRel (0, 5 * H);
         Poly (9).X := GetX;
         Poly (9).Y := GetY;
         MoveRel (-W, H);
         Poly (10).X := GetX;
         Poly (10).Y := GetY;
         MoveRel (-(4 * W), 0);
         Poly (11).X := GetX;
         Poly (11).Y := GetY;
         MoveRel (0, -5 * H);
         Poly (12).X := GetX;
         Poly (12).Y := GetY;

         -- Fill the polygon with a user defined fill pattern
         SetFillPattern (CheckerBoard, MaxColor);
         --           FillPoly (12, Poly); -- fill all aera
         DrawPoly (12, Poly);

         MoveRel (W, -H);
         LineRel (0, 5 * H);
         LineRel (2 * W, 0);
         LineRel (0, -3 * H);
         LineRel (W, -H);
         LineRel (0, 5 * H);
         MoveRel (0, -5 * H);
         LineRel (-(2 * W), 0);
         LineRel (0, 3 * H);
         LineRel (-W, H);
         MoveRel (W, -H);
         LineRel (W, 0);
         MoveRel (0, -(2 * H));
         LineRel (-W, 0);

         -- Flood fill the center
         FloodFill ((CurrPort.x2 - CurrPort.x1) / 2, (CurrPort.y2 - CurrPort.y1) / 2, MaxColor);
         null;
      end DrawTesseract; -- DrawTesseract

   begin
      MainWindow (To_TPString ("LineRel / MoveRel demonstration"));
      GetViewSettings (CurrPort);
      -- Move the viewport out 1 pixel from each end
      SetViewPort (CurrPort.x1 - 1, CurrPort.y1 - 1, CurrPort.x2 + 1, CurrPort.y2 + 1, ClipOn);
      DrawTesseract;
      WaitToGo;
      null;
   end LineRelPlay; -- LineRelPlay

   procedure PiePlay
      -- Demonstrate  PieSlice and GetAspectRatio commands
   is

      ViewInfo   : ViewPortType;
      CenterX    : Integer;
      CenterY    : Integer;
      Radius     : Word;
      Xasp, Yasp : Word;
      X, Y       : Integer := 0;

      function AdjAsp (Value : Integer) return Integer
         -- Adjust a value for the aspect ratio of the device
           is
         ResultAdjAsp : Integer;
      begin
         declare
         begin
            ResultAdjAsp := Integer (Longint (Value) * Longint (Xasp) / Longint (Yasp));
            null;
         end;
         return ResultAdjAsp;
      end AdjAsp; -- AdjAsp

      procedure GetTextCoords (AngleInDegrees, Radius : Word; X, Y : in out Integer)
         -- Get the coordinates of text for pie slice labels
      is

         Radians : Real;
      begin
         Radians := Real (AngleInDegrees) * Pi / 180.0;
         X       := Integer (Round (Cos (Radians) * Real (Radius)));
         Y       := Integer (Round (Sin (Radians) * Real (Radius)));
         null;
      end GetTextCoords; -- GetTextCoords

   begin
      MainWindow (To_TPString ("PieSlice / GetAspectRatio demonstration"));
      GetAspectRatio (Xasp, Yasp);
      GetViewSettings (ViewInfo);
      CenterX := (ViewInfo.x2 - ViewInfo.x1) / 2;
      CenterY := ((ViewInfo.y2 - ViewInfo.y1) / 2) + 20;
      Radius  := Word (ViewInfo.y2 - ViewInfo.y1) / 3;
      while AdjAsp (Integer (Radius)) <
            Integer (Round (Real (ViewInfo.y2 - ViewInfo.y1) / 3.6))
      loop

         Inc (Radius);
      end loop;

      SetTextStyle (TriplexFont, HorizDir, 4);
      SetTextJustify (CenterText, TopText);
      OutTextXY (CenterX, 0, To_TPString ("This is a pie chart!"));

      SetTextStyle (TriplexFont, HorizDir, 3);

      SetFillStyle (SolidFill, RandColor);
      PieSlice (CenterX + 10, CenterY - AdjAsp (10), 0, 90, Radius);
      GetTextCoords (45, Radius, X, Y);
      SetTextJustify (LeftText, BottomText);
      OutTextXY
        (CenterX + 10 + X + Integer (TextWidth (To_TPString (+'H'))),
         CenterY - AdjAsp (10 + Y),
         To_TPString ("25 %"));

      SetFillStyle (HatchFill, RandColor);
      PieSlice (CenterX, CenterY, 225, 360, Radius);
      GetTextCoords (293, Radius, X, Y);
      SetTextJustify (LeftText, TopText);
      OutTextXY
        (CenterX + X + Integer (TextWidth (To_TPString (+'H'))),
         CenterY - AdjAsp (Y),
         To_TPString ("37.5 %"));

      SetFillStyle (InterleaveFill, RandColor);
      PieSlice (CenterX - 10, CenterY, 135, 225, Radius);
      GetTextCoords (180, Radius, X, Y);
      SetTextJustify (RightText, CenterText);
      OutTextXY
        (CenterX - 10 + X - Integer (TextWidth (To_TPString (+'H'))),
         CenterY - AdjAsp (Y),
         To_TPString ("25 %"));

      SetFillStyle (WideDotFill, RandColor);
      PieSlice (CenterX, CenterY, 90, 135, Radius);
      GetTextCoords (112, Radius, X, Y);
      SetTextJustify (RightText, BottomText);
      OutTextXY
        (CenterX + X - Integer (TextWidth (To_TPString (+'H'))),
         CenterY - AdjAsp (Y),
         To_TPString ("12.5 %"));

      WaitToGo;
      null;
   end PiePlay; -- PiePlay

   procedure Bar3DPlay
      -- Demonstrate Bar3D command
   is

      NumBars : constant Word := 7;    -- The number of bars
                                       --drawn
      BarHeight : constant array (1 .. NumBars) of Integer := (1, 3, 2, 5, 4, 2, 1);
      YTicks : constant Integer := 5;  -- The number of tick
                                       --marks on the Y axis

      ViewInfo  : ViewPortType;
      H         : Integer;
      XStep     : Real;
      YStep     : Real;
      J         : Integer;
      Depth     : Word;
      Color     : Word;
   begin
      MainWindow (To_TPString ("Bar3D / Rectangle demonstration"));
      H := 3 * Integer (TextHeight (To_TPString (+'M')));
      GetViewSettings (ViewInfo);
      SetTextJustify (CenterText, TopText);
      SetTextStyle (TriplexFont, HorizDir, 4);
      OutTextXY (MaxX / 2, 6, To_TPString ("These are 3D bars !"));
      SetTextStyle (DefaultFont, HorizDir, 1);
      SetViewPort
        (ViewInfo.x1 + 50,
         ViewInfo.y1 + 40,
         ViewInfo.x2 - 50,
         ViewInfo.y2 - 10,
         ClipOn);
      GetViewSettings (ViewInfo);
      Line (H, H, H, (ViewInfo.y2 - ViewInfo.y1) - H);
      Line
        (H,
         (ViewInfo.y2 - ViewInfo.y1) - H,
         (ViewInfo.x2 - ViewInfo.x1) - H,
         (ViewInfo.y2 - ViewInfo.y1) - H);
      YStep := Real ((ViewInfo.y2 - ViewInfo.y1) - (2 * H)) / Real (YTicks);
      XStep := Real ((ViewInfo.x2 - ViewInfo.x1) - (2 * H)) / Real (NumBars);
      J := (ViewInfo.y2 - ViewInfo.y1) - H;
      SetTextJustify (CenterText, CenterText);

      -- Draw the Y axis and ticks marks
      for I in 0 .. YTicks loop

         Line (H / 2, J, H, J);
         OutTextXY (0, J, Int2Str (I));
         J := Integer (Round (Real (J) - YStep));
         null;
      end loop;

      Depth := Word (Trunc (0.25 * XStep));    -- Calculate depth of bar

      -- Draw X axis, bars, and tick marks
      SetTextJustify (CenterText, TopText);
      J     := H;
      for I in 1 .. Succ (NumBars) loop

         SetColor (MaxColor);
         Line
           (J,
            (ViewInfo.y2 - ViewInfo.y1) - H,
            J,
            (ViewInfo.y2 - ViewInfo.y1 - 3) - (H / 2));
         OutTextXY (J, (ViewInfo.y2 - ViewInfo.y1) - (H / 2), Int2Str (I - 1));
         if I /= Succ (NumBars) then

            Color := RandColor;
            SetFillStyle (I, Color);
            SetColor (Color);
            Bar3D
              (J,
               Integer (Round
                           (Real (ViewInfo.y2 - ViewInfo.y1 - H) -
                            Real (BarHeight (I)) * YStep)),
               Integer (Round (Real (J) + XStep - Real (Depth))),
               Integer (Round (Real (ViewInfo.y2 - ViewInfo.y1))) - H - 1,
               Depth,
               TopOn);
            J := Integer (Round (Real (J) + XStep));
            null;
         end if;
         null;
      end loop;

      WaitToGo;
      null;
   end Bar3DPlay; -- Bar3DPlay

   procedure BarPlay
      -- Demonstrate Bar command
   is

      NumBars   : constant Word                            := 5;
      BarHeight : constant array (1 .. NumBars) of Integer := (1, 3, 5, 2, 4);
      Styles    : constant array (1 .. NumBars) of Word    := (1, 3, 10, 5, 9);

      ViewInfo : ViewPortType;
      --        BarNum   : Word;
      H, J  : Integer;
      XStep : Real;
      YStep : Real;
      Color : Word;
   begin
      MainWindow (To_TPString ("Bar / Rectangle demonstration"));
      H := 3 * Integer (TextHeight (To_TPString (+'M')));
      GetViewSettings (ViewInfo);
      SetTextJustify (CenterText, TopText);
      SetTextStyle (TriplexFont, HorizDir, 4);
      OutTextXY (MaxX / 2, 6, To_TPString ("These are 2D bars !"));
      SetTextStyle (DefaultFont, HorizDir, 1);
      SetViewPort
        (ViewInfo.x1 + 50,
         ViewInfo.y1 + 30,
         ViewInfo.x2 - 50,
         ViewInfo.y2 - 10,
         ClipOn);
      GetViewSettings (ViewInfo);
      Line (H, H, H, (ViewInfo.y2 - ViewInfo.y1) - H);
      Line
        (H,
         (ViewInfo.y2 - ViewInfo.y1) - H,
         (ViewInfo.x2 - ViewInfo.x1) - H,
         (ViewInfo.y2 - ViewInfo.y1) - H);
      YStep := Real ((ViewInfo.y2 - ViewInfo.y1) - (2 * H)) / Real (NumBars);
      XStep := Real ((ViewInfo.x2 - ViewInfo.x1) - (2 * H)) / Real (NumBars);
      J     := (ViewInfo.y2 - ViewInfo.y1) - H;
      SetTextJustify (CenterText, CenterText);

      -- Draw Y axis with tick marks
      for I in 0 .. NumBars loop

         Line (H / 2, J, H, J);
         OutTextXY (0, J, Int2Str (I));
         J := Integer (Round (Real (J) - YStep));
         null;
      end loop;

      -- Draw X axis, bars, and tick marks
      J := H;
      SetTextJustify (CenterText, TopText);
      for I in 1 .. Succ (NumBars) loop

         SetColor (MaxColor);
         Line
           (J,
            (ViewInfo.y2 - ViewInfo.y1) - H,
            J,
            (ViewInfo.y2 - ViewInfo.y1 - 3) - (H / 2));
         OutTextXY (J, (ViewInfo.y2 - ViewInfo.y1) - (H / 2), Int2Str (I));
         if I /= Succ (NumBars) then

            Color := RandColor;
            SetFillStyle (Styles (I), Color);
            SetColor (Color);
            Bar
              (J,
               Integer (Round
                           (Real (ViewInfo.y2 - ViewInfo.y1 - H) -
                            Real (BarHeight (I)) * YStep)),
               Integer (Round (Real (J) + XStep)),
               (ViewInfo.y2 - ViewInfo.y1) - H - 1);
            Rectangle
              (J,
               Integer (Round
                           (Real (ViewInfo.y2 - ViewInfo.y1 - H) -
                            Real (BarHeight (I)) * YStep)),
               Integer (Round (Real (J) + XStep)),
               (ViewInfo.y2 - ViewInfo.y1) - H - 1);
            null;
         end if;
         J := Integer (Round (Real (J) + XStep));
         null;
      end loop;

      WaitToGo;
      null;
   end BarPlay; -- BarPlay

   procedure CirclePlay
      -- Draw random circles on the screen
   is

      MaxRadius : Word;
   begin
      MainWindow (To_TPString ("Circle demonstration"));
      StatusLine (To_TPString ("Esc aborts or press a key"));
      MaxRadius := Word (MaxY) / 10;
      SetLineStyle (SolidLn, 0, NormWidth);
      loop

         SetColor (RandColor);
         Circle
           (Integer (Random (Word (MaxX))),
            Integer (Random (Word (MaxY))),
            Random (MaxRadius));
         null;
         Delay1 (500);
         exit when KeyPressed;
      end loop;

      WaitToGo;
      null;
   end CirclePlay; -- CirclePlay

   procedure RandBarPlay
      -- Draw random bars on the screen
   is

      MaxWidth  : Integer;
      MaxHeight : Integer;
      ViewInfo  : ViewPortType;
      Color     : Word;
   begin
      MainWindow (To_TPString ("Random Bars"));
      StatusLine (To_TPString ("Esc aborts or press a key"));
      GetViewSettings (ViewInfo);
      MaxWidth  := ViewInfo.x2 - ViewInfo.x1;
      MaxHeight := ViewInfo.y2 - ViewInfo.y1;
      loop

         Color := RandColor;
         SetColor (Color);
         SetFillStyle (Random (CloseDotFill) + 1, Color);
         Bar3D
           (Integer (Random (Word (MaxWidth))),
            Integer (Random (Word (MaxHeight))),
            Integer (Random (Word (MaxWidth))),
            Integer (Random (Word (MaxHeight))),
            0,
            TopOff);
         null;
         Delay1 (500);
         exit when KeyPressed;
      end loop;

      WaitToGo;
      null;
   end RandBarPlay; -- RandBarPlay

   procedure ArcPlay
      -- Draw random arcs on the screen
   is

      MaxRadius : Word;
      EndAngle  : Word;
      ArcInfo   : ArcCoordsType;
   begin
      MainWindow (To_TPString ("Arc / GetArcCoords demonstration"));
      StatusLine (To_TPString ("Esc aborts or press a key"));
      MaxRadius := Word (MaxY) / 10;
      loop

         SetColor (RandColor);
         EndAngle := Random (360);
         SetLineStyle (SolidLn, 0, NormWidth);
         Arc
           (Integer (Random (Word (MaxX))),
            Integer (Random (Word (MaxY))),
            Random (EndAngle),
            EndAngle,
            Random (MaxRadius));
         GetArcCoords (ArcInfo);
         Line (ArcInfo.X, ArcInfo.Y, ArcInfo.Xstart, ArcInfo.Ystart);
         Line (ArcInfo.X, ArcInfo.Y, ArcInfo.Xend, ArcInfo.Yend);
         null;
         Delay1 (500);
         exit when KeyPressed;
      end loop;

      WaitToGo;
      null;
   end ArcPlay; -- ArcPlay

   procedure PutPixelPlay
      -- Demonstrate the PutPixel and GetPixel commands
   is

      Seed   : constant := 1962; -- A seed for the random number generator
      NumPts : constant := 2000; -- The number of pixels plotted
      --        Esc    : constant String := To_TPString (+Char'Val (27));

      I          : Word;
      Color      : Word;
      X, Y       : Integer;
      XMax, YMax : Integer;
      ViewInfo   : ViewPortType;
   begin
      MainWindow (To_TPString ("PutPixel / GetPixel demonstration"));
      StatusLine (To_TPString ("Esc aborts or press a key..."));

      GetViewSettings (ViewInfo);
      XMax := (ViewInfo.x2 - ViewInfo.x1 - 1);
      YMax := (ViewInfo.y2 - ViewInfo.y1 - 1);
      while not KeyPressed loop

         -- Plot random pixels
         RandSeed := Seed;
         I        := 0;
         while (not KeyPressed) and (I < NumPts) loop

            Inc (I);
            PutPixel
              (Integer (Random (Word (XMax))) + 1,
               Integer (Random (Word (YMax))) + 1,
               RandColor);
            null;
         end loop;

         -- Erase pixels
         RandSeed := Seed;
         I        := 0;
         while (not KeyPressed) and (I < NumPts) loop

            Inc (I);
            X     := Integer (Random (Word (XMax))) + 1;
            Y     := Integer (Random (Word (YMax))) + 1;
            Color := GetPixel (X, Y);
            if Color = RandColor then
               PutPixel (X, Y, 0);
            end if;
            null;
         end loop;

         null;
      end loop;

      WaitToGo;
      null;
   end PutPixelPlay; -- PutPixelPlay

   procedure PutImagePlay
      -- Demonstrate the GetImage and PutImage commands

   is

      r      : constant := 20;
      StartX : constant := 100;
      StartY : constant := 50;

      CurPort : ViewPortType;

      procedure MoveSaucer (X, Y : in out Integer; Width, Height : Integer) is

         Step : Integer;
      begin
         Step := Integer (Random (2 * r));
         if Odd (Step) then
            Step := -Step;
         end if;
         X    := X + Step;
         Step := Integer (Random (r));
         if Odd (Step) then
            Step := -Step;
         end if;
         Y := Y + Step;

         -- Make saucer bounce off viewport walls
         if (CurPort.x1 + X + Width - 1 > CurPort.x2) then
            X := CurPort.x1 - Width + 1;
         else
            if (X < 0) then
               X := 0;
            end if;
         end if;
         if (CurPort.y1 + Y + Height - 1 > CurPort.y2) then
            Y := CurPort.y2 - CurPort.y1 - Height + 1;
         else
            if (Y < 0) then
               Y := 0;
            end if;
         end if;
         null;
      end MoveSaucer; -- MoveSaucer

      Pausetime : Word;
      Saucer    : Pointer;
      X, Y      : Integer;
      ulx, uly  : Integer;
      lrx, lry  : Integer;
      Size      : Word;
   --        I         : Word;
   begin
      ClearDevice;
      FullPort;

      -- PaintScreen
      ClearDevice;
      MainWindow (To_TPString ("GetImage / PutImage Demonstration"));
      StatusLine (To_TPString ("Esc aborts or press a key..."));
      GetViewSettings (CurPort);

      -- DrawSaucer
      Ellipse (StartX, StartY, 0, 360, r, (r / 3) + 2);
      Ellipse (StartX, StartY - 4, 190, 357, r, r / 3);
      Line (StartX + 7, StartY - 6, StartX + 10, StartY - 12);
      Circle (StartX + 10, StartY - 12, 2);
      Line (StartX - 7, StartY - 6, StartX - 10, StartY - 12);
      Circle (StartX - 10, StartY - 12, 2);
      SetFillStyle (SolidFill, MaxColor);
      FloodFill (StartX + 1, StartY + 4, GetColor);

      -- ReadSaucerImage
      ulx := StartX - (r + 1);
      uly := StartY - 14;
      lrx := StartX + (r + 1);
      lry := StartY + (r / 3) + 3;

      Size := Word (ImageSize (ulx, uly, lrx, lry));
      GetMem (Saucer, Size);
      GetImage (ulx, uly, lrx, lry, Saucer);
      PutImage (ulx, uly, Saucer, XORPut);               -- erase image

      -- Plot some "stars"
      for I in 1 .. 1000 loop

         PutPixel (Integer (Random (Word (MaxX))), Integer (Random (Word (MaxY))), RandColor);
      end loop;

      X         := MaxX / 2;
      Y         := MaxY / 2;
      Pausetime := 70;

      -- Move the saucer around
      loop

         PutImage (X, Y, Saucer, XORPut);                 -- draw image
         Delay1 (Pausetime);
         PutImage (X, Y, Saucer, XORPut);                 -- erase image
         MoveSaucer (X, Y, lrx - ulx + 1, lry - uly + 1);  -- width/height
         null;
         Delay1 (500);
         exit when KeyPressed;
      end loop;

      FreeMem (Saucer, Size);
      WaitToGo;
      null;
   end PutImagePlay; -- PutImagePlay

   procedure PolyPlay
      -- Draw random polygons with random fill styles on the screen
   is

      MaxPts : constant := 5;

      Poly  : PolygonType (1 .. MaxPts);
      Color : Word;
   begin
      MainWindow (To_TPString ("FillPoly demonstration"));
      StatusLine (To_TPString ("Esc aborts or press a key..."));
      loop

         Color := RandColor;
         SetFillStyle (Random (11) + 1, Color);
         SetColor (Color);
         for I in 1 .. MaxPts loop

            Poly (I).X := Integer (Random (Word (MaxX)));
            Poly (I).Y := Integer (Random (Word (MaxY)));
         end loop;

         FillPoly (MaxPts, Poly);
         null;
         Delay1 (500);
         exit when KeyPressed;
      end loop;

      WaitToGo;
      null;
   end PolyPlay; -- PolyPlay

   procedure FillStylePlay
      -- Display all of the predefined fill styles available
   is

      Style    : Word;
      Width    : Integer;
      Height   : Integer;
      X, Y     : Integer;
      ViewInfo : ViewPortType;

      procedure DrawBox (X, Y : Integer) is
      begin
         Bar (X, Y, X + Width, Y + Height);
         Rectangle (X, Y, X + Width, Y + Height);
         OutTextXY (X + (Width / 2), Y + Height + 4, Int2Str (Style));
         Inc (Style);
         null;
      end DrawBox; -- DrawBox

   begin
      MainWindow (To_TPString ("Pre-defined fill styles"));
      GetViewSettings (ViewInfo);
      Width  := 2 * ((ViewInfo.x2 + 1) / 13);
      Height := 2 * ((ViewInfo.y2 - 10) / 10);
      X      := Width / 2;
      Y      := Height / 2;
      Style  := 0;
      for J in 1 .. 3 loop

         for I in 1 .. 4 loop

            DrawBox (X, Y);
            Inc (X, (Longint (Width) / 2) * 3);
            null;
         end loop;

         X := Width / 2;
         Inc (Y, (Longint (Height) / 2) * 3);
         null;
      end loop;

      SetTextJustify (LeftText, TopText);
      WaitToGo;
      null;
   end FillStylePlay; -- FillStylePlay

   procedure FillPatternPlay
      -- Display some user defined fill patterns
   is

      Patterns : constant array (0 .. Word (11)) of FillPatternType :=
        ((16#AA#, 16#55#, 16#AA#, 16#55#, 16#AA#, 16#55#, 16#AA#, 16#55#),
         (16#33#, 16#33#, 16#CC#, 16#CC#, 16#33#, 16#33#, 16#CC#, 16#CC#),
         (16#F0#, 16#F0#, 16#F0#, 16#F0#, 16#F#, 16#F#, 16#F#, 16#F#),
         (0, 16#10#, 16#28#, 16#44#, 16#28#, 16#10#, 0, 0),
         (0, 16#70#, 16#20#, 16#27#, 16#25#, 16#27#, 16#4#, 16#4#),
         (0, 0, 0, 16#18#, 16#18#, 0, 0, 0),
         (0, 0, 16#3C#, 16#3C#, 16#3C#, 16#3C#, 0, 0),
         (0, 16#7E#, 16#7E#, 16#7E#, 16#7E#, 16#7E#, 16#7E#, 0),
         (0, 0, 16#22#, 16#8#, 0, 16#22#, 16#1C#, 0),
         (16#FF#, 16#7E#, 16#3C#, 16#18#, 16#18#, 16#3C#, 16#7E#, 16#FF#),
         (0, 16#10#, 16#10#, 16#7C#, 16#10#, 16#10#, 0, 0),
         (0, 16#42#, 16#24#, 16#18#, 16#18#, 16#24#, 16#42#, 0));

      Style    : Word;
      Width    : Integer;
      Height   : Integer;
      X, Y     : Integer;
      ViewInfo : ViewPortType;

      procedure DrawBox (X, Y : Integer) is
      begin
         SetFillPattern (Patterns (Style), MaxColor);
         Bar (X, Y, X + Width, Y + Height);
         Rectangle (X, Y, X + Width, Y + Height);
         Inc (Style);
         null;
      end DrawBox; -- DrawBox

   begin
      MainWindow (To_TPString ("User defined fill styles"));
      GetViewSettings (ViewInfo);
      Width  := 2 * ((ViewInfo.x2 + 1) / 13);
      Height := 2 * ((ViewInfo.y2 - 10) / 10);
      X      := Width / 2;
      Y      := Height / 2;
      Style  := 0;
      for J in 1 .. 3 loop

         for I in 1 .. 4 loop

            DrawBox (X, Y);
            Inc (X, (Longint (Width) / 2) * 3);
            null;
         end loop;

         X := Width / 2;
         Inc (Y, (Longint (Height) / 2) * 3);
         null;
      end loop;

      SetTextJustify (LeftText, TopText);
      WaitToGo;
      null;
   end FillPatternPlay; -- FillPatternPlay

   procedure ColorPlay
      -- Display all of the colors available for the current driver and mode
   is

      Color    : Word;
      Width    : Integer;
      Height   : Integer;
      X, Y     : Integer;
      ViewInfo : ViewPortType;

      procedure DrawBox (X, Y : Integer) is
      begin
         SetFillStyle (SolidFill, Color);
         SetColor (Color);
         Bar (X, Y, X + Width, Y + Height);
         Rectangle (X, Y, X + Width, Y + Height);
         Color := GetColor;
         if Color = 0 then

            SetColor (MaxColor);
            Rectangle (X, Y, X + Width, Y + Height);
            null;
         end if;
         OutTextXY (X + (Width / 2), Y + Height + 4, Int2Str (Color));
         Color := Succ (Color) mod (MaxColor + 1);
         null;
      end DrawBox; -- DrawBox

   begin
      MainWindow (To_TPString ("Color demonstration"));
      Color := 1;
      GetViewSettings (ViewInfo);
      Width  := 2 * ((ViewInfo.x2 + 1) / 16);
      Height := 2 * ((ViewInfo.y2 - 10) / 10);
      X      := Width / 2;
      Y      := Height / 2;
      for J in 1 .. 3 loop

         for I in 1 .. 5 loop

            DrawBox (X, Y);
            Inc (X, (Longint (Width) / 2) * 3);
            null;
         end loop;

         X := Width / 2;
         Inc (Y, (Longint (Height) / 2) * 3);
         null;
      end loop;

      WaitToGo;
      null;
   end ColorPlay; -- ColorPlay

   procedure PalettePlay
      -- Demonstrate the use of the SetPalette command
   is

      XBars : constant := 15;
      YBars : constant := 10;

      X, Y     : Integer;
      Color    : Word;
      ViewInfo : ViewPortType;
      Width    : Integer;
      Height   : Integer;
      OldPal   : PaletteType;
   begin
      GetPalette (OldPal);
      MainWindow (To_TPString ("Palette demonstration"));
      StatusLine (To_TPString ("Press any key..."));
      GetViewSettings (ViewInfo);
      Width  := (ViewInfo.x2 - ViewInfo.x1) / XBars;
      Height := (ViewInfo.y2 - ViewInfo.y1) / YBars;
      X      := 0;
      Y      := 0;
      Color  := 0;
      for J in 1 .. YBars loop

         for I in 1 .. XBars loop

            SetFillStyle (SolidFill, Color);
            Bar (X, Y, X + Width, Y + Height);
            Inc (X, Longint (Width) + 1);
            Inc (Color);
            Color := Color mod (MaxColor + 1);
            null;
         end loop;

         X := 0;
         Inc (Y, Longint (Height) + 1);
         null;
      end loop;

      loop

         SetPalette (Random (GetMaxColor + 1), Shortint (Random (65)));
         null;
         Delay1 (500);
         exit when KeyPressed;
      end loop;

      SetAllPalette (OldPal);
      WaitToGo;
      null;
   end PalettePlay; -- PalettePlay

   procedure CrtModePlay
      -- Demonstrate the use of RestoreCrtMode and SetGraphMode
   is

      ViewInfo : ViewPortType;
      Ch       : Char;
   begin
      MainWindow (To_TPString ("SetGraphMode / RestoreCrtMode demo"));
      GetViewSettings (ViewInfo);
      SetTextJustify (CenterText, CenterText);
      OutTextXY
        ((ViewInfo.x2 - ViewInfo.x1) / 2,
         (ViewInfo.y2 - ViewInfo.y1) / 2,
         To_TPString ("Now you are in graphics mode"));
      StatusLine (To_TPString ("Press any key for text mode..."));
      loop
         null;
         Delay1 (500);
         exit when KeyPressed;
      end loop;

      Ch := ReadKey;
      if Ch = Char'Val (0) then
         Ch := ReadKey;
      end if;    -- trap function keys
      RestoreCrtMode;
      Writeln ("Now you are in text mode.");
      Write ("Press any key to go back to graphics...");
      loop
         null;
         Delay1 (500);
         exit when KeyPressed;
      end loop;

      Ch := ReadKey;
      if Ch = Char'Val (0) then
         Ch := ReadKey;
      end if;    -- trap function keys
      SetGraphMode (GetGraphMode);
      MainWindow (To_TPString ("SetGraphMode / RestoreCrtMode demo"));
      SetTextJustify (CenterText, CenterText);
      OutTextXY
        ((ViewInfo.x2 - ViewInfo.x1) / 2,
         (ViewInfo.y2 - ViewInfo.y1) / 2,
         To_TPString ("Back in graphics mode..."));
      WaitToGo;
      null;
   end CrtModePlay; -- CrtModePlay

   procedure LineStylePlay
      -- Demonstrate the predefined line styles available
   is

      --        Style    : Word;
      Step     : Longint;
      X, Y     : Integer;
      ViewInfo : ViewPortType;

   begin
      ClearDevice;
      DefaultColors;
      MainWindow (To_TPString ("Pre-defined line styles"));
      GetViewSettings (ViewInfo);
      X    := 35;
      Y    := 10;
      Step := Longint (ViewInfo.x2 - ViewInfo.x1) / 11;
      SetTextJustify (LeftText, TopText);
      OutTextXY (X, Y, To_TPString ("NormWidth"));
      SetTextJustify (CenterText, TopText);
      for Style in 0 .. Word (3) loop

         SetLineStyle (Style, 0, NormWidth);
         Line (X, Y + 20, X, ViewInfo.y2 - 40);
         OutTextXY (X, ViewInfo.y2 - 30, Int2Str (Style));
         Inc (X, Step);
         null;
      end loop;

      Inc (X, 2 * Step);
      SetTextJustify (LeftText, TopText);
      OutTextXY (X, Y, To_TPString ("ThickWidth"));
      SetTextJustify (CenterText, TopText);
      for Style in 0 .. Word (3) loop

         SetLineStyle (Style, 0, ThickWidth);
         Line (X, Y + 20, X, ViewInfo.y2 - 40);
         OutTextXY (X, ViewInfo.y2 - 30, To_TPString (Word'Image (Style)));
         Inc (X, Step);
         null;
      end loop;

      SetTextJustify (LeftText, TopText);
      WaitToGo;
      null;
   end LineStylePlay; -- LineStylePlay

   procedure UserLineStylePlay
      -- Demonstrate user defined line styles
   is

      Style    : Word;
      X, Y, I  : Integer;
      ViewInfo : ViewPortType;
   begin
      MainWindow (To_TPString ("User defined line styles"));
      GetViewSettings (ViewInfo);
      X     := 4;
      Y     := 10;
      Style := 0;
      I     := 0;
      while X < ViewInfo.x2 - 4 loop

         --$B+
         Style := Word (Word1 (Style) or Word1 (2 ** Natural (I mod 16)));
         --$B-
         SetLineStyle (UserBitLn, Style, NormWidth);
         Line (X, Y, X, (ViewInfo.y2 - ViewInfo.y1) - Y);
         Inc (X, 5);
         Inc (I);
         if Style = 65535 then

            I     := 0;
            Style := 0;
            null;
         end if;
         null;
      end loop;

      WaitToGo;
      null;
   end UserLineStylePlay; -- UserLineStylePlay

   procedure SayGoodbye
      -- Say goodbye and then exit the program
   is

      ViewInfo : ViewPortType;
   begin
      MainWindow (To_TPString (""));
      GetViewSettings (ViewInfo);
      SetTextStyle (TriplexFont, HorizDir, 4);
      SetTextJustify (CenterText, CenterText);
      OutTextXY
        ((ViewInfo.x2 - ViewInfo.x1) / 2,
         (ViewInfo.y2 - ViewInfo.y1) / 2,
         To_TPString ("That's all folks!"));
      StatusLine (To_TPString ("Press any key to quit..."));
      loop
         null;
         Delay1 (500);
         exit when KeyPressed;
      end loop;

      null;
   end SayGoodbye; -- SayGoodbye

begin -- program body
   Initialize;
   ReportStatus;

   AspectRatioPlay;
   FillEllipsePlay;
   SectorPlay;
   WriteModePlay;

   ColorPlay;
   -- PalettePlay only intended to work on these drivers:
   if (GraphDriver = EGA) or
      (GraphDriver = EGA64) or
      (GraphDriver = VGA) or
      (GraphDriver = VESA16)
   then -- - LastDriverNum) then
      PalettePlay;
   end if;
   PutPixelPlay;
   PutImagePlay;
   RandBarPlay;
   BarPlay;
   Bar3DPlay;
   ArcPlay;
   CirclePlay;
   PiePlay;
   LineToPlay;
   LineRelPlay;
   LineStylePlay;
   UserLineStylePlay;
   TextDump;
   TextPlay;
   CrtModePlay;
   FillStylePlay;
   FillPatternPlay;
   PolyPlay;
   SayGoodbye;
   CloseGraph;
   null;
end BGIDemo;
