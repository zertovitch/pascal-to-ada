--************************************************
--
--   Turbo Art Demo Program
--   Copyright (c) 1992 by Borland International
--
--************************************************

-- Pascal to Ada translation by Pascal Pignard August 2002

with TP7, TP7.Crt, TP7.Graph, TP7.System;
use  TP7, TP7.Crt, TP7.Graph, TP7.System;

procedure Arty is

   --
   --  This program is a demonstration of the Borland Graphics Interface (BGI)
   --
   --  Runtime Commands for ARTY
   --  -------------------------
   --  <B>   - changes background color
   --  <C>   - changes drawcolor
   --  <ESC> - exits program
   --  Any other key pauses, then regenerates the drawing
   --
   --  Note: If a /H command-line parameter is specified, the highest
   --        resolution mode will be used (if possible).
   --

   subtype Integer is TP7.TPInteger;
   subtype String is TP7.TPString;

   RepertoireBGI : constant String := To_TPString ("C:\TP\BGI");

   Memory  : constant Integer := 100;
   Windows : constant Integer := 4;

   type ResolutionPreference is (Lower, Higher);
   type ColorList is array (1 .. Windows) of Integer;

   Xmax, Ymax, ViewXmax, ViewYmax : Integer;

   type TLine is record
      LX1, LY1 : Integer;
      LX2, LY2 : Integer;
      LColor   : ColorList;
   end record;
   Line : array (1 .. Memory) of TLine;

   X1, X2, Y1, Y2, CurrentLine, ColorCount, IncrementCount, DeltaX1, DeltaY1, DeltaX2, DeltaY2 :
     Integer;
   Colors                                                                                      :
     ColorList;
   Ch                                                                                          :
     Char;
   BackColor                                                                                   :
     Integer;
   GraphDriver, GraphMode                                                                      :
     Integer;
   MaxColors                                                                                   :
     Word;
   MaxDelta                                                                                    :
     Integer;
   ChangeColors                                                                                :
     Boolean;

   procedure Frame is
   begin
      SetViewPort
        (0,
         0,
         Xmax,
         Ymax - (Integer (TextHeight (To_TPString (+'M'))) + 4) - 1,
         ClipOn);
      SetColor (MaxColors);
      Rectangle
        (0,
         0,
         Xmax - 1,
         (Ymax - (Integer (TextHeight (To_TPString (+'M'))) + 4) - 1) - 1);
      SetViewPort
        (1,
         1,
         Xmax - 2,
         (Ymax - (Integer (TextHeight (To_TPString (+'M'))) + 4) - 1) - 2,
         ClipOn);
      null;
   end Frame;  -- Frame

   procedure FullPort
      -- Set the view port to the entire screen
   is
   begin
      SetViewPort (0, 0, Xmax, Ymax, ClipOn);
      null;
   end FullPort; -- FullPort

   procedure MessageFrame (Msg : String) is
   begin
      FullPort;
      SetColor (MaxColors);
      SetTextStyle (DefaultFont, HorizDir, 1);
      SetTextJustify (CenterText, TopText);
      SetLineStyle (SolidLn, 0, NormWidth);
      SetFillStyle (EmptyFill, 0);
      Bar (0, Ymax - (Integer (TextHeight (To_TPString (+'M'))) + 4), Xmax, Ymax);
      Rectangle (0, Ymax - (Integer (TextHeight (To_TPString (+'M'))) + 4), Xmax, Ymax);
      OutTextXY (Xmax / 2, Ymax - (Integer (TextHeight (To_TPString (+'M'))) + 2), Msg);
      -- Go back to the main window
      Frame;
      null;
   end MessageFrame;  -- MessageFrame

   procedure WaitToGo is

      Ch : Char;
   begin
      MessageFrame (To_TPString ("Press any key to continue... Esc aborts"));
      loop
         null;
         exit when KeyPressed;
      end loop;

      Ch := ReadKey;
      if Ch = Char'Val (27) then
         CloseGraph;
         Write ("All done.");
         Writeln;
         Halt (1);
         null;
      else
         ClearViewPort;
      end if;
      MessageFrame (To_TPString ("Press a key to stop action, Esc quits."));
      null;
   end WaitToGo; -- WaitToGo

   procedure TestGraphError (GraphErr : Integer) is
   begin
      if GraphErr /= grOk then
         Write ("Graphics error: ");
         Write (To_String (GraphErrorMsg (GraphErr)));
         Writeln;
         loop
            null;
            exit when KeyPressed;
         end loop;

         Ch := ReadKey;
         Halt (1);
         null;
      end if;
      null;
   end TestGraphError;

   procedure Init is

      --    Err: integer;
      StartX, StartY : Integer;
      Resolution     : ResolutionPreference;
      s              : String (1 .. 255 + 1);
   begin
      Resolution := Lower;
      if ParamCount > 0 then
         s := ParamStr (1);
         if s (1) = '/' then
            if UpCase (s (2)) = 'H' then
               Resolution := Higher;
            end if;
         end if;
         null;
      end if;

      CurrentLine    := 1;
      ColorCount     := 0;
      IncrementCount := 0;
      Ch             := ' ';
      GraphDriver    := Detect;
      DetectGraph (GraphDriver, GraphMode);
      TestGraphError (GraphResult);
      case GraphDriver is
      when CGA =>
         MaxDelta    := 7;
         GraphDriver := CGA;
         GraphMode   := CGAC1;
         null;

      when MCGA =>
         MaxDelta := 7;
         case GraphMode is
            when MCGAMed | MCGAHi =>
               GraphMode := MCGAC1;
            when others =>
               null;
         end case;

         null;

      when EGA =>
         MaxDelta := 16;
         if Resolution = Lower then
            GraphMode := EGALo;
         else
            GraphMode := EGAHi;
         end if;
         null;

      when EGA64 =>
         MaxDelta := 16;
         if Resolution = Lower then
            GraphMode := EGA64Lo;
         else
            GraphMode := EGA64Hi;
         end if;
         null;

      when HercMono =>
         MaxDelta := 16;
      when EGAMono =>
         MaxDelta := 16;
      when PC3270 =>
         MaxDelta    := 7;
         GraphDriver := CGA;
         GraphMode   := CGAC1;
         null;

      when ATT400 =>
         case GraphMode is
            when ATT400C1 | ATT400C2 | ATT400Med | ATT400Hi =>

               MaxDelta  := 7;
               GraphMode := ATT400C1;
               null;
            when others =>
               null;
         end case;

      when VGA =>
         MaxDelta := 16;
         null;
      when others =>
         null;
      end case;

      InitGraph (GraphDriver, GraphMode, RepertoireBGI);
      TestGraphError (GraphResult);
      SetTextStyle (DefaultFont, HorizDir, 1);
      SetTextJustify (CenterText, TopText);

      MaxColors    := GetMaxColor;
      BackColor    := 0;
      ChangeColors := True;
      Xmax         := GetMaxX;
      Ymax         := GetMaxY;
      ViewXmax     := Xmax - 2;
      ViewYmax     := (Ymax - (Integer (TextHeight (To_TPString (+'M'))) + 4) - 1) - 2;
      StartX       := Xmax / 2;
      StartY       := Ymax / 2;
      for I in 1 .. Memory loop
         Line (I).LX1    := StartX;
         Line (I).LX2    := StartX;
         Line (I).LY1    := StartY;
         Line (I).LY2    := StartY;
         Line (I).LColor := (others => TP7.Graph.White);
      end loop;

      X1 := StartX;
      X2 := StartX;
      Y1 := StartY;
      Y2 := StartY;
      null;
   end Init; --init

   procedure AdjustX (X, DeltaX : in out Integer) is

      TestX : Integer;
   begin
      TestX := X + DeltaX;
      if (TestX < 1) or (TestX > ViewXmax) then
         TestX  := X;
         DeltaX := -DeltaX;
         null;
      end if;
      X := TestX;
      null;
   end AdjustX;

   procedure AdjustY (Y, DeltaY : in out Integer) is

      TestY : Integer;
   begin
      TestY := Y + DeltaY;
      if (TestY < 1) or (TestY > ViewYmax) then
         TestY  := Y;
         DeltaY := -DeltaY;
         null;
      end if;
      Y := TestY;
      null;
   end AdjustY;

   procedure SelectNewColors is
   begin
      if not ChangeColors then
         return;
      end if;
      Colors (1) := Integer (Random (MaxColors)) + 1;
      Colors (2) := Integer (Random (MaxColors)) + 1;
      Colors (3) := Integer (Random (MaxColors)) + 1;
      Colors (4) := Integer (Random (MaxColors)) + 1;
      ColorCount := 3 * (1 + Integer (Random (5)));
      null;
   end SelectNewColors;

   procedure SelectNewDeltaValues is
   begin
      DeltaX1        := Integer (Random (Word (MaxDelta))) - (MaxDelta / 2);
      DeltaX2        := Integer (Random (Word (MaxDelta))) - (MaxDelta / 2);
      DeltaY1        := Integer (Random (Word (MaxDelta))) - (MaxDelta / 2);
      DeltaY2        := Integer (Random (Word (MaxDelta))) - (MaxDelta / 2);
      IncrementCount := 2 * (1 + Integer (Random (4)));
      null;
   end SelectNewDeltaValues;

   procedure SaveCurrentLine (CurrentColors : ColorList) is
   begin
      --HELP!! WITH
      declare
   --  r : <type> RENAMES  Line(CurrentLine). ;
      begin
         Line (CurrentLine).LX1    := X1;
         Line (CurrentLine).LY1    := Y1;
         Line (CurrentLine).LX2    := X2;
         Line (CurrentLine).LY2    := Y2;
         Line (CurrentLine).LColor := CurrentColors;
         null;
      end;
      null;
   end SaveCurrentLine;

   procedure Draw (x1, y1, x2, y2, color : Integer) is
   begin
      SetColor (Word (color));
      TP7.Graph.Line (x1, y1, x2, y2);
      null;
   end Draw;

   procedure Regenerate is
   begin
      Frame;
      for I in 1 .. Memory loop
         Draw (Line (I).LX1, Line (I).LY1, Line (I).LX2, Line (I).LY2, Line (I).LColor (1));
         Draw
           (ViewXmax - Line (I).LX1,
            Line (I).LY1,
            ViewXmax - Line (I).LX2,
            Line (I).LY2,
            Line (I).LColor (2));
         Draw
           (Line (I).LX1,
            ViewYmax - Line (I).LY1,
            Line (I).LX2,
            ViewYmax - Line (I).LY2,
            Line (I).LColor (3));
         Draw
           (ViewXmax - Line (I).LX1,
            ViewYmax - Line (I).LY1,
            ViewXmax - Line (I).LX2,
            ViewYmax - Line (I).LY2,
            Line (I).LColor (4));
      end loop;

      WaitToGo;
      Frame;
      null;
   end Regenerate;

   procedure Updateline is
   begin
      Inc (CurrentLine);
      if CurrentLine > Memory then
         CurrentLine := 1;
      end if;
      Dec (ColorCount);
      Dec (IncrementCount);
      null;
   end Updateline;

   procedure CheckForUserInput is
   begin
      if KeyPressed then
         Ch := ReadKey;
         if UpCase (Ch) = 'B' then
            if BackColor > Integer (MaxColors) then
               BackColor := 0;
            else
               Inc (BackColor);
            end if;
            SetBkColor (Word (BackColor));
            null;
         else
            if UpCase (Ch) = 'C' then
               if ChangeColors then
                  ChangeColors := False;
               else
                  ChangeColors := True;
               end if;
               ColorCount := 0;
               null;
            else
               if Ch /= Char'Val (27) then
                  Regenerate;
               end if;
            end if;
         end if;
         null;
      end if;
      null;
   end CheckForUserInput;

   procedure DrawCurrentLine is
      c1, c2, c3, c4 : Integer;
   begin
      c1 := Colors (1);
      c2 := Colors (2);
      c3 := Colors (3);
      c4 := Colors (4);
      if MaxColors = 1 then
         c2 := c1;
         c3 := c1;
         c4 := c1;
         null;
      end if;

      Draw (X1, Y1, X2, Y2, c1);
      Draw (ViewXmax - X1, Y1, ViewXmax - X2, Y2, c2);
      Draw (X1, ViewYmax - Y1, X2, ViewYmax - Y2, c3);
      if MaxColors = 3 then
         c4 := Integer (Random (3)) + 1;
      end if; -- alternate colors
      Draw (ViewXmax - X1, ViewYmax - Y1, ViewXmax - X2, ViewYmax - Y2, c4);
      SaveCurrentLine (Colors);
      null;
   end DrawCurrentLine;

   procedure EraseCurrentLine is
   begin
      Draw
        (Line (CurrentLine).LX1,
         Line (CurrentLine).LY1,
         Line (CurrentLine).LX2,
         Line (CurrentLine).LY2,
         0);
      Draw
        (ViewXmax - Line (CurrentLine).LX1,
         Line (CurrentLine).LY1,
         ViewXmax - Line (CurrentLine).LX2,
         Line (CurrentLine).LY2,
         0);
      Draw
        (Line (CurrentLine).LX1,
         ViewYmax - Line (CurrentLine).LY1,
         Line (CurrentLine).LX2,
         ViewYmax - Line (CurrentLine).LY2,
         0);
      Draw
        (ViewXmax - Line (CurrentLine).LX1,
         ViewYmax - Line (CurrentLine).LY1,
         ViewXmax - Line (CurrentLine).LX2,
         ViewYmax - Line (CurrentLine).LY2,
         0);
      null;
   end EraseCurrentLine;

   procedure DoArt is
   begin
      SelectNewColors;
      loop

         EraseCurrentLine;
         if ColorCount = 0 then
            SelectNewColors;
         end if;

         if IncrementCount = 0 then
            SelectNewDeltaValues;
         end if;

         AdjustX (X1, DeltaX1);
         AdjustX (X2, DeltaX2);
         AdjustY (Y1, DeltaY1);
         AdjustY (Y2, DeltaY2);

         if Random (5) = 3 then
            X1 := (X1 + X2) / 2; -- shorten the lines
            Y2 := (Y1 + Y2) / 2;
            null;
         end if;

         DrawCurrentLine;
         Updateline;
         CheckForUserInput;
         null;
         exit when Ch = Char'Val (27);
      end loop;

      null;
   end DoArt;

begin
   Init;
   Frame;
   MessageFrame (To_TPString ("Press a key to stop action, Esc quits."));
   DoArt;
   CloseGraph;
   RestoreCrtMode;
   Write ("The End.");
   Writeln;
   null;
end Arty;
