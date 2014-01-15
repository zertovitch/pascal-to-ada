-------------------------------------------------------------------------------
-- NOM DU CSU (principal)           : editfont.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 1.0a
-- DATE DE LA DERNIERE MISE A JOUR  : 24 octobre 2012
-- ROLE DU CSU                      : Édite une fonte graphique
--
--
-- FONCTIONS EXPORTEES DU CSU       :
--
-- FONCTIONS LOCALES DU CSU         :
--
--
-- NOTES                            :
--
-- COPYRIGHT                        : (c) Pascal Pignard 1990-2012
-- LICENCE                          : CeCILL V2 (http://www.cecill.info)
-- CONTACT                          : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------
-- Translated on 24-Oct-2012 by (New) P2Ada v. 28-Oct-2009

with TP7.System, TP7.Graph, TP7.Crt;
use  TP7, TP7.System, TP7.Graph, TP7.Crt;

procedure EditFont is

   MaxBuff : constant := 30000;

   type Tampon is array (0 .. MaxBuff) of Byte1;
   --     type BuffPtr is access Tampon;
   subtype Char03 is String (1 .. 4); --array (0 .. 3) of Char;
   --  procedure Dispose is new Ada.Unchecked_Deallocation(Tampon, BuffPtr);
   type Partie2 is record
   -- Font description : chars until <CR><LF><EOF>
   -- Header size : word
   -- Font name : 4 chars
   -- Font file size : word
   -- Font driver major version : byte
   -- Font driver minor version : byte
   -- ID3 : word -> 16#0100#
   -- Zeros : padding until end of the header
      Description : String (1 .. 256);
      DebPart3    : Integer;
      Nom         : Char03;
      DataSize    : Integer;
      Major       : Byte;
      Minor       : Byte;
   end record;
   type Partie3 is record
   -- ID4 : byte -> 16#2B# (stroke font)
   -- Number of character : word
   -- Undefined : byte
   -- First defined character : ASCII
   -- Offset of character definitions : word
   -- Scan flag : byte
   -- Distance from origin to top of capital : byte
   -- Distance from origin to baseline : byte
   -- Distance from origin to bottom descender : byte
      NbrCar        : Byte;
      FirstCar      : Char;
      ShiftDef      : Integer;
      HauteurMaj    : Shortint;
      OffsetOrigine : Shortint;
      TailleJambage : Shortint;
   end record;
   type AdrCar is array (Chr (0) .. Chr (255)) of Integer;
   type TailleCar is array (Chr (0) .. Chr (255)) of Byte;
   type CodeFont is record
      NomFont      : String (1 .. 256);
      Part2Ptr     : Partie2;
      Part3Ptr     : Partie3;
      AdrCarPtr    : AdrCar;
      TailleCarPtr : TailleCar;
      BuffFont     : Tampon;
   end record;
   --  procedure Dispose is new Ada.Unchecked_Deallocation(Partie2, Part2Ptr);
   --  procedure Dispose is new Ada.Unchecked_Deallocation(Partie3, Part3Ptr);
   --  procedure Dispose is new Ada.Unchecked_Deallocation(AdrCar, AdrCarPtr);
   --  procedure Dispose is new Ada.Unchecked_Deallocation(TailleCar, TailleCarPtr);
   type Commande is (Move, Line);
   type CmdPart;
   type CmdPtr is access CmdPart;
   --  procedure Dispose is new Ada.Unchecked_Deallocation(CmdPart, CmdPtr);
   type CmdPart is record
      CmdType : Commande;
      X, Y    : Integer;
      CmdSui  : CmdPtr;
   end record;

   Ch, Car : Char;
   Font    : CodeFont;
   DebFont : array (Chr (0) .. Chr (255)) of CmdPtr;
   FinFont : array (Chr (0) .. Chr (255)) of CmdPtr;
   Zoom    : Boolean;
   Mode    : Integer;
   DebHeap : Pointer;

   function Hexa (X : Integer) return String is
      Result_Hexa             : String (1 .. 256);
      P2Ada_no_keyword_Digits : constant array (0 .. 15) of Char := "0123456789abcdef";
      Chn                     : String (1 .. 256);

   begin
      Assign_String (Chn, +'$');
      for I in reverse 0 .. 3 loop
         Assign_String
           (Chn,
            Chn +
            P2Ada_no_keyword_Digits (Integer ((Word1 (X) / (2 ** (4 * I))) and 16#000F#)));
      end loop;
      Result_Hexa := Chn;
      return Result_Hexa;
   end Hexa;

   procedure ZLineTo (X, Y : Integer) is
   begin
      if Zoom then
         LineTo (GetMaxY / 128 * (X + 64) + 40, GetMaxY / 128 * (64 - Y) + 40);
      else
         LineTo (100 + X, 100 - Y);
      end if;
   end ZLineTo;

   procedure ZMoveTo (X, Y : Integer) is
   begin
      if Zoom then
         MoveTo (GetMaxY / 128 * (X + 64) + 40, GetMaxY / 128 * (64 - Y) + 40);
      else
         MoveTo (100 + X, 100 - Y);
      end if;
   end ZMoveTo;

   procedure InitTable is
   begin
      Release (DebHeap);
      --      FillChar(DebFont,(DebFont'size / 8),0);
      for Ind in DebFont'Range loop
         DebFont (Ind) := null;
      end loop;
      --      FillChar(FinFont,(FinFont'size / 8),0);
      for Ind in FinFont'Range loop
         FinFont (Ind) := null;
      end loop;
      Car := 'A';
   end InitTable;

   procedure AnalyseCar (Ind : Integer; DebCarPtr, FinCarPtr : in out CmdPtr) is
      Cmd : CmdPtr;

      procedure DecodeVect (PX, PY : Integer) is
         Com : Byte;

         procedure DecodeCoord (Coord : Integer; Result : in out Integer) is
         begin
            Result := Integer (Word1 (Coord) and 16#7f#);
            if Result >= 64 then
               Result := Result - 128;
            end if;
         end DecodeCoord;

      begin
         Cmd := new CmdPart;
         -- [P2Ada]: WITH instruction
         declare
            P2Ada_Var_1 : CmdPart renames Cmd.all;
         begin
            Com :=
              Byte ((Word1 (PX) and 16#80#) / (2 ** 6) +
                    (Word1 (PY) and 16#80#) / (2 ** 7));
            case Com is
               when 2 =>
                  P2Ada_Var_1.CmdType := Move;
               when 3 =>
                  P2Ada_Var_1.CmdType := Line;
               when others =>
                  null;  -- [P2Ada]: no otherwise / else in Pascal
            end case;
            DecodeCoord (PX, P2Ada_Var_1.X);
            DecodeCoord (PY, P2Ada_Var_1.Y);
            P2Ada_Var_1.CmdSui := null;
            if FinCarPtr = null then
               DebCarPtr := Cmd;
            else
               FinCarPtr.CmdSui := Cmd;
            end if;
            FinCarPtr := Cmd;
         end; -- [P2Ada]: end of WITH

      end DecodeVect;

   begin
      DebCarPtr := null;
      FinCarPtr := null;
      -- [P2Ada]: WITH instruction
      declare
         P2Ada_Var_2 : CodeFont renames Font;
         Ind1        : Integer := Ind;
      begin
         while P2Ada_Var_2.BuffFont (Ind1) /= 0 loop

            DecodeVect
              (Byte (P2Ada_Var_2.BuffFont (Ind1)),
               Byte (P2Ada_Var_2.BuffFont (Ind1 + 1)));
            Ind1 := Ind1 + 2;
         end loop;
      end; -- [P2Ada]: end of WITH

   end AnalyseCar;

   procedure DecodeFont (Font : in out CodeFont) is
      procedure ChargeFont is
         FichFont : File;
         Ind      : Integer;

      begin
         -- [P2Ada]: WITH instruction
         declare
            P2Ada_Var_3 : CodeFont renames Font;
         begin

            Writeln;
            Write ("Charge ");
            Writeln (P2Ada_Var_3.NomFont);
            Assign (FichFont, P2Ada_Var_3.NomFont);
            Reset (FichFont, 1);
            Ind := FileSize (FichFont);
            if Ind > MaxBuff then
               Writeln ("Fichier trop important !");
            else
               --PP            FillChar(P2Ada_Var_3.BuffFont,(P2Ada_Var_3.BuffFont'size / 8),0);
               BlockRead (FichFont, P2Ada_Var_3.BuffFont'Address, Ind);
            end if;
            Close (FichFont);

         end; -- [P2Ada]: end of WITH

      end ChargeFont;

      procedure AnalyseFont is
         Car                                  : Char;
         Ind, DebAdrCar, DebTailleCar, DebDef : Integer;

      begin
         -- [P2Ada]: WITH instruction
         declare
            P2Ada_Var_4 : CodeFont renames Font;
         begin
            Ind := 0;
            while P2Ada_Var_4.BuffFont (Ind) /= 16#1a# loop
               Font.Part2Ptr.Description (Ind + 1) := Chr (Byte (Font.BuffFont (Ind)));
               Ind                                 := Ind + 1;
            end loop;
            Ind := Ind + 1;
            --PP          P2Ada_Var_4.Part2Ptr:=Ptr(DSeg,Ofs(P2Ada_Var_4.BuffFont(Ind)));
            Font.Part2Ptr.DebPart3 := Byte (Font.BuffFont (Ind)) +
                                      Byte (Font.BuffFont (Ind + 1)) * 256;
            Ind                    := Ind + 2;
            for Index in Char03'Range loop
               Font.Part2Ptr.Nom (Index) := Chr (Byte (Font.BuffFont (Ind + Index - 1)));
            end loop;
            Ind                    := Ind + 4;
            Font.Part2Ptr.DataSize := Byte (Font.BuffFont (Ind)) +
                                      Byte (Font.BuffFont (Ind + 1)) * 256;
            Ind                    := Ind + 2;
            Font.Part2Ptr.Major    := Byte (Font.BuffFont (Ind));
            Ind                    := Ind + 1;
            Font.Part2Ptr.Minor    := Byte (Font.BuffFont (Ind));
            Ind                    := Font.Part2Ptr.DebPart3;
            -- [P2Ada]: WITH instruction
            declare
               P2Ada_Var_5 : Partie2 renames P2Ada_Var_4.Part2Ptr;
            begin
               --PP
               --P2Ada_Var_4.Part3Ptr:=Ptr(DSeg,Ofs(P2Ada_Var_4.BuffFont(P2Ada_Var_5.DebPart3)));
               -- [P2Ada]: WITH instruction
               declare
                  P2Ada_Var_6 : Partie3 renames P2Ada_Var_4.Part3Ptr;
                  function To_Shortint (V : Byte) return Shortint is
                  begin
                     if V > 127 then
                        return V - 256;
                     else
                        return V;
                     end if;
                  end To_Shortint;
               begin
                  Ind                         := Ind + 1;
                  Font.Part3Ptr.NbrCar        := Byte (Font.BuffFont (Ind));
                  Ind                         := Ind + 3;
                  Font.Part3Ptr.FirstCar      := Chr (Byte (Font.BuffFont (Ind)));
                  Ind                         := Ind + 1;
                  Font.Part3Ptr.ShiftDef      := Byte (Font.BuffFont (Ind)) +
                                                 Byte (Font.BuffFont (Ind + 1)) * 256;
                  Ind                         := Ind + 3;
                  Font.Part3Ptr.HauteurMaj    := To_Shortint (Byte (Font.BuffFont (Ind)));
                  Ind                         := Ind + 1;
                  Font.Part3Ptr.OffsetOrigine := To_Shortint (Byte (Font.BuffFont (Ind)));
                  Ind                         := Ind + 1;
                  Font.Part3Ptr.TailleJambage := To_Shortint (Byte (Font.BuffFont (Ind)));
                  DebAdrCar                   := P2Ada_Var_5.DebPart3 + 16#10#;
                  --PP
                  --P2Ada_Var_4.AdrCarPtr:=Ptr(DSeg,Ofs(P2Ada_Var_4.BuffFont(DebAdrCar)));
                  for Index in
                       Font.Part3Ptr.FirstCar ..
                       Chr (Ord (Font.Part3Ptr.FirstCar) + Font.Part3Ptr.NbrCar - 1)
                  loop
                     Font.AdrCarPtr (Index) :=
                       Byte (Font.BuffFont (DebAdrCar +
                                            (Ord (Index) - Ord (Font.Part3Ptr.FirstCar)) * 2)) +
                       Byte (Font.BuffFont (DebAdrCar +
                                            (Ord (Index) - Ord (Font.Part3Ptr.FirstCar)) * 2 +
                                            1)) *
                       256;
                  end loop;
                  DebTailleCar := P2Ada_Var_5.DebPart3 + 16#10# + 2 * P2Ada_Var_6.NbrCar;
                  --PP
                  --P2Ada_Var_4.TailleCarPtr:=Ptr(DSeg,Ofs(P2Ada_Var_4.BuffFont(DebTailleCar)));
                  for Index in
                       Font.Part3Ptr.FirstCar ..
                       Chr (Ord (Font.Part3Ptr.FirstCar) + Font.Part3Ptr.NbrCar - 1)
                  loop
                     Font.TailleCarPtr (Index) :=
                       Byte (Font.BuffFont (DebTailleCar +
                                            Ord (Index) -
                                            Ord (Font.Part3Ptr.FirstCar)));
                  end loop;
                  DebDef               := P2Ada_Var_5.DebPart3 + 16#10# + 3 * P2Ada_Var_6.NbrCar;
                  P2Ada_Var_6.ShiftDef := DebDef;
                  Write ("adresses:");
                  Write (Hexa (DebAdrCar));
                  Write (+' ');
                  Write ("taille:");
                  Write (Hexa (DebTailleCar));
                  Write (+' ');
                  Write ("définition:");
                  Write (Hexa (DebDef));
                  Write (+' ');
                  Write ("décalages:");
                  Writeln (Hexa (P2Ada_Var_6.ShiftDef));
                  Write ("   ");
                  for Rang in 0 .. P2Ada_Var_6.NbrCar - 1 loop
                     if (P2Ada_Var_4.AdrCarPtr (Chr (Ord (P2Ada_Var_6.FirstCar) + Rang)) /= 0) or
                        (P2Ada_Var_4.TailleCarPtr (Chr (Ord (P2Ada_Var_6.FirstCar) + Rang)) /=
                         0)
                     then
                        Car := Chr (Ord (P2Ada_Var_6.FirstCar) + Rang);
                        Write (+Car);
                        AnalyseCar
                          (DebDef +
                           P2Ada_Var_4.AdrCarPtr (Chr (Ord (P2Ada_Var_6.FirstCar) + Rang)),
                           DebFont (Car),
                           FinFont (Car));
                     else
                        Writeln;
                        Writeln ("erreur" + Car);
                     end if;
                  end loop;
                  Writeln;
               end; -- [P2Ada]: end of WITH
            end; -- [P2Ada]: end of WITH
         end; -- [P2Ada]: end of WITH
         Writeln ("Description : " + Font.Part2Ptr.Description);
         Writeln ("Nom : " + Font.Part2Ptr.Nom);
         Writeln ("Taille : " + Font.Part2Ptr.DataSize'Img);
         Writeln ("Major version : " + Font.Part2Ptr.Major'Img);
         Writeln ("Minor version : " + Font.Part2Ptr.Minor'Img);
         Writeln ("Nb car : " + Font.Part3Ptr.NbrCar'Img);
         Writeln ("Premier car : " + Font.Part3Ptr.FirstCar);
         Writeln ("Décalage : " + Font.Part3Ptr.ShiftDef'Img);
         Writeln ("Hauteur majuscule : " + Font.Part3Ptr.HauteurMaj'Img);
         Writeln ("Décalage origine : " + Font.Part3Ptr.OffsetOrigine'Img);
         Writeln ("Taille jambage : " + Font.Part3Ptr.TailleJambage'Img);

      end AnalyseFont;

   begin
      ChargeFont;
      InitTable;
      AnalyseFont;
   end DecodeFont;

   procedure AffichCar (Car : Char) is
      Cmd                    : CmdPtr;
      Xmin, Xmax, Ymin, Ymax : Integer;
      Chaine                 : String (1 .. 256);

   begin
      ZMoveTo (-64, -64);
      ZLineTo (63, -64);
      ZLineTo (63, 63);
      ZLineTo (-64, 63);
      ZLineTo (-64, -64);
      Xmax := -64;
      Xmin := 63;
      Ymax := -64;
      Ymin := 63;
      Cmd  := DebFont (Car);
      while Cmd /= null loop

         -- [P2Ada]: WITH instruction
         declare
            P2Ada_Var_7 : CmdPart renames Cmd.all;
         begin
            case P2Ada_Var_7.CmdType is
               when Move =>
                  ZMoveTo (P2Ada_Var_7.X, P2Ada_Var_7.Y);
               when Line =>
                  ZLineTo (P2Ada_Var_7.X, P2Ada_Var_7.Y);
                  --            when others=> null;  -- [P2Ada]: no otherwise / else in Pascal
            end case;
            if P2Ada_Var_7.X < Xmin then
               Xmin := P2Ada_Var_7.X;
            end if;
            if P2Ada_Var_7.X > Xmax then
               Xmax := P2Ada_Var_7.X;
            end if;
            if P2Ada_Var_7.Y < Ymin then
               Ymin := P2Ada_Var_7.Y;
            end if;
            if P2Ada_Var_7.Y > Ymax then
               Ymax := P2Ada_Var_7.Y;
            end if;
         end; -- [P2Ada]: end of WITH
         Cmd := Cmd.CmdSui;
      end loop;
      Assign_String (Chaine, Xmin'Img);
      OutTextXY (0, 0, Chaine);
      Assign_String (Chaine, Xmax'Img);
      OutTextXY (50, 0, Chaine);
      -- [P2Ada]: WITH instruction
      declare
         P2Ada_Var_8 : CodeFont renames Font;
      begin
         Assign_String
           (Chaine,
            P2Ada_Var_8.TailleCarPtr (Chr (Ord (Car) - Ord (P2Ada_Var_8.Part3Ptr.FirstCar)))'Img);
      end; -- [P2Ada]: end of WITH

      OutTextXY (100, 0, Chaine);
      Assign_String (Chaine, Ymin'Img);
      OutTextXY (0, 10, Chaine);
      Assign_String (Chaine, Ymax'Img);
      OutTextXY (50, 10, Chaine);
      Assign_String (Chaine, Ord (Car)'Img);
      OutTextXY (0, 20, Chaine);
   end AffichCar;

   procedure DefilFont is
      Preced : constant Char := Char'Val (80);
      Suiv   : constant Char := Char'Val (72);
      Esc    : constant Char := Char'Val (27);
      F5     : constant Char := Char'Val (63);

      Ch : Char;

   begin
      SetGraphMode (Mode);
      loop
         ClearViewPort;
         AffichCar (Car);
         OutTextXY
           (10,
            GetMaxY - 10,
            "précédente: fléche bas, suivante: fléche haut ," + "zoom: F5, sortir:Esc");
         Ch := ReadKey;
         if Ch = Chr (0) then
            -- [P2Ada]: WITH instruction
            declare
               P2Ada_Var_9 : Partie3 renames Font.Part3Ptr;
            begin
               case ReadKey is
                  when Preced =>
                     if Car /= P2Ada_Var_9.FirstCar then
                        Dec (Car);
                     else
                        Car := Chr (Ord (P2Ada_Var_9.FirstCar) + P2Ada_Var_9.NbrCar - 1);
                     end if;
                  when Suiv =>
                     if Car /= Chr (Ord (P2Ada_Var_9.FirstCar) + P2Ada_Var_9.NbrCar - 1) then
                        Inc (Car);
                     else
                        Car := P2Ada_Var_9.FirstCar;
                     end if;
                  when F5 =>
                     Zoom := not Zoom;
                  when others =>
                     null;  -- [P2Ada]: no otherwise / else in Pascal
               end case;

            end; -- [P2Ada]: end of WITH
         end if;
         exit when (DebFont (Car) = null) or (Ch = Esc);
      end loop;
      RestoreCrtMode;
   end DefilFont;

   procedure ListCar (Car : Char) is
      Cmd : CmdPtr;

   begin
      Cmd := DebFont (Car);
      while Cmd /= null loop
         -- [P2Ada]: WITH instruction
         declare
            P2Ada_Var_10 : CmdPart renames Cmd.all;
         begin
            case P2Ada_Var_10.CmdType is
               when Move =>
                  Writeln;
                  Write (+'G');
               when Line =>
                  Write (", D");
                  --            when others=> null;  -- [P2Ada]: no otherwise / else in Pascal
            end case;
            Write (P2Ada_Var_10.X, 3);
            Write (P2Ada_Var_10.Y, 3);
         end; -- [P2Ada]: end of WITH
         Cmd := Cmd.CmdSui;
      end loop;
   end ListCar;

   procedure EditFont (Car : Char) is
      Est   : constant Char := Char'Val (77);
      Sud   : constant Char := Char'Val (80);
      Ouest : constant Char := Char'Val (75);
      Nord  : constant Char := Char'Val (72);
      Ch    : Char;
      Press : Boolean;
      --        type P2Ada_Type_1 is access Byte;
      --  procedure Dispose is new Ada.Unchecked_Deallocation(Byte, P2Ada_Type_1); -- [P2Ada]:
      --type definition needed in Ada
      CursPtr                  : Pointer;
      PX, PY, Xanc, Yanc, Fact : Integer;
      DebCmdPtr, FinCmdPtr     : CmdPtr;
      pragma Unreferenced (DebCmdPtr);

      procedure ImCurseur is
         Taille : Integer;
      begin
         ZMoveTo (0, 5);
         LineRel (10, 0);
         ZMoveTo (5, 0);
         LineRel (0, 10);
         Taille := ImageSize (0, 0, 10, 10);
         GetMem (CursPtr, Taille);
         GetImage (0, 0, 10, 10, CursPtr);
         ClearViewPort;
      end ImCurseur;

      procedure AffInv (Inv : String) is
      begin
         SetViewPort (0, 0, 450, 10, True);
         ClearViewPort;
         OutTextXY (0, 0, Inv);
         SetViewPort (0, 0, GetMaxX, GetMaxY, True);
      end AffInv;

      procedure AffPos is
         ChX, ChY : String (1 .. 256);
      begin
         SetViewPort (0, 15, 100, 30, True);
         ClearViewPort;
         --        Str(PX-100,3,ChX); Str(100-PY,3,ChY);
         Assign_String (ChX, Integer'Image (PX - 100));
         Assign_String (ChY, Integer'Image (100 - PY));
         OutTextXY (0, 0, ChX + ' ' + ChY);
         SetViewPort (0, 0, GetMaxX, GetMaxY, True);
      end AffPos;

      procedure AjoutCmd (ComType : Commande) is
         Cmd : CmdPtr;

      begin
         Cmd := new CmdPart;
         -- [P2Ada]: WITH instruction
         declare
            P2Ada_Var_11 : CmdPart renames Cmd.all;
         begin
            P2Ada_Var_11.CmdType := ComType;
            P2Ada_Var_11.X       := PX - 100;
            P2Ada_Var_11.Y       := 100 - PY;
            P2Ada_Var_11.CmdSui  := null;
            if FinCmdPtr = null then
               DebCmdPtr := Cmd;
            else
               FinCmdPtr.CmdSui := Cmd;
            end if;
         end; -- [P2Ada]: end of WITH

      end AjoutCmd;

      procedure DeplaceCurs is
      begin
         AffInv
           ("Deplace: <-,->," + Chr (25) + ',' + Chr (26) + ",+,-,Espace,Positon,Quitte ?");
         Xanc := PX;
         Yanc := PY;
         loop
            AffPos;
            loop
               PutImage (PX - 5, PY - 5, CursPtr, XORPut);
               Press := KeyPressed;
               if not Press then
                  Delay1 (50);
               end if;
               PutImage (PX - 5, PY - 5, CursPtr, XORPut);
               if not Press then
                  Delay1 (50);
               end if;
               exit when Press;
            end loop;
            Ch := UpCase (ReadKey);
            if Ch = Chr (0) then
               case ReadKey is
                  when Est =>
                     PX := PX + Fact;
                  when Ouest =>
                     PX := PX - Fact;
                  when Sud =>
                     PY := PY + Fact;
                  when Nord =>
                     PY := PY - Fact;
                  when others =>
                     null;  -- [P2Ada]: no otherwise / else in Pascal
               end case;
            else
               case Ch is
                  when '+' =>
                     Fact := Fact * 5;
                  when '-' =>
                     if Fact > 1 then
                        Fact := Fact / 5;
                     end if;
                  when others =>
                     null;  -- [P2Ada]: no otherwise / else in Pascal
               end case;
            end if;
            exit when Ch = ' ' or Ch = 'Q';
         end loop;
         AjoutCmd (Move);
      end DeplaceCurs;

      procedure Ligne is
      begin
         AffInv
           ("Deplace: <-,->," + Chr (25) + ',' + Chr (26) + ",+,-,Espace,Positon,Quitte ?");
         Xanc := PX;
         Yanc := PY;
         loop
            AffPos;
            loop
               ZMoveTo (Xanc, Yanc);
               ZLineTo (PX, PY);
               PutImage (PX - 5, PY - 5, CursPtr, XORPut);
               Press := KeyPressed;
               if not Press then
                  Delay1 (50);
               end if;
               PutImage (PX - 5, PY - 5, CursPtr, XORPut);
               SetColor (0);
               ZMoveTo (Xanc, Yanc);
               ZLineTo (PX, PY);
               SetColor (15);
               if not Press then
                  Delay1 (50);
               end if;
               exit when Press;
            end loop;
            Ch := UpCase (ReadKey);
            if Ch = Chr (0) then
               case ReadKey is
                  when Est =>
                     PX := PX + Fact;
                  when Ouest =>
                     PX := PX - Fact;
                  when Sud =>
                     PY := PY + Fact;
                  when Nord =>
                     PY := PY - Fact;
                  when others =>
                     null;  -- [P2Ada]: no otherwise / else in Pascal
               end case;
            else
               case Ch is
                  when 'T' | ' ' =>
                     ZMoveTo (Xanc, Yanc);
                     ZLineTo (PX, PY);
                     AjoutCmd (Line);
                     Xanc := PX;
                     Yanc := PY;
                  when '+' =>
                     Fact := Fact * 5;
                  when '-' =>
                     if Fact > 1 then
                        Fact := Fact / 5;
                     end if;
                  when others =>
                     null;  -- [P2Ada]: no otherwise / else in Pascal
               end case;
            end if;
            exit when Ch = ' ' or Ch = 'Q';
         end loop;
      end Ligne;

   begin
      Zoom := False;
      SetGraphMode (Mode);
      ImCurseur;
      PX   := 100;
      PY   := 100;
      Fact := 1;
      AffichCar (Car);
      DebCmdPtr := DebFont (Car);
      FinCmdPtr := FinFont (Car);
      loop
         DeplaceCurs;
         if Ch /= 'Q' then
            Ligne;
         end if;
         AffInv ("Quitte ?");
         exit when UpCase (ReadKey) = 'Q';
      end loop;
      RestoreCrtMode;
      FinFont (Car) := FinCmdPtr;
   end EditFont;

   procedure SauveFont (Font : in out CodeFont) is
      FichFont             : File;
      FinCopyright, CmdInd : Integer;

      procedure MetPart1 is
         Copyright : String (1 .. 100);
      begin
         -- [P2Ada]: WITH instruction
         declare
            P2Ada_Var_12 : CodeFont renames Font;
         begin
            Write ("Copyright <aha> ?");
            Readln (Copyright);
            if Is_Equal (Copyright, "") then
               Assign_String (Copyright, "aha");
            end if;
            --PP          Move(Copyright(1),P2Ada_Var_12.BuffFont(4),(Copyright'length));
            FinCopyright                             := 4 + (Copyright'Length);
            P2Ada_Var_12.BuffFont (FinCopyright)     := 16#00#;
            P2Ada_Var_12.BuffFont (FinCopyright + 1) := 16#1a#;
            --PP
            --P2Ada_Var_12.Part2Ptr:=Ptr(DSeg,Ofs(P2Ada_Var_12.BuffFont(FinCopyright+2)));
         end; -- [P2Ada]: end of WITH
      end MetPart1;

      procedure MetPart2 is
      begin
         -- [P2Ada]: WITH instruction
         declare
            P2Ada_Var_13 : CodeFont renames Font;
            P2Ada_Var_14 : Partie2 renames P2Ada_Var_13.Part2Ptr;
         begin
            P2Ada_Var_14.DebPart3 := 16#0080#;
            --PP          Move(P2Ada_Var_13.NomFont(1),P2Ada_Var_14.Nom,4);
            --              P2Ada_Var_14.ZeroUn := Chr (1) & Chr (0) & Chr (1) & Chr (0);
            --PP          FillChar(P2Ada_Var_14.Zeros,P2Ada_Var_14.DebPart3-FinCopyright-14,0);
            --PP
            --P2Ada_Var_13.Part3Ptr:=Ptr(DSeg,Ofs(P2Ada_Var_13.BuffFont(P2Ada_Var_14.DebPart3)));
         end; -- [P2Ada]: end of WITH
      end MetPart2;

      procedure MetPart3 is
         DebCode, FinCode, DebAdrCar, DebTailleCar, Rang1, DebCmd : Integer;
         Xmin, Xmax, Ymin, Ymax                                   : Integer;

         procedure MetCar is
            Car : Char;
            Cmd : CmdPtr;

            procedure MetCode (Com : Commande; Coord : Integer) is
               Coord1 : Integer := Coord;
            begin
               if Coord1 < 0 then
                  Coord1 := Coord1 + 128;
               end if;
               case Com is
                  when Move =>
                     Font.BuffFont (CmdInd) := Byte1 (Coord1);
                  when Line =>
                     Font.BuffFont (CmdInd) := 16#80# or Byte1 (Coord1);
                     --              when others=> null;  -- [P2Ada]: no otherwise / else in Pascal
               end case;
               CmdInd := CmdInd + 1;
            end MetCode;

         begin
            -- [P2Ada]: WITH instruction
            declare
               P2Ada_Var_15 : CodeFont renames Font;
            begin
               Car := Chr (DebCode + Rang1);
               Write (+Car);
               Xmax                                  := -64;
               Xmin                                  := 63;
               Ymax                                  := -64;
               Ymin                                  := 63;
               P2Ada_Var_15.AdrCarPtr (Chr (Rang1))  := CmdInd - DebCmd;
               Cmd                                   := DebFont (Chr (DebCode + Rang1));
               while Cmd /= null loop
                  -- [P2Ada]: WITH instruction
                  declare
                     P2Ada_Var_16 : CmdPart renames Cmd.all;
                  begin
                     case P2Ada_Var_16.CmdType is
                        when Move =>
                           MetCode (P2Ada_Var_16.CmdType, P2Ada_Var_16.X);
                        when Line =>
                           MetCode (P2Ada_Var_16.CmdType, P2Ada_Var_16.X);
                           --                  when others=> null;  -- [P2Ada]: no otherwise /
                           --else in Pascal
                     end case;
                     if P2Ada_Var_16.X < Xmin then
                        Xmin := P2Ada_Var_16.X;
                     end if;
                     if P2Ada_Var_16.X > Xmax then
                        Xmax := P2Ada_Var_16.X;
                     end if;
                     if P2Ada_Var_16.Y < Ymin then
                        Ymin := P2Ada_Var_16.Y;
                     end if;
                     if P2Ada_Var_16.Y > Ymax then
                        Ymax := P2Ada_Var_16.Y;
                     end if;
                  end; -- [P2Ada]: end of WITH
                  Cmd := Cmd.CmdSui;
               end loop;
               --PP            FillChar(Font.BuffFont(CmdInd),3,0);
               CmdInd                                   := CmdInd + (3);
               P2Ada_Var_15.TailleCarPtr (Chr (Rang1))  := Xmax - Xmin;
               -- [P2Ada]: WITH instruction
               declare
                  P2Ada_Var_17 : Partie3 renames P2Ada_Var_15.Part3Ptr;
               begin
                  if Car >= 'A' or Car <= 'Z' then
                     if Ymax > P2Ada_Var_17.HauteurMaj then
                        P2Ada_Var_17.HauteurMaj := Ymax;
                     end if;
                  end if;
                  if Ymin < P2Ada_Var_17.TailleJambage then
                     P2Ada_Var_17.TailleJambage := Ymin;
                  end if;
               end; -- [P2Ada]: end of WITH
            end; -- [P2Ada]: end of WITH
         end MetCar;

      begin
         -- [P2Ada]: WITH instruction
         declare
            P2Ada_Var_18 : CodeFont renames Font;
            P2Ada_Var_19 : Partie3 renames P2Ada_Var_18.Part3Ptr;
         begin
            Write ("Code ASCII du premier caractère ? ");
            Readln (DebCode);
            Write ("Code ASCII du dernier caractère ? ");
            Readln (FinCode);
            P2Ada_Var_19.NbrCar   := FinCode - DebCode + 1;
            P2Ada_Var_19.FirstCar := Chr (DebCode);
            DebAdrCar             := P2Ada_Var_18.Part2Ptr.DebPart3 + 16#10#;
            --PP          P2Ada_Var_18.AdrCarPtr:=Ptr(DSeg,Ofs(P2Ada_Var_18.BuffFont(DebAdrCar)));
            DebTailleCar := DebAdrCar + 2 * P2Ada_Var_19.NbrCar;
            --PP
            --P2Ada_Var_18.TailleCarPtr:=Ptr(DSeg,Ofs(P2Ada_Var_18.BuffFont(DebTailleCar)));
            DebCmd                     := DebTailleCar + P2Ada_Var_19.NbrCar;
            CmdInd                     := DebCmd;
            P2Ada_Var_19.ShiftDef      := DebCmd - P2Ada_Var_18.Part2Ptr.DebPart3;
            P2Ada_Var_19.HauteurMaj    := -64;
            P2Ada_Var_19.TailleJambage := 63;
            Write ("partie 3: ");
            Write (Hexa (DebAdrCar));
            Write (", tailles: ");
            Write (Hexa (DebTailleCar));
            Write (", commandes: ");
            Writeln (Hexa (CmdInd));
            Write ("sauve les caractères: ");
            for Rang in 0 .. FinCode - DebCode loop
               Rang1 := Rang;
               MetCar;
            end loop;
            Writeln;
            --          if  P2Ada_Var_19.TailleJambage<0 then
            --P2Ada_Var_19.TailleJambage:=P2Ada_Var_19.TailleJambage+256;end if;
         end; -- [P2Ada]: end of WITH
      end MetPart3;

      procedure MetTaille is
      begin
         -- [P2Ada]: WITH instruction
         declare
            P2Ada_Var_20 : CodeFont renames Font;
            P2Ada_Var_21 : Partie2 renames P2Ada_Var_20.Part2Ptr;
         begin
            P2Ada_Var_21.DataSize := CmdInd - P2Ada_Var_21.DebPart3;
         end; -- [P2Ada]: end of WITH

      end MetTaille;

   begin
      -- [P2Ada]: WITH instruction
      declare
         P2Ada_Var_22 : CodeFont renames Font;
      begin
         Write ("nom du fichier <Sans> : ");
         Readln (P2Ada_Var_22.NomFont);
         if Is_Equal (P2Ada_Var_22.NomFont, "") then
            Assign_String (P2Ada_Var_22.NomFont, "Sans");
         end if;
         Write ("le fichier ");
         Write (P2Ada_Var_22.NomFont);
         Writeln (".Ch2 est créé.");
         Assign (FichFont, P2Ada_Var_22.NomFont + ".Ch2");
         Rewrite (FichFont, 1);
         MetPart1;
         MetPart2;
         MetPart3;
         MetTaille;
         BlockWrite (FichFont, P2Ada_Var_22.BuffFont'Address, CmdInd);
         Close (FichFont);
         Write ("a écrit ");
         Write (Hexa (CmdInd));
         Writeln (" octets.");
      end; -- [P2Ada]: end of WITH
   end SauveFont;

   procedure CodeAda (Font : CodeFont) is
      function BlankCtrlChar (S : String) return String is
         R : String (1 .. Length (S));
      begin
         Assign_String (R, S);
         for Ind in R'Range loop
            if Ord (R (Ind)) < 32 then
               R (Ind) := ' ';
            end if;
         end loop;
         return R;
      end BlankCtrlChar;
      f : Text;
   begin
      Assign (f, Font.Part2Ptr.Nom + ".adb");
      Rewrite (f);
      Writeln (f, Font.Part2Ptr.Nom + "Char : constant DescFont :=");
      Writeln (f, "new StructDescFont'(");
      Writeln
        (f,
         "  Description    => new String'(""" +
         BlankCtrlChar (Font.Part2Ptr.Description) +
         """),");
      Writeln (f, "  Name           => """ + Font.Part2Ptr.Nom + """,");
      Writeln (f, "  MajorVersion   => " + Font.Part2Ptr.Major'Img + ",");
      Writeln (f, "  MinorVersion   => " + Font.Part2Ptr.Minor'Img + ",");
      Writeln (f, "  AscenderLine   => " + Font.Part3Ptr.HauteurMaj'Img + ",");
      Writeln (f, "  OffsetBaseLine => " + Font.Part3Ptr.OffsetOrigine'Img + ",");
      Writeln (f, "  DescenderLine  => " + Font.Part3Ptr.TailleJambage'Img + ",");
      if Font.Part3Ptr.NbrCar = 0 then
         Writeln (f, " CharWidths     => null,");
      else
         Writeln (f, " CharWidths     => new TabCharWidth'(");
         for Ind in
              Font.Part3Ptr.FirstCar ..
              Chr (Ord (Font.Part3Ptr.FirstCar) + Font.Part3Ptr.NbrCar - 1)
         loop
            Write (f, Ord (Ind)'Img + " => " + Font.TailleCarPtr (Ind)'Img);
            if Ind /= Chr (Ord (Font.Part3Ptr.FirstCar) + Font.Part3Ptr.NbrCar - 1) then
               if Ord (Ind) mod 10 = 0 then
                  Writeln (f, ", ");
               else
                  Write (f, ", ");
               end if;
            end if;
         end loop;
         Writeln (f, "),");
      end if;
      if Font.Part3Ptr.NbrCar = 0 then
         Writeln (f, " CharCmds     => null,");
      else
         Writeln (f, " CharCmds     => new TabCharCmd'(");
         for Ind in
              Font.Part3Ptr.FirstCar ..
              Chr (Ord (Font.Part3Ptr.FirstCar) + Font.Part3Ptr.NbrCar - 1)
         loop
            declare
               cmd : CmdPtr := DebFont (Ind);
            begin
               if cmd = null then
                  Write (f, Ord (Ind)'Img + " => null");
               else
                  Write (f, Ord (Ind)'Img + " => new TabDescCmd'(");
                  if cmd.CmdSui = null then
                     Write
                       (f,
                        "1 => (" +
                        Commande'Image (cmd.CmdType) +
                        ", " +
                        cmd.X'Img +
                        ", " +
                        cmd.Y'Img +
                        "))");
                  else
                     while cmd /= null loop
                        Write
                          (f,
                           "(" +
                           Commande'Image (cmd.CmdType) +
                           ", " +
                           cmd.X'Img +
                           ", " +
                           cmd.Y'Img +
                           ")");
                        cmd := cmd.CmdSui;
                        if cmd /= null then
                           Write (f, ", ");
                        end if;
                     end loop;
                     Write (f, ")");
                  end if;
               end if;
               if Ind /= Chr (Ord (Font.Part3Ptr.FirstCar) + Font.Part3Ptr.NbrCar - 1) then
                  Writeln (f, ", ");
               end if;
            end;
         end loop;
         Writeln (f, "));");
      end if;
      Close (f);
      Writeln ("Création du source Ada : " + Font.Part2Ptr.Nom + ".adb");
   end CodeAda;

   procedure Initialise is
      Driver : Integer;
   begin
      --   InitTable;
      Assign_String (Font.NomFont, "");
      Zoom   := False;
      Driver := Detect;
      Mode   := 0;
      InitGraph (Driver, Mode, "");
      RestoreCrtMode;
      Mark (DebHeap);
      Car := ' ';
   end Initialise;

begin
   Initialise;
   loop
      Writeln;
      Write ("- Fonte courante: ");
      Writeln (Font.NomFont);
      Write ("- Caractère courant: ");
      Writeln (+Car);
      Writeln ("-> Commandes:");
      Writeln ("cHarge, (Sauve), Nom, Caractère courant, Liste commandes, Affiche, ");
      Write ("(Edite), (Zéro), (Restaure), coDe ada, ");
      Write ("Quitte ?");
      Ch := UpCase (ReadKey);
      Writeln (+Ch);
      case Ch is
      when 'C' =>
         Write ("Caractère < A > ? ");
         Car := ReadKey;
         if Car = Chr(13) then
            Car := 'A';
         end if;
         Writeln (+Car);
      when 'A' =>
         DefilFont;
      when 'E' =>
         EditFont (Car);
      when 'H' =>
         DecodeFont (Font);
      when 'L' =>
         ListCar (Car);
      when 'N' =>
         Write ("nom < Font > ? ");
         Readln (Font.NomFont);
      when 'R' => -- [P2Ada]: WITH instruction
         declare
            P2Ada_Var_23 : CodeFont renames Font;
         begin
            AnalyseCar
              (P2Ada_Var_23.Part2Ptr.DebPart3 +
               P2Ada_Var_23.Part3Ptr.ShiftDef +
               P2Ada_Var_23.AdrCarPtr (Chr (Ord (Car) - Ord (P2Ada_Var_23.Part3Ptr.FirstCar))),
               DebFont (Car),
               FinFont (Car));
         end; -- [P2Ada]: end of WITH
      when 'S' =>
         SauveFont (Font);
      when 'Z' =>
         DebFont (Car) := null;
         FinFont (Car) := null;
      when 'D' =>
         CodeAda (Font);
      when others =>
         null;  -- [P2Ada]: no otherwise / else in Pascal
      end case;
      if Ch = Chr (0) then
         Ch := ReadKey;
      end if;
      exit when Ch = 'Q';
   end loop;
   CloseGraph;
end EditFont;
