-------------------------------------------------------------------------------
-- NOM DU CSU (principal)           : courbes.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 1.1a
-- DATE DE LA DERNIERE MISE A JOUR  : 24 octobre 2012
-- ROLE DU CSU                      : Représentation graphique d'équations
--
--
-- FONCTIONS EXPORTEES DU CSU       :
--
-- FONCTIONS LOCALES DU CSU         :
--
--
-- NOTES                            :
--
-- COPYRIGHT                        : (c) Pascal Pignard 1988-2012
-- LICENCE                          : CeCILL V2 (http://www.cecill.info)
-- CONTACT                          : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------
-- Translated on 26-Mar-2012 by (New) P2Ada v. 28-Oct-2009

with TP7.System, TP7.Graph, TP7.Crt, TP7.Mouse;
use  TP7, TP7.System, TP7.Graph, TP7.Crt, TP7.Mouse;

procedure Courbes is
   subtype Integer is TPInteger;

   BackColor : constant := 0;
   Menu      : constant array (0 .. 4) of String (1 .. 6) :=
     ("Trace ",
      "Valeur",
      "Zoom  ",
      "HPGL  ",
      "Quitte");

   GraphMode, GraphDriver, VX1, VX2, VY1, VY2, WMaxX, WMaxY, I, TColor, WColor, AColor : Integer;

   XMin, XMax, YMin, YMax, TMin, TMax, TPas, AX, BX, AY, BY : Real;

   type TFonction is access procedure (A, B : in out Real; C : Real);
   type TNature is (XY, Param, Polaire);
   type TCourbe is record
      Nature      : TNature;
      Fonction    : TFonction;
      Description : String (1 .. 40);
   end record;

   FuncPtr            : TFonction;
   WindPtr1, WindPtr2 : Pointer;
   Ouvert             : Boolean;

   procedure YFX (Nc, Y : in out Real; X : Real) is
   begin
      --  modif->
      Y  := Sin (X);
      Nc := X;
   end YFX;

   procedure Cart (X, Y : in out Real; T : Real) is
   begin
      --  modif->
      X := Cos (3.0 * T);
      --    ->
      Y := Sin (5.0 * T);
   end Cart;

   procedure Cart2 (X, Y : in out Real; T : Real) is
   begin
      X := T * Ln (T);
      Y := Ln (T) / T;
   end Cart2;

   procedure Cart3 (X, Y : in out Real; T : Real) is
   begin
      X := 3.0 * T - T ** 3;
      Y := 2.0 * T ** 2 - T ** 4;
   end Cart3;

   procedure Cart4 (X, Y : in out Real; T : Real) is
   begin
      X := 5.0 * Cos (T) ** 3;
      Y := 5.0 * Sin (T) ** 3;
   end Cart4;

   procedure Pol (X, Y : in out Real; Theta : Real) is
      Rho : Real;
   begin
      --  modif->
      Rho := Cos (2.0 * Theta) / (1.0 + 2.0 * Sin (Theta));
      X   := Rho * Cos (Theta);
      Y   := Rho * Sin (Theta);
   end Pol;

   procedure Pol2 (X, Y : in out Real; Theta : Real) is
      Rho : Real;
   begin
      Rho := Exp (Theta);
      X   := Rho * Cos (Theta);
      Y   := Rho * Sin (Theta);
   end Pol2;

   procedure Pol3 (X, Y : in out Real; Theta : Real) is
      Rho : Real;
   begin
      Rho := Cos (Theta / 2.0);
      X   := Rho * Cos (Theta);
      Y   := Rho * Sin (Theta);
   end Pol3;

   procedure Pol4 (X, Y : in out Real; Theta : Real) is
      Rho : Real;
   begin
      Rho := Cos (3.0 * Theta);
      X   := Rho * Cos (Theta);
      Y   := Rho * Sin (Theta);
   end Pol4;

   procedure Pol5 (X, Y : in out Real; Theta : Real) is
      Rho : Real;
   begin
      Rho := 1.0 + Cos (Theta);
      X   := Rho * Cos (Theta);
      Y   := Rho * Sin (Theta);
   end Pol5;

   Fonctions : constant array (Positive range <>) of TCourbe :=
     ((XY, YFX'Access, To_TPString (39, "y = sin(x)")),
      (Param, Cart'Access, To_TPString (39, "x = cos (3t); y = sin(5t)")),
      (Param, Cart2'Access, To_TPString (39, "x = t ln (t); y = ln (t) / t")),
      (Param, Cart3'Access, To_TPString (39, "x = 3t-t^3; y = 2t^2-t^4")),
      (Param, Cart4'Access, To_TPString (39, "x = 5*cos (t)^3; y = 5*sin (t)^3")),
      (Polaire, Pol'Access, To_TPString (39, "r = cos (2t) / (1+2*sin (t))")),
      (Polaire, Pol2'Access, To_TPString (39, "r = exp (t)")),
      (Polaire, Pol3'Access, To_TPString (39, "r = cos (t/2)")),
      (Polaire, Pol4'Access, To_TPString (39, "r = cos (3t)")),
      (Polaire, Pol5'Access, To_TPString (39, "r = 1+cos (2t)")));

   type TMenu is array (Natural range <>) of String (1 .. 6);

   procedure AffMenu (Nbr : Integer; MotClef : TMenu) is
   begin
      SetViewPort (0, 0, 319, 14, ClipOn);
      ClearViewPort;
      for Ind in 0 .. Nbr - 1 loop
         SetColor (WColor);
         Rectangle (5 + Ind * 64, 0, 58 + Ind * 64, 13);
         SetColor (TColor);
         OutTextXY (31 + Ind * 64, 6, MotClef (Ind));
      end loop;
      SetViewPort (0, 0, GetMaxX, GetMaxY, ClipOn);
   end AffMenu;

   procedure AffText (Texte : String) is
      ViewPort : ViewPortType;
   begin
      GetViewSettings (ViewPort);
      SetViewPort (0, GetMaxY - 9, GetMaxX, GetMaxY, ClipOn);
      ClearViewPort;
      SetColor (TColor);
      OutTextXY (GetMaxX / 2, 5, Texte);
      SetViewPort (ViewPort.X1, ViewPort.Y1, ViewPort.X2, ViewPort.Y2, ClipOn);
   end AffText;

   procedure GetText (Comment : String; Texte : out String) is
      Ch : Char;
   begin
      SetViewPort (0, GetMaxY - 9, GetMaxX, GetMaxY, ClipOn);
      SetTextJustify (LeftText, TopText);
      loop
         ClearViewPort;
         SetColor (TColor);
         Assign_String (Texte, "");
         OutText (Comment);
         SetColor (AColor);
         loop
            Ch := ReadKey;
            if Ch >= Character'Val (48) and Ch <= Character'Val (122) then
               Assign_String (Texte, Texte + Ch);
               OutText (+Ch);
            end if;
            exit when Ch = Character'Val (13) or Ch = Character'Val (27);
         end loop;
         exit when Ch = Character'Val (13);
      end loop;
      SetTextJustify (CenterText, CenterText);
      SetViewPort (0, 0, GetMaxX, GetMaxY, ClipOn);
   end GetText;

   procedure Clear is
      MenuC   : constant array (0 .. 1) of String (1 .. 6) := ("Oui   ", "Non   ");
      Quitter : Boolean;
   begin
      SetViewPort (1, 16, GetMaxX - 1, GetMaxY - 11, ClipOn);
      ClearViewPort;
      AffText ("Voulez-vous quitter ?");
      AffMenu (2, TMenu (MenuC));
      Quitter := False;
      loop
         if KeyPressed then
            case UpCase (ReadKey) is
               when 'O' =>
                  AffText ("C'est fini...");
                  CloseGraph;
                  Halt (1);
               when 'N' =>
                  Quitter := True;
               when others =>
                  null;
            end case;
         end if;
         if ButtonPress (1) then
            loop
               exit when ButtonRelease (1);
            end loop;
            if LastYRelease (1) < 16 then
               case LastXRelease (1) is
                  when 1 .. 63 =>
                     AffText ("C'est fini...");
                     CloseGraph;
                     Halt (1);
                  when 64 .. 127 =>
                     Quitter := True;
                  when others =>
                     null;
               end case;
            end if;
         end if;
         exit when Quitter;
      end loop;
      Ouvert := False;
      AffMenu (5, TMenu (Menu));
      AffText ("Choisisez une option du menu!");
      SetViewPort (0, 0, GetMaxX, GetMaxY, ClipOn);
   end Clear;

   function Int (X : Real) return Real is
      Result_Int : Real;
   begin
      if X < 0.0 then
         Result_Int := System.Int (X) - 1.0;
      else
         Result_Int := System.Int (X);
      end if;
      return Result_Int;
   end Int;

   procedure RLine (X1, Y1, X2, Y2 : Real) is
   begin
      Line
        (Round (AX * X1 + BX),
         Round (AY * Y1 + BY),
         Round (AX * X2 + BX),
         Round (AY * Y2 + BY));
   end RLine;

   procedure RMoveTo (X, Y : Real) is
   begin
      MoveTo (Round (AX * X + BX), Round (AY * Y + BY));
   end RMoveTo;

   procedure RLineTo (X, Y : Real) is
   begin
      LineTo (Round (AX * X + BX), Round (AY * Y + BY));
   end RLineTo;

   procedure GetWindow (Comment : String) is
      UpArrow        : constant Char := Character'Val (72);
      LeftArrow      : constant Char := Character'Val (75);
      RightArrow     : constant Char := Character'Val (77);
      DownArrow      : constant Char := Character'Val (80);
      CtrlLeftArrow  : constant Char := Character'Val (115);
      CtrlRightArrow : constant Char := Character'Val (116);
      PgUp           : constant Char := Character'Val (73);
      PgDn           : constant Char := Character'Val (81);
      Ch             : Char;
      RelX, RelY, N  : Integer;
      Good           : Boolean;
   begin
      while KeyPressed loop
         Ch := ReadKey;
      end loop;
      AffText (Comment);
      SetLineStyle (CenterLn, 0, NormWidth);
      MouseWindow (1, 16, GetMaxX - 1, GetMaxY - 11);
      Good := False;
      loop
         VX1 := 24;
         VY1 := 24;
         VX2 := 24;
         VY2 := 24;
         if KeyPressed then
            AffText
              ("Clavier, déplacer l'origine avec les flèches, puis valider avec <entrée>...");
            SetColor (AColor);
            Circle (VX1, VY1, 3);
            loop
               Ch := ReadKey;
               if Ch = Character'Val (0) then
                  RelX := 0;
                  RelY := 0;
                  case ReadKey is
                     when UpArrow =>
                        RelY := -2;
                     when LeftArrow =>
                        RelX := -2;
                     when RightArrow =>
                        RelX := 2;
                     when DownArrow =>
                        RelY := 2;
                     when CtrlLeftArrow =>
                        RelX := -20;
                     when CtrlRightArrow =>
                        RelX := 20;
                     when PgUp =>
                        RelY := -20;
                     when PgDn =>
                        RelY := 20;
                     when others =>
                        null;
                  end case;
                  if (VX1 + RelX > 0) and
                     (VX1 + RelX < GetMaxX) and
                     (VY1 + RelY > 15) and
                     (VY1 + RelY < GetMaxY - 10)
                  then
                     SetColor (BackColor);
                     Circle (VX1, VY1, 3);
                     VX1 := VX1 + RelX;
                     VY1 := VY1 + RelY;
                     SetColor (AColor);
                     Circle (VX1, VY1, 3);
                  end if;
               end if;
               exit when Ch = Character'Val (13);
            end loop;
            VX2 := VX1;
            VY2 := VY1;
            AffText
              ("Clavier, dimensionner la fenêtre avec les flèches, puis valider avec <entrée>...");
            loop
               Ch := ReadKey;
               if Ch = Character'Val (0) then
                  RelX := 0;
                  RelY := 0;
                  case ReadKey is
                     when UpArrow =>
                        RelY := -3;
                     when LeftArrow =>
                        RelX := -3;
                     when RightArrow =>
                        RelX := 3;
                     when DownArrow =>
                        RelY := 3;
                     when CtrlLeftArrow =>
                        RelX := -21;
                     when CtrlRightArrow =>
                        RelX := 21;
                     when PgUp =>
                        RelY := -21;
                     when PgDn =>
                        RelY := 21;
                     when others =>
                        null;
                  end case;
                  if (VX2 + RelX > 0) and
                     (VX2 + RelX < GetMaxX) and
                     (VY2 + RelY > 15) and
                     (VY2 + RelY < GetMaxY - 10)
                  then
                     SetColor (BackColor);
                     Rectangle (VX1, VY1, VX2, VY2);
                     SetColor (WColor);
                     VX2 := VX2 + RelX;
                     VY2 := VY2 + RelY;
                     Rectangle (VX1, VY1, VX2, VY2);
                  end if;
               end if;
               exit when (Ch = Character'Val (13)) or (Ch = Character'Val (27));
            end loop;
            Good := (VX1 /= VX2) and (VY1 /= VY2) and (Ch = Character'Val (13));
            if not Good then
               SetColor (BackColor);
               Rectangle (VX1, VY1, VX2, VY2);
               AffText (Comment);
            end if;
         end if;
         if ButtonPress (1) then
            AffText ("Souris, cliquer, tirer pour ouvrir une fenêtre...");
            VX1 := LastXPress (1);
            VY1 := LastYPress (1);
            VX2 := VX1;
            VY2 := VY1;
            loop
               if GetXPos /= VX2 or GetYPos /= VY2 then
                  SetColor (BackColor);
                  Rectangle (VX1, VY1, VX2, VY2);
                  SetColor (WColor);
                  VX2 := GetXPos;
                  VY2 := GetYPos;
                  Rectangle (VX1, VY1, VX2, VY2);
               end if;
               exit when ButtonRelease (1);
            end loop;
            SetColor (BackColor);
            Rectangle (VX1, VY1, VX2, VY2);
            SetColor (WColor);
            VX2 := LastXRelease (1);
            VY2 := LastYRelease (1);
            Rectangle (VX1, VY1, VX2, VY2);
            AffText ("Souris, valider...");
            loop
               N := NumButton;
               exit when N /= 0;
            end loop;
            Good := (VX1 /= VX2) and (VY1 /= VY2) and (N = RightButtonActive);
            if not Good then
               SetColor (BackColor);
               Rectangle (VX1, VY1, VX2, VY2);
               AffText (Comment);
            end if;
         end if;
         exit when Good;
      end loop;
      SetLineStyle (SolidLn, 0, NormWidth);
      MouseWindow (1, 0, GetMaxX - 1, GetMaxY - 11);
      if VX2 < VX1 then
         RelX := VX1;
         VX1  := VX2;
         VX2  := RelX;
      end if;
      if VY2 < VY1 then
         RelY := VY1;
         VY1  := VY2;
         VY2  := RelY;
      end if;
      WMaxX := VX2 - VX1;
      WMaxY := VY2 - VY1;
   end GetWindow;

   procedure Ouvrir is
      Ch               : Char;
      Choix            : Integer;
      TStr             : String (1 .. 256);
      Axes             : Boolean;
      EchX, EchY, X, Y : Real;

      procedure TraceAxes is
         X0, Y0 : Real;
      begin
         AffText ("Tracé des axes:");
         X0 := 0.0;
         Y0 := 0.0;
         if (XMin > 0.0) or (XMax < 0.0) then
            X0 := (Int (XMin / EchX) + 1.0) * EchX;
         end if;
         if (XMin > 0.0) or (XMax < 0.0) then
            X0 := (Int (XMin / EchX) + 1.0) * EchX;
         end if;
         SetColor (AColor);
         RLine (XMin, Y0, XMax, Y0);
         RLine (X0, YMin, X0, YMax);
         SetLineStyle (SolidLn, 0, ThickWidth);
         for Ind in 1 .. Round ((XMax - XMin) / EchX) loop
            RMoveTo ((Int (XMin / EchX) + Real (Ind)) * EchX, Y0 - 1.0 / AY);
            LineRel (0, 3);
         end loop;
         for Ind in 1 .. Round ((YMax - YMin) / EchY) loop
            RMoveTo (X0 - 1.0 / AX, (Int (YMin / EchY) + Real (Ind)) * EchY);
            LineRel (3, 0);
         end loop;
         SetLineStyle (SolidLn, 0, NormWidth);
      end TraceAxes;

   begin
      GetImage (1, 16, GetMaxX - 1, GetMaxY / 2, WindPtr1);
      GetImage (1, GetMaxY / 2, GetMaxX - 1, GetMaxY - 11, WindPtr2);
      GetWindow ("Ouvrir une fenêtre avec les fléches du clavier ou la souris...");
      AffText ("Entrer les paramètres de la courbe dans la fenêtre texte...");
      RestoreCrtMode;
      loop
         loop
            Writeln ("Courbes disponibles :");
            for ind in Fonctions'Range loop
               Writeln (ind'Img + " : " + Fonctions (ind).Description);
            end loop;
            Write ("Choix : ");
            Readln (Choix);
            exit when (Choix >= 1) and (Choix <= Fonctions'Last);
         end loop;
         loop
            Write ("X minimum: ");
            Readln (XMin);
            Write ("X maximum: ");
            Readln (XMax);
            exit when XMin < XMax;
         end loop;
         loop
            Write ("Y minimum: ");
            Readln (YMin);
            Write ("Y maximum: ");
            Readln (YMax);
            exit when YMin < YMax;
         end loop;
         AX := Real (WMaxX) / (XMax - XMin);
         BX := Real (WMaxX) * XMin / (XMin - XMax);
         AY := Real (WMaxY) / (YMin - YMax);
         BY := Real (WMaxY) * YMax / (YMax - YMin);
         loop
            Write ("voulez-vous tracer les axes (O/N): ");
            Readln (Ch);
            Ch := UpCase (Ch);
            exit when (Ch = 'O') or (Ch = 'N');
         end loop;
         Axes := Ch = 'O';
         if Axes then
            loop
               Write ("Échelle X: ");
               Readln (EchX);
               exit when AX * EchX > 2.0;
            end loop;
            loop
               Write ("Échelle Y: ");
               Readln (EchY);
               exit when -AY * EchY > 2.0;
            end loop;
         end if;
         case Fonctions (Choix).Nature is
            when XY =>
               TMin := XMin;
               TMax := XMax;
               TPas := 1.0 / AX;
            when Param =>
               Assign_String (TStr, "T ");
            when Polaire =>
               Assign_String (TStr, "Théta ");
         end case;
         FuncPtr := Fonctions (Choix).Fonction;
         if Fonctions (Choix).Nature in Param .. Polaire then
            loop
               Write (TStr);
               Write (" minimum: ");
               Readln (TMin);
               Write (TStr);
               Write (" maximun: ");
               Readln (TMax);
               exit when TMin < TMax;
            end loop;
            loop
               Write ("Nombre de points sur la courbe: ");
               Readln (TPas);
               exit when TPas > 2.0;
            end loop;
            TPas := (TMax - TMin) / TPas;
         end if;
         Write ("Correct (O/N): ");
         Readln (Ch);
         exit when UpCase (Ch) = 'O';
      end loop;
      SetGraphMode (GraphMode);
      ShowMouse;
      SetTextJustify (CenterText, CenterText);
      SetColor (WColor);
      Rectangle (0, 15, GetMaxX, GetMaxY - 10);
      PutImage (1, 16, WindPtr1, NormalPut);
      PutImage (1, GetMaxY / 2, WindPtr2, NormalPut);
      SetViewPort (VX1, VY1, VX2, VY2, ClipOn);
      ClearViewPort;
      SetColor (WColor);
      Rectangle (0, 0, WMaxX, WMaxY);
      if Axes then
         TraceAxes;
      end if;
      AffText ("Tracé en cours:");
      X := 0.0;
      Y := 0.0;
      FuncPtr (X, Y, TMin);
      RMoveTo (X, Y);
      for Ind in 1 .. Round ((TMax - TMin) / TPas) loop
         FuncPtr (X, Y, TMin + Real (Ind) * TPas);
         RLineTo (X, Y);
      end loop;
      Ouvert := True;
      AffMenu (5, TMenu (Menu));
      AffText ("Choisissez une option du menu!");
      SetViewPort (0, 0, GetMaxX, GetMaxY, ClipOn);
   end Ouvrir;

   procedure Trace is
      FichPlot             : Text;
      Name                 : String (1 .. 256);
      AX, BX, AY, BY, X, Y : Real;
   begin
      if not Ouvert then
         AffText ("Vous devez ouvrir une fenêtre!");
         return;
      end if;
      GetText ("Donner le nom du fichier de sortie:", Name);
      Assign (FichPlot, Name + ".HPL");
      Rewrite (FichPlot);
      Writeln (FichPlot, "IN; SP1; SC 0,20000,0,20000;");
      AffText ("Chargement en cours...");
      AX := 20000.0 / (XMax - XMin);
      BX := 20000.0 * XMin / (XMin - XMax);
      AY := 20000.0 / (YMax - YMin);
      BY := 20000.0 * YMax / (YMax - YMin);
      X  := 0.0;
      Y  := 0.0;
      FuncPtr (X, Y, TMin);
      Write (FichPlot, "PA ");
      Write (FichPlot, Round (AX * X + BX)'Img);
      Write (FichPlot, +',');
      Write (FichPlot, Round (AY * Y + BY)'Img);
      Writeln (FichPlot, "; PD;");
      for Ind in 1 .. Round ((TMax - TMin) / TPas) loop
         FuncPtr (X, Y, TMin + Real (Ind) * TPas);
         Write (FichPlot, "PA ");
         Write (FichPlot, Round (AX * X + BX)'Img);
         Write (FichPlot, +',');
         Write (FichPlot, Round (AY * Y + BY)'Img);
         Writeln (FichPlot, "; PD;");
      end loop;
      Writeln (FichPlot, "PU; SP0;");
      Close (FichPlot);
      if IOResult /= 0 then
         AffText ("Erreur E/S. Choisissez une option du menu!");
      else
         AffText ("Choisissez une option du menu!");
      end if;
   end Trace;

   procedure Zoom is
      VX, VY : Integer;
      X, Y   : Real;
   begin
      if not Ouvert then
         AffText ("Vous devez ouvrir une fenêtre!");
         return;
      end if;
      VX := VX1;
      VY := VY1;
      GetImage (1, 16, GetMaxX - 1, GetMaxY / 2, WindPtr1);
      GetImage (1, GetMaxY / 2, GetMaxX - 1, GetMaxY - 11, WindPtr2);
      GetWindow ("Zone à agrandir, cliquer et tirer...");
      XMin := (Real (VX1) - Real (VX) - BX) / AX;
      YMin := (Real (VY2) - Real (VY) - BY) / AY;
      XMax := (Real (VX2) - Real (VX) - BX) / AX;
      YMax := (Real (VY1) - Real (VY) - BY) / AY;
      GetWindow ("Fenêtre de sortie, cliquer et tirer...");
      PutImage (1, 16, WindPtr1, NormalPut);
      PutImage (1, GetMaxY / 2, WindPtr2, NormalPut);
      SetColor (WColor);
      Rectangle
        (Round (AX * XMin + BX + Real (VX)),
         Round (AY * YMin + BY + Real (VY)),
         Round (AX * XMax + BX + Real (VX)),
         Round (AY * YMax + BY + Real (VY)));
      AX := Real (WMaxX) / (XMax - XMin);
      BX := Real (WMaxX) * XMin / (XMin - XMax);
      AY := Real (WMaxY) / (YMin - YMax);
      BY := Real (WMaxY) * YMax / (YMax - YMin);
      SetViewPort (VX1, VY1, VX2, VY2, ClipOn);
      ClearViewPort;
      Rectangle (0, 0, WMaxX, WMaxY);
      AffText ("Tracé en cours...");
      X := 0.0;
      Y := 0.0;
      FuncPtr (X, Y, TMin);
      RMoveTo (X, Y);
      for Ind in 1 .. Round ((TMax - TMin) / TPas) loop
         FuncPtr (X, Y, TMin + Real (Ind) * TPas);
         RLineTo (X, Y);
      end loop;
      SetViewPort (0, 0, GetMaxX, GetMaxY, ClipOn);
      AffText ("Choisissez une option du menu!");
   end Zoom;

begin
   GraphDriver := Detect;
   GraphMode   := 0;
   InitGraph (GraphDriver, GraphMode, "c:");
   I := GraphResult;
   if I /= grOk then
      Write ("Erreur Graphique: ");
      Writeln (GraphErrorMsg (I));
      Halt (1);
   end if;
   case GraphDriver is
      when CGA | MCGA | EGAMono | HercMono | ATT400 | PC3270 =>
         TColor := 1;
         WColor := 1;
         AColor := 1;
      when EGA64 =>
         TColor := 1;
         WColor := 2;
         AColor := 3;
      when EGA | VGA =>
         TColor := 10;
         WColor := 14;
         AColor := 7;
      when others =>
         null;
   end case;

   GetMem (WindPtr1, 53000);
   GetMem (WindPtr2, 53000);
   SetTextJustify (CenterText, CenterText);
   SetColor (WColor);
   Rectangle (0, 15, GetMaxX, GetMaxY - 10);
   AffMenu (5, TMenu (Menu));
   if MouseInit then
      SetViewPort (1, 16, GetMaxX - 1, GetMaxY - 11, ClipOn);
      MouseSetGraphBlock (StandardCursor);
      MouseWindow (1, 0, GetMaxX - 1, GetMaxY - 11);
      ShowMouse;
      SetColor (TColor);
      OutTextXY (GetMaxX / 2, GetMaxY / 2 + 20, "La Souris est installée");
      OutTextXY (GetMaxX / 2, GetMaxY / 2 + 40, "cliquer gauche: choix, annulation");
      OutTextXY (GetMaxX / 2, GetMaxY / 2 + 60, "        droite: validation");
      loop
         exit when NumButton = RightButtonActive or KeyPressed;
      end loop;
      ClearViewPort;
      SetViewPort (0, 0, GetMaxX, GetMaxY, ClipOn);
      MouseNewPosition (GetMaxX / 2, GetMaxY / 2);
   end if;
   AffText ("Choisissez une option du menu!");
   Ouvert := False;
   loop
      if KeyPressed then
         case UpCase (ReadKey) is
            when 'T' =>
               Ouvrir;
            --  'V': Valeur;
            when 'Z' =>
               Zoom;
            when 'H' =>
               Trace;
            when 'Q' =>
               Clear;
            when others =>
               null;
         end case;
      end if;
      if ButtonPress (1) then
         loop
            exit when ButtonRelease (1);
         end loop;
         if LastYRelease (1) < 16 then
            case LastXRelease (1) is
               when 0 .. 63 =>
                  Ouvrir;
               --  64..127: Valeur;
               when 128 .. 191 =>
                  Zoom;
               when 192 .. 255 =>
                  Trace;
               when 256 .. 320 =>
                  Clear;
               when others =>
                  null;
            end case;
         end if;
      end if;
   end loop;
end Courbes;
