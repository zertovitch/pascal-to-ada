-------------------------------------------------------------------------------
-- NOM DU CSU (principal)           : sudoku.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 1.0a
-- DATE DE LA DERNIERE MISE A JOUR  : 5 mai 2012
-- ROLE DU CSU                      : Résolution de Sudoku
--
--
-- FONCTIONS EXPORTEES DU CSU       :
--
-- FONCTIONS LOCALES DU CSU         :
--
--
-- NOTES                            :
--
-- COPYRIGHT                        : (c) Pascal Pignard 2010-2012
-- LICENCE                          : CeCILL V2 (http://www.cecill.info)
-- CONTACT                          : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with TP7.System, TP7.Graph, TP7.Crt, TP7.Mouse;
use  TP7, TP7.System, TP7.Graph, TP7.Crt, TP7.Mouse;
with Ada.Calendar;

procedure Sudoku is
   subtype Integer is TPInteger;

   BackColor : constant := 0;
   Menu      : constant array (0 .. 4) of String (1 .. 6) :=
     ("Résoud",
      "Efface",
      "Sauve ",
      "Charge",
      "Quitte");

   GraphMode, GraphDriver, I, TColor, WColor, AColor : Integer;

   type TMenu is array (Natural range <>) of String (1 .. 6);

   procedure AffMenu (Nbr : Integer; MotClef : TMenu) is
   begin
      SetViewPort (0, 0, 319, 14, ClipOn);
      ClearViewPort;
      for Ind in 0 .. Nbr - 1 loop
         SetColor (WColor);
         Rectangle (5 + Ind * 64, 0, 58 + Ind * 64, 13);
         SetColor (TColor);
         OutTextXY (5 + Ind * 64, 0, MotClef (Ind));
      end loop;
      SetViewPort (0, 0, GetMaxX, GetMaxY, ClipOn);
   end AffMenu;

   procedure AffText (Texte : String) is
      ViewPort : ViewPortType;
   begin
      SetTextJustify (CenterText, TopText);
      GetViewSettings (ViewPort);
      SetViewPort (0, GetMaxY - 9, GetMaxX, GetMaxY, ClipOn);
      ClearViewPort;
      SetColor (TColor);
      OutTextXY (GetMaxX / 2, 0, Texte);
      SetViewPort (ViewPort.X1, ViewPort.Y1, ViewPort.X2, ViewPort.Y2, ClipOn);
      SetTextJustify (LeftText, TopText);
   end AffText;

   procedure GetText (Comment : String; Texte : out String) is
      Ch : Char;
   begin
      loop
         SetViewPort (0, GetMaxY - 9, GetMaxX, GetMaxY, ClipOn);
         ClearViewPort;
         SetColor (TColor);
         Assign_String (Texte, "");
         OutText (Comment);
         SetColor (WColor);
         loop
            Ch := ReadKey;
            if Ch >= Character'Val (33) and Ch <= Character'Val (122) then
               Assign_String (Texte, Texte + Ch);
               OutText (+Ch);
            end if;
            exit when Ch = Character'Val (13) or Ch = Character'Val (27);
         end loop;
         exit when Ch = Character'Val (13);
      end loop;
      ClearViewPort;
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
      AffMenu (5, TMenu (Menu));
      AffText ("Choisisez une option du menu!");
      SetViewPort (0, 0, GetMaxX, GetMaxY, ClipOn);
   end Clear;

   -- Renvoie le compteur horaire interne en milisecondes.
   function HorlogeMS return Natural is
   begin
      return Natural (Ada.Calendar.Seconds (Ada.Calendar.Clock) * 1000.0);
   end HorlogeMS;

   procedure Affiche_Chiffre (Ligne, Colonne : Integer; Chiffre : Integer) is
      Ch : constant Char := Chr (48 + Chiffre);
      TH : Integer;
      TW : Integer;
   begin
      SetTextStyle (DefaultFont, HorizDir, 3);
      TH := TextHeight (+Ch);
      TW := TextWidth (+Ch);
      Bar
        (100 + Colonne * TW,
         100 + Ligne * TH,
         100 + (Colonne + 1) * TW,
         100 + (Ligne + 1) * TH);
      OutTextXY (100 + Colonne * TW, 100 + Ligne * TH, +Ch);
      SetTextStyle (DefaultFont, HorizDir, 1);
   end Affiche_Chiffre;

   package Grille is
      subtype Chiffre is Integer range 0 .. 9;
      subtype Indice is Integer range 1 .. 9;
      type TGrille is array (Indice, Indice) of Chiffre;
      type ChiffresPossibles is array (Chiffre range 1 .. 9) of Boolean;
      type TableauValeursPossibles is array (Indice, Indice) of ChiffresPossibles;
      function NumCarre (Ligne, Colonne : Indice) return Indice;
      procedure Imprime;
      protected Sudoku is
         function RetourneValeur (Ligne, Colonne : Indice) return Chiffre;
         procedure PositionneValeur (Ligne, Colonne : Indice; Valeur : Chiffre);
         procedure PositionneGrille (UneGrille : TGrille);
         function RetourneValeursPossibles (Ligne, Colonne : Indice) return ChiffresPossibles;
         procedure SupprimeValeur (Ligne, Colonne : Indice; Valeur : Chiffre);
         procedure ConstruitValeursPossibles;
      private
         Sudoku           : TGrille                 := (others => (others => 0));
         ValeursPossibles : TableauValeursPossibles :=
           (others => (others => (others => True)));
      end Sudoku;
   end Grille;

   package body Grille is
      function NumCarre (Ligne, Colonne : Indice) return Indice is
      begin
         return ((Ligne - 1) / 3) * 3 + ((Colonne - 1) / 3) + 1;
      end NumCarre;

      protected body Sudoku is
         function RetourneValeur (Ligne, Colonne : Indice) return Chiffre is
         begin
            return Sudoku (Ligne, Colonne);
         end RetourneValeur;
         procedure PositionneValeur (Ligne, Colonne : Indice; Valeur : Chiffre) is
         begin
            Writeln ("Positionne valeur : " & Ligne'Img & ", " & Colonne'Img & ", " & Valeur'Img);
            Affiche_Chiffre (Ligne, Colonne, Valeur);
            Sudoku (Ligne, Colonne) := Valeur;
            --              delay 2.0;
         end PositionneValeur;
         procedure PositionneGrille (UneGrille : TGrille) is
         begin
            Sudoku := UneGrille;
         end PositionneGrille;
         function RetourneValeursPossibles (Ligne, Colonne : Indice) return ChiffresPossibles is
         begin
            return ValeursPossibles (Ligne, Colonne);
         end RetourneValeursPossibles;
         procedure SupprimeValeur (Ligne, Colonne : Indice; Valeur : Chiffre) is
            Trouvé : Chiffre := 0;
            Unique : Boolean := True;
         begin
            if RetourneValeur (Ligne, Colonne) = 0 then
               ValeursPossibles (Ligne, Colonne) (Valeur)  := False;
               for V in ChiffresPossibles'Range loop
                  if ValeursPossibles (Ligne, Colonne) (V) then
                     if Trouvé = 0 then
                        Trouvé := V;
                     else
                        Unique := False;
                     end if;
                  end if;
               end loop;
               if Unique and then Trouvé /= 0 then
                  PositionneValeur (Ligne, Colonne, Trouvé);
               end if;
            end if;
         end SupprimeValeur;
         procedure ConstruitValeursPossibles is
         begin
            for Ligne in Indice loop
               for Colonne in Indice loop
                  if RetourneValeur (Ligne, Colonne) /= 0 then
                     ValeursPossibles (Ligne, Colonne) := (others => False);
                  else
                     ValeursPossibles (Ligne, Colonne) := (others => True);
                  end if;
               end loop;
            end loop;
         end ConstruitValeursPossibles;
      end Sudoku;
      procedure Imprime is
      begin
         for Index1 in TGrille'Range (1) loop
            for Index2 in TGrille'Range (2) loop
               Write (Sudoku.RetourneValeur (Index1, Index2)'Img);
            end loop;
            Writeln;
         end loop;
      end Imprime;
   end Grille;

   task type RésoudLigne (Ligne : Grille.Indice) is
      entry Terminé;
   end RésoudLigne;

   task body RésoudLigne is
      Fini, Vide : Boolean;
      Position   : Grille.Indice;
      type Possibilités is (Pas_Présent, Unique, Unique_Carré, Multiple);
      type Status is record
         Carré    : Grille.Indice;
         Présence : Possibilités := Pas_Présent;
      end record;
      Appartenance : array (Grille.ChiffresPossibles'Range) of Status;
   begin
      loop
      -- Règle 1 : supprimer le chiffre des cases de la même ligne
         for Colonne in Grille.Indice loop
            if Grille.Sudoku.RetourneValeur (Ligne, Colonne) /= 0 then
               for Colonne2 in Grille.Indice loop
                  Grille.Sudoku.SupprimeValeur
                    (Ligne,
                     Colonne2,
                     Grille.Sudoku.RetourneValeur (Ligne, Colonne));
               end loop;
            end if;
         end loop;
         -- Règle 4 : valider si le chiffre est le seul possible de la ligne
         --         for Valeur in Grille.Chiffre loop  -- pas d'erreur -v pour la valeur 0 !!!
         for Valeur in Grille.ChiffresPossibles'Range loop
            Vide := True;
            for Colonne in Grille.Indice loop
               if Grille.Sudoku.RetourneValeursPossibles (Ligne, Colonne) (Valeur) then
                  if Vide then
                     Position := Colonne;
                     Vide     := False;
                  else
                     Vide := True;
                     exit;
                  end if;
               end if;
            end loop;
            if not Vide and then Grille.Sudoku.RetourneValeur (Ligne, Position) = 0 then
               Write ("Régle 4 : ");
               Grille.Sudoku.PositionneValeur (Ligne, Position, Valeur);
            end if;
         end loop;
         -- Règle 7 : Dans une ligne si une valeur possible n'apparait que dans un carré
         --           alors supprimer les valeurs du carré hors de la ligne
         for Colonne in Grille.Indice loop
            for Valeur in Grille.ChiffresPossibles'Range loop
               if Grille.Sudoku.RetourneValeursPossibles (Ligne, Colonne) (Valeur) then
                  case Appartenance (Valeur).Présence is
                     when Pas_Présent =>
                        Appartenance (Valeur) := (Grille.NumCarre (Ligne, Colonne), Unique_Carré);
                     when Unique =>
                        null;
                     when Unique_Carré =>
                        if Appartenance (Valeur).Carré /= Grille.NumCarre (Ligne, Colonne) then
                           Appartenance (Valeur).Présence := Multiple;
                        end if;
                     when Multiple =>
                        null;
                  end case;
               end if;
               if Grille.Sudoku.RetourneValeur (Ligne, Colonne) = Valeur then
                  Appartenance (Valeur).Présence := Multiple;
               end if;
            end loop;
         end loop;
         for Valeur in Grille.ChiffresPossibles'Range loop
            if Appartenance (Valeur).Présence = Unique_Carré then
               Writeln
                 ("Régle 7 : Ligne " &
                  Ligne'Img &
                  " Carré " &
                  Appartenance (Valeur).Carré'Img &
                  ", Valeur " &
                  Valeur'Img);
               for Colonne in Grille.Indice loop
                  if Grille.NumCarre (Ligne, Colonne) /= Appartenance (Valeur).Carré then
                     Grille.Sudoku.SupprimeValeur (Ligne, Colonne, Valeur);
                  end if;
               end loop;
            end if;
            Appartenance (Valeur).Présence := Pas_Présent;
         end loop;

         -- Vérifie si la ligne est complète
         Fini := True;
         for Colonne in Grille.Indice loop
            if Grille.Sudoku.RetourneValeur (Ligne, Colonne) = 0 then
               Fini := False;
            end if;
         end loop;
         exit when Fini;
      end loop;
      Writeln ("Tâche ligne " & Ligne'Img & " est complète.");
      accept Terminé;
   end RésoudLigne;

   task type RésoudColonne (Colonne : Grille.Indice) is
      entry Terminé;
   end RésoudColonne;

   task body RésoudColonne is
      Fini, Vide : Boolean;
      Position   : Grille.Indice;
   begin
      loop
      -- Règle 2 : supprimer le chiffre des cases de la même colonne
         for Ligne in Grille.Indice loop
            if Grille.Sudoku.RetourneValeur (Ligne, Colonne) /= 0 then
               for Ligne2 in Grille.Indice loop
                  Grille.Sudoku.SupprimeValeur
                    (Ligne2,
                     Colonne,
                     Grille.Sudoku.RetourneValeur (Ligne, Colonne));
               end loop;
            end if;
         end loop;
         -- Règle 5 : valider si le chiffre est le seul possible de la colonne
         --         for Valeur in Grille.Chiffre loop  -- pas d'erreur -v pour la valeur 0 !!!
         for Valeur in Grille.ChiffresPossibles'Range loop
            Vide := True;
            for Ligne in Grille.Indice loop
               if Grille.Sudoku.RetourneValeursPossibles (Ligne, Colonne) (Valeur) then
                  if Vide then
                     Position := Ligne;
                     Vide     := False;
                  else
                     Vide := True;
                     exit;
                  end if;
               end if;
            end loop;
            if not Vide and then Grille.Sudoku.RetourneValeur (Position, Colonne) = 0 then
               Write ("Régle 5 : ");
               Grille.Sudoku.PositionneValeur (Position, Colonne, Valeur);
            end if;
         end loop;

         -- Vérifie si la colonne est complète
         Fini := True;
         for Ligne in Grille.Indice loop
            if Grille.Sudoku.RetourneValeur (Ligne, Colonne) = 0 then
               Fini := False;
            end if;
         end loop;
         exit when Fini;
      end loop;
      Writeln ("Tâche colonne " & Colonne'Img & " est complète.");
      accept Terminé;
   end RésoudColonne;

   task type RésoudCarré (Carré : Grille.Indice) is
      entry Terminé;
   end RésoudCarré;

   task body RésoudCarré is
      Vide, Fini      : Boolean;
      PositionLigne   : Grille.Indice;
      PositionColonne : Grille.Indice;
   begin
      loop
      -- Règle 3 : supprimer le chiffre des cases du même carré
      --         for Carré in Grille.Indice loop
         for Ligne in ((Carré - 1) / 3) * 3 + 1 .. ((Carré - 1) / 3) * 3 + 3 loop
            for Colonne in ((Carré - 1) rem 3) * 3 + 1 .. ((Carré - 1) rem 3) * 3 + 3 loop
               if Grille.Sudoku.RetourneValeur (Ligne, Colonne) /= 0 then
                  for Ligne2 in ((Carré - 1) / 3) * 3 + 1 .. ((Carré - 1) / 3) * 3 + 3 loop
                     for Colonne2 in
                          ((Carré - 1) rem 3) * 3 + 1 .. ((Carré - 1) rem 3) * 3 + 3
                     loop
                        Grille.Sudoku.SupprimeValeur
                          (Ligne2,
                           Colonne2,
                           Grille.Sudoku.RetourneValeur (Ligne, Colonne));
                     end loop;
                  end loop;
               end if;
            end loop;
         end loop;
         --         end loop;
         -- Règle 6 : valider si le chiffre est le seul possible du carré
         --         for Valeur in Grille.Chiffre loop  -- pas d'erreur -v pour la valeur 0 !!!
         for Valeur in Grille.ChiffresPossibles'Range loop
            Vide := True;
            Double_Boucle : for Ligne in
                 ((Carré - 1) / 3) * 3 + 1 .. ((Carré - 1) / 3) * 3 + 3
            loop
               for Colonne in ((Carré - 1) rem 3) * 3 + 1 .. ((Carré - 1) rem 3) * 3 + 3 loop
                  if Grille.Sudoku.RetourneValeursPossibles (Ligne, Colonne) (Valeur) then
                     if Vide then
                        PositionLigne   := Ligne;
                        PositionColonne := Colonne;
                        Vide            := False;
                     else
                        Vide := True;
                        exit Double_Boucle;
                     end if;
                  end if;
               end loop;
            end loop Double_Boucle;
            if not Vide
              and then Grille.Sudoku.RetourneValeur (PositionLigne, PositionColonne) = 0
            then
               Write ("Régle 6 : ");
               Grille.Sudoku.PositionneValeur (PositionLigne, PositionColonne, Valeur);
            end if;
         end loop;

         -- Vérifie si le carré est complet
         Fini := True;
         --         for Carré in Grille.Indice loop
         for Ligne in ((Carré - 1) / 3) * 3 + 1 .. ((Carré - 1) / 3) * 3 + 3 loop
            for Colonne in ((Carré - 1) rem 3) * 3 + 1 .. ((Carré - 1) rem 3) * 3 + 3 loop
               if Grille.Sudoku.RetourneValeur (Ligne, Colonne) = 0 then
                  Fini := False;
               end if;
            end loop;
         end loop;
         --         end loop;
         exit when Fini;
      end loop;
      Writeln ("Tâche carré " & Carré'Img & " est complète.");
      accept Terminé;
   end RésoudCarré;

   task Ordonanceur is
      entry Démarre;
      entry Terminé;
   end Ordonanceur;
   task body Ordonanceur is
      TâcheLignes   : array (Grille.Indice) of access RésoudLigne;
      TâcheColonnes : array (Grille.Indice) of access RésoudColonne;
      TâcheCarrés   : array (Grille.Indice) of access RésoudCarré;
   begin
      loop
         select
            accept Démarre;
            for Index in Grille.Indice loop
               TâcheLignes (Index)   := new RésoudLigne (Index);
               TâcheColonnes (Index) := new RésoudColonne (Index);
               TâcheCarrés (Index)   := new RésoudCarré (Index);
            end loop;
            for Index in Grille.Indice loop
               select
                  TâcheLignes (Index).Terminé;
               or
                  delay 1.0;
                  Writeln ("Tâche ligne " & Index'Img & " non terminée.");
               end select;
               select
                  TâcheColonnes (Index).Terminé;
               or
                  delay 1.0;
                  Writeln ("Tâche colonne " & Index'Img & " non terminée.");
               end select;
               select
                  TâcheCarrés (Index).Terminé;
               or
                  delay 1.0;
                  Writeln ("Tâche carré " & Index'Img & " non terminée.");
               end select;
            end loop;
            for Index in Grille.Indice loop
               if not TâcheLignes (Index)'Terminated then
                  abort TâcheLignes (Index).all;
               end if;
               if not TâcheColonnes (Index)'Terminated then
                  abort TâcheColonnes (Index).all;
               end if;
               if not TâcheCarrés (Index)'Terminated then
                  abort TâcheCarrés (Index).all;
               end if;
            end loop;
            accept Terminé;
         or
            terminate;
         end select;
      end loop;
   end Ordonanceur;

   G : Grille.TGrille :=
     ((0, 0, 6, 5, 0, 0, 4, 0, 0),
      (5, 9, 4, 6, 0, 0, 3, 8, 0),
      (0, 0, 0, 7, 4, 0, 5, 0, 9),
      (3, 0, 8, 0, 0, 0, 6, 0, 0),
      (0, 0, 0, 3, 0, 4, 0, 0, 0),
      (0, 0, 2, 0, 0, 0, 8, 0, 3),
      (6, 0, 9, 0, 2, 1, 0, 0, 0),
      (0, 8, 1, 0, 0, 5, 9, 3, 6),
      (0, 0, 5, 0, 0, 3, 1, 0, 0));

   procedure Affiche_Grille is
   begin
      SetColor (WColor);
      for Lig in Grille.Indice loop
         for Col in Grille.Indice loop
            Affiche_Chiffre (Lig, Col, G (Lig, Col));
         end loop;
      end loop;
   end Affiche_Grille;

   procedure Efface is
   begin
      SetViewPort (1, 16, GetMaxX - 1, GetMaxY - 11, ClipOn);
      ClearViewPort;
      SetViewPort (0, 0, GetMaxX, GetMaxY, ClipOn);
      Affiche_Grille;
      AffMenu (5, TMenu (Menu));
      AffText ("Choisisez une option du menu!");
   end Efface;

   procedure Sauve is
      Nom : String (1 .. 256);
      F   : Text;
   begin
      GetText ("Entrer le nom du fichier : ", Nom);
      Assign (F, Nom);
      Rewrite (F);
      for Lig in Grille.Indice loop
         for Col in Grille.Indice loop
            Write (F, G (Lig, Col)'Img);
         end loop;
         Writeln (F);
      end loop;
      Close (F);
   end Sauve;

   procedure Charge is
      Nom : String (1 .. 256);
      F   : Text;
   begin
      GetText ("Entrer le nom du fichier : ", Nom);
      Assign (F, Nom);
      Reset (F);
      for Lig in Grille.Indice loop
         for Col in Grille.Indice loop
            Read (F, G (Lig, Col));
         end loop;
      end loop;
      Close (F);
      Affiche_Grille;
   end Charge;

   procedure Démarre is
      T0 : Natural;
   begin
      Grille.Sudoku.PositionneGrille (G);
      Grille.Sudoku.ConstruitValeursPossibles;
      SetColor (AColor);
      T0 := HorlogeMS;
      Ordonanceur.Démarre;
      Ordonanceur.Terminé;
      Writeln ("Sudoku completed ?!");
      Grille.Imprime;
      Writeln ("Temps d'exécution :" & Natural'Image (HorlogeMS - T0) & " millisecondes");
   end Démarre;

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
         AColor := 4;
      when others =>
         null;
   end case;
   SetColor (WColor);
   Rectangle (0, 15, GetMaxX, GetMaxY - 10);
   SetFillStyle (SolidFill, BackColor);
   AffMenu (5, TMenu (Menu));
   if MouseInit then
      MouseSetGraphBlock (StandardCursor);
      MouseWindow (1, 0, GetMaxX - 1, GetMaxY - 11);
      ShowMouse;
   end if;
   Affiche_Grille;
   AffText ("Choisissez une option du menu!");
   loop
      if KeyPressed then
         case UpCase (ReadKey) is
            when 'R' =>
               Démarre;
            when 'E' =>
               Efface;
            when 'S' =>
               Sauve;
            when 'C' =>
               Charge;
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
                  Démarre;
               when 64 .. 127 =>
                  Efface;
               when 128 .. 191 =>
                  Sauve;
               when 192 .. 255 =>
                  Charge;
               when 256 .. 320 =>
                  Clear;
               when others =>
                  null;
            end case;
         end if;
      end if;
   end loop;
end Sudoku;
