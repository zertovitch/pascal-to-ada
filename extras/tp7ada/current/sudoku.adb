-------------------------------------------------------------------------------
-- NOM DU CSU (principal)           : sudoku.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 3.0a
-- DATE DE LA DERNIERE MISE A JOUR  : 12 janvier 2014
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
-- COPYRIGHT                        : (c) Pascal Pignard 2010-2014
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
   Pause                                             : constant Word := 500;

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
      TH := TextHeight (+Ch);
      TW := TextWidth (+Ch);
      Bar
        (100 + Colonne * TW,
         100 + Ligne * TH,
         100 + (Colonne + 1) * TW,
         100 + (Ligne + 1) * TH);
      OutTextXY (100 + Colonne * TW, 100 + Ligne * TH, +Ch);
   end Affiche_Chiffre;

   package Grille is
      subtype Chiffre is Integer range 0 .. 9;
      subtype Indice is Integer range 1 .. 9;
      type TGrille is array (Indice, Indice) of Chiffre;
      type ChiffresPossibles is array (Chiffre range 1 .. 9) of Boolean;
      function NumCarré (Ligne, Colonne : Indice) return Indice;
      function EstAligné
        (DansCarré        : Indice;
         Valeur           : Chiffre;
         SaufLig, SaufCol : Chiffre)
         return             Boolean;
      procedure Imprime;
      function Résolu return Boolean;
      procedure Prend (Ligne, Colonne : Indice);
      procedure Libère (Ligne, Colonne : Indice);
      function RetourneValeur (Ligne, Colonne : Indice) return Chiffre;
      procedure PositionneValeur (Ligne, Colonne : Indice; Valeur : Chiffre);
      procedure InitialiseGrille (UneGrille : TGrille);
      function RetourneValeursPossibles (Ligne, Colonne : Indice) return ChiffresPossibles;
      procedure SupprimeValeur (Ligne, Colonne : Indice; Valeur : Chiffre);
   end Grille;

   package body Grille is
      type TableauValeursPossibles is array (Indice, Indice) of ChiffresPossibles;
      Sudoku           : TGrille                 := (others => (others => 0));
      ValeursPossibles : TableauValeursPossibles := (others => (others => (others => True)));

      protected type Cases is
         entry Prend;
         procedure Libère;
      private
         Verrouillé : Boolean := False;
      end Cases;

      protected body Cases is
         entry Prend when not Verrouillé is
         begin
            Verrouillé := True;
         end Prend;
         procedure Libère is
         begin
            Verrouillé := False;
         end Libère;
      end Cases;

      type TVerrou is array (Indice, Indice) of Cases;
      Verrous : TVerrou;

      procedure Prend (Ligne, Colonne : Indice) is
      begin
         Verrous (Ligne, Colonne).Prend;
      end Prend;

      procedure Libère (Ligne, Colonne : Indice) is
      begin
         Verrous (Ligne, Colonne).Libère;
      end Libère;

      function NumCarré (Ligne, Colonne : Indice) return Indice is
      begin
         return ((Ligne - 1) / 3) * 3 + ((Colonne - 1) / 3) + 1;
      end NumCarré;

      function EstAligné
        (DansCarré        : Indice;
         Valeur           : Chiffre;
         SaufLig, SaufCol : Chiffre)
         return             Boolean
      is
         NbVal, ValCar : Natural := 0;
      begin
         for Ligne in ((DansCarré - 1) / 3) * 3 + 1 .. ((DansCarré - 1) / 3) * 3 + 3 loop
            for Colonne in
                 ((DansCarré - 1) rem 3) * 3 + 1 .. ((DansCarré - 1) rem 3) * 3 + 3
            loop
               if Grille.RetourneValeur (Ligne, Colonne) = Valeur
                 or else (Grille.RetourneValeur (Ligne, Colonne) = 0
                         and then Grille.RetourneValeursPossibles (Ligne, Colonne) (Valeur))
               then
                  if Ligne = SaufLig or Colonne = SaufCol then
                     NbVal := NbVal + 1;
                  else
                     ValCar := ValCar + 1;
                  end if;
               end if;
            end loop;
         end loop;
         return NbVal > 1 and ValCar = 0;
      end EstAligné;

      procedure SupprimeValeurLigne (Ligne : Indice; Valeur : Chiffre) is
      begin
         -- Règle 1' : supprimer le chiffre des cases de la même ligne
         for Colonne in Grille.Indice loop
            if Grille.RetourneValeursPossibles (Ligne, Colonne) (Valeur) then
               Writeln ("Régle 1' :" & Ligne'Img & ',' & Colonne'Img & ',' & Valeur'Img);
               Grille.SupprimeValeur (Ligne, Colonne, Valeur);
            end if;
         end loop;
      end SupprimeValeurLigne;

      procedure SupprimeValeurColonne (Colonne : Indice; Valeur : Chiffre) is
      begin
         -- Règle 2' : supprimer le chiffre des cases de la même colonne
         for Ligne in Grille.Indice loop
            if Grille.RetourneValeursPossibles (Ligne, Colonne) (Valeur) then
               Writeln ("Régle 2' :" & Ligne'Img & ',' & Colonne'Img & ',' & Valeur'Img);
               Grille.SupprimeValeur (Ligne, Colonne, Valeur);
            end if;
         end loop;
      end SupprimeValeurColonne;

      procedure SupprimeValeurCarré (Ligne, Colonne : Indice; Valeur : Chiffre) is
         Carré : constant Indice := NumCarré (Ligne, Colonne);
      begin
         -- Règle 3' : supprimer le chiffre des cases du même carré
         for Ligne in ((Carré - 1) / 3) * 3 + 1 .. ((Carré - 1) / 3) * 3 + 3 loop
            for Colonne in ((Carré - 1) rem 3) * 3 + 1 .. ((Carré - 1) rem 3) * 3 + 3 loop
               if Grille.RetourneValeursPossibles (Ligne, Colonne) (Valeur) then
                  Writeln ("Régle 3' : " & Ligne'Img & ',' & Colonne'Img & ',' & Valeur'Img);
                  Grille.SupprimeValeur (Ligne, Colonne, Valeur);
               end if;
            end loop;
         end loop;
      end SupprimeValeurCarré;

      function RetourneValeur (Ligne, Colonne : Indice) return Chiffre is
      begin
         return Sudoku (Ligne, Colonne);
      end RetourneValeur;

      procedure PositionneValeur (Ligne, Colonne : Indice; Valeur : Chiffre) is
      begin
         Writeln ("Positionne valeur : " & Ligne'Img & ',' & Colonne'Img & ',' & Valeur'Img);
         Sudoku (Ligne, Colonne)           := Valeur;
         ValeursPossibles (Ligne, Colonne) := (others => False);
         SupprimeValeurLigne (Ligne, Valeur);
         SupprimeValeurColonne (Colonne, Valeur);
         SupprimeValeurCarré (Ligne, Colonne, Valeur);
         Affiche_Chiffre (Ligne, Colonne, Valeur);
         --           Delay1 (Pause);
      end PositionneValeur;

      procedure InitialiseGrille (UneGrille : TGrille) is
      begin
         Sudoku := UneGrille;
         for Ligne in Indice loop
            for Colonne in Indice loop
               Grille.Libère (Ligne, Colonne);
               if RetourneValeur (Ligne, Colonne) /= 0 then
                  ValeursPossibles (Ligne, Colonne) := (others => False);
               else
                  ValeursPossibles (Ligne, Colonne) := (others => True);
               end if;
            end loop;
         end loop;
      end InitialiseGrille;

      function RetourneValeursPossibles (Ligne, Colonne : Indice) return ChiffresPossibles is
      begin
         return ValeursPossibles (Ligne, Colonne);
      end RetourneValeursPossibles;

      procedure SupprimeValeur (Ligne, Colonne : Indice; Valeur : Chiffre) is
      begin
         ValeursPossibles (Ligne, Colonne) (Valeur)  := False;
      end SupprimeValeur;

      procedure Imprime is
      begin
         for Index1 in TGrille'Range (1) loop
            for Index2 in TGrille'Range (2) loop
               Write (RetourneValeur (Index1, Index2)'Img);
            end loop;
            Writeln;
         end loop;
      end Imprime;

      function Résolu return Boolean is
         function VérifLigne (Ligne : Indice) return Boolean is
            Tab_Vérif : array (Indice) of Chiffre := (1, 2, 3, 4, 5, 6, 7, 8, 9);
         begin
            for Colonne in Indice loop
               if Grille.RetourneValeur (Ligne, Colonne) = 0 then
                  return False;
               end if;
               if Grille.RetourneValeur (Ligne, Colonne) =
                  Tab_Vérif (Grille.RetourneValeur (Ligne, Colonne))
               then
                  Tab_Vérif (Grille.RetourneValeur (Ligne, Colonne))  := 0;
               else
                  return False;
               end if;
            end loop;
            return True;
         end VérifLigne;
         function VérifColonne (Colonne : Indice) return Boolean is
            Tab_Vérif : array (Indice) of Chiffre := (1, 2, 3, 4, 5, 6, 7, 8, 9);
         begin
            for Ligne in Indice loop
               if Grille.RetourneValeur (Ligne, Colonne) = 0 then
                  return False;
               end if;
               if Grille.RetourneValeur (Ligne, Colonne) =
                  Tab_Vérif (Grille.RetourneValeur (Ligne, Colonne))
               then
                  Tab_Vérif (Grille.RetourneValeur (Ligne, Colonne))  := 0;
               else
                  return False;
               end if;
            end loop;
            return True;
         end VérifColonne;
         function VérifCarré (Ligne, Colonne : Natural) return Boolean is
            Tab_Vérif : array (Indice) of Chiffre := (1, 2, 3, 4, 5, 6, 7, 8, 9);
         begin
            for Ligne2 in Ligne + 1 .. Ligne + 3 loop
               for Colonne2 in Colonne + 1 .. Colonne + 3 loop
                  if Grille.RetourneValeur (Ligne2, Colonne2) = 0 then
                     return False;
                  end if;
                  if Grille.RetourneValeur (Ligne2, Colonne2) =
                     Tab_Vérif (Grille.RetourneValeur (Ligne2, Colonne2))
                  then
                     Tab_Vérif (Grille.RetourneValeur (Ligne2, Colonne2))  := 0;
                  else
                     return False;
                  end if;
               end loop;
            end loop;
            return True;
         end VérifCarré;
      begin
         for Ind in Indice loop
            if not VérifLigne (Ind) or not VérifColonne (Ind) then
               return False;
            end if;
         end loop;
         for Lig in 0 .. 2 loop
            for Col in 0 .. 2 loop
               if not VérifCarré (Lig * 3, Col * 3) then
                  return False;
               end if;
            end loop;
         end loop;
         return True;
      end Résolu;

   end Grille;

   task type RésoudLigne (Ligne : Grille.Indice) is
      entry Terminé;
   end RésoudLigne;

   task body RésoudLigne is
      Fini               : Boolean;
      Carré              : Grille.Indice;
      ValeurAPositionner : Grille.Chiffre;
      Seul               : Boolean;
   begin
      loop

      -- Règle 1 : supprimer le chiffre des cases de la même ligne
         for Colonne in Grille.Indice loop
            Grille.Prend (Ligne, Colonne);
            if Grille.RetourneValeur (Ligne, Colonne) /= 0 then
               for Colonne2 in Grille.Indice loop
                  if Grille.RetourneValeursPossibles (Ligne, Colonne2) (Grille.RetourneValeur
                                                                           (Ligne,
                                                                            Colonne))
                  then
                     Writeln
                       ("Régle 1 :" &
                        Ligne'Img &
                        ',' &
                        Colonne2'Img &
                        ',' &
                        Grille.RetourneValeur (Ligne, Colonne)'Img);
                     Grille.SupprimeValeur
                       (Ligne,
                        Colonne2,
                        Grille.RetourneValeur (Ligne, Colonne));
                  end if;
               end loop;
            end if;
         end loop;

         -- Règle 4 : valider si le chiffre est le seul possible de la ligne
         for Colonne in Grille.Indice loop
            if Grille.RetourneValeur (Ligne, Colonne) = 0 then
               ValeurAPositionner := 0;
               for Valeur in Grille.ChiffresPossibles'Range loop
                  if Grille.RetourneValeursPossibles (Ligne, Colonne) (Valeur) then
                     Seul := True;
                     for Colonne2 in Grille.Indice loop
                        if Colonne2 /= Colonne then
                           if Grille.RetourneValeursPossibles (Ligne, Colonne2) (Valeur) then
                              Seul := False;
                              exit;
                           end if;
                        end if;
                     end loop;
                     if Seul then
                        if ValeurAPositionner = 0 then
                           ValeurAPositionner := Valeur;
                        else
                           ValeurAPositionner := 0;
                        end if;
                     end if;
                  end if;
               end loop;
               if ValeurAPositionner /= 0 then
                  Writeln
                    ("Régle 4 :" & Ligne'Img & ',' & Colonne'Img & ',' & ValeurAPositionner'Img);
                  Grille.PositionneValeur (Ligne, Colonne, ValeurAPositionner);
               end if;
            end if;
         end loop;

         -- Règle 7 : Dans un carré si une valeur possible n'apparait que sur une ligne
         --           alors supprimer les valeurs de la ligne hors du carré
         for Colonne in Grille.Indice loop
            if Grille.RetourneValeur (Ligne, Colonne) = 0 then
               for Valeur in Grille.ChiffresPossibles'Range loop
                  if Grille.RetourneValeursPossibles (Ligne, Colonne) (Valeur) then
                     Carré := Grille.NumCarré (Ligne, Colonne);
                     if Grille.EstAligné (Carré, Valeur, Ligne, 0) then
                        for Colonne in Grille.Indice loop
                           if Grille.NumCarré (Ligne, Colonne) /= Carré then
                              if Grille.RetourneValeursPossibles (Ligne, Colonne) (Valeur) then
                                 Grille.SupprimeValeur (Ligne, Colonne, Valeur);
                                 Writeln
                                   ("Régle 7 : Ligne " &
                                    Ligne'Img &
                                    " Colonne " &
                                    Colonne'Img &
                                    " Carré " &
                                    Carré'Img &
                                    ", Valeur " &
                                    Valeur'Img);
                              end if;
                           end if;
                        end loop;
                     end if;
                  end if;
               end loop;
            end if;
         end loop;

         -- Vérifie si la ligne est complète
         Fini := True;
         for Colonne in Grille.Indice loop
            if Grille.RetourneValeur (Ligne, Colonne) = 0 then
               Fini := False;
            end if;
            Grille.Libère (Ligne, Colonne);
         end loop;
         exit when Fini;
         Delay1 (Pause);
      end loop;
      Writeln ("Tâche ligne " & Ligne'Img & " est complète.");
      accept Terminé;
   end RésoudLigne;

   task type RésoudColonne (Colonne : Grille.Indice) is
   end RésoudColonne;

   task body RésoudColonne is
      Fini               : Boolean;
      Carré              : Grille.Indice;
      Seul               : Boolean;
      ValeurAPositionner : Grille.Chiffre;
   begin
      loop

      -- Règle 2 : supprimer le chiffre des cases de la même colonne
         for Ligne in Grille.Indice loop
            Grille.Prend (Ligne, Colonne);
            if Grille.RetourneValeur (Ligne, Colonne) /= 0 then
               for Ligne2 in Grille.Indice loop
                  if Grille.RetourneValeursPossibles (Ligne2, Colonne) (Grille.RetourneValeur
                                                                           (Ligne,
                                                                            Colonne))
                  then
                     Writeln
                       ("Régle 2 :" &
                        Ligne'Img &
                        ',' &
                        Colonne'Img &
                        ',' &
                        Grille.RetourneValeur (Ligne, Colonne)'Img);
                     Grille.SupprimeValeur
                       (Ligne2,
                        Colonne,
                        Grille.RetourneValeur (Ligne, Colonne));
                  end if;
               end loop;
            end if;
         end loop;

         -- Règle 5 : valider si le chiffre est le seul possible de la colonne
         for Ligne in Grille.Indice loop
            if Grille.RetourneValeur (Ligne, Colonne) = 0 then
               ValeurAPositionner := 0;
               for Valeur in Grille.ChiffresPossibles'Range loop
                  if Grille.RetourneValeursPossibles (Ligne, Colonne) (Valeur) then
                     Seul := True;
                     for Ligne2 in Grille.Indice loop
                        if Ligne2 /= Ligne then
                           if Grille.RetourneValeursPossibles (Ligne2, Colonne) (Valeur) then
                              Seul := False;
                              exit;
                           end if;
                        end if;
                     end loop;
                     if Seul then
                        if ValeurAPositionner = 0 then
                           ValeurAPositionner := Valeur;
                        else
                           ValeurAPositionner := 0;
                        end if;
                     end if;
                  end if;
               end loop;
               if ValeurAPositionner /= 0 then
                  Writeln
                    ("Régle 5 :" & Ligne'Img & ',' & Colonne'Img & ',' & ValeurAPositionner'Img);
                  Grille.PositionneValeur (Ligne, Colonne, ValeurAPositionner);
               end if;
            end if;
         end loop;

         -- Règle 8 : Dans un carré si une valeur possible n'apparait que sur une colonne
         --           alors supprimer les valeurs de la colonne hors du carré
         for Ligne in Grille.Indice loop
            if Grille.RetourneValeur (Ligne, Colonne) = 0 then
               for Valeur in Grille.ChiffresPossibles'Range loop
                  if Grille.RetourneValeursPossibles (Ligne, Colonne) (Valeur) then
                     Carré := Grille.NumCarré (Ligne, Colonne);
                     if Grille.EstAligné (Carré, Valeur, 0, Colonne) then
                        for Ligne in Grille.Indice loop
                           if Grille.NumCarré (Ligne, Colonne) /= Carré then
                              if Grille.RetourneValeursPossibles (Ligne, Colonne) (Valeur) then
                                 Grille.SupprimeValeur (Ligne, Colonne, Valeur);
                                 Writeln
                                   ("Régle 8 : Ligne " &
                                    Ligne'Img &
                                    " Colonne " &
                                    Colonne'Img &
                                    " Carré " &
                                    Carré'Img &
                                    ", Valeur " &
                                    Valeur'Img);
                              end if;
                           end if;
                        end loop;
                     end if;
                  end if;
               end loop;
            end if;
         end loop;

         -- Vérifie si la colonne est complète
         Fini := True;
         for Ligne in Grille.Indice loop
            if Grille.RetourneValeur (Ligne, Colonne) = 0 then
               Fini := False;
            end if;
            Grille.Libère (Ligne, Colonne);
         end loop;
         exit when Fini;
         Delay1 (Pause);
      end loop;
      Writeln ("Tâche colonne " & Colonne'Img & " est complète.");
   end RésoudColonne;

   task type RésoudCarré (Carré : Grille.Indice) is
   end RésoudCarré;

   task body RésoudCarré is
      Fini               : Boolean;
      Seul               : Boolean;
      ValeurAPositionner : Grille.Chiffre;
   begin
      loop

      -- Règle 3 : supprimer le chiffre des cases du même carré
         for Ligne in ((Carré - 1) / 3) * 3 + 1 .. ((Carré - 1) / 3) * 3 + 3 loop
            for Colonne in ((Carré - 1) rem 3) * 3 + 1 .. ((Carré - 1) rem 3) * 3 + 3 loop
               Grille.Prend (Ligne, Colonne);
               if Grille.RetourneValeur (Ligne, Colonne) /= 0 then
                  for Ligne2 in ((Carré - 1) / 3) * 3 + 1 .. ((Carré - 1) / 3) * 3 + 3 loop
                     for Colonne2 in
                          ((Carré - 1) rem 3) * 3 + 1 .. ((Carré - 1) rem 3) * 3 + 3
                     loop
                        if Grille.RetourneValeursPossibles (Ligne2, Colonne2) (
                             Grille.RetourneValeur (Ligne, Colonne))
                        then
                           Writeln
                             ("Régle 3 : " &
                              Ligne2'Img &
                              ',' &
                              Colonne2'Img &
                              ',' &
                              Grille.RetourneValeur (Ligne, Colonne)'Img);
                           Grille.SupprimeValeur
                             (Ligne2,
                              Colonne2,
                              Grille.RetourneValeur (Ligne, Colonne));
                        end if;
                     end loop;
                  end loop;
               end if;
            end loop;
         end loop;

         -- Règle 6 : valider si le chiffre est le seul possible du carré
         for Ligne in ((Carré - 1) / 3) * 3 + 1 .. ((Carré - 1) / 3) * 3 + 3 loop
            for Colonne in ((Carré - 1) rem 3) * 3 + 1 .. ((Carré - 1) rem 3) * 3 + 3 loop
               if Grille.RetourneValeur (Ligne, Colonne) = 0 then
                  ValeurAPositionner := 0;
                  for Valeur in Grille.ChiffresPossibles'Range loop
                     if Grille.RetourneValeursPossibles (Ligne, Colonne) (Valeur) then
                        Seul := True;
                        DoubleBoucle : for Ligne2 in
                             ((Carré - 1) / 3) * 3 + 1 .. ((Carré - 1) / 3) * 3 + 3
                        loop
                           for Colonne2 in
                                ((Carré - 1) rem 3) * 3 + 1 .. ((Carré - 1) rem 3) * 3 + 3
                           loop
                              if Ligne2 /= Ligne or Colonne2 /= Colonne then
                                 if Grille.RetourneValeursPossibles (Ligne2, Colonne2) (Valeur)
                                 then
                                    Seul := False;
                                    exit DoubleBoucle;
                                 end if;
                              end if;
                           end loop;
                        end loop DoubleBoucle;
                        if Seul then
                           if ValeurAPositionner = 0 then
                              ValeurAPositionner := Valeur;
                           else
                              ValeurAPositionner := 0;
                           end if;
                        end if;
                     end if;
                  end loop;
                  if ValeurAPositionner /= 0 then
                     Writeln
                       ("Régle 6 :" &
                        Ligne'Img &
                        ',' &
                        Colonne'Img &
                        ',' &
                        ValeurAPositionner'Img);
                     Grille.PositionneValeur (Ligne, Colonne, ValeurAPositionner);
                  end if;
               end if;
            end loop;
         end loop;

         -- Vérifie si le carré est complet
         Fini := True;
         for Ligne in ((Carré - 1) / 3) * 3 + 1 .. ((Carré - 1) / 3) * 3 + 3 loop
            for Colonne in ((Carré - 1) rem 3) * 3 + 1 .. ((Carré - 1) rem 3) * 3 + 3 loop
               if Grille.RetourneValeur (Ligne, Colonne) = 0 then
                  Fini := False;
               end if;
               Grille.Libère (Ligne, Colonne);
            end loop;
         end loop;
         exit when Fini;
         Delay1 (Pause);
      end loop;
      Writeln ("Tâche carré " & Carré'Img & " est complète.");
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
                  delay 10.0 - Duration (Index);
                  Writeln ("Tâche ligne " & Index'Img & " non terminée.");
                  exit;
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
      SetTextStyle (DefaultFont, HorizDir, 3);
      for Lig in Grille.Indice loop
         for Col in Grille.Indice loop
            Affiche_Chiffre (Lig, Col, G (Lig, Col));
         end loop;
      end loop;
      SetTextStyle (DefaultFont, HorizDir, 1);
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
      MouseSetGraphBlock (75); -- GDK_WATCH
      Grille.InitialiseGrille (G);
      SetColor (AColor);
      T0 := HorlogeMS;
      SetTextStyle (DefaultFont, HorizDir, 3);
      Ordonanceur.Démarre;
      Ordonanceur.Terminé;
      SetTextStyle (DefaultFont, HorizDir, 1);
      Grille.Imprime;
      if Grille.Résolu then
         Writeln ("Sudoku résolu.");
      else
         Writeln ("Sudoku non résolu.");
      end if;
      Writeln ("Temps d'exécution :" & Natural'Image (HorlogeMS - T0) & " millisecondes.");
      MouseSetGraphBlock (StandardCursor);
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
