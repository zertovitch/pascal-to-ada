-------------------------------------------------------------------------------
-- NOM DU CSU (principal)           : surface.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 1.0a
-- DATE DE LA DERNIERE MISE A JOUR  : 3 mai 2012
-- ROLE DU CSU                      : Représentation graphique de surfaces
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
-- Translated on 3-May-2012 by (New) P2Ada v. 28-Oct-2009

with TP7, TP7.System, TP7.Crt, TP7.Graph;
use  TP7, TP7.System, TP7.Crt, TP7.Graph;

procedure Surfaces is
   subtype Integer is TPInteger;

   kz      : Real             := 10.0;
   Premier : Boolean          := True;
   Limite  : constant Integer := 4;
   TColor  : Integer;

   function Z (X, Y : Real) return Real is
   begin
      --        return -kz * (Sqr ((X - 1.0) * Sin (2.0 * Y)) + Sqr ((X + 1.0) * Cos (2.0 * Y))) +
      --               10.0;
      return X * X + Y * Y - 49.0;
   end Z;

   function maxZ return Real is
   begin
      return kz;
   end maxZ;

   type Tableau is array (0 .. 800) of Integer;

   infx, infy, supx, supy, AX, BX, AY, BY, minx, miny, maxx, maxy, a1, a2 : Real;
   NbrCourbes, xpos, ypos, xgraph, ygraph                                 : Integer;
   Proj                                                                   :
     array (1 .. 2, 1 .. 3) of Real;
   Bas, Haut                                                              : Tableau;

   procedure defineworld (Xmin, Ymax, Xmax, Ymin : Real) is
   begin
      AX := Real (GetMaxX) / (Xmax - Xmin);
      BX := Real (GetMaxX) * Xmin / (Xmin - Xmax);
      AY := Real (GetMaxY) / (Ymin - Ymax);
      BY := Real (GetMaxY) * Ymin / (Ymin - Ymax);
   end defineworld;

   function WindowX (X : Real) return Integer is
   begin
      return Round (BX + X * AX);
   end WindowX;

   function WindowY (Y : Real) return Integer is
   begin
      return Round (BY + Y * AY);
   end WindowY;

   procedure Calcule is
      ang1, ang2, m : Real;
   begin
      if Premier then
         infx       := -7.0;
         supx       := 7.0;
         infy       := -7.0;
         supy       := 7.0;
         a1         := 45.0;
         a2         := 70.0;
         NbrCourbes := 20;
         Premier    := False;
      end if;
      Write ("nombre de courbes (");
      Write (NbrCourbes);
      Write ("): ");
      Readln (NbrCourbes);
      Write ("multiplicateur de z (");
      Write (kz, 2, 0);
      Write (") ? ");
      Readln (kz);
      m := maxZ;
      Write ("premier angle (");
      Write (a1, 2, 0);
      Write ("°) ? ");
      Readln (a1);
      Write ("deuxieme angle (");
      Write (a2, 2, 0);
      Write ("°) ? ");
      Readln (a2);
      ang1 := Pi * a1 / 180.0;
      ang2 := Pi * a2 / 180.0;
      Write ("domaine de x: inf (");
      Write (infx, 5, 2);
      Write (") ? ");
      Readln (infx);
      Write ("              sup (");
      Write (supx, 5, 2);
      Write (") ? ");
      Readln (supx);
      Write ("domaine de y: inf (");
      Write (infy, 5, 2);
      Write (") ? ");
      Readln (infy);
      Write ("              sup (");
      Write (supy, 5, 2);
      Write (") ? ");
      Readln (supy);
      Proj (1, 1) := Cos (ang1);
      Proj (1, 2) := -Sin (ang1);
      Proj (1, 3) := 0.0;
      Proj (2, 1) := Sin (ang1) * Sin (ang2);
      Proj (2, 2) := Cos (ang1) * Sin (ang2);
      Proj (2, 3) := Cos (ang2);
      minx        := 1.1 * abs (Proj (1, 1)) * infx +
                     abs (Proj (1, 2)) * infy -
                     m * abs (Proj (1, 3));
      maxx        := 1.1 * abs (Proj (1, 1)) * supx +
                     abs (Proj (1, 2)) * supy +
                     m * abs (Proj (1, 3));
      miny        := 1.1 * abs (Proj (2, 1)) * infx +
                     abs (Proj (2, 2)) * infy -
                     m * abs (Proj (2, 3));
      maxy        := 1.1 * abs (Proj (2, 1)) * supx +
                     abs (Proj (2, 2)) * supy +
                     m * abs (Proj (2, 3));
   end Calcule;

   procedure DrawLineDirect (x1, y1, x2, y2 : Integer) is
      x, y, Deltax, Deltay, xstep, ystep, direction : Integer;
      LineInfo                                      : LineSettingsType;
      ok                                            : Boolean := True;
   begin
      GetLineSettings (LineInfo);
      x     := x1;
      y     := y1;
      xstep := 1;
      ystep := 1;
      if x1 > x2 then
         xstep := -1;
      end if;
      if y1 > y2 then
         ystep := -1;
      end if;
      Deltax := abs (x2 - x1);
      Deltay := abs (y2 - y1);
      if Deltax = 0 then
         direction := -1;
      else
         direction := 0;
      end if;
      while not ((x = x2) and (y = y2)) loop
         if ok then
            PutPixel (x, y, TColor);
         end if;
         if direction < 0 then
            y         := y + ystep;
            direction := direction + Deltax;
         else
            if y > Haut (x) then
               Haut (x) := y;
            end if;
            if y < Bas (x) then
               Bas (x) := y;
            end if;
            x         := x + xstep;
            direction := direction - Deltay;
         end if;
         ok := (y < Bas (x)) or (y > Haut (x));
      end loop;
   end DrawLineDirect;

   procedure NextLine is
   begin
      DrawLineDirect (xpos, ypos, xgraph, ygraph);
      xpos := xgraph;
      ypos := ygraph;
   end NextLine;

   procedure InitTab (Tab : in out Tableau; n : Integer) is
   begin
      for i in 0 .. GetMaxX loop
         Tab (i) := n;
      end loop;
   end InitTab;

   procedure Projette (X, Y, Z : Real; Xg, Yg : in out Integer) is
   begin
      Xg := WindowX (Proj (1, 1) * X + Proj (1, 2) * Y + Proj (1, 3) * Z);
      Yg := WindowY (Proj (2, 1) * X + Proj (2, 2) * Y + Proj (2, 3) * Z);
   end Projette;

   procedure Dessine is
      Test                     : Integer;
      r, h, StepX, StepY, X, Y : Real;
   begin
      StepY := (supy - infy) / Real (NbrCourbes);
      StepX := (supx - infx) / Real (NbrCourbes);
      Y     := infy;
      h     := StepX;
      InitTab (Haut, 0);
      InitTab (Bas, 1000);
      for i in 0 .. NbrCourbes loop
         X := infx;
         Projette (X, Y, Z (X, Y), xpos, ypos);
         loop
            loop
               r := X + h;
               Projette (r, Y, Z (r, Y), xgraph, ygraph);
               Test := abs (xgraph - xpos) + abs (ygraph - ypos);
               if Test > Limite then
                  h := h / 2.0;
               end if;
               if Test <= Limite / 4 then
                  h := h * 2.0;
               end if;
               exit when Test <= Limite;
            end loop;
            X := r;
            NextLine;
            exit when X >= supx;
         end loop;
         if KeyPressed then
            return;
         end if;
         Y := Y + StepY;
      end loop;
      X := infx;
      h := StepY;
      InitTab (Haut, 0);
      InitTab (Bas, 1000);
      for i in 0 .. NbrCourbes loop
         Y := infy;
         Projette (X, Y, Z (X, Y), xpos, ypos);
         loop
            loop
               r := Y + h;
               Projette (X, r, Z (X, r), xgraph, ygraph);
               Test := abs (xgraph - xpos) + abs (ygraph - ypos);
               if Test > Limite then
                  h := h / 2.0;
               end if;
               if Test <= Limite / 4 then
                  h := h * 2.0;
               end if;
               exit when Test <= Limite;
            end loop;
            Y := r;
            NextLine;
            exit when Y >= supy;
         end loop;
         if KeyPressed then
            return;
         end if;
         X := X + StepX;
      end loop;
   end Dessine;

   car    : Char;
   gd, gm : Integer := 0;
begin
   loop
      ClrScr;
      Calcule;
      while KeyPressed loop
         car := ReadKey;
      end loop;
      gd := Detect;
      InitGraph (gd, gm, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      if (gd = EGA) or (gd = VGA) then
         TColor := 14;
      else
         TColor := 1;
      end if;
      defineworld (minx, maxy, maxx, miny);
      Dessine;
      OutText ("Appuyer sur: R pour recommencer, ESC pour arrêter.");
      loop
         car := ReadKey;
         car := UpCase (car);
         exit when car = 'R' or car = Character'Val (27);
      end loop;
      CloseGraph;
      exit when car /= 'R';
   end loop;
end Surfaces;
-- Translated on 3-May-2012 by (New) P2Ada v. 28-Oct-2009
