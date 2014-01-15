with TP7, TP7.System, TP7.Crt, TP7.Dos, TP7.Printer, TP7.Graph;
use  TP7, TP7.System, TP7.Crt, TP7.Dos, TP7.Printer, TP7.Graph;
with Ada.Unchecked_Deallocation;

procedure Exemples_TP7 is
   -- Based on Turbo Pascal examples from online help
   -- Usage with args : lisez-moi.txt test.txt 1256 src test

   subtype Integer is TPInteger;
   subtype P2Ada_String is String;

   procedure ExDebug is
   begin
      Writeln (Debug'Img);
   end ExDebug;
   procedure ExAbs is
      r : Real;
      i : Integer;
   begin
      r := Abs1 (-2.3);
      i := Abs1 (-157);
      Writeln (r'Img + i'Img);
   end ExAbs; -- block
   procedure ExAddr is
      p : Pointer;
   begin
      p := p'Address;
      Writeln (Pointer_Image (p));
   end ExAddr; -- block
   procedure ExAppend is
      f : Text;
   begin
      Assign (f, "test.txt");
      Rewrite (f);
      Writeln (f, "texte originel");
      Close (f);
      Append (f);
      Writeln (f, "texte ajouté");
      Close (f);
   end ExAppend; -- block
   procedure ExArc is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      for Rayon in 1 .. 5 loop
         Arc (100, 100, 0, 90, Rayon * 10);
      end loop;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExArc; -- block
   procedure ExArcTan is
      r : Real;
   begin
      r := ArcTan (Pi);
      Writeln (r'Img);
   end ExArcTan; -- block
   procedure ExAssign is
      f : Text;
   begin
      Assign (f, "");
      Rewrite (f);
      Writeln (f, "Sortie standard...");
      Close (f);
   end ExAssign; -- block
   procedure ExAssigned is
      P : Pointer;
   begin
      P := nil;
      if Assigned (P) then
         Writeln ("You won't see this");
      end if;
      P := P'Address;
      if Assigned (P) then
         Writeln ("You'll see this");
      end if;
   end ExAssigned; -- block
   procedure ExAssignCrt is
      f : Text;
   begin
      Write ("Sortie à l'écran ou à l'imprimante" + "[E, I] ? ");
      if UpCase (ReadKey) = 'I' then
         Assign (f, "PRN");
      else
         AssignCrt (f);
      end if;
      Rewrite (f);
      Writeln (f, "Affichage accéléré ....");
      Close (f);
   end ExAssignCrt; -- block
   procedure ExAssign_String is
      f1 : String (1 .. 11); -- String[10]
      f2 : String (1 .. 10); -- String[9]
   begin
      Assign_String (f1, "123456789");
      Writeln (f1);
      Assign_String (f1, To_TPString ("123456789"));
      Writeln (f1);
      Assign_String (f2, f1);
      Writeln (f2);
      Writeln (Is_Equal (f1, f2)'Img);
      Assign_String (f1, "1234567890123456789");
      Writeln (f1);
      Assign_String (f1, To_TPString ("1234567890123456789"));
      Writeln (f1);
      Assign_String (f2, f1);
      Writeln (f2);
   end ExAssign_String; -- block
   procedure ExBar is
      GraphPilote, GraphMode, Taille : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Taille := 10;
      for I in 1 .. 5 loop
         Bar (I * Taille, I * 10, Succ (I) * Taille, 200);
      end loop;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExBar; -- block
   procedure ExBar3D is
      GraphPilote, GraphMode : Integer := 0;
      y0, y1, y2, x1, x2     : Integer;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      y0 := 10;
      y1 := 60;
      y2 := 110;
      x1 := 10;
      x2 := 50;
      Bar3D (x1, y0, x2, y1, 10, TopOn);
      Bar3D (x1, y1, x2, y2, 10, TopOff);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExBar3D; -- block
   procedure ExAssignBlockReadWrite is
      DepuisF, DansF    : File;
      NumLus, NumEcrits : Word;
      type P2Ada_Anonym_1 is array (1 .. 2048) of Char;
      Tampon : P2Ada_Anonym_1;
   begin
      Assign (DepuisF, ParamStr (1));
      Reset (DepuisF, 1);
      Assign (DansF, "copie-" + ParamStr (1));
      Rewrite (DansF, 1);
      Writeln ("Copie de " + FileSize (DepuisF)'Img + " octets...");
      loop
         BlockRead (DepuisF, Tampon'Address, Tampon'Size / 8, NumLus);
         BlockWrite (DansF, Tampon'Address, NumLus, NumEcrits);
         exit when (NumLus = 0) or (NumEcrits /= NumLus);
      end loop;
      Writeln (Boolean'Image (Eof (DepuisF)));
      Close (DepuisF);
      Close (DansF);
   end ExAssignBlockReadWrite; -- block
   procedure ExChdir is
   begin
      ChDir (ParamStr (4));
      if IOResult /= 0 then
         Writeln ("Répertoire inexistant ");
      else
         ChDir ("..");
      end if;
   end ExChdir; -- block
   procedure ExChr is
   begin
      Writeln (Lst, Chr (12)'Img);
   end ExChr; -- block
   procedure ExCircle is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      for Rayon in 1 .. 5 loop
         Circle (100, 100, Rayon * 10);
      end loop;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExCircle; -- block
   procedure ExClearDevice is
      GraphPilote, GraphMode : Integer := 0;
      Ch                     : Char;
      pragma Unreferenced (Ch);
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Randomize;
      loop
         LineTo (Random (200), Random (200));
         exit when KeyPressed;
         Delay1 (1000);
      end loop;
      Ch := ReadKey;
      ClearDevice;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExClearDevice; -- block
   procedure ExClearViewport is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Rectangle (19, 19, GetMaxX - 19, GetMaxY - 19);
      SetViewPort (20, 20, GetMaxX - 20, GetMaxY - 20, ClipOn);
      OutTextXY (0, 0, "Frappez <Enter> pour effacer la fenêtre");
      Readln;
      ClearViewPort;
      OutTextXY (0, 0, "Frappez <Enter> pour quitter");
      Readln;
      CloseGraph;
   end ExClearViewport; -- block
   procedure ExClose is
      f : File;
   begin
      Assign (f, "lisez-moi.txt");
      Reset (f, 1);
      Writeln ("Taille de fichier = " + FileSize (f)'Img);
      Close (f);
   end ExClose; -- block
   procedure ExCloseGraph is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Line (0, 0, GetMaxX, GetMaxY);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExCloseGraph; -- block
   procedure ExClrEol is
   begin
      TextBackground (Crt.Cyan);
      GotoXY (8, 5);
      ClrEol;
   end ExClrEol; -- block
   procedure ExClrscr is
   begin
      TextBackground (Crt.LightGray);
      TextColor (Crt.Black);
      ClrScr;
   end ExClrscr; -- block
   procedure ExConcat is
   begin
      Writeln (Concat ("ABC", "DEF"));
      Writeln ("ABC" + "DEF");
   end ExConcat; -- block
   procedure ExCopy is
      s : P2Ada_String (1 .. 255);
   begin
      Assign_String (s, "ABCDEF");
      Writeln (Copy (s, 2, 3));
   end ExCopy; -- block
   procedure ExCos is
      r : Real;
   begin
      r := Cos (Pi);
      Writeln (r'Img);
   end ExCos; -- block
   procedure ExCsegDsegSsegSptrOfsSeg is
      procedure WriteHexWord (w : Word) is
         hexChars : P2Ada_String (1 .. 16);
      begin
         hexChars := "0123456789ABCDEF";
         --  Write(hexChars(BSR(Hi(w), 4) + 1)'img + hexChars(BAnd(Hi(w),
         --16#F#) + 1)'img + hexChars(BSR(Lo(w), 4) + 1)'img +
         --hexChars(BAnd(Lo(w), 16#F#) + 1)'img);
         Write
           (hexChars (w / 2 ** 12 + 1) +
            hexChars ((w / 2 ** 8) mod 2 ** 4 + 1) +
            hexChars ((w / 2 ** 4) mod 2 ** 4 + 1) +
            hexChars (w mod 2 ** 4 + 1));
      end WriteHexWord; -- block
      i : Integer;
   begin
      Write ("Le segment de code courant est    : $");
      WriteHexWord (CSeg);
      Writeln;
      Write ("Le segment de données courant est : $");
      WriteHexWord (DSeg);
      Writeln;
      Write ("Le segment de pile est            : $");
      WriteHexWord (SSeg);
      Writeln;
      Write ("Le pointeur de pile est en        : $");
      WriteHexWord (SPtr);
      Writeln;
      Write ("i est au déplacement $");
      WriteHexWord (Ofs (i'Address));
      Write (" du segment $");
      WriteHexWord (Seg (i'Address));
      Writeln;
   end ExCsegDsegSsegSptrOfsSeg; -- block
   procedure ExDec is
      IntVar     : Integer;
      LongintVar : Longint;
      Ch         : Char;
   begin
      IntVar     := 10;
      LongintVar := 20;
      Ch         := 'b';
      Dec (IntVar);
      Dec (LongintVar, 5);
      Dec (Ch);
      Dec (Ch, 32);
      Writeln (IntVar'Img + LongintVar'Img + Ch);
   end ExDec; -- block
   procedure ExDelayNoSoundSound is
   begin
      Sound (220);
      Delay1 (200);
      NoSound;
   end ExDelayNoSoundSound; -- block
   procedure ExDelete is
      s : P2Ada_String (1 .. 255);
   begin
      Assign_String (s, "Incorruptible Maximilien Robespierre");
      Delete (s, 15, 11);
      Writeln (s);
   end ExDelete; -- block
   procedure ExDelLine is
   begin
      Window (1, 10, 60, 20);
      GotoXY (1, 1);
      TextColor (Crt.Black);
      TextBackground (Crt.White);
      ClrScr;
      Writeln ("Ligne 1");
      Writeln ("Ligne 2");
      Writeln ("Ligne 3");
      Writeln;
      Write ("Frappez <Entrée> pour supprimer la ligne 2");
      Readln;
      GotoXY (1, 2);
      DelLine;
      Writeln;
      Writeln;
      --        ClrEol;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      TextBackground (Crt.Black);
      TextColor (Crt.White);
      Window (1, 1, 80, 25);
      ClrScr;
   end ExDelLine; -- block
   procedure ExDetectGraph is
      GraphPilote, GraphMode : Integer := 0;
   begin
      DetectGraph (GraphPilote, GraphMode);
      if (GraphPilote = EGA) or (GraphPilote = EGA64) then
         GraphPilote := CGA;
         GraphMode   := CGAHi;
      end if;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Line (0, 0, GetMaxX, GetMaxY);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExDetectGraph; -- block
   procedure ExDiskFree is
   begin
      Writeln (DiskFree (0)'Img + " octets disponibles.");
   end ExDiskFree; -- block
   procedure ExDiskSize is
   begin
      Writeln ("Capacité totale : " + DiskSize (0)'Img + " octets.");
   end ExDiskSize; -- block
   procedure ExDisposeNew is
      subtype Str41 is String (1 .. 41 + 1);
      type PStr41 is access Str41;
      p : PStr41;
      procedure Dispose is new Ada.Unchecked_Deallocation (Str41, PStr41);
   begin
      p := new Str41;--  New(p);
      Assign_String (p.all, "Maintenant vous pouvez référencer p ...");
      Writeln (p.all);
      Dispose (p);
      Writeln ("Maintenant vous ne le pouvez plus ...");
   end ExDisposeNew; -- block
   procedure ExDosExitCodeExec is
      NomProgram : PathStr;
      LigneCmd   : ComStr;
   begin
      Write ("Programme à executer (avec chemin d'accès) : ");
      Readln (NomProgram);
      Write ("Ligne de commande pour " + NomProgram + ": ");
      Readln (LigneCmd);
      Writeln ("Lancement d'exécution...");
      SwapVectors;
      Exec (NomProgram, LigneCmd);
      SwapVectors;
      Writeln ("...Retour après exécution");
      if DosError /= 0 then
         Writeln ("Erreur Dos No " + DosError'Img);
      else
         Writeln
           ("Exécution complète. " + "Code de sortie du programme fils = " + DosExitCode'Img);
      end if;
   end ExDosExitCodeExec; -- block
   procedure ExDosVersion is
      Ver : Word;
   begin
      Ver := DosVersion;
      Writeln ("La version DOS utilisée est la " + Lo (Ver)'Img + '.' + Hi (Ver)'Img);
   end ExDosVersion; -- block
   procedure ExDrawPoly is
      type P2Ada_Anonym_2 is array (1 .. 4) of PointType;
      Triangle               : P2Ada_Anonym_2;
      GraphPilote, GraphMode : Integer := 0;
   begin
      Triangle (1).X := 50;
      Triangle (1).Y := 100;
      Triangle (2).X := 100;
      Triangle (2).Y := 100;
      Triangle (3).X := 150;
      Triangle (3).Y := 150;
      Triangle (4).X := 50;
      Triangle (4).Y := 100;
      GraphPilote    := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      DrawPoly (Triangle'Size / PointType'Size, PolygonType (Triangle));
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExDrawPoly; -- block
   procedure ExEllipse is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Ellipse (100, 100, 0, 360, 30, 50);
      Ellipse (100, 100, 0, 180, 50, 30);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExEllipse; -- block
   procedure ExEnvcountEnvstr is
   begin
      for I in 1 .. EnvCount loop
         Writeln (EnvStr (I));
      end loop;
   end ExEnvcountEnvstr; -- block
   procedure ExEofReadWrite is
      f  : Text;
      ch : Char;
   begin
      Assign (f, ParamStr (2));
      Reset (f);
      while not Eof (f) loop
         Read (f, ch);
         Write (+ch);
      end loop;
      Close (f);
      Writeln;
   end ExEofReadWrite; -- block
   procedure ExEoln is
   begin
      Writeln (Boolean'Image (Eoln));
   end ExEoln; -- block
   procedure ExErase is
      f  : File;
      ch : Char;
   begin
      Assign (f, ParamStr (2));
      Reset (f, 1);
      if IOResult /= 0 then
         Writeln (ParamStr (2) + " inexistant");
      else
         Close (f);
         Write ("Effacement de " + ParamStr (2) + " [O,N] ? ");
         Readln (ch);
         if UpCase (ch) = 'O' then
            Erase (f);
         end if;
         Writeln;
      end if;
   end ExErase; -- block
   procedure ExExit is
      procedure Perte_de_Temps is
         Ch : Char;
         pragma Unreferenced (Ch);
      begin
         loop
            if KeyPressed then
               null; --  Exit_k(Perte_de_Temps);
            end if;
            Write ("Xx");
            exit when False;
         end loop;
         Ch := ReadKey;
      end Perte_de_Temps; -- block
   begin
      Perte_de_Temps;
   end ExExit; -- block
   procedure ExExp is
   begin
      Writeln ("e = " + Exp (1.0)'Img);
   end ExExp; -- block
   procedure ExFexpandFsearch is
      S : PathStr;
   begin
      Assign_String (S, FSearch (To_TPString (PathStr'Length - 1, "gnat"), GetEnv ("PATH")));
      if Is_Equal (S, "") then
         Writeln ("Fichier gnat introuvable");
      else
         Writeln ("Fichier gnat trouvé comme " + FExpand (S));
      end if;
   end ExFexpandFsearch; -- block
   procedure ExFileposFilesizeSeek is
      f      : File;
      Taille : Longint;
   begin
      Assign (f, ParamStr (1));
      Reset (f, 1);
      Seek (f, FileSize (f));
      Taille := FilePos (f);
      Writeln ("Taille du fichier en octets : " + Taille'Img + " / " + FileSize (f)'Img);
      Close (f);
   end ExFileposFilesizeSeek; -- block
   procedure ExFillChar is
      s : P2Ada_String (1 .. 20 + 1);
      t : TTabByte (1 .. 10);
   begin
      FillChar (s, s'Size / 8, '@');
      s (20 + 1) := Char'Val (0);
      Writeln (s);
      FillChar (t, t'Size / 8, 3);
      Writeln (t (10)'Img);
   end ExFillChar; -- block
   procedure ExFillEllipse is
      R                      : constant := 30;
      GraphPilote, GraphMode : Integer := 0;
      Xasp, Yasp             : Word;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult < 0 then
         Halt (1);
      end if;
      FillEllipse (GetMaxX / 2, GetMaxY / 2, 50, 150);
      GetAspectRatio (Xasp, Yasp);
      FillEllipse (R, R, R, R * Longint (Xasp) / Yasp);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExFillEllipse; -- block
   procedure ExFillPoly is
      type P2Ada_Anonym_3 is array (1 .. 3) of PointType;
      Triangle               : P2Ada_Anonym_3;
      GraphPilote, GraphMode : Integer := 0;
   begin
      Triangle (1).X := 50;
      Triangle (1).Y := 100;
      Triangle (2).X := 100;
      Triangle (2).Y := 100;
      Triangle (3).X := 150;
      Triangle (3).Y := 150;
      GraphPilote    := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      FillPoly (Triangle'Size / PointType'Size, PolygonType (Triangle));
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExFillPoly; -- block
   procedure ExFindfirstFindnext is
      DirInfo : SearchRec;
   begin
      FindFirst (To_TPString (PathStr'Length - 1, "*.txt"), Archive, DirInfo);
      while DosError = 0 loop
         Writeln (DirInfo.Name);
         FindNext (DirInfo);
      end loop;
   end ExFindfirstFindnext; -- block
   procedure ExFloodFill is
      r                      : constant := 20;
      StartX                 : constant := 100;
      StartY                 : constant := 50;
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Ellipse (50, 50, 0, 360, 20, 30);
      FloodFill (50, 50, GetMaxColor);
      Ellipse (StartX, StartY, 0, 360, r, (r / 3) + 2);
      Ellipse (StartX, StartY - 4, 190 - 10, 357 + 3, r, r / 3);
      Line (StartX + 7, StartY - 6, StartX + 10, StartY - 12);
      Circle (StartX + 10, StartY - 12, 2);
      Line (StartX - 7, StartY - 6, StartX - 10, StartY - 12);
      Circle (StartX - 10, StartY - 12, 2);
      SetFillStyle (SolidFill, GetMaxColor);
      Line (101, 50 + 4 - 20, 101, 50 + 4 + 20);
      Line (101 - 20, 50 + 4, 101 + 20, 50 + 4);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      FloodFill (StartX - 1, StartY - 1, GetColor);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExFloodFill; -- block
   procedure ExFlush is
      procedure IndiqueErreur (s : P2Ada_String) is
      begin
         Flush (Output);
         Writeln (s);
         Halt (1);
      end IndiqueErreur; -- block
   begin
      IndiqueErreur ("Une erreur est survenue.");
   end ExFlush; -- block
   procedure ExFrac is
      r : Real;
   begin
      r := Frac (123.456);
      Write (r'Img);
      r := Frac (-123.456);
      Writeln (r'Img);
   end ExFrac; -- block
   procedure ExFreememGetmemMaxavail is
      type AmiRec is record
         Nom : P2Ada_String (1 .. 30);
         Age : Byte;
      end record;
      p : Pointer;
   begin
      if MaxAvail < AmiRec'Size / 8 then
         Writeln ("Pas assez de mémoire");
      else
         GetMem (p, AmiRec'Size / 8);
         FreeMem (p, AmiRec'Size / 8);
      end if;
   end ExFreememGetmemMaxavail; -- block
   procedure ExFSplit is
      P : PathStr;
      D : DirStr;
      N : NameStr;
      E : ExtStr;
   begin
      Write ("Nom de fichier (défaut = ESSAI.PAS) : ");
      Readln (P);
      FSplit (P, D, N, E);
      if Is_Equal (N, "") then
         Assign_String (N, "ESSAI");
      end if;
      if Is_Equal (E, "") then
         Assign_String (E, ".PAS");
      end if;
      Assign_String (P, Concat (D, Concat (N, E)));
      Writeln ("Spécification résultante " + P);
   end ExFSplit; -- block
   procedure ExGetArcCoords is
      GraphPilote, GraphMode : Integer := 0;
      ArcCoords              : ArcCoordsType;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Arc (100, 100, 0, 270, 30);
      GetArcCoords (ArcCoords);
      declare
         P2Ada_Var_1 : ArcCoordsType renames ArcCoords;  -- !Help! No type
                                                         --found -> add
                                                         --'P2Ada_Var_1.' to
                                                         --fields of ArcCoords;
      begin
         Line (P2Ada_Var_1.XStart, P2Ada_Var_1.YStart, P2Ada_Var_1.XEnd, P2Ada_Var_1.YEnd);
      end; -- declare
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetArcCoords; -- block
   procedure ExGetAspectRatio is
      GraphPilote, GraphMode, TailleX, TailleY : Integer := 0;
      Xasp, Yasp                               : Word;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      GetAspectRatio (Xasp, Yasp);
      TailleX := 20;
      TailleY := Round ((Real (Xasp) / Real (Yasp)) * Real (TailleX));
      Rectangle (0, 0, TailleX, TailleY);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetAspectRatio; -- block
   procedure ExGetBkColor is
      GraphPilote, GraphMode : Integer := 0;
      Couleur                : Word;
      Pal                    : PaletteType;
      Ch                     : Char;
      pragma Unreferenced (Ch);
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Randomize;
      GetPalette (Pal);
      if Pal.Size /= 1 then
         loop
            Couleur := Succ (GetBkColor);
            if Couleur > Pal.Size - 1 then
               Couleur := 0;
            end if;
            SetBkColor (Couleur);
            LineTo (Random (GetMaxX), Random (GetMaxY));
            exit when KeyPressed;
            Delay1 (1000);
         end loop;
         Ch := ReadKey;
      else
         Line (0, 0, GetMaxX, GetMaxY);
      end if;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetBkColor; -- block
   procedure ExGetcbreakSetcbreak is
      type P2Ada_Anonym_4 is array (Boolean) of P2Ada_String (1 .. 7);
      OffOn : P2Ada_Anonym_4;
      cb    : Boolean;
   begin
      OffOn (False) := "actif  ";
      OffOn (True)  := "inactif";
      GetCBreak (cb);
      Writeln ("Le mécanisme d'interruption clavier (Control-Break) est " + OffOn (cb));
      cb := not (cb);
      Writeln ("Maintenant il est " + OffOn (cb));
      SetCBreak (cb);
   end ExGetcbreakSetcbreak; -- block
   procedure ExGetColor is
      GraphPilote, GraphMode : Integer := 0;
      Couleur                : Word;
      Ch                     : Char;
      pragma Unreferenced (Ch);
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Randomize;
      loop
         Couleur := Succ (GetColor);
         if Couleur >= GetMaxColor then
            Couleur := 0;
         end if;
         SetColor (Couleur);
         LineTo (Random (GetMaxX), Random (GetMaxY));
         exit when KeyPressed;
         Delay1 (1000);
      end loop;
      Ch := ReadKey;
      CloseGraph;
   end ExGetColor; -- block
   procedure ExGetDate is
      type P2Ada_Anonym_5 is array (0 .. 6) of P2Ada_String (1 .. 8);
      jours       : P2Ada_Anonym_5;
      a, m, j, js : Word;
   begin
      jours (0) := "Dimanche";
      jours (1) := "Lundi   ";
      jours (2) := "Mardi   ";
      jours (3) := "Mercredi";
      jours (4) := "Jeudi   ";
      jours (5) := "Vendredi";
      jours (6) := "Samedi  ";
      GetDate (a, m, j, js);
      Writeln
        ("Aujourd'hui nous sommes le " + jours (js) + ", " + a'Img + '/' + m'Img + '/' + j'Img);
   end ExGetDate; -- block
   procedure ExGetDefaultPalette is
      GraphPilote, GraphMode : Integer := 0;
      MaPalett, ExPalett     : PaletteType;
      Ch                     : Char;
      pragma Unreferenced (Ch);
   begin
      DirectVideo := False;
      Randomize;
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult < 0 then
         Halt (1);
      end if;
      GetDefaultPalette (ExPalett);
      MaPalett := ExPalett;
      for i in 0 .. MaPalett.Size - 1 loop
         SetColor (i);
         OutTextXY (10, i * 10, "...Frappez une touche...");
      end loop;
      loop
         declare
            P2Ada_Var_2 : PaletteType renames MaPalett;  -- !Help! No type
                                                         --found -> add
                                                         --'P2Ada_Var_2.' to
                                                         --fields of MaPalett;
         begin
            P2Ada_Var_2.Colors (Random (P2Ada_Var_2.Size))  := Random (P2Ada_Var_2.Size + 1);
         end; -- declare
         SetAllPalette (MaPalett);
         exit when KeyPressed;
         Delay1 (1000);
      end loop;
      Ch := ReadKey;
      SetAllPalette (ExPalett);
      ClearDevice;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetDefaultPalette; -- block
   procedure ExGetDir is
      s : P2Ada_String (1 .. 255);
   begin
      GetDir (0, s);
      Writeln ("Répertoire courant : " + s);
   end ExGetDir; -- block
   procedure ExGetDriverName is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult < 0 then
         Halt (1);
      end if;
      OutText (Concat ("GraphPilote actif: ", GetDriverName));
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetDriverName; -- block
   procedure ExGetenvSwapvectors is
      Commande : ComStr;
   begin
      Write ("Une commande SHELL : ");
      Readln (Commande);
      if not Is_Equal (Commande, "") then
         Assign_String (Commande, Concat ("-c ", Commande));
      end if;
      SwapVectors;
      Exec (To_TPString (PathStr'Length - 1, GetEnv ("SHELL")), Commande);
      SwapVectors;
      if DosError /= 0 then
         Writeln ("Impossible d'exécuter SHELL");
      end if;
   end ExGetenvSwapvectors; -- block
   procedure ExGetFAttr is
      f    : File;
      attr : Word1;
   begin
      Assign (f, ParamStr (1));
      GetFAttr (f, attr);
      Writeln (ParamStr (1));
      if DosError /= 0 then
         Writeln ("Code erreur DOS = " + DosError'Img);
      else
         Writeln ("Attribut = " + attr'Img);
         if (attr and ReadOnly) /= 0 then
            Writeln ("Lecture seule");
         end if;
         if (attr and Hidden) /= 0 then
            Write ("Fichier caché");
         end if;
         if (attr and SysFile) /= 0 then
            Write ("Fichier système");
         end if;
         if (attr and VolumeID) /= 0 then
            Writeln ("Identificateur de volume");
         end if;
         if (attr and Directory) /= 0 then
            Writeln ("Répertoire");
         end if;
         if (attr and Archive) /= 0 then
            Writeln ("Archive ( Fichier normal)");
         end if;
      end if;
   end ExGetFAttr; -- block
   procedure ExGetfillpatternSetfillpattern is
      Gris50                 : FillPatternType;
      GraphPilote, GraphMode : Integer := 0;
      AncienMotif            : FillPatternType;
   begin
      Gris50 (1)  := 16#AA#;
      Gris50 (2)  := 16#55#;
      Gris50 (3)  := 16#AA#;
      Gris50 (4)  := 16#55#;
      Gris50 (5)  := 16#AA#;
      Gris50 (6)  := 16#55#;
      Gris50 (7)  := 16#AA#;
      Gris50 (8)  := 16#55#;
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      GetFillPattern (AncienMotif);
      SetFillPattern (Gris50, Graph.White);
      Bar (0, 0, 100, 100);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      SetFillPattern (AncienMotif, Graph.White);
      Bar (0, 0, 100, 100);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetfillpatternSetfillpattern; -- block
   procedure ExGetfillsettingsSetfillstyle is
      GraphPilote, GraphMode : Integer := 0;
      Remplissage            : FillSettingsType;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      GetFillSettings (Remplissage);
      Bar (0, 0, 50, 50);
      SetFillStyle (XHatchFill, GetMaxColor);
      Bar (50, 0, 100, 50);
      declare
         P2Ada_Var_3 : FillSettingsType renames Remplissage;-- !Help! No type
                                                            --found -> add
                                                            --'P2Ada_Var_3.'
                                                            --to fields of
                                                            --Remplissage;
      begin
         SetFillStyle (P2Ada_Var_3.Pattern, P2Ada_Var_3.Color);
      end; -- declare
      Bar (100, 0, 150, 50);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetfillsettingsSetfillstyle; -- block
   procedure ExGetftimePacktimeSetftimeUnpacktime is
      f             : File;
      h, m, s, hund : Word;
      ftime         : Longint;
      dt            : DateTime;
      function LeadingZero (w : Word) return P2Ada_String is
         P2Ada_Result_LeadingZero : P2Ada_String (1 .. 255);
         s                        : P2Ada_String (1 .. 255);
      begin
         Assign_String (s, w'Img);
         if Length (s) = 1 then
            s := Concat (+'0', s);
         end if;
         P2Ada_Result_LeadingZero := s;
         return P2Ada_Result_LeadingZero;
      end LeadingZero; -- block
   begin
      Assign (f, "test.txt");
      GetTime (h, m, s, hund);
      Rewrite (f, 1);
      GetFTime (f, ftime);
      Writeln
        ("Fichier créé à " + LeadingZero (h) + ':' + LeadingZero (m) + ':' + LeadingZero (s));
      UnpackTime (ftime, dt);
      declare
         P2Ada_Var_4 : DateTime renames dt;  -- !Help! No type found -> add
                                             --'P2Ada_Var_4.' to fields of dt;
      begin
         Writeln
           ("La date de valeur du fichier test.txt est " +
            LeadingZero (P2Ada_Var_4.Hour) +
            ':' +
            LeadingZero (P2Ada_Var_4.Min) +
            ':' +
            LeadingZero (P2Ada_Var_4.Sec));
         P2Ada_Var_4.Hour := 0;
         P2Ada_Var_4.Min  := 1;
         P2Ada_Var_4.Sec  := 0;
         PackTime (dt, ftime);
         Writeln ("Configuration de la date de valeur à " + "minuit et une minute");
         Reset (f, 1);
         SetFTime (f, ftime);
      end; -- declare
      Close (f);
   end ExGetftimePacktimeSetftimeUnpacktime; -- block
   procedure ExGetGraphMode is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      OutText ("<Entrée> Pour quitter le mode graphique");
      Readln;
      RestoreCrtMode;
      Writeln ("Maintenant, voici le mode texte");
      Write ("<Entrée> pour passer en mode graphique");
      Readln;
      SetGraphMode (GetGraphMode);
      OutTextXY (0, 0, "Le retour du mode graphique");
      OutTextXY (0, TextHeight (+'H'), "<Entrée> pour quitter");
      Readln;
      CloseGraph;
   end ExGetGraphMode; -- block
   procedure ExGetimageImagesizePutimage is
      GraphPilote, GraphMode : Integer := 0;
      P                      : Pointer;
      Size                   : Word;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      SetBkColor (Graph.Black);
      SetFillStyle (1, Graph.Red);
      Bar (0, 0, 100, 100);
      SetUserCharSize (2, 1, 1, 1);
      SetColor (Graph.Red);
      OutTextXY (0, 0, "ROUGE");
      SetColor (Graph.Cyan);
      OutTextXY (0, 50, "CYAN");
      SetColor (Graph.Green);
      Line (10, 25, 150, 25);
      OutTextXY (0, 100, "VERT");
      SetColor (Graph.Yellow);
      OutTextXY (0, 150, "JAUNE");
      SetColor (Graph.White);
      Size := ImageSize (10, 20, 142, 52);
      GetMem (P, Size);
      GetImage (10, 20, 142, 52, P);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      SetBkColor (Graph.LightGreen);
      PutImage (100, 100, P, CopyPut);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      PutImage (300, 100, P, XORPut);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      SetBkColor (Graph.White);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      PutImage (300, 100, P, XORPut);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetimageImagesizePutimage; -- block
   IndicBreak : Boolean;
   procedure GestionBreak is
   begin
      IndicBreak := True;
   end GestionBreak; -- block
   procedure ExGetintvectSetintvect is
      Int1BSauve : Pointer;
   begin
      IndicBreak := False;
      GetIntVec (16#1B#, Int1BSauve);
      SetIntVec (16#1B#, GestionBreak'Address);
      Write ("Frappez Ctrl-Break pour quitter");
      loop
         Delay1 (1000);
         exit when IndicBreak;
         exit when KeyPressed;
      end loop;
      SetIntVec (16#1B#, Int1BSauve);
   end ExGetintvectSetintvect; -- block
   procedure ExGetLineSettings is
      GraphPilote, GraphMode : Integer := 0;
      AncienStyle            : LineSettingsType;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Line (0, 0, 100, 0);
      GetLineSettings (AncienStyle);
      SetLineStyle (DottedLn, 0, ThickWidth);
      Line (0, 10, 100, 10);
      declare
         P2Ada_Var_5 : LineSettingsType renames AncienStyle;-- !Help! No type
                                                            --found -> add
                                                            --'P2Ada_Var_5.'
                                                            --to fields of
                                                            --AncienStyle;
      begin
         SetLineStyle (P2Ada_Var_5.LineStyle, P2Ada_Var_5.Pattern, P2Ada_Var_5.Thickness);
      end; -- declare
      Line (0, 20, 100, 20);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetLineSettings; -- block
   procedure ExGetmaxcolorSetcolor is
      GraphPilote, GraphMode : Integer := 0;
      Ch                     : Char;
      pragma Unreferenced (Ch);
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Randomize;
      loop
         SetColor (Random (GetMaxColor) + 1);
         LineTo (Random (GetMaxX), Random (GetMaxY));
         exit when KeyPressed;
         Delay1 (1000);
      end loop;
      Ch := ReadKey;
      CloseGraph;
   end ExGetmaxcolorSetcolor; -- block
   procedure ExGetmaxmodeGetmodename is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult < 0 then
         Halt (1);
      end if;
      for i in 0 .. GetMaxMode loop
         OutTextXY (10, 10 * Succ (i), GetModeName (i));
      end loop;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetmaxmodeGetmodename; -- block
   procedure ExGetmaxxGetmaxy is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Rectangle (0, 0, GetMaxX, GetMaxY);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetmaxxGetmaxy; -- block
   procedure ExGetmoderangeSetgraphmode is
      GraphPilote : Integer;
      GraphMode   : Integer := 0;
      ModeInf     : Integer;
      ModeSup     : Integer;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      GetModeRange (GraphPilote, ModeInf, ModeSup);
      SetGraphMode (ModeInf);
      Line (0, 0, GetMaxX, GetMaxY);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetmoderangeSetgraphmode; -- block
   procedure ExGetPalette is
      GraphPilote, GraphMode : Integer := 0;
      Palette                : PaletteType;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      GetPalette (Palette);
      if Palette.Size /= 1 then
         for Couleur in 0 .. Pred (Palette.Size) loop
            SetColor (Couleur);
            Line (0, Couleur * 5, 100, Couleur * 5);
         end loop;
      else
         Line (0, 0, 100, 0);
      end if;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetPalette; -- block
   procedure ExGetPaletteSize is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult < 0 then
         Halt (1);
      end if;
      SetColor (1);
      OutText ("Couleur = 1, ");
      SetColor (GetPaletteSize - 1);
      OutText ("Couleur = Max");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetPaletteSize; -- block
   procedure ExGetPixel is
      GraphPilote, GraphMode : Integer := 0;
      PixelColor             : Word;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      PixelColor := GetPixel (10, 10);
      if PixelColor = 0 then
         PutPixel (10, 10, GetMaxColor);
      end if;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetPixel; -- block
   procedure ExGetTextSettings is
      GraphPilote, GraphMode : Integer := 0;
      AncienStyle            : TextSettingsType;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      GetTextSettings (AncienStyle);
      OutTextXY (0, 0, "Ancien style texte");
      SetTextJustify (LeftText, CenterText);
      SetTextStyle (TriplexFont, VertDir, 4);
      OutTextXY (GetMaxX / 2, GetMaxY / 2, "Nouveau Style");
      declare
         P2Ada_Var_6 : TextSettingsType renames AncienStyle;-- !Help! No type
                                                            --found -> add
                                                            --'P2Ada_Var_6.'
                                                            --to fields of
                                                            --AncienStyle;
      begin
         SetTextJustify (P2Ada_Var_6.Horiz, P2Ada_Var_6.Vert);
         SetTextStyle (P2Ada_Var_6.Font, P2Ada_Var_6.Direction, P2Ada_Var_6.CharSize);
      end; -- declare
      OutTextXY (0, TextHeight (+'H'), "Retour de l'ancien style");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetTextSettings; -- block
   procedure ExGetTime is
      h, m, s, hund : Word;
      function LeadingZero (w : Word) return P2Ada_String is
         s : P2Ada_String (1 .. 255);
      begin
         Assign_String (s, w'Img);
         if Length (s) = 1 then
            s := Concat (+'0', s);
         end if;
         return s;
      end LeadingZero; -- block
   begin
      GetTime (h, m, s, hund);
      Writeln
        ("Il est " +
         LeadingZero (h) +
         ':' +
         LeadingZero (m) +
         ':' +
         LeadingZero (s) +
         '.' +
         LeadingZero (hund) +
         " et tout va bien.");
   end ExGetTime; -- block
   procedure ExGetVerifySetverify is
      type P2Ada_Anonym_6 is array (Boolean) of P2Ada_String (1 .. 8);
      OffOn : P2Ada_Anonym_6;
      v     : Boolean;
   begin
      OffOn (False) := "inactive";
      OffOn (True)  := "active  ";
      GetVerify (v);
      Writeln ("La vérification d'écriture est : " + OffOn (v));
      v := not (v);
      Writeln ("Vérification d'écriture maintenant " + OffOn (v));
      SetVerify (v);
   end ExGetVerifySetverify; -- block
   procedure ExGetViewSettings is
      GraphPilote, GraphMode : Integer := 0;
      Fenetre                : ViewPortType;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      GetViewSettings (Fenetre);
      declare
         P2Ada_Var_7 : ViewPortType renames Fenetre;  -- !Help! No type found
                                                      ---> add 'P2Ada_Var_7.'
                                                      --to fields of Fenetre;
      begin
         Rectangle (0, 0, P2Ada_Var_7.X2 - P2Ada_Var_7.X1, P2Ada_Var_7.Y2 - P2Ada_Var_7.Y1);
         if P2Ada_Var_7.Clip then
            OutText ("Limitation d'affichage active.");
         else
            OutText ("Pas de limitation aujourd'hui.");
         end if;
      end; -- declare
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetViewSettings; -- block
   procedure ExGetxGety is
      GraphPilote, GraphMode, X, Y : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      OutText ("Début. ");
      X := GetX;
      Y := GetY;
      OutTextXY (20, 10, "Maintenant nous sommes ici...");
      OutTextXY (X, Y, "Revenons à nos photons.");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGetxGety; -- block
   procedure ExGotoXY is
   begin
      Window (1, 10, 60, 20);
      GotoXY (1, 1);
      TextColor (Crt.Black);
      TextBackground (Crt.White);
      ClrScr;
      Writeln (Concat ("* <-Voici le coin supérieur gauche ", "de la fenêtre."));
      Writeln;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      TextBackground (Crt.Black);
      TextColor (Crt.White);
      Window (1, 1, 80, 25);
      ClrScr;
   end ExGotoXY; -- block
   procedure ExGraphDefaults is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      SetColor (1);
      OutText ("Ce texte est en couleur #1");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      ClearViewPort;
      GraphDefaults;
      OutText ("Voici un texte en couleur standard");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGraphDefaults; -- block
   procedure ExGraphErrorMsg is
      GraphPilote, GraphMode, CodeErreur : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      CodeErreur := GraphResult;
      if CodeErreur /= grOk then
         Writeln ("Erreur graphique: " + GraphErrorMsg (CodeErreur));
         Write ("Frappez <Entrée> pour continuer");
         Readln;
         Halt (1);
      end if;
      Line (0, 0, GetMaxX, GetMaxY);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGraphErrorMsg; -- block
   procedure ExGraphResult is
      CodeErreur             : Integer;
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      CodeErreur := GraphResult;
      if CodeErreur /= grOk then
         Writeln ("Erreur Graphique :");
         Writeln (GraphErrorMsg (CodeErreur));
         Writeln ("Interruption immédiate du programme...");
         Halt (1);
      end if;
      ClearDevice;
      Rectangle (0, 0, GetMaxX, GetMaxY);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExGraphResult; -- block
   procedure ExHalt is
   begin
      Halt;
      Writeln ("Ceci ne pourra pas être exécuté");
      Halt (2);
      Writeln ("Ceci n'ont plus");
   end ExHalt; -- block
   procedure ExHi is
      w : Word;
   begin
      w := Hi (16#1234#);
      Writeln (w'Img);
   end ExHi; -- block
   procedure ExHighVideo is
   begin
      TextAttr := Crt.LightGray;
      HighVideo;
      Writeln ("HighVideo");
   end ExHighVideo; -- block
   procedure ExInc is
      IntVar     : Integer;
      LongintVar : Longint;
      Ch         : Char;
   begin
      IntVar     := 2;
      LongintVar := 8;
      Ch         := '@';
      Inc (IntVar);
      Inc (LongintVar, 5);
      Inc (Ch);
      Inc (Ch, 32);
      Writeln (IntVar'Img + LongintVar'Img + Ch);
   end ExInc; -- block
   procedure ExInitGraph is
      GraphPilote : Integer;
      GraphMode   : Integer := 0;
      CodeErreur  : Integer;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      CodeErreur := GraphResult;
      if CodeErreur = grOk then
         Line (0, 0, GetMaxX, GetMaxY);
         Write ("Frappez <Entrée> pour continuer");
         Readln;
         CloseGraph;
      else
         Writeln ("Erreur graphique:" + GraphErrorMsg (CodeErreur));
      end if;
   end ExInitGraph; -- block
   procedure ExInsert is
      s : P2Ada_String (1 .. 255);
   begin
      Assign_String (s, "Incorruptible Robespierre");
      Insert ("Maximilien ", s, 15);
      Writeln (s);
   end ExInsert; -- block
   procedure ExInsLine is
   begin
      Window (1, 10, 60, 20);
      GotoXY (1, 1);
      TextColor (Crt.Black);
      TextBackground (Crt.White);
      ClrScr;
      Writeln ("Ligne 1");
      Writeln ("Ligne 2");
      Writeln;
      Write ("Frappez <Entrée> pour insérer une ligne");
      Readln;
      GotoXY (1, 2);
      InsLine;
      Writeln ("Ligne insérée");
      Writeln;
      Writeln;
      --        ClrEol;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      TextBackground (Crt.Black);
      TextColor (Crt.White);
      Window (1, 1, 80, 25);
      ClrScr;
   end ExInsLine; -- block
   function TestDetect return Integer is
      P2Ada_Result_TestDetect : Integer;
   begin
      P2Ada_Result_TestDetect := 1;
      return P2Ada_Result_TestDetect;
   end TestDetect; -- block
   procedure ExInstallUserDriver is
      GraphPilote, GraphMode, TestPilote, CodeErreur : Integer := 0;
   begin
      TestPilote := InstallUserDriver ("TEST", TestDetect'Address);
      if GraphResult /= grOk then
         Writeln ("Erreur d'intallation pour TestPilote" + TestPilote'Img);
         Halt (1);
      end if;
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      CodeErreur := GraphResult;
      if CodeErreur /= grOk then
         Writeln ("Erreur pendant l'initialisation : " + CodeErreur'Img);
         Halt (1);
      end if;
      OutText ("Pilotes installables supportés.");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExInstallUserDriver; -- block
   procedure ExInstallUserFont is
      GraphPilote, GraphMode : Integer := 0;
      TestPolice             : Integer;
   begin
      TestPolice := InstallUserFont ("TEST");
      if GraphResult /= grOk then
         Writeln
           (Concat
               ("Erreur d'installation de TestPolice",
                "(Utilisation de la police par défaut)"));
         Write ("Frappez <Entrée> pour continuer");
         Readln;
      end if;
      GraphPilote := Detect;
      GraphMode   := 0;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      SetTextStyle (TestPolice, HorizDir, 2);
      OutText ("Polices installables supportées.");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExInstallUserFont; -- block
   procedure ExInt is
      r : Real;
   begin
      r := Int (123.456);
      Write (r'Img);
      r := Int (-123.456);
      Writeln (r'Img);
   end ExInt; -- block
   procedure ExIntr is
      msg  : P2Ada_String (1 .. 255);
      regs : Registers;
   begin
      Assign_String (msg, "Bonjour, monde!$");
      regs.AX := 9;
      regs.DS := Seg (msg'Address);
      regs.DX := Ofs (msg'Address);
      Intr (16#21#, regs);
   end ExIntr; -- block
   procedure ExIOResult is
      f : File;
   begin
      Assign (f, ParamStr (1));
      Reset (f, 1);
      if IOResult = 0 then
         Writeln ("Taille du fichier en octets : " + FileSize (f)'Img);
      else
         Write ("Fichier inexistant");
      end if;
   end ExIOResult; -- block
   procedure KbdIntVec is
   begin
      null;
   end KbdIntVec; -- block
   procedure Keyclick is
   begin
      --        if Port (16#60#) < 16#80# then
      Sound (5000);
      Delay1 (1);
      NoSound;
      --        end if;
      KbdIntVec;
   end Keyclick; -- block
   procedure ExKeep is
      KbdIntVecPtr : Pointer;
   begin
      GetIntVec (16#9#, KbdIntVecPtr);
      SetIntVec (16#9#, Keyclick'Address);
      Keep (0);
   end ExKeep; -- block
   procedure ExKeyPressed is
      Ch : Char;
      pragma Unreferenced (Ch);
   begin
      loop
         Writeln ("Xx");
         Delay1 (1000);
         exit when KeyPressed;
      end loop;
      Ch := ReadKey;
      Writeln;
   end ExKeyPressed; -- block
   procedure ExLenght is
      f : Text;
      s : P2Ada_String (1 .. 255);
   begin
      Assign (f, "test.txt");
      Reset (f);
      Readln (f, s);
      Close (f);
      Writeln ('"' + s + '"');
      Writeln ("longueur = " + Length (s)'Img);
   end ExLenght; -- block
   procedure ExLine is
      GraphPilote, GraphMode : Integer := 0;
      Ch                     : Char;
      pragma Unreferenced (Ch);
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Randomize;
      loop
         Line (Random (200), Random (200), Random (200), Random (200));
         exit when KeyPressed;
         Delay1 (1000);
      end loop;
      Ch := ReadKey;
      CloseGraph;
   end ExLine; -- block
   procedure ExLineRel is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      MoveTo (1, 2);
      LineRel (100, 100);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExLineRel; -- block
   procedure ExLineTo is
      GraphPilote, GraphMode : Integer := 0;
      Ch                     : Char;
      pragma Unreferenced (Ch);
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Randomize;
      loop
         LineTo (Random (200), Random (200));
         exit when KeyPressed;
         Delay1 (1000);
      end loop;
      Ch := ReadKey;
      CloseGraph;
   end ExLineTo; -- block
   procedure ExLn is
      e : Real;
   begin
      e := Exp (1.0);
      Writeln ("ln(e) = " + Ln (e)'Img);
   end ExLn; -- block
   procedure ExLo is
      w : Word;
   begin
      w := Lo (16#1234#);
      Writeln (w'Img);
   end ExLo; -- block
   procedure ExLowVideo is
   begin
      TextAttr := Crt.White;
      LowVideo;
      Writeln ("LowVideo");
   end ExLowVideo; -- block
   procedure ExMarkRelease is
      p          : Pointer;
      p1, p2, p3 : access Integer;
      pragma Unreferenced (p3, p2, p1);
   begin
      p1 := new Integer;--  New_k(p1);
      Mark (p);
      p2 := new Integer;--  New_k(p2);
      p3 := new Integer;--  New_k(p3);
      Release (p);
      Writeln ("p2 et p3 ne sont plus accessibles");
   end ExMarkRelease; -- block
   procedure ExMemavailMaxavail is
   begin
      Writeln (MemAvail'Img + " Octets libres");
      Writeln ("Le plus grand bloc : " + MaxAvail'Img + " octets");
   end ExMemavailMaxavail; -- block
   procedure ExMkdir is
   begin
      MkDir (ParamStr (5));
      if IOResult /= 0 then
         Writeln ("Impossible de créer le répertoire");
      else
         Writeln ("Le répertoire est créé");
      end if;
   end ExMkdir; -- block
   procedure ExMove is
      type P2Ada_Anonym_7 is array (1 .. 4) of Char;
      a : P2Ada_Anonym_7;
      b : Longint;
   begin
      Move (a'Address, b'Address, a'Size / 8);
   end ExMove; -- block
   procedure ExMoveRel is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      MoveTo (1, 2);
      MoveRel (10, 10);
      PutPixel (GetX, GetY, GetMaxColor);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExMoveRel; -- block
   procedure ExMoveTo is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      MoveTo (0, 0);
      LineTo (GetMaxX, GetMaxY);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExMoveTo; -- block
   procedure ExMsdos is
      msg  : P2Ada_String (1 .. 255);
      regs : Registers;
   begin
      Assign_String (msg, "Bonjour, monde!$");
      regs.AH := 9;
      regs.DS := Seg (msg'Address);
      regs.DX := Ofs (msg (1)'Address);
      MsDos (regs);
   end ExMsdos; -- block
   procedure ExNormvideoTextbackgroundTextcolor is
   begin
      TextColor (Crt.Green);
      TextBackground (Crt.Black);
      Writeln ("Noir c'est noir !");
      TextColor (Crt.LightRed + Blink);
      TextBackground (Crt.LightGray);
      Writeln ("Je vois rouge !");
      TextColor (14);
      TextBackground (Crt.Blue);
      Writeln ("Le grand ? bleu !");
      NormVideo;
      Writeln ("Retour à la normale...");
   end ExNormvideoTextbackgroundTextcolor; -- block
   procedure ExOdd is
   begin
      if Odd (5) then
         Writeln ("5 est impair");
      else
         Writeln ("Quelque chose étrange");
      end if;
   end ExOdd; -- block
   procedure ExOrd is
      type Couleurs is (ROUGE, BLEU, VERT);
   begin
      Writeln ("BLEU à une valeur ordinale de " + Couleurs'Pos (BLEU)'Img);
      Writeln ("Le code ASCII de 'c' est " + Ord ('c')'Img + " décimal");
   end ExOrd; -- block
   procedure ExOutText is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      OutText ("Utilisation aisée");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExOutText; -- block
   procedure ExOutTextXY is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      MoveTo (0, 0);
      OutText ("Inefficace");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      OutTextXY (GetX, GetY, "Egalement inefficace");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      ClearDevice;
      OutTextXY (0, 0, "A Préférer !");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExOutTextXY; -- block
   procedure ExParamCount is
   begin
      if ParamCount < 1 then
         Writeln ("Aucun paramètre sur la ligne de commande");
      else
         Writeln (ParamCount'Img + " paramètre(s)");
      end if;
   end ExParamCount; -- block
   procedure ExParamStr is
   begin
      for i in 1 .. ParamCount loop
         Writeln (ParamStr (i));
      end loop;
      Writeln;
   end ExParamStr; -- block
   procedure ExPi is
   begin
      Writeln ("Pi = " + Pi'Img);
   end ExPi; -- block
   procedure ExPieSlice is
      Rayon                  : constant := 30;
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      PieSlice (100, 100, 0, 270, Rayon);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExPieSlice; -- block
   procedure ExPos is
      s : P2Ada_String (1 .. 255);
   begin
      Assign_String (s, "   123.5");
      while Pos (+' ', s) > 0 loop
         s (Pos (+' ', s))    := '0';
      end loop;
      Writeln (s);
   end ExPos; -- block
   procedure ExPredSucc is
      type Couleurs is (ROUGE, BLEU, VERT);
   begin
      Writeln ("Le prédécesseur de 5 est " + Pred (5)'Img);
      Writeln ("Le successeur de 10 est " + Succ (10)'Img);
      Writeln
        ("Dans le type Couleurs, " +
         Couleurs'Succ (BLEU)'Img +
         " est " +
         "le successeur de BLEU.");
      Writeln
        ("Dans le type Couleurs, " + Couleurs'Pred (BLEU)'Img + " est le prédécesseur de BLEU.");
      Writeln ("Le prédécesseur de A est " + Pred ('A'));
      Writeln ("Le successeur de A est " + Succ ('A'));
   end ExPredSucc; -- block
   procedure ExPtr is
      p : Pointer;
   begin
      p := Ptr (16#40#, 16#49#);
      Writeln ("Le mode vidéo courant est " + Pointer_Image (p));
   end ExPtr; -- block
   procedure ExPutPixel is
      GraphPilote, GraphMode : Integer := 0;
      Couleur                : Word;
      Ch                     : Char;
      pragma Unreferenced (Ch);
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Couleur := GetMaxColor;
      Randomize;
      loop
         PutPixel (Random (100), Random (100), Couleur);
         exit when KeyPressed;
         Delay1 (1000);
      end loop;
      Ch := ReadKey;
      CloseGraph;
   end ExPutPixel; -- block
   procedure ExRandomRandomize is
      Ch : Char;
      pragma Unreferenced (Ch);
   begin
      Randomize;
      Write ("Press a key...");
      loop
         TextAttr := Byte1 (Random (256));
         Write (+'!');
         exit when KeyPressed;
         Delay1 (1000);
      end loop;
      Ch := ReadKey;
      NormVideo;
      Writeln;
      Writeln (Real'Image (Random));
   end ExRandomRandomize; -- block
   procedure ExReadKey is
      Ch : Char;
   begin
      Write ("Frappez une touche");
      Ch := ReadKey;
      if Ch = Chr (0) then
         Ch := ReadKey;
         Writeln ("Touche spéciale " + Ord (Ch)'Img);
      else
         Writeln ("Touche de code ASCII " + Ord (Ch)'Img);
      end if;
   end ExReadKey; -- block
   procedure ExReadlnWriteln is
      s : P2Ada_String (1 .. 255);
   begin
      Write ("Saisissez une ligne de texte : ");
      Readln (s);
      Writeln ("Vous avez écrit : " + s);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
   end ExReadlnWriteln; -- block
   procedure ExRectangle is
      GraphPilote, GraphMode : Integer := 0;
      x1, y1, x2, y2         : Integer;
      Ch                     : Char;
      pragma Unreferenced (Ch);
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Randomize;
      loop
         x1 := Random (GetMaxX);
         y1 := Random (GetMaxY);
         x2 := Random (GetMaxX - x1) + x1;
         y2 := Random (GetMaxY - y1) + y1;
         Rectangle (x1, y1, x2, y2);
         exit when KeyPressed;
         Delay1 (1000);
      end loop;
      Ch := ReadKey;
      CloseGraph;
   end ExRectangle; -- block
   procedure ExRegisterBGIDriver is
      GraphPilote, GraphMode : Integer := 0;
      PiloteF                : File;
      PiloteP                : Pointer;
   begin
      Assign (PiloteF, "CGA.BGI");
      Reset (PiloteF, 1);
      GetMem (PiloteP, FileSize (PiloteF));
      BlockRead (PiloteF, PiloteP, FileSize (PiloteF));
      if RegisterBGIdriver (PiloteP) < 0 then
         Writeln ("Erreur lors de l'enregistrement du pilote : " + GraphErrorMsg (GraphResult));
         Halt (1);
      end if;
      GraphPilote := CGA;
      GraphMode   := CGAHi;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult < 0 then
         Halt (1);
      end if;
      OutText ("Pilote chargé par le programme utilisateur");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExRegisterBGIDriver; -- block
   procedure ExRegisterBGIFont is
      GraphPilote, GraphMode : Integer := 0;
      PoliceF                : File;
      PoliceP                : Pointer;
   begin
      Assign (PoliceF, "TRIP.CHR");
      Reset (PoliceF, 1);
      GetMem (PoliceP, FileSize (PoliceF));
      BlockRead (PoliceF, PoliceP, FileSize (PoliceF));
      if RegisterBGIfont (PoliceP) < 0 then
         Writeln ("Erreur d'enregistrement de police: " + GraphErrorMsg (GraphResult));
         Halt (1);
      end if;
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "..\");
      if GraphResult < 0 then
         Halt (1);
      end if;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      SetTextStyle (TriplexFont, HorizDir, 4);
      OutText ("Triplex enregistrée par le programme");
      MoveTo (0, TextHeight (+'a'));
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      SetTextStyle (SansSerifFont, HorizDir, 4);
      OutText ("On accède au disque pour SansSerif...");
      MoveTo (0, GetY + TextHeight (+'a'));
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      SetTextStyle (TriplexFont, HorizDir, 4);
      OutText ("On revient en Triplex");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExRegisterBGIFont; -- block
   procedure ExRename is
      f : File;
   begin
      if ParamCount /= 2 then
         Writeln ("Nombre de paramètres erroné");
         Halt (1);
      end if;
      Assign (f, ParamStr (2));
      Writeln ("Renomme " + ParamStr (2) + " en ren-" + ParamStr (2));
      Rename (f, "ren-" + ParamStr (2));
   end ExRename; -- block
   procedure ExReset is
      function FichierExiste (NomFichier : P2Ada_String) return Boolean is
         P2Ada_Result_FichierExiste : Boolean;
         f                          : File;
      begin
         Assign (f, NomFichier);
         Reset (f, 1);
         Close (f);
         P2Ada_Result_FichierExiste := (IOResult = 0) and (not Is_Equal (NomFichier, ""));
         return P2Ada_Result_FichierExiste;
      end FichierExiste; -- block
   begin
      if FichierExiste (ParamStr (1)) then
         Writeln ("Ce fichier existe");
      else
         Write ("Fichier inexistant");
      end if;
   end ExReset; -- block
   procedure ExRestoreCRTMode is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      OutText ("<Entrée> pour quitter le mode graphique");
      Readln;
      RestoreCrtMode;
      Writeln ("Maintenant nous voici en mode texte");
      Write ("<Entrée> pour revenir en mode graphique");
      Readln;
      SetGraphMode (GetGraphMode);
      OutTextXY (0, 0, "Le retour du mode graphique");
      OutTextXY (0, TextHeight (+'H'), "<Entrée> pour quitter");
      Readln;
      CloseGraph;
   end ExRestoreCRTMode; -- block
   procedure ExRewrite is
      f : Text;
   begin
      Assign (f, "test.txt");
      Rewrite (f);
      Writeln (f, "Ceci est la première ligne du" + " nouveau fichier...");
      Close (f);
   end ExRewrite; -- block
   procedure ExRmdir is
   begin
      RmDir (ParamStr (5));
      if IOResult /= 0 then
         Writeln ("Suppression du répertoire impossible");
      else
         Writeln ("Répertoire effacé");
      end if;
   end ExRmdir; -- block
   procedure ExRound is
   begin
      Writeln (Real'Image (1.4) + " arrondi : " + Round (1.4)'Img);
      Writeln (Real'Image (1.5) + " arrond  : " + Round (1.5)'Img);
      Writeln (Real'Image (-1.4) + " arrondi : " + Round (-1.4)'Img);
      Writeln (Real'Image (-1.5) + " arrondi : " + Round (-1.5)'Img);
   end ExRound; -- block
   procedure ExRunError is
   begin
      RunError (1);
      Writeln ("Ceci ne pourra pas être exécuté");
      RunError;
      Writeln ("Ceci non plus");
   end ExRunError; -- block
   procedure ExSector is
      R                      : constant := 50;
      GraphPilote, GraphMode : Integer := 0;
      Xasp, Yasp             : Word;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult < 0 then
         Halt (1);
      end if;
      Sector (GetMaxX / 2, GetMaxY / 2 - R, 0, 45, R, 2 * R);
      GetAspectRatio (Xasp, Yasp);
      Sector (GetMaxX / 2, GetMaxY / 2 + R, 180, 135, 2 * R, R * Longint (Xasp) / Yasp);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExSector; -- block
   procedure ExSeekeofSeekeoln is
      f : Text;
      j : Integer;
   begin
      Assign (f, "test.txt");
      Rewrite (f);
      Writeln (f, "1 2 3 4 ");
      Writeln (f, "5 6 7 8 ");
      Reset (f);
      while not SeekEof (f) loop
         if SeekEoln (f) then
            Readln (f);
         end if;
         Read (f, j);
         Writeln (j'Img);
      end loop;
      Close (f);
   end ExSeekeofSeekeoln; -- block
   procedure ExSetactivatepageSetvisualpage is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      if (GraphPilote = HercMono) or
         (GraphPilote = EGA) or
         (GraphPilote = EGA64) or
         (GraphPilote = VGA)
      then
         SetVisualPage (0);
         SetActivePage (1);
         Rectangle (30, 40, 60, 80);
         SetVisualPage (1);
      else
         OutText ("Pagination non supportée.");
      end if;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExSetactivatepageSetvisualpage; -- block
   procedure ExSetAllPalette is
      GraphPilote, GraphMode : Integer := 0;
      Palette                : PaletteType;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Line (0, 0, GetMaxX, GetMaxY);
      declare
         P2Ada_Var_8 : PaletteType renames Palette;-- !Help! No type found ->
                                                   --add 'P2Ada_Var_8.' to
                                                   --fields of Palette;
      begin
         P2Ada_Var_8.Size       := 4;
         P2Ada_Var_8.Colors (0) := 5;
         P2Ada_Var_8.Colors (1) := 3;
         P2Ada_Var_8.Colors (2) := 1;
         P2Ada_Var_8.Colors (3) := 2;
         SetAllPalette (Palette);
      end; -- declare
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExSetAllPalette; -- block
   procedure ExSetAspectRatio is
      R                      : constant := 50;
      GraphPilote, GraphMode : Integer := 0;
      Xasp, Yasp             : Word;
      Ch                     : Char;
      pragma Unreferenced (Ch);
   begin
      DirectVideo := False;
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult < 0 then
         Halt (1);
      end if;
      GetAspectRatio (Xasp, Yasp);
      if Xasp = Yasp then
         Yasp := 5 * Xasp;
      end if;
      while (Xasp < Yasp) and not KeyPressed loop
         SetAspectRatio (Xasp, Yasp);
         Circle (GetMaxX / 2, GetMaxY / 2, R);
         Inc (Xasp, 20);
         Delay1 (1000);
      end loop;
      Ch := ReadKey;
      SetTextJustify (CenterText, CenterText);
      OutTextXY (GetMaxX / 2, GetMaxY / 2, "Fini!");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExSetAspectRatio; -- block
   procedure ExSetBkColor is
      GraphPilote, GraphMode : Integer := 0;
      Palette                : PaletteType;
      Ch                     : Char;
      pragma Unreferenced (Ch);
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      Randomize;
      if GraphResult /= grOk then
         Halt (1);
      end if;
      GetPalette (Palette);
      loop
         if Palette.Size /= 1 then
            SetBkColor (Random (Palette.Size));
         end if;
         SetColor (Random (Palette.Size));
         LineTo (Random (GetMaxX), Random (GetMaxY));
         exit when KeyPressed;
         Delay1 (1000);
      end loop;
      Ch := ReadKey;
      CloseGraph;
   end ExSetBkColor; -- block
   procedure ExSetDate is
   begin
      SetDate (1990, 11, 6);
   end ExSetDate; -- block
   procedure ExSetFAttr is
      readonly  : constant := 16#01#;
      Hidden    : constant := 16#02#;
      SysFile   : constant := 16#04#;
      VolumeID  : constant := 16#08#;
      Directory : constant := 16#10#;
      Archive   : constant := 16#20#;
      f         : File;
   begin
      Assign (f, "lisez-moi.txt");
      SetFAttr (f, Hidden);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      SetFAttr (f, Archive);
   end ExSetFAttr; -- block
   procedure ExSetGraphBufSize is
      type P2Ada_Anonym_8 is array (1 .. 2) of PointType;
      BigPoly                : P2Ada_Anonym_8;
      GraphPilote, GraphMode : Integer := 0;
   begin
      BigPoly (1).X := 50;
      BigPoly (1).Y := 100;
      BigPoly (2).X := 100;
      BigPoly (2).Y := 100;
      SetGraphBufSize (8 * 1024);
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      FillPoly (BigPoly'Size / PointType'Size, PolygonType (BigPoly));
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExSetGraphBufSize; -- block
   procedure ExSetLineStyle is
      GraphPilote, GraphMode : Integer := 0;
      x1, y1, x2, y2         : Integer;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      x1 := 10;
      y1 := 10;
      x2 := 200;
      y2 := 150;
      SetLineStyle (DottedLn, 0, NormWidth);
      Rectangle (x1, y1, x2, y2);
      SetLineStyle (CenterLn, 0, ThickWidth);
      Rectangle (Pred (x1) + 5, Pred (y1) + 5, Succ (x2) - 5, Succ (y2) - 5);
      SetLineStyle (DashedLn, 0, NormWidth);
      Rectangle (Pred (x1) + 10, Pred (y1) + 10, Succ (x2) - 10, Succ (y2) - 10);
      --        SetLineStyle (UserBitLn, 16#C3#, ThickWidth);
      SetLineStyle (UserBitLn, 16#0003#, ThickWidth);
      Rectangle (Pred (x1) + 15, Pred (y1) + 15, Succ (x2) - 15, Succ (y2) - 15);
      SetLineStyle (UserBitLn, 16#C000#, NormWidth);
      Rectangle (Pred (x1) + 20, Pred (y1) + 20, Succ (x2) - 20, Succ (y2) - 20);
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExSetLineStyle; -- block
   procedure ExSetPalette is
      GraphPilote, GraphMode : Integer := 0;
      Palette                : PaletteType;
      Ch                     : Char;
      pragma Unreferenced (Ch);
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      GetPalette (Palette);
      if Palette.Size /= 1 then
         for Couleur in 0 .. Pred (Palette.Size) loop
            SetColor (Couleur);
            Line (0, Couleur * 5, 100, Couleur * 5);
         end loop;
         Randomize;
         loop
            SetPalette (Random (Palette.Size), Random (Palette.Size));
            exit when KeyPressed;
            Delay1 (1000);
         end loop;
         Ch := ReadKey;
      else
         Line (0, 0, 100, 0);
      end if;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExSetPalette; -- block
   procedure ExSetRGBPalette is
      type RVBRec is record
         RougeVal, VertVal, BleuVal : Integer;
      end record;
      type P2Ada_Anonym_9 is array (0 .. MaxColors) of RVBRec;
      EGAColors              : P2Ada_Anonym_9;
      GraphPilote, GraphMode : Integer := 0;
   begin
      EGAColors (0).RougeVal  := 16#00#;
      EGAColors (0).VertVal   := 16#00#;
      EGAColors (0).BleuVal   := 16#00#;
      EGAColors (1).RougeVal  := 16#00#;
      EGAColors (1).VertVal   := 16#00#;
      EGAColors (1).BleuVal   := 16#fc#;
      EGAColors (2).RougeVal  := 16#24#;
      EGAColors (2).VertVal   := 16#fc#;
      EGAColors (2).BleuVal   := 16#24#;
      EGAColors (3).RougeVal  := 16#00#;
      EGAColors (3).VertVal   := 16#fc#;
      EGAColors (3).BleuVal   := 16#fc#;
      EGAColors (4).RougeVal  := 16#fc#;
      EGAColors (4).VertVal   := 16#14#;
      EGAColors (4).BleuVal   := 16#14#;
      EGAColors (5).RougeVal  := 16#b0#;
      EGAColors (5).VertVal   := 16#00#;
      EGAColors (5).BleuVal   := 16#f#;
      EGAColors (6).RougeVal  := 16#70#;
      EGAColors (6).VertVal   := 16#48#;
      EGAColors (6).BleuVal   := 16#00#;
      EGAColors (7).RougeVal  := 16#c4#;
      EGAColors (7).VertVal   := 16#c4#;
      EGAColors (7).BleuVal   := 16#c4#;
      EGAColors (8).RougeVal  := 16#34#;
      EGAColors (8).VertVal   := 16#34#;
      EGAColors (8).BleuVal   := 16#34#;
      EGAColors (9).RougeVal  := 16#00#;
      EGAColors (9).VertVal   := 16#00#;
      EGAColors (9).BleuVal   := 16#70#;
      EGAColors (10).RougeVal := 16#00#;
      EGAColors (10).VertVal  := 16#70#;
      EGAColors (10).BleuVal  := 16#00#;
      EGAColors (11).RougeVal := 16#00#;
      EGAColors (11).VertVal  := 16#70#;
      EGAColors (11).BleuVal  := 16#70#;
      EGAColors (12).RougeVal := 16#70#;
      EGAColors (12).VertVal  := 16#00#;
      EGAColors (12).BleuVal  := 16#00#;
      EGAColors (13).RougeVal := 16#70#;
      EGAColors (13).VertVal  := 16#00#;
      EGAColors (13).BleuVal  := 16#70#;
      EGAColors (14).RougeVal := 16#fc#;
      EGAColors (14).VertVal  := 16#fc#;
      EGAColors (14).BleuVal  := 16#24#;
      EGAColors (15).RougeVal := 16#fc#;
      EGAColors (15).VertVal  := 16#fc#;
      EGAColors (15).BleuVal  := 16#fc#;
      GraphPilote             := IBM8514;
      GraphMode               := IBM8514HI;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult < 0 then
         Halt (1);
      end if;
      for i in 0 .. MaxColors loop
         declare
            P2Ada_Var_9 : RVBRec renames EGAColors (i);  -- !Help! No type
                                                         --found -> add
                                                         --'P2Ada_Var_9.' to
                                                         --fields of
                                                         --EGAColors(i);
         begin
            SetRGBPalette (i, 0, 0, 0);
         end; -- declare
      end loop;
      for i in 0 .. MaxColors loop
         SetColor (i);
         OutTextXY (10, i * 10, " ..Frappez une touche.. ");
      end loop;
      for i in 0 .. MaxColors loop
         declare
            P2Ada_Var_10 : RVBRec renames EGAColors (i); -- !Help! No type
                                                         --found -> add
                                                         --'P2Ada_Var_10.' to
                                                         --fields of
                                                         --EGAColors(i);
         begin
            SetRGBPalette
              (i,
               P2Ada_Var_10.RougeVal * 128,
               P2Ada_Var_10.VertVal * 128,
               P2Ada_Var_10.BleuVal * 128);
         end; -- declare
      end loop;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExSetRGBPalette; -- block
   procedure ExSetTextBuf is
      f  : Text;
      ch : Char;
      type P2Ada_Anonym_10 is array (1 .. 10240) of Char;
      tampon : P2Ada_Anonym_10;
   begin
      SetTextBuf (f, tampon'Address);
      Assign (f, "test.txt");
      Reset (f);
      while not Eof (f) loop
         Read (f, ch);
         Write (+ch);
      end loop;
      Close (f);
      Writeln;
   end ExSetTextBuf; -- block
   procedure ExSetTextJustify is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      SetTextJustify (CenterText, CenterText);
      OutTextXY (Succ (GetMaxX) / 2, Succ (GetMaxY) / 2, "Ce texte est centré sans fatigue");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExSetTextJustify; -- block
   procedure ExSetTextStyle is
      GraphPilote, GraphMode : Integer := 0;
      Y                      : Integer;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Y := 0;
      for Taille in 1 .. 4 loop
         SetTextStyle (DefaultFont, HorizDir, Taille);
         OutTextXY (0, Y, Concat ("Taille = ", +Chr (Taille + 48)));
         SetTextStyle (TriplexFont, HorizDir, Taille);
         OutTextXY (100, Y, Concat ("Taille = ", +Chr (Taille + 48)));
         SetTextStyle (SmallFont, HorizDir, Taille);
         OutTextXY (200, Y, Concat ("Taille = ", +Chr (Taille + 48)));
         SetTextStyle (SansSerifFont, HorizDir, Taille);
         OutTextXY (300, Y, Concat ("Taille = ", +Chr (Taille + 48)));
         SetTextStyle (GothicFont, HorizDir, Taille);
         OutTextXY (400, Y, Concat ("Taille = ", +Chr (Taille + 48)));
         Inc (Y, TextHeight (+'H') + 1);
      end loop;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExSetTextStyle; -- block
   procedure ExSetTime is
   begin
      SetTime (0, 1, 0, 0);
   end ExSetTime; -- block
   procedure ExSetUserCharSize is
      GraphPilote, GraphMode : Integer := 0;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      SetTextStyle (TriplexFont, HorizDir, 4);
      OutText ("Normal");
      SetUserCharSize (1, 3, 1, 2);
      OutText ("Petit");
      SetUserCharSize (3, 1, 1, 1);
      OutText ("Large");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExSetUserCharSize; -- block
   procedure ExSetViewport is
      vp1                    : ViewPortType;
      vp2                    : ViewPortType;
      GraphPilote, GraphMode : Integer := 0;
   begin
      vp1.X1      := 10;
      vp1.Y1      := 80;
      vp1.X2      := 100;
      vp1.Y2      := 150;
      vp1.Clip    := ClipOn;
      vp2.X1      := 110;
      vp2.Y1      := 0;
      vp2.X2      := 200;
      vp2.Y2      := 70;
      vp2.Clip    := ClipOn;
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      declare
         P2Ada_Var_11 : ViewPortType renames vp1;  -- !Help! No type found ->
                                                   --add 'P2Ada_Var_11.' to
                                                   --fields of vp1;
      begin
         Rectangle
           (Succ (P2Ada_Var_11.X1),
            Succ (P2Ada_Var_11.Y1),
            Pred (P2Ada_Var_11.X2),
            Pred (P2Ada_Var_11.Y2));
         SetViewPort (P2Ada_Var_11.X1, P2Ada_Var_11.Y1, P2Ada_Var_11.X2, P2Ada_Var_11.Y2, ClipOn);
         OutText ("Fenêtre 1");
      end; -- declare
      SetViewPort (0, 0, GetMaxX, GetMaxY, ClipOn);
      declare
         P2Ada_Var_12 : ViewPortType renames vp2;  -- !Help! No type found ->
                                                   --add 'P2Ada_Var_12.' to
                                                   --fields of vp2;
      begin
         Rectangle
           (Succ (P2Ada_Var_12.X1),
            Succ (P2Ada_Var_12.Y1),
            Pred (P2Ada_Var_12.X2),
            Pred (P2Ada_Var_12.Y2));
         SetViewPort (P2Ada_Var_12.X1, P2Ada_Var_12.Y1, P2Ada_Var_12.X2, P2Ada_Var_12.Y2, ClipOn);
         OutText ("Fenêtre 2");
      end; -- declare
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExSetViewport; -- block
   procedure ExSetWriteMode is
      GraphPilote, GraphMode : Integer := 0;
      x1, y1, dx, dy         : Integer;
      Remplissage            : FillSettingsType;
      Ch                     : Char;
      pragma Unreferenced (Ch);
   begin
      DirectVideo := False;
      Randomize;
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult < 0 then
         Halt (1);
      end if;
      GetFillSettings (Remplissage);
      SetFillStyle (WideDotFill, Remplissage.Color);
      Bar (0, 0, GetMaxX, GetMaxY);
      dx := GetMaxX / 4;
      dy := GetMaxY / 4;
      SetLineStyle (SolidLn, 0, ThickWidth);
      SetWriteMode (XORPut);
      loop
         x1 := Random (GetMaxX - dx);
         y1 := Random (GetMaxY - dy);
         Rectangle (x1, y1, x1 + dx, y1 + dy);
         Delay1 (1000);
         Rectangle (x1, y1, x1 + dx, y1 + dy);
         exit when KeyPressed;
         Delay1 (1000);
      end loop;
      Ch := ReadKey;
      CloseGraph;
   end ExSetWriteMode; -- block
   procedure ExSin is
      r : Real;
   begin
      r := Sin (Pi);
      Writeln (r'Img);
   end ExSin; -- block
   procedure ExSizeof is
      type Contact is record
         Nom : P2Ada_String (1 .. 30);
         Tel : P2Ada_String (1 .. 14);
      end record;
      a : Char;
      b : Real;
      p : Pointer;
   begin
      Writeln (Integer'Image (Integer'Size / 8));
      Writeln (Integer'Image (a'Size / 8));
      Writeln (Integer'Image (b'Size / 8));
      Writeln (Integer'Image (p'Size / 8));
      Writeln (Integer'Image (Contact'Size / 8));
      GetMem (p, Contact'Size / 8);
   end ExSizeof; -- block
   procedure ExSqrSqrt is
   begin
      Writeln ("Le carré de 5 est " + Sqr (5.0)'Img);
      Writeln ("La racine carrée de 2 est " + Sqrt (2.0)'Img);
   end ExSqrSqrt; -- block
   procedure ExStr is
      function IntEnStr (i : Longint) return P2Ada_String is
         P2Ada_Result_IntEnStr : P2Ada_String (1 .. 255);
         s                     : P2Ada_String (1 .. 11);
      begin
         Assign_String (s, i'Img);
         Assign_String (P2Ada_Result_IntEnStr, s);
         return P2Ada_Result_IntEnStr;
      end IntEnStr; -- block
   begin
      Writeln (IntEnStr (-5322));
   end ExStr; -- block
   procedure ExSwap is
      x : Word;
   begin
      x := Swap (16#1234#);
      Writeln (x'Img);
   end ExSwap; -- block
   procedure ExTextHeight is
      GraphPilote, GraphMode : Integer := 0;
      Y                      : Integer;
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Y := 0;
      for Taille in 1 .. 5 loop
         SetTextStyle (DefaultFont, HorizDir, Taille);
         OutTextXY (0, Y, "Graphiques par Turbo ");
         Inc (Y, TextHeight ("Graphiques par Turbo "));
      end loop;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExTextHeight; -- block
   procedure ExTextMode is
      OrigMode : Integer;
   begin
      OrigMode := LastMode;
      TextMode (CO80);
      Writeln ("Grands caractères");
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      TextMode (OrigMode);
   end ExTextMode; -- block
   procedure ExTextWidth is
      GraphPilote, GraphMode : Integer := 0;
      Colonne                : Integer;
      Taille                 : Integer;
      Titre                  : P2Ada_String (1 .. 255);
   begin
      GraphPilote := Detect;
      InitGraph (GraphPilote, GraphMode, "");
      if GraphResult /= grOk then
         Halt (1);
      end if;
      Colonne := 0;
      Assign_String (Titre, "Graphiques Turbo");
      Taille := 1;
      while TextWidth (Titre) < GetMaxX loop
         OutTextXY (0, Colonne, Titre);
         Inc (Colonne, TextHeight (+'M'));
         Inc (Taille);
         SetTextStyle (DefaultFont, HorizDir, Taille);
      end loop;
      Write ("Frappez <Entrée> pour continuer");
      Readln;
      CloseGraph;
   end ExTextWidth; -- block
   procedure ExTrunc is
   begin
      Writeln (Real'Image (1.4) + " devient " + Trunc (1.4)'Img);
      Writeln (Real'Image (1.5) + " devient " + Trunc (1.5)'Img);
      Writeln (Real'Image (-1.4) + " devient " + Trunc (-1.4)'Img);
      Writeln (Real'Image (-1.5) + " devient " + Trunc (-1.5)'Img);
   end ExTrunc; -- block
   procedure ExTruncate is
      --  package P2Ada_File_Of_INTEGER is new Ada.Direct_IO (INTEGER);
      f    : Text; --P2Ada_File_Of_INTEGER.File_Type;
      i, j : Integer;
   begin
      Assign (f, "test.txt");
      Rewrite (f);
      for i in 1 .. 6 loop
         Write (f, i'Img);
      end loop;
      Write ("Fichier avant qu'il soit tronqué :");
      Reset (f);
      while not Eof (f) loop
         Read (f, i);
         Writeln (i'Img);
      end loop;
      Reset (f);
      for i in 1 .. 3 loop
         Read (f, j);
      end loop;
      Truncate (f);
      Write ("Fichier après qu'il eut été tronqué :");
      Reset (f);
      while not Eof (f) loop
         Read (f, i);
         Writeln (i'Img);
      end loop;
      Close (f);
   end ExTruncate; -- block
   procedure ExUpCase is
      s : P2Ada_String (1 .. 255);
   begin
      Write ("Entrez une chaîne: ");
      Readln (s);
      for i in 1 .. Length (s) loop
         s (i) := UpCase (s (i));
      end loop;
      Writeln ("La voila en majuscule : " + s);
   end ExUpCase; -- block
   procedure ExVal is
      i, Erreur : Integer;
   begin
      Val (ParamStr (3), i, Erreur);
      if Erreur /= 0 then
         Writeln ("Erreur à la position: " + Erreur'Img);
      else
         Writeln ("Valeur = " + i'Img);
      end if;
   end ExVal; -- block
   procedure ExWherexWherey is
   begin
      Writeln ("Le curseur se trouvait en (" + WhereX'Img + ',' + WhereY'Img + ')');
   end ExWherexWherey; -- block
   procedure ExWindow is
      x, y : Byte;
      Ch   : Char;
      pragma Unreferenced (Ch);
   begin
      TextBackground (Crt.Black);
      ClrScr;
      loop
         x := Succ (Random (80));
         y := Succ (Random (25));
         Window (x, y, x + Random (10), y + Random (8));
         TextBackground (Random (16));
         ClrScr;
         exit when KeyPressed;
         Delay1 (1000);
      end loop;
      Ch := ReadKey;
      NormVideo;
   end ExWindow; -- block
begin
   Writeln ("Test ExDebug");
   ExDebug;
   --     Writeln ("Test ExAbs");
   --     ExAbs;
   --     Writeln ("Test ExAddr");
   --     ExAddr;
   --     Writeln ("Test ExAppend");
   --     ExAppend;
   --     Writeln ("Test ExArc");
   --     ExArc;
   --     Writeln ("Test ExArcTan");
   --     ExArcTan;
   --     Writeln ("Test ExAssign");
   --     ExAssign;
   --     Writeln ("Test ExAssigned");
   --     ExAssigned;
   --     Writeln ("Test ExAssignCrt");
   --     ExAssignCrt;
   --     Writeln ("Test ExAssign_String");
   --     ExAssign_String;
   --     Writeln ("Test ExBar");
   --     ExBar;
   --     Writeln ("Test ExBar3D");
   --     ExBar3D;
   --     Writeln ("Test ExAssignBlockReadWrite");
   --     ExAssignBlockReadWrite;
   --     Writeln ("Test ExChdir");
   --     ExChdir;
   --     Writeln ("Test ExChr");
   --     ExChr;
   --     Writeln ("Test ExCircle");
   --     ExCircle;
   --     Writeln ("Test ExClearDevice");
   --     ExClearDevice;
   --     Writeln ("Test ExClearViewport");
   --     ExClearViewport;
   --     Writeln ("Test ExClose");
   --     ExClose;
   --     Writeln ("Test ExCloseGraph");
   --     ExCloseGraph;
   --     Writeln ("Test ExClrEol");
   --     ExClrEol;
   Writeln ("Test ExClrscr");
   --     ExClrscr;
   --     Writeln ("Test ExConcat");
   --     ExConcat;
   --     Writeln ("Test ExCopy");
   --     ExCopy;
   --     Writeln ("Test ExCos");
   --     ExCos;
   --     Writeln ("Test ExCsegDsegSsegSptrOfsSeg");
   --     ExCsegDsegSsegSptrOfsSeg;
   --     Writeln ("Test ExDec");
   --     ExDec;
   --     Writeln ("Test ExDelayNoSoundSound");
   --     ExDelayNoSoundSound;
   --     Writeln ("Test ExDelete");
   --     ExDelete;
   --     Writeln ("Test ExDelLine");
   --     ExDelLine;
   --     Writeln ("Test ExDetectGraph");
   --     ExDetectGraph;
   --     Writeln ("Test ExDiskFree");
   --     ExDiskFree;
   --     Writeln ("Test ExDiskSize");
   --     ExDiskSize;
   --     Writeln ("Test ExDisposeNew");
   --     ExDisposeNew;
   --     Writeln ("Test ExDosExitCodeExec");
   --     ExDosExitCodeExec;
   --     Writeln ("Test ExDosVersion");
   --     ExDosVersion;
   --     Writeln ("Test ExDrawPoly");
   --     ExDrawPoly;
   --     Writeln ("Test ExEllipse");
   --     ExEllipse;
   --     Writeln ("Test ExEnvcountEnvstr");
   --     ExEnvcountEnvstr;
   --     Writeln ("Test ExEofReadWrite");
   --     ExEofReadWrite;
   --     Writeln ("Test ExEoln");
   --     ExEoln;
   --     Writeln ("Test ExErase");
   --     ExErase;
   --     Writeln ("Test ExExit"); ExExit;
   --     Writeln ("Test ExExp");
   --     ExExp;
   --     Writeln ("Test ExFexpandFsearch");
   --     ExFexpandFsearch;
   --     Writeln ("Test ExFileposFilesizeSeek");
   --     ExFileposFilesizeSeek;
   --     Writeln ("Test ExFillChar");
   --     ExFillChar;
   --     Writeln ("Test ExFillEllipse");
   --     ExFillEllipse;
   --     Writeln ("Test ExFillPoly");
   --     ExFillPoly;
   --     Writeln ("Test ExFindfirstFindnext");
   --     ExFindfirstFindnext;
   Writeln ("Test ExFloodFill");
   --     ExFloodFill;
   --     Writeln ("Test ExFlush");
   --     ExFlush;
   --     Writeln ("Test ExFrac");
   --     ExFrac;
   --     Writeln ("Test ExFreememGetmemMaxavail");
   --     ExFreememGetmemMaxavail;
   --     Writeln ("Test ExFSplit");
   --     ExFSplit;
   --     Writeln ("Test ExGetArcCoords");
   --     ExGetArcCoords;
   --     Writeln ("Test ExGetAspectRatio");
   --     ExGetAspectRatio;
   --     Writeln ("Test ExGetBkColor");
   --     ExGetBkColor;
   --     Writeln ("Test ExGetcbreakSetcbreak");
   --     ExGetcbreakSetcbreak;
   --     Writeln ("Test ExGetColor");
   --     ExGetColor;
   --     Writeln ("Test ExGetDate");
   --     ExGetDate;
   Writeln ("Test ExGetDefaultPalette");
   --     ExGetDefaultPalette;
   --     Writeln ("Test ExGetDir");
   --     ExGetDir;
   --     Writeln ("Test ExGetDriverName");
   --     ExGetDriverName;
   --     Writeln ("Test ExGetenvSwapvectors");
   --     ExGetenvSwapvectors;
   --     Writeln ("Test ExGetFAttr");
   --     ExGetFAttr;
   Writeln ("Test ExGetfillpatternSetfillpattern");
   --     ExGetfillpatternSetfillpattern;
   Writeln ("Test ExGetfillsettingsSetfillstyle");
   --     ExGetfillsettingsSetfillstyle;
   --     Writeln ("Test ExGetftimePacktimeSetftimeUnpacktime");
   --     ExGetftimePacktimeSetftimeUnpacktime;
   --     Writeln ("Test ExGetGraphMode");
   --     ExGetGraphMode;
   Writeln ("Test ExGetimageImagesizePutimage");
   --     ExGetimageImagesizePutimage;
   --     Writeln ("Test ExGetintvectSetintvect");
   --     ExGetintvectSetintvect;
   --     Writeln ("Test ExGetLineSettings");
   --     ExGetLineSettings;
   --     Writeln ("Test ExGetmaxcolorSetcolor");
   --     ExGetmaxcolorSetcolor;
   --     Writeln ("Test ExGetmaxmodeGetmodename");
   --     ExGetmaxmodeGetmodename;
   --     Writeln ("Test ExGetmaxxGetmaxy");
   --     ExGetmaxxGetmaxy;
   --     Writeln ("Test ExGetmoderangeSetgraphmode");
   --     ExGetmoderangeSetgraphmode;
   --     Writeln ("Test ExGetPalette");
   --     ExGetPalette;
   --     Writeln ("Test ExGetPaletteSize");
   --     ExGetPaletteSize;
   Writeln ("Test ExGetPixel");
   --     ExGetPixel;
   --     Writeln ("Test ExGetTextSettings");
   --     ExGetTextSettings;
   --     Writeln ("Test ExGetTime");
   --     ExGetTime;
   --     Writeln ("Test ExGetverifySetverify");
   --     ExGetverifySetverify;
   --     Writeln ("Test ExGetViewSettings");
   --     ExGetViewSettings;
   --     Writeln ("Test ExGetxGety");
   --     ExGetxGety;
   --     Writeln ("Test ExGotoXY");
   --     ExGotoXY;
   --     Writeln ("Test ExGraphDefaults");
   --     ExGraphDefaults;
   --     Writeln ("Test ExGraphErrorMsg");
   --     ExGraphErrorMsg;
   --     Writeln ("Test ExGraphResult");
   --     ExGraphResult;
   --     Writeln ("Test ExHalt");
   --     ExHalt;
   --     Writeln ("Test ExHi");
   --     ExHi;
   --     Writeln ("Test ExHighVideo");
   --     ExHighVideo;
   --     Writeln ("Test ExInc");
   --     ExInc;
   --     Writeln ("Test ExInitGraph");
   --     ExInitGraph;
   --     Writeln ("Test ExInsert");
   --     ExInsert;
   --     Writeln ("Test ExInsLine");
   --     ExInsLine;
   --     Writeln ("Test ExInstallUserDriver");
   --     ExInstallUserDriver;
   --     Writeln ("Test ExInstallUserFont");
   --     ExInstallUserFont;
   --     Writeln ("Test ExInt");
   --     ExInt;
   --     Writeln ("Test ExIntr");
   --     ExIntr;
   --     Writeln ("Test ExIOResult");
   --     ExIOResult;
   --     Writeln ("Test ExKeep");
   --     ExKeep;
   --     Writeln ("Test ExKeyPressed");
   --     ExKeyPressed;
   --     Writeln ("Test ExLenght");
   --     ExLenght;
   --     Writeln ("Test ExLine");
   --     ExLine;
   --     Writeln ("Test ExLineRel");
   --     ExLineRel;
   --     Writeln ("Test ExLineTo");
   --     ExLineTo;
   --     Writeln ("Test ExLn");
   --     ExLn;
   --     Writeln ("Test ExLo");
   --     ExLo;
   --     Writeln ("Test ExLowVideo");
   --     ExLowVideo;
   --     Writeln ("Test ExMarkRelease");
   --     ExMarkRelease;
   --     Writeln ("Test ExMemavailMaxavail");
   --     ExMemavailMaxavail;
   --     Writeln ("Test ExMkdir");
   --     ExMkdir;
   --     Writeln ("Test ExMove");
   --     ExMove;
   --     Writeln ("Test ExMoveRel");
   --     ExMoveRel;
   --     Writeln ("Test ExMoveTo");
   --     ExMoveTo;
   --     Writeln ("Test ExMsdos");
   --     ExMsdos;
   --     Writeln ("Test ExNormvideoTextbackgroundTextcolor");
   --     ExNormvideoTextbackgroundTextcolor;
   --     Writeln ("Test ExOdd");
   --     ExOdd;
   --     Writeln ("Test ExOrd");
   --     ExOrd;
   Writeln ("Test ExOutText");
   ExOutText;
   Writeln ("Test ExOutTextXY");
   ExOutTextXY;
   --     Writeln ("Test ExParamCount");
   --     ExParamCount;
   --     Writeln ("Test ExParamStr");
   --     ExParamStr;
   --     Writeln ("Test ExPi");
   --     ExPi;
   --     Writeln ("Test ExPieSlice");
   --     ExPieSlice;
   --     Writeln ("Test ExPos");
   --     ExPos;
   --     Writeln ("Test ExPredSucc");
   --     ExPredSucc;
   --     Writeln ("Test ExPtr");
   --     ExPtr;
   --     Writeln ("Test ExPutPixel");
   --     ExPutPixel;
   --     Writeln ("Test ExRandomRandomize");
   --     ExRandomRandomize;
   --     Writeln ("Test ExReadKey");
   --     ExReadKey;
   --     Writeln ("Test ExReadlnWriteln");
   --     ExReadlnWriteln;
   --     Writeln ("Test ExRectangle");
   --     ExRectangle;
   --     Writeln ("Test ExRegisterBGIDriver");
   --     ExRegisterBGIDriver;
   --     Writeln ("Test ExRegisterBGIFont");
   --     ExRegisterBGIFont;
   --     Writeln ("Test ExRename");
   --     ExRename;
   --     Writeln ("Test ExReset");
   --     ExReset;
   --     Writeln ("Test ExRestoreCRTMode");
   --     ExRestoreCRTMode;
   --     Writeln ("Test ExRewrite");
   --     ExRewrite;
   --     Writeln ("Test ExRmdir");
   --     ExRmdir;
   --     Writeln ("Test ExRound");
   --     ExRound;
   --     Writeln ("Test ExRunError");
   --     ExRunError;
   --     Writeln ("Test ExSector");
   --     ExSector;
   --     Writeln ("Test ExSeekeofSeekeoln");
   --     ExSeekeofSeekeoln;
   --     Writeln ("Test ExSetactivatepageSetvisualpage");
   --     ExSetactivatepageSetvisualpage;
   Writeln ("Test ExSetAllPalette");
   --     ExSetAllPalette;
   Writeln ("Test ExSetAspectRatio");
   --     ExSetAspectRatio;
   --     Writeln ("Test ExSetBkColor");
   --     ExSetBkColor;
   --     Writeln ("Test ExSetDate"); ExSetDate;
   --     Writeln ("Test ExSetFAttr"); ExSetFAttr;
   --     Writeln ("Test ExSetGraphBufSize");
   --     ExSetGraphBufSize;
   --     Writeln ("Test ExSetLineStyle");
   --     ExSetLineStyle;
   Writeln ("Test ExSetPalette");
   --     ExSetPalette;
   Writeln ("Test ExSetRGBPalette");
   --     ExSetRGBPalette;
   --     Writeln ("Test ExSetTextBuf");
   --     ExSetTextBuf;
   --     Writeln ("Test ExSetTextJustify");
   --     ExSetTextJustify;
   Writeln ("Test ExSetTextStyle");
   ExSetTextStyle;
   --     Writeln ("Test ExSetTime");
   --     ExSetTime;
   Writeln ("Test ExSetUserCharSize");
   ExSetUserCharSize;
   --     Writeln ("Test ExSetViewport");
   --     ExSetViewport;
   Writeln ("Test ExSetWriteMode");
   ExSetWriteMode;
   --     Writeln ("Test ExSin");
   --     ExSin;
   --     Writeln ("Test ExSizeof"); ExSizeof;
   --     Writeln ("Test ExSqrSqrt");
   --     ExSqrSqrt;
   --     Writeln ("Test ExStr"); ExStr;
   --     Writeln ("Test ExSwap");
   --     ExSwap;
   Writeln ("Test ExTextHeight");
   ExTextHeight;
   --     Writeln ("Test ExTextMode");
   --     ExTextMode;
   Writeln ("Test ExTextWidth");
   ExTextWidth;
   --     Writeln ("Test ExTrunc");
   --     ExTrunc;
   --     Writeln ("Test ExTruncate");
   --     ExTruncate;
   --     Writeln ("Test ExUpCase");
   --     ExUpCase;
   --     Writeln ("Test ExVal");
   --     ExVal;
   --     Writeln ("Test ExWherexWherey");
   --     ExWherexWherey;
   Writeln ("Test ExWindow");
   --     ExWindow;
   Write ("Fin des exemples TP7, frappez entrée pour quitter.");
   Readln;
end Exemples_TP7; -- block
-- Translated on 8-Aug-2011 by (Obj) P2Ada V1.4a 28-Mar-2010
