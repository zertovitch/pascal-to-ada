-------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : tp7-system.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 2.3c
-- DATE DE LA DERNIERE MISE A JOUR  : 29 octobre 2012
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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Directories;
with Ada.Command_Line;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;
with Ada.Long_Long_Float_Text_IO;

package body TP7.System is

   IntCheck : constant Boolean := False;
   noErr    : constant Word    := 0;
   FNOErr   : constant Word    := 103;
   DirErr   : constant Word    := 107;

   procedure FSError (Num : Word; S : String) is
   begin
      if Debug or else IntCheck then
         Put_Line ("File error: " & Num'Img & ", " & S);
         if IntCheck then
            Halt;
         end if;
      end if;
      InOutRes := Num;
   end FSError;

   ------------
   -- Assign --
   ------------

   procedure Assign (F : out Text; Name : String) is
   -- TIPS : if Name is "" then F is linked to stdout or stdin
   --        if Name is "PRN" then F is linked to printer (here stdout)
   --        TBF CON, LPT1, LPT2, LPT3, COM1, COM2, NUL
   begin
      if Is_Equal (Name, "") then
         F.Device := Stdinout;
      elsif Is_Equal (Name, "PRN") then
         F.Device := Stdinout;
      else
         F.Device := File_System;
         F.Name   := new String'(To_String (Name));
      end if;
   end Assign;

   -----------
   -- Reset --
   -----------

   procedure Reset (F : in out Text) is
   begin
      case F.Device is
         when File_System =>
            if Ada.Text_IO.Is_Open (F.File) then
               Ada.Text_IO.Reset (F.File, Ada.Text_IO.In_File);
            else
               Ada.Text_IO.Open (F.File, Ada.Text_IO.In_File, F.Name.all);
            end if;
         when Stdinout =>
            null;
         when Win_CRT =>
            null;
      end case;
   end Reset;

   -------------
   -- Rewrite --
   -------------

   procedure Rewrite (F : in out Text) is
   begin
      case F.Device is
         when File_System =>
            if Ada.Text_IO.Is_Open (F.File) then
               Ada.Text_IO.Reset (F.File, Ada.Text_IO.Out_File);
            else
               Ada.Text_IO.Create (F.File, Ada.Text_IO.Out_File, F.Name.all);
            end if;
         when Stdinout =>
            null;
         when Win_CRT =>
            null;
      end case;
   end Rewrite;

   ------------
   -- Append --
   ------------

   procedure Append (F : in out Text) is
   begin
      case F.Device is
         when File_System =>
            if Ada.Text_IO.Is_Open (F.File) then
               Ada.Text_IO.Reset (F.File, Ada.Text_IO.Append_File);
            else
               Ada.Text_IO.Open (F.File, Ada.Text_IO.Append_File, F.Name.all);
            end if;
         when Stdinout =>
            null;
         when Win_CRT =>
            null;
      end case;
   end Append;

   -----------
   -- Close --
   -----------

   procedure Close (F : in out Text) is
   begin
      case F.Device is
         when File_System =>
            Ada.Text_IO.Close (F.File);
         when Stdinout =>
            null;
         when Win_CRT =>
            null;
      end case;
   end Close;

   ---------
   -- Eof --
   ---------

   function Eof (F : Text) return Boolean is
   begin
      case F.Device is
         when File_System =>
            return Ada.Text_IO.End_Of_File (F.File);
         when Stdinout =>
            return False;
         when Win_CRT =>
            return False;
      end case;
   end Eof;

   function Eof return Boolean is
   begin
      case Input.Device is
         when File_System =>
            return Ada.Text_IO.End_Of_File;
         when Stdinout =>
            return False;
         when Win_CRT =>
            return False;
      end case;
   end Eof;

   ----------
   -- Eoln --
   ----------

   function Eoln (F : Text) return Boolean is
   begin
      case F.Device is
         when File_System =>
            return Ada.Text_IO.End_Of_Line (F.File);
         when Stdinout =>
            return False;
         when Win_CRT =>
            return False;
      end case;
   end Eoln;

   ----------
   -- Eoln --
   ----------

   function Eoln return Boolean is
   begin
      case Input.Device is
         when File_System =>
            return Ada.Text_IO.End_Of_Line;
         when Stdinout =>
            return False;
         when Win_CRT =>
            return False;
      end case;
   end Eoln;

   -------------
   -- SeekEof --
   -------------

   function SeekEof (F : Text) return Boolean is
      C   : Char    := ' ';
      EOL : Boolean := False;
   begin
      case F.Device is
         when File_System =>
            while not EOL and then (C = ' ' or else C = Ada.Characters.Latin_1.HT) loop
               Ada.Text_IO.Look_Ahead (F.File, C, EOL);
               if not EOL and then (C = ' ' or else C = Ada.Characters.Latin_1.HT) then
                  Ada.Text_IO.Get (F.File, C);
               end if;
            end loop;
            return Ada.Text_IO.End_Of_File (F.File);
         when Stdinout =>
            return False;
         when Win_CRT =>
            return False;
      end case;
   end SeekEof;

   --------------
   -- SeekEoln --
   --------------

   function SeekEoln (F : Text) return Boolean is
      C   : Char    := ' ';
      EOL : Boolean := False;
   begin
      case F.Device is
         when File_System =>
            while not EOL and then (C = ' ' or else C = Ada.Characters.Latin_1.HT) loop
               Ada.Text_IO.Look_Ahead (F.File, C, EOL);
               if not EOL and then (C = ' ' or else C = Ada.Characters.Latin_1.HT) then
                  Ada.Text_IO.Get (F.File, C);
               end if;
            end loop;
            return EOL;
         when Stdinout =>
            return False;
         when Win_CRT =>
            return False;
      end case;
   end SeekEoln;

   ------------
   -- Assign --
   ------------

   procedure Assign (F : out File; Name : String) is
   begin
      F.Name := new String'(To_String (Name));
   end Assign;

   -----------
   -- Reset --
   -----------

   procedure Reset (F : in out File; TailleRec : Word := 0) is
      pragma Unreferenced (TailleRec);
   begin
      F.File := GNAT.OS_Lib.Open_Read (F.Name.all, GNAT.OS_Lib.Binary);
   end Reset;

   -------------
   -- Rewrite --
   -------------

   procedure Rewrite (F : in out File; TailleRec : Word := 0) is
      pragma Unreferenced (TailleRec);
   begin
      F.File := GNAT.OS_Lib.Create_File (F.Name.all, GNAT.OS_Lib.Binary);
   end Rewrite;

   -----------
   -- Close --
   -----------

   procedure Close (F : in out File) is
   begin
      GNAT.OS_Lib.Close (F.File);
   end Close;

   ---------------
   -- BlockRead --
   ---------------

   procedure BlockRead (F : in out File; Buf : Pointer; Quanti : Word) is
      Res : Integer;
      pragma Unreferenced (Res);
   begin
      Res := GNAT.OS_Lib.Read (F.File, Buf, Quanti);
   end BlockRead;

   ---------------
   -- BlockRead --
   ---------------

   procedure BlockRead
     (F        : in out File;
      Buf      : Pointer;
      Quanti   : Word;
      Resultat : out Word)
   is
   begin
      Resultat := GNAT.OS_Lib.Read (F.File, Buf, Quanti);
   end BlockRead;

   ----------------
   -- BlockWrite --
   ----------------

   procedure BlockWrite (F : in out File; Buf : Pointer; Quanti : Word) is
      Res : Integer;
      pragma Unreferenced (Res);
   begin
      Res := GNAT.OS_Lib.Write (F.File, Buf, Quanti);
   end BlockWrite;

   ----------------
   -- BlockWrite --
   ----------------

   procedure BlockWrite
     (F        : in out File;
      Buf      : Pointer;
      Quanti   : Word;
      Resultat : out Word)
   is
   begin
      Resultat := GNAT.OS_Lib.Write (F.File, Buf, Quanti);
   end BlockWrite;

   -------------
   -- FilePos --
   -------------

   function FilePos (F : File) return Longint is
      pragma Unreferenced (F);
   begin
      if Debug then
         Writeln ("La fonction FilePos n'est pas définie !");
      end if;
      return 0;
   end FilePos;

   --------------
   -- FileSize --
   --------------

   function FileSize (F : File) return Longint is
   begin
      return Longint (GNAT.OS_Lib.File_Length (F.File));
   end FileSize;

   --------------
   -- Truncate --
   --------------

   procedure Truncate (F : in out Text) is
      pragma Unreferenced (F);
   begin
      if Debug then
         Writeln ("La fonction Truncate n'est pas définie !");
      end if;
   end Truncate;

   -----------
   -- Erase --
   -----------

   procedure Erase (F : in out File) is
      Success : Boolean;
   begin
      GNAT.OS_Lib.Delete_File (To_String (F.Name.all), Success);
      if not Success then
         FSError (FNOErr, To_String (F.Name.all));
      end if;
   end Erase;

   ------------
   -- Rename --
   ------------

   procedure Rename (F : in out File; NewName : String) is
      Success : Boolean;
   begin
      GNAT.OS_Lib.Rename_File (To_String (F.Name.all), To_String (NewName), Success);
      if not Success then
         FSError (FNOErr, To_String (F.Name.all + ", " + NewName));
      end if;
   end Rename;

   ---------
   -- Eof --
   ---------

   function Eof (F : File) return Boolean is
      pragma Unreferenced (F);
   begin
      if Debug then
         Writeln ("La fonction Eof n'est pas définie !");
      end if;
      return False;
   end Eof;

   ----------
   -- Seek --
   ----------

   procedure Seek (F : in out File; N : Longint) is
   begin
      GNAT.OS_Lib.Lseek (F.File, Standard.Long_Integer (N), GNAT.OS_Lib.Seek_Set);
   end Seek;

   --------------
   -- IOResult --
   --------------

   function IOResult return Integer is
      LInOutRes : constant Integer := InOutRes;
   begin
      InOutRes := noErr;
      return LInOutRes;
   end IOResult;

   -----------
   -- Write --
   -----------

   procedure Write (S : String) is
   begin
      case Output.Device is
         when File_System =>
            Ada.Text_IO.Put (Output.File, To_String (S));
         when Stdinout =>
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Output, To_String (S));
         when Win_CRT =>
            TP7.Put (S);
      end case;
   end Write;

   -------------
   -- Writeln --
   -------------

   procedure Writeln (S : String) is
   begin
      case Output.Device is
         when File_System =>
            Ada.Text_IO.Put_Line (Output.File, To_String (S));
         when Stdinout =>
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output, To_String (S));
         when Win_CRT =>
            TP7.Put_Line (S);
      end case;
   end Writeln;

   -------------
   -- Writeln --
   -------------

   procedure Writeln is
   begin
      case Output.Device is
         when File_System =>
            Ada.Text_IO.New_Line (Output.File);
         when Stdinout =>
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Output);
         when Win_CRT =>
            TP7.New_Line;
      end case;
   end Writeln;

   -----------
   -- Write --
   -----------

   procedure Write (F : Text; S : String) is
   begin
      case F.Device is
         when File_System =>
            Ada.Text_IO.Put (F.File, To_String (S));
         when Stdinout =>
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Output, To_String (S));
         when Win_CRT =>
            TP7.Put (S);
      end case;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write (I : Integer; MinWidth : Integer := 0) is
      S : String (1 .. Standard.Integer'Max (MinWidth, Integer'Width));
   begin
      case Output.Device is
         when File_System =>
            Ada.Integer_Text_IO.Put (Output.File, I, MinWidth);
         when Stdinout =>
            Ada.Integer_Text_IO.Put (Ada.Text_IO.Standard_Output, I, MinWidth);
         when Win_CRT =>
            Ada.Integer_Text_IO.Put (S, I);
            TP7.Put (S);
      end case;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write (R : Real; MinWidth : Integer := 0; DecPlaces : Integer := 0) is
      S : String (1 .. Standard.Integer'Max (MinWidth, Real'Width));
   begin
      case Output.Device is
         when File_System =>
            Ada.Long_Long_Float_Text_IO.Put (Output.File, R, MinWidth, DecPlaces, 0);
         when Stdinout =>
            Ada.Long_Long_Float_Text_IO.Put
              (Ada.Text_IO.Standard_Output,
               R,
               MinWidth,
               DecPlaces,
               0);
         when Win_CRT =>
            Ada.Long_Long_Float_Text_IO.Put (S, R, DecPlaces, 0);
            TP7.Put (S);
      end case;
   end Write;

   -------------
   -- Writeln --
   -------------

   procedure Writeln (F : Text; S : String) is
   begin
      case F.Device is
         when File_System =>
            Ada.Text_IO.Put_Line (F.File, To_String (S));
         when Stdinout =>
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output, To_String (S));
         when Win_CRT =>
            TP7.Put_Line (S);
      end case;
   end Writeln;

   -------------
   -- Writeln --
   -------------

   procedure Writeln (F : Text) is
   begin
      case F.Device is
         when File_System =>
            Ada.Text_IO.New_Line (F.File);
         when Stdinout =>
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Output);
         when Win_CRT =>
            TP7.New_Line;
      end case;
   end Writeln;

   ---------
   -- Chr --
   ---------

   function Chr (V : Byte) return Char is
   begin
      return Char'Val (V);
   end Chr;

   ---------
   -- Ord --
   ---------

   function Ord (C : Char) return Longint is
   begin
      return Char'Pos (C);
   end Ord;

   ----------
   -- Pred --
   ----------

   function Pred (Val : Char) return Char is
   begin
      return Char'Pred (Val);
   end Pred;

   function Pred (Val : Longint) return Longint is
   begin
      return Val - 1;
   end Pred;

   ----------
   -- Mark --
   ----------

   procedure Mark (Object : out Pointer) is
      pragma Unreferenced (Object);
   begin
      if Debug then
         Writeln ("La fonction Mark n'est pas définie !");
      end if;
   end Mark;

   -------------
   -- Release --
   -------------

   procedure Release (Object : Pointer) is
      pragma Unreferenced (Object);
   begin
      if Debug then
         Writeln ("La fonction Release n'est pas définie !");
      end if;
   end Release;

   ----------
   -- Abs1 --
   ----------

   function Abs1 (V : Real) return Real is
   begin
      return abs V;
   end Abs1;

   function Abs1 (V : Longint) return Longint is
   begin
      return abs V;
   end Abs1;

   ---------
   -- Sqr --
   ---------

   function Sqr (V : Real) return Real is
   begin
      return V * V;
   end Sqr;

   ----------------
   -- SetTextBuf --
   ----------------

   procedure SetTextBuf (F : Text; Buffer : Pointer; Size : Word := 0) is
      pragma Unreferenced (Size, Buffer, F);
   begin
      if Debug then
         Writeln ("La fonction SetTextBuf n'est pas définie !");
      end if;
   end SetTextBuf;

   -----------
   -- Flush --
   -----------

   procedure Flush (F : Text) is
   begin
      case F.Device is
         when File_System =>
            Ada.Text_IO.Flush (F.File);
         when Stdinout =>
            Ada.Text_IO.Flush (Ada.Text_IO.Standard_Input);
         when Win_CRT =>
            null;
      end case;
   end Flush;

   ------------
   -- Readln --
   ------------

   procedure Readln is
   begin
      case Input.Device is
         when File_System =>
            Ada.Text_IO.Skip_Line (Input.File);
         when Stdinout =>
            Ada.Text_IO.Skip_Line (Ada.Text_IO.Standard_Input);
         when Win_CRT =>
            TP7.Get_Line;
      end case;
   end Readln;

   ------------
   -- Readln --
   ------------

   procedure Readln (F : Text) is
   begin
      case F.Device is
         when File_System =>
            Ada.Text_IO.Skip_Line (F.File);
         when Stdinout =>
            Ada.Text_IO.Skip_Line (Ada.Text_IO.Standard_Input);
         when Win_CRT =>
            TP7.Get_Line;
      end case;
   end Readln;

   ------------
   -- Readln --
   ------------

   procedure Readln (S : out String) is
   begin
      case Input.Device is
         when File_System =>
            Assign_String (S, Ada.Text_IO.Get_Line (Input.File));
         when Stdinout =>
            Assign_String (S, Ada.Text_IO.Get_Line (Ada.Text_IO.Standard_Input));
         when Win_CRT =>
            Assign_String (S, TP7.Get_Line);
      end case;
   end Readln;

   ------------
   -- Readln --
   ------------

   procedure Readln (F : Text; S : out String) is
   begin
      case F.Device is
         when File_System =>
            Assign_String (S, Ada.Text_IO.Get_Line (F.File));
         when Stdinout =>
            Assign_String (S, Ada.Text_IO.Get_Line (Ada.Text_IO.Standard_Input));
         when Win_CRT =>
            Assign_String (S, TP7.Get_Line);
      end case;
   end Readln;

   ------------
   -- Readln --
   ------------

   procedure Readln (C : out Char) is
   begin
      case Input.Device is
         when File_System =>
            Ada.Text_IO.Get (Input.File, C);
            Ada.Text_IO.Skip_Line (Input.File);
         when Stdinout =>
            Ada.Text_IO.Get (Ada.Text_IO.Standard_Input, C);
            Ada.Text_IO.Skip_Line (Input.File);
         when Win_CRT =>
            declare
               S : constant String := TP7.Get_Line;
            begin
               C := S (S'First);
            end;
      end case;
   end Readln;

   ------------
   -- Readln --
   ------------

   procedure Readln (F : Text; C : out Char) is
   begin
      case F.Device is
         when File_System =>
            Ada.Text_IO.Get (F.File, C);
            Ada.Text_IO.Skip_Line (F.File);
         when Stdinout =>
            Ada.Text_IO.Get (Ada.Text_IO.Standard_Input, C);
            Ada.Text_IO.Skip_Line (Input.File);
         when Win_CRT =>
            declare
               S : constant String := TP7.Get_Line;
            begin
               C := S (S'First);
            end;
      end case;
   end Readln;

   ------------
   -- Readln --
   ------------

   procedure Readln (I : out Integer) is
   begin
      case Input.Device is
         when File_System =>
            Ada.Integer_Text_IO.Get (Input.File, I);
         when Stdinout =>
            Ada.Integer_Text_IO.Get (I);
         when Win_CRT =>
            declare
               S : constant String := TP7.Get_Line;
            begin
               I := Integer'Value (To_String (S));
            end;
      end case;
   end Readln;

   ------------
   -- Readln --
   ------------

   procedure Readln (R : out Real) is
   begin
      case Input.Device is
         when File_System =>
            Ada.Long_Long_Float_Text_IO.Get (Input.File, R);
         when Stdinout =>
            Ada.Long_Long_Float_Text_IO.Get (R);
         when Win_CRT =>
            declare
               S : constant String := TP7.Get_Line;
            begin
               R := Real'Value (To_String (S));
            end;
      end case;
   end Readln;

   ----------
   -- Read --
   ----------

   procedure Read (C : out Char) is
   begin
      case Input.Device is
         when File_System =>
            Ada.Text_IO.Get (Input.File, C);
         when Stdinout =>
            Ada.Text_IO.Get (Ada.Text_IO.Standard_Input, C);
         when Win_CRT =>
            declare
               S : constant String := TP7.Get_Line;
            begin
               C := S (S'First);
            end;
      end case;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (F : Text; S : out String) is
   begin
      case F.Device is
         when File_System =>
            Ada.Text_IO.Get (F.File, S);
         when Stdinout =>
            Ada.Text_IO.Get (Ada.Text_IO.Standard_Input, S);
         when Win_CRT =>
            Assign_String (S, TP7.Get_Line);
      end case;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (S : out String) is
   begin
      case Input.Device is
         when File_System =>
            Ada.Text_IO.Get (Input.File, S);
         when Stdinout =>
            Ada.Text_IO.Get (Ada.Text_IO.Standard_Input, S);
         when Win_CRT =>
            Assign_String (S, TP7.Get_Line);
      end case;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (F : Text; C : out Char) is
   begin
      case F.Device is
         when File_System =>
            Ada.Text_IO.Get (F.File, C);
         when Stdinout =>
            Ada.Text_IO.Get (Ada.Text_IO.Standard_Input, C);
         when Win_CRT =>
            declare
               S : constant String := TP7.Get_Line;
            begin
               C := S (S'First);
            end;
      end case;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (F : Text; I : out Integer) is
   begin
      case F.Device is
         when File_System =>
            Ada.Integer_Text_IO.Get (F.File, I);
         when Stdinout =>
            Ada.Integer_Text_IO.Get (I);
         when Win_CRT =>
            declare
               S : constant String := TP7.Get_Line;
            begin
               I := Integer'Value (To_String (S));
            end;
      end case;
   end Read;

   --------------
   -- MemAvail --
   --------------

   function MemAvail return Longint is
   begin
      if Debug then
         Writeln ("La fonction MemAvail n'est pas définie !");
      end if;
      return 0;
   end MemAvail;

   ----------
   -- Copy --
   ----------

   function Copy (S : String; Pos, Len : Integer) return String is
      Index : constant Natural := Ada.Strings.Fixed.Index (S, Null_TPString);
   begin
      if Index = 0 then
         return S (Pos .. Pos + Len - 1) & Ada.Characters.Latin_1.NUL;
      else
         if Pos + Len > Index then
            return S (Pos .. Index) & Ada.Characters.Latin_1.NUL;
         else
            return S (Pos .. Pos + Len - 1) & Ada.Characters.Latin_1.NUL;
         end if;
      end if;
   end Copy;

   ------------
   -- Concat --
   ------------

   function Concat (S1, S2 : String) return String is
   begin
      return S1 + S2;
   end Concat;

   ------------
   -- Delete --
   ------------

   procedure Delete (S : in out String; Pos, Len : Integer) is
      Index : constant Natural := Ada.Strings.Fixed.Index (S, Null_TPString);
   begin
      if Index = 0 then
         Ada.Strings.Fixed.Delete (S, Pos, Pos + Len - 1);
         S (S'Last - Len + 1) := Ada.Characters.Latin_1.NUL;
      else
         if Pos + Len - 1 < Index then
            Ada.Strings.Fixed.Delete (S, Pos, Pos + Len - 1);
         else
            Ada.Strings.Fixed.Delete (S, Pos, Index);
         end if;
      end if;
   end Delete;

   ------------
   -- Insert --
   ------------

   procedure Insert (Src : String; Dest : in out String; Pos : Integer) is
      Index : constant Natural := Ada.Strings.Fixed.Index (Src, Null_TPString);
   begin
      if Index = 0 then
         Assign_String (Dest, Ada.Strings.Fixed.Insert (Dest, Pos, Src));
      else
         Assign_String
           (Dest,
            Ada.Strings.Fixed.Insert (Dest, Pos, Src (Src'First .. Index - 1)));
      end if;
   end Insert;

   ------------
   -- Length --
   ------------

   function Length (S : String) return Integer is
      Index : constant Natural := Ada.Strings.Fixed.Index (S, Null_TPString);
   begin
      if Index = 0 then
         return S'Length;
      else
         return Index - 1;
      end if;
   end Length;

   procedure ChDir (S : String) is
   begin
      Ada.Directories.Set_Directory (To_String (S));
   exception
      when E : others =>
         FSError (DirErr, Ada.Exceptions.Exception_Information (E));
   end ChDir;

   procedure GetDir (D : Byte; S : out String) is
      pragma Unreferenced (D);
   begin
      Assign_String (S, To_TPString (Ada.Directories.Current_Directory));
   exception
      when E : others =>
         FSError (DirErr, Ada.Exceptions.Exception_Information (E));
   end GetDir;

   procedure MkDir (S : String) is
   begin
      Ada.Directories.Create_Directory (To_String (S));
   exception
      when E : others =>
         FSError (DirErr, Ada.Exceptions.Exception_Information (E));
   end MkDir;

   procedure RmDir (S : String) is
   begin
      Ada.Directories.Delete_Directory (To_String (S));
   exception
      when E : others =>
         FSError (DirErr, Ada.Exceptions.Exception_Information (E));
   end RmDir;

   function CSeg return Word is
   begin
      if Debug then
         Writeln ("La fonction CSeg n'est pas définie !");
      end if;
      return 0;
   end CSeg;

   function DSeg return Word is
   begin
      if Debug then
         Writeln ("La fonction DSeg n'est pas définie !");
      end if;
      return 0;
   end DSeg;

   function SSeg return Word is
   begin
      if Debug then
         Writeln ("La fonction SSeg n'est pas définie !");
      end if;
      return 0;
   end SSeg;

   function SPtr return Word is
   begin
      if Debug then
         Writeln ("La fonction SPtr n'est pas définie !");
      end if;
      return 0;
   end SPtr;

   function Seg (X : Pointer) return Word is
      pragma Unreferenced (X);
   begin
      if Debug then
         Writeln ("La fonction Seg n'est pas définie !");
      end if;
      return 0;
   end Seg;

   function Ofs (X : Pointer) return Word is
      pragma Unreferenced (X);
   begin
      if Debug then
         Writeln ("La fonction Ofs n'est pas définie !");
      end if;
      return 0;
   end Ofs;

   procedure Dec (Val : in out Char) is
   begin
      Val := Char'Pred (Val);
   end Dec;

   --     procedure Dec (Val : in out Byte) is
   --     begin
   --        Val := Val - 1;
   --        null;
   --     end Dec;
   --
   --     procedure Dec (Val : in out Shortint) is
   --     begin
   --        Val := Val - 1;
   --        null;
   --     end Dec;
   --
   --     procedure Dec (Val : in out Word) is
   --     begin
   --        Val := Val - 1;
   --        null;
   --     end Dec;
   --
   --     procedure Dec (Val : in out Integer) is
   --     begin
   --        Val := Val - 1;
   --        null;
   --     end Dec;

   procedure Dec (Val : in out Longint) is
   begin
      Val := Val - 1;
   end Dec;

   procedure Dec (Val : in out Char; N : Longint) is
   begin
      Val := Char'Val (Char'Pos (Val) - N);
   end Dec;

   --     procedure Dec (Val : in out Byte; N : Longint) is
   --     begin
   --        Val := Val - Byte (N);
   --        null;
   --     end Dec;
   --
   --     procedure Dec (Val : in out Shortint; N : Longint) is
   --     begin
   --        Val := Val - Shortint (N);
   --        null;
   --     end Dec;
   --
   --     procedure Dec (Val : in out Word; N : Longint) is
   --     begin
   --        Val := Val - Word (N);
   --        null;
   --     end Dec;
   --
   --     procedure Dec (Val : in out Integer; N : Longint) is
   --     begin
   --        Val := Val - Integer (N);
   --        null;
   --     end Dec;

   procedure Dec (Val : in out Longint; N : Longint) is
   begin
      Val := Val - N;
   end Dec;

   procedure FillChar (X : out String; Nombre : Word; Ch : Char) is
   begin
      X (X'First .. X'First + Nombre - 1) := (others => Ch);
   end FillChar;

   procedure FillChar (X : out TTabByte; Nombre : Word; Val : Byte) is
   begin
      X (X'First .. X'First + Nombre - 1) := (others => Byte1 (Val));
   end FillChar;

   function Frac (X : Real) return Real is
   begin
      return X - Real'Truncation (X);
   end Frac;

   procedure FreeMem (P : in out Pointer; Taille : Word) is
      pragma Unreferenced (Taille);
   begin
      if Debug then
         Writeln ("La procédure FreeMem n'est pas définie !");
      end if;
      P := nil;
   end FreeMem;

   procedure GetMem (P : out Pointer; Taille : Word) is
      pragma Unreferenced (Taille);
   begin
      if Debug then
         Writeln ("La procédure GetMem n'est pas définie !");
      end if;
      P := nil;
   end GetMem;

   function MaxAvail return Longint is
   begin
      if Debug then
         Writeln ("La fonction MaxAvail n'est pas définie !");
      end if;
      return 0;
   end MaxAvail;

   procedure Halt (CodeSortie : Word) is
   begin
      while ExitProc /= null loop
         declare
            LEP : constant TPProc := ExitProc;
         begin
            ExitProc := null;
            LEP.all;
         end;
      end loop;
      if Debug then
         Writeln ("Code de sortie = " + CodeSortie'Img);
      end if;
      raise TP7.Halt;
   end Halt;

   procedure Halt is
   begin
      Halt (0);
   end Halt;

   function Hi (X : Word) return Byte is
   begin
      return Byte (X / 256);
   end Hi;

   function Lo (X : Word) return Byte is
   begin
      return Byte (X mod 256);
   end Lo;

   procedure Inc (Val : in out Char) is
   begin
      Val := Char'Succ (Val);
   end Inc;

   --     procedure Inc (Val : in out Byte) is
   --     begin
   --        Val := Val + 1;
   --        null;
   --     end Inc;
   --
   --     procedure Inc (Val : in out Shortint) is
   --     begin
   --        Val := Val + 1;
   --        null;
   --     end Inc;
   --
   --     procedure Inc (Val : in out Word) is
   --     begin
   --        Val := Val + 1;
   --        null;
   --     end Inc;
   --
   --     procedure Inc (Val : in out Integer) is
   --     begin
   --        Val := Val + 1;
   --        null;
   --     end Inc;

   procedure Inc (Val : in out Longint) is
   begin
      Val := Val + 1;
   end Inc;

   procedure Inc (Val : in out Char; N : Longint) is
   begin
      Val := Char'Val (Char'Pos (Val) + N);
   end Inc;

   --     procedure Inc (Val : in out Byte; N : Longint) is
   --     begin
   --        Val := Val + Byte (N);
   --        null;
   --     end Inc;
   --
   --     procedure Inc (Val : in out Shortint; N : Longint) is
   --     begin
   --        Val := Val + Shortint (N);
   --        null;
   --     end Inc;
   --
   --     procedure Inc (Val : in out Word; N : Longint) is
   --     begin
   --        Val := Val + Word (N);
   --        null;
   --     end Inc;
   --
   --     procedure Inc (Val : in out Integer; N : Longint) is
   --     begin
   --        Val := Val + Integer (N);
   --        null;
   --     end Inc;

   procedure Inc (Val : in out Longint; N : Longint) is
   begin
      Val := Val + N;
      null;
   end Inc;

   function Succ (Val : in Char) return Char is
   begin
      return Char'Succ (Val);
   end Succ;

   --     function Succ (Val : in Byte) return Byte is
   --     begin
   --        return Val + 1;
   --     end Succ;
   --
   --     function Succ (Val : in Shortint) return Shortint is
   --     begin
   --        return Val + 1;
   --     end Succ;
   --
   --     function Succ (Val : in Word) return Word is
   --     begin
   --        return Val + 1;
   --     end Succ;
   --
   --     function Succ (Val : in Integer) return Integer is
   --     begin
   --        return Val + 1;
   --     end Succ;

   function Succ (Val : in Longint) return Longint is
   begin
      return Val + 1;
   end Succ;

   function Int (X : Real) return Real is
   begin
      return Real'Floor (X);
   end Int;

   procedure Move (Source, Dest : Pointer; Nombre : Word) is
      pragma Unreferenced (Source, Dest, Nombre);
   begin
      if Debug then
         Writeln ("La fonction Move n'est pas définie !");
      end if;
   end Move;

   --     function Odd (Val : in Byte) return Boolean is
   --     begin
   --        return (Val mod 2) = 1;
   --     end Odd;
   --
   --     function Odd (Val : in Shortint) return Boolean is
   --     begin
   --        return (Val mod 2) = 1;
   --     end Odd;
   --
   --     function Odd (Val : in Word) return Boolean is
   --     begin
   --        return (Val mod 2) = 1;
   --     end Odd;
   --
   --     function Odd (Val : in Integer) return Boolean is
   --     begin
   --        return (Val mod 2) = 1;
   --     end Odd;

   function Odd (Val : in Longint) return Boolean is
   begin
      return (Val mod 2) = 1;
   end Odd;

   function ParamCount return Word is
   begin
      return Word (Ada.Command_Line.Argument_Count);
   end ParamCount;

   function ParamStr (Indice : Word) return String is
   begin
      return To_TPString (Ada.Command_Line.Argument (Positive (Indice)));
   end ParamStr;

   function Ptr (Seg, Dep : Word) return Pointer is
      pragma Unreferenced (Dep, Seg);
   begin
      if Debug then
         Writeln ("La fonction Ptr n'est pas définie !");
      end if;
      return nil;
   end Ptr;

   package RandomWord is new Ada.Numerics.Discrete_Random (Word);
   IntWordGenerator : RandomWord.Generator;
   IntRealGenerator : Ada.Numerics.Float_Random.Generator;

   procedure Randomize is
   begin
      if RandSeed = 0 then
         RandomWord.Reset (IntWordGenerator);
         Ada.Numerics.Float_Random.Reset (IntRealGenerator);
      else
         RandomWord.Reset (IntWordGenerator, Standard.Integer (RandSeed));
         Ada.Numerics.Float_Random.Reset (IntRealGenerator, Standard.Integer (RandSeed));
      end if;
   end Randomize;

   function Random (ValMax : Word) return Word is
      ResultRandom : Word;
   begin
      if ValMax = 0 then
         ResultRandom := 0;
      else
         ResultRandom := RandomWord.Random (IntWordGenerator) mod ValMax;
      end if;
      return ResultRandom;
   end Random;

   function Random return Real is
   begin
      return Real (Ada.Numerics.Float_Random.Random (IntRealGenerator));
   end Random;

   procedure RunError is
   begin
      Writeln ("Exécution de RunError.");
      Halt (0);
   end RunError;

   procedure RunError (ErreurCode : Word) is
   begin
      Writeln ("Exécution de RunError : " + ErreurCode'Img);
      Halt (ErreurCode);
   end RunError;

   function Swap (X : Word) return Word is
   begin
      return (X mod 256) * 256 + X / 256;
   end Swap;

   function UpCase (Val : Char) return Char is
   begin
      return Ada.Characters.Handling.To_Upper (Val);
   end UpCase;

   function Pos (Substr, S : String) return Byte is
      Index : constant Natural := Ada.Strings.Fixed.Index (Substr, Null_TPString);
   begin
      if Index = 0 then
         return Ada.Strings.Fixed.Index (S, Substr);
      else
         return Ada.Strings.Fixed.Index (S, Substr (Substr'First .. Index - 1));
      end if;
   end Pos;

   function Round (X : Real) return Longint is
   begin
      return Longint (Real'Rounding (X));
   end Round;

   function Trunc (X : Real) return Longint is
   begin
      return Longint (Real'Truncation (X));
   end Trunc;

   function ArcTan (X : Real) return Real is
   begin
      return Ada.Numerics.Long_Long_Elementary_Functions.Arctan (X);
   end ArcTan;

   function Assigned (P : Pointer) return Boolean is
      use type Standard.System.Address;
   begin
      return P /= nil;
   end Assigned;

   procedure Val (S : String; V : out Longint; Erreur : out Integer) is
   begin
      V      := Longint'Value (To_String (S));
      Erreur := 0;
   exception
      when others =>
         V      := 0;
         Erreur := -1;
   end Val;

begin
   Input.Device  := Stdinout;
   Output.Device := Stdinout;
end TP7.System;
