-------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : tp7-dos.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 2.2a
-- DATE DE LA DERNIERE MISE A JOUR  : 12 mars 2012
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

with Ada.Calendar;
with Ada.Calendar.Formatting;
with TP7.System;
with GNAT.Directory_Operations.Iteration;
with Ada.Environment_Variables;
with Ada.Unchecked_Conversion;

package body TP7.Dos is

   function DosVersion return Word is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction DosVersion n'est pas définie !");
      end if;
      return 0;
   end DosVersion;

   procedure Intr (IntNo : Byte; Regs : in out Registers) is
      pragma Unreferenced (Regs, IntNo);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction Intr n'est pas définie !");
      end if;
   end Intr;

   procedure MsDos (Regs : in out Registers) is
      pragma Unreferenced (Regs);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction MsDos n'est pas définie !");
      end if;
   end MsDos;

   procedure GetDate (Year, Month, Day, DayOfWeek : out Word) is
      Today      : constant Ada.Calendar.Time                := Ada.Calendar.Clock;
      Today_Name : constant Ada.Calendar.Formatting.Day_Name :=
        Ada.Calendar.Formatting.Day_Of_Week (Today);
   begin
      Year  := Word (Ada.Calendar.Year (Today));
      Month := Word (Ada.Calendar.Month (Today));
      Day   := Word (Ada.Calendar.Day (Today));
      case Today_Name is
         when Ada.Calendar.Formatting.Monday =>
            DayOfWeek := 1;
         when Ada.Calendar.Formatting.Tuesday =>
            DayOfWeek := 2;
         when Ada.Calendar.Formatting.Wednesday =>
            DayOfWeek := 3;
         when Ada.Calendar.Formatting.Thursday =>
            DayOfWeek := 4;
         when Ada.Calendar.Formatting.Friday =>
            DayOfWeek := 5;
         when Ada.Calendar.Formatting.Saturday =>
            DayOfWeek := 6;
         when Ada.Calendar.Formatting.Sunday =>
            DayOfWeek := 0;
      end case;
   end GetDate;

   procedure SetDate (Year, Month, Day : Word) is
      pragma Unreferenced (Day, Month, Year);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetDate n'est pas définie !");
      end if;
   end SetDate;

   procedure GetTime (Hour, Minute, Second, Sec100 : out Word) is
      Seconds_Of_Day : constant Duration := Ada.Calendar.Seconds (Ada.Calendar.Clock);
      IntSec         : constant Natural  := Natural (Seconds_Of_Day);
      FracSec        : constant Real     := Real'Fraction (Real (Seconds_Of_Day));
   begin
      Hour   := Word (IntSec / 3600);
      Minute := Word ((IntSec - Hour * 3600) / 60);
      Second := Word (IntSec - Hour * 3600 - Minute * 60);
      Sec100 := Word (FracSec * 100.0);
   end GetTime;

   procedure SetTime (Hour, Minute, Second, Sec100 : Word) is
      pragma Unreferenced (Sec100, Second, Minute, Hour);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetTime n'est pas définie !");
      end if;
   end SetTime;

   procedure GetCBreak (Break : out Boolean) is
   begin
      Break := False;
   end GetCBreak;

   procedure SetCBreak (Break : Boolean) is
      pragma Unreferenced (Break);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetCBreak n'est pas définie !");
      end if;
   end SetCBreak;

   procedure GetVerify (Verify : out Boolean) is
   begin
      Verify := False;
   end GetVerify;

   procedure SetVerify (Verify : Boolean) is
      pragma Unreferenced (Verify);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetVerify n'est pas définie !");
      end if;
   end SetVerify;

   function DiskFree (Drive : Byte) return Longint is
      pragma Unreferenced (Drive);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction DiskFree n'est pas définie !");
      end if;
      return -1;
   end DiskFree;

   function DiskSize (Drive : Byte) return Longint is
      pragma Unreferenced (Drive);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction DiskSize n'est pas définie !");
      end if;
      return -1;
   end DiskSize;

   procedure GetFAttr (F : File; Attr : out Word1) is
   begin
      Attr := 0;
      if GNAT.OS_Lib.Is_Directory (F.Name.all) then
         Attr := Attr or Directory;
      end if;
      if GNAT.OS_Lib.Is_Readable_File (F.Name.all)
        and then not GNAT.OS_Lib.Is_Writable_File (F.Name.all)
      then
         Attr := Attr or ReadOnly;
      end if;
   end GetFAttr;

   procedure SetFAttr (F : File; Attr : Word1) is
      pragma Unreferenced (Attr, F);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetFAttr n'est pas définie !");
      end if;
   end SetFAttr;

   procedure GetFTime (F : File; Time : out Longint) is
      Y, Mo, D, H, Mi, S : Standard.Integer;
      use type GNAT.OS_Lib.File_Descriptor;
   begin
      if F.File /= 0 then
         GNAT.OS_Lib.GM_Split (GNAT.OS_Lib.File_Time_Stamp (F.File), Y, Mo, D, H, Mi, S);
         PackTime ((Y, Mo, D, H, Mi, S), Time);
      else
         Time := 0;
      end if;
   end GetFTime;

   procedure SetFTime (F : File; Time : Longint) is
      pragma Unreferenced (Time, F);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetFTime n'est pas définie !");
      end if;
   end SetFTime;

   procedure FillSearchRec (F : in out SearchRec) is
      use type Ada.Containers.Count_Type;
   begin
      if F.Fill.Length > 0 then
         declare
            Item               : constant String :=
              Ada.Strings.Unbounded.To_String (F.Fill.Element (1));
            Y, Mo, D, H, Mi, S : Standard.Integer;
         begin
            F.Attr := 0; -- Not set at the moment without openning the file
            GNAT.OS_Lib.GM_Split (GNAT.OS_Lib.File_Time_Stamp (Item), Y, Mo, D, H, Mi, S);
            PackTime ((Y, Mo, D, H, Mi, S), F.Time);
            F.Size := 0; -- Not set at the moment without openning the file
            Assign_String (F.Name, GNAT.Directory_Operations.Base_Name (Item));
            F.Fill.Delete_First;
            DosError := deOk;
         end;
      else
         DosError := deNoMoreFiles;
      end if;
   end FillSearchRec;

   procedure FindFirst (Path : PathStr; Attr : Word1; F : out SearchRec) is
      pragma Unreferenced (Attr); -- Not used at the moment
      procedure Action (Item : String; Index : Positive; Quit : in out Boolean) is
         pragma Unreferenced (Quit, Index);
      begin
         F.Fill.Append (Ada.Strings.Unbounded.To_Unbounded_String (Item));
      end Action;
      procedure Find is new GNAT.Directory_Operations.Iteration.Wildcard_Iterator (Action);
   begin
      Find (To_String (Path));
      FillSearchRec (F);
   end FindFirst;

   procedure FindNext (F : in out SearchRec) is
   begin
      FillSearchRec (F);
   end FindNext;

   type TimePacked is record
      Year  : Word range 1980 .. 2107;
      Month : Word range 1 .. 12;
      Day   : Word range 1 .. 31;
      Hour  : Word range 0 .. 23;
      Min   : Word range 0 .. 59;
      Sec2  : Word range 0 .. 29; -- double of seconds
   end record;
   for TimePacked use record
      Sec2  at 0 range 0 .. 4;
      Min   at 0 range 5 .. 10;
      Hour  at 0 range 11 .. 15;
      Day   at 0 range 16 .. 20;
      Month at 0 range 21 .. 24;
      Year  at 0 range 25 .. 31;
   end record;
   for TimePacked'Size use 32;
   function To_TimePacked is new Ada.Unchecked_Conversion (Longint, TimePacked);
   function To_Longint is new Ada.Unchecked_Conversion (TimePacked, Longint);

   procedure UnpackTime (P : Longint; T : out DateTime) is
      Dum : constant TimePacked := To_TimePacked (P);
   begin
      T :=
        (Sec   => Dum.Sec2 * 2,
         Min   => Dum.Min,
         Hour  => Dum.Hour,
         Day   => Dum.Day,
         Month => Dum.Month,
         Year  => Dum.Year);
   end UnpackTime;

   procedure PackTime (T : DateTime; P : out Longint) is
      Dum : constant TimePacked :=
        (Sec2  => T.Sec / 2,
         Min   => T.Min,
         Hour  => T.Hour,
         Day   => T.Day,
         Month => T.Month,
         Year  => T.Year);
   begin
      P := To_Longint (Dum);
   end PackTime;

   procedure GetIntVec (IntNo : Byte; Vector : out Pointer) is
      pragma Unreferenced (IntNo);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction GetIntVec n'est pas définie !");
      end if;
      Vector := nil;
   end GetIntVec;

   procedure SetIntVec (IntNo : Byte; Vector : Pointer) is
      pragma Unreferenced (Vector, IntNo);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetIntVec n'est pas définie !");
      end if;
   end SetIntVec;

   procedure SwapVectors is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SwapVectors n'est pas définie !");
      end if;
   end SwapVectors;

   procedure Keep (ExitCode : Word) is
      pragma Unreferenced (ExitCode);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction Keep n'est pas définie !");
      end if;
   end Keep;

   procedure Exec (Path : PathStr; ComLine : ComStr) is
      Args : GNAT.OS_Lib.String_List_Access :=
        GNAT.OS_Lib.Argument_String_To_List (To_String (ComLine));
   begin
      DosError := GNAT.OS_Lib.Spawn (To_String (Path), Args.all);
      GNAT.OS_Lib.Free (Args);
   end Exec;

   function DosExitCode return Word is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction DosExitCode n'est pas définie !");
      end if;
      return 0;
   end DosExitCode;

   function FSearch (Path : PathStr; DirList : String) return PathStr is
      NameAccess : GNAT.OS_Lib.String_Access :=
        GNAT.OS_Lib.Locate_Regular_File (To_String (Path), To_String (DirList));
      use type GNAT.OS_Lib.String_Access;
   begin
      if NameAccess /= null then
         declare
            Name : constant PathStr :=
              To_TPString
                 (PathStr'Length - 1,
                  GNAT.OS_Lib.Locate_Regular_File (To_String (Path), To_String (DirList)).all);
         begin
            GNAT.OS_Lib.Free (NameAccess);
            return Name;
         end;
      else
         return To_TPString (PathStr'Length - 1, "");
      end if;
   end FSearch;

   function FExpand (Path : PathStr) return PathStr is
   begin
      return To_TPString (PathStr'Length - 1, GNAT.OS_Lib.Normalize_Pathname (To_String (Path)));
   end FExpand;

   procedure FSplit
     (Path : PathStr;
      Dir  : out DirStr;
      Name : out NameStr;
      Ext  : out ExtStr)
   is
      Ind1, Ind2 : Integer := TP7.System.Length (Path);
   begin
      while Ind2 > 0 and then Path (Ind2) /= '.' and then Path (Ind2) /= '/' loop
         Ind2 := Ind2 - 1;
      end loop;
      if Ind2 > 0 and then Path (Ind2) = '.' then
         Assign_String (Ext, Path (Ind2 .. Ind1));
         Ind1 := Ind2 - 1;
      else
         Assign_String (Ext, Null_TPString);
      end if;
      if Ind2 > 0 and then Path (Ind2) /= '/' then
         while Ind2 > 0 and then Path (Ind2) /= '/' loop
            Ind2 := Ind2 - 1;
         end loop;
      end if;
      if Ind2 /= Ind1 then
         Assign_String (Name, Path (Ind2 + 1 .. Ind1));
      else
         Assign_String (Name, Null_TPString);
      end if;
      Assign_String (Dir, Path (1 .. Ind2));
   end FSplit;

   package Environment_Vector is new Ada.Containers.Vectors (
      Positive,
      Ada.Strings.Unbounded.Unbounded_String,
      Ada.Strings.Unbounded."=");

   Environment_Pairs : Environment_Vector.Vector;

   function EnvCount return Integer is
   begin
      return Integer (Environment_Pairs.Length);
   end EnvCount;

   function EnvStr (Index : Integer) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Environment_Pairs.Element (Index));
   end EnvStr;

   function GetEnv (EnvVar : String) return String is
   begin
      return To_TPString (Ada.Environment_Variables.Value (To_String (EnvVar)));
   end GetEnv;

   procedure Add (Name, Value : String) is
      use Environment_Vector, Ada.Strings.Unbounded;
   begin
      Environment_Pairs.Append (To_Unbounded_String (Name & '=' & Value));
   end Add;

begin
   Ada.Environment_Variables.Iterate (Add'Access);
end TP7.Dos;
