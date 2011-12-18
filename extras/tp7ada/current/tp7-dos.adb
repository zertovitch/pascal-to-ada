-------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : tp7-dos.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 2.0b
-- DATE DE LA DERNIERE MISE A JOUR  : 16 octobre 2011
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
-- COPYRIGHT                        : (c) Pascal Pignard 2002-2011
-- LICENCE                          : CeCILL V2 (http://www.cecill.info)
-- CONTACT                          : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Calendar.Formatting;
with TP7.System;

package body TP7.Dos is

   function DosVersion return Word is
      ResultDosVersion : Word;
   begin
      declare
      begin
         if Debug then
            TP7.System.Writeln ("La fonction DosVersion n'est pas définie !");
         end if;
         ResultDosVersion := 0;
         null;
      end;
      return ResultDosVersion;
   end DosVersion;

   procedure Intr (IntNo : Byte; Regs : in out Registers) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction Intr n'est pas définie !");
      end if;
      null;
   end Intr;

   procedure MsDos (Regs : in out Registers) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction MsDos n'est pas définie !");
      end if;
      null;
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
      pragma Compile_Time_Warning (True, "SetDate not implemented"); -- TBF
      Date : Ada.Calendar.Time :=
         Ada.Calendar.Time_Of (Year, Month, Day, Ada.Calendar.Seconds (Ada.Calendar.Clock));
      pragma Unreferenced (Date);
   --  			Date: aliased DateTimeRec;
   begin
      --  	GetTime(Date'access);
      --  	Date.Year := Short_Integer(Year);
      --  	Date.Month := Short_Integer(Month);
      --  	Date.Day := Short_Integer(Day);
      --  	SetTime(Date'access);
      null;
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
      pragma Compile_Time_Warning (True, "SetDate not implemented"); -- TBF
      Today : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Date  : Ada.Calendar.Time          :=
         Ada.Calendar.Time_Of
           (Ada.Calendar.Year (Today),
            Ada.Calendar.Month (Today),
            Ada.Calendar.Day (Today),
            Hour * 3600.0 + Minute * 60.0 + Second * 1.0 + Duration (Sec100) / 100.0);
      pragma Unreferenced (Date);
   --  			Date: aliased DateTimeRec;
   begin
      --  	GetTime(Date'access);
      --  	Date.Hour := Short_Integer(Hour);
      --  	Date.Minute := Short_Integer(Minute);
      --  	Date.Second := Short_Integer(Second);
      --  	SetTime(Date'access);
      null;
   end SetTime;

   procedure GetCBreak (Break : out Boolean) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction GetCBreak n'est pas définie !");
      end if;
      Break := True;
      null;
   end GetCBreak;

   procedure SetCBreak (Break : Boolean) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetCBreak n'est pas définie !");
      end if;
      null;
   end SetCBreak;

   procedure GetVerify (Verify : out Boolean) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction GetVerify n'est pas définie !");
      end if;
      Verify := True;
      null;
   end GetVerify;

   procedure SetVerify (Verify : Boolean) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetVerify n'est pas définie !");
      end if;
      null;
   end SetVerify;

   function DiskFree (Drive : Byte) return Longint is
      ResultDiskFree : Longint;
   begin
      declare
      begin
         if Debug then
            TP7.System.Writeln ("La fonction CSeg n'est pas définie !");
         end if;
         ResultDiskFree := 0;
         null;
      end;
      return ResultDiskFree;
   end DiskFree;

   function DiskSize (Drive : Byte) return Longint is
      ResultDiskSize : Longint;
   begin
      declare
      begin
         if Debug then
            TP7.System.Writeln ("La fonction CSeg n'est pas définie !");
         end if;
         ResultDiskSize := 0;
         null;
      end;
      return ResultDiskSize;
   end DiskSize;

   procedure GetFAttr (F : in out File; Attr : out Word) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction CSeg n'est pas définie !");
      end if;
      null;
   end GetFAttr;

   procedure SetFAttr (F : in out File; Attr : Word) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetFAttr1 n'est pas définie !");
      end if;
      null;
   end SetFAttr;

   procedure GetFTime (F : in out File; Time : out Longint) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction CSeg n'est pas définie !");
      end if;
      null;
   end GetFTime;

   procedure SetFTime (F : in out File; Time : Longint) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction CSeg n'est pas définie !");
      end if;
      null;
   end SetFTime;

   procedure FindFirst (Path : PathStr; Attr : Word; F : in out SearchRec) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction CSeg n'est pas définie !");
      end if;
      null;
   end FindFirst;

   procedure FindNext (F : in out SearchRec) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction CSeg n'est pas définie !");
      end if;
      null;
   end FindNext;

   type IntDateTime is record
      Heure : Integer range 0 .. 31;
      Min   : Integer range 0 .. 63;
      Sec   : Integer range 0 .. 31;
      Annee : Integer range 0 .. 127;
      Mois  : Integer range 0 .. 15;
      Jour  : Integer range 0 .. 32;
   end record;

   procedure UnpackTime (P : Longint; T : in out DateTime) is
      Dum : IntDateTime;
   begin
      --Dum.Ext := P;
      T.Year  := Dum.Annee;
      T.Month := Dum.Mois;
      T.Day   := Dum.Jour;
      T.Hour  := Dum.Heure;
      T.Min   := Dum.Min;
      T.Sec   := Dum.Sec * 2;
      null;
   end UnpackTime;

   procedure PackTime (T : in out DateTime; P : in out Longint) is
      Dum : IntDateTime;
   begin
      Dum.Annee := T.Year;
      Dum.Mois  := T.Month;
      Dum.Jour  := T.Day;
      Dum.Heure := T.Hour;
      Dum.Min   := T.Min;
      Dum.Sec   := T.Sec / 2;
      --P := Dum.Ext;
      null;
   end PackTime;

   procedure GetIntVec (IntNo : Byte; Vector : in out Pointer) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction GetIntVec n'est pas définie !");
      end if;
      Vector := nil;
      null;
   end GetIntVec;

   procedure SetIntVec (IntNo : Byte; Vector : Pointer) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SetIntVec n'est pas définie !");
      end if;
      null;
   end SetIntVec;

   procedure SwapVectors is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction SwapVectors n'est pas définie !");
      end if;
      null;
   end SwapVectors;

   procedure Keep (ExitCode : Word) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction Keep n'est pas définie !");
      end if;
      null;
   end Keep;

   procedure Exec (Path : PathStr; ComLine : ComStr) is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction Exec n'est pas définie !");
      end if;
      null;
   end Exec;

   function DosExitCode return Word is
      ResultDosExitCode : Word;
   begin
      declare
      begin
         if Debug then
            TP7.System.Writeln ("La fonction DosExitCode n'est pas définie !");
         end if;
         ResultDosExitCode := 0;
         null;
      end;
      return ResultDosExitCode;
   end DosExitCode;

   function FSearch (Path : PathStr; DirList : String) return PathStr is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction CSeg n'est pas définie !");
      end if;
      return (others => ' ');
   end FSearch;

   function FExpand (Path : PathStr) return PathStr is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction CSeg n'est pas définie !");
      end if;
      return (others => ' ');
   end FExpand;

   procedure FSplit
     (Path : PathStr;
      Dir  : in out DirStr;
      Name : in out NameStr;
      Ext  : in out ExtStr)
   is
      Ind1, Ind2 : Integer;
   begin
      Ind1 := Path'Length;
      Ind2 := Ind1;
      -- 	while  (Ind2 > 0)  and  (Path(Ind2) /= '.')  and  (Path(Ind2) /=
      --':')  loop
      --
      -- 		Ind2 := Ind2 - 1;end loop;
      --
      -- 	if  Path(Ind2) = '.' then
      -- 		Ext := ExtStr(Path(Ind2 .. 1 + Ind1 - Ind2))
      -- 	;else
      -- 		Ext := (others => ' ');end if;
      -- 	if  Path(Ind2) /= ':' then
      --
      -- 		Ind1 := Ind2 - 1;
      -- 		while  (Ind2 > 0)  and  (Path(Ind2) /= ':')  loop
      --
      -- 			Ind2 := Ind2 - 1;end loop;
      --
      -- 		 null; end if;
      -- 	if  Ind2 /= Ind1 then
      -- 		Name := NameStr(Path(Ind2 + 1 .. 1 + Ind1 - Ind2))
      -- 	;else
      -- 		Name := (others => ' ');end if;
      -- 	Dir := DirStr(Path(1.. Ind1));
      null;
   end FSplit;

   function EnvCount return Integer is
      ResultEnvCount : Integer;
   begin
      declare
      begin
         if Debug then
            TP7.System.Writeln ("La fonction EnvCount n'est pas définie !");
         end if;
         ResultEnvCount := 0;
         null;
      end;
      return ResultEnvCount;
   end EnvCount;

   function EnvStr (Index : Integer) return String is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction EnvStr n'est pas définie !");
      end if;
      return "";
   end EnvStr;

   function GetEnv (EnvVar : String) return String is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction GetEnv n'est pas définie !");
      end if;
      return "";
   end GetEnv;

end TP7.Dos;
