-------------------------------------------------------------------------------
-- NOM DU CSU (spécification)       : tp7-dos.ads
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
-- Based on:
--*******************************************************
--
--       Turbo Pascal Version 7.0
--       DOS Interface Unit
--
--       Copyright (C) 1988,92 Borland International
--
--*******************************************************
-- Pascal to Ada translation by Pascal Pignard August 2002

--$D-,I-,S-,O+

private with Ada.Strings.Unbounded;
private with Ada.Containers.Vectors;

package TP7.Dos is

   subtype Integer is TPInteger;

   -- Flags bit masks

   FCarry     : constant := 16#0001#;
   FParity    : constant := 16#0004#;
   FAuxiliary : constant := 16#0010#;
   FZero      : constant := 16#0040#;
   FSign      : constant := 16#0080#;
   FOverflow  : constant := 16#0800#;

   -- File mode magic numbers

   fmClosed : constant := 16#D7B0#;
   fmInput  : constant := 16#D7B1#;
   fmOutput : constant := 16#D7B2#;
   fmInOut  : constant := 16#D7B3#;

   -- File attribute constants

   ReadOnly  : constant := 16#01#;
   Hidden    : constant := 16#02#;
   SysFile   : constant := 16#04#;
   VolumeID  : constant := 16#08#;
   Directory : constant := 16#10#;
   Archive   : constant := 16#20#;
   AnyFile   : constant := 16#3F#;

   -- String types

   subtype ComStr is String (1 .. 127 + 1);        -- Command line string
   subtype PathStr is String (1 .. 79 + 1);        -- File pathname string
   subtype DirStr is String (1 .. 67 + 1);         -- Drive and directory string
   subtype NameStr is String (1 .. 8 + 1);         -- File name string
   subtype ExtStr is String (1 .. 4 + 1);          -- File extension string

   -- Registers record used by Intr and MsDos

   type Registers (Variant : Boolean := False) is record
      case Variant is
         when False =>
            AX, BX, CX, DX, BP, SI, DI, DS, ES, Flags : Word;
         when True =>
            AL, AH, BL, BH, CL, CH, DL, DH : Byte;
      end case;
   end record;

   -- Typed-file and untyped-file record

   type TData is array (Positive range <>) of Byte1;
   type TName is array (0 .. 79) of Char;
   type FileRec is record
      Handle   : Word;
      Mode     : Word;
      RecSize  : Word;
      Private1 : TData (1 .. 26); -- array (1..26) of Byte;
      UserData : TData (1 .. 16); -- array (1..16) of Byte;
      Name     : TName; -- array (0..79) of Char ;
   end record;

   -- Textfile record

   type TextBuf is array (0 .. 127) of Char;
   type TBufPtr is access TextBuf;
   type TextRec is record
      Handle    : Word;
      Mode      : Word;
      BufSize   : Word;
      Private1  : Word;
      BufPos    : Word;
      BufEnd    : Word;
      BufPtr    : TBufPtr; -- access TextBuf;
      OpenFunc  : Pointer;
      InOutFunc : Pointer;
      FlushFunc : Pointer;
      CloseFunc : Pointer;
      UserData  : TData (1 .. 16); -- array (1..16) of Byte;
      Name      : TName; -- array (0..79) of Char ;
      Buffer    : TextBuf;
   end record;

   -- Search record used by FindFirst and FindNext

   type TFill is private;
   type SearchRec is record
      Fill : TFill; -- array (1..21) of  Byte;
      Attr : Byte1;
      Time : Longint;
      Size : Longint;
      Name : String (1 .. 12 + 1);
   end record;

   -- Date and time record used by PackTime and UnpackTime

   type DateTime is record
      Year, Month, Day, Hour, Min, Sec : Word;
   end record;

   -- Error status constants and variable

   deOk                 : constant := 0; -- No error
   deFileNotFound       : constant := 2; -- File not found
   dePathNotFound       : constant := 3; -- Path not found
   deAccessDenied       : constant := 5; -- Access denied
   deInvalidFileHhandle : constant := 6; -- Invalid file handle
   deNotEnoughMemory    : constant := 8; -- Not enough memory
   deInvalidEnvironment : constant := 10; -- Invalid environment
   deInvalidFormat      : constant := 11; -- Invalid format
   deNoMoreFiles        : constant := 18; -- No more files

   DosError : Integer := 0;

   -- DosVersion returns the DOS version number. The low byte of
   -- the result is the major version number, and the high byte is
   -- the minor version number. For example, DOS 3.20 returns 3 in
   -- the low byte, and 20 in the high byte.
   function DosVersion return Word;

   -- Intr executes a specified software interrupt with a specified
   -- Registers package.
   procedure Intr (IntNo : Byte; Regs : in out Registers);

   -- MsDos invokes the DOS function call handler with a specified
   -- Registers package.
   procedure MsDos (Regs : in out Registers);

   -- GetDate returns the current date set in the operating system.
   -- Ranges of the values returned are: Year 1980-2099, Month
   -- 1-12, Day 1-31 and DayOfWeek 0-6 (0 corresponds to Sunday).
   procedure GetDate (Year, Month, Day, DayOfWeek : out Word);

   -- SetDate sets the current date in the operating system. Valid
   -- parameter ranges are: Year 1980-2099, Month 1-12 and Day
   -- 1-31. If the date is not valid, the function call is ignored.
   procedure SetDate (Year, Month, Day : Word);

   -- GetTime returns the current time set in the operating system.
   -- Ranges of the values returned are: Hour 0-23, Minute 0-59,
   -- Second 0-59 and Sec100 (hundredths of seconds) 0-99.
   procedure GetTime (Hour, Minute, Second, Sec100 : out Word);

   -- SetTime sets the time in the operating system. Valid
   -- parameter ranges are: Hour 0-23, Minute 0-59, Second 0-59 and
   -- Sec100 (hundredths of seconds) 0-99. If the time is not
   -- valid, the function call is ignored.
   procedure SetTime (Hour, Minute, Second, Sec100 : Word);

   -- GetCBreak returns the state of Ctrl-Break checking in DOS.
   -- When off (False), DOS only checks for Ctrl-Break during I/O
   -- to console, printer, or communication devices. When on
   -- (True), checks are made at every system call.
   procedure GetCBreak (Break : out Boolean);

   -- SetCBreak sets the state of Ctrl-Break checking in DOS.
   procedure SetCBreak (Break : Boolean);

   -- GetVerify returns the state of the verify flag in DOS. When
   -- off (False), disk writes are not verified. When on (True),
   -- all disk writes are verified to insure proper writing.
   procedure GetVerify (Verify : out Boolean);

   -- SetVerify sets the state of the verify flag in DOS.
   procedure SetVerify (Verify : Boolean);

   -- DiskFree returns the number of free bytes on the specified
   -- drive number (0=Default,1=A,2=B,..). DiskFree returns -1 if
   -- the drive number is invalid.
   function DiskFree (Drive : Byte) return Longint;

   -- DiskSize returns the size in bytes of the specified drive
   -- number (0=Default,1=A,2=B,..). DiskSize returns -1 if the
   -- drive number is invalid.
   function DiskSize (Drive : Byte) return Longint;

   -- GetFAttr returns the attributes of a file. F must be a file
   -- variable (typed, untyped or textfile) which has been assigned
   -- a name. The attributes are examined by ANDing with the
   -- attribute masks defined as constants above. Errors are
   -- reported in DosError.
   procedure GetFAttr (F : File; Attr : out Word1);

   -- SetFAttr sets the attributes of a file. F must be a file
   -- variable (typed, untyped or textfile) which has been assigned
   -- a name. The attribute value is formed by adding (or ORing)
   -- the appropriate attribute masks defined as constants above.
   -- Errors are reported in DosError.
   procedure SetFAttr (F : File; Attr : Word1);

   -- GetFTime returns the date and time a file was last written.
   -- F must be a file variable (typed, untyped or textfile) which
   -- has been assigned and opened. The Time parameter may be
   -- unpacked throgh a call to UnpackTime. Errors are reported in
   -- DosError.
   procedure GetFTime (F : File; Time : out Longint);

   -- SetFTime sets the date and time a file was last written.
   -- F must be a file variable (typed, untyped or textfile) which
   -- has been assigned and opened. The Time parameter may be
   -- created through a call to PackTime. Errors are reported in
   -- DosError.
   procedure SetFTime (F : File; Time : Longint);

   -- FindFirst searches the specified (or current) directory for
   -- the first entry that matches the specified filename and
   -- attributes. The result is returned in the specified search
   -- record. Errors (and no files found) are reported in DosError.
   procedure FindFirst (Path : PathStr; Attr : Word1; F : out SearchRec);

   -- FindNext returs the next entry that matches the name and
   -- attributes specified in a previous call to FindFirst. The
   -- search record must be one passed to FindFirst. Errors (and no
   -- more files) are reported in DosError.
   procedure FindNext (F : in out SearchRec);

   -- UnpackTime converts a 4-byte packed date/time returned by
   -- FindFirst, FindNext or GetFTime into a DateTime record.
   procedure UnpackTime (P : Longint; T : out DateTime);

   -- PackTime converts a DateTime record into a 4-byte packed
   -- date/time used by SetFTime.
   procedure PackTime (T : DateTime; P : out Longint);

   -- GetIntVec returns the address stored in the specified
   -- interrupt vector.
   procedure GetIntVec (IntNo : Byte; Vector : out Pointer);

   -- SetIntVec sets the address in the interrupt vector table for
   -- the specified interrupt.
   procedure SetIntVec (IntNo : Byte; Vector : Pointer);

   -- FSearch searches for the file given by Path in the list of
   -- directories given by DirList. The directory paths in DirList
   -- must be separated by semicolons. The search always starts
   -- with the current directory of the current drive. The returned
   -- value is a concatenation of one of the directory paths and
   -- the file name, or an empty string if the file could not be
   -- located.
   function FSearch (Path : PathStr; DirList : String) return PathStr;

   -- FExpand expands the file name in Path into a fully qualified
   -- file name. The resulting name consists of a drive letter, a
   -- colon, a root relative directory path, and a file name.
   -- Embedded '.' and '..' directory references are removed.
   function FExpand (Path : PathStr) return PathStr;

   -- FSplit splits the file name specified by Path into its three
   -- components. Dir is set to the drive and directory path with
   -- any leading and trailing backslashes, Name is set to the file
   -- name, and Ext is set to the extension with a preceding dot.
   -- Each of the component strings may possibly be empty, if Path
   -- contains no such component.
   procedure FSplit
     (Path : PathStr;
      Dir  : out DirStr;
      Name : out NameStr;
      Ext  : out ExtStr);

   -- EnvCount returns the number of strings contained in the DOS
   -- environment.
   function EnvCount return Integer;

   -- EnvStr returns a specified environment string. The returned
   -- string is of the form "VAR=VALUE". The index of the first
   -- string is one. If Index is less than one or greater than
   -- EnvCount, EnvStr returns an empty string.
   function EnvStr (Index : Integer) return String;

   -- GetEnv returns the value of a specified environment variable.
   -- The variable name can be in upper or lower case, but it must
   -- not include the '=' character. If the specified environment
   -- variable does not exist, GetEnv returns an empty string.
   function GetEnv (EnvVar : String) return String;

   -- SwapVectors swaps the contents of the SaveIntXX pointers in
   -- the System unit with the current contents of the interrupt
   -- vectors. SwapVectors is typically called just before and just
   -- after a call to Exec. This insures that the Exec'd process
   -- does not use any interrupt handlers installed by the current
   -- process, and vice versa.
   procedure SwapVectors;

   -- Keep (or Terminate Stay Resident) terminates the program and
   -- makes it stay in memory. The entire program stays in memory,
   -- including data segment, stack segment, and heap. The ExitCode
   -- corresponds to the one passed to the Halt standard procedure.
   procedure Keep (ExitCode : Word);

   -- Exec executes another program. The program is specified by
   -- the Path parameter, and the command line is specified by the
   -- CmdLine parameter. To execute a DOS internal command, run
   -- COMMAND.COM, e.g. "Exec('\COMMAND.COM','/C DIR *.PAS');".
   -- Note the /C in front of the command. Errors are reported in
   -- DosError. When compiling a program that uses Exec, be sure
   -- to specify a maximum heap size as there will otherwise not be
   -- enough memory.
   procedure Exec (Path : PathStr; ComLine : ComStr);

   -- DosExitCode returns the exit code of a sub-process. The low
   -- byte is the code sent by the terminating process. The high
   -- byte is zero for normal termination, 1 if terminated by
   -- Ctrl-C, 2 if terminated due to a device error, or 3 if
   -- terminated by the Keep procedure (function call 31 hex).
   function DosExitCode return Word;

private
   package FileVector is new Ada.Containers.Vectors (
      Positive,
      Ada.Strings.Unbounded.Unbounded_String,
      Ada.Strings.Unbounded."=");
   type TFill is new FileVector.Vector with null record;
   --     for TFill'Size use 21*8;
end TP7.Dos;
