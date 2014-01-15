-------------------------------------------------------------------------------
-- NOM DU CSU (spécification)       : tp7-system.ads
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 2.3a
-- DATE DE LA DERNIERE MISE A JOUR  : 4 mai 2012
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
--       System Unit
--
--       Copyright (C) 1988,92 Borland International
--
--*******************************************************

-- Pascal to Ada translation by Pascal Pignard August 2002

private with Ada.Numerics.Long_Long_Elementary_Functions;

package TP7.System is
   pragma Elaborate_Body;

   subtype Integer is TPInteger;

   OvrCodeList  : Word    := 0;        -- Overlay code segment list
   OvrHeapSize  : Word    := 0; -- Initial overlay buffer size
   OvrDebugPtr  : Pointer := nil; -- Overlay debugger hook
   OvrHeapOrg   : Word    := 0;  -- Overlay buffer origin
   OvrHeapPtr   : Word    := 0;  -- Overlay buffer pointer
   OvrHeapEnd   : Word    := 0;  -- Overlay buffer end
   OvrLoadList  : Word    := 0; -- Loaded overlays list
   OvrDosHandle : Word    := 0; -- Overlay DOS handle
   OvrEmsHandle : Word    := 16#FFFF#; -- Overlay EMS handle
   HeapOrg      : Pointer := nil; -- Heap origin
   HeapPtr      : Pointer := nil; -- Heap pointer
   HeapEnd      : Pointer := nil; -- Heap end
   FreeList     : Pointer := nil; -- Free list pointer
   FreeZero     : Pointer := nil; -- Must be zero
   HeapError    : Pointer := nil;     -- Heap error function
   ExitProc     : TPProc  := null; -- Exit procedure
   ExitCode     : Integer := 0; -- Exit code
   ErrorAddr    : Pointer := nil; -- Runtime error address
   PrefixSeg    : Word    := 0;  -- Program segment prefix
   StackLimit   : Word    := 0;  -- Stack pointer low limit
   InOutRes     : Integer := 0; -- I/O result buffer
   RandSeed     : Longint := 0; -- Random seed
   FileMode     : Byte    := 2;  -- File open mode
   Test8087     : Byte    := 0;  -- 8087 test result

   Input  : Text;   -- Input standard file
   Output : Text;   -- Output standard file

   SaveInt00 : Pointer := nil;  -- Saved interrupt $00
   SaveInt02 : Pointer := nil;  -- Saved interrupt $02
   SaveInt09 : Pointer := nil;  -- Saved interrupt $09
   SaveInt1B : Pointer := nil;  -- Saved interrupt $1B
   SaveInt21 : Pointer := nil;  -- Saved interrupt $21
   SaveInt23 : Pointer := nil;  -- Saved interrupt $23
   SaveInt24 : Pointer := nil;  -- Saved interrupt $24
   SaveInt34 : Pointer := nil;  -- Saved interrupt $34
   SaveInt35 : Pointer := nil;  -- Saved interrupt $35
   SaveInt36 : Pointer := nil;  -- Saved interrupt $36
   SaveInt37 : Pointer := nil;  -- Saved interrupt $37
   SaveInt38 : Pointer := nil;  -- Saved interrupt $38
   SaveInt39 : Pointer := nil;  -- Saved interrupt $39
   SaveInt3A : Pointer := nil;  -- Saved interrupt $3A
   SaveInt3B : Pointer := nil;  -- Saved interrupt $3B
   SaveInt3C : Pointer := nil;  -- Saved interrupt $3C
   SaveInt3D : Pointer := nil;  -- Saved interrupt $3D
   SaveInt3E : Pointer := nil;  -- Saved interrupt $3E
   SaveInt3F : Pointer := nil;  -- Saved interrupt $3F
   SaveInt75 : Pointer := nil;  -- Saved interrupt $75

   SelectorInc : Word := 16#1000#; -- Selector increment
   Seg0040     : Word := 16#0040#; -- Selector for segment $0040
   SegA000     : Word := 16#A000#; -- Selector for segment $A000
   SegB000     : Word := 16#B000#; -- Selector for segment $B000
   SegB800     : Word := 16#B800#; -- Selector for segment $B800

   procedure Halt (CodeSortie : Word);
   procedure Halt;
   procedure RunError;
   procedure RunError (ErreurCode : Word);

   function Chr (V : Byte) return Char;
   function Ord (C : Char) return Longint;
   function Round (X : Real) return Longint;
   function Trunc (X : Real) return Longint;
   function Abs1 (V : Real) return Real;
   function Abs1 (V : Longint) return Longint;
   function ArcTan (X : Real) return Real;
   function Cos (X : Real) return Real;
   function Exp (V : Real) return Real;
   function Frac (X : Real) return Real;
   function Int (X : Real) return Real;
   function Ln (V : Real) return Real;
   Pi : constant Real;
   function Sin (X : Real) return Real;
   function Sqr (V : Real) return Real;
   function Sqrt (V : Real) return Real;

   procedure Dec (Val : in out Char);
   --     procedure Dec (Val : in out Byte);
   --     procedure Dec (Val : in out Shortint);
   --     procedure Dec (Val : in out Word);
   --     procedure Dec (Val : in out Integer);
   procedure Dec (Val : in out Longint);

   procedure Dec (Val : in out Char; N : Longint);
   --     procedure Dec (Val : in out Byte; N : Longint);
   --     procedure Dec (Val : in out Shortint; N : Longint);
   --     procedure Dec (Val : in out Word; N : Longint);
   --     procedure Dec (Val : in out Integer; N : Longint);
   procedure Dec (Val : in out Longint; N : Longint);
   procedure Inc (Val : in out Char);
   --     procedure Inc (Val : in out Byte);
   --     procedure Inc (Val : in out Shortint);
   --     procedure Inc (Val : in out Word);
   --     procedure Inc (Val : in out Integer);
   procedure Inc (Val : in out Longint);
   procedure Inc (Val : in out Char; N : Longint);
   --     procedure Inc (Val : in out Byte; N : Longint);
   --     procedure Inc (Val : in out Shortint; N : Longint);
   --     procedure Inc (Val : in out Word; N : Longint);
   --     procedure Inc (Val : in out Integer; N : Longint);
   procedure Inc (Val : in out Longint; N : Longint);
   --     function Odd (Val : in Byte) return Boolean;
   --     function Odd (Val : in Shortint) return Boolean;
   --     function Odd (Val : in Word) return Boolean;
   --     function Odd (Val : in Integer) return Boolean;
   function Odd (Val : in Longint) return Boolean;
   function Pred (Val : in Char) return Char;
   --     function Pred (Val : in Byte) return Byte;
   --     function Pred (Val : in Shortint) return Shortint;
   --     function Pred (Val : in Word) return Word;
   --     function Pred (Val : in Integer) return Integer;
   function Pred (Val : Longint) return Longint;
   function Succ (Val : in Char) return Char;
   --     function Succ (Val : in Byte) return Byte;
   --     function Succ (Val : in Shortint) return Shortint;
   --     function Succ (Val : in Word) return Word;
   --     function Succ (Val : in Integer) return Integer;
   function Succ (Val : in Longint) return Longint;

   function Concat (S1, S2 : String) return String;
   function Copy (S : String; Pos, Len : Integer) return String;
   procedure Delete (S : in out String; Pos, Len : Integer);
   procedure Insert (Src : String; Dest : in out String; Pos : Integer);
   function Length (S : String) return Integer;
   function Pos (Substr, S : String) return Byte;
   procedure Val (S : String; V : out Longint; Erreur : out Integer);

   procedure FreeMem (P : in out Pointer; Taille : Word);
   procedure GetMem (P : out Pointer; Taille : Word);
   function MaxAvail return Longint;
   function MemAvail return Longint;
   function Assigned (P : Pointer) return Boolean;
   function CSeg return Word;
   function DSeg return Word;
   function Ofs (X : Pointer) return Word;
   function Ptr (Seg, Dep : Word) return Pointer;
   function Seg (X : Pointer) return Word;
   function SPtr return Word;
   function SSeg return Word;

   procedure FillChar (X : out String; Nombre : Word; Ch : Char);
   type TTabByte is array (Positive range <>) of Byte1;
   procedure FillChar (X : out TTabByte; Nombre : Word; Val : Byte);
   function Hi (X : Word) return Byte;
   function Lo (X : Word) return Byte;
   procedure Move (Source, Dest : Pointer; Nombre : Word);

   function ParamCount return Word;
   function ParamStr (Indice : Word) return String;

   function Random (ValMax : Word) return Word;
   function Random return Real;
   procedure Randomize;

   function Swap (X : Word) return Word;

   function UpCase (Val : Char) return Char;

   procedure Append (F : in out Text);
   procedure Assign (F : out Text; Name : String);
   procedure Reset (F : in out Text);
   procedure Rewrite (F : in out Text);
   procedure Close (F : in out Text);
   function Eof (F : Text) return Boolean;
   function Eof return Boolean;
   function Eoln (F : Text) return Boolean;
   function Eoln return Boolean;
   function SeekEof (F : Text) return Boolean;
   function SeekEoln (F : Text) return Boolean;
   procedure Flush (F : Text);

   procedure Assign (F : out File; Name : String);
   procedure BlockRead (F : in out File; Buf : Pointer; Quanti : Word);
   procedure BlockRead
     (F        : in out File;
      Buf      : Pointer;
      Quanti   : Word;
      Resultat : out Word);
   procedure BlockWrite (F : in out File; Buf : Pointer; Quanti : Word);
   procedure BlockWrite
     (F        : in out File;
      Buf      : Pointer;
      Quanti   : Word;
      Resultat : out Word);
   procedure Close (F : in out File);
   function Eof (F : File) return Boolean;
   procedure Erase (F : in out File);
   function FilePos (F : File) return Longint;
   function FileSize (F : File) return Longint;
   procedure Rename (F : in out File; NewName : String);
   procedure Reset (F : in out File; TailleRec : Word := 0);
   procedure Rewrite (F : in out File; TailleRec : Word := 0);
   procedure Seek (F : in out File; N : Longint);
   procedure SetTextBuf (F : Text; Buffer : Pointer; Size : Word := 0);
   procedure Truncate (F : in out Text);

   function IOResult return Integer;

   procedure Write (S : String);
   procedure Write (F : Text; S : String);
   procedure Write (I : Integer; MinWidth : Integer := 0);
   procedure Write (R : Real; MinWidth : Integer := 0; DecPlaces : Integer := 0);
   procedure Writeln (S : String);
   procedure Writeln (F : Text; S : String);
   procedure Writeln;
   procedure Writeln (F : Text);
   procedure Read (C : out Char);
   procedure Read (F : Text; C : out Char);
   procedure Read (S : out String);
   procedure Read (F : Text; S : out String);
   procedure Read (F : Text; I : out Integer);
   procedure Readln (C : out Char);
   procedure Readln (F : Text; C : out Char);
   procedure Readln (S : out String);
   procedure Readln (F : Text; S : out String);
   procedure Readln (I : out Integer);
   procedure Readln (R : out Real);
   procedure Readln;
   procedure Readln (F : Text);

   procedure ChDir (S : String);
   procedure GetDir (D : Byte; S : out String);
   procedure MkDir (S : String);
   procedure RmDir (S : String);

   procedure Mark (Object : out Pointer);
   procedure Release (Object : Pointer);

private
   function Exp (V : Real) return Real renames Ada.Numerics.Long_Long_Elementary_Functions.Exp;
   function Ln (V : Real) return Real renames Ada.Numerics.Long_Long_Elementary_Functions.Log;
   function Sqrt (V : Real) return Real renames Ada.Numerics.Long_Long_Elementary_Functions.Sqrt;
   Pi : constant Real := Ada.Numerics.Pi;
   function Cos (X : Real) return Real renames Ada.Numerics.Long_Long_Elementary_Functions.Cos;
   function Sin (X : Real) return Real renames Ada.Numerics.Long_Long_Elementary_Functions.Sin;
end TP7.System;
