-------------------------------------------------------------------------------
-- NOM DU CSU (spécification)       : tp7.ads
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 2.4a
-- DATE DE LA DERNIERE MISE A JOUR  : 26 décembre 2011
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

with System;
with System.Address_Image;
with Ada.Characters.Latin_1;
private with Ada.Text_IO;
private with Ada.Finalization;
private with GNAT.OS_Lib;
private with Gtk.Widget;

package TP7 is
   pragma Elaborate_Body;

   -- Tips : subtypes of Ada Integer to avoid annoying conversion
   subtype Byte is Standard.Integer range 0 .. 2 ** 8 - 1;
   subtype Word is Standard.Integer range 0 .. 2 ** 16 - 1;
   subtype Shortint is Standard.Integer range -2 ** 7 .. 2 ** 7 - 1;
   -- Tips : Turbo Pascal "Integer" type,
   --        add "subtype Integer is TPInteger;" at the beginning of your code
   subtype TPInteger is Standard.Integer range -2 ** 15 .. 2 ** 15 - 1;
   subtype Longint is Standard.Integer range -2 ** 31 .. 2 ** 31 - 1;

   -- Tips : types defined for mod operations (and, or, not...) and
   --        record components with corresponding TP size
   type Byte1 is mod 2 ** 8;
   for Byte1'Size use 8;
   type Word1 is mod 2 ** 16;
   for Word1'Size use 16;
   type Shortint1 is range -2 ** 7 .. 2 ** 7 - 1;
   for Shortint1'Size use 8;
   type Integer1 is range -2 ** 15 .. 2 ** 15 - 1;
   for Integer1'Size use 16;
   type Longint1 is range -2 ** 31 .. 2 ** 31 - 1;
   for Longint1'Size use 32;

   -- TIPS : type Boolean is same as Ada and subtypes declared to avoid annoying conversion
   subtype ByteBool is Standard.Boolean;
   subtype WordBool is Standard.Boolean;
   subtype LongBool is Standard.Boolean;

   -- Tips : types defined for record components with corresponding TP size
   type Boolean1 is (False1, True1);
   for Boolean1 use (False1 => 0, True1 => 1);
   for Boolean1'Size use 1 * 8;
   subtype ByteBool1 is Boolean1;
   type WordBool1 is (False2, True2);
   for WordBool1 use (False2 => 0, True2 => 1);
   for WordBool1'Size use 2 * 8;
   type LongBool1 is (False3, True3);
   for LongBool1 use (False3 => 0, True3 => 1);
   for LongBool1'Size use 4 * 8;

   -- Tips : subtypes of Ada Long_Long_Float to avoid annoying conversion
   subtype Real is Long_Long_Float range -1.7e38 .. 1.7e38;
   subtype Single is Long_Long_Float range -3.4e38 .. 3.4e38;
   subtype Double is Long_Long_Float range -1.7e308 .. 1.7e308;
   --     subtype Extented is Long_Long_Float range -1.7e308 .. 1.7e308; -- 32 bits target
   subtype Extented is Long_Long_Float range -1.1e4932 .. 1.1e4932; -- 64 bits target
   subtype Comp is Long_Long_Integer range -2 ** 63 .. 2 ** 63 - 1;

   -- Tips : types defined for record components with corresponding TP size
   type Real1 is digits 6 range -1.7e38 .. 1.7e38;
   for Real1'Size use 6 * 8;
   type Single1 is digits 6 range -3.4e38 .. 3.4e38;
   for Single1'Size use 4 * 8;
   type Double1 is digits 15 range -1.7e308 .. 1.7e308;
   for Double1'Size use 8 * 8;
   --     type Extented1 is digits 15 range -1.7e308 .. 1.7e308; -- 32 bits target
   --     for Extented1'Size use 10 * 8; -- 32 bits target
   type Extented1 is digits 18 range -1.1e4932 .. 1.1e4932; -- 64 bits target
   for Extented1'Size use 16 * 8; -- 64 bits target, take care that TP7 required only 10 bytes
   type Comp1 is range -2 ** 63 .. 2 ** 63 - 1;
   for Comp1'Size use 8 * 8;

   -- Tips : Pointer type size maybe 32 or 64 bits, take care in record components
   subtype Pointer is System.Address;
   nil : System.Address renames System.Null_Address;
   function Pointer_Image (A : Pointer) return String renames System.Address_Image;

   type File is limited private;
   type Text is limited private;

   subtype Char is Character;

   -- Tips : TPString type emulate zero terminated Latin 1 string
   -- Subtype declaration just to distingushed both types
   -- but user can use only String name as internal proc checks for zero
   -- Declare String(1..16) for corresponding String[15] (same user size 15)
   subtype TPString is String;
   -- Convert Ada String to zero terminated string
   function To_TPString (Source : String) return TPString;
   -- Convert Ada String to zero terminated string of specified user size as String[Size]
   function To_TPString (Size : Byte; Source : String) return TPString;
   -- Convert zero terminated string to Ada string
   function To_String (Source : TPString) return String;
   -- String assignment, could be of any type, Ada string or zero terminated string
   procedure Assign_String (Dest : out String; Source : String);
   Null_TPString : constant String := (1 => Ada.Characters.Latin_1.NUL);

   function Is_Equal (Left, Right : String) return Boolean;
   --function "/=" (Left, Right: String) return Boolean;
   --   function "<"  (Left, Right: String) return Boolean;
   --  function "<=" (Left, Right: String) return Boolean;
   --  function ">"  (Left, Right: String) return Boolean;
   --   function ">=" (Left, Right: String) return Boolean;
   function "+" (Left : String; Right : String) return String;
   function "+" (Left : Char; Right : String) return String;
   function "+" (Left : String; Right : Char) return String;
   function "+" (Left : Char; Right : Char) return String;
   function "+" (C : Char) return String;

   -- Tips : call your code from main with "Init (MyCode'Access);"
   type TPProc is access procedure;
   procedure Init (My_Principal_Proc : TPProc);

   -- Tips : call this function to test if user as checked Debug in Win_Ctrl
   function Debug return Boolean;

private
   -- Internal types
   type File is new Ada.Finalization.Limited_Controlled with record
      File : GNAT.OS_Lib.File_Descriptor := 0;
      Name : GNAT.OS_Lib.String_Access;
   end record;
   procedure Finalize (F : in out File);
   type File_Kind is (File_System, Stdinout, Win_CRT);
   type Text is new Ada.Finalization.Limited_Controlled with record
      Device : File_Kind;
      File   : Ada.Text_IO.File_Type;
      Name   : GNAT.OS_Lib.String_Access;
   end record;
   procedure Finalize (F : in out Text);

   -- Internal procedures for adding widgets in control window
   procedure Add_Ctrl (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --     procedure Show_All_Ctrl;

   -- Internal procedure for halt
   procedure Stop;

   -- Internal I/O procedures to GTK.Text_View
   procedure Put (S : String);
   procedure Put_Line (S : String);
   procedure New_Line;
   procedure Get_line (S : out String);
   function Get return String;
   procedure Get_Line;
   function Is_Key_Pressed return Boolean;
   function Read_Key return Char;

   -- Internal registeration for init proc in CRT child unit
   procedure Init_CRT (InitProc : TPProc);

end TP7;
