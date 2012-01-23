-------------------------------------------------------------------------------
-- NOM DU CSU (spécification)       : tp7-crt.ads
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 2.3a
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
-- Based on:
--*******************************************************
--
--       Turbo Pascal Version 7.0
--       CRT Interface Unit
--
--       Copyright (C) 1988,92 Borland International
--
--*******************************************************
-- Pascal to Ada translation by Pascal Pignard August 2002

--$D-,I-,S-
package TP7.Crt is
   --     pragma Elaborate_Body;

   subtype Integer is TPInteger;

   -- CRT modes

   BW40    : constant := 0;            -- 40x25 B/W on Color Adapter
   CO40    : constant := 1;            -- 40x25 Color on Color Adapter
   BW80    : constant := 2;            -- 80x25 B/W on Color Adapter
   CO80    : constant := 3;            -- 80x25 Color on Color Adapter
   Mono    : constant := 7;            -- 80x25 on Monochrome Adapter
   Font8x8 : constant := 256;          -- Add-in for ROM font

   -- Mode constants for 3.0 compatibility

   C40 : constant := CO40;
   C80 : constant := CO80;

   -- Foreground and background color constants

   Black     : constant := 0;
   Blue      : constant := 1;
   Green     : constant := 2;
   Cyan      : constant := 3;
   Red       : constant := 4;
   Magenta   : constant := 5;
   Brown     : constant := 6;
   LightGray : constant := 7;

   -- Foreground color constants

   DarkGray     : constant := 8;
   LightBlue    : constant := 9;
   LightGreen   : constant := 10;
   LightCyan    : constant := 11;
   LightRed     : constant := 12;
   LightMagenta : constant := 13;
   Yellow       : constant := 14;
   White        : constant := 15;

   -- Add-in for blinking

   Blink : constant := 128;

   -- Interface variables

   CheckBreak  : Boolean := False;    -- Enable Ctrl-Break
   CheckEOF    : Boolean := False;    -- Enable Ctrl-Z
   DirectVideo : Boolean := False;    -- Enable direct video addressing
   CheckSnow   : Boolean := False;    -- Enable snow filtering
   LastMode    : Word    := CO80;                -- Current text mode
   TextAttr    : Byte    := Black * 16 + White;  -- Current text attribute
   WindMin     : Word    := 0;                   -- Window upper left coordinates
   WindMax     : Word    := 24 * 256 + 79;       -- Window lower right coordinates

   -- Interface procedures

   procedure AssignCrt (F : in out Text);
   function KeyPressed return Boolean;
   function ReadKey return Char;
   procedure TextMode (Mode : Integer);
   procedure Window (X1, Y1, X2, Y2 : Byte);
   procedure GotoXY (X, Y : Byte);
   function WhereX return Byte;
   function WhereY return Byte;
   procedure ClrScr;
   procedure ClrEol;
   procedure InsLine;
   procedure DelLine;
   procedure TextColor (Color : Byte);
   procedure TextBackground (Color : Byte);
   procedure LowVideo;
   procedure HighVideo;
   procedure NormVideo;
   procedure Delay1 (MS : Word);
   procedure Sound (Hz : Word);
   procedure NoSound;

   procedure Init;

end TP7.Crt;
