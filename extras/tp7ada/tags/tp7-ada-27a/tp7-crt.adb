-------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : tp7-crt.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 2.4a
-- DATE DE LA DERNIERE MISE A JOUR  : 5 février 2012
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

with Gdk.Color;
with TP7.System;
with Gtk.Text_Tag;
pragma Elaborate_All (Gdk.Color);

package body TP7.Crt is

   MaxColors : constant := 15;
   type IntTabColors is array (Byte1 range 0 .. MaxColors) of Gdk.Color.Gdk_Color;
   type IntPaletteType is record
      Size   : Byte;
      Colors : IntTabColors;
   end record;
   IntPalette         : constant IntPaletteType                    :=
     (Size   => 16,
      Colors => (Black        => Gdk.Color.Parse ("black"),
                 Blue         => Gdk.Color.Parse ("blue"),
                 Green        => Gdk.Color.Parse ("green"),
                 Cyan         => Gdk.Color.Parse ("cyan"),
                 Red          => Gdk.Color.Parse ("red"),
                 Magenta      => Gdk.Color.Parse ("magenta"),
                 Brown        => Gdk.Color.Parse ("brown"),
                 LightGray    => Gdk.Color.Parse ("LightGray"),
                 DarkGray     => Gdk.Color.Parse ("DarkGray"),
                 LightBlue    => Gdk.Color.Parse ("LightBlue"),
                 LightGreen   => Gdk.Color.Parse ("LightGreen"),
                 LightCyan    => Gdk.Color.Parse ("LightCyan"),
                 LightRed     => Gdk.Color.Parse ("LightCoral"),
                 LightMagenta => Gdk.Color.Parse ("LightPink"),
                 Yellow       => Gdk.Color.Parse ("yellow"),
                 White        => Gdk.Color.Parse ("white")));
   IntTags            : array (Byte1) of Gtk.Text_Tag.Gtk_Text_Tag := (others => null);
   IntDefaultTextAttr : constant array (CO80 .. CO80X) of Byte1    :=
     (Black * 16 + LightGray, -- DOS screen setting
      White * 16 + Black); -- Color terminal setting

   procedure Get_Tag (Tag : out Gtk.Text_Tag.Gtk_Text_Tag; NewTag : out Boolean) is
      use type Gtk.Text_Tag.Gtk_Text_Tag;
   begin
      NewTag := False;
      if IntTags (TextAttr) = null then
         Gtk.Text_Tag.Gtk_New (IntTags (TextAttr));
         Gdk.Color.Set_Property
           (IntTags (TextAttr),
            Gtk.Text_Tag.Foreground_Gdk_Property,
            IntPalette.Colors (TextAttr and 16#0F#));
         Gdk.Color.Set_Property
           (IntTags (TextAttr),
            Gtk.Text_Tag.Background_Gdk_Property,
            IntPalette.Colors ((TextAttr and 16#F0#) / 16));
         NewTag := True;
      end if;
      Tag := IntTags (TextAttr);
   end Get_Tag;

   procedure AssignCrt (F : in out Text) is
   begin
      F.Device := Win_CRT;
   end AssignCrt;

   procedure TextMode (Mode : Integer) is
   begin
      LastMode := Mode;
      NormVideo;
   end TextMode;

   procedure Window (X1, Y1, X2, Y2 : Byte) is
   begin
      WindMin := Word (Byte'Pred (Y1)) * 256 + Word (Byte'Pred (X1));
      WindMax := Word (Byte'Pred (Y2)) * 256 + Word (Byte'Pred (X2));
      if Debug then
         TP7.System.Writeln ("La fonction Window n'est pas définie !");
      end if;
   end Window;

   procedure TextColor (Color : Byte) is
   begin
      TextAttr := (TextAttr and 16#F0#) or (Byte1 (Color) and 16#0F#);
   end TextColor;

   procedure TextBackground (Color : Byte) is
   begin
      TextAttr := (TextAttr and 16#8F#) or ((Byte1 (Color) and 16#07#) * 16);
   end TextBackground;

   procedure LowVideo is
   begin
      TextAttr := TextAttr and 16#F7#;
   end LowVideo;

   procedure HighVideo is
   begin
      TextAttr := TextAttr or 16#08#;
   end HighVideo;

   procedure NormVideo is
   begin
      if LastMode in CO80 .. CO80X then
         TextAttr := IntDefaultTextAttr (LastMode);
      end if;
   end NormVideo;

   procedure Delay1 (MS : Word) is
   begin
      delay Duration (MS / 1000);
   end Delay1;

   procedure Sound (Hz : Word) is
      pragma Unreferenced (Hz);
   --        Level : Integer;
   begin
      --GetSoundVol(Level);
      --Note(Hz, Level * 32, 6);
      if Debug then
         TP7.System.Writeln ("La fonction Sound n'est pas définie !");
      end if;
   end Sound;

   procedure NoSound is
   begin
      --StopSound;
      if Debug then
         TP7.System.Writeln ("La fonction NoSound n'est pas définie !");
      end if;
   end NoSound;

   procedure Init is
   begin
      AssignCrt (TP7.System.Input);
      AssignCrt (TP7.System.Output);
      TP7.System.Writeln ("Turbo Pascal emulated console:");
   end Init;

begin
   TP7.Init_CRT (Init'Access, Get_Tag'Access);
end TP7.Crt;
