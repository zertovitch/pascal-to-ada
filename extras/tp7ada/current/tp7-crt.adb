-------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : tp7-crt.adb
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

with Gdk.Color;
with Glib;
with Gtk.Text_Iter;
with TP7.System;
pragma Elaborate_All (Gdk.Color);

package body TP7.Crt is

   CCharSize : constant := 8;
   MaxColors : constant := 15;

   type IntTabColors is array (0 .. MaxColors) of Gdk.Color.Gdk_Color;

   type IntPaletteType is record
      Size   : Byte;
      Colors : IntTabColors;
   end record;

   IntPalette : IntPaletteType :=
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

   procedure AssignCrt (F : in out Text) is
   begin
      F.Device := Win_CRT;
   end AssignCrt;

   function KeyPressed return Boolean is
   begin
      return Is_Key_Pressed;
   end KeyPressed;

   function ReadKey return Char is
   begin
      return Read_Key;
   end ReadKey;

   procedure TextMode (Mode : Integer) is
   begin
      if Debug then
         Put ("La fonction TextMode n'est pas définie pour le Macintosh !");
         New_Line;
      end if;
      null;
      LastMode := Mode;
   end TextMode;

   procedure Window (X1, Y1, X2, Y2 : Byte) is
   --		R: aliased Rect;
   begin
      WindMin := Word (Byte'Pred (Y1)) * 256 + Word (Byte'Pred (X1));
      WindMax := Word (Byte'Pred (Y2)) * 256 + Word (Byte'Pred (X2));
      --        Move (Win_Text, Gint (X1 * CCharSize), Gint (Y1 * CCharSize));
      --        Resize (Win_Text, Gint ((X2 - X1) * CCharSize), Gint ((Y2 - Y1) * CCharSize));
      --SetOrigin(Short_Integer(X1 * CCharSize), Short_Integer(Y1 *
      --CCharSize));
      --SetRect(R'access, 0, 0, Short_Integer((X2 - X1) * CCharSize),
      --Short_Integer((Y2 - Y1) * CCharSize));
      --ClipRect(R'access);
      null;
   end Window;

   procedure GotoXY (X, Y : Byte) is
      Target_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Gtk.Text_Iter.Set_Line (Target_Iter, Glib.Gint (X));
      Gtk.Text_Iter.Set_Line_Offset (Target_Iter, Glib.Gint (Y));
      --        Place_Cursor (Gtk.Text_View.Get_Buffer (Aera_Text), Target_Iter);
      null;
   end GotoXY;

   function WhereX return Byte is
      ResultWhereX : Byte;
   begin
      declare
      --P: aliased Point;
         Current_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      begin
         --           Get_Iter_At_Mark
         --             (Gtk.Text_View.Get_Buffer (Aera_Text),
         --              Current_Iter,
         --              Get_Insert (Gtk.Text_View.Get_Buffer (Aera_Text)));
         ResultWhereX := Byte (Gtk.Text_Iter.Get_Line (Current_Iter)) + 1;
         --GetPen(P'access);
         --ResultWhereX := Byte(P.h   /   CCharSize);
         null;
      end;
      return ResultWhereX;
   end WhereX;

   function WhereY return Byte is
      ResultWhereY : Byte;
   begin
      declare
      --P: aliased Point;
         Current_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      begin
         --           Get_Iter_At_Mark
         --             (Gtk.Text_View.Get_Buffer (Aera_Text),
         --              Current_Iter,
         --              Get_Insert (Gtk.Text_View.Get_Buffer (Aera_Text)));
         ResultWhereY := Byte (Gtk.Text_Iter.Get_Chars_In_Line (Current_Iter));
         --GetPen(P'access);
         --ResultWhereY := Byte(P.v   /   CCharSize);
         null;
      end;
      return ResultWhereY;
   end WhereY;

   procedure ClrScr is

   --R: aliased Rect;
   begin
      --Page;
      --SetRect(R'access, Short_Integer'First, Short_Integer'First,
      --Short_Integer'Last, Short_Integer'Last);
      --EraseRect(R'access);
      --        Set_Text (Gtk.Text_View.Get_Buffer (Aera_Text), "");
      null;
   end ClrScr;

   procedure ClrEol is
   begin
      null;
   end ClrEol;

   procedure InsLine is
   begin
      null;
   end InsLine;

   procedure DelLine is
   begin
      null;
   end DelLine;

   procedure TextColor (Color : Byte) is
   begin
      TextAttr := Byte ((Byte1 (TextAttr) and 16#70#) or Byte1 (Color));
      --ForeColor(Integer(IntPalette.Colors(Color)));
      null;
   end TextColor;

   procedure TextBackground (Color : Byte) is
   begin
      TextAttr := Byte ((Byte1 (TextAttr) and 16#8F#) or Byte1 (Color));
      --        TextAttr := (TextAttr and 16#8F#) or Color;
      --BackColor(Integer(IntPalette.Colors(Color)));
      null;
   end TextBackground;

   procedure LowVideo is
   begin
      TextAttr := Byte ((Byte1 (TextAttr) and 16#F7#));
      --        TextAttr := TextAttr and 16#F7#;
      --ForeColor(Integer(IntPalette.Colors(TextAttr and 16#0F#)));
      null;
   end LowVideo;

   procedure HighVideo is
   begin
      TextAttr := Byte ((Byte1 (TextAttr) or 16#08#));
      --      TextAttr := TextAttr or 16#08#;
      --ForeColor(Integer(IntPalette.Colors(TextAttr and 16#0F#)));
      null;
   end HighVideo;

   procedure NormVideo is
   begin
      TextAttr := Black * 16 + White;
      --ForeColor(Integer(IntPalette.Colors(White)));
      --BackColor(Integer(IntPalette.Colors(Black)));
      null;
   end NormVideo;

   procedure Delay1 (MS : Word) is
   begin
      delay Duration (MS / 1000);
   end Delay1;

   procedure Sound (Hz : Word) is
      Level : Integer;
   begin
      --GetSoundVol(Level);
      --Note(Hz, Level * 32, 6);
      null;
   end Sound;

   procedure NoSound is
   begin
      --StopSound;
      null;
   end NoSound;

   procedure Init is
   begin
      AssignCrt (TP7.System.Input);
      AssignCrt (TP7.System.Output);
      TP7.Put_Line ("Turbo Pascal emulated console:");
   end Init;

begin
   TP7.Init_CRT (Init'Access);
end TP7.Crt;
