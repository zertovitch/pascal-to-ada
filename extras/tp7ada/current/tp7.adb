-------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : tp7.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 2.3a
-- DATE DE LA DERNIERE MISE A JOUR  : 16 décembre 2011
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

with Ada.Exceptions;
with Ada.Strings.Fixed;

with Gtk.Main;
with Gtk.Handlers;
with Gtk.Button;
with Gtk.Vbutton_Box;
with Gtk.Enums;
with Gtk.Window;
with Gtk.Check_Button;
with Gtk.Text_Buffer;
with Gtk.Text_View;
with Gtk.Scrolled_Window;
with Gdk.Threads;
with Gtk.Text_Mark;
with Gdk.Event;
with Gtk.Arguments;
with Gtkada.Handlers;
with Gdk.Types;
with Gtk.Text_Iter;
with Gdk.Types.Keysyms;
--  with Ada.Integer_Text_IO;
with Glib.Convert;

package body TP7 is

   function To_TPString (Source : String) return TPString is
      Index : constant Natural :=
         Ada.Strings.Fixed.Index (Source, (1 => Ada.Characters.Latin_1.NUL));
   begin
      if Index = 0 then
         return Source & Ada.Characters.Latin_1.NUL;
      else
         -- We keep the ending zero
         return Source (Source'First .. Index);
      end if;
   end To_TPString;

   function To_TPString (Size : Byte; Source : String) return TPString is
      Index : constant Natural :=
         Ada.Strings.Fixed.Index (Source, (1 => Ada.Characters.Latin_1.NUL));
      use Ada.Strings.Fixed;
   begin
      if Index = 0 then
         if Source'Length <= Size then
            return Source & Ada.Characters.Latin_1.NUL & (Size - Source'Length) * ' ';
         else
            return Source (Source'First .. Source'First + Size - 1) &
                   Ada.Characters.Latin_1.NUL;
         end if;
      else
         -- We keep the ending zero
         if Index < Size then
            return Source (Source'First .. Index) & (Size - Index + Source'First) * ' ';
         else
            return Source (Source'First .. Source'First + Size - 1) &
                   Ada.Characters.Latin_1.NUL;
         end if;
      end if;
   end To_TPString;

   function To_String (Source : TPString) return String is
      Index : constant Natural :=
         Ada.Strings.Fixed.Index (Source, (1 => Ada.Characters.Latin_1.NUL));
   begin
      if Index = 0 then
         return Source;
      else
         return Source (Source'First .. Index - 1); -- Without ending zero
      end if;
   end To_String;

   function "+" (C : Char) return String is
   begin
      return (1 => C);
   end "+";

   function "+" (Left : String; Right : String) return String is
      IndexLeft  : constant Natural :=
         Ada.Strings.Fixed.Index (Left, (1 => Ada.Characters.Latin_1.NUL));
      IndexRight : constant Natural :=
         Ada.Strings.Fixed.Index (Right, (1 => Ada.Characters.Latin_1.NUL));
   begin
      if IndexLeft = 0 then
         if IndexRight = 0 then
            return Left & Right & Ada.Characters.Latin_1.NUL;
         else
            return Left & Right;
         end if;
      else
         if IndexRight = 0 then
            return Left (Left'First .. IndexLeft - 1) & Right & Ada.Characters.Latin_1.NUL;
         else
            return Left (Left'First .. IndexLeft - 1) & Right;
         end if;
      end if;
   end "+";

   function "+" (Left : Char; Right : String) return String is
      IndexRight : constant Natural :=
         Ada.Strings.Fixed.Index (Right, (1 => Ada.Characters.Latin_1.NUL));
   begin
      if IndexRight = 0 then
         return Left & Right & Ada.Characters.Latin_1.NUL;
      else
         return Left & Right;
      end if;
   end "+";

   function "+" (Left : String; Right : Char) return String is
      IndexLeft : constant Natural :=
         Ada.Strings.Fixed.Index (Left, (1 => Ada.Characters.Latin_1.NUL));
   begin
      if IndexLeft = 0 then
         return Left & Right & Ada.Characters.Latin_1.NUL;
      else
         return Left (Left'First .. IndexLeft - 1) & Right & Ada.Characters.Latin_1.NUL;
      end if;
   end "+";

   function "+" (Left : Char; Right : Char) return String is
   begin
      return Left & Right & Ada.Characters.Latin_1.NUL;
   end "+";

   procedure Assign_String (Dest : out String; Source : String) is
      Index : constant Natural :=
         Ada.Strings.Fixed.Index (Source, (1 => Ada.Characters.Latin_1.NUL));
   begin
      if Index = 0 then
         if Dest'Length > Source'Length then
            Dest (Dest'First .. Dest'First + Source'Length - 1) := Source;
            if Source'Length = 0 then
               Dest (Dest'First) := Ada.Characters.Latin_1.NUL;
            else
               Dest (Dest'First + Source'Length) := Ada.Characters.Latin_1.NUL;
            end if;
         else
            -- Source is truncated at dest'lenght and last is forced to zero
            Dest             := Source (Source'First .. Source'First + Dest'Length - 1);
            Dest (Dest'Last) := Ada.Characters.Latin_1.NUL;
         end if;
      else
         if Dest'Length > Index - Source'First then
            Dest (Dest'First .. Dest'First + Index - Source'First) :=
              Source (Source'First .. Index);
         else
            -- Source is truncated at dest'lenght and last is forced to zero
            Dest             := Source (Source'First .. Source'First + Dest'Length - 1);
            Dest (Dest'Last) := Ada.Characters.Latin_1.NUL;
         end if;
      end if;
   end Assign_String;

   function Is_Equal (Left, Right : String) return Boolean is
      IndexLeft  : constant Natural :=
         Ada.Strings.Fixed.Index (Left, (1 => Ada.Characters.Latin_1.NUL));
      IndexRight : constant Natural :=
         Ada.Strings.Fixed.Index (Right, (1 => Ada.Characters.Latin_1.NUL));
   begin
      if IndexLeft = 0 then
         if IndexRight = 0 then
            return Left = Right;
         else
            return Left = Right (Right'First .. IndexRight - 1);
         end if;
      else
         if IndexRight = 0 then
            return Left (Left'First .. IndexLeft - 1) = Right;
         else
            return Left (Left'First .. IndexLeft - 1) = Right (Right'First .. IndexRight - 1);
         end if;
      end if;
   end Is_Equal;

   procedure Finalize (F : in out File) is
   begin
      GNAT.OS_Lib.Free (F.Name);
   end Finalize;

   procedure Finalize (F : in out Text) is
   begin
      GNAT.OS_Lib.Free (F.Name);
   end Finalize;

   --     task Ordonanceur is
   --        entry Start;
   --        --      entry Stop;
   --     end Ordonanceur;
   --     task body Ordonanceur is
   --     --      Dead : Boolean;
   --     --      pragma Unreferenced (Dead);
   --     begin
   --        accept Start;
   --        --        loop
   --        --        select
   --        --           accept Stop;
   --        --              --exit;
   --        --              raise Program_Error;
   --        --        or
   --        --          delay 0.01;
   --        --             while Gtk.Main.Events_Pending loop
   --        --                Dead := Gtk.Main.Main_Iteration;
   --        --             end loop;
   --        --           end select;
   --        --           end loop;
   --        Gtk.Main.Main;
   --     end Ordonanceur;

   IntPrincipalProc : TPProc := null;

   task Principal is
      entry Start;
   end Principal;
   task body Principal is
   begin
      select
         accept Start;
         if IntPrincipalProc = null then
            Ada.Text_IO.Put_Line ("Error: principal proc not initialised properly!");
         else
            IntPrincipalProc.all;
         end if;
      or
         terminate;
      end select;
   exception
      when E : others =>
         -- Output to Stdout as Text window may not be readable
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Principal;

   procedure Stop is
   begin
      abort Principal;
   end Stop;

   package Button_Callback is new Gtk.Handlers.Callback (Gtk.Button.Gtk_Button_Record);

   IntStartButton : Gtk.Button.Gtk_Button;

   procedure On_Start_Clicked (Object : access Gtk.Button.Gtk_Button_Record'Class) is
   begin
      Object.Set_State (Gtk.Enums.State_Insensitive);
      Principal.Start;
   end On_Start_Clicked;

   IntQuitButton : Gtk.Button.Gtk_Button;

   procedure On_Quit_Clicked (Object : access Gtk.Button.Gtk_Button_Record'Class) is
      pragma Unreferenced (Object);
   begin
      abort Principal;
      Gtk.Main.Main_Quit;
   end On_Quit_Clicked;

   Win_Ctrl      : Gtk.Window.Gtk_Window;
   IntVBBox_Ctrl : Gtk.Vbutton_Box.Gtk_Vbutton_Box;

   procedure Add_Ctrl (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      IntVBBox_Ctrl.Pack_Start (Widget);
   end Add_Ctrl;

   procedure Show_All_Ctrl is
   begin
      Win_Ctrl.Show_All;
   end Show_All_Ctrl;

   IntDebugButton : Gtk.Check_Button.Gtk_Check_Button;

   function Debug return Boolean is
   begin
      return IntDebugButton.Get_Active;
   end Debug;

   IntKeyBuffer : String (1 .. 100);
   IntKeyRead   : Positive := IntKeyBuffer'First;
   IntKeyWrite  : Positive := IntKeyBuffer'First;

   procedure Write_Key (Ch : Char) is
   begin
      IntKeyBuffer (IntKeyWrite) := Ch;
      IntKeyWrite                := IntKeyWrite + 1;
      if IntKeyWrite > IntKeyBuffer'Last then
         IntKeyWrite := IntKeyBuffer'First;
      end if;
   end Write_Key;

   function Is_Key_Pressed return Boolean is
   begin
      delay 0.01;
      return IntKeyWrite /= IntKeyRead;
   end Is_Key_Pressed;

   function Read_Key return Char is
      Ch : Char;
   begin
      while not Is_Key_Pressed loop
         delay 0.01;
      end loop;
      Ch         := IntKeyBuffer (IntKeyRead);
      IntKeyRead := IntKeyRead + 1;
      if IntKeyRead > IntKeyBuffer'Last then
         IntKeyRead := IntKeyBuffer'First;
      end if;
      return Ch;
   end Read_Key;

   function On_Key_Press_Event
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
      return   Boolean
   is
      pragma Unreferenced (Object);
      Arg1 : constant Gdk.Event.Gdk_Event    := Gtk.Arguments.To_Event (Params, 1);
      Key  : constant Gdk.Types.Gdk_Key_Type := Gdk.Event.Get_Key_Val (Arg1);
      Ch   : constant String                 := Gdk.Event.Get_String (Arg1);
      use type Ada.Text_IO.Count;
      use Gdk.Types.Keysyms;
   begin
      if Ch'Length > 1 then -- UTF8 char
         Write_Key (Glib.Convert.Locale_From_UTF8 (Ch) (1));
         return True;
      elsif Ch'Length /= 0 then -- normal ASCII char
         Write_Key (Ch (1));
         --           Ada.Text_IO.Put(Char'Pos(ch(1))'img);
         return True;
      end if; -- other special keys
      case Key is
         when GDK_BackSpace =>
            Write_Key (Ada.Characters.Latin_1.BS);
         when GDK_Tab =>
            Write_Key (Ada.Characters.Latin_1.HT);
         when GDK_F1 .. GDK_F15 =>
            Write_Key (Ada.Characters.Latin_1.NUL);
         --              Write_Key(Tab....);
         when others =>
            null;
            --              Ada.Integer_Text_IO.Put(Standard.Integer(Key), 9, 16);
            --              Ada.Text_IO.Put(Gdk.Event.Get_String(Arg1));
            --              if Ada.Text_IO.Col > 80 then
            --                 Ada.Text_IO.New_Line;
            --              end if;
      end case;
      return True;
   end On_Key_Press_Event;

   Win_Text    : Gtk.Window.Gtk_Window;
   IntScrolled : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   Aera_Text   : Gtk.Text_View.Gtk_Text_View;
   IntMark     : Gtk.Text_Mark.Gtk_Text_Mark;

   procedure Activate_Win_CRT is
      Index : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Gtk.Text_View.Gtk_New (Aera_Text);
      Aera_Text.Set_Editable (False);
      Gtk.Scrolled_Window.Gtk_New (IntScrolled);
      IntScrolled.Add (Aera_Text);
      Gtk.Text_Buffer.Get_End_Iter (Gtk.Text_View.Get_Buffer (Aera_Text), Index);
      IntMark :=
         Gtk.Text_Buffer.Create_Mark (Gtk.Text_View.Get_Buffer (Aera_Text), "", Index, False);

      Gtk.Window.Gtk_New (Win_Text);
      Gtk.Window.Set_Title (Win_Text, "Win CRT");
      Win_Text.Set_Default_Size (680, 400);
      Win_Text.Add (IntScrolled);
      Win_Text.Show_All;
      Gtkada.Handlers.Return_Callback.Connect
        (Win_Text,
         "key_press_event",
         On_Key_Press_Event'Access);

      Show_All_Ctrl;
   end Activate_Win_CRT;

   procedure Put (S : String) is
      Index : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Gdk.Threads.Enter;
      Gtk.Text_Buffer.Get_End_Iter (Gtk.Text_View.Get_Buffer (Aera_Text), Index);
      Gtk.Text_Buffer.Insert
        (Gtk.Text_View.Get_Buffer (Aera_Text),
         Index,
         Glib.Convert.Locale_To_UTF8 (To_String (S)));
      Gtk.Text_View.Scroll_To_Mark (Aera_Text, IntMark);
      Gdk.Threads.Leave;
   end Put;

   procedure Put_Line (S : String) is
      Index : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Gdk.Threads.Enter;
      Gtk.Text_Buffer.Get_End_Iter (Gtk.Text_View.Get_Buffer (Aera_Text), Index);
      Gtk.Text_Buffer.Insert
        (Gtk.Text_View.Get_Buffer (Aera_Text),
         Index,
         Glib.Convert.Locale_To_UTF8 (To_String (S)) & Ada.Characters.Latin_1.LF);
      Gtk.Text_View.Scroll_To_Mark (Aera_Text, IntMark);
      Gdk.Threads.Leave;
   end Put_Line;

   procedure New_Line is
      Index : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Gdk.Threads.Enter;
      Gtk.Text_Buffer.Get_End_Iter (Gtk.Text_View.Get_Buffer (Aera_Text), Index);
      Gtk.Text_Buffer.Insert
        (Gtk.Text_View.Get_Buffer (Aera_Text),
         Index,
         (1 => Ada.Characters.Latin_1.LF));
      Gtk.Text_View.Scroll_To_Mark (Aera_Text, IntMark);
      Gdk.Threads.Leave;
   end New_Line;

   procedure BackSpace is
      Index : Gtk.Text_Iter.Gtk_Text_Iter;
      Ok    : Boolean;
      pragma Unreferenced (Ok);
   begin
      Gdk.Threads.Enter;
      Gtk.Text_Buffer.Get_End_Iter (Gtk.Text_View.Get_Buffer (Aera_Text), Index);
      Ok := Gtk.Text_Buffer.Backspace (Gtk.Text_View.Get_Buffer (Aera_Text), Index, True, True);
      Gtk.Text_View.Scroll_To_Mark (Aera_Text, IntMark);
      Gdk.Threads.Leave;
   end BackSpace;

   procedure Get_line (S : out String) is
      Index : Positive := S'First;
      Ch    : Char;
   begin
      loop
         Ch := Read_Key;
         if Ch = Ada.Characters.Latin_1.BS then
            if Index > S'First then
               Index := Index - 1;
               BackSpace;
            end if;
         else
            Put ((1 => Ch));
            exit when Ch = Ada.Characters.Latin_1.CR;
            S (Index) := Ch;
            Index     := Index + 1;
            exit when Index >= S'Last;
         end if;
      end loop;
      S (Index) := Ada.Characters.Latin_1.NUL;
   end Get_line;

   function Get return String is
      S     : String (1 .. 256); -- Turbo Pascal string length
      Index : Positive := S'First;
      Ch    : Char;
   begin
      loop
         Ch := Read_Key;
         if Ch = Ada.Characters.Latin_1.BS then
            if Index > S'First then
               Index := Index - 1;
               BackSpace;
            end if;
         else
            Put ((1 => Ch));
            exit when Ch = Ada.Characters.Latin_1.CR;
            S (Index) := Ch;
            Index     := Index + 1;
            exit when Index >= S'Last;
         end if;
      end loop;
      S (Index) := Ada.Characters.Latin_1.NUL;
      return S;
   end Get;

   procedure Get_line is
      Index : Positive := 1;
      Ch    : Char;
   begin
      loop
         Ch := Read_Key;
         if Ch = Ada.Characters.Latin_1.BS then
            if Index > 1 then
               Index := Index - 1;
               BackSpace;
            end if;
         else
            Put ((1 => Ch));
            exit when Ch = Ada.Characters.Latin_1.CR;
            Index := Index + 1;
         end if;
      end loop;
   end Get_line;

   procedure Init (My_Principal_Proc : TPProc) is
   begin
      IntPrincipalProc := My_Principal_Proc;

      Gtk.Button.Gtk_New (IntStartButton, "Start");
      Button_Callback.Connect
        (IntStartButton,
         "clicked",
         Button_Callback.To_Marshaller (On_Start_Clicked'Access),
         False);

      Gtk.Button.Gtk_New (IntQuitButton, "Quit");
      Button_Callback.Connect
        (IntQuitButton,
         "clicked",
         Button_Callback.To_Marshaller (On_Quit_Clicked'Access),
         False);

      Gtk.Window.Gtk_New (Win_Ctrl);
      Gtk.Window.Set_Title (Win_Ctrl, "Win Ctrl");
      Gtk.Vbutton_Box.Gtk_New (IntVBBox_Ctrl);
      Win_Ctrl.Add (IntVBBox_Ctrl);

      Gtk.Check_Button.Gtk_New (IntDebugButton, "Debug");

      Add_Ctrl (IntStartButton);
      Add_Ctrl (IntQuitButton);
      Add_Ctrl (IntDebugButton);

      Win_Ctrl.Show_All;
   end Init;

end TP7;
