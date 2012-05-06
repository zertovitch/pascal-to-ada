-------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : tp7.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 2.6a
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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
--  with Ada.Integer_Text_IO;
with Gtk.Main;
with Gtk.Button;
with Gtk.Vbutton_Box;
with Gtk.Window;
with Gtk.Check_Button;
with Gtk.Text_Buffer;
with Gtk.Text_View;
with Gtk.Scrolled_Window;
with Gtk.Text_Mark;
with Gtk.Text_Iter;
with Gtk.Text_Tag_Table;
with Gtk.Clipboard;
with Gdk.Types.Keysyms;
with Gdk.Threads;
with Gdk.Types;
with Gdk.Event;
with Glib.Convert;

package body TP7 is

   function To_TPString (Source : String) return TPString is
      Index : constant Natural := Ada.Strings.Fixed.Index (Source, Null_TPString);
   begin
      if Index = 0 then
         return Source & Ada.Characters.Latin_1.NUL;
      else
         -- We keep the ending zero
         return Source (Source'First .. Index);
      end if;
   end To_TPString;

   function To_TPString (Size : Byte; Source : String) return TPString is
      Index : constant Natural := Ada.Strings.Fixed.Index (Source, Null_TPString);
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
      Index : constant Natural := Ada.Strings.Fixed.Index (Source, Null_TPString);
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
      IndexLeft  : constant Natural := Ada.Strings.Fixed.Index (Left, Null_TPString);
      IndexRight : constant Natural := Ada.Strings.Fixed.Index (Right, Null_TPString);
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
      IndexRight : constant Natural := Ada.Strings.Fixed.Index (Right, Null_TPString);
   begin
      if IndexRight = 0 then
         return Left & Right & Ada.Characters.Latin_1.NUL;
      else
         return Left & Right;
      end if;
   end "+";

   function "+" (Left : String; Right : Char) return String is
      IndexLeft : constant Natural := Ada.Strings.Fixed.Index (Left, Null_TPString);
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
      Index : constant Natural := Ada.Strings.Fixed.Index (Source, Null_TPString);
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
      IndexLeft  : constant Natural := Ada.Strings.Fixed.Index (Left, Null_TPString);
      IndexRight : constant Natural := Ada.Strings.Fixed.Index (Right, Null_TPString);
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

   task Control is
      entry Start;
      entry Stop;
      entry Over;
   end Control;

   task type Principal;
   type Principal_Task_Access is access Principal;
   procedure Free is new Ada.Unchecked_Deallocation (Principal, Principal_Task_Access);

   Principal_Task   : Principal_Task_Access;
   IntPrincipalProc : TPProc := null;
   IntStartButton   : Gtk.Button.Gtk_Button;
   IntStopButton    : Gtk.Button.Gtk_Button;

   task body Principal is
   begin
      IntPrincipalProc.all;
      Control.Over;
   exception
      when Halt =>
         Control.Over;
      when E : others =>
         -- Write to Stdout as CRT text window may not be readable
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         Control.Over;
   end Principal;

   task body Control is
   begin
      loop
         select
            accept Start;
            Principal_Task := new Principal;
            select
               accept Over;
            or
               accept Stop;
               if not Principal_Task'Terminated then
                  abort Principal_Task.all;
               end if;
            end select;
            Free (Principal_Task);
            IntStopButton.Set_Sensitive (False);
            IntStartButton.Set_Sensitive;
         or
            terminate;
         end select;
      end loop;
   end Control;

   IntKeyBuffer : String (1 .. 100);
   IntKeyRead   : Positive := IntKeyBuffer'First;
   IntKeyWrite  : Positive := IntKeyBuffer'First;

   procedure Init_Key is
   begin
      IntKeyRead := IntKeyWrite;
   end Init_Key;

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

   procedure On_Start_Clicked (Object : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Object.Set_Sensitive (False);
      IntStopButton.Set_Sensitive;
      Init_Key;
      Control.Start;
   end On_Start_Clicked;

   procedure On_Stop_Clicked (Object : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Object.Set_Sensitive (False);
      Control.Stop;
   end On_Stop_Clicked;

   procedure On_Quit_Clicked (Object : access Gtk.Widget.Gtk_Widget_Record'Class) is
      pragma Unreferenced (Object);
   begin
      if Principal_Task /= null and then not Principal_Task'Terminated then
         Control.Stop;
      end if;
      Gtk.Main.Main_Quit;
   end On_Quit_Clicked;

   Aera_Text : Gtk.Text_View.Gtk_Text_View;

   function On_Key_Press_Event
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event)
      return   Boolean
   is
      pragma Unreferenced (Object);
      Key : constant Gdk.Types.Gdk_Key_Type := Gdk.Event.Get_Key_Val (Event);
      Ch  : constant String                 := Gdk.Event.Get_String (Event);
      --        use type Ada.Text_IO.Count;
      use Gdk.Types.Keysyms;
      use type Gdk.Types.Gdk_Key_Type;
      use type Gdk.Types.Gdk_Modifier_Type;
   begin
      if Ch'Length > 1 then -- UTF8 char
         Write_Key (Glib.Convert.Locale_From_UTF8 (Ch) (1));
         return True;
      elsif Ch'Length /= 0 then -- ASCII char
         -- Ctrl-C
         if Char'Pos (Ch (1)) = Char'Pos ('C') - Char'Pos ('@') then
            Gtk.Text_Buffer.Copy_Clipboard
              (Gtk.Text_View.Get_Buffer (Aera_Text),
               Gtk.Clipboard.Get);
            return True;
         end if;
         -- Ctrl-V
         if Char'Pos (Ch (1)) = Char'Pos ('V') - Char'Pos ('@') then
            declare
               S : constant String :=
                  Glib.Convert.Locale_From_UTF8 (Gtk.Clipboard.Wait_For_Text (Gtk.Clipboard.Get));
            begin
               for Ind in S'Range loop
                  Write_Key (S (Ind));
               end loop;
            end;
            return True;
         end if;
         Write_Key (Ch (1));
         return True;
      end if;
      --        Ada.Integer_Text_IO.Put (Standard.Integer (Gdk.Event.Get_State (Event)), 8, 16);
      -- Other special keys
      case Key is
      when GDK_BackSpace =>
         Write_Key (Ada.Characters.Latin_1.BS);
      when GDK_Tab =>
         Write_Key (Ada.Characters.Latin_1.HT);
      when GDK_F1 .. GDK_F10 =>
         Write_Key (Ada.Characters.Latin_1.NUL);
         -- Alt modifier
         if (Gdk.Event.Get_State (Event) and Gdk.Types.Release_Mask) /= 0 then
            Write_Key (Char'Val (Key - GDK_F1 + 104));
         -- Control modifier
         elsif (Gdk.Event.Get_State (Event) and Gdk.Types.Control_Mask) /= 0 then
            Write_Key (Char'Val (Key - GDK_F1 + 94));
         -- Shift modifier
         elsif (Gdk.Event.Get_State (Event) and Gdk.Types.Shift_Mask) /= 0 then
            Write_Key (Char'Val (Key - GDK_F1 + 84));
         -- No modifier
         else
            Write_Key (Char'Val (Key - GDK_F1 + 59));
         end if;
      when GDK_Home =>
         Write_Key (Ada.Characters.Latin_1.NUL);
         if (Gdk.Event.Get_State (Event) and Gdk.Types.Control_Mask) /= 0 then
            Write_Key ('w');
         else
            Write_Key ('G');
         end if;
      when GDK_Left =>
         Write_Key (Ada.Characters.Latin_1.NUL);
         if (Gdk.Event.Get_State (Event) and Gdk.Types.Control_Mask) /= 0 then
            Write_Key ('s');
         else
            Write_Key ('K');
         end if;
      when GDK_Up =>
         Write_Key (Ada.Characters.Latin_1.NUL);
         if (Gdk.Event.Get_State (Event) and Gdk.Types.Control_Mask) /= 0 then
            Write_Key (Char'Val (141));
         else
            Write_Key ('H');
         end if;
      when GDK_Right =>
         Write_Key (Ada.Characters.Latin_1.NUL);
         if (Gdk.Event.Get_State (Event) and Gdk.Types.Control_Mask) /= 0 then
            Write_Key ('t');
         else
            Write_Key ('M');
         end if;
      when GDK_Down =>
         Write_Key (Ada.Characters.Latin_1.NUL);
         if (Gdk.Event.Get_State (Event) and Gdk.Types.Control_Mask) /= 0 then
            Write_Key (Char'Val (145));
         else
            Write_Key ('P');
         end if;
      when GDK_Page_Up =>
         Write_Key (Ada.Characters.Latin_1.NUL);
         if (Gdk.Event.Get_State (Event) and Gdk.Types.Control_Mask) /= 0 then
            Write_Key (Char'Val (132));
         else
            Write_Key ('I');
         end if;
      when GDK_Page_Down =>
         Write_Key (Ada.Characters.Latin_1.NUL);
         if (Gdk.Event.Get_State (Event) and Gdk.Types.Control_Mask) /= 0 then
            Write_Key ('v');
         else
            Write_Key ('Q');
         end if;
      when GDK_End =>
         Write_Key (Ada.Characters.Latin_1.NUL);
         if (Gdk.Event.Get_State (Event) and Gdk.Types.Control_Mask) /= 0 then
            Write_Key ('u');
         else
            Write_Key ('O');
         end if;
      when GDK_Delete =>
         Write_Key (Ada.Characters.Latin_1.NUL);
         if (Gdk.Event.Get_State (Event) and Gdk.Types.Control_Mask) /= 0 then
            Write_Key (Char'Val (147));
         else
            Write_Key ('S');
         end if;
      when others =>
         null;
         --           Ada.Integer_Text_IO.Put(Standard.Integer(Key), 9, 16);
         --           Ada.Integer_Text_IO.Put (Standard.Integer (Gdk.Event.Get_State (Event)), 8,
         --16);
         --           if Ada.Text_IO.Col > 80 then
         --              Ada.Text_IO.New_Line;
         --           end if;
      end case;
      return True;
   end On_Key_Press_Event;

   Win_Text      : Gtk.Window.Gtk_Window;
   IntScrolled   : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   IntCursorMark : Gtk.Text_Mark.Gtk_Text_Mark;

   procedure Activate_Win_CRT is
      Index : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Gtk.Text_View.Gtk_New (Aera_Text);
      Aera_Text.Set_Editable (False);
      Gtk.Scrolled_Window.Gtk_New (IntScrolled);
      IntScrolled.Add (Aera_Text);
      Gtk.Text_Buffer.Get_End_Iter (Gtk.Text_View.Get_Buffer (Aera_Text), Index);
      IntCursorMark :=
         Gtk.Text_Buffer.Create_Mark (Gtk.Text_View.Get_Buffer (Aera_Text), "", Index, False);
      Gtk.Window.Gtk_New (Win_Text);
      Win_Text.Set_Title ("Win CRT");
      Win_Text.Set_Default_Size (640, 480);
      Win_Text.Add (IntScrolled);
      Win_Text.Show_All;
      Gtkada.Handlers.Return_Callback.Connect
        (Win_Text,
         Gtk.Widget.Signal_Key_Press_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller (On_Key_Press_Event'Access));
   end Activate_Win_CRT;

   IntTag    : Gtk.Text_Tag.Gtk_Text_Tag;
   IntGetTag : TPProcGetTag;

   procedure Put (S : String) is
      Index  : Gtk.Text_Iter.Gtk_Text_Iter;
      NewTag : Boolean;
   begin
      Gdk.Threads.Enter;
      IntGetTag (IntTag, NewTag);
      if NewTag then
         Gtk.Text_Tag_Table.Add
           (Gtk.Text_Buffer.Get_Tag_Table (Gtk.Text_View.Get_Buffer (Aera_Text)),
            IntTag);
      end if;
      Gtk.Text_Buffer.Get_Iter_At_Mark
        (Gtk.Text_View.Get_Buffer (Aera_Text),
         Index,
         IntCursorMark);
      Gtk.Text_Buffer.Insert_With_Tags
        (Gtk.Text_View.Get_Buffer (Aera_Text),
         Index,
         Glib.Convert.Locale_To_UTF8 (To_String (S)),
         IntTag);
      Gtk.Text_View.Scroll_To_Mark (Aera_Text, IntCursorMark);
      Gtk.Text_Buffer.Place_Cursor (Gtk.Text_View.Get_Buffer (Aera_Text), Index);
      Gdk.Threads.Leave;
   end Put;

   procedure Put_Line (S : String) is
   begin
      Put (S + Ada.Characters.Latin_1.LF);
   end Put_Line;

   procedure New_Line is
   begin
      Put_Line (Null_TPString);
   end New_Line;

   procedure BackSpace is
      Index : Gtk.Text_Iter.Gtk_Text_Iter;
      Ok    : Boolean;
      pragma Unreferenced (Ok);
   begin
      Gdk.Threads.Enter;
      Gtk.Text_Buffer.Get_Iter_At_Mark
        (Gtk.Text_View.Get_Buffer (Aera_Text),
         Index,
         IntCursorMark);
      Ok := Gtk.Text_Buffer.Backspace (Gtk.Text_View.Get_Buffer (Aera_Text), Index, True, True);
      Gtk.Text_View.Scroll_To_Mark (Aera_Text, IntCursorMark);
      Gtk.Text_Buffer.Place_Cursor (Gtk.Text_View.Get_Buffer (Aera_Text), Index);
      Gdk.Threads.Leave;
   end BackSpace;

   function Get_Line return String is
      S     : String (1 .. 255 + 1); -- Turbo Pascal string size
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
         elsif Ch = Ada.Characters.Latin_1.ESC then
            if Index > S'First then
               Index := Index - 1;
               BackSpace;
            end if;
         else
            Put ((1 => Ch));
            exit when Ch = Ada.Characters.Latin_1.CR;
            if Index < S'Last then
               S (Index) := Ch;
            end if;
            Index := Index + 1;
         end if;
      end loop;
      S (Positive'Min (Index, S'Last))  := Ada.Characters.Latin_1.NUL;
      return S;
   end Get_Line;

   procedure Get_line is
   begin
      loop
         exit when Read_Key = Ada.Characters.Latin_1.CR;
      end loop;
   end Get_line;

   function Where_X return Byte is
      Current_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Gtk.Text_Buffer.Get_Iter_At_Mark
        (Gtk.Text_View.Get_Buffer (Aera_Text),
         Current_Iter,
         IntCursorMark);
      return Byte (Gtk.Text_Iter.Get_Line_Offset (Current_Iter)) + 1;
   end Where_X;

   function Where_Y return Byte is
      Current_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Gtk.Text_Buffer.Get_Iter_At_Mark
        (Gtk.Text_View.Get_Buffer (Aera_Text),
         Current_Iter,
         IntCursorMark);
      return Byte (Gtk.Text_Iter.Get_Line (Current_Iter)) + 1;
   end Where_Y;

   procedure Goto_XY (X, Y : Byte) is
      Target_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Gdk.Threads.Enter;
      Gtk.Text_Buffer.Get_Iter_At_Mark
        (Gtk.Text_View.Get_Buffer (Aera_Text),
         Target_Iter,
         IntCursorMark);
      Gtk.Text_Iter.Set_Line (Target_Iter, Glib.Gint (Y - 1));
      Gtk.Text_Iter.Set_Line_Offset (Target_Iter, Glib.Gint (X - 1));
      Gtk.Text_Buffer.Move_Mark
        (Gtk.Text_View.Get_Buffer (Aera_Text),
         IntCursorMark,
         Target_Iter);
      Gdk.Threads.Leave;
   end Goto_XY;

   procedure Clr_Scr is
   begin
      Gdk.Threads.Enter;
      --        Gtk.Text_Buffer.Set_Text (Gtk.Text_View.Get_Buffer (Aera_Text), "");
      Gdk.Threads.Leave;
   end Clr_Scr;

   procedure Clr_Eol is
      Y                    : constant Byte := Where_Y;
      Start_Iter, End_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Gdk.Threads.Enter;
      Gtk.Text_Buffer.Get_Iter_At_Mark
        (Gtk.Text_View.Get_Buffer (Aera_Text),
         Start_Iter,
         IntCursorMark);
      Gtk.Text_Buffer.Get_Iter_At_Line
        (Gtk.Text_View.Get_Buffer (Aera_Text),
         End_Iter,
         Glib.Gint (Y));
      Gtk.Text_Buffer.Delete (Gtk.Text_View.Get_Buffer (Aera_Text), Start_Iter, End_Iter);
      Gtk.Text_Buffer.Insert_With_Tags
        (Gtk.Text_View.Get_Buffer (Aera_Text),
         End_Iter,
         (1 => Ada.Characters.Latin_1.LF),
         IntTag);
      Gdk.Threads.Leave;
   end Clr_Eol;

   procedure Ins_Line is
      X : constant Byte := Where_X;
      Y : constant Byte := Where_Y;
   begin
      Goto_XY (1, Y + 1);
      New_Line;
      Goto_XY (X, Y);
   end Ins_Line;

   procedure Del_Line is
      Y                    : constant Byte := Where_Y;
      Start_Iter, End_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Gdk.Threads.Enter;
      Gtk.Text_Buffer.Get_Iter_At_Line
        (Gtk.Text_View.Get_Buffer (Aera_Text),
         Start_Iter,
         Glib.Gint (Y - 1));
      Gtk.Text_Buffer.Get_Iter_At_Line
        (Gtk.Text_View.Get_Buffer (Aera_Text),
         End_Iter,
         Glib.Gint (Y));
      Gtk.Text_Buffer.Delete (Gtk.Text_View.Get_Buffer (Aera_Text), Start_Iter, End_Iter);
      Gdk.Threads.Leave;
   end Del_Line;

   CRT_Init_Proc : TPProc := null;

   procedure Init_CRT (InitProc : TPProc; GetTag : TPProcGetTag) is
   begin
      CRT_Init_Proc := InitProc;
      IntGetTag     := GetTag;
   end Init_CRT;

   Mouse_Event_Handler : Gtkada.Handlers.Return_Callback.Event_Marshaller.Handler := null;

   procedure Set_Mouse_Event
     (Event_Handler : Gtkada.Handlers.Return_Callback.Event_Marshaller.Handler)
   is
   begin
      Mouse_Event_Handler := Event_Handler;
   end Set_Mouse_Event;

   procedure Get_Mouse_Event
     (Event_Handler : out Gtkada.Handlers.Return_Callback.Event_Marshaller.Handler)
   is
   begin
      Event_Handler := Mouse_Event_Handler;
   end Get_Mouse_Event;

   procedure Get_Key_Event
     (Event_Handler : out Gtkada.Handlers.Return_Callback.Event_Marshaller.Handler)
   is
   begin
      Event_Handler := On_Key_Press_Event'Access;
   end Get_Key_Event;

   IntWinGraph : Gdk.Window.Gdk_Window := null;

   procedure Set_Graph (Window : Gdk.Window.Gdk_Window) is
   begin
      IntWinGraph := Window;
   end Set_Graph;

   procedure Get_Graph (Window : out Gdk.Window.Gdk_Window) is
   begin
      Window := IntWinGraph;
   end Get_Graph;

   Win_Ctrl       : Gtk.Window.Gtk_Window;
   IntVBBox_Ctrl  : Gtk.Vbutton_Box.Gtk_Vbutton_Box;
   IntDebugButton : Gtk.Check_Button.Gtk_Check_Button;

   procedure Add_Ctrl (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      IntVBBox_Ctrl.Pack_Start (Widget);
      Win_Ctrl.Show_All;
   end Add_Ctrl;

   function Debug return Boolean is
   begin
      return IntDebugButton.Get_Active;
   end Debug;

   IntQuitButton : Gtk.Button.Gtk_Button;

   procedure Init (My_Principal_Proc : TPProc) is
   begin
      IntPrincipalProc := My_Principal_Proc;

      Gtk.Button.Gtk_New (IntStartButton, "Start");
      Gtkada.Handlers.Widget_Callback.Connect
        (IntStartButton,
         Gtk.Button.Signal_Clicked,
         On_Start_Clicked'Access,
         False);

      Gtk.Button.Gtk_New (IntStopButton, "Stop");
      IntStopButton.Set_Sensitive (False);
      Gtkada.Handlers.Widget_Callback.Connect
        (IntStopButton,
         Gtk.Button.Signal_Clicked,
         On_Stop_Clicked'Access,
         False);

      Gtk.Button.Gtk_New (IntQuitButton, "Quit");
      Gtkada.Handlers.Widget_Callback.Connect
        (IntQuitButton,
         Gtk.Button.Signal_Clicked,
         On_Quit_Clicked'Access,
         False);

      Gtk.Check_Button.Gtk_New (IntDebugButton, "Debug");

      Gtk.Window.Gtk_New (Win_Ctrl);
      Win_Ctrl.Set_Title ("Win Ctrl");
      Gtk.Vbutton_Box.Gtk_New (IntVBBox_Ctrl);
      Win_Ctrl.Add (IntVBBox_Ctrl);

      Add_Ctrl (IntStartButton);
      Add_Ctrl (IntStopButton);
      Add_Ctrl (IntQuitButton);
      Add_Ctrl (IntDebugButton);

      if CRT_Init_Proc /= null then
         Activate_Win_CRT;
         CRT_Init_Proc.all;
      end if;
   end Init;

end TP7;
