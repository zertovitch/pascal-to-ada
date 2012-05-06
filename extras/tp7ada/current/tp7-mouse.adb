-------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : tp7-mouse.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 1.0a
-- DATE DE LA DERNIERE MISE A JOUR  : 5 mai 2012
-- ROLE DU CSU                      : Unité d'émulation de la souris DOS.
--
--
-- FONCTIONS EXPORTEES DU CSU       :
--
-- FONCTIONS LOCALES DU CSU         :
--
--
-- NOTES                            :
--
-- COPYRIGHT                        : (c) Pascal Pignard 1988-2012
-- LICENCE                          : CeCILL V2 (http://www.cecill.info)
-- CONTACT                          : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Glib;
with Gdk.Event;
with Gdk.Cursor;
with Gdk.Window;
with Gdk.Types;
with Gdk.Threads;
with TP7.System;

package body TP7.Mouse is

   GTKMaxButton : constant := 5;
   type ButtonState is record
      Pressed, Released, DoubleClic, TripleClic          : Boolean;
      Press_Count, Release_Count                         : Integer;
      LastXPress, LastYPress, LastXRelease, LastYRelease : Glib.Gdouble;
   end record;
   type ButtonsState is array (1 .. GTKMaxButton) of ButtonState;
   IntButtons                 : ButtonsState;
   IntVisible                 : Integer;
   IntPositionX, IntPositionY : Glib.Gint;
   IntScroll                  : Integer;
   IntWindow                  : Gdk.Gdk_Window;
   To_Integer                 : constant array (Gdk.Event.Gdk_Scroll_Direction) of Integer :=
     (ScrollUp,
      ScrollDown,
      ScrollLeft,
      ScrollRight);

   ---------------
   -- MouseInit --
   ---------------

   function MouseInit return Boolean is
      use type Gdk.Window.Gdk_Window;
   begin
      IntButtons   := (others => (False, False, False, False, 0, 0, 0.0, 0.0, 0.0, 0.0));
      IntVisible   := -1;
      IntPositionX := 0;
      IntPositionY := 0;
      IntScroll    := 0;
      -- Get back the graph window to set the cursor
      TP7.Get_Graph (IntWindow);
      return IntWindow /= null;
   end MouseInit;

   ---------------
   -- NbrButton --
   ---------------

   function NbrButton return Integer is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction NbrButton n'est pas définie !");
      end if;
      return GTKMaxButton;
   end NbrButton;

   ---------------
   -- HideMouse --
   ---------------

   procedure HideMouse is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction HideMouse n'est pas définie !");
      end if;
      IntVisible := IntVisible - 1;
   end HideMouse;

   ---------------
   -- ShowMouse --
   ---------------

   procedure ShowMouse is
   begin
      if Debug then
         TP7.System.Writeln ("La fonction ShowMouse n'est pas définie !");
      end if;
      IntVisible := IntVisible + 1;
   end ShowMouse;

   -------------
   -- GetXPos --
   -------------

   function GetXPos return Integer is
      X, Y : Glib.Gint;
      Mask : Gdk.Types.Gdk_Modifier_Type;
      win  : Gdk.Window.Gdk_Window;
   begin
      delay 0.01;
      Gdk.Threads.Enter;
      Gdk.Window.Get_Pointer (IntWindow, X, Y, Mask, win);
      Gdk.Threads.Leave;
      return Integer (X);
   end GetXPos;

   -------------
   -- GetYPos --
   -------------

   function GetYPos return Integer is
      X, Y : Glib.Gint;
      Mask : Gdk.Types.Gdk_Modifier_Type;
      win  : Gdk.Window.Gdk_Window;
   begin
      delay 0.01;
      Gdk.Threads.Enter;
      Gdk.Window.Get_Pointer (IntWindow, X, Y, Mask, win);
      Gdk.Threads.Leave;
      return Integer (Y);
   end GetYPos;

   ---------------
   -- NumButton --
   ---------------

   function GetStatus return Integer is
      X, Y : Glib.Gint;
      Mask : Gdk.Types.Gdk_Modifier_Type;
      win  : Gdk.Window.Gdk_Window;
      use type Gdk.Types.Gdk_Modifier_Type;
   begin
      delay 0.01;
      Gdk.Threads.Enter;
      Gdk.Window.Get_Pointer (IntWindow, X, Y, Mask, win);
      Gdk.Threads.Leave;
      return Integer (Mask / Gdk.Types.Button1_Mask);
   end GetStatus;

   ---------------
   -- GetScroll --
   ---------------

   function GetScroll return Integer is
   begin
      delay 0.01;
      return Scroll : constant Integer := IntScroll do
         IntScroll := 0;
      end return;
   end GetScroll;

   ----------------------
   -- MouseNewPosition --
   ----------------------

   procedure MouseNewPosition (NouvX, NouvY : Integer) is
      pragma Unreferenced (NouvX, NouvY);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction MouseNewPosition n'est pas définie !");
      end if;
   end MouseNewPosition;

   -----------------
   -- ButtonPress --
   -----------------

   function ButtonPress (Button : Integer) return Boolean is
   begin
      delay 0.01;
      return Pressed : constant Boolean := IntButtons (Button).Pressed do
         IntButtons (Button).Pressed := False;
      end return;
   end ButtonPress;

   function ButtonDoublePress (Button : Integer) return Boolean is
   begin
      delay 0.01;
      return Pressed : constant Boolean := IntButtons (Button).DoubleClic do
         IntButtons (Button).DoubleClic := False;
      end return;
   end ButtonDoublePress;

   function ButtonTriplePress (Button : Integer) return Boolean is
   begin
      delay 0.01;
      return Pressed : constant Boolean := IntButtons (Button).TripleClic do
         IntButtons (Button).TripleClic := False;
      end return;
   end ButtonTriplePress;

   ----------------
   -- CountPress --
   ----------------

   function CountPress (Button : Integer) return Integer is
   begin
      return Count : constant Integer := IntButtons (Button).Press_Count do
         IntButtons (Button).Press_Count := 0;
      end return;
   end CountPress;

   ----------------
   -- LastXPress --
   ----------------

   function LastXPress (Button : Integer) return Integer is
   begin
      return Integer (IntButtons (Button).LastXPress);
   end LastXPress;

   ----------------
   -- LastYPress --
   ----------------

   function LastYPress (Button : Integer) return Integer is
   begin
      return Integer (IntButtons (Button).LastYPress);
   end LastYPress;

   -------------------
   -- ButtonRelease --
   -------------------

   function ButtonRelease (Button : Integer) return Boolean is
   begin
      delay 0.01;
      return Released : constant Boolean := IntButtons (Button).Released do
         IntButtons (Button).Released := False;
      end return;
   end ButtonRelease;

   ------------------
   -- CountRelease --
   ------------------

   function CountRelease (Button : Integer) return Integer is
   begin
      return Count : constant Integer := IntButtons (Button).Release_Count do
         IntButtons (Button).Release_Count := 0;
      end return;
   end CountRelease;

   ------------------
   -- LastXRelease --
   ------------------

   function LastXRelease (Button : Integer) return Integer is
   begin
      return Integer (IntButtons (Button).LastXRelease);
   end LastXRelease;

   ------------------
   -- LastYRelease --
   ------------------

   function LastYRelease (Button : Integer) return Integer is
   begin
      return Integer (IntButtons (Button).LastYRelease);
   end LastYRelease;

   -----------------
   -- MouseWindow --
   -----------------

   procedure MouseWindow (MinX, MinY, MaxX, MaxY : Integer) is
      pragma Unreferenced (MinX, MinY, MaxX, MaxY);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction MouseWindow n'est pas définie !");
      end if;
   end MouseWindow;

   ------------------------
   -- MouseSetGraphBlock --
   ------------------------

   procedure MouseSetGraphBlock (Cursor : Integer) is
      LCursor : Gdk.Cursor.Gdk_Cursor;
   begin
      Gdk.Threads.Enter;
      Gdk.Cursor.Gdk_New (LCursor, Gdk.Cursor.Gdk_Cursor_Type'Val (Cursor));
      Gdk.Window.Set_Cursor (IntWindow, LCursor);
      Gdk.Cursor.Unref (LCursor);
      Gdk.Threads.Leave;
   end MouseSetGraphBlock;

   procedure MouseSetGraphBlock
     (Source             : String;
      Mask               : String;
      FGColor, BGColor   : Integer;
      HotSpotX, HotSpotY : Integer)
   is
      pragma Unreferenced (Source, Mask, BGColor, FGColor, HotSpotY, HotSpotX);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction MouseSetGraphBlock n'est pas définie !");
      end if;
   end MouseSetGraphBlock;

   -----------------------
   -- MouseSetTextBlock --
   -----------------------

   procedure MouseSetTextBlock (MType, MScreen, MCursor : Integer) is
      pragma Unreferenced (MType, MScreen, MCursor);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction MouseSetTextBlock n'est pas définie !");
      end if;
   end MouseSetTextBlock;

   -----------------------
   -- MouseXMotionCount --
   -----------------------

   function MouseXMotionCount return Integer is
      X, Y : Glib.Gint;
      Mask : Gdk.Types.Gdk_Modifier_Type;
      win  : Gdk.Window.Gdk_Window;
      use type Glib.Gint;
   begin
      Gdk.Threads.Enter;
      Gdk.Window.Get_Pointer (IntWindow, X, Y, Mask, win);
      Gdk.Threads.Leave;
      return DeltaX : constant Integer := Integer (X - IntPositionX) do
         IntPositionX := X;
      end return;
   end MouseXMotionCount;

   -----------------------
   -- MouseYMotionCount --
   -----------------------

   function MouseYMotionCount return Integer is
      X, Y : Glib.Gint;
      Mask : Gdk.Types.Gdk_Modifier_Type;
      win  : Gdk.Window.Gdk_Window;
      use type Glib.Gint;
   begin
      Gdk.Threads.Enter;
      Gdk.Window.Get_Pointer (IntWindow, X, Y, Mask, win);
      Gdk.Threads.Leave;
      return DeltaY : constant Integer := Integer (Y - IntPositionY) do
         IntPositionY := Y;
      end return;
   end MouseYMotionCount;

   ---------------------
   -- MousePixelRatio --
   ---------------------

   procedure MousePixelRatio (MXRatio, MYRatio : Integer) is
      pragma Unreferenced (MXRatio, MYRatio);
   begin
      if Debug then
         TP7.System.Writeln ("La fonction MousePixelRatio n'est pas définie !");
      end if;
   end MousePixelRatio;

   --------------------
   -- On_Mouse_Event --
   --------------------

   function On_Mouse_Event
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event)
      return   Boolean
   is
      pragma Unreferenced (Object);
      use type Gdk.Event.Gdk_Event_Type;
      LButtonNum : Integer;
   begin
      if Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Button_Press then
         LButtonNum                         := Integer (Gdk.Event.Get_Button (Event));
         IntButtons (LButtonNum).Pressed    := True;
         IntButtons (LButtonNum).LastXPress := Gdk.Event.Get_X (Event);
         IntButtons (LButtonNum).LastYPress := Gdk.Event.Get_Y (Event);
         if IntButtons (LButtonNum).Press_Count < Integer'Last then
            IntButtons (LButtonNum).Press_Count := IntButtons (LButtonNum).Press_Count + 1;
         else
            IntButtons (LButtonNum).Press_Count := 0;
         end if;
      end if;
      if Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Button_Release then
         LButtonNum                           := Integer (Gdk.Event.Get_Button (Event));
         IntButtons (LButtonNum).Released     := True;
         IntButtons (LButtonNum).LastXRelease := Gdk.Event.Get_X (Event);
         IntButtons (LButtonNum).LastYRelease := Gdk.Event.Get_Y (Event);
         if IntButtons (LButtonNum).Release_Count < Integer'Last then
            IntButtons (LButtonNum).Release_Count := IntButtons (LButtonNum).Release_Count + 1;
         else
            IntButtons (LButtonNum).Release_Count := 0;
         end if;
      end if;
      if Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Gdk_2button_Press then
         IntButtons (Integer (Gdk.Event.Get_Button (Event))).DoubleClic   := True;
      end if;
      if Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Gdk_3button_Press then
         IntButtons (Integer (Gdk.Event.Get_Button (Event))).TripleClic   := True;
      end if;
      if Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Scroll then
         IntScroll := To_Integer (Gdk.Event.Get_Direction (Event));
      end if;
      return False;
   end On_Mouse_Event;

begin
   TP7.Set_Mouse_Event (On_Mouse_Event'Access);
end TP7.Mouse;
