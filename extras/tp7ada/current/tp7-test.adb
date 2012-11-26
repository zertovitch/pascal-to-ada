-------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : tp7-test.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 1.2a
-- DATE DE LA DERNIERE MISE A JOUR  : 19 juin 2012
-- ROLE DU CSU                      : Unité de test.
--
--
-- FONCTIONS EXPORTEES DU CSU       :
--
-- FONCTIONS LOCALES DU CSU         :
--
--
-- NOTES                            : Ada 2005, GTKAda 2.24.2
--
-- COPYRIGHT                        : (c) Pascal Pignard 2011-2012
-- LICENCE                          : CeCILL V2 (http://www.cecill.info)
-- CONTACT                          : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Gtk.Check_Button;
with Gtk.Separator;
with TP7.System;
with Glib.Convert;
with Ada.Containers.Vectors;

package body TP7.Test is

   type TestProcRec is record
      Proc     : TPProc;
      IsMarked : Gtk.Check_Button.Gtk_Check_Button;
   end record;
   package TestProcVect is new Ada.Containers.Vectors (Positive, TestProcRec);
   TestProcs  : TestProcVect.Vector;
   HSeparator : Gtk.Separator.Gtk_Hseparator;

   ---------
   -- Add --
   ---------

   procedure Add (TestProc : TPProc; TestName : TPString) is
      CheckButton : Gtk.Check_Button.Gtk_Check_Button;
   begin
      if TestProcs.Is_Empty then
         Gtk.Separator.Gtk_New_Hseparator (HSeparator);
         Add_Ctrl (HSeparator);
      end if;
      Gtk.Check_Button.Gtk_New (CheckButton, Glib.Convert.Locale_To_UTF8 (To_String (TestName)));
      Add_Ctrl (CheckButton);
      TestProcs.Append ((TestProc, CheckButton));
   end Add;

   ---------------
   -- SelectAll --
   ---------------

   procedure SelectAll is
      procedure SelectTest (Position : in TestProcVect.Cursor) is
      begin
         TestProcVect.Element (Position).IsMarked.Set_Active (True);
      end SelectTest;
   begin
      TestProcs.Iterate (SelectTest'Access);
   end SelectAll;

   -------------
   -- Execute --
   -------------

   procedure Execute is
      procedure ExecuteTest (Position : in TestProcVect.Cursor) is
      begin
         if TestProcVect.Element (Position).IsMarked.Get_Active then
            if TP7.Debug then
               TP7.System.Writeln
                 ("Test of " +
                  Glib.Convert.Locale_From_UTF8
                     (TestProcVect.Element (Position).IsMarked.Get_Label));
            end if;
            TestProcVect.Element (Position).Proc.all;
         end if;
      end ExecuteTest;
   begin
      -- Can't use Iterate because of Program_Error when finalization during aborting
      -- TestProcs.Iterate (ExecuteTest'Access);
      for Ind in 1 .. Positive (TestProcs.Length) loop
         ExecuteTest (TestProcs.To_Cursor (Ind));
      end loop;
   end Execute;

end TP7.Test;
