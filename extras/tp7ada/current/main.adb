with Ada.Text_IO;
with Ada.Exceptions;
with Gtk.Main;
with Gdk.Threads;
with TP7.Crt;
with TP7.Test;
with Exemples_TP7;
with BGIDemo;
with Arty;

procedure main is

begin
   Gdk.Threads.G_Init;
   Gdk.Threads.Init;
   Gtk.Main.Init;
   TP7.Init (TP7.Test.Execute'Access);
   TP7.Test.Add (Exemples_TP7'Access, "Exemples TP7");
   TP7.Test.Add (BGIDemo'Access, "BGI Demo");
   TP7.Test.Add (Arty'Access, "Art Demo");
   TP7.Test.SelectAll;
   TP7.Crt.Init;
   Gdk.Threads.Enter;
   Gtk.Main.Main;
   Gdk.Threads.Leave;
exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
end main;
