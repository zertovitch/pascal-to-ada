with Gtk.Main;
with Gdk.Threads;
with TP7;
with Hello_GTKAda;

procedure main_mini is

begin
   Gdk.Threads.G_Init;
   Gdk.Threads.Init;
   Gtk.Main.Init;
   TP7.Init (Hello_GTKAda'Access);
   Gdk.Threads.Enter;
   Gtk.Main.Main;
   Gdk.Threads.Leave;
end main_mini;
