with TP7.System;
with TP7.Crt; -- if you comment this line then I/O use stdinout

procedure Hello_GTKAda is
   use TP7, TP7.System;
   N : Byte;

begin
   Write ("How many hello ? ");
   Readln (N);
   for I in 1 .. N loop
      Writeln ("Hello with GTKAda console.");
   end loop;
end Hello_GTKAda;
