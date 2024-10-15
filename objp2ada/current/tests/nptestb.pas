{ This is a test of the Import/Export of definitions }

{ P2Ada NPTest2.pas -ENPTest2.def >NPTest2.ada }
{ P2Ada NPTestB.pas -INPTest2.def >NPTestB.adb }

program NPTestB;

  uses NPTest2;

  var
   p: ptr_obj_type; { From NPTest2 }
   b:byte;
   last,next: ptr_obj_type;

begin
  New(p);
  with p^ do begin
    x_pos:= 0.0;
    b:= 1;
    picked:= False;
    inc(b);
    next:= NIL; { <- p^.next }
    picked:= next=last
  end;
  next:= NIL { <- next }
end.