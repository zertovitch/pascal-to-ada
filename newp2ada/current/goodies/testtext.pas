uses Ada_Text_IO;

  var
    t: Text;
    c: Char;

begin
  Create(t,out_file,'$$test$$');
  WriteLn(t,'Hello!');
  New_Line(t);
  Write(t,pi,'finish here ->');
  Close(t);

  Open(t,in_file,'ada_text.pas');
  while not eof(t) do begin
    Read(t,c);
    Write(c)
  end;
  Close(t)
end.
