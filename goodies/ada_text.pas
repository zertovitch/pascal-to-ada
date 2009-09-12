{ TP unit for allowing a smooth translation via P2Ada }

{ 1/ use Create / Open in your TP program X.pas instead
     of Assign / Reset / Rewrite !
  2/ run "p2ada -EAda_Text.def Ada_Text.pas >nul"
  3/ run "p2ada -IAda_Text.def X.pas >X.ada" for your program

}


unit Ada_Text_IO;

interface

  type File_mode = (in_file, out_file); { Bogus for TP }

  procedure Create( var f: Text; mode: File_mode; name: String );
  procedure Open( var f: Text; mode: File_mode; name: String );

  { Provisory: workaround of the P2Ada bug with ReadLn(f) / WriteLn(f) }

  procedure Skip_Line( var f: Text );
  procedure New_Line( var f: Text );

implementation

  procedure Create( var f: Text; mode: File_mode; name: String );
  begin
    Assign(f,name);
    Rewrite(f)
  end;

  procedure Open( var f: Text; mode: File_mode; name: String );
  begin
    Assign(f,name);
    case mode of
      in_file : Reset(f);
      out_file: Rewrite(f)
    end
  end;

  procedure Skip_Line( var f: Text );
  begin
    ReadLn(f)
  end;

  procedure New_Line( var f: Text );
  begin
    WriteLn(f);
  end;

end.