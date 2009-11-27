{ ANSI / ISO Pascal }

program NPTestC;

  procedure Bill_Findlay_2;
    type
      CODEFILE2 = FILE OF INTEGER;

      R = record
        case i: integer of
          1: (a: integer);
          2: (case x: char of
               'X': (b: char;);
               'Y': (c: real)
             )
      end;


    var f: CODEFILE2;
        p: ^R;

  begin

    {5}

    Get(f); i:=f^;

    {3}

    New(p,2,'Y');
    with p^ do c:= 3.9;
  end;

begin
end.
