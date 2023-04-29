{ The Ada translation compiles. Contents are nonsense. }

program NPTestA;

  procedure Enums_and_predef;
    type e = (odd,length,sqr,maxint);
    var v:e;
  begin
    v:= odd;
    v:= length;
    v:= sqr;
    v:= maxint
  end;

  procedure Constants;
    const
      a = True;
      b=false;
      c = 1234;
      d = $1234;
      e = 'c';
      f = 'Coucou';
      m1 = maxint;
      m2 = maxlongint;
  begin
  end;

  { Correct translation on 25-Jan-2003 }
  procedure Mask_predefined_identifiers;
    procedure Rocade;
      type
        T1 = Char;    { 'Char' should be translated into 'Character' }
        Char = Boolean; { 'Char' should NOT be translated into 'Character' }
        Longint = Integer;
      var
        c: Char; { 'Char' should NOT be translated into 'Character' }
        l: Longint;
        odd: Integer;
      function Chr(i:integer): integer;
      begin
        Chr:= 999
      end;
      function Sizeof(x: Longint): Byte;
      begin
        Sizeof:= 123
      end;
      function Sqr(x:real):real;
      begin
        Sqr:= 0.0
      end;
    begin
      c:= True;
      if Chr(123) = 999 then ; { "Chr" should appear "Chr" }
      if SizeOf(1) = 8 then ;
      if Sqr(1.0) = 0.0 then ;
      if odd = 1 then ;
    end;
  { All our type perversions forgotten }
  var
    c: Char; { 'Char' should be translated into 'Character' }
    l: Longint;
  begin
    c:= Chr(123);  { "Chr" should appear "Character'Val" }
    if Sqr(1.0) = 0.0 then ;
    if odd(1) then ;
  end;

  procedure Case_Of(c:Char);
  begin
    CASE c OF
      'A'..'Z', 'a'..'z': Writeln('Lettre ',UpCase(c));
      '0'..'9':           Writeln('Chiffre ');
      '+', '-', '*', '/': Writeln('Operateur ');
    ELSE
      Writeln('Caractere special');;;
    END

  end;

  procedure Record_1;
    TYPE
      obj_art_type=(txt,box,lin,vec,circ,oval,aux,putaux,bezier,bezvec,
                    unitl,spez,beginn,ende1,ende2,point,option);
      p_string = ^string;
      Str100=String[100];
      p_str100 = ^Str100;
      obj_type=record {JW,GH}
         x_pos, y_pos, width, height:real;
         picked:boolean;
      end; {record}

      Classe = (Num, Dat, Stx);
      Date   = RECORD
                 J, M, A: INTEGER { no ';' }
               END;

      Date2   = RECORD
                 J, M, A: INTEGER; { ';' }
               END;

      r0 = ^char;
      r1 = record f0: r0 end;
      r2 = record f1: r1 end;
      r3 = record f2: r2 end;
      r4 = record f3: r3 end;
      r5 = record f4: r4 end;
      r6 = record f5: r5 end;
    var
      ps1: p_string;
      ps2: p_str100;
      v6: r6;
      v1: r1;
  begin
    new(ps1);
    new(ps2);
    new(v6.f5.f4.f3.f2.f1.f0);
  end;

  procedure Pointeurs;
    { Creation of "Dispose" }
    type Type_1 = ^Char;     { Type_1 = ^Char }
         Type_2 = ^Type_1;   { Type_2 = ^Type_1 }

         Type_a11 = array [1..1] of integer;
         Type_3a = record a11: Type_a11 end;
         Type_3b = record end;

         Type_4 = array[1..5] of Type_3b;
         pType_4 = ^Type_4;
         Type_5 = record a: pType_4 end;
         pt5=^Type_5;
         Type_6 = array[1..5,1..2] of pt5;

    var  p1 : ^Char; { p1 : ^Char; }
         p2 : ^Type_2;   { p2 : ^Type_2;  }
         v6 : Type_6;
         v3a: Type_3a;

  begin
    New(p1);
    New(p2);
    p1^:= 'X';
    v3a.a11[1]:= 2;            { v3a.a11[1]:= 2;         }
    New(v6[v3a.a11[1],2]);     { New(v6[v3a.a11[1],2]);    }
    New(v6[v3a.a11[1],2]^.a);  { New(v6[v3a.a11[1],2]^.a); }
  end;

  { 8-Jan-2003 : relaxed subrange_type }

  procedure Bornes;

    const m=10; n=99;

    type

      asd = array[m .. n] of integer;

      smd = -m .. m;

      sd = m .. n;

    var
      vsd1, vsd2   :   m .. n;
      vsd3:m..n;
      vsd4 :m..n;
    type

      sa = 0..m;
      sb = 0..9+3;
      sc = 0..m+1;

      aa  = array[0..m] of integer;
      asa = array[ sa ] of integer;
      ab  = array[0..9+3] of integer;
      ac  = array[0..m+1] of integer;
      ad  = array[ sa, -m .. m, sb ] of integer;
    var i: integer;
     j: asa;
  begin
    i:= 0
  end;

  procedure Id_Field_A;
    const aaa: String ='aaa';
    type R = record length,chr,ord: integer end;
    var v: R;
  begin
    v.chr:= v.ord;
    { Should appear as "v.chr:= v.ord",
      not as "v.Character'Val:= v.Character'Pos" }
    v.ord:= ord('X'); { one "ord", "one...'Pos" }
  end;

  procedure Niklaus_With;

    type
      obj_art_type=(txt,box,lin,vec,circ,oval,aux,putaux,bezier,bezvec,
                    unitl,spez,beginn,ende1,ende2,point,option);

      p_string = ^string;

      Object_1=record
         x_pos, y_pos, width, height:real;
         picked:boolean;
      end;

      Object_2=record
         src,dst: Object_1
      end;

      Object_3=record
         z_pos:real;
      end;

      t1 = record x,y: integer end;
      t2 = array(.1..2.) of t1;
      t3 = record i,j: t2 end;
      t4 = record a,b: t3; end;

      R = record a : Integer; end;
      RR = record a : R; end;
      RRR = record a : RR; end;
      RRRR = record a : RRR; end;

    var
      o1,o11,o12,o13,o14: Object_1;
      picked: Boolean;
      o2: Object_2;
      o3: Object_3;
      art: obj_art_type;

      u3: t3;
      u123: t3;
      ru4: record u,v: t4; end;

      aaaa : RRRR;

    procedure x_pos; begin end;

    procedure Bob(a1,b1: Object_1; var a3,b3: Object_3);
    begin
      with b3,a1,b1,a3 do  {"with b3,a1,b1,a3 do", 2 types}
    end;

  begin
    WriteLn('Testing WITH');

    picked:= False;
    o1.picked:= True;
    x_pos;
    with o1 do y_pos:= 0.6;
    with o1 do begin
      picked:= False;  { == o1.picked:= False }
      x_pos:= 1.0;
      with o2 do begin
        src.y_pos:= 2.0;
        with o3 do z_pos:= src.y_pos+1.0
      end
    end;

    WriteLn ('WITH aaaa, a, a, a DO a := 10312;');
    aaaa.a.a.a.a := 4503;  {should be overwritten by next statement}
    with aaaa, a, a, a do a := 10312;
    WriteLn ('aaaa.a.a.a.a = [should be 10312] ', aaaa.a.a.a.a:0);

    with u3 do i[2].x:= 2;
    with u123,o1,o3 do { <-- 3 different types }
      i[2].x:= 2;
    ru4.u.b.i[2].x:= 9;

    WriteLn( 'picked=(F)', picked );
    WriteLn( 'o1.picked=(F)', o1.picked );
    WriteLn( 'o1.x_pos=(1)', o1.x_pos );
    WriteLn( 'o2.y_pos=(2)', o2.src.y_pos );
    WriteLn( 'o3.z_pos=(3)', o3.z_pos );

  end;

  procedure WITH_and_hiding;
    type
      R = record c : Integer end;
    var
      a, b : R;
      c : Integer;
  begin
    c := 0;
    a.c := 0;
    b.c := 0;
    with a, b do
      { b.c hides a.c, which hides c }
      c := 1;
    Write ('WITH and hiding. (c=', c:0, ', a.c=', a.c:0, ', b.c=', b.c:0);
    WriteLn (')   TP6 and FPC 3.0.2 produce: (0, 0, 1)');
    with b, a do
      { a.c hides b.c, which hides c }
      c := 2;
    Write ('WITH and hiding. (c=', c:0, ', a.c=', a.c:0, ', b.c=', b.c:0);
    WriteLn (')   TP6 and FPC 3.0.2 produce: (0, 2, 1)')
  end;

  var x: Double;

begin
  Case_of('u');
  Niklaus_With;
  WITH_and_hiding;
  { Concat(a,b) becomes "&"(a,b) : }
  writeln(Concat('Con',Concat('c','at')));
  { Ln(a) becomes Log(a) : }
  x:= 2.718;
  writeln('Ln ',x,ln(x));
end.
