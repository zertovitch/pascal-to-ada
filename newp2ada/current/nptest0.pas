{> Pascal-S, Pascal-T and P2ada test }

{> This Pascal file has successful compilation and runs under Pascal-S & TP6. }

{> The Ada translation by P2Ada has successful compilation and runs. }


{ --- Absolutely Normal Pascal (Pascal-S compatible) --- }

program Test(input,output);

  const cxmax  = 2000;     
        amax   = 1000;
        xamax = -amax;

        truth   = True;
        letter  = 'x';
        number  = 666;

  type MySet = array[ 400 .. 2000 ] of boolean;

       Tabloint = array [0..amax] of Integer; { Comment
       on two or
       tree lines! }

       OtherSet = MySet;
       JetSet = OtherSet;
       JetSets = array[1..10] of JetSet;

  var m : Tabloint;

      i,j,k,l: Integer;
      
      c1,c2: record            { A sort of complex... }
               r,i: Integer;
             end;

  { 10-Jan-2003 - Output must be: "Coucou !" }
  procedure Guillemets;
  begin
    write('"');
    write('Coucou !"');
    write(', dit-il, ');
    write('"C''est un Coucou !"');
     writeln
  end;

  procedure Apostrophes;
  { 10-Jan-2003 - Output must be:
  'Coucou !'
  'Coucou !'
  'C'est le coucou !'
  5 apostrophes: '''''
  }
  begin
    write(''''); write('Coucou !'''); writeln;
    write('''Coucou !'''); writeln;
    write('''C''est le coucou !'''); writeln;
    write('5 apostrophes: ');
    write('''''''''''');
    writeln
  end;

  function TrueFalse(v:Boolean): char;
  begin
    if v then
      TrueFalse:= 'T'
    else
      TrueFalse:= 'F'
  end;

  function TestNestedFunctions: integer;
    function FNestor1: integer;
      function FNestor2: integer;
        function FNestor3: integer;
        begin
          FNestor3:= 7 * (2+ number + FNestor1 * 3);
          FNestor2:= 1 + FNestor3;
          FNestor1:= FNestor2
        end;
      begin
        FNestor2:= FNestor3
      end;
    begin
      FNestor1:= FNestor2
    end;
  begin
    TestNestedFunctions:= FNestor1
  end;

  procedure Nestor1;
    type t = record a:Integer; b:Char; end;
    var i: Integer;
    procedure Nestor2;
      type t = record a:Integer; b:Char; end;
      var i: Integer;
      procedure Nestor3;
        type t = record a:Integer; b:Char; end;
        var i: Integer;
        procedure Nestor4;
          type t = record a:Integer; b:Char; end;
          var i: Integer;
          procedure Nestor5;
            type t = record a:Integer; b:Char; end;
            var i: Integer;
            procedure Nestor6;
              type t = record a:Integer; b:Char; end;
              var i: Integer;
            begin
              i:= 0;
              repeat
                i:= i+1;
              until i=10
            end;
          begin
            i:= i+1;
            Nestor6
          end;
        begin
          i:= i+1;
          Nestor5
        end;
      begin
        i:= i+1;
        Nestor4
      end;
    begin
      i:= i+1;
      Nestor3
    end;
  begin
    i:= i+1;
    Nestor2
  end;

  procedure Vide;
  begin end;

  procedure Affsigne(x,y:integer);
    const uu=2;
  begin
    if x>0 then
      writeln('positif!')
    else begin
      writeln('beuh    ')
    end
  end;
 
  function factorielle(n:integer): integer;
  begin
    if n>1 then
      factorielle:= factorielle(n-1) * n
    else
      factorielle:= 1
  end;

  {
  procedure Typ(x: record u,v: integer end);
  begin
    x.u:= 9
  end;
  }

  function fib(n : integer): integer;
  begin
    if (n <= 2) then
      fib := 1
    else
      fib := fib(n-1) + fib(n-2);
  end; {fib}
 
  procedure Primetest;
    VAR i,j,max:integer;

    function prime(i:integer): Boolean;
    var j:integer;
        stillprime:boolean;
    begin
      j:=2;
      stillprime:=true;
      while ((j*j<=i) and stillprime) do
      begin
        stillprime:= (i mod j)<>0;
        j:=j+1;
      end;
      prime:= stillprime;
    end;
 
  begin
    writeln;
    repeat
      write('Primetest: max ? (0:exit) ');
      readln(max);  j:=0;
      
      i:=1; while i<=max do begin
      { for i:=1 to max do }
        if prime(i) then  begin  j:=j+1  end;

      { absence of 'for' in Pascal-S }
        i:= i+1
      end;
      writeln;
      writeln('Between 1 and ',max,' is there ',j,' primes');
    until max=0;
  end;

  function hex2int(ch:char) : integer;
  { Coverts character representing hexadecimal number to its integer
    value in a machine-independent way. }
  begin
     if (ch >= '0') and (ch <= '9') then
        hex2int := ord(ch)-ord('0')
     else
        hex2int := ord(ch)-ord('A')+10
  end;
  
  function int2hex(i:integer) : char;
  { Inverse of hex2int }
  begin
     if i <= 9 then
        int2hex := chr(i+ord('0'))
     else
        int2hex := chr(i-10+ord('A'))
  end;

  { Nombres de Syracuse. Idee transmise par Eric Batard. }

  procedure Syracuse(ii: integer; var m: tabloint);
    var it,i: integer;
  begin
    i:= ii;
    it:= 0;
    m[it]:= i;
    it:= it+1;
    repeat
      if i mod 2 = 0 then
        i:= i div 2
      else
        i:= (3*i+1) div 2;
      m[it]:= i;
      it:= it+1;
    until (i=1) or (it>amax)
  end {Syracuse};

begin
  i:= 1234;
  j:= 4321;

  c1.r:=  4455;
  c2.i:= -1234;

  k:= i+j;
  k:= k div 2;

  writeln('Ver 5555 / 2',k);
  m[8]:= 9;

  Apostrophes;
  Guillemets;

  i:= 8;
  while i <= 13 do begin
    Writeln(i,
      '-> Hex -> ',int2hex(i),' -> Int -> ',
      hex2int(int2hex(i)),'  Odd: ',TrueFalse(Odd(i)));
    i:= i+1
  end;

  i:= 0;
  while i <= amax do begin
    m[i]:= amax-i;
    i:= i+1;
  end;

  writeln('Les',' ','nombres',' de ','Syracuse');
  Syracuse(76,m);

  i:= 0;
  repeat
    writeln(i:4,' -> ',m[i]:0);
    i:= i+1;
  until (i > amax) or (m[i-1]=1);

  i:= 1 + 0;
  Affsigne(i,1);
  Affsigne(-3,0);

  writeln; write('while: ');

  while i <= 10 do begin
    write(i:5,',');
    i:= i+1;
  end;

  writeln; write('repeat: ');

  repeat
    write(i:4,',');
    i:= i+1;
  until i>20;

  writeln; write('Factorielles: ');

  i:= 1;
  while i <= 6 do begin
    write( factorielle(i):3,',');
    i:= i+1;
  end;

  writeln; writeln;
  write('Fibonacci: ');

  i:= 1;
  while i <= 22 do begin
    write( fib(i):0,',');
    i:= i+1;
  end;


  { for i:=1 to 10 do; }

  Primetest;

  write('Enter!');
  if eoln then;
  readln;
end.
