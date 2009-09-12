  {     This is PURE JUNK! (but it compiles under TP6...)
        just a test of new p2ada features GdM VIII.1999      }
  
{ (no) program ;-) }
  {$N+}

  uses Graph;

  procedure Prefixed_Calls;
   { 14-Dec-2003 }
   { unit.procedure, unit.function.
     Instead of unit it can also be generally a variable access (OO) }
    var x: Integer;
  begin

    {** 1/ unit.function (works already with CC's modifs) }
    x:= Graph.GetMaxX;
    x:= Graph.TextWidth('Coucou');

    {** 2/ unit.procedure }
    Graph.InitGraph(x,x,'');
    Graph.ClearViewPort;
  end;

  procedure Postfixed;
  begin
    { Pascal:  Odd( SizeOf( Char ) * Sqr( Hi(1000) )) }
    if Odd( SizeOf( Char ) * Sqr( Hi(1000) )) then
  end;

  procedure Files_of;
    type
      Person = record
                 FirstName: string[15];
                 LastName : string[25];
                 Address  : string[35];
               end;
      PersonFile = file of Person;
      NumberFile = file of Integer;
      SwapFile = file;  { File without type (Borland) }

    var
      vPersonFile : file of Person;
      vNumberFile : file of Integer;
      vSwapFile : file;  { File without type (Borland) }
  begin
  end;

  procedure Variant;
    TYPE
      obj_art_type=(txt,box,lin,vec,circ,oval,aux,putaux,bezier,bezvec,
                    unitl,spez,beginn,ende1,ende2,point,option);
      ptr_obj_type=^obj_type;
      p_string = ^string;
      obj_type=record {JW,GH}
         x_pos, y_pos, width, height:real;
         next:ptr_obj_type;
         picked:boolean;
         case art:obj_art_type of
            txt,putaux,box:(inhalt:p_string; adjust:string[2];
                     dash,solid:boolean; dash_dimen:real;);
            lin,vec:(em:boolean; h_slope, v_slope:integer; len:real;);
            circ:(rad:real; fill:boolean;);
            oval:(lux,luy:real; part:string[2];);
            bezier,
            bezvec:(xx_pos,yy_pos,num:real;h_sl,v_sl:integer;);

      end; {record}

      Classe = (Num, Dat, Stx);
      Date   = RECORD
                 J, M, A: INTEGER { no ';' }
               END;

      Date2   = RECORD
                 J, M, A: INTEGER; { ';' }
               END;

      Truc_Semicolon = RECORD
                 Nom:  STRING[10];
                 CASE Kind: Classe OF
                   Num: (N: REAL;);     {';' inside variant}
                   Dat: (D: Date);
                   Stx: (S: STRING;);   {';' inside variant}
                 END;

      Truc_Semicolon2 = RECORD
                 Nom:  STRING[10];
                 CASE Kind: Classe OF
                   Num: (N: REAL);     {no ';' inside variant}
                   Dat: (D: Date);
                   Stx: (S: STRING);   {no ';' inside variant}
                 END;

      Truc   = RECORD
                 Nom:  STRING[10];
                 CASE Kind,Enfant: Classe OF
                   Num: (N: REAL);     {no ';' inside variant}
                   Dat: (D: Date);
                   Stx: (S: STRING)    {no ';' inside variant}
                   {no terminal ';'}
                 END;


      Muche = record
                case i: Integer of
                  1223: (X: Double);
                  ord('O'): (O: String)
              end;

      Classe2 = (Num2, Dat2, Str);

  begin
  end;

 { 14-Jan-2003: absolute }
 procedure Vodka;
  type
   Str10 = String[10];
   Twins = record S1,S2: Str10 end;
   Peaks = array[1..10] of Twins;
  var
   TP: Peaks;
   S: Str10 absolute TP; { TP[1].S1 not accepted !}
 begin
  S:= 'Glou glou!';
  WriteLn( TP[1].S1 );
 end;

  { 12-Jan-2003 }
  procedure Conditional;
(*---->*)
    procedure Options;
      function Useless: Boolean;  {$F+}
      begin
        WriteLn('[Useless!]');
        Useless:= True
      end;
    begin
      WriteLn('1/ Borland: the only clean "or else" / "and then" !');
      {$ifopt B+}
        {$B-}
      if true or Useless then
        {$B+}
      {$else}
      if true or Useless then
      {$endif}

      WriteLn('2/ Borland: the only clean "or" / "and" !');
      {$ifopt B-}  {a comment} {$define a_directive}
        {$B+}      {a comment} {$define a_directive}
      if not (false and Useless) then
        {$B-}
      {$else}
      if not (false and Useless) then
      {$endif}
      
      {zBOOLEVAL ON}
      ; if true or Useless then
      {zBOOLEVAL OFF}
      ; if true or Useless then
      
    end;


    procedure Defines;
    begin
      WriteLn(
      (*$ifdef cOucOu*)
       'Coucou',
        {$ifndef chouette}
          ', Pas chouette'
        {$else}
          ', Chouette'
        {$endif}
      {$else}
        'Ni coucou ni chouette'
      {$endif}
      );

      {$define  tralala}
      {$define tsoin}
      {$define tsoin2}
      {$ifdef traLALA}
        WriteLn('Tralala');
      {$endif}
      {$ifdef tsoin}
        WriteLn('Tsoin tsoin');
      {$endif}
      {$undef tsoin2}
      {$ifdef tsoin2}
        { "Dead" part, but this comment should remain }
        WriteLn('Tsoin tsoin2');
      {$endif}
      {$ifdef a_directive}
         WriteLn('A_directive');
      {$endif}
    end;

  begin
    Options;
    Defines;
  end;


{-------------- **** }
  proceDure Test_Inline;   
    PROCEDURE FillWord(VAR Dest; Count: WORD;
      Data: WORD);
    BEGIN
      INLINE(
        $C4/$7E/<Dest/   { LES   DI,Dest[BP]  }
        $8B/$4E/<Count/  { MOV   CX,Count[BP] }
        $8B/$46/<Data/   { MOV   AX,Data[BP]  }
        $FC/             { CLD                }
        $F3/$AB);        { REP   STOSW        }
    END;
    { Excerpt from a public domain piece of code }    
    cONSt
      TABLESIZE   =   8191;    
    tyPe
      CodeRec     =  Record                { Code Table record format...         }
                        Child   : Integer; { Addr of 1st suffix for this prefix  }
                        Sibling : Integer; { Addr of next suffix in chain        }
                        Suffix  : Byte;    { Suffix character                    }
                     end {CodeRec};
      CodeArray   =  Array[0..TABLESIZE] of CodeRec; { Define the code table     }
      TablePtr    =  ^CodeArray;
    vaR
      CodeTable   :  TablePtr;
    FunCtion Table_Lookup(    TargetPrefix : Integer;
                              TargetSuffix : Byte;
                          Var FoundAt      : Integer   ) : Boolean;
    { --------------------------------------------------------------------------- }
    { Search for a Prefix:Suffix pair in our Symbol table.  If found, return the  }
    { index value where found.  If not found, return FALSE and set the VAR parm   }
    { FoundAt to -1.                                                              }
    { --------------------------------------------------------------------------- }
    BegIn
       InliNe(
                                {;}
                                {; Lookup an entry in the Hash Table.  If found, return TRUE and set the VAR}
                                {; parameter FoundAt with the index of the entry at which the match was found.}
                                {; If not found, return FALSE and plug a -1 into the FoundAt var.}
                                {;}
                                {;}
                                {; Register usage:}
                                {;   AX - varies                     BL - holds target suffix character}
                                {;                                   BH - If search fails, determines how to}
                                {;                                        add the new entry}
                                {;   CX - not used                   DX - holds size of 1 table entry (5)}
                                {;   DI - varies                     SI - holds offset of 1st table entry}
                                {;   ES - seg addr of hash table     DS - program's data segment}
                                {;}
                                {;}
         $8A/$5E/<TargetSuffix/ {            mov byte    bl,[bp+<TargetSuffix]   ;Target Suffix character}
         $8B/$46/<TargetPrefix/ {            mov word    ax,[bp+<TargetPrefix]   ;Index into table}
         $BA/$05/$00/           {            mov         dx,5                    ;5 byte table entries}
         $F7/$E2/               {            mul         dx                      ;AX now an offset into table}
         $C4/$3E/>CodeTable/    {            les         di,[>CodeTable]         ;Hash table address}
         $89/$FE/               {            mov         si,di                   ;save offset in SI}
         $01/$C7/               {            add         di,ax                   ;es:di points to table entry}
                                {;}
         $B7/$00/               {            mov         bh,0                    ;Chain empty flag (0=empty)}
         $26/$83/$3D/$FF/       {        es: cmp word    [di],-1                 ;Anything on the chain?}
         $74/$33/               {            jz          NotFound                ;Nope, search fails}
         $B7/$01/               {            mov         bh,1                    ;Chain empty flag (1=not empty)}
                                {;}
         $26/$8B/$05/           {        es: mov word    ax,[di]                 ;Get index of 1st entry in chain}
         $89/$46/<TargetPrefix/ {Loop:       mov word    [bp+<TargetPrefix],ax   ;Save index for later}
         $BA/$05/$00/           {            mov         dx,5}
         $F7/$E2/               {            mul         dx                      ;convert index to offset}
         $89/$F7/               {            mov         di,si                   ;es:di points to start of table}
         $01/$C7/               {            add         di,ax                   ;es:di points to table entry}
                                {;}
         $26/$3A/$5D/$04/       {        es: cmp byte    bl,[di+4]               ;match on suffix?}
         $74/$0D/               {            jz          Found                   ;Yup, search succeeds}
                                {;}
         $26/$83/$7D/$02/$FF/   {        es: cmp word    [di+2],-1               ;any more entries in chain?}
         $74/$15/               {            jz          NotFound                ;nope, search fails}
                                {;}
         $26/$8B/$45/$02/       {        es: mov word    ax,[di+2]               ;get index of next chain entry}
         $EB/$E1/               {            jmp short   Loop                    ;   and keep searching}
                                {;}
         $C6/$46/$FF/$01/       {Found:      mov byte    [bp-1],1                ;return TRUE}
         $C4/$7E/<FoundAt/      {            les         di,[bp+<FoundAt]        ;get address of Var parameter}
         $8B/$46/<TargetPrefix/ {            mov word    ax,[bp+<TargetPrefix]   ;get index of entry where found}
         $26/$89/$05/           {        es: mov         [di],ax                 ;and store it}
         $EB/$0C/               {            jmp short   Done}
                                {;}
         $C6/$46/$FF/$00/       {NotFound:   mov byte    [bp-1],0                ;return FALSE}
         $C4/$7E/<FoundAt/      {            les         di,[bp+<FoundAt]        ;get address of Var parameter}
         $26/$C7/$05/$FF/$FF);  {        es: mov word    [di],-1                 ;and store a -1 in it}
                                {;}
                                {Done:}
                                {;}

    eNd {Table_Lookup}; 
  bEGin
  enD;
{-------------- **** }

{##---}
    procedure Asm_test;

      procedure Pure_asm; assembler;
      asm
        xor ax,ax;mov bx,0
        mov cx,0{hello cx!}inc ax
      end;
  {hello cx!
           encore 1
           encore 2}
      procedure Insert_asm;
      begin
        if true then
          asm xor ax,ax;mov bx,0
           mov cx,0{hello cx!
           encore 1
           encore 2}inc ax end
        else
          asm xor ax,ax;mov bx,0
           mov cx,0{hello cx!}inc ax end
      end;

    begin
      Pure_asm;
      Insert_asm
    end;
{##---}

  {13-Jan-2003: This is standard Pascal !}

  procedure Set_and_array_of_enum;
    { You can define nice useless types: }
    type
      set_enum = set of (a,b,c,d);
      array_enum = array[ (x,y,z) ] of Integer;
  begin
  end;


  { 9-Jan-2003 : '^X' characters in Borland Pascal }

  procedure Hats;

    const

      ch2: Char = ^#;    ch102='#';
      ch3: Char = ^^;    ch103='^';
      ch4: Char = ^A;    ch104='A';
      ch5: Char = ^a;    ch105='a';
      nom2: String = 'toto'^M'titi';

    type pstring = ^string;

    var u: ^char;

  begin
    writeln('toto'^M'titi')
  end;

  { 9-Jan-2003 : Inc(x[,y]), Dec(x[,y]): intrinsic in Borland Pascal }
    
  procedure Inc_Dec;
    var i,j: Integer;
  begin
    Inc(i);       { add  1 }
    Inc(i,9);     { add  9 }
    Inc(i,-9);    { add  -9 }
    Dec(i);       { sub  1 }
    Dec(i,8);     { sub  8 }
    Dec(i,8+j);   { sub  (8+j) }
  end;

  procedure Label_test; { 8-Jan-2003 }
    type t = Integer;
    var i, univ: t;
    label U,1,2,3,4,x,_,_____O;
  begin
    _____O: ;  {';' could be ommited (2 labels)!}

    1: i:= 0; goto _;
    2: repeat
    U:; {';' could be ommited (2 labels)!}
    3:   i:= i+1;
    4: until i=10;
    x: i:=1111; goto 2;
    _: i:=1010
  end;

  procedure ___; begin end; { A nice one...}

  procedure Test_special_procedure_calls;
    type Z = procedure;
         PZ = ^Z;
         APZ = array[1..5] of PZ;
    var a: ^Z;
        b: PZ;
        t: APZ;
  begin
    a^;
    t[5]^;
    Line(1,2,3,4);
    {Graph.Line(1,2,3,4);}
  end;

  procedure Test_External; external;
  {$L Test_ext.obj}

  const
    ch: Char = #32;
    nom: String = 'toto'#96'titi';

  const m=20; excl=#33;
  blorg= 'Blorg'+'oo';
  blorg2='Blorg'#33#34'zoo';

  type univ = integer;

  const CIndicator = #2#3;

  type
    String200 = String[200];
    P_String200 = ^String200;

  var
    p1: P_String200; { direcly translated }
    p2: ^String200;  { this needs a workaround }

  procedure Test_Downto_Reverse;
  var J, CHI: Integer;
  begin
    for j:= 123 to 456 do;
    for j:= 123 downto 456 do; { 0 iterations }
    for j:=chi+0 to chi+1 do;
    for j:=chi+1 downto chi-1 do;
    FOR J := INTEGER(CHI) - 1 DOWNTO 1 DO BEGIN
    END
  end;

  function Test_exit: Integer;
  type Enum = (a,b,c,d,e,f,g,h,univ);
  begin
    if a in [b,c,d..f,h] then ;
    Test_exit:= 1234;
    exit;
    Test_exit:= 5678;
  end;

  { TP violates Pascal rule where order is const-type-var-proc/func: }
  var s: string;
      c: char;
      s200  : String[200];
      s200_a: String200;

  const n=10;


  {19-Jan-2002: fixed translation of TP's subprogram types}

  type proc1 = procedure;
  type proc2 = procedure(x:integer);
  type func1 = function: string;
  type func2 = function(x:integer;y:univ): string;

  type p_proc1 = ^proc1;
  var ppr1: p_proc1;

  {$F+} {TP: Far pointers needed for compiling procedure references!}
  
  type fmath = function (x : double) : double;

  function sinc(x : double) : double;
  begin
    sinc:= x * x
  end;
  
  procedure fourier(t : double; f : fmath);
    var x: double;
  begin
    ppr1^;      { <-- pointed_procedure_call }
    x:= f(t);
    writeln(x)
  end;

  procedure Test_fourier;
  begin
    fourier(10.0, sinc)  { TP: We need $F+ to compile it! }
  end;

  procedure grounf; begin s:=c; end;

  { ---VARiables--- }

  var i,j,k:integer;
  var s80: string[80];
      v2,v3,v4 : array[1..200] of byte;
  c1,c2: record r,i: Real; end;

  {-- More complicated --}
  var compliq1, compliq2: array[-4..5] of record a: string[123]; b:byte end;

  { ---CONSTants (or initialized variables--- }

  const c3: record r,i: Real; end = (r:1;i:2);

  const a: array[1..2,1..4] of integer= ((1,n,3,4),(5,6,7,8));
  const np=7;
  pnp: array[1..np] of string[80]=
 ('PKLITE.EXE -bn', 'PKLITE2.EXE -bn', 'DIET.EXE',
  'WWPACK.EXE p -b-', 'WWP305.EXE p -b-', 'UPX.EXE -9 --8086 --best',
  'APCK_X_X.BAT');
  const b: record x,y:byte end= (x:3*1;y:2);

  {-- More complicated --}

  const e: array[1..2] of record x,y:byte end= ((x:3*1;y:2),(x:5;y:6));
  const d: array[1..2] of record x,y:array[0..1] of byte end =
           ((x:(3*1,4);y:(2,1)),(x:(5,2);y:(6,0)));
  const f: array[1..5] of word = (1,2,3,4,5);

  procedure Test_with;
  begin
    with c1 do r:= 6;
    with c1 do begin i:= 8; r:= 6; end
  end;

  function zembla: real; begin zembla:= 7 xor 5 end;

    function  HexStr(var num; byteCount: Byte): string;
      begin HexStr:= 'Beurk!'; end;

  function tasm:word; far; assembler;
    var near: byte; far:word;
    assembler: integer;
     asm
      cli
      xor ax,ax
      sti
     end;

  { 29-Jan-2003 }
  procedure Incomplete_types;
    type
      p_List1 = ^List1;
      p_List2 = ^List2; 
      List1 = record
        item1 : Char;
        next1 : p_List1;        
        ref2  : p_List2
      end;
      List2 = record
        item2 : Integer;
        next2 : p_List2;
        ref1 : p_List1
      end;
    var
      p1: p_List1;
  begin
    New(p1);
    with p1^ do begin
      item1:= 'Z';
      next1:= NIL;
      New(ref2);
      with ref2^ do begin
        item2:= 1000;
        next2:= NIL;
        ref1:= p1
      end
    end
  end;

  type r = record a,b:integer end;
  type v = record a,b: integer; case c,d:integer of 1:(e:integer)end;
  type ra = record a,b: array[1..2] of integer end;
  type rr = record a,b: record c,d:integer end end;

  procedure Haddock;
    { Where are ad-hoc types needed }
    var
      a: array[1..100] of Char;
      r: record x,y: Integer end;
      p: ^Char;
      s: Set of Char;
      f: File of Integer;

  begin
    s:= [];
    s:= ['A'..'Z','0'..'9','_']
  end;

  procedure Array_bracketing;
    type
      c = boolean;
      ac = array[1..10] of c;            

      a = array[1..3] of Integer;
      aa = array[1..2] of a;
      aaa = array[1..2,1..3] of Integer;

    var
      va: a;
      vaa: aa;
      vaaa: aaa;
      i,j: Integer;

  begin
    Write('Array bracketing: ');
    for i:= 1 to 2 do
      for j:= 1 to 3 do begin
        vaa[i][j]:= 0;
        vaaa[i,j]:= 0
      end;

    vaa[2][3]:= 125;          { "Normal" : [2][3] }
    if vaa[2,3] = 125 then    { "Alternative" : [2,3] }
      Write('test 1 passed, ')
    else
      Write('test 1 failed, ');

    vaaa[2,3]:= 888;          { "Normal" : [2,3] }
    if vaaa[2][3] = 888 then  { "Alternative" : [2][3] }
      WriteLn('test 2 passed')
    else
      WriteLn('test 2 failed');

    vaa[2]:= va;
  end;

  procedure Files;
    type
      File_of_integer = File of Integer;
      File_of_nothing = File;
    var
      t: Text;
      ta: array[1..10] of Text;
      fi: File_of_Integer;
      fia: array[1..10] of File_of_Integer;
      f: File_of_nothing;
      fa: array[1..10] of File_of_nothing;
      a,b,c,d: Integer;
  begin

    {*** Simple combinations with Write,Read ***}

    write;               {write}
    writeln;             {writeln}
    write(t);            {write(t)}
    writeln(t);          {writeln(t)}
    write(a);            {write(a)}
    writeln(a);          {writeln(a)}
    write(t,a);          {write(t,a)}
    writeln(t,a);        {writeln(t,a)}

    read;                {read}
    readln;              {readln}
    read(t);             {read(t)}
    readln(t);           {readln(t)}
    read(a);             {read(a)}
    readln(a);           {readln(a)}
    read(t,a);           {read(t,a)}
    readln(t,a);         {readln(t,a)}

    {######## DIRECT ########}

    Write('coucou',a,b,c,d);
    Write(a,'coucou',b,c,d);
    Write(a,b,'coucou',c,d);
    Write(1234.5,a,b,c,d);
    Write(a,1234.5,b,c,d);
    Write(a,b,1234.5,c,d);
    Write(1234.5:5:2,a,b,c,d);
    Write(a,1234.5:5:2,b,c,d);
    Write(a,b,1234.5:5:2,c,d);

    { \/  \/ \/ \/ \/ \/ HERE AND ONLY HERE P2ada HAS A DOUBT  }
    Write(topon,a,b,c,d);
    Write(a,topon,b,c,d);
    Write(a,b,topon,c,d);

    {######## TO t ########}

    Write(t,'coucou',a,b,c,d);
    Write(t,a,'coucou',b,c,d);
    Write(t,a,b,'coucou',c,d);
    Write(t,1234.5,a,b,c,d);
    Write(t,a,1234.5,b,c,d);
    Write(t,a,b,1234.5,c,d);
    Write(t,1234.5:5:2,a,b,c,d);
    Write(t,a,1234.5:5:2,b,c,d);
    Write(t,a,b,1234.5:5:2,c,d);
    Write(t,topon,a,b,c,d);
    Write(t,a,topon,b,c,d);
    Write(t,a,b,topon,c,d);

    {######## TO ta[2*3] ########}

    Write(ta[2*3],'coucou',a,b,c,d);
    Write(ta[2*3],a,'coucou',b,c,d);
    Write(ta[2*3],a,b,'coucou',c,d);
    Write(ta[2*3],1234.5,a,b,c,d);
    Write(ta[2*3],a,1234.5,b,c,d);
    Write(ta[2*3],a,b,1234.5,c,d);
    Write(ta[2*3],1234.5:5:2,a,b,c,d);
    Write(ta[2*3],a,1234.5:5:2,b,c,d);
    Write(ta[2*3],a,b,1234.5:5:2,c,d);
    Write(ta[2*3],topon,a,b,c,d);
    Write(ta[2*3],a,topon,b,c,d);
    Write(ta[2*3],a,b,topon,c,d);

  end;

  procedure Bill_Findlay_1;

    {1}
    var
      external,forward: integer;

    {2}
    const a=1; b=10;

    type
      T = array[1..10] of a..b;
      T2 = array[1..10] of a..b;
      R = record x: a..b end;

    {4}
    var
      CODEFILE :FILE OF INTEGER ;  { <- ' ' harms INTEGER_Direct_IO }
      CODEFILE2 :FILE OF INTEGER;
      CODEFILE3 :FILE OF array[1..10] of INTEGER;
  begin
    forward:= 12;
    external:= 9;
  end;

  Const
    CopyRight = 'Bzzzt Copyright 2003';
    Version   : String = 'Version 1.2';

begin
  for i:=1 to ParamCount do Writeln(ParamStr(i));
  { ParamCount and ParamStr can be translated without too much confusion
    by Argument_Count and Argument }

  writeln(Concat('Con','cat'));

  writeln('UpCase of ''x'':', upcase('x'));

  Writeln(Copyright);
  Writeln(Version);

  Vodka;
  Conditional;

  { Spit all ^J like characters }
  WriteLn('begin');
  for c:=' ' to #255 do begin
    WriteLn('  WriteLn(',ord(c),', ''^',c,' has code '',ord(^',c,'));')
  end;
  WriteLn('end.');

  Writeln(ord(ch));

  readln;


  WriteLn(Test_Exit);
  Test_fourier;
  c:= #32;
  s:= 'ab'+#32+'zozo';

  New(p1);
  Dispose(p1);

  asm cli end;
  for j:= 1 to 100 do
    for k:= 300 downto 100 do begin { that begin/end can disappear }
      i:= 100 div n;
      {16-Jan-2002: fixed translation of TP's exit statement}
      if k=3 then exit; {TP's exit should be translated into "return"}
      i:= m shl n;
      i:= m shr n; { a "null" will appear here (normal) }
    end;
  for j:= 1 to 10000 do;
  if i=m then begin { that begin/end can disappear }
    s:= '';
    s:='''';
    s:='aa1''aa2''''aa3''''''';
    case c of
      'a': ;
      'b': i:= 0;
    end
  end else begin { that begin/end can disappear }
    s:='ab h‚ ben'; { an accent here... }
    c:='a';
    case c of
      'c': str(123.0:10:9,s);    { TP's Str pseudo-procedure }
      'a': str(123:10,s);    { TP's Str pseudo-procedure }
      'b': begin str(123,s); { TP's Str pseudo-procedure }
           i:=321; end; { that begin/end can disappear }
      else i:= 4444
    end
  end;

  {---- Testing empty statements or statement sequences ---- }
  {-- * REPEAT UNTIL (not translation issue!) }
  repeat
    {...nothing...}
  until true;
  
  {-- * CASE ... ELSE END }
  { test several 'else' parts }
  { No else: }
  case c of
    'a':;
  end;
  { An else: }
  case c of
    'a':;
    else
  end;
  case c of
    'a':;
    else i:=i
  end;
  case c of
    'a':;
    else i:=i; i:=i
  end;
  
  {-- * Separated empty statements }

  ;;;; { <<<-- useless empty statements (;) }
  
  {-- * BEGIN END }
  begin

         { 1 empty statement in Pascal, 1 null in Ada }
  end;
  begin
    ;
         { 2 empty statements in Pascal, 1 null in Ada }
  end;
  begin
    ;;;
         { 4 empty statements in Pascal, 1 null in Ada }
  end

  {
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum  begin   70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum  goto    70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *
     Immense commentaire      Immense commentaire  *** boum          70                            100                        *

       }
    
end.
{--- end of file ---}
