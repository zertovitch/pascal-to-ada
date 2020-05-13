--  BP2P. Author of preprocessing part: G. de Montmollin.
--  Feature list in the eponym procedure below.
-------------------------------------------------------------------------------
--  Program based on a Pascal pretty-printer written by Ledgard,
--  Hueras, and Singer.  See SIGPLAN Notices, Vol. 12, No. 7, July 1977,
--  pages 101-105, and PP.DOC/HLP.
--
--  - Version of PP developed under Pascal/Z V4.0 or later by Peter Grogono.
--
--  - Very minor modifications for Turbo Pascal made by Willett Kempton
--      March 1984 and Oct 84.  Runs under 8-bit Turbo or 16-bit Turbo.
--
--  - Toad Hall tweak, rewrite for TP 5, 28 Nov 89
--

-- Converted by (New) P2Ada v. 8-Jan-2003
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;

procedure BP2P is 

  procedure Feature_list is
  begin
    Put_Line(Standard_error, "BP2P does almost nothing else than transforming");
    Put_Line(Standard_error, "a few Borland Pascal oddities into forms easier for P2Ada.");
    New_Line(Standard_error);
    Put_Line(Standard_error, "  1/ Labels: identifiers are translated into numbers");
    Put_Line(Standard_error, "  2/ Characters like ^M are transformed into their #xxx form");
    Put_Line(Standard_error, "  3/ Comment out with remarks the ASM and INLINE parts");
    Put_Line(Standard_error, "  4/ Process Boolean evaluation switches ($B+/$B-) and translate short circuits");
    Put_Line(Standard_error, "     into ISO Extended Pascal's 'and_else' and 'or_else'");
    Put_Line(Standard_error, "  5/ Process conditional defines: $ifdef, $ifopt, $else, $endif, $define, $undef");
  end;

  trace: constant Boolean:= False;

  switch: array( Character'('A')..'Z' ) of Boolean:=
    ('B' => True, others => False);

  type p_String is access String;
  procedure Dispose is new Ada.Unchecked_Deallocation(String, p_String);
  define: array(1..200) of p_String:= (others => null);

  Too_many_defines: exception;

  function Defined(symbol: String) return Boolean is
    up: constant String:= Trim(To_Upper(symbol),both);
  begin
    for i in define'range loop
      if define(i) /= null and then define(i).all = up then
        return True;
      end if;
    end loop;
    return False;
  end Defined;

  procedure Add_define(symbol: String) is
    up: constant String:= Trim(To_Upper(symbol),both);
  begin
    if up="" then return; end if;
    
    -- 1/ maybe already defined
    if defined(symbol) then
      return;
    end if;
    
    -- 2/ to be defined
    for i in define'range loop
      if define(i) = null then
        define(i):= new String'(up);
        return;
      end if;
    end loop;
    
    Put_Line(Standard_error, "Too many defines");
    raise Too_many_defines;
  end Add_define;

  procedure Add_defines(symbols: String) is
    i,v: Natural:= symbols'first;
  begin
    loop
      v:= Index(symbols(i..symbols'last),",");
      if v = 0 then
        Add_define(symbols(i..symbols'last));
        exit;
      end if;
      Add_define(symbols(i..v-1));
      i:= v+1;
      exit when i > symbols'last;
    end loop;
  end Add_defines;

  procedure Remove_define(symbol: String)  is
    up: constant String:= Trim(To_Upper(symbol),both);
  begin
    for i in define'range loop
      if define(i) /= null and then define(i).all = up then
        Dispose(define(i));
        define(i):= null;
      end if;
    end loop;
  end Remove_define;

  procedure Translation( InFile, OutFile: File_Type ) is

    INDENT : constant := 2;  
    --  Indentation step size for structured statements 
    UPCASEKEYWORDS : constant Boolean := false;  
    --  If all keywords are to be capitalized 

    TAB : constant := 9; --  ASCII tab character  
    Blank   : constant Character := ' ';  
    MAXBYTE : constant           := 255;   --  Largest value of 1 byte variable  

    MAXSYMBOLSIZE : constant := 255;  
    MAXSTACKSIZE  : constant := 100;  
    MAXKEYLENGTH  : constant := 9;     --  The longest keyword is PROCEDURE  

    MAXLINESIZE : constant := 255; --  Maximum length of output line  

    --  I PPTYPES.PAS 

    -- type String0  is   new STRING(1 ..1); --  Pascal/z had 0
    -- type FileName  is   new STRING(1 ..20);
    type Keysymbol  is
      --  keywords 
      (Endsym,Beginsym,Ifsym,Thensym,Elsesym,Procsym,Varsym,Ofsym,
      Whilesym,Dosym,Casesym,Withsym,Forsym,Repeatsym,Untilsym,
      Funcsym,Labelsym,Constsym,Typesym,Recordsym,Stringsym,Progsym,
      Andsym,Arrsym,Divsym,Downsym,Filesym,Gotosym,Insym,Modsym,
      Notsym,Nilsym,Orsym,Setsym,Tosym,
      Casevarsym,
      --  other symbols 

      Becomes,Opencomment,Closecomment,Semicolon,Colon,Equals,
      Openparen,Closeparen,Period,Endoffile,Othersym);

    type Options  is  (Crsupp,Crbefore,Blinbefore,
      Dindonkey,Dindent,Spbef,
      Spaft,Gobsym,Inbytab,Crafter);

    type Optionset  is array(Options) of Boolean;
    type Keysymset  is array(Keysymbol) of Boolean;

    type Tableentry  is
    record
      Selected : Optionset;
      Dindsym : Keysymset;
      Terminators : Keysymset;
    end record;

    type Tableptr  is  access Tableentry;
    --  procedure Dispose is new Ada.Unchecked_Deallocation(Tableentry, Tableptr);
    type Optiontable  is  array ( Keysymbol ) of  Tableptr;
    subtype Key  is  String( 1 .. MAXKEYLENGTH );
    type KeywordTable  is  array ( Endsym .. Tosym ) of  Key;
    type SpecialChar  is  array ( 1 .. 2 ) of   Character ;
    type Dblcharset  is   array(Endsym .. Othersym) of Boolean;
    type DblCharTable  is  array ( Becomes .. Opencomment ) of  SpecialChar;
    type SglCharTable  is array ( Opencomment .. Period ) of   Character ;

    comment_sym: constant Keysymset:=
      (Opencomment | Closecomment => True, others => False );
    
    subtype Token  is  String( 1 .. MAXSYMBOLSIZE );

    type Set_of_char is array(Character) of Boolean;

    alpha: constant Set_of_char:= ('A'..'Z'|'a'..'z'|'_' => True, others => False); 
    num  : constant Set_of_char:= ('0'..'9'|',' => True, others => False);

    type Symbol  is
    record
      Name : Keysymbol;
      Value : Token;
      IsKeyWord : BOOLEAN;
      Length, Spacesbefore, Crsbefore : INTEGER;
      Label_Replacement_Value: Integer;
    end record;

    empty_symbol: constant Symbol:= (othersym, (others=>' '), False, 0,0,0,0);

    type Symbolinfo  is  access  Symbol;
    --  procedure Dispose is new Ada.Unchecked_Deallocation(Symbol, Symbolinfo);
    type Charname  is  (Letter,Digit,Space,Quote,Endofline,Filemark,Otherchar);

    type Charinfo  is
    record
      Name  : Charname;
      Value : Character;
    end record;

    type Stackentry  is
    record
      Indentsymbol : Keysymbol;
      Prevmargin : INTEGER;
    end record;

    type Symbolstack  is  array ( 1 .. MAXSTACKSIZE ) of  Stackentry;

    type Hashentry  is
    record
      Keyword : Key;
      Symtype : Keysymbol;
    end record;

    RecordSeen  : BOOLEAN;  
    Currchar,  
    Nextchar    : Charinfo;  
    Currsym,  
    Nextsym     : Symbolinfo;  
    CRPending   : BOOLEAN;  
    Option      : Optiontable;  
    Sets        : Tableptr;  
    Dblch       : Dblcharset;  
    Stack       : Symbolstack;  

    Top,  
    Startpos,  
    Currlinepos,  
    Currmargin,  
    Inlines,  
    Outlines    : INTEGER;  
    Hashtable : array ( 0..MAXBYTE ) of  Hashentry;  

    --  Keywords used for formatting
    --  endsym,beginsym,ifsym,thensym,elsesym,procsym,varsym,ofsym,
    --  whilesym,dosym,casesym,withsym,forsym,repeatsym,untilsym,
    --  funcsym,labelsym,constsym,typesym,recordsym,stringsym,progsym,
    --  andsym,arrsym,divsym,downsym,filesym,gotosym,insym,modsym,
    --  notsym,nilsym,orsym,setsym,tosym)
    --
    Keyword : constant KeywordTable := (    
      "END      ", "BEGIN    ", "IF       ", "THEN     ",
      "ELSE     ", "PROCEDURE", "VAR      ", "OF       ", "WHILE    ", "DO       ",
      "CASE     ", "WITH     ", "FOR      ", "REPEAT   ", "UNTIL    ", "FUNCTION ",
      "LABEL    ", "CONST    ", "TYPE     ", "RECORD   ", "STRING   ", "PROGRAM  ",
      "AND      ", "ARRAY    ", "DIV      ", "DOWNTO   ", "FILE     ", "GOTO     ",
      "IN       ", "MOD      ", "NOT      ", "NIL      ", "OR       ", "SET      ",
      "TO       "); --  keywords not used for formatting  

    --  DblCharTable = ARRAY [becomes..opencomment] OF SpecialChar;
    DblChar : constant DblCharTable := (":=", "(*");  

    --  SglCharTable = ARRAY [opencomment..period] OF CHAR;
    --  opencomment,closecomment,semicolon,colon,equals,
    --  openparen,closeparen,period
    --
    SglChar : constant SglCharTable := ('{', '}', ';', ':', '=', '(', ')', '.');  

    --  I PPINC1.PAS 
    procedure GetChar is 
      --  Read the next character and classify it 
      Ch : Character;  
    begin
      Currchar := Nextchar;
      declare
        r : Charinfo renames Nextchar ;
      begin
        if  End_Of_File(InFile) then
          r.Name := Filemark;
          r.Value:= Blank;
        else
          if  End_Of_Line(InFile) then
            r.Name := Endofline;
            r.Value := Blank;
            Inlines:= Inlines + 1;
            Skip_Line(InFile);
          else
            Get(InFile, Ch);
            r.Value := Ch;
            case  Ch  is 
              when 'a'..'z'| 'A'..'Z'| '_'   => r.Name := Letter;
              when '0'..'9'                  => r.Name := Digit;
              when '''                       => r.Name := Quote;
              when Blank| Character'Val(TAB) => r.Name := Space;
              when others                    => r.Name := Otherchar;
            end case;
          end if;
        end if;
      end;
    end GetChar;

    procedure StoreNextChar (
          Lngth : in out INTEGER; 
          Value : in out Token    ) is 
      --  Store a character in the current symbol 
    begin
      GetChar;
      if  Lngth < Maxsymbolsize then
        Lngth:= Lngth + 1;
        Value(Lngth) := Currchar.Value;
      end if;
    end StoreNextChar; 

    procedure SkipBlanks (
          Spacesbefore,                 
          Crsbefore    : in out INTEGER ) is 
      --  Count the spaces between symbols 
    begin
      Spacesbefore := 0;
      Crsbefore := 0;
      while Nextchar.Name = Space or Nextchar.Name = Endofline loop
        GetChar;
        case  Currchar.Name  is
          when Space=>     Spacesbefore:= Spacesbefore + 1;
          when Endofline=> Crsbefore:= Crsbefore + 1; Spacesbefore := 0;
          when others=>    null;
        end case;
      end loop;
    end SkipBlanks; --  of SkipBlanks 

    --*BP2P*-- {$Blabla }
    subtype cond_stack_range is Integer range 0..1023;
    condition_accepted: array(cond_stack_range) of Boolean;
    condition_level: cond_stack_range;
    exceeds_condition_stack, endif_too_much: exception;
    short_circuit: Boolean;

    function All_conditions_valid return Boolean is
      ok: Boolean:= True;
    begin
      for i in 1 .. condition_level loop
        ok := ok and condition_accepted(i);
      end loop;
      return ok;
    end;
    
    procedure Nest_condition(accepted: Boolean) is -- {$IFDEF ...}, {$IFOPT ...}
    begin
      if condition_level = cond_stack_range'last then
        Put(Standard_error, "Condition stack " );
        Put(Standard_error, cond_stack_range'last );
        Put_Line(Standard_error, " -- too deeply nested " );
        raise exceeds_condition_stack;
      else
        condition_level:= condition_level + 1;
        condition_accepted(condition_level):= accepted;
      end if;
    end Nest_condition;

    procedure Close_condition is -- {$ENDIF}
    begin
      if condition_level = cond_stack_range'first then
        Put_Line(Standard_error, "One {$ENDIF} too much !" );
        raise endif_too_much;
      else
        condition_level:= condition_level - 1;
      end if;
    end Close_condition;

    -- All happens for the NEXT symbol
    curr_acceptation: Boolean;
    curr_short_circuit: Boolean;

    procedure Directive_in_a_comment (s: Symbol) is
      i, fin: Natural;
      type directive is (
        none,
        -- TP 6: $B+/$B- short circuit
        B,
        -- Delphi 6: {$BOOLEVAL ON} or {$BOOLEVAL OFF}
        BOOLEVAL,
        -- TP 6: conditional
        ifdef, ifndef, ifopt, slse, endif, define, undef
      );
      dir: directive;
    begin
      if s.value(1)='{' then
        i:= 2;
      else
        i:= 3; -- "(*"
      end if;
      if s.value(s.length)='}' then
        fin:= s.length-1;
      else
        fin:= s.length-2; -- "*)"
      end if;
      if s.value(i) /= '$' then -- no conditional
        return;
      end if;
      i:= i + 1;
      dir:= none;
      for d in directive loop
        declare
          name: String:= directive'image(d);
        begin
          if d=slse then name(1):= 'E'; end if;
          if fin-i+1 >= name'length and then
             To_Upper(s.value(i..i+name'length-1)) = name and then
             not Alpha(s.value(i+name'length)) then
            dir:= d;
            i:= i+name'length;
            exit;
          end if;   
        end;
      end loop;
      if dir = none then -- directive not recognized
        return;
      end if;
      if trace then
        Put(Outfile,"[Directive=" & directive'image(dir) & ',' &
            "Parameter=" & s.value(i..fin) & ']');
      end if;
      case dir is
        when none => null;
        when B      => if All_conditions_valid then
                         short_circuit:= s.value(i)='-';
                       end if;
                       if trace then
                         Put(Outfile,"[$B:" & s.value(i) & ']');
                       end if;
        when BOOLEVAL => if All_conditions_valid then
                           short_circuit:= To_Upper(s.value(i+1..i+3))="OFF";
                         end if;
        when define => Add_define(s.value(i..fin));
        when undef =>  Remove_define(s.value(i..fin));
        when ifdef =>  Nest_condition( Defined(s.value(i..fin)) );
        when ifndef => Nest_condition( not Defined(s.value(i..fin)) );
        when ifopt =>  Nest_condition(
                         switch( To_Upper(s.value(i+1)) ) =
                         ( s.value(i+2) = '+' )
                       );
                       if trace then
                         Put(Outfile,"[$ifopt:" & s.value(i+1..i+2) & ']');
                       end if;
        when slse =>   condition_accepted(condition_level):=
                         not condition_accepted(condition_level);
                       -- {$ELSE} -> invert acceptance
        when endif =>  Close_condition;
      end case;
      switch('B'):= not short_circuit;
    end Directive_in_a_comment;
    --*BP2P*--

    procedure GetComment (
          Sym : Symbolinfo ) is 
      --  Process comments using either brace or parenthesis notation 
    begin
      Sym.Name := Opencomment;
      while not (((Currchar.Value = '*')  and  (Nextchar.Value = ')'))
          or  (Currchar.Value = '}')  or  (Nextchar.Name = Endofline)
          or  (Nextchar.Name = Filemark)) loop
        StoreNextChar(Sym.Length, Sym.Value);
      end loop;
      if  (Currchar.Value = '*')  and  (Nextchar.Value = ')') then
        StoreNextChar(Sym.LENGTH, Sym.Value);
        Sym.Name := Closecomment;
      end if;
      if  Currchar.Value = '}' then
        Sym.Name := Closecomment;
      end if;
      if Sym.Name = Closecomment then
        Directive_in_a_comment(Sym.all);
      end if;
    end GetComment;

    --  end of PPINC1

    function Hash (
          Symbol : Key; 
          Lngth  : Natural ) 
      return Natural is 
      --  Hashing function for identifiers.  The formula gives a unique value
      --     in the range 0..255 for each Pascal/Z keyword.  Note that range and
      --     overflow checking must be turned off for this function even if they
      --     are enabled for the rest of the program.  
    begin
      return ((Character'Pos(Symbol(1)) * 5 + Character'Pos(Symbol(
            Lngth))) * 5 + Lngth) mod 256;
    end Hash;

    procedure ClassID (
          Value     :        Token;     
          Lngth     :        INTEGER;   
          Idtype    : in out Keysymbol; 
          IsKeyWord : in out BOOLEAN    ) is 
      --  Classify an identifier.  We are only interested
      --     in it if it is a keyword, so we use the hash table. 

      Keyvalue : Key;  
      Tabent   : INTEGER;  
    begin
      if  Lngth > MAXKEYLENGTH then
        Idtype := Othersym;
        IsKeyWord := False;
      else

        Keyvalue:= (others => Blank); --  v1.1 fill with spaces

        for I  in  1 .. Lngth loop
          Keyvalue(I) := To_Upper(Value(I));
        end loop;

        Tabent := Hash(Keyvalue, Lngth);
        if  Keyvalue = Hashtable(Tabent).Keyword then
          Idtype := Hashtable(Tabent).Symtype;
          IsKeyWord := True;
        else
          Idtype := Othersym;
          IsKeyWord := False;
        end if;
      end if;
    end ClassID;

    procedure GetIdentifier (
          Sym : Symbolinfo ) is 
      --  Read an identifier and classify it 
    begin
      while Nextchar.Name = Letter or Nextchar.Name = Digit loop
        StoreNextChar(Sym.Length, Sym.Value);
      end loop;
      ClassID(Sym.Value, Sym.Length, Sym.Name, Sym.IsKeyWord);
      case  Sym.Name  is
        when Endsym    =>          RecordSeen := False;
        when Recordsym =>          RecordSeen := True;
        when Casesym   =>
          if  RecordSeen then
            Sym.Name := Casevarsym;
          end if;
        when others=> null;
      end case;
    end GetIdentifier;

    --  Read a number and store it as a string 

    procedure GetNumber (
          Sym : Symbolinfo ) is 
    begin
      while Nextchar.Name = Digit loop
        StoreNextChar(Sym.Length,
          Sym.Value);
      end loop;
      Sym.Name := Othersym;
    end GetNumber;

    procedure GetCharLiteral (
          Sym : Symbolinfo ) is 
      --  Read a quoted string 
    begin
      while Nextchar.Name = Quote loop
        StoreNextChar(Sym.Length, Sym.Value);
        while not (Nextchar.Name = Quote or Nextchar.Name = Endofline or
                   Nextchar.Name = Filemark) loop
          StoreNextChar(Sym.Length, Sym.Value);
        end loop;
        if  Nextchar.Name = Quote then
          StoreNextChar(Sym.Length,
            Sym.Value);
        end if;
      end loop;
      Sym.Name := Othersym;
    end GetCharLiteral; --  of GetCharLiteral 



    function Char_Type return Keysymbol is 
      --  Classify a character pair 
      NextTwoChars : SpecialChar;  
      Hit          : BOOLEAN;  
      Thischar     : Keysymbol;  
    begin
      NextTwoChars(1) := Currchar.Value;
      NextTwoChars(2) := Nextchar.Value;
      Thischar := Becomes;
      Hit := False;
      while not (Hit  or  (Thischar = Closecomment)) loop
        if  NextTwoChars = DblChar(Thischar) then
          Hit := True;
        else
          Thischar:= Keysymbol'Succ(Thischar);
        end if;
      end loop;
      if not Hit then
        Thischar := Opencomment;
        while not (Hit  or  (Keysymbol'PRED(Thischar) = Period)) loop
          if  Currchar.Value = SglChar(Thischar) then
            Hit := True;
          else
            Thischar:= Keysymbol'Succ(Thischar);
          end if;
        end loop;
      end if;
      if Hit then
        return Thischar;
      else
        return Othersym;
      end if;
    end Char_Type;

    procedure GetSpecialChar ( Sym : Symbolinfo ) is 
      --  Read special characters 
    begin
      StoreNextChar(Sym.Length, Sym.Value);
      Sym.Name := Char_Type;
      if  Dblch(Sym.Name) then
        StoreNextChar(Sym.Length, Sym.Value);
      end if;
    end GetSpecialChar; --  of GetSpecialChar 

    procedure GetNextSymbol (
          Sym : Symbolinfo ) is 
      --  Read a symbol using the appropriate procedure 
    begin
      case  Nextchar.Name  is
        when Letter=>       GetIdentifier(Sym);
        when Digit=>        GetNumber(Sym);
        when Quote=>        GetCharLiteral(Sym);
        when Otherchar=>    GetSpecialChar(Sym);
          if  Sym.Name = Opencomment then
            GetComment(Sym);
          end if;

        when Filemark=>
          Sym.Name := Endoffile;
        when others =>
          Put_Line("Unknown character type: " & Charname'Image(Nextchar.Name));
      end case;
    end GetNextSymbol;

    procedure GetSymbol is 
      --  Store the next symbol in NEXTSYM 

      Dummy : Symbolinfo;  
    begin
      Dummy := Currsym;
      Currsym := Nextsym;
      Nextsym := Dummy;
      Currsym.Label_Replacement_Value:= 0;
      SkipBlanks(Nextsym.Spacesbefore, Nextsym.Crsbefore);
      Nextsym.Length := 0;
      Nextsym.IsKeyWord := False;
      if  Currsym.Name = Opencomment then
        GetComment(Nextsym);
      else
        GetNextSymbol(Nextsym);
      end if;
    end GetSymbol;

    procedure PopStack (
          Indentsymbol : out Keysymbol; 
          Prevmargin   : out INTEGER    ) is 
      --  Manage stack of indentation symbols and margins 
    begin
      if  Top > 0 then
        Indentsymbol := Stack(Top).Indentsymbol;
        Prevmargin := Stack(Top).Prevmargin;
        Top:= Top - 1;
      else
        Indentsymbol := Othersym;
        Prevmargin := 0;
      end if;
    end PopStack;

    procedure PushStack (
          Indentsymbol : in Keysymbol; 
          Prevmargin   : in INTEGER    ) is 
    begin
      Top:= Top + 1;
      Stack(Top).Indentsymbol := Indentsymbol;
      Stack(Top).Prevmargin := Prevmargin;
    end PushStack;

    procedure WriteCRs (
          Numberofcrs : INTEGER ) is 
    begin
      if  Numberofcrs > 0 then
        for I  in  1 .. Numberofcrs loop
          New_Line(OutFile);
        end loop;
        Outlines:= Outlines + Numberofcrs;
        Currlinepos := 0;
      end if;
    end WriteCRs;

    procedure InsertCR is 
    begin
      if  Currsym.Crsbefore = 0 then
        WriteCRs(1);
        Currsym.Spacesbefore := 0;
      end if;
    end InsertCR;

    procedure InsertBlankLine is 
    begin
      if  Currsym.Crsbefore = 0 then
        if  Currlinepos = 0 then
          WriteCRs(1);
        else
          WriteCRs(2);
        end if;
        Currsym.Spacesbefore := 0;
      else
        if  Currsym.Crsbefore = 1 then
          if  Currlinepos > 0 then
            WriteCRs(1);
          end if;
        end if;
      end if;
    end InsertBlankLine;

    procedure LShiftOn (
          Dindsym : Keysymset ) is 
      --  Move margin left according to stack configuration and current symbol 

      Indentsymbol : Keysymbol;  
      Prevmargin   : INTEGER;  
    begin
      if  Top > 0 then
        loop
          PopStack(Indentsymbol, Prevmargin);
          if  Dindsym(Indentsymbol) then
            Currmargin := Prevmargin;
          end if;
          exit when not Dindsym(Indentsymbol)  or  (Top = 0); -- !!@
        end loop;
        if  not Dindsym(Indentsymbol) then
          PushStack(Indentsymbol, Prevmargin);
        end if;
      end if;
    end LShiftOn;

    procedure LShift is 
      --  Move margin left according to stack top 
      Indentsymbol : Keysymbol;  
      Prevmargin   : INTEGER;  
    begin
      if  Top > 0 then
        PopStack(Indentsymbol, Prevmargin);
        Currmargin := Prevmargin;
        --  maybe PopStack(indentsymbol,currmargin); 
      end if;
    end LShift;

    procedure InsertSpace (Symbol : Symbolinfo) is 
      --  Insert space if room on line 
    begin
      if  Currlinepos < MAXLINESIZE then
        Put(OutFile, Blank);
        Currlinepos := Currlinepos + 1;
        if  (Symbol.Crsbefore = 0)  and  (Symbol.Spacesbefore > 0) then
          Symbol.Spacesbefore:= Symbol.Spacesbefore -1; -- !!@
        end if;
      end if;
    end InsertSpace; 

    procedure MoveLinePos (
          Newlinepos : INTEGER ) is 
      --  Insert spaces until correct line position reached 
    begin
      for I  in  Currlinepos+1 .. Newlinepos loop
        Put(OutFile, Blank);
      end loop;
      Currlinepos := Newlinepos;
    end MoveLinePos;

    procedure PrintSymbol(general_output_switch: Boolean) is 
      --  Print a symbol converting keywords to upper case 
       L : INTEGER;
       output_switch: constant Boolean:=
         general_output_switch or comment_sym(currsym.name);
    begin
      if trace then
        Put(OutFile,'[' & keysymbol'image(Currsym.Name) & ':');
      end if;
      if curr_short_circuit then
        if currsym.name=orsym then
          currsym.length:= 7;
          currsym.value(1..7) := "or_else";
        elsif currsym.name=andsym then
          currsym.length:= 8;
          currsym.value(1..8) := "and_then";
        end if;
      end if;
        
      if  (Currsym.Name=Othersym)  and  (
          Currsym.Label_Replacement_Value > 0)
          then
        L:= 6*2 + Currsym.Length + 26;
        if output_switch then
          Put(OutFile, Currsym.Label_Replacement_Value,6);

          Put(OutFile, "{ [BP2P]: Label """);
          Put(OutFile, Currsym.Label_Replacement_Value,6);
          Put(OutFile, """ Was """ );
          Put(OutFile, Currsym.Value(1 .. Currsym.Length));
          Put(OutFile, """}" );
        end if;
      elsif  Currsym.IsKeyWord  and  UPCASEKEYWORDS then
        if output_switch then
          for I  in  1 .. Currsym.Length loop
            Put(OutFile, To_Upper(Currsym.Value(I)));
          end loop;
        end if;
        L:= Currsym.Length;
      else
        if output_switch then
          Put(OutFile, Currsym.Value(1 .. Currsym.Length));
        end if;
        L:= Currsym.Length;
      end if;
      Startpos := Currlinepos;
      Currlinepos:= Currlinepos + L;
      if trace then Put(OutFile,']'); end if;
    end PrintSymbol;

    procedure PPSymbol(output_switch: Boolean) is 
      --  Find position for symbol and then print it 
      Newlinepos:INTEGER;
    begin
      WriteCRs(Currsym.Crsbefore);
      if  (Currlinepos + Currsym.Spacesbefore > Currmargin)
         or comment_sym(Currsym.Name)
        then
        Newlinepos := Currlinepos + Currsym.Spacesbefore;
      else
        Newlinepos := Currmargin;
      end if;

      if  Newlinepos + Currsym.Length > MAXLINESIZE then
        WriteCRs(1);
        if Currmargin + Currsym.Length <= MAXLINESIZE then
          Newlinepos := Currmargin;
        elsif  Currsym.Length < MAXLINESIZE then
          Newlinepos := MAXLINESIZE - Currsym.Length;
        else
          Newlinepos := 0;
        end if;
      end if;
      MoveLinePos(Newlinepos);
      PrintSymbol(output_switch);
    end PPSymbol;

    procedure Gobble (
          Terminators : Keysymset ) is 
      --  Print symbols which follow a formatting symbol but which do not
      --     affect layout 
    begin
      if  Top < MAXSTACKSIZE then
        PushStack(Currsym.Name, Currmargin);end if;
      Currmargin := Currlinepos;
      while not (Terminators(Nextsym.Name) or  (Nextsym.Name = Endoffile)) loop
        GetSymbol;
        PPSymbol(True);
      end loop;
      LShift;
    end Gobble;

    procedure RShift (
          Currsym : Keysymbol ) is 
      --  Move right, stacking margin positions 
    begin
      if  Top < MAXSTACKSIZE then
        PushStack(Currsym, Currmargin);
      end if;
      if  Startpos > Currmargin then
        Currmargin := Startpos;
      end if;
      Currmargin:= Currmargin + INDENT;
    end RShift;

    procedure Initialize is 
      Psn,Len: Natural;
    begin
      Top := 0;
      Currlinepos := 0;
      Currmargin := 0;
      Inlines := 0;
      Outlines := 0;

      --  Create hash table 
      for Psn  in Hashtable'range loop
        Hashtable(Psn).Keyword := "         ";
        Hashtable(Psn).Symtype := Othersym;
      end loop;

      for Sym  in  Endsym .. Tosym loop
        Len := MAXKEYLENGTH;
        while Len > 0 and then Keyword(Sym)(Len) = Blank loop
          Len:= Len -1;
        end loop;
        Psn := Hash(Keyword(Sym), Len);
        Hashtable(Psn).Keyword := Keyword(Sym);
        Hashtable(Psn).Symtype := Sym;
      end loop;

      --  Set up other special symbols 

      Dblch := (Becomes| Opencomment => True, others => False) ;

      --  Set up the sets that control formatting.  If you want PP to insert a
      --  line break before every statement, include CRBEFORE in the SELECTED
      --  set of the appropriate keywords (WHILE, IF, REPEAT, etc.).  The
      --  disadvantage of this is that PP will sometimes put line breaks
      --  where you don't want them, e.g. after ':' in CASE statements.  Note
      --  also that PP does not understand the Pascal/Z use of ELSE as a
      --  CASE label -- I wish they'd used OTHERWISE like everybody else.  

      for Sym  in  Endsym .. Othersym loop
        Option(Sym):= new Tableentry;
        Option(Sym).Selected := (others => False) ;
        Option(Sym).Dindsym := (others => False) ;
        Option(Sym).Terminators := (others => False);
      end loop;

      --  Start i/o 

      CrPending := False;
      RecordSeen := False;
      GetChar;
      Currsym:= new Symbol'(empty_symbol);
      Nextsym:= new Symbol'(empty_symbol);
      GetSymbol;
    end Initialize;

    --***  BP2P tools: ***--
     
    Inside_Label_Decl,Inside_Goto,Possible_Label:Boolean;
    
    end_period_seen: Boolean;

    Beginstack:Integer;

    Max_Labels: constant := 50;
    Labels: array (1 .. Max_Labels ) of  Symbolinfo;
    Label_Ptr:Integer;

    procedure Replace_Label (
          S : in out Symbol ) is 
    begin
      for I  in  1 .. Label_Ptr loop
        if To_Upper(S.Value(1..S.length)) =
           To_Upper(Labels(I).Value(1..Labels(I).length)) then
          S.Label_Replacement_Value:= Labels(I).Label_Replacement_Value;
        end if;
      end loop;
    end Replace_Label;

    procedure Maybe_Set_Possible_Label is
    begin
      Possible_Label:=
      (Beginstack > 0)  and  (Nextsym.Name=Othersym)  and (alpha(Nextsym.value(1)));
        
    end Maybe_Set_Possible_Label;

    -- Traitement de [^][M] (Turbo Pascal)

    hat_char_allowed : Boolean:= False;
    
    procedure Transform_hat_char( s1,s2: in out symbol ) is
      code: Natural;
      c: Character;
      forbidden: constant
        Set_of_char:= ('['|','|'+'|'-'|'*'|'/' => True, others => False);
      
    begin
      if hat_char_allowed and then
         s1.name = othersym and then s1.length = 1 and then
         s1.value(1)='^' and then
         s2.Name = othersym and then s2.length = 1 and then
         not forbidden(s2.value(1)) then

        c:= s2.value(1);
        case c is
          when character'val(0) .. character'val(31) => code:= character'pos(c); -- no idea...
          when ' ' .. '?' => code:= character'pos(c) + 64;
          when '@' .. '`' => code:= character'pos(c) - 64;
          when 'a' .. 'z' => code:= character'pos(c) - 97;
          when '{' .. character'val(127) => code:= character'pos(c) - 64;
          when character'val(128) .. character'val(191) => code:= character'pos(c) + 64;
          when character'val(192) .. character'val(255) => code:= character'pos(c) - 64;
        end case;
         
        s1.value(1):= '#';
        s2.length:= 3;
        declare
          scode : constant String := Natural'image (1000 + code);
        begin
          s2.value(1..3):= scode( scode'last-2 .. scode'last );
        end;

      end if;
    end Transform_hat_char;

    program_begun: Boolean;

    ASM_Block, INLINE_instruction, previous_was_comment: Boolean;
    
    procedure Init_BP2P is
    begin
      Inside_Label_Decl:= False;
      Inside_Goto:= False;
      Possible_Label:= False;
      hat_char_allowed:= False;
      end_period_seen:= False;
      Beginstack:= 0;
      Label_Ptr:= 0;
      condition_level:= 0;
      condition_accepted(0):= True;
      curr_acceptation:= True;
      short_circuit:= not switch('B');
      curr_short_circuit:= short_circuit;
      for I  in  Labels'range loop
        Labels(I):= new symbol;
      end loop;
      ASM_Block:= False;
      INLINE_instruction:= False;
      previous_was_comment:= False;
      program_begun:= False;
    end Init_BP2P;

    procedure Add_Header is
    begin
      Put_Line(Outfile,"program I_love_Borland;" &
        " { [BP2P]: cured a headless Borland program }");
    end;

  begin -- Translate
      Initialize;
      Init_BP2P;
      
      while Nextsym.Name /= Endoffile loop
        GetSymbol;
        
        --***  BP2P extras: ***--
        
        if not program_begun then
          case  Currsym.Name  is
            when constsym | typesym | varsym | beginsym | 
                 procsym | funcsym =>
              Add_Header;   
              program_begun:= True;
            when progsym =>
              program_begun:= True;
            when othersym =>  -- othersym must be "UNIT" (head) or "USES" (no head)
              declare
                os: constant String:= To_Upper(currsym.value(1..currsym.length));
              begin
                if os = "USES" then
                  Add_Header;   
                end if;
                program_begun:= True;                
              end;
            when others => null;
          end case;
        end if;

        case  Currsym.Name  is
          when constsym =>
            hat_char_allowed:= True;
          when varsym | typesym =>
            hat_char_allowed:= False; -- could be e.g. ^Integer
          when Beginsym =>
            Beginstack:= Beginstack + 1;
            Maybe_Set_Possible_Label;
            hat_char_allowed:= True;

          when Endsym   => 
            if ASM_Block then
              ASM_Block:= False;
              Put(Outfile," (:end of ASM block) }");
            elsif Beginstack > 0 then    -- could be a "record .. END"
              Beginstack:= Beginstack - 1;
            end if;
            if nextsym.name = period then  -- END. 
              end_period_seen:= True;
            end if;
          when CloseParen =>
            if INLINE_instruction then
              INLINE_instruction:= False;
              Put(Outfile,") (:end of INLINE instruction) }");
              currsym.length:= 0;
            end if;              
          when Labelsym =>  Inside_Label_Decl:= True;
          when Gotosym  =>  Inside_Goto:= True;
          when Repeatsym |
            Thensym |
            Colon    =>  Maybe_Set_Possible_Label;
          when Semicolon=>

            Inside_Label_Decl:= False;
            Inside_Goto:= False;
            Maybe_Set_Possible_Label;
            
          when Closecomment | Opencomment =>
            if not previous_was_comment then
              if ASM_Block then
                Put(Outfile, " (ASM after comment)}");
              elsif INLINE_instruction then
                Put(Outfile, " (INLINE after comment)}");
              end if;
            end if;

          when Othersym =>

            --  identifier or ...

            if Inside_Label_Decl then           -- LABEL label1, label2, ...
              if not num(Currsym.Value(1)) then
                Label_Ptr:= Label_Ptr+1;
                Currsym.Label_Replacement_Value:= 100000 + Label_Ptr;
                Labels(Label_Ptr).all:= Currsym.all;

              end if;
            elsif Possible_Label then           -- label: instr
              if  Nextsym.Name=Colon then
                Replace_Label(Currsym.All);
              end if;
              Possible_Label:= False;
            elsif Inside_Goto then            -- GOTO label
              Replace_Label(Currsym.All);
            end if;
            
            declare
              extra_pascal: constant String:= To_Upper(currsym.value(1..currsym.length));
            begin
              if extra_pascal = "ASM" then
                ASM_Block:= True;
                declare
                  blabla: constant String:= " begin { (ASM Block:) ";
                begin
                  currsym.value(blabla'range):= blabla;
                  currsym.length:= blabla'length;
                end;
              elsif extra_pascal = "INLINE" and nextsym.name = Openparen then
                INLINE_instruction:= True;
                declare
                  blabla: constant String:= " { (INLINE Instruction:) ";
                begin
                  currsym.value(blabla'range):= blabla;
                  currsym.length:= blabla'length;
                end;
              end if;
            end;
            
            Transform_hat_char(currsym.all,nextsym.all);

          when others=> null;
        end case;


        Sets := Option(Currsym.Name);
        if  (CrPending  and  not (Sets.Selected(Crsupp)))
          or  Sets.Selected(Crbefore) then
          InsertCR;
          CrPending := False;
        end if;
        if  Sets.Selected(Blinbefore)   then
          InsertBlankLine;
          CrPending := False;
        end if;
        if  Sets.Selected(Dindonkey) then
          LShiftOn(Sets.Dindsym);
        end if;
        if  Sets.Selected(Dindent) then
          LShift;
        end if;
        if  Sets.Selected(Spbef) then
          InsertSpace(Currsym);
        end if;

        PPSymbol(curr_acceptation);  --   <---- THE Output
        
        curr_acceptation:=  All_conditions_valid ;
        curr_short_circuit:= short_circuit;

        if comment_sym(currsym.name) then
          previous_was_comment:= True;
          if not comment_sym(nextsym.name) then
            if ASM_Block then
              Put(Outfile, "{(Cont'd ASM:) ");
            elsif INLINE_instruction then
              Put(Outfile, "{(Cont'd INLINE:) ");
            end if;
          end if;
        else
          previous_was_comment:= False;
        end if;

        if  Sets.Selected(Spaft) then
          InsertSpace(Nextsym);end if;
        if  Sets.Selected(Inbytab) then
          RShift(Currsym.Name);end if;
        if  Sets.Selected(Gobsym) then
          Gobble(Sets.Terminators);
        end if;
        if  Sets.Selected(Crafter) then
          CrPending := True;
        end if;
      end loop;
      if  CrPending then
        WriteCRs(1);
      end if;
      
      if not end_period_seen then -- "END." is missing, I add a "begin end."
        New_Line(Outfile);
        Put_Line(Outfile,"begin end. { [BP2P]: ""begin end."" Added to avoid a syntax error }");
      end if;

      if trace then
        New_Line(Standard_Error);
        Put(Standard_Error,  "BP2P: ");
        Put(Standard_Error, Inlines, 1);
        Put(Standard_Error,  " lines read, ");
        Put(Standard_Error,  Outlines, 1);
        Put_Line(Standard_Error,  " lines written.");
      end if;
  end Translation;

  Bad_arguments: exception;

  first_file, second_file: Natural:= 0;

  procedure Process_Arguments is
  
    procedure Help is
    begin
      Put_Line(Standard_error, "BP2P: ""Borland Pascal to Pascal"" preprocessor");
      Put_Line(Standard_error, "Syntax: BP2P [options] [infile [outfile]] [options]");
      Put_Line(Standard_error, "When no file infile [or outfile] given, standard I[/O]");
      Put_Line(Standard_error, "Options:");
      Put_Line(Standard_error, "  -D<syms> = Define conditionals");
      --  -I<path> = Include directories
      Put_Line(Standard_error, "  -$B+    Complete boolean evaluation (default)");
      Put_Line(Standard_error, "  -$B-    Short-circuit boolean evaluation (Ada's ""and then"", ""or else"")");
      Put_Line(Standard_error, "  -$<letter><state>   Any other switch has default '-' state");
      New_Line(Standard_error);
      Feature_list;
      raise Bad_arguments;
    end Help;

  begin
    if  Argument_Count = 0 then
      Help;
    end if;

    for i in 1..Argument_Count loop
      declare
        a: constant String:= Argument(i);
        c: Character;
      begin
        if a'length > 0 then
          case a(1) is
            when '-'|'/' =>
              if a'length < 2 then
                Help;
              end if;
              c:= To_Upper(a(2));
              case c is
                when '$' =>
                  if a'length < 4 then
                    Help;
                  end if;
                  c:= To_Upper(a(3));
                  case c is
                    when 'A'..'Z' =>
                      case a(4) is
                        when '+'|'-' => switch(c):= a(4)='+';
                        when others  => Help;
                      end case;
                    when others => Help;
                  end case;
                when 'D' =>
                  Add_defines(a(3..a'last));
                when others =>
                  Help;
              end case;
            when others =>
              if first_file = 0 then
                first_file:= i;
              elsif second_file = 0 then
                second_file:= i;
              else
                Help;
              end if;
          end case;
        end if;
      end;
    end loop;
    
    Put(Standard_error, "BP2P from ");

    if first_file = 0 then
      Put(Standard_error, "Standard_Input to Standard_Output" );
    else
      Put(Standard_error, ''' & Argument(first_file) & "' to ");
      if second_file = 0 then
        Put(Standard_error, "Standard_Output" );
      else
        Put(Standard_error, ''' & Argument(second_file) & ''');
      end if;
    end if;
    New_Line(Standard_error);

    if trace then
      Put(Standard_error, "Enabled switches: ");
      for c in switch'range loop
        if switch(c) then
          Put(Standard_error,c & "+ ");
        end if;
      end loop;
      New_Line(Standard_error);
      
      Put_Line(Standard_error, "Defined symbols:");
      for i in define'range loop
        if define(i) /= null then
          Put_Line(Standard_error, Integer'image(i) & " :" & define(i).all);
        end if;
      end loop;
    end if;
  end;

  InFile, OutFile: File_Type;

begin
  Process_Arguments;
  if first_file = 0 then
    Translation( Standard_Input, Standard_Output );
  else
    Open(InFile, in_file, Argument(first_file));
    if second_file = 0 then
      Translation( InFile, Standard_Output );
    else
      Create(OutFile,out_file, Argument(second_file));
      Translation( InFile, OutFile );
      Close(Outfile);
    end if;
    Close(InFile);
  end if;

exception
  when Bad_arguments => null;
end BP2P;
-- Converted by (New) P2Ada v. 8-Jan-2003                                                     
