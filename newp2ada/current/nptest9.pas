{ Early version of BP2P, still in Pascal, now a new
  syntax test for P2Ada (NPTest9.pas)
}

{ This program does almost nothing other than transforming
  a few Borland Pascal oddities into forms easier for P2Ada.

  1/ Labels: only numbers, no identifiers

  ---

  Program based on a Pascal pretty-printer written by Ledgard,
  Hueras, and Singer.  See SIGPLAN Notices, Vol. 12, No. 7, July 1977,
  pages 101-105, and PP.DOC/HLP.

 - Version of PP developed under Pascal/Z V4.0 or later by Peter Grogono.

 - Very minor modifications for Turbo Pascal made by Willett Kempton
   March 1984 and Oct 84.  Runs under 8-bit Turbo or 16-bit Turbo.

 - Toad Hall tweak, rewrite for TP 5, 28 Nov 89
}

PROGRAM BP2P;

  CONST

{ I PPCONST.PAS }

  NUL = 0;      { ASCII null character }
  TAB = 9;      { ASCII tab character }
  FF = 12;      { ASCII formfeed character }
  CR = 13;      { ASCII carriage return }
  ESC = 27;     { ASCII escape character }
  Blank = ' ';
  MAXBYTE = 255;{ Largest value of 1 byte variable }

  MAXSYMBOLSIZE = 255;
  MAXSTACKSIZE = 100;
  MAXKEYLENGTH = 9;     { The longest keyword is PROCEDURE }
  MAXLINESIZE = 255;     { Maximum length of output line }
  INDENT = 2;           { Indentation step size for structured statements }
  UPCASEKEYWORDS = false;  { If all keywords are to be capitalized }

{ I PPTYPES.PAS }

TYPE

  String0 = STRING[1]; {Pascal/z had 0}
  FileName = STRING[20];
  keysymbol =  { keywords }
              (endsym,beginsym,ifsym,thensym,elsesym,procsym,varsym,ofsym,
               whilesym,dosym,casesym,withsym,forsym,repeatsym,untilsym,
               funcsym,labelsym,constsym,typesym,recordsym,stringsym,progsym,
               andsym,arrsym,divsym,downsym,filesym,gotosym,insym,modsym,
               notsym,nilsym,orsym,setsym,tosym,
               casevarsym,
{ other symbols }
               becomes,opencomment,closecomment,semicolon,colon,equals,
               openparen,closeparen,period,endoffile,othersym);

  options = (crsupp,crbefore,blinbefore,
             dindonkey,dindent,spbef,
             spaft,gobsym,inbytab,crafter);

  optionset = SET OF options;
  keysymset = SET OF keysymbol;

  tableentry = RECORD
                 selected : optionset;
                 dindsym : keysymset;
                 terminators : keysymset
               END;

  tableptr = ^tableentry;
  optiontable = ARRAY [keysymbol] OF tableptr;
  Key = ARRAY [1..MAXKEYLENGTH] OF CHAR;
  KeywordTable = ARRAY [endsym..tosym] OF Key;
  SpecialChar = ARRAY [1..2] OF CHAR;
  dblcharset = SET OF endsym..othersym;
  DblCharTable = ARRAY [becomes..opencomment] OF SpecialChar;
  SglCharTable = ARRAY [opencomment..period] OF CHAR;
  Token = ARRAY [1..MAXSYMBOLSIZE] OF CHAR;

  symbol = RECORD
             name : keysymbol;
             Value : Token;
             IsKeyWord : BOOLEAN;
             length, spacesbefore, crsbefore : INTEGER;
             label_replacement_value: Longint;
           END;

  symbolinfo = ^ symbol;
  charname = (letter,digit,space,quote,endofline,
              filemark,otherchar);

  charinfo = RECORD
               name : charname;
               Value : CHAR
             END;

  stackentry = RECORD
                 indentsymbol : keysymbol;
                 prevmargin : INTEGER
               END;

  symbolstack = ARRAY [1..MAXSTACKSIZE] OF stackentry;

  hashentry = RECORD
                Keyword : Key;
                symtype : keysymbol
              END;

VAR
  InFileName,OutFileName : FileName;
  InFile,OutFile : TEXT;
  RecordSeen : BOOLEAN;
  currchar,nextchar : charinfo;
  currsym,nextsym : symbolinfo;
  CRPending : BOOLEAN;
  option : optiontable;
  sets : tableptr;
  dblch   : dblcharset;
  stack   : symbolstack;
  top,startpos,currlinepos,currmargin,
  inlines,outlines : INTEGER;
  hashtable : ARRAY [Byte] OF hashentry;

CONST
(* Keywords used for formatting
endsym,beginsym,ifsym,thensym,elsesym,procsym,varsym,ofsym,
whilesym,dosym,casesym,withsym,forsym,repeatsym,untilsym,
funcsym,labelsym,constsym,typesym,recordsym,stringsym,progsym,
andsym,arrsym,divsym,downsym,filesym,gotosym,insym,modsym,
notsym,nilsym,orsym,setsym,tosym)
*)

  Keyword : KeywordTable =
     ('END      ', 'BEGIN    ', 'IF       ', 'THEN     ',
      'ELSE     ', 'PROCEDURE', 'VAR      ', 'OF       ',
      'WHILE    ', 'DO       ', 'CASE     ', 'WITH     ',
      'FOR      ', 'REPEAT   ', 'UNTIL    ', 'FUNCTION ',
      'LABEL    ', 'CONST    ', 'TYPE     ', 'RECORD   ',
      'STRING   ', 'PROGRAM  ',
      {keywords not used for formatting }
      'AND      ', 'ARRAY    ', 'DIV      ', 'DOWNTO   ',
      'FILE     ', 'GOTO     ', 'IN       ', 'MOD      ',
      'NOT      ', 'NIL      ', 'OR       ', 'SET      ',
      'TO       '
     );

{DblCharTable = ARRAY [becomes..opencomment] OF SpecialChar;}

  DblChar : DblCharTable =
     ( ':=', '(*' );

(*
  SglCharTable = ARRAY [opencomment..period] OF CHAR;
opencomment,closecomment,semicolon,colon,equals,
openparen,closeparen,period
*)
  SglChar : SglCharTable =
    ('{', '}', ';', ':', '=', '(', ')', '.' );


{ I PPINC1.PAS }


PROCEDURE GetChar;
{ Read the next character and classify it }
  VAR  Ch: CHAR;
  BEGIN
    currchar := nextchar;
    WITH nextchar DO
      IF EOF(InFile) THEN BEGIN
        name := filemark;
        Value := Blank
      END
      ELSE IF EOLN(InFile) THEN BEGIN
        name := endofline;
        Value := Blank;
        Inc(inlines);
        READLN(InFile)
      END
      ELSE BEGIN
        READ(InFile, Ch);
        Value := Ch;
        IF Ch IN ['a'..'z', 'A'..'Z', '_'] THEN name := letter
        ELSE IF Ch IN ['0'..'9'] THEN name := digit
        ELSE IF Ch = '''' THEN name := quote
        ELSE IF (Ch = Blank) OR (Ch = CHR(TAB)) THEN name := space
        ELSE name := otherchar
      END
  END; { of GetChar }


PROCEDURE StoreNextChar(VAR lngth: INTEGER;
                        VAR Value: Token);
  { Store a character in the current symbol }
  BEGIN
    GetChar;
    IF lngth < maxsymbolsize THEN BEGIN
      Inc(lngth);
      Value[lngth] := currchar.Value
    END;
  END; { of StoreNextChar }


PROCEDURE SkipBlanks(VAR spacesbefore, crsbefore: INTEGER);
  { Count the spaces between symbols }
  BEGIN
    spacesbefore := 0;
    crsbefore := 0;
    WHILE nextchar.name IN [space, endofline] DO BEGIN
      GetChar;
      CASE currchar.name OF
        space:      Inc(spacesbefore);
        endofline:  BEGIN
                      Inc(crsbefore);
                      spacesbefore := 0;
                    END;
      END;  {case}
    END;
  END; { of SkipBlanks }


PROCEDURE GetComment(sym: symbolinfo);
  { Process comments using either brace or parenthesis notation }
  BEGIN
    sym^.name := opencomment;
    WHILE NOT (((currchar.Value = '*') AND (nextchar.Value = ')'))
    OR (currchar.Value = '}') OR (nextchar.name = endofline)
    OR (nextchar.name = filemark)) DO
      StoreNextChar(sym^.length, sym^.Value);
    IF (currchar.Value = '*') AND (nextchar.Value = ')') THEN BEGIN
      StoreNextChar(sym^.LENGTH, sym^.Value);
      sym^.name := closecomment;
    END;
    IF currchar.Value = '}' THEN sym^.name := closecomment;
  END; { of GetCommment }


FUNCTION Reset_Ok(VAR F: TEXT;
                 Name: FileName): BOOLEAN;
{ Associate name with file variable.  Return true if file is nonempty. }
  BEGIN
    Assign(F, Name);
    {$I-}  RESET(F);
    Reset_Ok := (IOResult = 0);
    {$I+}
  END; { of Reset_Ok }

{end of PPINC1}


  FUNCTION hash(Symbol: Key; lngth: Byte): Byte;
    { Hashing function for identifiers.  The formula gives a unique value
      in the range 0..255 for each Pascal/Z keyword.  Note that range and
      overflow checking must be turned off for this function even if they
      are enabled for the rest of the program.  }
    BEGIN
      hash := (ORD(Symbol[1]) * 5 + ORD(Symbol[lngth])) * 5 + lngth
    END; { of hash }


  PROCEDURE ClassID(Value: Token;
                    lngth: INTEGER;
                    VAR idtype: keysymbol;
                    VAR IsKeyWord: BOOLEAN);
    { Classify an identifier.  We are only interested
      in it if it is a keyword, so we use the hash table. }
    VAR
      Keyvalue: Key;
      i, tabent: INTEGER;
    BEGIN
      IF lngth > MAXKEYLENGTH THEN BEGIN
        idtype := othersym;
        IsKeyWord := FALSE
      END
      ELSE BEGIN

        FillChar(Keyvalue[1],MAXKEYLENGTH, Blank);  {v1.1 fill with spaces}
        FOR i := 1 TO lngth DO Keyvalue[i] := UpCase(Value[i]);

        tabent := hash(Keyvalue, lngth);
        IF Keyvalue = hashtable[tabent].Keyword THEN BEGIN
          idtype := hashtable[tabent].symtype;
          IsKeyWord := TRUE;
        END
        ELSE BEGIN
          idtype := othersym;
          IsKeyWord := FALSE;
        END
      END
    END; { of ClassID }


  PROCEDURE GetIdentifier(sym: symbolinfo);
    { Read an identifier and classify it }
    BEGIN
      WHILE nextchar.name IN [letter, digit] DO
        StoreNextChar(sym^.length, sym^.Value);
      ClassID(sym^.Value, sym^.length, sym^.name, sym^.IsKeyWord);
      IF sym^.name IN [recordsym, casesym, endsym] THEN
        CASE sym^.name OF
          recordsym : RecordSeen := TRUE;
          casesym   : IF RecordSeen THEN sym^.name := casevarsym;
          endsym    : RecordSeen := FALSE;
        END;  {case}
    END; { of GetIdentifier }


  { Read a number and store it as a string }
  PROCEDURE GetNumber(sym: symbolinfo);
    BEGIN
      WHILE nextchar.name = digit DO StoreNextChar(sym^.length, sym^.Value);
      sym^.name := othersym;
    END; { of GetNumber }


  PROCEDURE GetCharLiteral(sym: symbolinfo);
    { Read a quoted string }
    BEGIN
      WHILE nextchar.name = quote DO BEGIN
        StoreNextChar(sym^.length, sym^.Value);
        WHILE NOT (nextchar.name IN [quote, endofline, filemark]) DO
          StoreNextChar(sym^.length, sym^.Value);
        IF nextchar.name = quote THEN StoreNextChar(sym^.length, sym^.Value);
      END;
      sym^.name := othersym;
    END; { of GetCharLiteral }


  FUNCTION char_Type: keysymbol;
    { Classify a character pair }
    VAR
      NextTwoChars: SpecialChar;
      Hit: BOOLEAN;
      thischar: keysymbol;
    BEGIN
      NextTwoChars[1] := currchar.Value;
      NextTwoChars[2] := nextchar.Value;
      thischar := becomes;
      Hit := FALSE;
      WHILE NOT (Hit OR (thischar = closecomment)) DO BEGIN
        IF NextTwoChars = DblChar[thischar] THEN Hit := TRUE
        ELSE Inc(thischar);
      END;
      IF NOT Hit THEN BEGIN
        thischar := opencomment;
        WHILE NOT (Hit OR (PRED(thischar) = period)) DO BEGIN
          IF currchar.Value = SglChar[thischar] THEN Hit := TRUE
          ELSE Inc(thischar);
        END;
      END;
      IF Hit THEN char_Type := thischar
      ELSE char_Type := othersym;
    END; { of char_Type }


   PROCEDURE GetSpecialChar(sym: symbolinfo);
     { Read special characters }
    BEGIN
      StoreNextChar(sym^.length, sym^.Value);
      sym^.name := char_Type;
      IF sym^.name IN dblch THEN StoreNextChar(sym^.length, sym^.Value)
    END; { of GetSpecialChar }


  PROCEDURE GetNextSymbol(sym: symbolinfo);
    { Read a symbol using the appropriate procedure }
    BEGIN
      CASE nextchar.name OF
        letter:     GetIdentifier(sym);
        digit:      GetNumber(sym);
        quote:      GetCharLiteral(sym);
        otherchar:  BEGIN
                      GetSpecialChar(sym);
                      IF sym^.name = opencomment THEN GetComment(sym);
                    END;
        filemark:   sym^.name := endoffile;
        ELSE {:} {Turbo}
          WRITELN('Unknown character type: ', ORD(nextchar.name));
      END;  {case}
    END; { of GetNextSymbol }


  PROCEDURE GetSymbol;
  { Store the next symbol in NEXTSYM }
    VAR
      dummy: symbolinfo;
    BEGIN
      dummy := currsym;
      currsym := nextsym;
      nextsym := dummy;
      currsym^.label_replacement_value:= 0;
      SkipBlanks(nextsym^.spacesbefore, nextsym^.crsbefore);
      nextsym^.length := 0;
      nextsym^.IsKeyWord := FALSE;
      IF currsym^.name = opencomment THEN GetComment(nextsym)
      ELSE GetNextSymbol(nextsym);
    END;  {of GetSymbol}


  PROCEDURE PopStack(VAR indentsymbol: keysymbol;
                     VAR prevmargin: INTEGER);
    { Manage stack of indentation symbols and margins }
    BEGIN
      IF top > 0 THEN BEGIN
        indentsymbol := stack[top].indentsymbol;
        prevmargin := stack[top].prevmargin;
        Dec(top);
      END
      ELSE BEGIN
        indentsymbol := othersym;
        prevmargin := 0;
      END;
    END; { of PopStack }


  PROCEDURE PushStack(indentsymbol: keysymbol;
                      prevmargin: INTEGER );
    BEGIN
      Inc(top);
      stack[top].indentsymbol := indentsymbol;
      stack[top].prevmargin := prevmargin;
    END; { of PushStack }


  PROCEDURE WriteCRs(numberofcrs: INTEGER);
    VAR
      i: INTEGER;
    BEGIN
      IF numberofcrs > 0 THEN BEGIN
        FOR i := 1 TO numberofcrs DO WRITELN(OutFile);
        Inc(outlines,numberofcrs);
        currlinepos := 0;
      END;
    END; { of WriteCRs }


  PROCEDURE InsertCR;
    BEGIN
      IF currsym^.crsbefore = 0 THEN BEGIN
        WriteCRs(1);
        currsym^.spacesbefore := 0;
      END;
    END; { of InsertCR }


  PROCEDURE InsertBlankLine;
    BEGIN
      IF currsym^.crsbefore = 0 THEN BEGIN
        IF currlinepos = 0 THEN WriteCRs(1)
        ELSE WriteCRs(2);
        currsym^.spacesbefore := 0;
      END
      ELSE IF currsym^.crsbefore = 1 THEN
        IF currlinepos > 0 THEN WriteCRs(1);
    END; { of InsertBlankLine }


  PROCEDURE LShiftOn(dindsym: keysymset);
    { Move margin left according to stack configuration and current symbol }
    VAR
      indentsymbol: keysymbol;
      prevmargin: INTEGER;
    BEGIN
      IF top > 0 THEN BEGIN
        REPEAT
          PopStack(indentsymbol, prevmargin);
          IF indentsymbol IN dindsym THEN currmargin := prevmargin;
        UNTIL NOT (indentsymbol IN dindsym) OR (top = 0);
        IF NOT (indentsymbol IN dindsym) THEN
          PushStack(indentsymbol, prevmargin);
      END;
    END; { of LShiftOn }


  PROCEDURE LShift;
  { Move margin left according to stack top }
    VAR
      indentsymbol: keysymbol;
      prevmargin: INTEGER;
    BEGIN
      IF top > 0 THEN BEGIN
        PopStack(indentsymbol, prevmargin);
        currmargin := prevmargin;
(* maybe PopStack(indentsymbol,currmargin); *)
      END;
    END; { of LShift }


  PROCEDURE InsertSpace(VAR symbol: symbolinfo);
    { Insert space if room on line }
    BEGIN
      IF currlinepos < MAXLINESIZE THEN BEGIN
        WRITE(OutFile, Blank);
        Inc(currlinepos);
        IF (symbol^.crsbefore = 0) AND (symbol^.spacesbefore > 0)
        THEN Dec(symbol^.spacesbefore);
      END;
    END; { of InsertSpace }


  PROCEDURE MoveLinePos(newlinepos: INTEGER);
    { Insert spaces until correct line position reached }
    VAR  i: INTEGER;
    BEGIN
      FOR i := SUCC(currlinepos) TO newlinepos DO WRITE(OutFile, Blank);
      currlinepos := newlinepos;
    END; { of MoveLinePos }


  PROCEDURE PrintSymbol;
    { Print a symbol converting keywords to upper case }
    VAR  i,l: INTEGER;
    BEGIN
      if (currsym^.name=othersym) and (currsym^.label_replacement_value > 0)
      then begin
        l:= 6*2 + currsym^.length + 26;
        WRITE(OutFile,currsym^.label_replacement_value:6);
        WRITE(OutFile,'{ [BP2P]: Label "',currsym^.label_replacement_value:6,'" Was "');
        FOR i := 1 TO currsym^.length DO WRITE(OutFile, currsym^.Value[i]);
        WRITE(OutFile,'"}');
      end else IF (currsym^.IsKeyWord AND UPCASEKEYWORDS) THEN begin
        FOR i := 1 TO currsym^.length DO
          WRITE(OutFile, Upcase(currsym^.Value[i]));
        l:= currsym^.length;
      end ELSE begin
        FOR i := 1 TO currsym^.length DO WRITE(OutFile, currsym^.Value[i]);
        l:= currsym^.length;
      end;
      startpos := currlinepos;
      Inc(currlinepos,l);
    END; { of PrintSymbol }


  PROCEDURE PPSymbol;
  { Find position for symbol and then print it }
    VAR  newlinepos: INTEGER;
    BEGIN
      WriteCRs(currsym^.crsbefore);
      IF (currlinepos + currsym^.spacesbefore > currmargin)
      OR (currsym^.name IN [opencomment, closecomment])
      THEN newlinepos := currlinepos + currsym^.spacesbefore
      ELSE newlinepos := currmargin;

      IF newlinepos + currsym^.length > MAXLINESIZE THEN BEGIN
        WriteCRs(1);
        IF currmargin + currsym^.length <= MAXLINESIZE
        THEN newlinepos := currmargin
        ELSE IF currsym^.length < MAXLINESIZE
        THEN newlinepos := MAXLINESIZE - currsym^.length
        ELSE newlinepos := 0;
      END;
      MoveLinePos(newlinepos);
      PrintSymbol;
    END; { of PPSymbol }


  PROCEDURE Gobble(terminators: keysymset);
    { Print symbols which follow a formatting symbol but which do not
      affect layout }
    BEGIN
      IF top < MAXSTACKSIZE THEN PushStack(currsym^.name, currmargin);
      currmargin := currlinepos;
      WHILE NOT ((nextsym^.name IN terminators)
      OR (nextsym^.name = endoffile)) DO BEGIN
        GetSymbol;
        PPSymbol;
      END;
      LShift;
    END; { of Gobble }


  PROCEDURE RShift(currsym: keysymbol);
    { Move right, stacking margin positions }
    BEGIN
      IF top < MAXSTACKSIZE THEN PushStack(currsym, currmargin);
      IF startpos > currmargin THEN currmargin := startpos;
      Inc(currmargin,INDENT);
    END; { of RShift }


  PROCEDURE GoodBye;
    BEGIN
      CLOSE(InFile);
      CLOSE(OutFile);
    END;  {of GoodBye}


  PROCEDURE Initialize;
    { Initialize everything }
    VAR
      sym: keysymbol;
      psn, len: Byte;
      i,numfiles: INTEGER;                { from Command Line }
      ArgString1, ArgString2: FileName; { File name }
    BEGIN

      { Get file name and open files }

      for i:= 1 to Paramcount do
        Writeln('Arg ',i,'  ',ParamStr(i));

      numfiles := ParamCount;
      IF numfiles <> 2 THEN BEGIN
        WRITELN('Usage:  PP OldProgram NewProgram');
        HALT;
      END;
      ArgString1 := ParamStr(1);
      ArgString2 := ParamStr(2);
      WRITELN('Reading from ', ArgString1);
      IF NOT Reset_Ok(InFile, ArgString1) THEN BEGIN
        WRITELN('empty file');
        HALT;
      END;

      WRITELN('Writing to   ', ArgString2);
      Assign(OutFile, ArgString2);
      REWRITE(OutFile);

      { Initialize variables and set up control tables }

      top := 0;
      currlinepos := 0;
      currmargin := 0;
      inlines := 0;
      outlines := 0;

      { Create hash table }

      FOR psn := 0 TO MAXBYTE DO BEGIN
        hashtable[psn].Keyword := '         ';
        hashtable[psn].symtype := othersym
      END;

      FOR sym := endsym TO tosym DO BEGIN
        len := MAXKEYLENGTH;
        WHILE Keyword[sym, len] = Blank DO Dec(len);
        psn := hash(Keyword[sym], len);
        hashtable[psn].Keyword := Keyword[sym];
        hashtable[psn].symtype := sym
      END; { for }

      { Set up other special symbols }

      dblch := [becomes, opencomment];

{ Set up the sets that control formatting.  If you want PP to insert a
  line break before every statement, include CRBEFORE in the SELECTED
  set of the appropriate keywords (WHILE, IF, REPEAT, etc.).  The
  disadvantage of this is that PP will sometimes put line breaks
  where you don't want them, e.g. after ':' in CASE statements.  Note
  also that PP does not understand the Pascal/Z use of ELSE as a
  CASE label -- I wish they'd used OTHERWISE like everybody else.  }

      FOR sym := endsym TO othersym DO BEGIN
        NEW(option[sym]);
        option[sym]^.selected := [];
        option[sym]^.dindsym := [];
        option[sym]^.terminators := []
      END;

{
      option[progsym]^.selected := [blinbefore, spaft];
      option[funcsym]^.selected := [blinbefore, dindonkey, spaft];
      option[funcsym]^.dindsym := [labelsym, constsym, typesym, varsym];
      option[procsym]^.selected := [blinbefore, dindonkey, spaft];
      option[procsym]^.dindsym := [labelsym, constsym, typesym, varsym];
      option[labelsym]^.selected := [blinbefore, spaft, inbytab];
      option[constsym]^.selected := [blinbefore, dindonkey, spaft, inbytab];
      option[constsym]^.dindsym := [labelsym];
      option[typesym]^.selected := [blinbefore, dindonkey, spaft, inbytab];
      option[typesym]^.dindsym := [labelsym, constsym];
      option[varsym]^.selected := [blinbefore, dindonkey, spaft, inbytab];
      option[varsym]^.dindsym := [labelsym, constsym, typesym];
      option[beginsym]^.selected := [dindonkey, inbytab, crafter];
      option[beginsym]^.dindsym := [labelsym, constsym, typesym, varsym];
      option[repeatsym]^.selected := [inbytab, crafter];
      option[recordsym]^.selected := [inbytab, crafter];
      option[casesym]^.selected := [spaft, inbytab, gobsym, crafter];
      option[casesym]^.terminators := [ofsym];
      option[casevarsym]^.selected := [spaft, inbytab, gobsym, crafter];
      option[casevarsym]^.terminators := [ofsym];
      option[ofsym]^.selected := [crsupp, spbef];
      option[forsym]^.selected := [spaft, inbytab, gobsym, crafter];
      option[forsym]^.terminators := [dosym];
      option[whilesym]^.selected := [spaft, inbytab, gobsym, crafter];
      option[whilesym]^.terminators := [dosym];
      option[withsym]^.selected := [spaft, inbytab, gobsym, crafter];
      option[withsym]^.terminators := [dosym];
      option[dosym]^.selected := [crsupp, spbef];
      option[ifsym]^.selected := [spaft, inbytab, gobsym, crafter];
      option[ifsym]^.terminators := [thensym];
      option[thensym]^.selected := [inbytab];
      option[elsesym]^.selected := [crbefore, dindonkey, dindent, inbytab];
      option[elsesym]^.dindsym := [ifsym, elsesym];
      option[endsym]^.selected := [crbefore, dindonkey, dindent, crafter];
      option[endsym]^.dindsym := [ifsym, thensym, elsesym, forsym, whilesym,
                                 withsym, casevarsym, colon, equals];
      option[untilsym]^.selected := [crbefore, dindonkey, dindent, spaft,
                                    gobsym, crafter];
      option[untilsym]^.dindsym := [ifsym, thensym, elsesym, forsym, whilesym,
                                   withsym, colon, equals];
      option[untilsym]^.terminators := [endsym, untilsym, elsesym, semicolon];
      option[becomes]^.selected := [spbef, spaft, gobsym];
      option[becomes]^.terminators := [endsym, untilsym, elsesym, semicolon];
      option[opencomment]^.selected := [crsupp];
      option[closecomment]^.selected := [crsupp];
      option[semicolon]^.selected := [crsupp, dindonkey, crafter];
      option[semicolon]^.dindsym := [ifsym, thensym, elsesym, forsym,
                                    whilesym, withsym, colon, equals];
      option[colon]^.selected := [inbytab];
      option[equals]^.selected := [spbef, spaft, inbytab];
      option[openparen]^.selected := [gobsym];
      option[openparen]^.terminators := [closeparen];
      option[period]^.selected := [crsupp];

}

      { Start i/o }

      CrPending := FALSE;
      RecordSeen := FALSE;
      GetChar;
      NEW(currsym);
      NEW(nextsym);
      GetSymbol;

    END; { Initialize }

  { Main Program }

  { BP2P: }

  var inside_label_decl, inside_goto, possible_label: Boolean;
  beginstack: Integer;

  const max_labels = 50;
  var labels: array[1..max_labels] of symbolinfo;
  label_ptr: Integer;

  procedure Init_Replace_Label;
    var i: Integer;
  begin
    for i := 0 to max_labels do
      New(labels[i])
  end;

  procedure Replace_Label(var s:symbol);
    var i,j:integer; c1,c2:char; ok: Boolean;
  begin
    for i := 0 to label_ptr do begin
      ok:= True;
      for j:= 1 to s.length do
        ok:= ok and (Upcase(s.value[j]) = Upcase(labels[i]^.value[j]));
      if ok then
        s.label_replacement_value:= labels[i]^.label_replacement_value
    end
  end;

  procedure Maybe_Set_possible_label;
  begin
    possible_label:=
      (beginstack > 0) and (nextsym^.name=othersym) and
      (nextsym^.value[1] in ['A'..'Z','a'..'z','_'])
  end;

  BEGIN
    inside_label_decl:= False;
    inside_goto:= False;
    possible_label:= False;
    beginstack:= 0;
    label_ptr:= 0;

    Initialize;
    Init_Replace_Label;
    WHILE nextsym^.name <> endoffile DO BEGIN
      GetSymbol;

      case currsym^.name of
        beginsym :
          begin
            beginstack:= beginstack + 1;
            Maybe_Set_possible_label;
          end;
        endsym   :  beginstack:= beginstack - 1;
        labelsym :  inside_label_decl:= True;
        gotosym  :  inside_goto:= True;
        repeatsym,
        thensym,
        colon    :  Maybe_Set_possible_label;
        semicolon:
          begin
            inside_label_decl:= False;
            inside_goto:= False;
            Maybe_Set_possible_label;
          end;
        othersym :
          { identifier }
            if inside_label_decl then begin
              if not (currsym^.value[1] in ['0'..'9',',']) then begin
                label_ptr:= label_ptr+1;
                currsym^.label_replacement_value:= 100000 + label_ptr;
                labels[label_ptr]^:= currsym^;
              end
            end else if possible_label then begin
              if nextsym^.name=colon then begin
                Replace_Label(currsym^)
              end;
              possible_label:= False
            end else if inside_goto then
              Replace_Label(currsym^)
      end;

      sets := option[currsym^.name];
      IF (CrPending AND NOT (crsupp IN sets^.selected))
      OR (crbefore IN sets^.selected) THEN BEGIN
        InsertCR;
        CrPending := FALSE
      END;
      IF blinbefore IN sets^.selected THEN BEGIN
        InsertBlankLine;
        CrPending := FALSE
      END;
      IF dindonkey IN sets^.selected THEN LShiftOn(sets^.dindsym);
      IF dindent IN sets^.selected THEN LShift;
      IF spbef IN sets^.selected THEN InsertSpace(currsym);

      PPSymbol;

      IF spaft IN sets^.selected THEN InsertSpace(nextsym);
      IF inbytab IN sets^.selected THEN RShift(currsym^.name);
      IF gobsym IN sets^.selected THEN Gobble(sets^.terminators);
      IF crafter IN sets^.selected THEN CrPending := TRUE
    END;
    IF CrPending THEN WriteCRs(1);

    WRITELN(inlines: 1, ' lines read, ', outlines: 1, ' lines written.');

    GoodBye;

  END.
