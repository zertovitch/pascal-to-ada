package body BorStrings is

  function To_String( b:BorString ) return String is
  begin
    return b.s( 1 .. b.length );
  end To_String;
  
  procedure Put( b: in out BorString; c: Character ) is
  begin
    b.length:= 1;
    b.s( 1 ):= c;
  end Put;

  procedure Put( b: in out BorString; s: String ) is
  begin
    if s'length > b.maxlength then
      raise string_overflow;
    end if;
    b.length:= s'length;
    b.s( s'range ):= s;
  end Put;

  procedure Put( bd: in out BorString; bs: BorString ) is
  begin
    Put( bd, To_String( bs ) ); -- reuse previous!
  end Put;

  function Copy(b: BorString; index: Integer; count: Integer) return String is
  begin
    return b.s( index .. index+count-1 );
  end Copy;

  procedure Delete( b: in out BorString; index: Positive; count: Positive) is
  n_after_min_1: Integer; -- -1 .. length-2
  begin
    if index > b.length or index+count > b.length then return; end if;
    -- "123456789"
    --     xxx
    n_after_min_1:= b.length - (index+count);
    b.s( index .. index+n_after_min_1 ):=
      b.s( index+count .. index+count+n_after_min_1 );
    b.length:= b.length - count;
  end Delete;

  procedure Insert( source: String; b: in out BorString; index: Integer) is
  n_after_min_1: Integer; -- -1 .. length-2
  count: Natural:= source'length;
  begin
    if index > b.length then return; end if;
    if b.length + count > b.maxlength then
      raise string_overflow;
    end if;
    -- "123456789"
    --        ^
    n_after_min_1:= b.length - index;
    b.s( index+count .. index+count+n_after_min_1 ):=
      b.s( index .. index+n_after_min_1 );
    b.s( index .. index+count-1 ):= source;
    b.length:= b.length + count;
  end Insert;

  procedure Insert( source: BorString; b: in out BorString; index: Integer) is
  begin
    Insert( To_String(source), b, index );
  end Insert;

  function  Length( b: BorString ) return Natural is
  begin
    return b.length;
  end Length;

  function  Pos( substr: string; b: BorString) return Natural is
    l: integer:= substr'last-substr'first;
    begin
      if b.length=0 then return 0; end if;
      if substr'length=0 then return 1; end if; -- 29.XI.1997 (=TP) :
      for i in 1..b.length - l loop         -- empty is everywhere...
        if b.s(i..i+l)=substr then return i; end if;
      end loop;
      return 0;
    end Pos;

  function  RPos( substr: string; b: BorString) return Natural is
    l: integer:= substr'last-substr'first;
    begin
      if b.length=0 then return 0; end if;
      if substr'length=0 then return b.length; end if; -- 29.XI,7.XII.1997
      for i in reverse 1..b.length - l loop
        if b.s(i..i+l)=substr then return i; end if;
      end loop;
      return 0;
    end RPos;

  ---------------- Concatenations ---------------------------------

  function  "&" ( c: Character; b: BorString) return BorString is
  r: BorString( b.maxlength );
  total: Natural:= b.length + 1;
  begin
    if total > r.maxlength then raise string_overflow; end if;
    r.length:= total;
    r.s( 1 ):= c;
    r.s( 2..total ):= b.s( 1..b.length );
    return r;
  end "&";

  function  "&" ( s: String; b: BorString) return BorString is
  r: BorString( b.maxlength );
  total: Natural:= b.length + s'length;
  begin
    if total > r.maxlength then raise string_overflow; end if;
    r.length:= total;
    r.s( 1..total ):= s & b.s( 1..b.length );
    return r;
  end "&";

  function  "&" ( b: BorString; c: Character) return BorString is
  r: BorString( b.maxlength ):= b;
  total: Natural:= b.length + 1;
  begin
    if total > r.maxlength then raise string_overflow; end if;
    r.length:= total;
    r.s( total ):= c;
    return r;
  end "&";

  function  "&" ( b: BorString; s: String) return BorString is
  r: BorString( b.maxlength );
  total: Natural:= b.length + s'length;
  begin
    if total > r.maxlength then raise string_overflow; end if;
    r.length:= total;
    r.s( 1..total ):= b.s( 1..b.length ) & s;
    return r;
  end "&";

  --- Concat to string

  function  "&" ( c: Character; b: BorString) return String is
  total: Natural:= b.length + 1;
  r: String( 1..total );
  begin
    r( 1 ):= c;
    r( 2..total ):= b.s( 1..b.length );
    return r;
  end "&";

  function  "&" ( s: String; b: BorString) return String is
  total: Natural:= b.length + s'length;
  r: String( 1..total );
  begin
    r( 1..total ):= s & b.s( 1..b.length );
    return r;
  end "&";

  function  "&" ( b: BorString; c: Character) return String is
  total: Natural:= b.length + 1;
  r: String( 1..total );
  begin
    r( total ):= c;
    return r;
  end "&";

  function  "&" ( b: BorString; s: String) return String is
  total: Natural:= b.length + s'length;
  r: String( 1..total );
  begin
    r( 1..total ):= b.s( 1..b.length ) & s;
    return r;
  end "&";

  --- end of concat to string
        
  function Eq ( b1,b2: BorString ) return Boolean is
  begin
    return b1.length = b2.length and then
           b1.s( 1..b1.length ) = b2.s( 1..b2.length);
  end Eq;

  function Eq ( b1: BorString; s2: String ) return Boolean is
  begin
    return b1.length = s2'length and then b1.s( 1..b1.length ) = s2;
  end Eq;
  
end BorStrings;
