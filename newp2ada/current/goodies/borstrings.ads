------------------------------------------------------------------------------
--  File:           BorString.ads
--  Description:    Variable-size, bounded strings as in early Borland Pascals
--                      Essentially for quick portability purposes.
--  Date / Version: 14-May-2001 ; 4-May-2000
--  Author:         Gautier de Montmollin
--  Portability:    Full: pure Ada 83. Nearest Ada 95+ package: the
--                  generic Ada.Strings.Bounded .
------------------------------------------------------------------------------

package BorStrings is

  type BorString( maxlength: positive ) is private;

  -- Idea: you can convert to string for _functional_ manipulations
  function To_String( b:BorString ) return String;
  -- ... then, put the result in a BorString if needed
  procedure Put( b : in out BorString; c: Character );
  procedure Put( b : in out BorString; s: String );
  procedure Put( bd: in out BorString; bs: BorString );
  -- Note: with the "&" concatenators defined below, you can do it
  -- straigthforward !

  -- * Procedures and functions that are in Turbo Pascal and later:
  --     Concat   Copy   Delete   Insert   Length   Pos
  -- * We add RPos for searching a string from the _right_

  --        Concat: use the "&" operator instead! NB: TP has "+"
  function  Copy(b: BorString; index: Integer; count: Integer) return String;
  procedure Delete( b: in out BorString; index: Positive; count: Positive);
  procedure Insert( source: String; b: in out BorString; index: Integer);
  procedure Insert( source: BorString; b: in out BorString; index: Integer);
  function  Length( b: BorString ) return Natural;
  function  Pos( substr: String; b: BorString) return Natural;
  function  RPos( substr: String; b: BorString) return Natural;

  -- "&" operators returning a BorString of _same_ maxlength as the input
  -- so you can write  b:= b & " "  or b:= "[" & b & "]" !

  function  "&" ( c: Character; b: BorString) return BorString;
  function  "&" ( s: String; b: BorString) return BorString;
  function  "&" ( b: BorString; c: Character) return BorString;
  function  "&" ( b: BorString; s: String) return BorString;

  -- same, returning strings (experimental: can cause ambiguities)

  function  "&" ( c: Character; b: BorString) return String;
  function  "&" ( s: String; b: BorString) return String;
  function  "&" ( b: BorString; c: Character) return String;
  function  "&" ( b: BorString; s: String) return String;

  function Eq ( b1,b2: BorString ) return Boolean;
  function Eq ( b1: BorString; s2: String ) return Boolean;

  string_overflow: exception;

  pragma Inline(To_String);

private
  type BorString( maxlength: positive ) is record
    length: Natural:= 0;
    s: String( 1..maxlength );
  end record;
  -- NB: length allows longer strings than the "s[0]" in T/B-Pascal

end BorStrings;
