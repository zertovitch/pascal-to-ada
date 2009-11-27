-- 7-Jan-2007 [GdM]: Added TRY_t, EXCEPT_t, FINALLY_t

-- 26-Dec-2006 [GdM]
--   added eol_in_comment increment (part of line count for input)

-- 15-Nov-2006 [GdM]
-- (B-F) Added //-style comments

-- 31-Dec-2003 [PP]
-- (PO-B) Added constructor, destructor, virtual, private, public, protected tokens
-- (PO-C) Added inherited, override tokens

-- 01-Oct-2003 [PP]
-- (PO) Added OBJECT_t token

-- 31-May-2003 [GdM]
-- (s) figures like "1.e-5" accepted (no '0').

-- 29-Jan-2003 [GdM]
-- (B) Removal of EXIT_t (-> into identifier table)

-- 27-Jan-2003 [GdM]
-- (s) Fixed: record fields can be empty.
-- (s) Removal of CHR_t, ORD_t, EOF_t, EOLN_t (-> into identifier table)

-- 25-Jan-2003 [GdM]
-- (s) Tokens CHAR_t, REAL_t, DOUBLE_t, PARAMSTR_t, PARAMCOUNT_t,
--     UPCASE_t, TRUE_t, FALSE_t, NIL_t
--     removed: Ada aliases of predefined types are now handled

-- 22-Jan-2003 [GdM]
-- (s) (. and .) recognized as [ and ].

-- 14-Jan-2003 [GdM]
--   Token for Absolute (Borland's untyped "renames")

-- 13-Jan-2003 [GdM]
-- * Tokens for Extended Pascal ISO 10206:1990: and_then, or_else
-- * Removed ASM_t : cleanly commented out by BP2P before P2Ada

-- 8,9-Jan-2003 [GdM]
--   Tokens for Borland's ParamCount, ParamStr, UpCase, Inc, Dec

-- 28-Dec-2002 [GdM]
--   LETTER [a-zA-Z_] (was [a-zA-Z])

-- 21,17,15-Dec-2002 [GdM]
--   Tokens for Read, ReadLn, Ord, Chr, New, Sqr, Odd, Eof, Eoln, Double

-- 20-Nov-2002 [GdM]
--   General decimal numbers (integer/float,with optional exponent)

-- 15-Nov-2002 [GACC]
-- - Resolved (* .... ** ....*) bug
-- - Added some exponent litterals (1.2345e7)

-- 16-Jan-2002 [GdM]
-- EXIT_t exit statement (is Ada's return)

-- * GdM xi.1999 *
-- added xor token

-- * GdM 26.viii.1999 *
-- '' handled
-- characters with explicit ascii (#223)
--
-- $Id: Pascal.l,v 2.2 1997/09/20 17:48:31 nestor Exp $
-- $Locker:  $
-- $Log: Pascal.l,v $
-- Revision 2.2  1997/09/20 17:48:31  nestor
-- Correct two problems with comments: remove the original comment
-- delimiters in Ada comments, admit odd number of stars between
-- delimiters in comments of this kind (*****).
--
-- Revision 2.1  1997/08/23  17:16:27  nestor
-- Laurent Gasser's correction for MacOS extended Pascal
-- added tokens: unit, interface, implementation, uses
-- remove reserved keywords in upper-case, use case insensitivity
-- remove definitions of LT [<] and GT [>]
-- add tokens @ and &
-- add hexadecimal notation
--
-- Revision 1.1  1997/08/22  21:00:01  nestor
-- Martin C. Carlisle's original version, standard Pascal
--
-- aflex specification for Pascal
--
-- Martin C. Carlisle, US Air Force Academy
-- mcc@cs.usafa.af.mil
-- http://www.usafa.af.mil/dfcs/bios/carlisle.html
-- November 26, 1996

-- Laurent Gasser, Hirondelle Lumineuse
-- lga@sma.ch
-- June 15, 1997
-- Adaptation to extended MacOS Pascal
--
-- added tokens: unit, interface, implementation, uses
-- remove reserved keywords in upper-case, use case insensitivity
-- remove definitions of LT [<] and GT [>]
-- add tokens @ and &
-- add hexadecimal notation
--
-- usage: aflex -i Pascal.l

LETTER [a-zA-Z_]
DIGIT [0-9]
HEXA [0-9a-fA-F]
ALPHANUM [0-9a-zA-Z]
IDCHAR [0-9a-zA-Z_]
PERIOD [.]
STAR [*]
PLUS [+]
SLASH [/]
LPAREN [(]
RPAREN [)]
LBRACK \[
RBRACK \]
LBRACE [{]
RBRACE [}]
NOTRPARENSTAR [^)*]
NOTSTAR [^*]
NOTRBRACE [^}]
NOTTICK [^']
TICK [']
UPARROW \^

%%

--------------
-- Comments --
--------------

-- Standard Pascal comments:
-- (* ... *)
{LPAREN}{STAR}(({NOTSTAR}*)|({STAR}+{NOTRPARENSTAR}))*({STAR}+{RPAREN}) {
         PascalHelp.Comment(YYText(3..YYLength-2));
         }
-- { ... }
{LBRACE}{NOTRBRACE}*{RBRACE} {PascalHelp.Comment(YYText(2..YYLength-1));}

-- Delphi, FreePascal comments:
{SLASH}{SLASH}.*\n  { -- Skip rest of line.
                      PascalHelp.Comment(YYText(3..YYLength-1));
                      PascalHelp.eol_in_comment:=
                        PascalHelp.eol_in_comment + 1; -- catch the \n
                    }


-- Regular pascal stuff
"|" {return BAR_t;}
"&" {return AMPERSAND_t;}
"@" {return AT_t;}
({LBRACK}|{LPAREN}{PERIOD}) {return LBRACK_t;}
({RBRACK}|{PERIOD}{RPAREN}) {return RBRACK_t;}
, { return COMMA_t; }
{UPARROW} { return UPARROW_t; }
{PERIOD}{PERIOD} { return DOUBLEDOT_t; }
{PERIOD} { return PERIOD_t; }
; { return SEMICOLON_t; }
:= {return ASSIGN_t;}
= {return EQUAL_t;}
: { return COLON_t; }
-> { return ARROW_t; }
{PLUS} { return PLUS_t; }
- { return MINUS_t; }
"<=" { return LE_t; }
">=" { return GE_t; }
"<>" { return NE_t; }
"<" { return LT_t; }
">" { return GT_t; }
{STAR}{STAR} { return DOUBLESTAR_t; }
{STAR} { return TIMES_t; }
{SLASH} { return DIVIDE_t; }
div {return DIV_t; }
mod {return MOD_t; }
shl {return SHL_t; }
shr {return SHR_t; }
{LPAREN} {return LPAREN_t;}
{RPAREN} {return RPAREN_t;}
and {return AND_t;}
or {return OR_t;}
xor {return XOR_t;}
not {return NOT_t;}
begin { return BEGIN_t; }
in { return IN_t; }
to { return TO_t; }
program { return PROGRAM_t; }
external { return EXTERNAL_t; }
otherwise { return OTHERWISE_t; }
end { return END_t; }
with { return WITH_t; }
label { return LABEL_t; }
const { return CONST_t; }
type { return TYPE_t; }
var { return VAR_t; }
array { return ARRAY_t; }
record { return RECORD_t; }
packed { return PACKED_t; }
of { return OF_t; }
case { return CASE_t; }
set { return SET_t; }
file { return FILE_t; }
forward { return FORWARD_t; }
procedure { return PROCEDURE_t; }
function { return FUNCTION_t; }
goto { return GOTO_t; }
if { return IF_t; }
for { return FOR_t; }
while { return WHILE_t; }
repeat { return REPEAT_t; }
until { return UNTIL_t; }
do { return DO_t; }
then { return THEN_t; }
else { return ELSE_t; }
downto { return DOWNTO_t; }
str { return STR_t; }
new { return NEW_t; }
write { return WRITE_t; }
writeln { return WRITELN_t; }
read { return READ_t; }
readln { return READLN_t; }
inc {return INC_t;}
dec {return DEC_t;}
unit {return UNIT_t;}
uses {return USES_t;}
interface {return INTERFACE_t;}
implementation {return IMPLEMENTATION_t;}
and_then {return AND_THEN_t;}
or_else {return OR_ELSE_t;}
absolute {return ABSOLUTE_t;}
object {return OBJECT_t;}
constructor {return CONSTRUCTOR_t;}
destructor {return DESTRUCTOR_t;}
virtual {return VIRTUAL_t;}
private {return PRIVATE_t;}
public {return PUBLIC_t;}
protected {return PROTECTED_t;}
override {return OVERRIDE_t;}
inherited {return INHERITED_t;}
try {return TRY_t;}
except {return EXCEPT_t;}
finally {return FINALLY_t;}

{LETTER}{IDCHAR}* {return ID_t;}
{DIGIT}+({PERIOD}{DIGIT}+)?("E"({PLUS}|-)?{DIGIT}+)? {return CONSTANT_t;}
{DIGIT}+{PERIOD}"E"({PLUS}|-)?{DIGIT}+ {return CONSTANT_t;}
"$"{HEXA}+ {return HEXADECIMAL_t;}
"#"{DIGIT}+ {return ASCII_t;}
'{NOTTICK}'|'''' {return CHAR_CONST_t;}
'({NOTTICK}|{TICK}{TICK}('')?)*' {return STRING_t;}

%%

WITH Pascal_Tokens;
WITH PascalHelp;
USE Pascal_Tokens;

##
