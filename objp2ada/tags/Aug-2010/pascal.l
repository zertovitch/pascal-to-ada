-------------------------------------------------------------------------------
-- Name        : pascal.l
-- Description : Object Pascal lexical items for objp2ada
-- Author      : P2Ada team
-- Version     : 1.1b
-- Last update : 2010-08-21
-- Licence     : GPL V3 (http://www.gnu.org/licenses/gpl.html)
-- Contact     : http://sourceforge.net/projects/p2ada
-- Notes       :
-- Based on reference guide for Free Pascal (FPC), version 2.2.4, March 2009
-- Based on programmer's guide Turbo Pascal (TP), version 7.0, 1992
-- Based on user manual Think Pascal (THP), version 4.0, 1990
-- Based on Code Warrior Pascal (CWP) language reference manual, version 11, 1998
-- Based on Delphi Object Pascal Language Guide, version 7, 2002
-- Some parts come from newp2ada
-------------------------------------------------------------------------------

-- Aliases
UNDERSCORE \_
PERIOD \.
STAR \*
PLUS \+
SLASH \/
LPAREN \(
RPAREN \)
LBRACK \[
RBRACK \]
LBRACE \{
RBRACE \}
NOTRPARENSTAR [^)*]
NOTSTAR [^*]
NOTRBRACE [^}]
NOTTICK [^']
TICK \'
UPARROW \^

-- FPC 1.1 Tokens
-- TP 2.1
-- THP 1.0
LETTER [a-zA-Z]
DIGIT [0-9]
HEXA_DIGIT [0-9a-fA-F]
OCTAL_DIGIT [0-7]

-- FPC 1.6 Numbers
-- TP 2.4
-- THP 1.4
-- CWP 6.6
HEX_DIGIT_SEQUENCE {HEXA_DIGIT}+

-- Only FPC:
OCTAL_DIGIT_SEQUENCE {OCTAL_DIGIT}+
BIN_DIGIT_SEQUENCE [01]+

DIGIT_SEQUENCE {DIGIT}+
UNSIGNED_INTEGER {DIGIT_SEQUENCE}|"$"{HEX_DIGIT_SEQUENCE}|"&"{OCTAL_DIGIT_SEQUENCE}|"%"{BIN_DIGIT_SEQUENCE}
SCALE_FACTOR [eE][+-]?{DIGIT_SEQUENCE}

-- FPC 1.8 Character strings
-- TP 2.6
-- THP 1.6
-- CWP 6.6
STRING_CHARACTER {NOTTICK}|{TICK}{TICK}
QUOTED_STRING {TICK}({STRING_CHARACTER})*{TICK}
CONTROL_STRING "#"{UNSIGNED_INTEGER}

%%

-- FPC 1.1 Symbols
-- TP 1.1
-- THP 1.1
-- CWP 6.2
-- Special TP symbols
{PLUS} { return PLUS_t; }
"-" { return MINUS_t; }
{STAR} { return TIMES_t; }
{SLASH} { return DIVIDE_t; }
"=" { return EQUAL_t; }
"<" { return LT_t; }
">" { return GT_t; }
({LBRACK}|{LPAREN}{PERIOD}) { return LBRACK_t; }
({RBRACK}|{PERIOD}{RPAREN}) { return RBRACK_t; }
{PERIOD} { return PERIOD_t; }
"," { return COMMA_t; }
{LPAREN} { return LPAREN_t; }
{RPAREN} { return RPAREN_t; }
":" { return COLON_t; }
";" { return SEMICOLON_t; }
-- ' never given to ayacc
{UPARROW} { return UPARROW_t; }
"@" { return AT_t; }
-- (*, *), { and } never given to ayacc
-- $ and # never given to ayacc
"<>" { return NE_t; }
"<=" { return LE_t; }
">=" { return GE_t; }
":=" { return ASSIGN_t; }
{PERIOD}{PERIOD} { return DOUBLEDOT_t; }

-- Special CWP symbols
"&" {return AMPERSAND_t;}
"|" {return BAR_t;}
{STAR}{STAR} { return DOUBLESTAR_t; }

-- Special Delphi symbols
-- // never given to ayacc

-- Special FPC symbols
"+=" { return PLUSASSIGN_t; }
"-=" { return MINUSASSIGN_t; }
"*=" { return TIMESASSIGN_t; }
"/=" { return DIVIDEASSIGN_t; }

-- FPC 1.2 Comments
-- TP 2.7
-- THP 1.8
-- CWP 6.9
-- Standard Pascal comments:
-- format: (* ... *)
{LPAREN}{STAR}(({NOTSTAR}*)|({STAR}+{NOTRPARENSTAR}))*({STAR}+{RPAREN}) { null; }
-- format: { ... }
{LBRACE}{NOTRBRACE}*{RBRACE} { null; }
-- Delphi, FreePascal comments:
-- format: // ...
{SLASH}{SLASH}.*\n { null; }

-- TP 22 asm .. end
-- asm(^end)*end { return ASM_t; } -- not used as bp2p utility does the job

-- FPC 1.3 Reserved words
-- FPC 1.3.1 Turbo Pascal reserved words
-- TP 2.2 reserved words
and { return AND_t; }
array { return ARRAY_t; }
asm { return ASM_t; }
begin { return BEGIN_t; }
case { return CASE_t; }
const { return CONST_t; }
constructor { return CONSTRUCTOR_t; }
destructor { return DESTRUCTOR_t; }
div { return DIV_t; }
do { return DO_t; }
downto { return DOWNTO_t; }
else { return ELSE_t; }
end { return END_t; }
file { return FILE_t; }
for { return FOR_t; }
function { return FUNCTION_t; }
goto { return GOTO_t; }
if { return IF_t; }
implementation { return IMPLEMENTATION_t; }
in { return IN_t; }
inherited { return INHERITED_t; }
inline { return INLINE_t; }
interface { return INTERFACE_t; }
label { return LABEL_t; }
mod { return MOD_t; }
nil { return NIL_t; }
not { return NOT_t;}
object { return OBJECT_t; }
of { return OF_t; }
or { return OR_t; }
packed { return PACKED_t; }
procedure { return PROCEDURE_t; }
program { return PROGRAM_t; }
record { return RECORD_t; }
repeat { return REPEAT_t; }
set { return SET_t; }
shl { return SHL_t; }
shr { return SHR_t; }
string { return STRING_t;}
then { return THEN_t; }
to { return TO_t; }
type { return TYPE_t; }
unit { return UNIT_t; }
until { return UNTIL_t; }
uses { return USES_t; }
var { return VAR_t; }
while { return WHILE_t; }
with { return WITH_t; }
xor { return XOR_t; }

-- write, writeln and str procedures are declared as keywords
-- in order to catch the specific syntax like Write(I:4);
write { return WRITE_t; }
writeln { return WRITELN_t; }
str { return STR_t; }

-- THP 1.1 reserved words
otherwise { return OTHERWISE_t; }
univ { return UNIV_t; } -- pb with nptest1.pas

-- CWP 6.2 reserved words
c { return C_t; } -- may easily conflicts with an id c !
cycle { return CYCLE_t; }
inherit { return INHERIT_t; }
leave { return LEAVE_t; }

-- FPC 1.3.2 Delphi reserved words
as { return AS_t; }
at { return AT_t; }
class { return CLASS_t; }
dispinterface { return DISPINTERFACE_t; }
except { return EXCEPT_t; }
exports { return EXPORTS_t; }
finalization { return FINALIZATION_t; }
finally { return FINALLY_t; }
initialization { return INITIALIZATION_t; }
is { return IS_t; }
on { return ON_t; }
library { return LIBRARY_t; }
out { return OUT_t; }
property { return PROPERTY_t; }
raise { return RAISE_t; }
resourcestring { return RESOURCESTRING_t; }
threadvar { return THREADVAR_t; }
try { return TRY_t; }

-- self is declared as keyword
-- in order to catch the specific syntax like Self.Add;
self { return SELF_t; }

-- FPC 1.3.3 Free Pascal reserved words
-- for the moment let them as ids
-- dispose { return DISPOSE_t; }
-- exit { return EXIT_t; }
-- false { return FALSE_t; }
-- new { return NEW_t; }
-- true { return TRUE_t; }
operator { return OPERATOR_t; }

-- 1.3.4 modifiers
-- TP 2.2 directives
absolute { return ABSOLUTE_t; }
assembler { return ASSEMBLER_t; }
external { return EXTERNAL_t; }
far { return FAR_t; }
forward { return FORWARD_t; }
interrupt { return INTERRUPT_t; }
near { return NEAR_t; }
private { return PRIVATE_t; }
public { return PUBLIC_t; }
virtual { return VIRTUAL_t; }

-- THP 1.1 directives
override { return OVERRIDE_t; }

-- Delphi directives
-- seems to be too much, for the moment let them as reserved words
abstract { return ABSTRACT_t; }
automated { return AUTOMATED_t; }
cdecl { return CDECL_t; }
contains { return CONTAINS_t; }
default { return DEFAULT_t; }
deprecated { return DEPRECATED_t; } -- portability directive
dispid { return DISPID_t; }
dynamic { return DYNAMIC_t; }
export { return EXPORT_t; }
implements { return IMPLEMENTS_t; }
index { return INDEX_t; }
-- library { return LIBRARY_t; } -- portability directive is already in reseved word list
local { return LOCAL_t; }
message { return MESSAGE_t; }
name { return NAME_t; }
nodefault { return NODEFAULT_t; }
overload { return OVERLOAD_t; }
package { return PACKAGE_t; }
pascal { return PASCAL_t; }
platform { return PLATFORM_t; } -- portability directive
protected { return PROTECTED_t; }
published { return PUBLISHED_t; }
read { return READ_t; } -- mismatches with read procedure
readonly { return READONLY_t; }
register { return REGISTER_t; }
reintroduce { return REINTRODUCE_t; }
requires { return REQUIRES_t; }
resident { return RESIDENT_t; }
safecall { return SAFECALL_t; }
stdcall { return STDCALL_t; }
stored { return STORED_t; }
varargs { return VARARGS_t; }
-- write { return WRITE_t; } -- is already in specific word list
writeonly { return WRITEONLY_t; }

-- FPC 1.3.4 directives
-- seems to be too much, for the moment let them as reserved words
alias { return ALIAS_t; }
cppdecl { return CPPDECL_t; }
far16 { return FAR16_t; }
nostackframe { return NOSTACKFRAME_t; }
oldfpccall { return OLDFPCCALL_t; }
softload { return SOFTFLOAT_t; }
saveregisters { return SAVEREGISTERS_t; }
cvar { return CVAR_t; }
experimental { return EXPERIMENTAL_t; }
unimplemented { return UNIMPLEMENTED_t; }
generic { return GENERIC_t; }
static { return STATIC_t; }
bitpacked { return BITPACKED_t; }
specialize { return SPECIALIZE_t; }

-- FPC 1.4 Identifiers
-- TP 2.3
-- THP 1.2 (can't begin with underscore)
-- CWP 6.3
({LETTER}|{UNDERSCORE})({LETTER}|{DIGIT}|{UNDERSCORE})* { return ID_t; }

-- FPC 1.6 Numbers
-- TP 2.4
-- THP 1.4
-- CWP 6.4 (CW adds also INF and NAN)
{UNSIGNED_INTEGER} { return UNSIGNED_INTEGER_t; }
{DIGIT_SEQUENCE}(("."{DIGIT_SEQUENCE})?{SCALE_FACTOR}?|"."{SCALE_FACTOR}) { return UNSIGNED_REAL_t; }

-- FPC 1.8 Character strings
-- TP 2.6
-- THP 1.6 (no control string permitted)
-- CWP 6.6 (no control string permitted)
({QUOTED_STRING}|{CONTROL_STRING})+ { return CHARACTER_STRING_t; }

-- capture all remaining blank characters
. { null; }
\n { null; }

%%

WITH Pascal_Tokens;
WITH PascalHelp;
USE Pascal_Tokens;

##
