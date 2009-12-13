-------------------------------------------------------------------------------
-- Name        : pascal.y
-- Description : Object Pascal grammar for objP2Ada
-- Author      : P2Ada team
-- Version     : 1.1a
-- Last update : 2009-12-12
-- Licence     : GPL V3 (http://www.gnu.org/licenses/gpl.html)
-- Contact     : http://sourceforge.net/projects/P2Ada
-- Notes       :
-- Based on reference guide for Free Pascal (FPC), version 2.2.4, March 2009
-- Based on programmer's guide Turbo Pascal (TP), version 7.0, 1992
-- Based on user manual Think Pascal (THP), version 4.0, 1990
-- Based on Code Warrior Pascal (CWP) language reference manual, version 11, 1998
-- Based on Delphi Object Pascal Language Guide, version 7, 2002
-- Some parts come from newP2Ada
-------------------------------------------------------------------------------

--{ P2Ada to do:
--{   add surrounding package over objects and classes
--{   add surrounding generic package over generic classes
--{   for function result replace "function_name :=" by "P2Ada_function_name :=" and add "return P2Ada_function_name" at then end
--{   translate identifier with underscore at beginning, with consecutive underscores, Ada keywords
--{   manage anonymous type inside upper type

-- syntax examples:
-- element : like this;
-- repeat_list : repeat_list repeat | repeat;
-- possibilities : first | second;
-- possibilities_with_empty : first | second | ;

-- FPC 1.1 Symbols
-- TP 1.1
-- THP 1.1
-- CWP 6.2
-- Special TP symbols
%token PLUS_t
%token MINUS_t
%token TIMES_t
%token DIVIDE_t -- e.g. /
%token EQUAL_t
%token LT_t
%token GT_t
%token LBRACK_t -- includes (.
%token RBRACK_t -- includes .)
%token PERIOD_t
%token COMMA_t
%token LPAREN_t
%token RPAREN_t
%token COLON_t
%token SEMICOLON_t
%token UPARROW_t
%token AT_t -- @
-- (*, *), { and } never given to ayacc
-- ', $ and # never given to ayacc
%token NE_t
%token LE_t
%token GE_t
%token ASSIGN_t
%token DOUBLEDOT_t

-- Special CWP symbols
%token AMPERSAND_t
%token BAR_t
%token DOUBLESTAR_t

-- Special Delphi symbols
-- // never given to ayacc

-- Special FPC symbols
%token PLUSASSIGN_t
%token MINUSASSIGN_t
%token TIMESASSIGN_t
%token DIVIDEASSIGN_t

-- FPC 1.3 Reserved words
-- FPC 1.3.1 Turbo Pascal reserved words
-- TP 2.2 reserved words
%token AND_t
%token ARRAY_t
%token ASM_t
%token BEGIN_t
%token CASE_t
%token CONST_t
%token CONSTRUCTOR_t
%token DESTRUCTOR_t
%token DIV_t
%token DO_t
%token DOWNTO_t
%token ELSE_t
%token END_t
%token FILE_t
%token FOR_t
%token FUNCTION_t
%token GOTO_t
%token IF_t
%token IMPLEMENTATION_t
%token IN_t
%token INHERITED_t
%token INLINE_t
%token INTERFACE_t
%token LABEL_t
%token MOD_t
%token NIL_t
%token NOT_t
%token OBJECT_t
%token OF_t
%token OR_t
%token PACKED_t
%token PROCEDURE_t
%token PROGRAM_t
%token RECORD_t
%token REPEAT_t
%token SET_t
%token SHL_t
%token SHR_t
%token STRING_t
%token THEN_t
%token TO_t
%token TYPE_t
%token UNIT_t
%token UNTIL_t
%token USES_t
%token VAR_t
%token WHILE_t
%token WITH_t
%token XOR_t

-- write, writeln and str procedures are declared as keywords
-- in order to catch the specific syntax like Write(I:4);
%token WRITE_t
%token WRITELN_t
%token STR_t

-- THP 1.1 reserved words
%token OTHERWISE_t
%token UNIV_t

-- CWP 6.2 reserved words
%token C_t -- may easily conflicts with an id c
%token CYCLE_t
%token INHERIT_t
%token LEAVE_t

-- FPC 1.3.2 Delphi reserved words
%token AS_t
%token AT_t
%token CLASS_t
%token DISPINTERFACE_t
%token EXCEPT_t
%token EXPORTS_t
%token FINALIZATION_t
%token FINALLY_t
%token INITIALIZATION_t
%token IS_t
%token LIBRARY_t
%token ON_t
%token OUT_t
%token PROPERTY_t
%token RAISE_t
%token REINTRODUCE_t
%token RESOURCESTRING_t
%token THREADVAR_t
%token TRY_t

-- self is declared as keyword
-- in order to catch the specific syntax like Self.Add;
%token SELF_t

-- FPC 1.3.3 Free Pascal reserved words
-- for the moment let them as ids
--%token DISPOSE_t
--%token EXIT_t
--%token FALSE_t
--%token NEW_t
--%token TRUE_t
%token OPERATOR_t -- extension not in Delphi manual

-- FPC 1.3.4 modifiers
-- TP 2.2 directives
%token ABSOLUTE_t
%token ASSEMBLER_t
%token EXTERNAL_t
%token FAR_t
%token FORWARD_t
%token INTERRUPT_t
%token NEAR_t
%token PRIVATE_t -- class visibility
%token PUBLIC_t -- class visibility
%token VIRTUAL_t

-- THP 1.1 directives
%token OVERRIDE_t

-- Delphi directives
-- seems to be too much, for the moment let them as reserved words
%token ABSTRACT_t
%token AUTOMATED_t -- class visibility
%token CDECL_t
%token CONTAINS_t
%token DEFAULT_t
%token DEPRECATED_t -- portability directive
%token DISPID_t
%token DYNAMIC_t
%token EXPORT_t
%token IMPLEMENTS_t
%token INDEX_t
-- %token LIBRARY_t -- portability directive is already in reseved word list
%token LOCAL_t
%token MESSAGE_t
%token NAME_t
%token NODEFAULT_t
%token OVERLOAD_t
%token PACKAGE_t
%token PASCAL_t
%token PLATFORM_t -- portability directive
%token PROTECTED_t -- class visibility
%token PUBLISHED_t -- class visibility
%token READ_t -- may mismatches with read procedure
%token READONLY_t -- usage not described in Delphi Manual, is it really used ?
%token REGISTER_t
%token REINTRODUCE_t
%token REQUIRES_t
%token RESIDENT_t
%token SAFECALL_t
%token STDCALL_t
%token STORED_t
%token VARARGS_t
-- %token WRITE_t -- is already taken as reseved word
%token WRITEONLY_t -- usage not described in Delphi Manual, is it really used ?

-- FPC 1.3.4 directives
-- seems to be too much, for the moment let them as reserved words
%token ALIAS_t
%token CPPDECL_t
%token FAR16_t
%token NOSTACKFRAME_t
%token OLDFPCCALL_t
%token SOFTFLOAT_t
%token SAVEREGISTERS_t
%token CVAR_t
%token EXPERIMENTAL_t
%token UNIMPLEMENTED_t
%token GENERIC_t
%token STATIC_t
%token BITPACKED_t
%token SPECIALIZE_t

-- FPC 1.4 Identifiers
-- TP 2.3
-- THP 1.2
-- CWP 6.3
%token ID_t

-- FPC 1.6 Numbers
-- TP 2.4
-- THP 1.4
-- CWP 6.4
%token UNSIGNED_INTEGER_t
%token UNSIGNED_REAL_t
%token CHARACTER_STRING_t

%start object_pascal

-- With clause and token type inserted in Pascal token file
%with Ada.Strings.Unbounded;
{ subtype YYSType is Ada.Strings.Unbounded.Unbounded_String; }

%%

-- FPC 1.4 Identifiers
-- TP 2.3
-- THP 1.2
-- CWP 6.3
identifier : ID_t
   { $$ := To_Ada_Identifier(YYText); }
   -- directives may be used as actual user defined identifiers
   -- TP directives used as identifiers in right place
   | ABSOLUTE_t
   { $$ := To_Unbounded_String("absolute"); }
   | ASSEMBLER_t
   { $$ := To_Unbounded_String("assembler"); }
   | EXTERNAL_t
   { $$ := To_Unbounded_String("external"); }
   | FAR_t
   { $$ := To_Unbounded_String("far"); }
   | FORWARD_t
   { $$ := To_Unbounded_String("forward"); }
   | INTERRUPT_t
   { $$ := To_Unbounded_String("interrupt"); }
   | NEAR_t
   { $$ := To_Unbounded_String("near"); }
   | PRIVATE_t -- Ada reserved word
   { $$ := To_Unbounded_String("private_k"); }
   | PUBLIC_t
   { $$ := To_Unbounded_String("public"); }
   | VIRTUAL_t
   { $$ := To_Unbounded_String("virtual"); }
   -- THP keyword used as identifier in right place, should be suppressed with Pascal mode option
   | UNIV_t
   { $$ := To_Unbounded_String("univ"); }
   -- CWP directives used as identifiers in right place, should be suppressed with Pascal mode option
   | C_t
   { $$ := To_Unbounded_String("c"); }
   | CYCLE_t
   { $$ := To_Unbounded_String("cycle"); }
   | INHERIT_t
   { $$ := To_Unbounded_String("inherit"); }
   | LEAVE_t
   { $$ := To_Unbounded_String("leave"); }
   -- Delphi directives used as identifiers in right place
   | ABSTRACT_t -- Ada reserved word
   { $$ := To_Unbounded_String("abstract_k"); }
   | AUTOMATED_t -- class visibility
   { $$ := To_Unbounded_String("automated"); }
   | CDECL_t
   { $$ := To_Unbounded_String("cdecl"); }
   | CONTAINS_t
   { $$ := To_Unbounded_String("contains"); }
   | DEFAULT_t
   { $$ := To_Unbounded_String("default"); }
   | DEPRECATED_t -- portability directive
   { $$ := To_Unbounded_String("deprecated"); }
   | DISPID_t
   { $$ := To_Unbounded_String("dispid"); }
   | DYNAMIC_t
   { $$ := To_Unbounded_String("dynamic"); }
   | EXPORT_t
   { $$ := To_Unbounded_String("export"); }
   | IMPLEMENTS_t
   { $$ := To_Unbounded_String("implements"); }
   | INDEX_t
   { $$ := To_Unbounded_String("index"); }
   | LOCAL_t
   { $$ := To_Unbounded_String("local"); }
   | MESSAGE_t
   { $$ := To_Unbounded_String("message"); }
   | NAME_t
   { $$ := To_Unbounded_String("name"); }
   | NODEFAULT_t
   { $$ := To_Unbounded_String("nodefault"); }
   | OVERLOAD_t
   { $$ := To_Unbounded_String("overload"); }
   | PACKAGE_t -- Ada reserved word
   { $$ := To_Unbounded_String("package_k"); }
   | PASCAL_t
   { $$ := To_Unbounded_String("pascal"); }
   | PLATFORM_t -- portability directive
   { $$ := To_Unbounded_String("platform"); }
   | PROTECTED_t -- class visibility -- Ada reserved word
   { $$ := To_Unbounded_String("protected_k"); }
   | PUBLISHED_t -- class visibility
   { $$ := To_Unbounded_String("published"); }
   | READ_t -- may mismatches with read procedure
   { $$ := To_Unbounded_String("read"); }
   | READONLY_t -- usage not described in Delphi Manual, is it really used
   { $$ := To_Unbounded_String("readonly"); }
   | REGISTER_t
   { $$ := To_Unbounded_String("register"); }
   | REINTRODUCE_t
   { $$ := To_Unbounded_String("reintroduce"); }
   | REQUIRES_t
   { $$ := To_Unbounded_String("requires"); }
   | RESIDENT_t
   { $$ := To_Unbounded_String("resident"); }
   | SAFECALL_t
   { $$ := To_Unbounded_String("safecall"); }
   | STDCALL_t
   { $$ := To_Unbounded_String("stdcall"); }
   | STORED_t
   { $$ := To_Unbounded_String("stored"); }
   | VARARGS_t
   { $$ := To_Unbounded_String("varargs"); }
   | WRITEONLY_t -- usage not described in Delphi Manual, is it really used ?
   { $$ := To_Unbounded_String("writeonly"); }
   -- FPC directives used as identifiers in right place
   | ALIAS_t
   { $$ := To_Unbounded_String("alias"); }
   | CPPDECL_t
   { $$ := To_Unbounded_String("cppdecl"); }
   | FAR16_t
   { $$ := To_Unbounded_String("far16"); }
   | NOSTACKFRAME_t
   { $$ := To_Unbounded_String("nostackframe"); }
   | OLDFPCCALL_t
   { $$ := To_Unbounded_String("oldfpccall"); }
   | SOFTFLOAT_t
   { $$ := To_Unbounded_String("softfloat"); }
   | SAVEREGISTERS_t
   { $$ := To_Unbounded_String("saveregisters"); }
   | CVAR_t
   { $$ := To_Unbounded_String("cvar"); }
   | EXPERIMENTAL_t
   { $$ := To_Unbounded_String("experimental"); }
   | UNIMPLEMENTED_t
   { $$ := To_Unbounded_String("unimplemented"); }
   | GENERIC_t -- Ada reserved word
   { $$ := To_Unbounded_String("generic_k"); }
   | STATIC_t
   { $$ := To_Unbounded_String("static"); }
   | BITPACKED_t
   { $$ := To_Unbounded_String("bitpacked"); }
   | SPECIALIZE_t
   { $$ := To_Unbounded_String("specialize"); }
   -- specific ids declared as keywords by P2Ada
   | WRITE_t
   { $$ := To_Unbounded_String("Write"); }
   | WRITELN_t
   { $$ := To_Unbounded_String("WriteLn"); }
   | STR_t
   { $$ := To_Unbounded_String("Str"); }
   ;
identifier_list : identifier_list COMMA_t identifier
   { $$ := $1 & ", " & $3; }
   | identifier
   { $$ := $1; }
   ;

-- FPC 1.5 Hint directives
-- TP not defined
-- THP not defined
-- CWP not defined
-- Delphi only DEPRECATED_t, PLATFORM_t and LIBRARY_t
-- LIBRARY_t not defined in FPC manual
hint_directive : -- portability directives in Delphi manual
   DEPRECATED_t
   { $$ := "-- P2Ada: deprecated" & NL; }
   | LIBRARY_t
   { $$ := "-- P2Ada: library" & NL; }
   | PLATFORM_t
   { $$ := "-- P2Ada: platform" & NL; }
   | EXPERIMENTAL_t
   { $$ := "-- P2Ada: experimental" & NL; }
   | UNIMPLEMENTED_t
   { $$ := "-- P2Ada: unimplemented" & NL; }
   |
   { $$ := Null_Unbounded_String; }
   ;
-- portability_directive : PLATFORM_t | DEPRECATED_t | LIBRARY_t; -- from Delphi manual

-- FPC 1.6 Numbers
-- TP 2.4
-- THP 1.4
-- CWP 6.4
unsigned_integer : UNSIGNED_INTEGER_t
   { $$ := To_Ada_Integer(YYText); }
   ;
sign : PLUS_t
   { $$ := To_Unbounded_String("+"); }
   | MINUS_t
   { $$ := To_Unbounded_String("-"); }
   ;
unsigned_real : UNSIGNED_REAL_t
   { $$ := To_Ada_Float(YYText); }
   ;
unsigned_number : unsigned_integer
   { $$ := $1; }
   | unsigned_real
   { $$ := $1; }
   ;
-- signed_number : sign unsigned_number | unsigned_number; -- not used !

-- FPC 1.7 Labels
-- TP 2.5
-- THP 1.5 (no identifier permitted)
-- CWP 6.5
label : unsigned_integer
   { $$ := "P2Ada_" & $1; }
   | identifier
   { $$ := $1; }
   ;

-- FPC 1.8 Character strings
-- TP 2.6
-- THP 1.6
-- CWP 6.6
character_string : CHARACTER_STRING_t
   { $$ := To_Ada_String(YYText); }
   ;

-- FPC 2.1 ordinary constants
-- TP 3
-- THP 1.7
-- CWP 8.1
constant_declaration : identifier EQUAL_t constant hint_directive SEMICOLON_t -- without loop because it is done at const section
   { $$ := $1 & " : constant := " & $3 & ';' & NL & $4 & "-- P2Ada to do: give a type to char, string, boolean and set constants " & NL; }
   ;
constant : expression -- from TP, CWP and Delphi manual
   { $$ := $1; }
   ;
-- constant : identifier | sign identifier | signed-number | character_string; -- from THP manual

-- FPC 2.2 Typed constants
-- TP 5.2
-- THP not defined
-- CWP 8.1
typed_constant_declaration : identifier COLON_t type EQUAL_t typed_constant hint_directive SEMICOLON_t -- without loop because it is done at const section
   { $$ := $1 & " : constant " & $3 & " := " & $5 & ';' & NL & $6; }
   ;
typed_constant : constant
   { $$ := $1; }
   -- | address_constant -- equivalent to constant definition
   | array_constant
   { $$ := $1; }
   | record_constant
   { $$ := $1; }
   -- | procedural_constant -- equivalent to constant definition
   ;
typed_constant_list : typed_constant_list COMMA_t typed_constant
   { $$ := $1 & ", " & $3; }
   | typed_constant
   { $$ := $1; }
   ;
-- address_constant : expression; -- equivalent to constant definition
array_constant : LPAREN_t typed_constant_list RPAREN_t
   { $$ := '(' & $2 & ')'; }
   ;
record_constant : LPAREN_t field_part_list RPAREN_t
   { $$ := '(' & $2 & ')'; }
   ;
field_part_list : field_part_list SEMICOLON_t field_part
   { $$ := $1 & ", " & $3; }
   | field_part_list COMMA_t field_part -- CWP only
   { $$ := $1 & ", " & $3; }
   | field_part
   { $$ := $1; }
   ;
field_part : identifier -- field identifier
   COLON_t typed_constant
   { $$ := $1 & " => " & $3; }
   | typed_constant -- CWP only
   { $$ := $1; }
   ;
-- procedural_constant : NIL_t | identifier; -- equivalent to constant definition

-- FPC 2.3 Resource strings
-- TP not defined
-- THP not defined
-- CWP not defined
resourcestring : identifier EQUAL_t constant SEMICOLON_t
   { $$ := $1 & " constant := " & $3 & ';' & NL; }
   ;

-- FPC 3 Types
-- TP 4
-- THP 3.0
-- CWP 7.1
type_declaration : identifier EQUAL_t type hint_directive SEMICOLON_t
   { $$ := "type " & $1 & " is " & $3 & ';' & NL & $4; }
   | GENERIC_t identifier LT_t identifier_list GT_t EQUAL_t generic_class hint_directive SEMICOLON_t
   { $$ := "-- P2Ada to do: generic package " & $2 & " with " & $4 & NL & "type " & $2 & " is " & $7 & ';' & NL & $8; }
   | GENERIC_t identifier LT_t identifier_list GE_t generic_class hint_directive SEMICOLON_t
   { $$ := "-- P2Ada to do: generic package " & $2 & " with " & $4 & NL & "type " & $2 & " is " & $6 & ';' & NL & $7; }
   ;
type : simple_type
   { $$ := $1; }
   | string_type
   { $$ := $1; }
   | structured_type
   { $$ := $1; }
   | pointer_type
   { $$ := $1; }
   | procedural_type
   { $$ := $1; }
--   | generic_type
--   { $$ := $1; }
   | specialized_type
   { $$ := $1; }
-- | identifier   -- type identifier is also declared in ordinal type
-- | variant_type -- Variant types are equivalent to identifier
   ;
type_identifier : identifier -- type identifier
   { $$ := $1; }
   | STRING_t
   { $$ := To_Unbounded_String("P2Ada_String"); }
   | FILE_t
   { $$ := To_Unbounded_String("P2Ada_no_type_file"); }
   | identifier -- unit identifier
   PERIOD_t identifier -- type identifier
   { $$ := $1 & '.' & $3; }
   ;

-- FPC 3.1 Base types
-- TP 4.1
-- THP 3.1
-- CWP 7.2
simple_type : ordinal_type
-- | real_type; -- equivalent to identifier already declared in type
   { $$ := $1; }
   ;

-- FPC 3.1.1 Ordinal types
-- TP 4.1.1
-- THP 3.1
-- CWP 7.3
ordinal_type : subrange_type
   { $$ := $1; }
   | enumerated_type
   { $$ := $1; }
   | ordinal_type_identifier
   { $$ := $1; }
   ;
subrange_type : constant DOUBLEDOT_t constant
   { $$ := $1 & " .. " & $3; }
   ;
enumerated_type : LPAREN_t enumerated_list RPAREN_t
   { $$ := '(' & $2 & ')'; }
   ;
enumerated_list : enumerated_list COMMA_t enumerated_part
   { $$ := $1 & ", " & $3; }
   | enumerated_part
   { $$ := $1; }
   ;
enumerated_part : identifier
   { $$ := $1; }
   | identifier ASSIGN_t expression -- from FPC manual
   { $$ := $1 & "; -- P2Ada to do: specify for clause with " & $3 & NL;}
   | identifier EQUAL_t constant -- from Delphi manual
   { $$ := $1 & "; -- P2Ada to do: specify for clause with " & $3 & NL;}
   ;
ordinal_type_identifier : identifier -- type identifier
   { $$ := $1; }
   | identifier -- unit identifier
   PERIOD_t identifier -- type identifier
   { $$ := $1 & '.' & $3; }
   ;

-- FPC 3.1.2 Real types
-- TP 4.1.2
-- THP 3.1.2
-- CWP 7.2
-- real_type : identifier; -- real type identifier already declared in type

-- FPC 3.2.2 Strings
-- TP 4.2
-- THP 3.3
-- CWP 7.4
string_type : STRING_t LBRACK_t constant RBRACK_t
   { $$ := "P2Ada_String" & '(' & $3 & ')'; }
   | STRING_t
   { $$ := To_Unbounded_String("P2Ada_String"); }
   ;

-- FPC 3.3 Structured Types
-- TP 4.3
-- THP 3.2
-- CWP 7.7
structured_type : array_type
   { $$ := $1; }
   | record_type
   { $$ := $1; }
   | object_type -- part of restricted type in Delphi manual
   { $$ := $1; }
   | class_type -- part of restricted type in Delphi manual
   { $$ := $1; }
   | class_reference_type -- part of type in Delphi manual
   { $$ := $1; }
   | interface_type -- part of restricted type in Delphi manual
   { $$ := $1; }
   | set_type
   { $$ := $1; }
   | file_type
   { $$ := $1; }
   ;

-- FPC 3.3.1 Arrays
-- TP 4.3.1
-- THP 3.2.1
-- CWP 7.7.1
array_type : packed_or_bitpacked ARRAY_t range_part OF_t type hint_directive
   { $$ := "array " & $3 & " of " & $5 & $1 & $6; }
   ;
packed_or_bitpacked : PACKED_t
   { $$ := "-- P2Ada: packed" & NL; }
   | BITPACKED_t
   { $$ := "-- P2Ada: bit packed" & NL; }
   |
   { $$ := Null_Unbounded_String; }
   ;
range_part : LBRACK_t ordinal_type_list RBRACK_t
   { $$ := '(' & $2 & ')'; }
   |
   { $$ := Null_Unbounded_String; }
   ;
ordinal_type_list : ordinal_type_list COMMA_t ordinal_type
   { $$ := $1 & ", " & $3; }
   | ordinal_type
   { $$ := $1; }
   ;

-- FPC 3.3.2 Record types
-- TP 4.3.2
-- THP 3.2.2
-- CWP 7.7.2
record_type : packed_or_bitpacked RECORD_t field_list END_t hint_directive
   { $$ := "record" & NL & $3 & "end record" & $1 & $5; }
   | packed_or_bitpacked RECORD_t END_t hint_directive
   { $$ := "null record" & $1 & $4; }
   ;
field_list : fixed_part
   { $$ := $1; }
   | fixed_and_variant_part
   { $$ := $1; }
   | variant_part
   { $$ := $1; }
   ;
fixed_part : fixed_part SEMICOLON_t field_declaration
   { $$ := $1 & $3; }
   | field_declaration
   { $$ := $1; }
   | fixed_part SEMICOLON_t -- extra allowed semi-colon by compilers
   { $$ := $1; }
   ;
field_declaration : identifier_list COLON_t type hint_directive -- from Delphi manual
   { $$ := $1 & ": " & $3 & ';' & NL & $4; }
   ;
fixed_and_variant_part : fixed_part SEMICOLON_t variant_part
   { $$ := $1 & $3; }
   ;
variant_part : CASE_t identifier COLON_t ordinal_type_identifier OF_t variant_list
   { $$ := "case " & $2 & " is " & NL & "-- P2Ada to do: add " & $2 & " of type " & $4 & " into type discriminant" & NL & $6 & "end case;" & NL;}
   | CASE_t ordinal_type_identifier OF_t variant_list
   { $$ := "case P2Ada_anonym is " & NL & "-- P2Ada to do: add anonym of type " & $2 & " into type discriminant" & NL & $4 & "end case;" & NL;}
   ;
variant_list : variant_list SEMICOLON_t variant
   { $$ := $1 & $3; }
   | variant
   { $$ := $1; }
   | variant_list SEMICOLON_t -- extra allowed semi-colon by compilers
   { $$ := $1; }
   ;
variant : constant_list COLON_t LPAREN_t field_list RPAREN_t
   { $$ := "when " & $1 & " => " & NL & $4; }
   ;
constant_list : constant_list COMMA_t constant
   { $$ := $1 & " | " & $3; }
   | constant
   { $$ := $1; }
   ;

-- FPC 3.3.3 Set types
-- TP 4.3.4
-- THP 3.2.3
-- CWP 7.2.3
set_type : packed SET_t OF_t ordinal_type hint_directive -- packed from TP manual
   { $$ := "array (P2Ada_Anonyme) of Boolean -- P2Ada to do: before declare anonyme ordinal type with " & $4 & NL & $1 & $5; }
   ;
packed : PACKED_t
   { $$ := "-- P2Ada: packed" & NL; }
   |
   { $$ := Null_Unbounded_String; }
   ;

-- 3.3.4 File types
-- TP 4.3.5
-- THP 3.2.4
-- CWP 7.2.4
file_type : packed FILE_t OF_t type hint_directive -- packed from TP manual
   { $$ := "P2Ada_File_Of_" & $4 & " -- P2Ada to do: declare package is new Ada.Direct_IO (file_of_xx)" & NL & $1 & $5; }
   | packed FILE_t
   { $$ := "P2Ada_No_Type_File" & $1; }
   ;

-- FPC 3.4 Pointers
-- TP 4.4
-- THP 3.4
-- CWP 7.5
pointer_type : UPARROW_t type_identifier hint_directive
   { $$ := "access " & $2 & $3; }
   ;

-- FPC 3.6 Procedural types
-- TP 4.5
-- THP not defined
-- CWP 7.6
procedural_type : procedural_part OF_t OBJECT_t SEMICOLON_t call_modifier_part
   { $$ := $1 & NL & "-- P2Ada: of object" & NL & $5; }
   | procedural_part  OF_t OBJECT_t -- from Delphi manual
   { $$ := $1 & NL & "-- P2Ada: of object" & NL; }
--   | procedural_part SEMICOLON_t call_modifier_part -- from FPC manual, semi-colon makes ayacc puzzled
--   { $$ := $1 & NL & $3; }
   | procedural_part
   { $$ := $1; }
   ;
procedural_part : function_header_type
   { $$ := $1; }
   | procedure_header_type
   { $$ := $1; }
   ;
function_header_type : FUNCTION_t formal_parameter_list COLON_t result_type
   { $$ := "access function " & $2 & " return " & $4; }
   ;
procedure_header_type : PROCEDURE_t formal_parameter_list
   { $$ := "access procedure " & $2; }
   ;
result_type : type_identifier
   { $$ := $1; }
   ;
-- result_type : simple_type -- from Delphi manual
--   | STRING_t;

-- FPC 4 Variables
-- FPC 4.2 Declaration
-- TP 5.1
-- THP 4.1
-- CWP 8.4
variable_declaration : identifier_list COLON_t type EQUAL_t expression var_mod_list SEMICOLON_t
   { $$ := $1 & ": " & $3 & " := " & $5 & ';' & NL & $6; }
   | identifier_list COLON_t type EQUAL_t expression SEMICOLON_t
   { $$ := $1 & ": " & $3 & " := " & $5 & ';' & NL; }
   | identifier_list COLON_t type var_mod_list SEMICOLON_t
   { $$ := $1 & ": " & $3 & ';' & NL & $4; }
   | identifier_list COLON_t type SEMICOLON_t
   { $$ := $1 & ": " & $3 & ';' & NL; }
   ;
var_mod_list : var_mod_list var_mod_part
   { $$ := $1 & $2; }
   | var_mod_part
   { $$ := $1; }
   ;
var_mod_part : SEMICOLON_t variable_modifiers -- from FPC manual
   | ABSOLUTE_t abs_val_part -- from TP manual
   { $$ := "-- P2Ada to do: absolute " & $2 & NL; }
   | hint_directive -- from Delphi manual
   { $$ := $1; }
   ;
variable_modifiers : -- from FPC manual
   EXPORT_t
   { $$ := "-- P2Ada to do: export " & NL; }
   | CVAR_t
   { $$ := "-- P2Ada to do: cvar " & NL; }
   | EXTERNAL_t -- from CWP manual
   { $$ := "-- P2Ada to do: external " & NL; }
--   | FAR_t -- from CWP manual but make ayacc puzzled with 'var near: byte; far:word;'
--   { $$ := "-- P2Ada to do: far " & NL; }
   | EXTERNAL_t constant -- string constant
   { $$ := "-- P2Ada to do: external " & $2 & NL; }
   | EXTERNAL_t NAME_t constant -- string constant
   { $$ := "-- P2Ada to do: external name " & $3 & NL; }
   | EXTERNAL_t constant NAME_t constant -- string constants
   { $$ := "-- P2Ada to do: external " & $2 & " name " & $4 & NL; }
   ;
abs_val_part : expression -- integer expression
   { $$ := $1; }
   | unsigned_integer COLON_t unsigned_integer
   { $$ := $1 & ", " & $2; }
   | identifier
   { $$ := $1; }
   ;

-- FPC 4.6 Properties
-- TP not defined
-- THP not defined
-- CWP not defined
-- Delphi not defined outside classes
property_declaration : identifier property_interface property_specifiers SEMICOLON_t
   { $$ := "-- P2Ada to do: property declaration " & $1 & $2 & $3 & NL; }
   | identifier property_specifiers SEMICOLON_t
   { $$ := "-- P2Ada to do: property declaration " & $1 & $2 & NL; }
   ;
property_interface : property_parameter_list COLON_t type_identifier
   { $$ := $1 & ": " & $3; }
   | COLON_t type_identifier
   { $$ := ": " & $2; }
   ;
property_parameter_list : LBRACK_t parameter_declaration_list RBRACK_t
   { $$ := '(' & $2 & ')'; }
   ;
property_specifiers : index_specifier read_specifier write_specifier default_specifier
   { $$ := $1 & $2 & $3 & $4; }
   ;
index_specifier : INDEX_t constant -- integer constant
   { $$ := "-- P2Ada to do: index " & $2 & NL; }
   | DISPID_t constant -- in Delphi manual only
   { $$ := "-- P2Ada to do: dispid " & $2 & NL; }
   |
   { $$ := Null_Unbounded_String; }
   ;
read_specifier : READ_t identifier -- field or function
   { $$ := "-- P2Ada to do: index " & $2 & NL; }
   |
   { $$ := Null_Unbounded_String; }
   ;
write_specifier : WRITE_t identifier -- field or procedure
   { $$ := "-- P2Ada to do: index " & $2 & NL; }
   |
   { $$ := Null_Unbounded_String; }
   ;
default_specifier : NODEFAULT_t
   { $$ := "-- P2Ada to do: no default" & NL; }
   | DEFAULT_t
   { $$ := "-- P2Ada to do: default" & NL; }
   | DEFAULT_t constant
   { $$ := "-- P2Ada to do: default " & $2 & NL; }
   |
   { $$ := Null_Unbounded_String; }
   ;

-- FPC 5 Objects
-- FPC 5.1 Declaration
-- TP 4.3.3
-- THP 3.2.5
-- CWP 7.7.5
object_type : packed OBJECT_t heritage component_list END_t
   { $$ := " new " & $3 & " with record" & NL & $1 & $4 & NL & "end record"; }
   | packed OBJECT_t heritage END_t
   { $$ := " new " & $3 & " with null record" & NL & $1; }
   | packed OBJECT_t component_list END_t
   { $$ := " tagged record" & NL & $1 & $3 & NL & "end record"; }
   | packed OBJECT_t END_t
   { $$ := " tagged null record" & $1; }
   ;
heritage : LPAREN_t type_identifier RPAREN_t -- object type identifier
   { $$ := $2; }
   ;
component_list : component_list component_part
   { $$ := $1 & $2; }
   | component_part
   { $$ := $1; }
   ;
component_part : object_visibility_specifier
   { $$ := $1; }
   | field_definition
   { $$ := $1; }
   | method_definition
   { $$ := $1; }
   ;
field_definition : identifier_list COLON_t type SEMICOLON_t
   { $$ := $1 & ": " & $3 & ';'; }
   | identifier_list COLON_t type SEMICOLON_t STATIC_t SEMICOLON_t -- from FPC manual
   { $$ := $1 & ": " & $3 & "; -- P2Ada: static" & NL; }
   ;
object_visibility_specifier : PRIVATE_t -- not defined for THP and CWP
   { $$ := "-- P2Ada: private" & NL; }
   | PROTECTED_t -- not defined for TP
   { $$ := "-- P2Ada: protected" & NL; }
   | PUBLIC_t
   { $$ := "-- P2Ada: public" & NL; }
   ;

-- FPC 5.4 Constructors and destructors
-- TP 9.3
-- THP not defined
-- CWP not defines
constructor_declaration : constructor_header subroutine_block SEMICOLON_t
   { $$ := $1 & NL & $2 & ';' & NL; }
   ;
destructor_declaration : destructor_header subroutine_block SEMICOLON_t
   { $$ := $1 & NL & $2 & ';' & NL; }
   ;
constructor_header : CONSTRUCTOR_t identifier formal_parameter_list SEMICOLON_t
   { $$ := "procedure " & $2 & $3 & "; -- P2Ada: constructor" & NL; }
   | CONSTRUCTOR_t qualified_method_identifier formal_parameter_list SEMICOLON_t
   { $$ := "procedure " & $2 & $3 & "; -- P2Ada: constructor" & NL; }
   ;
destructor_header : DESTRUCTOR_t identifier formal_parameter_list  SEMICOLON_t
   { $$ := "procedure " & $2 & $3 & "; -- P2Ada: destructor" & NL; }
   | DESTRUCTOR_t qualified_method_identifier formal_parameter_list SEMICOLON_t
   { $$ := "procedure " & $2 & $3 & "; -- P2Ada: destructor" & NL; }
   ;
qualified_method_identifier : identifier -- object type identifier
   PERIOD_t identifier -- method identifier
   { $$ := $1 & '.' & $3; }
   ;

-- FPC 5.5 Methods
-- FPC 5.5.1 Declaration
-- TP
-- THP
-- CWP
method_definition : method_header method_directives
   { $$ := $1 & $2; }
   | method_header
   { $$ := $1; }
   ;
method_header : function_header
   { $$ := $1; }
   | procedure_header
   { $$ := $1; }
   | constructor_header
   { $$ := $1; }
   | destructor_header -- not defined for THP and CWP
   { $$ := $1; }
   ;
method_directives : meth_virt call_modifiers
   { $$ := $1 & $2; }
   | meth_virt
   { $$ := $1; }
   ;
meth_virt : VIRTUAL_t SEMICOLON_t -- not defined for THP and CWP
   { $$ := "-- P2Ada to do: virtual" & NL; }
   |  VIRTUAL_t constant SEMICOLON_t -- integer constant from TP manual
   { $$ := "-- P2Ada to do: virtual" & $2 & NL; }
   |  VIRTUAL_t SEMICOLON_t ABSTRACT_t SEMICOLON_t -- from FPC manual
   { $$ := "-- P2Ada to do: virtual abstract" & NL; }
   | OVERRIDE_t SEMICOLON_t -- from THP and CWP manual
   { $$ := "-- P2Ada to do: override" & NL; }
   ;

-- FPC 6 Classes
-- TP not defined
-- THP not defined
-- CWP not defines

-- FPC 6.1 Class definitions
-- TP not defined
-- THP not defined
-- CWP not defines
class_type : packed CLASS_t class_heritage class_component_list END_t
   { $$ := " new " & $3 & " with record" & NL & $1 & $4 & NL & "end record"; }
   | packed CLASS_t class_heritage END_t
   { $$ := " new " & $3 & " with null record" & $1; }
   | packed CLASS_t class_component_list END_t
   { $$ := " tagged record" & NL & $1 & $3 & NL & "end record"; }
   | packed CLASS_t END_t
   { $$ := " tagged null record" & $1; }
   ;
class_heritage : LPAREN_t class_identifier_list RPAREN_t -- class type and implemented interfaces identifiers
   { $$ := $2; }
   ;
class_identifier_list : class_identifier_list COMMA_t type_identifier
   { $$ := $1 & " and " & $3; }
   | type_identifier
   { $$ := $1; }
   ;
class_component_list : class_component_list class_component_part
   { $$ := $1 & $2; }
   | class_component_part
   { $$ := $1; }
   ;
class_component_part : class_visibility_specifier
   { $$ := $1; }
   | field_definition
   { $$ := $1; }
   | class_method_definition
   { $$ := $1; }
   | class_property_definition
   { $$ := $1; }
   ;
class_visibility_specifier : PRIVATE_t
   { $$ := "-- P2Ada: private" & NL; }
   | PROTECTED_t
   { $$ := "-- P2Ada: protected" & NL; }
   | PUBLIC_t
   { $$ := "-- P2Ada: public" & NL; }
   | PUBLISHED_t
   { $$ := "-- P2Ada: published" & NL; }
   | AUTOMATED_t -- from Delphi manual
   { $$ := "-- P2Ada: automated" & NL; }
   ;
class_reference_type : packed CLASS_t OF_t type_identifier -- class type identifier, packed is added cause of ayacc puzzled with class type
   { $$ := $1 & "'Class"; }
   ;

-- FPC 6.3 Class Methods
-- FPC 6.3.1 Declaration
-- TP not defined
-- THP not defined
-- CWP not defines
class_method_definition : class_method_header class_method_directives call_modifiers
   { $$ := $1 & $2 & $3; }
   ;
class_method_header : function_header
   { $$ := $1; }
   | procedure_header
   { $$ := $1; }
   | CLASS_t function_header -- not defined for Delphi
   { $$ := $1 & "-- P2Ada: class header" & NL; }
   | CLASS_t procedure_header -- not defined for Delphi
   { $$ := $1 & "-- P2Ada: class header" & NL; }
   | constructor_header
   { $$ := $1; }
   | destructor_header
   { $$ := $1; }
   ;
class_method_directives : VIRTUAL_t SEMICOLON_t -- not defined for THP and CWP
   { $$ := "-- P2Ada to do: virtual" & NL; }
   | VIRTUAL_t constant SEMICOLON_t -- integer constant from TP manual
   { $$ := "-- P2Ada to do: virtual with " & $2 & NL; }
   | VIRTUAL_t SEMICOLON_t ABSTRACT_t SEMICOLON_t -- from FPC manual
   { $$ := "-- P2Ada to do: virtual abstract" & NL; }
   | OVERRIDE_t SEMICOLON_t -- from THP and CWP manual
   { $$ := "-- P2Ada to do: override" & NL; }
   | REINTRODUCE_t SEMICOLON_t -- from FPC manual
   { $$ := "-- P2Ada: reintroduce" & NL; }
   | MESSAGE_t constant SEMICOLON_t -- integer or string constant from FPC manual
   { $$ := "-- P2Ada: message with " & $2 & NL; }
   | DISPID_t constant SEMICOLON_t -- from Delphi manual
   { $$ := "-- P2Ada: dispid with " & $2 & NL; }
   |
   { $$ := Null_Unbounded_String; }
   ;

-- FPC 6.4 Class Properties
-- TP not defined
-- THP not defined
-- CWP not defines
class_property_definition : PROPERTY_t identifier property_interface class_property_specifiers hint_directive SEMICOLON_t
   { $$ := "-- P2Ada to do: property declaration " & $2 & $3 & $4 & $5 & NL; }
   ;
class_property_specifiers : index_specifier read_specifier write_specifier stored_specifier default_specifier implements_specifier
 -- is the order important, guess yes... Delphi manual order has taken
   { $$ := $1 & $2 & $3 & $4 & $5 & $6; }
   ;
implements_specifier : IMPLEMENTS_t type_identifier
   { $$ := "-- P2Ada to do: implements " & $2 & NL; }
   |
   { $$ := Null_Unbounded_String; }
   ;
stored_specifier : STORED_t constant
   { $$ := "-- P2Ada to do: stored " & $2 & NL; }
-- | STORED_t identifier; -- identifier already defined in constant
   |
   { $$ := Null_Unbounded_String; }
   ;

-- FPC 7 Interfaces
-- FPC 7.1 Definition
-- TP not defined
-- THP not defined
-- CWP not defines
interface_type : INTERFACE_t int_heritage guid int_component_list END_t
   { $$ := " new " & $2 & " with record" & NL & $4 & NL & "end record" & $3; }
   | INTERFACE_t int_heritage guid END_t
   { $$ := " new " & $2 & $3; }
   | INTERFACE_t guid int_component_list END_t
   { $$ := " interface " & " with record" & NL & $3 & NL & "end record" & $2; }
   | INTERFACE_t guid END_t
   { $$ := " interface " & $2; }
    -- from Delphi manual
   | DISPINTERFACE_t int_heritage guid int_component_list END_t
   { $$ := " new " & $2 & " with record" & NL & $4 & NL & "end record" & $3; }
   | DISPINTERFACE_t int_heritage guid END_t
   { $$ := " new " & $2 & $3; }
   | DISPINTERFACE_t guid int_component_list END_t
   { $$ := " interface " & " with record" & NL & $3 & NL & "end record" & $2; }
   | DISPINTERFACE_t guid END_t
   { $$ := " interface " & $2; }
   ;
int_heritage : LPAREN_t type_identifier_list RPAREN_t -- interface type identifiers
   { $$ := '(' & $2 & ')'; }
   ;
guid : LBRACK_t constant RBRACK_t -- constant string
   { $$ := "-- P2Ada: GUID " & $2 & NL; }
   |
   { $$ := Null_Unbounded_String; }
   ;
int_component_list : int_component_list int_component_part
   { $$ := $1 & $2; }
   | int_component_part
   { $$ := $1; }
   ;
int_component_part : class_visibility_specifier
   { $$ := $1; }
   | class_method_definition
   { $$ := $1; }
   | class_property_definition
   { $$ := $1; }
   ;

-- 8 Generics
-- 8.2 Generic class definition
-- TP not defined
-- THP not defined
-- CWP not defines
--generic_type : GENERIC_t LT_t identifier_list -- template list
-- GT_t EQUAL_t generic_class SEMICOLON_t
--   { $$ := $6 & NL & "-- P2Ada to do: generic package " & " with " & $3; }
--   ;
generic_class : packed CLASS_t int_heritage gen_class_block_list END_t
   { $$ := " new " & $3 & " with record" & NL & $4 & NL & "end record" & NL & $1; }
   | packed CLASS_t int_heritage END_t
   { $$ := " new " & $3 & NL & $1; }
   | packed CLASS_t gen_class_block_list END_t
   { $$ := " tagged record" & NL & $3 & NL & "end record" & NL & $1; }
   | packed CLASS_t END_t
   { $$ := " tagged null record" & NL & $1; }
   ;
gen_class_block_list : gen_class_block_list gen_class_block
   { $$ := $1 & $2; }
   | gen_class_block
   { $$ := $1; }
   ;
gen_class_block : local_type_block
   { $$ := $1; }
   | local_variable_block
   { $$ := $1; }
   | class_component_list
   { $$ := $1; }
   ;
local_type_block : TYPE_t class_visibility_specifier type_list
   { $$ := $2 & $3; }
   | TYPE_t type_list
   { $$ := $2; }
   ;
local_variable_block : VAR_t class_visibility_specifier var_list
   { $$ := $2 & $3; }
   | VAR_t var_list
   { $$ := $2; }
   ;

-- 8.3 Generic class specialization
-- TP not defined
-- THP not defined
-- CWP not defines
specialized_type : SPECIALIZE_t identifier LT_t type_identifier_list GT_t
   { $$ := " new " & $2 & '(' & $4 & ')' ; }
   ;
type_identifier_list : type_identifier_list COMMA_t type_identifier
   { $$ := $1 & ", " & $3; }
   | type_identifier
   { $$ := $1; }
   ;

-- FPC 9 Expressions
-- FPC 9.1 Expression syntax
-- TP 6.1
-- THP 5.0
-- CWP 9.3
expression : simple_expression
   { $$ := $1; }
   | simple_expression rel_op simple_expression
   { $$ := $1 & $2 & $3; }
   ;
rel_op : LT_t
   { $$ := To_Unbounded_String(" < "); }
   | LE_t
   { $$ := To_Unbounded_String(" <= "); }
   | GT_t
   { $$ := To_Unbounded_String(" > "); }
   | GE_t
   { $$ := To_Unbounded_String(" >= "); }
   | EQUAL_t
   { $$ := To_Unbounded_String(" = "); }
   | NE_t
   { $$ := To_Unbounded_String(" /= "); }
   | IN_t
   { $$ := To_Unbounded_String(" and "); }
   | IS_t
   { $$ := "-- P2Ada to do: is " & NL; }
   ;
simple_expression : simple_expression add_op term
   { $$ := $1 & $2 & $3; }
   | term
   { $$ := $1; }
   ;
add_op : PLUS_t
   { $$ := To_Unbounded_String(" + "); }
   | MINUS_t
   { $$ := To_Unbounded_String(" - "); }
   | OR_t
   { $$ := To_Unbounded_String(" or "); }
   | XOR_t
   { $$ := To_Unbounded_String(" xor "); }
   | BAR_t -- from CWP manual
   { $$ := To_Unbounded_String(" or else "); }
   ;
term : term mul_op factor
   { $$ := $1 & $2 & $3; }
   | term SHL_t factor -- put at this level to get both operands
   { $$ := "Interfaces.Shift_Left(" & $1 & ", " & $3 & ')'; }
   | term SHR_t factor -- put at this level to get both operands
   { $$ := "Interfaces.Shift_Right(" & $1 & ", " & $3 & ')'; }
   | factor
   { $$ := $1; }
   ;
mul_op : TIMES_t
   { $$ := To_Unbounded_String(" * "); }
   | DIVIDE_t
   { $$ := To_Unbounded_String(" / "); }
   | DIV_t
   { $$ := To_Unbounded_String(" / "); }
   | MOD_t -- TP, THP: i mod j = i - (i div j) * j
   { $$ := To_Unbounded_String(" rem "); }
   | AND_t
   { $$ := To_Unbounded_String(" and "); }
-- | SHL_t -- put at upper level to get both operands
-- | SHR_t -- put at upper level to get both operands
   | AS_t -- declared in relational operators in Delphi Manual
   { $$ := " -- P2Ada to do: as " & NL; }
   | AMPERSAND_t -- from CWP manual
   { $$ := To_Unbounded_String(" and then "); }
   | DOUBLESTAR_t
   { $$ := To_Unbounded_String(" ** "); }
   ;
factor : LPAREN_t expression RPAREN_t
   { $$ := '(' & $2 & ')'; }
   | variable_reference
   { $$ := $1; }
-- | function_call -- included in variable reference
   | unsigned_constant
   { $$ := $1; }
   | NOT_t factor
   { $$ := " not " & $2; }
   | sign factor
   { $$ := $1 & $2; }
   | set_constructor
   { $$ := $1; }
   | address_factor
   { $$ := $1; }
-- | value_typecast -- already defined by function call
   ;
unsigned_constant : unsigned_number
   { $$ := $1; }
   | character_string
   { $$ := $1; }
--   | identifier -- constant identifier already defined by variable reference
   | NIL_t
   { $$ := To_Unbounded_String(" null "); }
   ;
variable_reference : function_call
   { $$ := $1; }
   | LPAREN_t expression RPAREN_t qualifier_list -- parents help ayacc
   { $$ := $2 & $4; }
   ;
qualifier_list : qualifier_list qualifier
   { $$ := $1 & $2; }
   | qualifier
   { $$ := $1; }
   ;
qualifier : indice
   { $$ := $1; }
   | field_descriptor
   { $$ := $1; }
   | UPARROW_t
   { $$ := To_Unbounded_String(".all "); }
   ;
indice : LBRACK_t index_list RBRACK_t
   { $$ := '(' & $2 & ')'; }
   ;
index_list : index_list COMMA_t expression
   { $$ := $1 & ", " & $3; }
   | index_list COLON_t expression -- catch specific syntax for MemX pseudo tables
   { $$ := $1 & ", " & $3; }
   | expression
   { $$ := $1; }
   ;
field_descriptor : PERIOD_t identifier -- field identifier
   { $$ := '.' & $2; }
   ;

-- FPC 9.2 Function calls
-- TP 6.3
-- THP 5.2
-- CWP 9.1.7
function_call : function_designator actual_parameter_list
   { $$ := $1 & $2; }
   | function_designator
   { $$ := $1; }
   | function_designator actual_parameter_list qualifier_list
   { $$ := $1 & $2 & $3; }
   ;
function_designator : designator
   { $$ := $1; }
   | INHERITED_t identifier -- method identifier
   { $$ := "-- P2Ada to do: get parent's type (" & $2 & ')' & NL; }
   | SELF_t PERIOD_t identifier -- from FPC
   { $$ := "-- P2Ada to do: get object's name." & $3 & NL; }
   ;
designator : identifier
   { $$ := $1; }
   | identifier qualifier_list
   { $$ := $1 & $2; }
   ;
actual_parameter_list : LPAREN_t expression_list RPAREN_t
   { $$ := '(' & $2 & ')'; }
   ;
expression_list : expression_list COMMA_t expression
   { $$ := $1 & ", " & $3; }
   | expression
   { $$ := $1; }
   ;

-- FPC 9.3 Set constructors
-- TP 6.4
-- THP 5.3
-- CWP 9.2
set_constructor : LBRACK_t set_group_list RBRACK_t
   { $$ := '(' & $2 & ", others => False)"; }
   | LBRACK_t RBRACK_t
   { $$ := To_Unbounded_String("(others => False)"); }
   ;
set_group_list : set_group_list COMMA_t set_group
   { $$ := $1 & ", " & $3; }
   | set_group
   { $$ := $1; }
   ;
set_group : expression
   { $$ := $1 & " => True"; }
   | expression DOUBLEDOT_t expression
   { $$ := $1 & " .. " & $3 & " => True"; }
   ;

-- FPC 9.4 Value typecasts
-- TP 6.5
-- THP 5.4
-- CWP 7.9
-- value_typecast : type_identifier LPAREN_t expression RPAREN_t; -- already defined by function call

-- FPC 9.7 The @ operator
-- TP 6.2.8
-- THP 5.1.6
-- CWP 9.1.6
address_factor : AT_t designator
   { $$ := $2 & "'access "; }
   | AT_t AT_t designator -- procedure or function
   { $$ := $2 & "'access "; }
   ;

-- FPC 10 Statements
-- TP 7
-- THP 6
-- CWP 10.1
statement : label COLON_t statement_part
   { $$ := "<<" & $1 & ">>" & NL & $3; }
   | statement_part
   { $$ := $1; }
   ;
statement_part : simple_statement
   { $$ := $1; }
   | structured_statement
   { $$ := $1; }
   | asm_statement -- part of structured stmt in Delphi
   { $$ := $1; }
   | inline_directive -- from TP manual
   { $$ := $1; }
   |
   { $$ := "Null;" & NL; }
   ;

-- FPC 10.1 Simple statements
-- TP 7.1
-- THP 6.1
-- CWP defined at statement level
simple_statement : assignment_statement
   { $$ := $1; }
   | procedure_statement
   { $$ := $1; }
   | goto_statement
   { $$ := $1; }
   | raise_statement -- from FPC, part of structured stmt in Delphi
   { $$ := $1; }
   ;

-- FPC 10.1.1 Assignments
-- TP 7.1.1
-- THP 6.1.1
-- CWP 10.2
assignment_statement : variable_reference ASSIGN_t expression
   { $$ := Add_Result_If_Function ($1) & " := " & $3 & ';' & NL; }
   | variable_reference PLUSASSIGN_t expression
   { $$ := Add_Result_If_Function ($1) & " := " & $1 & " + (" & $3 & ')' & ';' & NL; }
   | variable_reference MINUSASSIGN_t expression
   { $$ := Add_Result_If_Function ($1) & " := " & $1 & " - (" & $3 & ')' & ';' & NL; }
   | variable_reference TIMESASSIGN_t expression
   { $$ := Add_Result_If_Function ($1) & " := " & $1 & " * (" & $3 & ')' & ';' & NL; }
   | variable_reference DIVIDEASSIGN_t expression
   { $$ := Add_Result_If_Function ($1) & " := " & $1 & " / (" & $3 & ')' & ';' & NL; }
   ;

-- FPC 10.1.2 Procedure statements
-- TP 7.1.2
-- THP 6.1.2
-- CWP 10.4
procedure_statement : variable_reference
   { $$ := $1 & ';' & NL; }
   | WRITE_t write_params
   { $$ := "Write; -- P2Ada to do: format parameters" & $2 & NL; }
   | WRITELN_t write_params
   { $$ := "WriteLn; -- P2Ada to do: format parameters" & $2 & NL; }
   | STR_t str_params
   { $$ := "Str; -- P2Ada to do: format parameters" & $2 & NL; }
   | CYCLE_t -- from CWP manual 10.7.2
   { $$ := "-- P2Ada to do: cycle" & NL; }
   | LEAVE_t -- from CWP manual 10.7.3
   { $$ := "-- P2Ada to do: leave" & NL; }
   ;
write_params : LPAREN_t write_actual_parameter_list RPAREN_t
   { $$ := '(' & $2 & ')'; }
   |
   { $$ := Null_Unbounded_String; }
   ;
write_actual_parameter_list : write_actual_parameter_list COMMA_t write_actual_parameter
   { $$ := $1 & ", " & $3; }
   | write_actual_parameter
   { $$ := $1; }
   ;
write_actual_parameter : expression
   { $$ := $1; }
   | expression COLON_t another_colon
   { $$ := $1 & ", " & $3; }
   ;
another_colon : expression
   { $$ := $1; }
   | expression COLON_t expression
   { $$ := $1 & ", " & $3; }
   ;
str_params : LPAREN_t write_actual_parameter COMMA_t variable_reference RPAREN_t
   { $$ := '(' & $2 & ", " & $4 & ')'; }
   ;

-- FPC 10.1.3 Goto statements
-- TP 7.1.3
-- THP 6.1.3
-- CWP not defined
goto_statement : GOTO_t label
   { $$ := "goto " & $2 & ';' & NL; }
   ;

-- FPC 10.2 Structured statements
-- TP 7.2
-- THP 6.2
-- CWP defined at statement level
structured_statement : compound_statement
   { $$ := $1; }
   | conditional_statement
   { $$ := $1; }
   | repetitive_statment
   { $$ := $1; }
   | with_statement
   { $$ := $1; }
   | exception_statement
   { $$ := $1; }
   ;
conditional_statement : case_statement
   { $$ := $1; }
   | if_statement
   { $$ := $1; }
   ;
repetitive_statment : for_statement
   { $$ := $1; }
   | repeat_statement
   { $$ := $1; }
   | while_statement
   { $$ := $1; }
   ;

-- FPC 10.2.1 Compound statements
-- TP 7.2.1
-- THP 6.2.1
-- CWP 10.3
compound_statement : BEGIN_t statement_list END_t
   { $$ := "begin" & NL & $2 & "end;" & NL; }
   | BEGIN_t END_t
   { $$ := "begin" & NL & "null;" & NL & "end;" & NL; }
   ;
block_statement : BEGIN_t statement_list END_t
   { $$ := "begin" & NL & $2 & Return_If_function & "end;" & NL; }
   | BEGIN_t END_t
   { $$ := "begin" & NL & Null_Or_Return_If_function & "end;" & NL; }
   ;
statement_list : statement_list SEMICOLON_t statement
   { $$ := $1 & $3; }
   | statement
   { $$ := $1; }
   ;

-- FPC 10.2.2 The Case statement
-- TP 7.3.2
-- THP 6.2.2.2
-- CWP 10.6.2
case_statement : CASE_t expression OF_t case_list else_part semicolon END_t
   { $$ := "case " & $2 &  " is" & NL & $4 & $5 & "end case;" & NL; }
   ;
case_list : case_list SEMICOLON_t case
   { $$ := $1 & $3; }
   | case
   { $$ := $1; }
   | case_list SEMICOLON_t
   { $$ := $1; }
   ;
semicolon : SEMICOLON_t
   { $$ := Null_Unbounded_String; }
   |
   { $$ := Null_Unbounded_String; }
   ;
case : subrange_list COLON_t statement
   { $$ := "when " & $1 & " => " & NL & $3; }
   ;
subrange_list : subrange_list COMMA_t subrange_part
   { $$ := $1 & " | " & $3; }
   | subrange_part
   { $$ := $1; }
   ;
subrange_part : constant
   { $$ := $1; }
   | subrange_type
   { $$ := $1; }
   ;
else_part : ELSE_t statement_list
   { $$ := "when others => " & NL & $2; }
   | OTHERWISE_t statement_list -- from THP and CWP manuals
   { $$ := "when others => " & NL & $2; }
   |
   { $$ := Null_Unbounded_String; }
   ;

-- FPC 10.2.3 The If..then..else statement
-- TP 7.3.1
-- THP 6.2.2.1
-- CWP 10.6.1
if_statement : IF_t expression THEN_t statement
   { $$ := "if " & $2 & " then" & NL & $4 & "end if;" & NL; }
   | IF_t expression THEN_t statement ELSE_t statement
   { $$ := "if " & $2 & " then" & NL & $4 & "else" & NL & $6 & "end if;" & NL; }
   ;

-- FPC 10.2.4 The For..to/downto..do statement
-- TP 7.4.3
-- THP 6.2.3.3
-- CWP 10.5.1
for_statement : FOR_t control_variable ASSIGN_t initial_value to_or_downto final_value DO_t statement
   { $$ := "for " & $2 & " in " & $5 & $4 & " .. " & $6 & " loop" & NL & $8 & NL & "end loop;" & NL; }
;
control_variable : identifier -- variable identifier
   { $$ := $1; }
   ;
initial_value : expression
   { $$ := $1; }
   ;
to_or_downto : TO_t
   { $$ := Null_Unbounded_String; } -- direct way e.g. nothing to do
   | DOWNTO_t
   { $$ := To_Unbounded_String("reverse "); }
   ;
final_value : expression
   { $$ := $1; }
   ;

-- FPC 10.2.5 The Repeat..until statement
-- TP 7.4.1
-- THP 6.2.3.1
-- CWP 10.5.3
repeat_statement : REPEAT_t statement_list UNTIL_t expression
   { $$ := "loop " & NL & $2 & "exit when " & $4 & ';' & NL & "end loop;" & NL; }
   | REPEAT_t statement_list SEMICOLON_t UNTIL_t expression -- this last case is permit by usual Pascal compilers
   { $$ := "loop " & NL & $2 & "exit when " & $5 & ';' & NL & "end loop;" & NL; }
   ;

-- FPC 10.2.6 The While..do statement
-- TP 7.4.2
-- THP 6.2.3.2
-- CWP 10.5.2
while_statement : WHILE_t expression DO_t statement
   { $$ := "while " & $2 & " loop" & NL & $4 & "end loop;" & NL; }
   ;

-- FPC 10.2.7 The With statement
-- TP 7.5
-- THP 6.2.4
-- CWP 10.7.4
with_statement : WITH_t var_ref_list DO_t statement
   { $$ := "declare " & $2 & NL & "begin" & NL & $4 & NL & "end;" & NL; }
   ;
var_ref_list : var_ref_list COMMA_t variable_reference
   { $$ := "-- P2Ada to do: get var's type and declare local rename of var " & $3 & NL; }
   | variable_reference
   { $$ := "-- P2Ada to do: get var's type and declare local rename of var " & $1 & NL; }
   ;

-- FPC 10.2.8 Exception Statements
exception_statement : try_except_statement
   { $$ := $1; }
   | try_finally_statement
   { $$ := $1; }
   ;

-- FPC 10.3 Assembler statements
-- TP 22
-- THP not defined
-- CWP 16
asm_statement : BEGIN_t -- ASM_t assembler_code -- free text convert to comment by bp2p!
   END_t register_list
   { $$ := "begin" & NL & Null_Or_Return_If_function & "end; -- P2Ada to do: assembler code with registers " & $3 & NL; }
   ;
register_list : LBRACK_t string_list RBRACK_t
   { $$ := $2; }
   |
   { $$ := Null_Unbounded_String; }
   ;
string_list : string_list COMMA_t constant -- string constant
   { $$ := $1 & ", " & $3; }
   | constant
   { $$ := $1; }
   ;

-- Inline directive
-- TP 23
-- THP 7.1.3
-- CWP 16
inline_directive : INLINE_t LPAREN_t inline_list RPAREN_t
   { $$ := "begin null; end; -- P2Ada to do: inline with code " & $3 & NL; }
   | INLINE_t constant_list
   { $$ := "begin null; end; -- P2Ada to do: assembler with code " & $2 & NL; }
   ;
inline_list : inline_list DIVIDE_t inline_part
   { $$ := $1 & ", " & $3; }
   | inline_part
   { $$ := $1; }
   ;
inline_part : LT_t constant
   { $$ := '<' & $2; }
   | GT_t constant
   { $$ := '>' & $1; }
   | identifier -- variable identifier
   { $$ := $1; }
   | identifier dep_list -- variable identifier
   { $$ := $1 & $2; }
   ;
dep_list : dep_list sign constant
   { $$ := $1 & $2 & $3; }
   | sign constant
   { $$ := $1 & $2; }
   ;

-- FPC 11 Using functions and procedures
-- TP 9
-- THP 7.0
-- CWP 11

-- FPC 11.1 Procedure declaration
-- TP 9.1
-- THP 7.1
-- CWP 11.1
procedure_declaration : procedure_header modifiers hint_directive subroutine_block SEMICOLON_t
   { $$ := Replace_SC_by_IS($1) & $2 & $3 & $4;
     Subprog_List.Delete_Last; }    -- P2Ada to do: process modifiers
   ;
procedure_header : PROCEDURE_t proc_or_func_identifier formal_parameter_list SEMICOLON_t
   { $$ := "procedure " & $2 & $3 & ';' & NL;
     Subprog_List.Append($2); }
   | CLASS_t PROCEDURE_t proc_or_func_identifier formal_parameter_list SEMICOLON_t -- from FPC manual
   { $$ := "procedure " & $3 & $4 & ';' & NL;
     Subprog_List.Append($3); }
   ;
proc_or_func_identifier : identifier
   { $$ := $1; }
   | qualified_method_identifier
   { $$ := $1; }
   ;
subroutine_block : block
   { $$ := $1;
     Set_Block_Flag; }
   | external_directive
   { $$ := "pragma Import (C, " & Get_Subprog_Name(Subprog_List.Last_Element) & ");" & NL & $1; }
   | asm_block
   { $$ := $1;
     Set_Block_Flag; }
   | FORWARD_t
   { $$ := "-- P2Ada: forward" & NL; } -- an Ada declaration is basicly a "forward" declaration so nothing to do
   | inline_directive
   { $$ := "pragma Import (C, " & Get_Subprog_Name(Subprog_List.Last_Element) & ");" & NL & $1; }
   ;

-- FPC 11.2 Function declaration
-- TP 9.2
-- THP 7.2
-- CWP 11.2
function_declaration : function_header modifiers hint_directive subroutine_block SEMICOLON_t
   { $$ := Replace_SC_by_IS($1) & $2 & $3 & Result_Declaration & $4;
     Subprog_List.Delete_Last; }    -- P2Ada to do: process modifiers
   ;
function_header : FUNCTION_t proc_or_func_identifier formal_parameter_list COLON_t result_type SEMICOLON_t
   { $$ := "function " & $2 & $3 & " return " & $5 & ';' & NL;
     Subprog_List.Append($2 & ':' & $5); }
   | CLASS_t FUNCTION_t proc_or_func_identifier formal_parameter_list COLON_t result_type SEMICOLON_t -- from FPC manual
   { $$ := "function " & $3 & $4 & " return " & $6 & ';' & NL;
     Subprog_List.Append($3 & ':' & $6); }
   ;

-- FPC 11.4 Parameter lists
-- TP 9.4
-- THP 7.3
-- CWP 11.4
formal_parameter_list : LPAREN_t parameter_declaration_list RPAREN_t
   { $$ := '(' & $2 & ')'; }
   |
   { $$ := Null_Unbounded_String; }
   ;
parameter_declaration_list : parameter_declaration_list SEMICOLON_t parameter_declaration
   { $$ := $1 & "; " & $3; }
   | parameter_declaration
   { $$ := $1; }
   ;
parameter_declaration : value_parameter
   { $$ := $1; }
   | variable_parameter
   { $$ := $1; }
   | out_parameter
   { $$ := $1; }
   | constant_parameter
   { $$ := $1; }
   | function_header
   { $$ := $1; }
   | procedure_header -- CWP 11.4.4, THP 7.3.3
   { $$ := $1; }
   ;

-- FPC 11.4.1 Value parameters
-- TP 9.4.1
-- THP 7.3.1
-- CWP 11.4.1
value_parameter : identifier_list COLON_t type_identifier
   { $$ := $1 & ": " & $3; }
   | identifier_list COLON_t ARRAY_t OF_t type_identifier -- from Delphi manual
   { $$ := $1 & ": -- P2Ada to do : anonymous array type " & $3 & NL; }
   | identifier_list COLON_t type_identifier EQUAL_t constant -- default parameter value from Delphi manual
   | identifier_list COLON_t UNIV_t type_identifier -- from THP, CWP manuals
   { $$ := $1 & ": " & $4 & "-- P2Ada: THP and CWP value univ parameter" & NL; }
   ;

-- FPC 11.4.2 Variable parameters
-- TP 9.4.3
-- THP 7.3.2
-- CWP 11.4.2
variable_parameter : VAR_t identifier_list COLON_t type_identifier
   { $$ := $2 & ": in out " & $4; }
   | VAR_t identifier_list
   { $$ := $2 & ": P2Ada_No_Type -- P2Ada to do : no type var para" & NL; }
   | VAR_t identifier_list COLON_t ARRAY_t OF_t type_identifier -- from Delphi manual
   { $$ := $1 & ": -- P2Ada to do : anonymous array type " & $3 & NL; }
   | VAR_t identifier_list COLON_t UNIV_t type_identifier -- from THP, CWP manuals
   { $$ := $2 & ": " & $5 & "-- P2Ada: THP and CWP var univ parameter" & NL; }
   ;

-- FPC 11.4.3 Out parameters
-- TP not defined
-- THP not defined
-- CWP not defined
out_parameter : OUT_t identifier_list COLON_t type_identifier
   { $$ := $2 & ": out " & $4; }
   | OUT_t identifier_list
   { $$ := $2 & ": -- P2Ada to do : no type out para" & NL; }
   | OUT_t identifier_list COLON_t ARRAY_t OF_t type_identifier -- Delphi
   { $$ := $1 & ": -- P2Ada to do : anonymous array type " & $3 & NL; }
   ;

-- FPC 11.4.4 Constant parameters
-- TP 9.4.2
-- THP not defined
-- CWP 11.4.3
constant_parameter : CONST_t identifier_list COLON_t type_identifier
   { $$ := $2 & ": " & $4; }
   | CONST_t identifier_list
   { $$ := $2 & ": -- P2Ada to do : no type const para" & NL; }
   | CONST_t identifier_list COLON_t ARRAY_t OF_t type_identifier -- from Delphi manual
   { $$ := $1 & ": -- P2Ada to do : anonymous array type " & $3 & NL; }
   | CONST_t identifier_list COLON_t type_identifier EQUAL_t constant -- default parameter value from Delphi manual
   | CONST_t identifier_list COLON_t UNIV_t type_identifier -- from CWP manual
   { $$ := $2 & ": " & $5 & "-- P2Ada: CWP var univ parameter" & NL; }
   ;

-- FPC 11.7 External functions
-- TP xx
-- THP x
-- CWP xx
external_directive : EXTERNAL_t
   { $$ := "-- P2Ada: external" & NL; }
   | EXTERNAL_t NAME_t constant -- string constant
   { $$ := "-- P2Ada: external " & $2 & ", name " & $3 & NL; }
   | EXTERNAL_t INDEX_t constant -- integer constant
   { $$ := "-- P2Ada: external " & $2 & ", index " & $3 & NL; }
   | EXTERNAL_t constant -- string constant
   { $$ := "-- P2Ada: external " & $2 & NL; }
   | EXTERNAL_t constant NAME_t constant -- string constant
   { $$ := "-- P2Ada: external " & $2 & ", name " & $4 & NL; }
   | EXTERNAL_t constant INDEX_t constant -- integer constant
   { $$ := "-- P2Ada: external " & $2 & ", index " & $4 & NL; }
   ;

-- FPC 11.8 Assembler functions
-- TP 22
-- THP not defined
-- CWP not defined
asm_block : ASSEMBLER_t SEMICOLON_t declaration_part asm_statement
   { $$ := "-- P2Ada: assembler" & NL & $3 & $4; }
   | ASSEMBLER_t SEMICOLON_t asm_statement
   { $$ := "-- P2Ada: assembler" & NL & $3; }
   ;

-- FPC 11.9 Modifiers
-- TP xx
-- THP xx
-- CWP xx
-- Named directives in Delphi manual
modifiers_list : modifiers_list modifiers
   { $$ := $1 & $2; }
   | modifiers
   { $$ := $1; }
   ;
modifiers : modifier_part SEMICOLON_t
   { $$ := $1; }
   |
   { $$ := Null_Unbounded_String; }
   ;
modifier_part : PUBLIC_t
   { $$ := "-- P2Ada: public" & NL; }
   | PUBLIC_t NAME_t constant -- string constant
   { $$ := "-- P2Ada: public name " & $3 & NL; }
   | EXPORT_t
   { $$ := "-- P2Ada: export" & NL; }
   | ALIAS_t COLON_t constant -- string constant
   { $$ := "-- P2Ada: alias " & $3 & NL; }
   | INTERRUPT_t
   { $$ := "-- P2Ada: interrupt" & NL; }
   | call_modifier_part
   { $$ := $1; }
   ;
call_modifiers : call_modifier_part SEMICOLON_t
   { $$ := $1; }
   |
   { $$ := Null_Unbounded_String; }
   ;
call_modifier_part : CDECL_t
   { $$ := "-- P2Ada: cdecl" & NL; }
   | INLINE_t
   { $$ := "-- P2Ada: inline" & NL; }
   | LOCAL_t
   { $$ := "-- P2Ada: local" & NL; }
   | NOSTACKFRAME_t
   { $$ := "-- P2Ada: nostackframe" & NL; }
   | OVERLOAD_t
   { $$ := "-- P2Ada: overload" & NL; }
   | PASCAL_t
   { $$ := "-- P2Ada: pascal" & NL; }
   | REGISTER_t
   { $$ := "-- P2Ada: register" & NL; }
   | SAFECALL_t
   { $$ := "-- P2Ada: safecall" & NL; }
   | SAVEREGISTERS_t
   { $$ := "-- P2Ada: saveregisters" & NL; }
   | SOFTFLOAT_t
   { $$ := "-- P2Ada: softload" & NL; }
   | STDCALL_t
   { $$ := "-- P2Ada: stdcall" & NL; }
   | VARARGS_t
   { $$ := "-- P2Ada: varargs" & NL; }
   | CPPDECL_t
   { $$ := "-- P2Ada: cppdecl" & NL; }
   | FAR16_t
   { $$ := "-- P2Ada: far16" & NL; }
   | OLDFPCCALL_t
   { $$ := "-- P2Ada: oldfpccall" & NL; }
   -- Delphi directives
   | DYNAMIC_t
   { $$ := "-- P2Ada: dynamic" & NL; }
   | NEAR_t
   { $$ := "-- P2Ada: near" & NL; }
   | FAR_t
   { $$ := "-- P2Ada: far" & NL; }
   -- CWP directive
   | C_t
   { $$ := "-- P2Ada: c" & NL; }
   ;

-- FPC 12.2 Operator declarations
-- Not defined in Delphi, TP, THP and CWP
operator_header : OPERATOR_t ope_part result_id COLON_t result_type SEMICOLON_t
   { $$ := "function " & $2 & " return " & $5 & ';' & NL; }
   ;
operator_definition : OPERATOR_t ope_part result_id COLON_t result_type SEMICOLON_t subroutine_block SEMICOLON_t
   { $$ := "function " & $2 & " return " & $5 & "is" & NL & $7 & ';' & NL; }
   ;
ope_part : assignment_operator_definition
   { $$ := $1; }
   | arithmetic_opertor_definition
   { $$ := $1; }
   | comparaison_operator_definition
   { $$ := $1; }
   ;
result_id : identifier -- result identifier
   { $$ := $1; }
   |
   { $$ := Null_Unbounded_String; }
   ;
assignment_operator_definition : ASSIGN_t LPAREN_t parameter_declaration RPAREN_t
   { $$ := "function " & $2 & " return " & $3 & ';' & NL; }
   ;
arithmetic_opertor_definition : arith_ope LPAREN_t parameter_declaration_list RPAREN_t
   { $$ := $1 & '(' & $3 & ')'; }
   ;
arith_ope : PLUS_t
   { $$ := To_Unbounded_String("""+"""); }
   | MINUS_t
   { $$ := To_Unbounded_String("""-"""); }
   | TIMES_t
   { $$ := To_Unbounded_String("""*"""); }
   | DIV_t
   { $$ := To_Unbounded_String("""/"""); }
   | DOUBLESTAR_t
   { $$ := To_Unbounded_String("""**"""); }
   ;
comparaison_operator_definition : comp_ope LPAREN_t parameter_declaration_list RPAREN_t
   { $$ := $1 & '(' & $3 & ')'; }
   ;
comp_ope : EQUAL_t
   { $$ := To_Unbounded_String("""="""); }
   | LT_t
   { $$ := To_Unbounded_String("""<"""); }
   | LE_t
   { $$ := To_Unbounded_String("""<="""); }
   | GT_t
   { $$ := To_Unbounded_String(""">"""); }
   | GE_t
   { $$ := To_Unbounded_String(""">="""); }
   ;

-- FPC 13.1 Programs
-- TP 10.1
-- THP 8.1 (program header always required)
-- CWP 14.2 (program header always required)
program : program_header uses_clause block PERIOD_t
   { $$ := Unit_List & $1 & $3; }
   | uses_clause block PERIOD_t
   { $$ := Unit_List & $1; }
   | program_header block PERIOD_t
   { $$ := $1 & $2; }
   | block PERIOD_t
   { $$ := $1; }
   ;
program_header : PROGRAM_t identifier program_parameters_part SEMICOLON_t
   { $$ := "procedure " & $2 & $3 & " is" & NL; }
   | PROGRAM_t identifier SEMICOLON_t
   { $$ := "procedure " & $2 & " is" & NL; }
   ;
program_parameters_part : LPAREN_t identifier_list RPAREN_t -- program parameters
   { $$ := '(' & $2 & " : Ada.Text_IO.File_Type)"; }
   ;
uses_clause : USES_t identifier_list SEMICOLON_t
   { Append (Unit_List, "with " & $2 & ';' & NL & "use  " & $2 & ';' & NL); }
   ;

-- FPC 13.2 Units
-- TP 10.2
-- THP 8.3
-- CWP 14.3
unit : unit_header hint_directive -- portability directive from Delphi manual
   interface_part implementation_part begin_init_final_part PERIOD_t
   { $$ := Unit_List & $2 & $3 & Unit_List & $4 & $5; }
   ;
unit_header : UNIT_t identifier SEMICOLON_t
   { Set_Unit_Name ($2); }
   ;
interface_part : INTERFACE_t uses_clause unit_declaration_list
   { $$ := "package " & Get_Unit_Name & " is" & NL & $3; }
   | INTERFACE_t uses_clause
   { $$ := "package " & Get_Unit_Name & " is" & NL; }
   | INTERFACE_t unit_declaration_list
   { $$ := "package " & Get_Unit_Name & " is" & NL & $2; }
   | INTERFACE_t
   { $$ := "package" & Get_Unit_Name & " is" & NL; }
   ;
unit_declaration_list : unit_declaration_list unit_declaration_part
   { $$ := $1 & $2; }
   | unit_declaration_part
   { $$ := $1; }
   ;
unit_declaration_part : constant_declaration_part
   { $$ := $1; }
   | type_declaration_part
   { $$ := $1; }
   | variable_declaration_part
   { $$ := $1; }
   | procedure_headers_part
   { $$ := $1; }
   | operator_header
   { $$ := $1; }
   ;
procedure_headers_part : procedure_header call_modifiers
   { $$ := $1 & $2; }
   | procedure_header inline_directive -- from TP and THP manual
   { $$ := $1 & $2; }
   | function_header call_modifiers
   { $$ := $1 & $2; }
   | function_header inline_directive -- from TP and THP manual
   { $$ := $1 & $2; }
   ;
implementation_part : IMPLEMENTATION_t uses_clause declaration_list -- exports_statement from Delphi manual
   { $$ := "end;" & NL & "package body " & Get_Unit_Name & " is" & NL & $3; }
   | IMPLEMENTATION_t uses_clause
   { $$ := "end;" & NL & "package body " & Get_Unit_Name & " is" & NL; }
   | IMPLEMENTATION_t declaration_list
   { $$ := "end;" & NL & "package body " & Get_Unit_Name & " is" & NL & $2; }
   | IMPLEMENTATION_t
   { $$ := "end;" & NL & "package body " & Get_Unit_Name & " is" & NL; }
   ;
begin_init_final_part : initialization_part END_t
   { $$ := "begin" & NL & $1 & NL & "end;" & NL; }
   | initialization_part finalization_part END_t
   { $$ := "begin" & NL & $1 & $2 & NL & "end;" & NL; }
   | BEGIN_t statement_list END_t
   { $$ := "begin" & NL & $2 & NL & "end;" & NL; }
   | END_t
   { $$ := Null_Unbounded_String; }
   ;
initialization_part : INITIALIZATION_t statement_list
   { $$ := "-- P2Ada: initialization" & NL & $2; }
   ;
finalization_part : FINALIZATION_t statement_list
   { $$ := "-- P2Ada: finalization" & NL & $2; }
   ;

-- FPC 13.4 Blocks
-- TP 8.1
-- THP 2.1
-- CWP 13.1
-- block : declaration_list exports_statement compound_statement exports_statement; -- from Delphi manual
block : declaration_list block_statement
   { $$ := $1 & $2; }
   | block_statement
   { $$ := $1; }
   ;
declaration_list : declaration_list declaration_part
   { $$ := $1 & $2; }
   | declaration_part
   { $$ := $1; }
   ;
declaration_part : label_declaration_part
   { $$ := $1; }
   | constant_declaration_part
   { $$ := $1; }
   | resourcestring_declaration_part -- from Delphi and FPC
   { $$ := $1; }
   | type_declaration_part
   { $$ := $1; }
   | variable_declaration_part
   { $$ := $1; }
   | threadvariable_declaration_part -- from Delphi and FPC
   { $$ := $1; }
   | property_declaration_part -- from FPC
   { $$ := $1; }
   | operator_definition -- from FPC
   { $$ := $1; }
   | procedure_function_declaration_part
   { $$ := $1; }
   ;
label_declaration_part : LABEL_t label_list SEMICOLON_t
   { $$ := "-- P2Ada: label declaration: " & $2 & NL; }
   ;
label_list : label_list COMMA_t label
   { $$ := $1 & $2; }
   | label
   { $$ := $1; }
   ;
constant_declaration_part : CONST_t const_list
   { $$ := $2; }
   ;
const_list : const_list const_part
   { $$ := $1 & $2; }
   | const_part
   { $$ := $1; }
   ;
const_part : constant_declaration
   { $$ := $1; }
   | typed_constant_declaration
   { $$ := $1; }
   ;
resourcestring_declaration_part : RESOURCESTRING_t const_list -- string constant
   { $$ := "-- P2Ada: resource string declaration: " & $2; }
   ;
type_declaration_part : TYPE_t type_list
   { $$ := $2; }
   ;
type_list : type_list type_declaration
   { $$ := $1 & $2; }
    | type_declaration
   { $$ := $1; }
   ;
variable_declaration_part : VAR_t var_list
   { $$ := $2; }
   ;
var_list : var_list variable_declaration
   { $$ := $1 & $2; }
   | variable_declaration
   { $$ := $1; }
   ;
threadvariable_declaration_part : THREADVAR_t var_list
   { $$ := "-- P2Ada: thread var declaration: " & $2; }
   ;
property_declaration_part : PROPERTY_t property_declaration_list
   { $$ := "-- P2Ada to do: property" & $2; }
   ;
property_declaration_list : property_declaration_list property_declaration
   { $$ := $1 & $2; }
   | property_declaration
   { $$ := $1; }
   ;
procedure_function_declaration_part : proc_func_list
   { $$ := $1; }
   ;
proc_func_list : proc_func_list proc_func_part
   { $$ := $1 & $2; }
   | proc_func_part
   { $$ := $1; }
   ;
proc_func_part : procedure_declaration
   { $$ := $1; }
   | function_declaration
   { $$ := $1; }
   | constructor_declaration
   { $$ := $1; }
   | destructor_declaration
   { $$ := $1; }
   ;
exports_statement : EXPORTS_t exports_item_list
   { $$ := "-- P2Ada to do: exports " & $2 & NL; }
   ;
exports_item_list : exports_item_list exports_item
   { $$ := $1 & NL & $2; }
   | exports_item
   { $$ := $1; }
   ;
exports_item : identifier NAME_t expression -- string constant
   { $$ := $1 & " -- P2Ada: name " & $3 & NL; }
   | identifier INDEX_t expression -- integer constant
   { $$ := $1 & " -- P2Ada: index " & $3 & NL; }
   | identifier NAME_t constant INDEX_t constant
   { $$ := $1 & " -- P2Ada: name " & $3 & " index " & $5 & NL; }
   | identifier INDEX_t constant NAME_t constant
   { $$ := $1 & " -- P2Ada: index " & $3 & " name " & $5 & NL; }
   | identifier RESIDENT_t
   { $$ := $1 & " -- P2Ada: resident" & NL; }
   | identifier
   { $$ := $1; }
   ;

-- FPC 13.6 Libraries
-- TP not defined
-- THP not defined
-- CWP not defined
library : library_header uses_clause block PERIOD_t
   { $$ := Unit_List & $1 & $3; }
   | library_header block PERIOD_t
   { $$ := $1 & $2; }
   ;
library_header : LIBRARY_t identifier SEMICOLON_t
   { $$ := "-- P2Ada to do: library " & $2 & NL; }
   ;

-- Delphi Packages
-- Not defined in FPC, TP, THP and CWP
package : package_header END_t PERIOD_t
   { $$ := $1; }
   | package_header requires_clause END_t PERIOD_t
   { $$ := $1 & $2; }
   | package_header requires_clause contains_clause END_t PERIOD_t
   { $$ := $1 & $2 & $3; }
   | package_header contains_clause END_t PERIOD_t
   { $$ := $1 & $2; }
   ;
package_header : PACKAGE_t identifier SEMICOLON_t
   { $$ := "-- P2Ada to do: package " & $2 & NL; }
   ;
requires_clause : REQUIRES_t identifier_list SEMICOLON_t
   { $$ := "-- P2Ada to do: requires " & $2 & NL; }
   ;
contains_clause : CONTAINS_t identifier_list SEMICOLON_t
   { $$ := "-- P2Ada to do: contains " & $2 & NL; }
   ;

-- FPC 14 Exceptions
-- FPC 14.1 The raise statement
-- TP not defined
-- THP not defined
-- CWP not defined
raise_statement : RAISE_t exception_instance
   { $$ := "raise " & $2 & ';' & NL; }
   ;
exception_instance : expression
   { $$ := $1; }
   | expression AT_t expression -- adress expression
   { $$ := $1 & " -- P2Ada: address " & $3 & NL; }
   |
   { $$ := Null_Unbounded_String; }
   ;

-- FPC 14.2 The try...except statement
-- TP not defined
-- THP not defined
-- CWP not defined
try_except_statement : TRY_t statement_list EXCEPT_t exception_handlers END_t
   { $$ := "begin" & NL & $2 & NL & "exception" & NL & $4 & "end;"; }
   ;
exception_handlers : statement_list
   { $$ := $1; }
   | exception_handler_list except_else_part
   { $$ := $1 & $2; }
   |
   { $$ := Null_Unbounded_String; }
   ;
exception_handler_list : exception_handler_list SEMICOLON_t exception_handler
   { $$ := $1 & ';' & NL & $3; }
   | exception_handler
   { $$ := $1; }
   | exception_handler_list SEMICOLON_t
   { $$ := $1; }
   ;
exception_handler : ON_t identifier COLON_t type_identifier DO_t statement -- class type identifier
   { $$ := "when " & $2 & ": " & $3 & " =>" & NL & $5; }
   | ON_t type_identifier DO_t statement -- class type identifier
   { $$ := "when " & $2 & " =>" & NL & $4; }
   ;
except_else_part : ELSE_t statement_list
   { $$ := "when others =>" & NL & $2; }
   |
   { $$ := Null_Unbounded_String; }
   ;

-- FPC 14.3 The try...finally statement
-- TP not defined
-- THP not defined
-- CWP not defined
try_finally_statement : TRY_t statement_list FINALLY_t statement_list END_t
   { $$ := "-- P2Ada to do: try with" & NL & $2 & "finally" & $4; }
   ;

object_pascal : program
   { PascalHelp.Put_Line(To_String($1)); }
   | unit
   { PascalHelp.Put_Line(To_String($1)); }
   | library
   { PascalHelp.Put_Line(To_String($1)); }
   | package -- package is from Delphi manual
   { PascalHelp.Put_Line(To_String($1)); }
   ;

%%
with Pascal_Tokens, Pascal_Shift_Reduce, Pascal_Goto;
--use  Pascal_Tokens, Pascal_Shift_Reduce, Pascal_Goto;
with Pascal_DFA, YYroutines, Text_IO, PascalHelp, YYerror;
use  Pascal_DFA, YYroutines, Text_IO, PascalHelp;
--with Ada.Characters.Handling;           use Ada.Characters.Handling;
--with Pascal_Error_Report; use Pascal_Error_Report;
with pascal_io; use pascal_io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
