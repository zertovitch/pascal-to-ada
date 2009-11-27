-- 31-Aug-2009 [PP]
-- Added string list facility for default withs from config file
-- 26-Dec-2006 [GdM]
--    Added eol_in_comment and the Line_number function

-- 23-Jan .. 6-Feb-2003 [GdM]
-- (s) ad-hoc variable names (for WITH)
-- (s) Rich Pascal Information. Translation of WITH and New.

-- 28-Dec-2002 [GdM]
-- Translation of "EXTERNAL"

-- 18,17-Dec-2002 [GdM]
-- Remembers recent identifiers, creates names for ad-hoc types

-- 15-Dec-2002 [GdM]
-- * Useless (and harmful in case of name conflict) extra
--   "declare..begin..end" removed in functions.
-- * Nested functions correclty translated.
-- * Name of procedures and functions put after "end".

-- 13-Dec-2002 [GdM]
--   Downto -> reverse bug fixed (interval must be reversed)

-- 1-Dec-2002 [GdM]
--   Treatement of empty statement sequences

-- 19-Nov-2002 [GdM]
-- Added blurb string

-- 16-Jan-2002 [GdM]
-- Added casing of Ada keywords (upper/lower/neutral) - see P2Ada_options

-- $Id: pascalhelp.ads,v 2.1 1997/08/23 20:00:09 nestor Rel $
-- $Locker:  $
-- $Log: pascalhelp.ads,v $
-- Revision 2.1  1997/08/23 20:00:09  nestor
-- Laurent Gasser's correction for MacOS extended Pascal
-- Added variables to store the unit (alias package) name.
-- Added flag for optional semi-colon and solve grammar conflict.
--
-- Revision 1.1  1997/08/23  08:05:50  nestor
-- Martin C. Carlisle's original version, standard Pascal
--
-- p2ada PascalHelp package
--
-- Martin C. Carlisle, US Air Force Academy
--
-- November 26, 1996
--
-- Laurent Gasser, Hirondelle Lumineuse
-- June 21, 1997
-- Adaptation to extended MacOS Pascal

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling;
PACKAGE PascalHelp IS

  blurb: String:= "-- Translated by (New) P2Ada v. 15-Nov-2006";

  SUBTYPE NameType IS String(1..255); -- (was 80) - 15-Dec-2002

  -- lga; jun 21, 97
  Package_Name : NameType;
  Package_Name_Length : Natural := 0;

  --lga; jul 12, 1997
  Has_Optional_Semicolon : BOOLEAN := FALSE;

  -- GdM 20-Jan-2003: fixes a bug for inside of (...) in record variants
  procedure Stack_optional_semicolon;
  procedure Recall_optional_semicolon;

  -- Gdm 15-Dec-2002: replaced FunctionName & FnNameLength
  --   by a nesting-proof variant; allows putting names at the end
  --   of procedures too.
  procedure Stack_Ada_subprog( name: String; is_function: Boolean );
  procedure Stack_Ada_subprog( is_function: Boolean );
  function Last_Ada_subprog_name return String;
  procedure Put_last_Ada_subprog_name;
  function Is_last_a_function return Boolean;
  function Function_in_stack( pas_name: String ) return Boolean;
  procedure De_Stack;
  function_result_flag: Boolean:= False;
  -- function_needs_goto: Boolean; -- <- no more used

  -- Unused but left here to test old Pascal.y's :
  -- FunctionName : NameType;
  -- FnNameLength : Integer := 0;
  -- FunctionReturn : Boolean := False; -- <- Never used

  DirectIO : Boolean := True;
  Downto: Boolean; -- GdM 13-Dec-2002

  eol_in_comment: Natural:= 0;
  function Current_line return Natural;
  function Current_column return Natural;

  Maybe_must_create_type: Boolean:= False; -- inside a VAR or CONST decl.
  Maybe_must_add_NEW_to_type: Boolean:= False; -- inside a TYPE decl.
  Just_after_TYPE_X_IS : Boolean; -- in this context, maybe "RANGE" must be added.

  -- prints this text as an Ada comment (possibly multiple lines)
  PROCEDURE Comment (text : IN String);

  -- prints this text as an Ada String
  PROCEDURE PrintString (text : IN String);

  -- IO routines, just call Ada IO if DirectIO is true, otherwise buffer
  PROCEDURE Put(text : IN String);
  PROCEDURE Put_keyword(text : IN String);
  PROCEDURE Put_translation_comment(text : IN String);
  PROCEDURE Put(text : IN Character);
  PROCEDURE Put_Line(text : IN String);
  PROCEDURE Put_keyword_Line(text : IN String);
  PROCEDURE New_Line;
  procedure Put_empty_otherwise;
  procedure Put_MOD;
  procedure Put_Decimal( s: String );     -- 31-May-2003
  procedure Put_Hexadecimal( s: String ); -- 31-May-2003

  -- Treatement of empty statement sequences GdM 1-Dec-2002
  procedure Set_null_flag;
  procedure Clear_null_flag;
  procedure Put_eventual_null;

  -- clear text buffer (erase contents)
  PROCEDURE Empty;
  -- partially clear text buffer
  procedure Shorten_buffer( length: Natural );
  -- flush text buffer to IO device (buffer unchanged)
  PROCEDURE Flush;

  -- Writes With/Use for Text_IO
  procedure Default_withs;
  procedure Default_instanciations;
  package String_List is new Ada.Containers.Doubly_Linked_Lists
     (Ada.Strings.Unbounded.Unbounded_String,
      Ada.Strings.Unbounded."=");
  Default_With_List : String_List.List;

  -- Writes translation of "var" in subprogram parameters (TP7's "const" too)
  procedure Put_VAR_param;
  procedure Put_CONST_param;

  -- Automatic creation of ad-hoc types for variables GdM 17-Dec-2002
  -- Generalized 22-Jan-2003
  type ad_hoc_item is ( tzpe, var );

  function Last(a: ad_hoc_item) return String;
  procedure Add(a: ad_hoc_item);
  procedure Put_Last(a: ad_hoc_item);

  -- Remember identifiers. GdM 18-Dec-2002. + pascal_id 25-Jan-2003
  procedure Memorize_identifier( id: String; pascal_id: String );
  function Recent_identifier( age: Natural ) return String;

  -- Translation of "EXTERNAL" - GdM 28-Dec-2002
  procedure Put_Import_directive( lang: String );
  procedure Put_Export_directive;

  -- Translation of "x in y" by "( (y)(x) )"
  member_of: Boolean:= False;
  procedure Finish_Member_of;

  -- Translation of Borland's Inc/Dec intrinsic
  procedure Inc_dec_part_1;
  procedure Inc_dec_part_2( operator: String );

  -- SHL/SHR
  procedure Open_Shift;
  procedure Close_Eventual_Shift;

  --------------------------------------------------------------
  -- Rich Pascal Information (types, identifiers, structures) --
  --------------------------------------------------------------

  -- Around procedure and function bodies
  procedure Open_function_body( type_ident: String );
  procedure Close_procedure_block;
  procedure Close_function_block;

  -- Around FOR loops
  procedure Direction_To;
  procedure Direction_Downto;
  procedure Close_eventual_Downto;

  -- Around WITH instruction
  procedure WITH_header;
  procedure WITH_variable;
  procedure Close_WITH;

  procedure Give_last_function_its_type;

  -- Around declarations.
  --   VAR and parameters: remember to give them a type
  procedure Set_variable_mark;
  -- After type denoter is read:
  procedure Give_variables_a_type;
  --   VAR and parameters: Just name, not yet type
  procedure Enter_var_name;

  --   TYPE : definition part.
  --          Needed for incomplete types:
  --          type p=^t {undefined here}; t = record next: p end;

  procedure Open_type_definition_part;
  procedure Close_type_definition_part;

  --   TYPE (and denoter)
  procedure Clear_type_denoter;
  procedure Type_identifier;
  procedure Denoter_is_String;

  procedure Open_type_declaration;
  procedure Close_type_declaration;

  procedure Open_eventual_type_creation;
  procedure No_need_of_type_creation;
  procedure Close_eventual_type_creation;

  --   TYPE - ARRAY
  procedure Open_array_dim( is_first: Boolean );  -- one dimension
  procedure Close_array_def; -- all dimensions

  --   TYPE - RECORD
  procedure Enter_field_name;
  procedure Open_record_def;
  procedure Close_record_def;
  procedure Set_field_mark( nr: Positive ); -- mark 1 or 2

  --   TYPE - POINTER
  procedure Open_pointer_def;
  procedure Close_pointer_def;

  --   TYPE - FILE
  procedure Open_file_def;
  procedure Close_file_def;

  -- Find the eventual Ada alias of a predefined Pascal identifier
  function Find_alias( Pascal: String ) return String;

  procedure Stack_Postfixed_alias;
  procedure Recall_Postfixed_alias;

  --   TYPE - FILE OF
  procedure Create_Direct_IO( anonymous: Boolean );


  --   TYPE - OBJECT
  procedure Remember_name_of_object( age: Natural );
  procedure Link_parent_of_object;
  procedure EnterObjectStruct;
  procedure LeaveObjectStruct;
  procedure Var_Self_If_Object( other_params: Boolean );
  procedure With_Self_If_Object;
  procedure End_Self_If_Object;
  procedure Enter_Methode_Name;
  procedure Set_Method_Type;

  ---------------
  -- Selectors --
  ---------------
  -- Aims:
  -- 1/ find the type T4 in
  --   Pascal: New(p6[v3a.i]^.a);
  --      Ada: p6(v3a.i).all.a:= new T4;
  -- 2/ find the record types of v1,v2 in
  --   Pascal: with v1,v2 do ...

  procedure Reset_selection;   -- Bring selection at level 0 and clear
  procedure Clear_Selection;   -- Clear selection on current level
  procedure Stack_selection;   -- Level -> +1
  procedure Destack_selection; -- Level -> -1

  procedure Conclude_allocator;

  procedure Select_identifier( name: String );
  procedure Select_pointed;

  procedure Select_one_dimension;
  function First_dim_selected return Boolean;
  procedure Close_one_dimension;

  function Lost_in_selection return Boolean; -- at least one undefined type
  Lost: Boolean; -- variable to remember the "lost" state before next selection

  procedure Select_litteral( sort: Character ); -- 'C'har, 'S'tring, 'N'umber

  ------------------------------------------
  -- Limit the exportation of definitions --
  ------------------------------------------

  procedure Stop_Export; -- Freezes and save now the definitions.

  ---------------------------------------------------------
  -- Write[Ln] or Read[Ln]: resolve cases of I/O to file --
  -- Write(f,a,b,c) -> Put(f,a); Put(f,b); Put(f,c);     --
  ---------------------------------------------------------

  package RW is
    parameter: Boolean;
    procedure Store_file_state;
    procedure Clear_file_state;
    procedure Complete;
    procedure Complete_Ln( skip_or_new: String );
    procedure Separate_arguments( get_or_put: String );
  end RW;

  procedure Put_masked_keyword( id: String );

  ------------------------------------------
  -- Routines specific for objp2ada       --
  ------------------------------------------

-- When subroutine has got a body replace ";" by " is "
function Replace_SC_by_IS (Source : Unbounded_String) return Unbounded_String;
-- Newline separator in internal text buffer
NL : constant Unbounded_String := To_Unbounded_String ((1=>ASCII.LF));
-- Translate Pascal string separator "'" to """, control character #nnn and double """
function To_Ada_String (Source : String) return Unbounded_String;
-- Get stored unit name
function Get_Unit_Name return Unbounded_String;
-- Store unit name
procedure Set_Unit_Name (Name : Unbounded_String);
-- Translate Pascal reel numbers with permissive syntax 10e3, 10.e3
function To_Ada_Float (Source : String) return Unbounded_String;
-- Translate Pascal integer numbers with syntax hexa $hh, octal &ooo and binary %bbbb
function To_Ada_Integer (Source : String) return Unbounded_String;
-- Translate Pascal identifiers which match Ada reserved word and replace __ by u_
function To_Ada_Identifier (Source : String) return Unbounded_String;

END PascalHelp;
