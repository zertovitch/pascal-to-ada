-------------------------------------------------------------------------------
-- Name        : pascalhelp.adb
-- Description : Object Pascal help utilities for objP2Ada
-- Author      : P2Ada team
-- Version     : 1.4a
-- Last update : 2009-03-28
-- Licence     : GPL V3 (http://www.gnu.org/licenses/gpl.html)
-- Contact     : http://sourceforge.net/projects/P2Ada
-- Notes       : First part come from newP2Ada, last are dedicated to objP2Ada
-------------------------------------------------------------------------------
-- When version change, change also Burlb function

-- 26-Dec-2006 [GdM]
-- * Output through Pascal_IO (as it should be) instead of Ada.Text_IO
-- * Current_line and Current_column to locate translation errors

-- 17-June-2006 [PP]
-- (s) extra brackets when adding attributes are suppressed

-- 18-May-2006 [PP]
-- (s) translate operator set "in" by "and", the overloading of proper "and" body is let to user
--     example :
--     type TChar is array (Character) of Boolean;
--     function "and" (Item : Character; Of_Set : TChar) return Boolean is
--     begin
--     return Of_Set(Item);
--     end;
-- (s) when translating readln(f); and writeln(f); additionnal put or get are suppressed

-- 14-May-2006 [PP]
-- (PO) Take advantage of Ada 2005 object.methode notation (with gnat add -gnat05 switch)

-- 31-May-2003 [GdM]
-- (s) Put_Decimal, Put_Hexadecimal

-- 23-Jan .. 6-Feb-2003 [GdM]
-- (s) ad-hoc variable names (for WITH)
-- (s) Rich Pascal Information. Translation of WITH and New.

--  9-Jan-2003 [GdM] Bug in Function_in_stack fixed
-- 22-Dec-2002 [GdM] Avoids commenting out outputs after comment on same line

-- 15-Dec-2002 [GdM]
-- * Useless (and harmful in case of name conflict) extra
--   "declare..begin..end" removed in functions.
-- * Nested functions correclty translated.
-- * Name of procedures and functions put after "end".

-- 1-Dec-2002 [GdM]
--   Treatement of empty statement sequences

-- 16 Nov 2002 [GACC]:
--   Improved indenting of multiline comments (see Comment procedure)

-- 16-Jan-2002 [GdM]
--   Added casing of Ada keywords (upper/lower/neutral) - see P2Ada_options

-- $Id: adb,v 2.1 1997/08/23 19:56:09 nestor Rel $
-- $Locker:  $
-- $Log: adb,v $
-- Revision 2.1  1997/08/23 19:56:09  nestor
-- Laurent Gasser's correction for MacOS extended Pascal
--
-- Revision 1.1  1997/08/23  08:20:27  nestor
-- Martin C. Carlisle's original version, standard Pascal

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;

with Pascal_IO;

with P2Ada_Options;                     use P2Ada_Options;
with P2Ada_Definition_info;             use P2Ada_Definition_info;
with P2Ada_keywords;

PACKAGE BODY PascalHelp IS

  function Blurb return String is
    c: constant Time:= Clock;
    function Ze_month return String is
    begin
      case Month(c) is
        when  1 => return "Jan";
        when  2 => return "Feb";
        when  3 => return "Mar";
        when  4 => return "Apr";
        when  5 => return "May";
        when  6 => return "Jun";
        when  7 => return "Jul";
        when  8 => return "Aug";
        when  9 => return "Sep";
        when 10 => return "Oct";
        when 11 => return "Nov";
        when 12 => return "Dec";
      end case;
    end Ze_month;
  begin
    return
      "-- Translated on" &
      Integer'Image(Day(c)) & '-' & Ze_month & '-' &
      Trim(Integer'Image(Year(c)),Both)
      & " by (Obj) P2Ada V1.4a 28-Mar-2010";
  end;

  function Current_line return Natural is
  begin
    return Natural(Pascal_IO.YY_Line_Number + eol_in_comment);
  end Current_line;

  function Current_column return Natural is
  begin
    return Natural(Pascal_IO.YY_begin_column);
  end Current_column;

  -- 22-Dec-2002: Avoids commenting out outputs after comment on same line
  maybe_in_comment: Boolean:= False;

  procedure Terminate_eventual_comment is
  begin
    if maybe_in_comment then
      New_Line;
      maybe_in_comment:= False;
    end if;
  end;

  package Buffered_output is
    procedure Put(text : String);
    procedure New_Line;
    procedure Empty;
    procedure Shorten_buffer( length: Natural );
    procedure Flush;
    -- function Give_Buffer return String;
    function Is_InBuffer (Ch : Character) return Boolean;
  end Buffered_output;

  package body Buffered_output is

    Memory : String(1 .. 1600);      --  400 seemed OK
    MemLength : Integer := 0;
    blanks: Integer:= 0;
    blank_up_to_there: Boolean:= True;

    PROCEDURE Put(text : String) IS
    BEGIN
      -- Resync the blank count...
      -- The "Pascal" blanks don't seem to go through PascalHelp !
      if DirectIO and blank_up_to_there then
        blanks:= Integer'Max(
          blanks,
          -- Integer(Ada.Text_IO.Col(Ada.Text_IO.Current_Output)) - 1
          Integer(Pascal_IO.Output_Column) - 1
        );
      end if;
      for i in text'range loop
        case text(i) is
          when ASCII.LF | ASCII.CR =>
            blank_up_to_there:= True;
            blanks:= 0;
          when ' ' =>
            if blank_up_to_there then
              blanks:= blanks + 1;
            end if;
          when others =>
            blank_up_to_there:= False;
        end case;
      end loop;
      IF DirectIO THEN
        -- Ada.Text_IO.Put(text);
        for i in text'Range loop
          Pascal_IO.Output(c => text(i));
        end loop;
      ELSE
        Memory(MemLength+1..MemLength+text'LENGTH) := text;
        MemLength := MemLength + text'LENGTH;
      END IF;
    END Put;

    PROCEDURE New_Line IS
      previous_blanks: constant Integer:= blanks;
    BEGIN
      IF DirectIO THEN
        -- Ada.Text_IO.New_Line;
        Pascal_IO.Output_New_Line;
      ELSE
        case new_line_endings is
          when Unix => Put(ASCII.LF);
          when DOS  => Put(ASCII.CR & ASCII.LF);
          when Mac  => Put(ASCII.CR);
        end case;
      END IF;
      -- Try to reproduce a little bit of indentation 31-Jan-2003
      Put( previous_blanks*' ' );
      -- Ada.Text_IO.put(Ada.Text_IO.standard_error,integer'image(previous_blanks));
    END New_Line;

    PROCEDURE Empty IS
    BEGIN
      MemLength := 0;
    END;

    procedure Shorten_buffer( length: Natural ) is
    begin
      MemLength := Integer'Max(0,MemLength-length);
    end;

    PROCEDURE Flush IS
    BEGIN
      FOR I IN 1..MemLength LOOP
        IF Memory(I) = Standard.ASCII.LF THEN
          -- Ada.Text_IO.New_Line;
          Pascal_IO.Output_New_Line;
        ELSE
          -- Ada.Text_IO.Put(Memory(I));
          Pascal_IO.Output(c => Memory(I));
        END IF;
      END LOOP;
    END Flush;

    --  function Give_Buffer return String is
    --  begin
    --    return Memory(1..MemLength);
    --  end;

     function Is_InBuffer (Ch : Character) return Boolean is
      begin
         for I in 1..MemLength loop
            if Ch = Memory(I) then
               return True;
            end if;
         end loop;
         return False;
    end;

  end Buffered_output;

  procedure Put(text : String) is
  begin
    Terminate_eventual_comment;
    Buffered_output.Put(text);
  end Put;

  procedure New_Line is
  begin
    maybe_in_comment:= False;
    Buffered_output.New_Line;
  end New_Line;

  procedure Empty renames Buffered_output.Empty;
  procedure Shorten_buffer( length: Natural ) renames Buffered_output.Shorten_buffer;
  procedure Flush renames Buffered_output.Flush;

  PROCEDURE Put(text : IN Character) IS
  BEGIN
    Put( (1=> text) );
    -- 31-Jan-2003 : equivalent to the previous explicit code.
  END Put;

  PROCEDURE Put_Line(text : IN String) IS
  BEGIN
    Put(text);
    New_Line;
    -- 31-Jan-2003 : equivalent to the previous explicit code.
  END Put_Line;

  PROCEDURE Put_keyword(text : IN String) IS
  BEGIN
    case P2Ada_Options.keyword_casing is
      when lower => Put(Ada.Characters.Handling.To_lower(text));
      when upper => Put(Ada.Characters.Handling.To_upper(text));
      when neutral => Put(text);
    end case;
  END Put_keyword;

  PROCEDURE Put_translation_comment(text : String) is
  begin
    if add_translation_comments then
      Put(" -- [P2Ada]: " & text);
      maybe_in_comment:= True;
      Put(""); -- !! <- provokes the line feed that the next
               --       Put isn't able to provoke
    end if;
  end;

  procedure Put_empty_otherwise is
  begin
    Put_keyword("WHEN OTHERS=> Null; ");
    Put_translation_comment("no otherwise / else in Pascal");
  end;

  procedure Put_MOD is
  begin
    case translation_of_MOD is
      when is_mod => Put_keyword(" MOD ");
      when is_rem => Put_keyword(" REM ");
    end case;
  end Put_MOD;

  procedure Put_Decimal( s: String ) is     -- 31-May-2003
    exp: Integer; -- position of 'E', or 1 after last.
  begin
    exp:= s'Last+1;
    for i in s'range loop
      if s(i)='e' or s(i)='E' then exp:= i; end if;
    end loop;
    if exp > s'first and then s(exp-1)='.' then -- Need to add '0' !
      Put( s(s'first..exp-1) & '0' & s(exp..s'last) );
    else
      Put( s );
    end if;
  end Put_Decimal;

  procedure Put_Hexadecimal( s: String ) is -- 31-May-2003
  begin
    Put("16#" & s(s'first+1..s'last) & "#");
  end Put_Hexadecimal;

  -- Treatement of empty statement sequences GdM 1-Dec-2002

  null_flag: Boolean; -- Must put 'null' statement or not
  -- NB: no need to stack this information across nesting levels
  -- Reason: after nesting, flag is to False (END passed), which
  -- is correct for lower level since the sequence cannot be empty

  procedure Set_null_flag is
  begin
    null_flag:= True;
  end;

  procedure Clear_null_flag is
  begin
    null_flag:= False;
  end;

  procedure Put_eventual_null is
  begin
    if null_flag then
      Put_keyword("null;");
      Put_translation_comment("empty in Pascal");
    end if;
    null_flag:= False;
  end;

  PROCEDURE Put_keyword_Line(text : IN String) IS
  BEGIN
    case P2Ada_Options.keyword_casing is
      when lower => Put_Line(Ada.Characters.Handling.To_lower(text));
      when upper => Put_Line(Ada.Characters.Handling.To_upper(text));
      when neutral => Put_Line(text);
    end case;
  END Put_keyword_Line;

  PROCEDURE Comment (text : IN String) IS
    Num_Initial_Spaces : Integer := 0;
    Ch : Character;
  BEGIN
    Put("--");
    FOR I IN text'RANGE LOOP
      IF text(I) = Standard.ASCII.LF THEN
        New_Line;
        Put("--");
        Num_Initial_Spaces := 0;
        eol_in_comment:= eol_in_comment + 1;
      ELSE
        Ch := text(I);
        IF Num_Initial_Spaces < 0 THEN
          Put(Ch);
        ELSIF Ch = ' ' THEN
          Num_Initial_Spaces := Num_Initial_Spaces + 1;
        ELSE
          for K in 1 .. Integer'Max (2, Num_Initial_Spaces - 1) loop
            Put(" ");
          end loop;
          Num_Initial_Spaces := -1;
          Put(Ch);
        END IF;
      END IF;
    END LOOP;
    maybe_in_comment:= True;
  END Comment;

  -- Bugfix 10-Jan-2003:
  -- 1/ Pascal's "''" are indeed "'"
  -- 2/ In an Ada String, " must be doubled

  PROCEDURE PrintString (text : IN String) IS
    message: constant String:= text(text'FIRST+1..text'LAST-1);
    skipped: Boolean; -- to reduce '''','''''',... to '', ''', ..., not to '
  BEGIN
    Put('"');
    if message /= "" then
      Put(message(message'first));
      if message(message'first)='"' then
        Put('"');            -- doubling the "
      end if;
      skipped:= False;
      for i in message'first+1 .. message'last loop
        if skipped or message(i-1..i) /= "''" then
          if message(i)='"' then
            Put('"');            -- doubling the "
          end if;
          Put(message(i));
          skipped:= False;
        else
          skipped:= True;        -- skipping the '
        end if;
      end loop;
    end if;
    Put('"');
  END PrintString;

  --------------------------------------

  -- 15-Dec-2002: Subprogram stack

  function Last_Pascal_identifier return String;

  subtype subprog_stack_range is Natural range 0..100;

  type Subprog_info is record
    ada_name, pas_name : NameType;
    ada_nlen, pas_nlen : Natural;
    is_func: Boolean;
  end record;

  subprog_stack: array( subprog_stack_range ) of Subprog_info;
  pointer: subprog_stack_range:= 0;

  -- Only internal:
  procedure Stack_Ada_subprog( Ada_name, Pas_name: String; is_function: Boolean ) is
  begin
    if pointer < subprog_stack_range'last then
      pointer:= pointer + 1;
      declare
        si: Subprog_info renames subprog_stack(pointer);
      begin
        si.ada_name(1..Ada_name'length):= Ada_name;
        si.ada_nlen:= Ada_name'length;
        si.pas_name(1..Pas_name'length):= Pas_name;
        si.pas_nlen:= Pas_name'length;
        si.is_func:= is_function;
      end;
      P2Ada_Definition_info.Enter(Pas_name,funkt,0);
      P2Ada_Definition_info.Mark;
    else
      raise Constraint_error;
    end if;
  end Stack_Ada_subprog;

  -- Available to Pascal.Y
  procedure Stack_Ada_subprog( name: String; is_function: Boolean ) is
  begin
    Stack_Ada_subprog( name, name, is_function );
  end Stack_Ada_subprog;

  -- Available to Pascal.Y
  procedure Stack_Ada_subprog( is_function: Boolean ) is
  begin
    Stack_Ada_subprog(
      Recent_identifier(0),
      Last_Pascal_identifier,
      is_function
    );
  end Stack_Ada_subprog;

  procedure Give_last_function_its_type renames
    P2Ada_Definition_info.Give_last_function_its_type;

  function Last_Ada_subprog_name return String is
  begin
    if pointer > 0 then
      return subprog_stack(pointer).ada_name(1..subprog_stack(pointer).ada_nlen);
    else
      return "";
    end if;
  end Last_Ada_subprog_name;

  function Is_last_a_function return Boolean is
  begin
    if pointer > 0 then
      return subprog_stack(pointer).is_func;
    else
      return False;
    end if;
  end Is_last_a_function;

  procedure Put_last_Ada_subprog_name is
  begin
    Put(Last_Ada_subprog_name);
  end;

  function Function_in_stack( pas_name: String ) return Boolean is
  begin
    for i in reverse 1..pointer loop
      if subprog_stack(i).pas_name(1..subprog_stack(i).pas_nlen) = pas_name then
        return subprog_stack(i).is_func; -- 9-Jan-2003: fix, was "return True;" !
      end if;
    end loop;
    return False;
  end Function_in_stack;

  procedure De_Stack is
  begin
    if pointer > 0 then
      pointer:= pointer - 1;
      P2Ada_Definition_info.Release;
    end if;
  end De_Stack;

  --------------------------------------

  procedure Default_withs is
      procedure Put_Line_With (Position : in String_List.Cursor) is
      begin
         Put_Line(Ada.Strings.Unbounded.To_String(String_List.Element(Position)));
      end;
  begin
      Put_Line(Blurb);
      Put_Line("-- The following with/use clauses are put graciously by P2Ada.");
      Put_Line("-- Some of them may be useless, your Ada compiler will tell it you.");
      Put_Line("--   (GNAT: with '-gnatwa')");
      if String_List.Is_Empty (Default_With_List) then
         Put_Line("with Ada.Text_IO;                       use Ada.Text_IO;");
         Put_Line("with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;");
         Put_Line("with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;");
         Put_Line("with Ada.Long_Float_Text_IO;            use Ada.Long_Float_Text_IO;");
         Put_Line("with Ada.Direct_IO;");
         Put_Line("with Ada.Command_Line;                  use Ada.Command_Line; -- ParamStr,...");
         Put_Line("with Ada.Characters.Handling;           use Ada.Characters.Handling; -- UpCase");
         Put_Line("with Interfaces;                        use Interfaces; -- For Shift_Left/Right");
         Put_Line("-- This is for Pi :");
         Put_Line("with Ada.Numerics;                      use Ada.Numerics;");
         Put_Line("-- This is for Sqrt, Sin, Cos, etc. :");
         Put_Line("with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;");
         Put_Line("with Ada.Numerics.Long_Elementary_Functions;");
         Put_Line(" use Ada.Numerics.Long_Elementary_Functions;");
         Put_Line("-- This is for Dispose. P2Ada writes automatically:");
         Put_Line("--   ""Dispose is new Ada.Unchecked_Deallocation(<type>, <pointer type>)"".");
         Put_Line("with Ada.Unchecked_Deallocation;");
      else
         String_List.Iterate (Default_With_List, Put_Line_With'Access);
      end if;

    New_Line;
  end Default_withs;

  procedure Default_instanciations is
  begin
    Put_Line("  package Boolean_Text_IO is new Enumeration_IO(Boolean);");
    Put("  use Boolean_Text_IO;");
    Put_translation_comment("This is for 'Write([Boolean])'");
    Put_Line("  package Byte_Direct_IO is new Ada.Direct_IO(Unsigned_8);");
    Put_translation_comment("This is for 'file' without type");
    Put("  Program_halted: exception;");
    Put_translation_comment("This is for the Halt pseudo-procedure");
  end Default_instanciations;

  procedure Put_VAR_param is
  begin
    case P2Ada_options.translation_of_VAR is
      when in_out => put_keyword(": IN OUT ");
      when var    => put_keyword(": VAR ");
    end case;
  end Put_VAR_param;

  procedure Put_CONST_param is
  begin
    case P2Ada_options.translation_of_VAR is
      when in_out => put_keyword(": IN ");
      when var    => put_keyword(": CONST ");
    end case;
  end Put_CONST_param;

  -- Automatic creation of ad-hoc types for variables GdM 17-Dec-2002
  -- Generalized 22-Jan-2003

  count : array(ad_hoc_item) of Natural:= (others => 0);

  function Last(a: ad_hoc_item) return String is
    id0: constant String:= Natural'image(count(a));
    id: constant String:= id0(id0'first+1 .. id0'last); -- avoid stupid ' '
    function Name return String is
    begin
      case a is
        when tzpe => return "Type";
        when var  => return "Var";
      end case;
    end;
  begin
    return "P2Ada_" & Name & '_' & id;
  end;

  procedure Add(a: ad_hoc_item) is
  begin
    count(a):= count(a) + 1;
    Memorize_identifier( Last(a), Last(a) );
  end;

  procedure Put_last(a: ad_hoc_item) is
  begin
    Put(Last(a));
  end;

  -- Remember identifiers. FIFO queue

  type Id_info is record
    name   : NameType;
    length : Natural:= 0;
  end record;

  function To_String( i: Id_info ) return String is
  begin
    return i.name(1..i.length);
  end;

  id_queue: array( 0..5 ) of Id_info;

  last_pascal_id: Id_info; -- Memorize untranslated Pascal identifier

  procedure Memorize_identifier( id: String; pascal_id: String ) is
    Ada_id: constant String:= P2Ada_keywords.Avoid_keyword( id );
  begin
    for i in reverse 1..id_queue'last loop
      id_queue(i):= id_queue(i-1);
    end loop;
    id_queue(0).name(1..Ada_id'length):= Ada_id;
    id_queue(0).length:= Ada_id'length;
    last_pascal_id.name(1..pascal_id'length):= pascal_id;
    last_pascal_id.length:= pascal_id'length;
  end Memorize_identifier;

  function Recent_identifier( age: Natural ) return Id_info is
  begin
    if age > id_queue'last then
      raise Constraint_Error;
    else
      return id_queue( age );
    end if;
  end Recent_identifier;

  function Recent_identifier( age: Natural ) return String is
  begin
    return To_String( Recent_identifier( age ) );
  end Recent_identifier;

  function Last_Pascal_identifier return String is
  begin
    return last_pascal_id.name(1..last_pascal_id.length);
  end;

  -- Translation of "EXTERNAL" - GdM 28-Dec-2002
  procedure Put_Import_directive( lang: String ) is
  begin
    put_keyword("pragma");
    put(" Import(" & lang & ", ");
    Put_last_Ada_subprog_name;
    put(", """);
    Put(Last_Pascal_identifier);
    put(""");");
    put_translation_comment("check name and language!");
  end Put_Import_directive;

  procedure Put_Export_directive is
  begin
    put_keyword("pragma");
    put(" Export(" & Recent_identifier(0) & ", ");
    Put_last_Ada_subprog_name;
    put(", """);
    Put_last_Ada_subprog_name;
    put(""");");
    put_translation_comment("check!");
  end Put_Export_directive;

  procedure Finish_Member_of is
  begin
    if member_of then
      member_of:= False;
      Put_translation_comment("""x in y"" -> ""x and y"" redefine ""and"" before");
    end if;
  end Finish_Member_of;

  -- For variants

  stack_of_semicolons: array(0..100) of Boolean;
  top_of_semicolons: Natural:= 0;

  procedure Stack_optional_semicolon is
  begin
    top_of_semicolons:= top_of_semicolons + 1;
    stack_of_semicolons(top_of_semicolons):= Has_Optional_Semicolon;
    Has_Optional_Semicolon:= False;
  end;

  procedure Recall_optional_semicolon is
  begin
    Has_Optional_Semicolon:= stack_of_semicolons(top_of_semicolons);
    top_of_semicolons:= top_of_semicolons - 1;
  end;

  -- Translation of Borland's Inc/Dec intrinsic
  procedure Inc_dec_part_1 is
  begin
    DirectIO:= True;
    Flush;          -- x
    Put(":= ");     -- x:=
    Flush;          -- x:= x
    Empty;
    DirectIO:= False;
  end;

  procedure Inc_dec_part_2( operator: String ) is
  begin
    DirectIO:= True;
    Put(operator); -- "x:= x + " or "x:= x - "
    Flush;
    Empty;
    Put(';');
  end;

  -- SHL/SHR

  shift_flag: Boolean:= False;
  shift_dir:  Shift_direction;

  procedure Open_Shift(d: Shift_direction) is
  begin
    shift_flag:= True;
    shift_dir:= d;
  end Open_Shift;

  procedure Close_Eventual_Shift is
    function dir_img return String is
    begin
      case shift_dir is
        when left => return "Left";
        when right => return "Right";
      end case;
    end;
  begin
    if shift_flag then
      Put(')');
      shift_flag:= False;
      Put_translation_comment(
        "If modular type, better Shift_" & dir_img & "(,)"
      );
    end if;
  end Close_Eventual_Shift;

  -- Around procedure and function bodies
  procedure Open_function_body( type_ident: String ) is
  begin
    Put_keyword_line(" IS");
    Put("Result_"); Put_last_Ada_subprog_name; Put(" : "); Put(type_ident); Put_line(";");
  end Open_Function_body;

  procedure Close_procedure_block is
  begin
    Put_keyword("END "); Put_last_Ada_subprog_name; Put(';');
    De_stack;
  end Close_procedure_block;

  procedure Close_function_block is
  begin
    Put_keyword(" RETURN"); Put(" Result_"); Put_last_Ada_subprog_name; Put_line(";");
    Close_procedure_block;
  end Close_function_block;

  -- Around FOR loops
  procedure Direction_To is
  begin
    DirectIO := True; Flush; Empty; Put(" .."); -- "Normal" situation
    downto:= False;
  end Direction_To;

  procedure Direction_Downto is
  begin
    DirectIO := True;
    put_keyword("REVERSE"); -- Reversing fixed 13-Dec-2002
    -- DirectIO = True, but we keep the Initial_value in the buffer.
    -- So, the second parameter is printed BEFORE the first one.
    downto:= True;
  end Direction_Downto;

  procedure Close_eventual_Downto is
  begin
    if downto then -- Pascal's initial_value is right part of Ada's interval
      Put(".. "); Flush; Empty; Put(' ');
    end if;
  end Close_eventual_Downto;

  -- Around WITH instruction
  procedure WITH_header is
  begin
    Put_translation_comment("WITH instruction");
    Put_keyword_line("DECLARE");
    DirectIO:= False; -- retain output for variable accesses
    Reset_selection;
    P2Ada_Definition_info.Mark;
  end WITH_header;

  procedure Close_WITH renames P2Ada_Definition_info.Release;

  procedure WITH_variable is
    t: constant String:= P2Ada_Definition_info.Name_of_type_selected;
    no_type_found: constant Boolean:= t = "";
    is_record: Boolean;
  begin
    Add(var);
    DirectIO:= True;
    Put("  " & Last(var) & " : ");
    if no_type_found then
      Put("<type>");
    else
      Put(t);
    end if;
    Put_keyword(" RENAMES "); Flush; Empty; Put(';');

    P2Ada_Definition_info.Add_WITH_variables( Last(var), is_record );

    if no_type_found then
      Put_translation_comment(
        "!Help! No type found -> add '" & Last(var) & ".' to fields"
      );
    elsif is_record then
      New_Line;
    else
      Put_translation_comment(
        "!Help! Seemingly a  non-record type found -> add '" &
        Last(var) & ".' to fields"
      );
    end if;
    DirectIO:= False; -- continue retaining (other variables)
    Reset_selection;
  end WITH_variable;

  -- Around declarations.
  --   VAR and parameters: remember to give them a type
  procedure Set_variable_mark renames P2Ada_Definition_info.Set_identifier_mark;
  procedure Give_variables_a_type
     renames P2Ada_Definition_info.Give_variables_a_type;

  --   VAR and parameters: Just name, not yet type
  procedure Enter_var_name is
  begin
    P2Ada_Definition_info.Enter( Last_Pascal_Identifier, Varbl, 0);
  end Enter_var_name;

  procedure Open_type_definition_part is
  begin
    P2Ada_Definition_info.Create_incomplete_types(True);
  end;

  procedure Close_type_definition_part is
  begin
    P2Ada_Definition_info.Create_incomplete_types(False);
  end;

  --   TYPE (and denoter)
  procedure Type_identifier is
    incomplete_found: Boolean;
  begin
    P2Ada_Definition_info.Type_identifier(
      Last_Pascal_Identifier,
      incomplete_found
    );
    if incomplete_found then
      Put_translation_comment(
        "Insert 'type " &
        Last_Pascal_Identifier & ";' before.");
    end if;
  end Type_identifier;

  procedure Denoter_is_String renames
    P2Ada_Definition_info.Denoter_is_String;

  procedure Clear_type_denoter renames
    P2Ada_Definition_info.Clear_type_denoter;

  is_type_decl_open: Boolean:= False;
  -- 20-Dec-2003 : OBJECT closes the declaration prematuraly, before methods

  procedure Open_type_declaration is
  begin
    P2Ada_Definition_info.Enter( Last_Pascal_Identifier, Tipe, 0);
    P2Ada_Definition_info.Set_type_identifier_mark;
    Clear_type_denoter;
    Put_keyword(" IS ");
    DirectIO:= False;
    Just_after_TYPE_X_IS:= True;
    is_type_decl_open:= True;
  end Open_type_declaration;

  procedure Close_type_declaration is
  begin
    if is_type_decl_open then
      P2Ada_Definition_info.Associate_new_type_to_denoter;
      Maybe_must_add_NEW_to_type:= False;
      Put(';');
      is_type_decl_open:= False;
    end if;
  end Close_type_declaration;

  procedure Open_eventual_type_creation is
  begin
    if Maybe_must_create_type then
      Add(tzpe);       -- Add Ad-hoc type
      DirectIO:= True;
      -- But the buffer is kept.
      Put_keyword("type "); Put_last(tzpe); Put_keyword(" is ");
      Just_after_TYPE_X_IS:= True;
    end if;
  end Open_eventual_type_creation;

  procedure No_need_of_type_creation is
  begin
    if Maybe_must_create_type then
      DirectIO:= True;
      Flush;
      Empty;
      Maybe_must_create_type:= False;
    end if;
  end No_need_of_type_creation;

  procedure Close_eventual_type_creation is
  begin
    if Maybe_must_create_type then
      Put(';');
      Put_translation_comment("type definition needed in Ada");
      Flush; -- The variable list comes now
      Empty; -- ':' included
      Put(' ');
      Put_last(tzpe); -- Ad-hoc type
    end if;
  end Close_eventual_type_creation;

  --   TYPE - ARRAY
  procedure Open_array_dim( is_first: Boolean ) renames
    P2Ada_Definition_info.Open_array_dim;

  procedure Close_array_def renames
    P2Ada_Definition_info.Close_array_def;

  --   TYPE - RECORD
  procedure Enter_field_name is
  begin
    P2Ada_Definition_info.Enter( Last_Pascal_Identifier, Field, 0);
  end;

  procedure Open_record_def renames
    P2Ada_Definition_info.Open_record_def;

  procedure Close_record_def renames
    P2Ada_Definition_info.Close_record_def;

  procedure Set_field_mark( nr: Positive ) renames
    P2Ada_Definition_info.Set_field_mark;

  --   TYPE - POINTER

  procedure Open_pointer_def renames
    P2Ada_Definition_info.Open_pointer_def;

  procedure Close_pointer_def renames
    P2Ada_Definition_info.Close_pointer_def;

  --   TYPE - FILE
  procedure Open_file_def renames
    P2Ada_Definition_info.Open_file_def;

  procedure Close_file_def renames
    P2Ada_Definition_info.Close_file_def;


  -- Find the eventual Ada alias of a predefined Pascal identifier
  function Find_alias( Pascal: String ) return String renames
    P2Ada_Definition_info.Find_alias;

  postfix_stack: array( 0..100 ) of Id_info;
  postfix_top: Natural:= 0;

  procedure Stack_Postfixed_alias is
    p: constant String:=
      P2Ada_Definition_info.Find_Postfixed_alias(
        Last_Pascal_identifier
      );
  begin
    postfix_stack( postfix_top ).name(1..p'length):= p;
    postfix_stack( postfix_top ).length:= p'length;
    postfix_top:= postfix_top + 1;
    -- Don't put extra parenthesis with attributes
    if Index(P, (1=>''')) = 0 then
      Put('(');
    end if;
  end Stack_Postfixed_alias;

  procedure Recall_Postfixed_alias is
  begin
    postfix_top:= postfix_top-1;
    declare
      id: Id_info renames postfix_stack( postfix_top );
      p: constant String:= id.name(1..id.length);
    begin
      -- Don't put extra parenthesis with attributes
      if Index(P, (1=>''')) = 0 then
        Put(')');
      end if;
      if p /= "" then
        Put(p);
      end if;
    end;
  end Recall_Postfixed_alias;

  --   TYPE - FILE OF
  procedure Create_Direct_IO( anonymous: Boolean ) is
    ft: constant String:= "_Direct_IO";
  begin
    if anonymous then
      Put( "Byte" );
    end if;
    Put_Line(ft & ".File_Type");
    if not anonymous then
      Put(';');
      Put_translation_comment("Put the following before!");
      Put_keyword("  package ");
      Put(Recent_identifier(0) & ft);
      Put_keyword(" is new ");
      Put("Ada.Direct_IO( " & Recent_identifier(0) & " )");
    end if;
  end Create_Direct_IO;

  --   TYPE - OBJECT
  ObjectStruct : Boolean := False;
  WithEntered  : Boolean := False;

  name_of_object : Id_info;

  procedure Remember_name_of_object( age: Natural ) is
  begin
    name_of_object:= Recent_identifier( age );
  end;

  procedure Link_parent_of_object renames
    P2Ada_Definition_info.Link_parent_of_object;

  procedure EnterObjectStruct is
    begin
    ObjectStruct := True;
    end;

  procedure LeaveObjectStruct is
    begin
    ObjectStruct := False;
    end;

  procedure Var_Self_If_Object( other_params: Boolean ) is
    noo: constant String:= To_String(name_of_object);
    begin
    if ObjectStruct then
      if not other_params then
        Put('(');
      end if;
      Set_variable_mark;
      Clear_Selection;
      Memorize_identifier("Self", "Self");
      Put(Recent_identifier(0));
      Enter_var_name;
      if Is_last_a_function then
        put(':');
      else
        Put_VAR_param;
      end if;
      Clear_Selection;
      Memorize_identifier(noo,noo);
      Put(Recent_identifier(0));
      Type_identifier;
      Give_variables_a_type;
      Clear_Selection;
      Put("'class");
      if not other_params then
        Put(')');
      else
        Put("; ");
      end if;
    end if;
    end Var_Self_If_Object;

  procedure With_Self_If_Object is
    is_record: Boolean;
    begin
    if ObjectStruct then
      -- Protection against recusive structure of parser
      if not WithEntered then
        Reset_selection;
        P2Ada_Definition_info.Mark;
        declare
          Pascal_id: constant String:= "Self";
          -- Up_Pas_id: constant String:= "SELF";
        begin
          Select_identifier( Pascal_id );
          declare
            Ada_id : constant String := Find_alias( Pascal_id );
          begin
            Memorize_identifier( Ada_id, Pascal_id );
            P2Ada_Definition_info.Add_WITH_variables( Ada_id, is_record );
          end;
        end;
        WithEntered := True;
      end if;
    end if;
    end;

  procedure End_Self_If_Object is
    begin
    if WithEntered then
      Close_WITH;
      Put_translation_comment("end of Object");
      WithEntered := False;
    end if;
    end;

  procedure Enter_Methode_Name is
    begin
    if ObjectStruct and not WithEntered then
      Enter_Field_Name;
    end if;
    end;

  procedure Set_Method_Type is
    incomplete_found: Boolean;
  begin
    P2Ada_Definition_info.Type_identifier(
      "",
      incomplete_found,
      True -- Method
      );
  end;

  ---------------
  -- Selectors --
  ---------------

  procedure Reset_selection renames
    P2Ada_Definition_info.Reset_selection;

  procedure Clear_selection renames
    P2Ada_Definition_info.Clear_selection;

  procedure Select_pointed renames
    P2Ada_Definition_info.Select_pointed;

  procedure Conclude_allocator is
  begin
    Select_pointed; -- An implicit step further: 'p^'
    Put_keyword(":= new ");
    declare
      t: constant String:= P2Ada_Definition_info.Name_of_type_selected;
    begin
      if t="" then
        Put("!Help! <type>");
      elsif t="String" then
        Put("String(1..255)");
      else
        Put(t);
      end if;
    end;
  end Conclude_allocator;

  procedure Select_identifier( name: String ) is
  begin
    P2Ada_Definition_info.Select_identifier( name );
  end;

  procedure Select_one_dimension renames
    P2Ada_Definition_info.Select_one_dimension;

  function First_dim_selected return Boolean renames
    P2Ada_Definition_info.First_dim_selected;

  procedure Close_one_dimension renames
    P2Ada_Definition_info.Close_one_dimension;

  procedure Stack_selection renames
    P2Ada_Definition_info.Stack_selection;
  procedure Destack_selection renames
    P2Ada_Definition_info.Destack_selection;

  function Lost_in_selection return Boolean renames
    P2Ada_Definition_info.Lost_in_selection;

  procedure Select_litteral( sort: Character ) renames
    P2Ada_Definition_info.Select_litteral;

  procedure Stop_Export renames
    P2Ada_Definition_info.Stop_Export;

  package body RW is

    -- Read_Write_maybe_to_file: Boolean; -- [ previous method ]

    io_to: File_typing; -- is (no_idea, is_file, is_no_file);

    file_variable: Id_info; -- can be an expression, not only an identifier.

    already_stored, third_or_more: Boolean;

    procedure Complete is
    begin
      DirectIO:= True;
      Flush;
      Empty;
    end;

    procedure Store_file_state is
      file_candidate: constant String:= Recent_identifier(0);
    begin
      if not already_stored then
        io_to:= Is_type_selected_a_file;
        already_stored:= True;
        Clear_Selection;
        file_variable.length:= file_candidate'length;
        file_variable.name(1..file_variable.length):= file_candidate;
      end if;
    end;

    procedure Clear_file_state is
    begin
      io_to:= no_idea;
      file_variable.length:= 0;
      already_stored:= False;
      third_or_more:= False;
      parameter:= False;
    end;

    procedure Complete_Ln( skip_or_new: String ) is
    begin
         if Buffered_output.Is_InBuffer(',') or io_to /= is_file then -- more than 2 parameters so no file name alone
            Complete;
         else
            Empty;
            DirectIO:=True;
         end if;
      Put( skip_or_new & "_Line" );
      if parameter then
        case io_to is
          when no_idea =>
            Put(';');
            Put_translation_comment("!Help! Maybe (file)");
          when is_file =>
            Put('(' & file_variable.name(1..file_variable.length) & ");");
          when is_no_file =>
            Put(';');
        end case;
      else
        Put(';');
      end if;
    end;

    procedure Separate_arguments( get_or_put: String ) is
      begin
      if io_to = is_file and not third_or_more then
        Put(',');
        -- Avoid translating Write(f,a1,a2,a3,...) by
        --   Put(f); Put(f,a1); Put(f,a2); Put(f,a3); ...
        -- but by
        --   Put(f,a1); Put(f,a2); Put(f,a3); ...
      else
        Put_Line(");");
        Put( get_or_put & '(' );
        case io_to is
          when no_idea =>
            Put_translation_comment("!Help! Maybe (file,...) here");
          when is_file =>
            Put(file_variable.name(1..file_variable.length) & ',');
          when is_no_file =>
            null;
        end case;
      end if;
      third_or_more:= True;
      Reset_selection;
    end;

  end RW;

  procedure Put_masked_keyword( id: String ) is
  begin
    Put(id);
    Memorize_identifier( id, id );
  end;

  ------------------------------------------
  -- Routines specific for objp2ada       --
  ------------------------------------------

   -- False if suroutine hasn't block instructions
   Block_Flag : Boolean := False;
   procedure Set_Block_Flag is
   begin
      Block_Flag := True;
   end;
   function Replace_SC_by_IS (Source : Unbounded_String) return Unbounded_String is
      From : constant Positive := Positive(Index (Source, ";", Length(Source), Backward));
   begin
      if Block_Flag then
         Block_Flag := False;
         return Replace_Slice (Source, From, From, " is");
      else
         return Source;
      end if;
   end;

   function To_Ada_String (Source : String) return Unbounded_String is
      Result                                : Unbounded_String :=
        Null_Unbounded_String;
      Enter_String, Enter_Quote, Enter_Ctrl : Boolean          := False;
      Ind_First : Positive := 1;
   begin
      for Ind in Source'Range loop
         case Source (Ind) is
            when '"' =>
               Append (Result, """""");
            when ''' =>
               if not Enter_String then
                  if Enter_Ctrl then
                     Enter_Ctrl := False;
                     Append (Result, To_Ada_Integer(Source(Ind_First..Ind-1)) & ") & """);
                  else
                     Append (Result, """");
                  end if;
                  Enter_String := True;
               elsif not Enter_Quote then
                  Enter_Quote := True;
               else
                  Enter_Quote := False;
                  -- Pascal double quote
                  Append (Result, ''');
               end if;
            when '#' =>
               if not Enter_Quote and Enter_String then
                  Append (Result, Source (Ind));
               elsif not Enter_Ctrl and not Enter_String then
                  Enter_Ctrl := True;
                  Append (Result, "Character'Val (");
                  Ind_First := Ind + 1;
               elsif not Enter_Ctrl and Enter_String then
                  Enter_String := False;
                  Enter_Quote  := False;
                  Enter_Ctrl   := True;
                  Ind_First := Ind + 1;
                  Append (Result, """ & Character'Val (");
               else
                  Append (Result, To_Ada_Integer(Source(Ind_First..Ind-1)) & ") & Character'Val (");
                  Ind_First := Ind + 1;
               end if;
            when others =>
               if not Enter_Ctrl then
                  Append (Result, Source (Ind));
               end if;
         end case;
      end loop;
      if Enter_Ctrl then
         Append (Result, To_Ada_Integer(Source(Ind_First..Source'Last)) & ')');
      end if;
      if Enter_Quote then
         Append (Result, '"');
      end if;
      if Length (Result) = 3
        or else (Length (Result) = 4 and then Element (Result, 2) = '"')
      then
         Result := To_Unbounded_String (''' & Element (Result, 2) & ''');
      end if;
      return Result;
   end To_Ada_String;

   -- Name of main unit
   Unit_Name : Unbounded_String;
   function Get_Unit_Name return Unbounded_String is
   begin
      return Unit_Name;
   end;
   procedure Set_Unit_Name (Name : Unbounded_String) is
   begin
      Unit_Name := Name;
   end;

   function To_Ada_Float (Source : String) return Unbounded_String is
      Result                                : Unbounded_String :=
        Null_Unbounded_String;
      Has_Dot : Boolean := False;
   begin
      for Ind in Source'Range loop
         case Source(Ind) is
         when '.' =>
            Has_Dot := True;
         when 'e' | 'E' =>
            if Has_Dot and then not Ada.Characters.Handling.Is_Decimal_Digit(Source(Ind-1)) then
               Append(Result, '0');
            elsif not Has_Dot then
               Append(Result, ".0");
            end if;
         when others => null;
         end case;
         Append (Result, Source(Ind));
      end loop;
      return Result;
   end;

   function To_Ada_Integer (Source : String) return Unbounded_String is
      Result                                : Unbounded_String :=
        Null_Unbounded_String;
   begin
      case Source(Source'First) is
      when '$' =>
         Append (Result, "16#" & Source(Source'First+1.. Source'Last) & '#');
      when '&' =>
         Append (Result,"8#" & Source(Source'First+1.. Source'Last) & '#');
      when '%' =>
         Append (Result,"2#" & Source(Source'First+1.. Source'Last) & '#');
      when others => Append (Result, Source);
      end case;
      return Result;
   end;

   function To_Ada_Identifier (Source : String) return Unbounded_String is
      type Strings is array (Positive range <>) of String(1..12);
      Ada_Reserved_Word_List : constant Strings := (
                                                    "abort       ", "abs         ", "abstract    ", "accept      ", "access      ",
                                                    "aliased     ", "all         ", "and         ", "array       ", "at          ",
                                                    "begin       ", "body        ", "case        ", "constant    ", "declare     ",
                                                    "delay       ", "delta       ", "digits      ", "do          ", "else        ",
                                                    "elsif       ", "end         ", "entry       ", "exception   ", "exit        ",
                                                    "for         ", "function    ", "generic     ", "goto        ", "if          ",
                                                    "in          ", "interface   ", "is          ", "limited     ", "loop        ",
                                                    "mod         ", "new         ", "not         ", "null        ", "of          ",
                                                    "or          ", "others      ", "out         ", "overriding  ", "package     ",
                                                    "pragma      ", "private     ", "procedure   ", "protected   ", "raise       ",
                                                    "range       ", "record      ", "rem         ", "rename      ", "requeue     ",
                                                    "return      ", "reverse     ", "select      ", "separate    ", "subtype     ",
                                                    "synchronized", "tagged      ", "task        ", "terminate   ", "then        ",
                                                    "type        ", "until       ", "use         ", "when        ", "while       ",
                                                    "with        ", "xor         "
                                                   );
      Result : Unbounded_String := To_Unbounded_String (Source);
      Underscore : Boolean := False;
      Ind_UU : Natural := Index(Result, "__");
   begin
      while Ind_UU /= 0 loop
         Underscore := True;
         Insert(Result, Ind_UU + 1, "u");
         Ind_UU := Index(Result, "__");
      end loop;
      if Element(Result, 1) = '_' then
         Underscore := True;
         Insert(Result, 1, "u");
      end if;
      if Element(Result, Length(Result)) = '_' then
         Underscore := True;
         Append(Result, "u");
      end if;
      if Underscore then
         return Result;
      else
         for Ind in Ada_Reserved_Word_List'Range loop
            if Trim(Ada_Reserved_Word_List(Ind), Right) = Ada.Characters.Handling.To_Lower(Source) then
               return Result & "_k";
            end if;
         end loop;
         return Result;
      end if;
   end;

   function Result_Declaration return Unbounded_String is
      Ind : constant Natural := Index(Subprog_List.Last_Element, ":");
   begin
      if Ind = 0 then
         return Null_Unbounded_String;
      else
         if Element(Subprog_List.Last_Element, 1) /= '#' then
            return "P2Ada_Result_" & Subprog_List.Last_Element & ';' & NL;
         else
            return Delete(Subprog_List.Last_Element, 1, 1) & ';' & NL;
         end if;
      end if;
   end;

   function Null_Or_Return_If_function return Unbounded_String is
   begin
      if Subprog_List.Is_Empty then
         return Null_If_No_Stmt;
      else
         declare
            Ind : constant Natural := Index(Subprog_List.Last_Element, ":");
         begin
            if Ind = 0 then
               return Null_If_No_Stmt;
            else
               if Element(Subprog_List.Last_Element, 1) /= '#' then
                  return "return P2Ada_Result_" &
                  Unbounded_Slice(Subprog_List.Last_Element, 1, Ind-1) & ';' & NL;
               else
                  return "return " &
                  Unbounded_Slice(Subprog_List.Last_Element, 2, Ind-1) & ';' & NL;
               end if;
            end if;
         end;
      end if;
   end;

   function Add_Result_If_Function (Source : Unbounded_String) return Unbounded_String is
      Found : Boolean := False;
      procedure Find (Pos : String_List.Cursor) is
         Ind : constant Natural := Index(String_List.Element(Pos), ":");
      begin
         if not Found and then Ind /= 0 and then Source = Unbounded_Slice (String_List.Element(Pos), 1, Ind-1) then
            Found := True;
         end if;
      end;
   begin
      if Subprog_List.Is_Empty then
         return Source;
      else
         Subprog_List.Iterate(Find'Access);
         if Found then
            return "P2Ada_Result_" & Source;
         else
            return Source;
         end if;
      end if;
   end;

   function Get_Subprog_Name (Source : Unbounded_String) return Unbounded_String is
      Ind : constant Natural := Index(Source, ":");
   begin
      if Ind = 0 then
         return Source;
      else
         return Unbounded_Slice(Source, 1, Ind-1);
      end if;
   end;

   procedure Init_And_Operator (Ordinal_Type : Unbounded_String) is
   begin
      Append (And_Operator_List, "function ""and"" (Item : " & Ordinal_Type & "; Of_Set : #) return Boolean is" & NL
              & "begin" & NL
              & "return Of_Set(Item);" & NL
              & "end;" & NL);
   end;
   procedure Finalize_And_Operator (Set_Type : Unbounded_String) is
      Ind : constant Natural := Index(And_Operator_List, "#");
   begin
      if Ind /= 0 then
         Replace_Slice(And_Operator_List, Ind, Ind, To_String(Set_Type));
      end if;
   end;

   -- True if subtype is required
   Subtype_Flag : Boolean := False;
   function Type_Or_Subtype return Unbounded_String is
   begin
      if Subtype_Flag then
         Subtype_Flag := False;
         return To_Unbounded_String("subtype ");
      else
         return To_Unbounded_String("type ");
      end if;
   end;
   procedure Reset_Subtype is
   begin
      Subtype_Flag := False;
   end;
   procedure Set_Subtype is
   begin
      Subtype_Flag := True;
   end;

   -- Index value for each creation of anonym types
   subtype Type_Index is Natural range 0..200;
   Anonym_Type_Index : Type_Index := 0;
   Procedure New_Anonym_Type_Name is
   begin
      Anonym_Type_Index := Anonym_Type_Index + 1;
   end;
   function Get_Anonym_Type_Name return Unbounded_String is
   begin
      return To_Unbounded_String("P2Ada_Anonym_" & Trim(Natural'Image(Anonym_Type_Index), Left));
   end;

   -- Block level
   subtype Levels is Natural range 0..200;
   Block_Level : Levels := 0;
   procedure Inc_Block_Level is
   begin
      Block_Level := Block_Level + 1;
   end;
   procedure Dec_Block_Level is
   begin
      Block_Level := Block_Level - 1;
   end;

   -- List of P2Ada methods declarations inside object or class
   Method_List : Unbounded_String := Null_Unbounded_String;
   -- True if object declaration
   Object_Flag : Boolean := False;
   Object_Level : Levels := 0;
   function Package_If_Object (Name : Unbounded_String) return Unbounded_String is
   begin
      if Object_Flag then
         return "package " & Name & " is" & NL;
      else
         return Null_Unbounded_String;
      end if;
   end;
   function Finalize_Object_Package return Unbounded_String is
      Result : Unbounded_String;
   begin
      if Object_Flag then
         Object_Flag := False;
         Result := Method_List & "end; -- package" & NL;
         Method_List := Null_Unbounded_String;
         return Result;
      else
         return Null_Unbounded_String;
      end if;
   end;
   function Instance_If_object (Name : Unbounded_String) return Unbounded_String is
   begin
      if Object_Flag then
         return To_Unbounded_String("Instance");
      else
         return Name;
      end if;
   end;
   procedure Append_Method_List (Method : Unbounded_String) is
      Result : Unbounded_String := Method;
      Ind : Natural := Index(Method, "(");
   begin
      if Ind /= 0 then
         if Element (Result, 1) = 'f' then
            Insert (Result, Ind + 1, "Self : Instance; ");
         else
            Insert (Result, Ind + 1, "Self : in out Instance; ");
         end if;
      else
         if Element (Result, 1) = 'f' then
            Ind := Index(Method, " return ");
            Insert (Result, Ind, " (Self : Instance)");
         else
            Ind := Index(Method, ";");
            Insert (Result, Ind, " (Self : Instance)");
         end if;
      end if;
      Append (Method_List, Result);
   end;
   procedure Reset_Object is
   begin
      Object_Flag := False;
   end;
   procedure Set_Object is
   begin
      Object_Flag := True;
      Object_Level := Block_Level;
   end;
   Object_Field_Flag : Boolean := False;
   function Null_If_No_Field return Unbounded_String is
   begin
      if Object_Field_Flag then
         Object_Field_Flag := False;
         return Null_Unbounded_String;
      else
         return "null;" & NL;
      end if;
   end;
   procedure Set_Object_Field is
   begin
      Object_Field_Flag := True;
   end;
   -- Package body list of all declared object
   Package_Body_List : String_List.List;
   procedure Append_Package_Body (Method : Unbounded_String) is
      use String_List;
      Found : Cursor := No_Element;
      procedure Find (Pos : Cursor) is
         Ind : constant Natural := Index(Element(Pos), To_String(Object_Name));
      begin
         if Found = No_Element and then Ind /= 0 then
            Found := Pos;
         end if;
      end;
   begin
      if not Package_Body_List.Is_Empty then
         Package_Body_List.Iterate(Find'Access);
      end if;
      if Found /= No_Element then
         Package_Body_List.Replace_Element (Found, Element (Found) & Method);
      else
         Package_Body_List.Append("package body " & Object_Name & " is" & NL & Method);
      end if;
   end;
   function Append_If_object (Method : Unbounded_String) return Unbounded_String is
      Result : Unbounded_String := Method;
      Ind_Par : constant Natural := Index(Method, "(");
      Ind_Is  : constant Natural := Index(Method, " is");
      Ind_Ret : constant Natural := Index(Method, " return ");
   begin
      if Object_Name /= Null_Unbounded_String then
         if Ind_Par /= 0 and then Ind_Par < Ind_Is then
            if Element (Method, 1) = 'f' then
               Insert (Result, Ind_Par + 1, "Self : Instance; ");
            else
               Insert (Result, Ind_Par + 1, "Self : in out Instance; ");
            end if;
         else
            if Element (Method, 1) = 'f' then
               Insert (Result, Ind_Ret, " (Self : Instance)");
            else
               Insert (Result, Ind_Is, " (Self : Instance)");
            end if;
         end if;
         Append_Package_Body (Result);
         Object_Name := Null_Unbounded_String;
         return Null_Unbounded_String;
      else
         return Method;
      end if;
   end;
   function Finalize_Package_Body return Unbounded_String is
      Result : Unbounded_String := Null_Unbounded_String;
      use String_List;
      procedure Append_To_Result (Pos : Cursor) is
      begin
         Append (Result, Element(Pos) & "end; -- package body" & NL);
      end;
   begin
      if Object_Level = Block_Level and then not Package_Body_List.Is_Empty then
         Package_Body_List.Iterate(Append_To_Result'access);
         Package_Body_List.Clear;
         return Result;
      else
         return Null_Unbounded_String;
      end if;
   end;

   function Tagged_Type (Heritage : Unbounded_String) return Unbounded_String is
   begin
      if Heritage /= Null_Unbounded_String then
         return "new " & Heritage & ".Instance with record" & NL;
      else
         return "tagged record" & NL;
      end if;
   end;

   Has_Stmt : Boolean := False;
   procedure Set_Has_Stmt is
   begin
      Has_Stmt := True;
   end;
   -- Reset has statement flag
   procedure Reset_Has_Stmt is
   begin
      Has_Stmt := False;
   end;
   -- Return null; or nothing if has already statement
   function Null_If_No_Stmt return Unbounded_String is
   begin
      if Has_Stmt then
         return Null_Unbounded_String;
      else
         return "null;" & NL;
      end if;
   end;

   function Program_Parameter (Source : Unbounded_String) return Unbounded_String is
      -- Only Input and Output program parameters are bounded to Ada.Text_IO
      -- as they are implicit declared in Pascal
      Result : Unbounded_String := "-- P2Ada: program parameters: " & Source & NL;
      Ind_Input : constant Natural := Index(Ada.Characters.Handling.To_Lower(To_String(Source)), "input");
      Ind_Output : constant Natural := Index(Ada.Characters.Handling.To_Lower(To_String(Source)), "output");
   begin
      if Ind_Input /= 0 then
         Append (Result, "Input : Ada.Text_IO.File_Type renames Ada.Text_IO.Standard_Input;" & NL);
      end if;
      if Ind_Output /= 0 then
         Append (Result, "Output : Ada.Text_IO.File_Type renames Ada.Text_IO.Standard_Output;" & NL);
      end if;
      return Result;
   end;

   -- List of record discriminants
   Discriminant_List : Unbounded_String := Null_Unbounded_String;
   procedure Add_Discriminant (Source : Unbounded_String) is
   begin
      if Discriminant_List = Null_Unbounded_String then
         Append (Discriminant_List, '(' & Source);
      else
         Append (Discriminant_List, ';' & Source);
      end if;
   end;
   function Get_Discriminant return Unbounded_String is
      Result : constant Unbounded_String := Discriminant_List & ')';
   begin
      if Discriminant_List = Null_Unbounded_String then
         return Null_Unbounded_String;
      else
         Discriminant_List := Null_Unbounded_String;
         return Result;
      end if;
   end;

  procedure OBJ_Open_type_declaration is
  begin
    P2Ada_Definition_info.Enter( Last_Pascal_Identifier, Tipe, 0);
    P2Ada_Definition_info.Set_type_identifier_mark;
    Clear_type_denoter;
    -- Put_keyword(" IS ");
    DirectIO:= False;
    Just_after_TYPE_X_IS:= True;
    is_type_decl_open:= True;
  end;

  procedure OBJ_Close_type_declaration is
  begin
    if is_type_decl_open then
      P2Ada_Definition_info.Associate_new_type_to_denoter;
      Maybe_must_add_NEW_to_type:= False;
      -- Put(';');
      is_type_decl_open:= False;
    end if;
  end;

  procedure OBJ_WITH_header is
  begin
    -- Put_translation_comment("WITH instruction");
    -- Put_keyword_line("DECLARE");
    DirectIO:= False; -- retain output for variable accesses
    Reset_selection;
    P2Ada_Definition_info.Mark;
  end;

  procedure OBJ_WITH_variable is
    t: constant String:= P2Ada_Definition_info.Name_of_type_selected;
    no_type_found: constant Boolean:= t = "";
    is_record: Boolean;
  begin
    Add(var);
    DirectIO:= True;
    Append (With_Suffixe, Last(var) & " : ");
    if no_type_found then
      Append (With_Suffixe, "<type>");
    else
      Append (With_Suffixe,t);
    end if;
    Append (With_Suffixe, " RENAMES "); -- Flush; Empty; Append (With_Suffixe, ';');

    P2Ada_Definition_info.Add_WITH_variables( Last(var), is_record );

    if no_type_found then
      Append (With_Suffixe,
        "-- !Help! No type found -> add '" & Last(var) & ".' to fields of "
      );
    elsif is_record then
      null; -- New_Line;
    else
      Append (With_Suffixe,
        "-- !Help! Seemingly a  non-record type found -> add '" &
        Last(var) & ".' to fields of "
      );
    end if;
    DirectIO:= False; -- continue retaining (other variables)
    Reset_selection;
  end;

  procedure OBJ_Var_Self_If_Object is
    noo: constant String:= To_String(name_of_object);
    begin
    if ObjectStruct then
      Set_variable_mark;
      Clear_Selection;
      Memorize_identifier("Self", "Self");
      Enter_var_name;
      Clear_Selection;
      Memorize_identifier(noo,noo);
      Type_identifier;
      Give_variables_a_type;
      Clear_Selection;
      end if;
    end;

   function Get_Variable_Type return Unbounded_String is
   begin
      return To_Unbounded_String(P2Ada_Definition_info.Get_Variable_Type);
   end;

   function Get_Img_Tag return Unbounded_String is
      T : constant String := P2Ada_Definition_info.Get_Last_Selected;
   begin
      if Index(T, "Char") /= 0 or else Index(T, "String") /= 0 then
         return Null_Unbounded_String;
      else
         return To_Unbounded_String("'img");
      end if;
   end;

   function Get_Unit_List return Unbounded_String is
     Dum : Unbounded_String := Null_Unbounded_String;
     procedure Add (Position : in String_List.Cursor) is
      begin
         Dum := Dum & String_List.Element(Position) & NL;
      end;
     begin
       if String_List.Is_Empty (Default_With_List) then
         return "with Ada.Direct_IO;" & NL
               & "with Ada.Text_IO; use Ada.Text_IO;" & NL
               & "with Interfaces; use Interfaces;" & NL
               & "with Ada.Unchecked_Deallocation;" & NL;
       else
         String_List.Iterate (Default_With_List, Add'Access);
         return Dum;
       end if;
     end;

END PascalHelp;
