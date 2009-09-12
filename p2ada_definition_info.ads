-- Started 23-Jan-2003 [GdM].

-- This pseudo-compiler construction was forseen just for
-- trying to translate the WITH instruction !

-- Currently it helps P2Ada supporting :

--  1/ Predefined identifiers - and their eventual Ada aliases -
--     from various Pascal dialects, since they can and should be
--     hidden by explicit identifiers in Pascal programs from other
--     dialects.
--
--  2/ Translation of New(...) : finds type for ":= new <type>".
--
--  3/ Translation of WITH.

-- Parts inspired by Pascal_S, other not: this is more relax and less
-- complete than for an actual Pascal compiler.

package P2Ada_Definition_info is

  procedure Set_Pascal_source(name: String);
  function  Get_Pascal_source return String;

  procedure Set_Ada_output(name: String);
  function  Get_Ada_output return String;

  type Idkind  is  (Konst, Varbl, Field, Tipe, Funkt, Alias);
  
  -- For variables and parameters, to allow give them a type
  -- after they are entered
  procedure Set_identifier_mark;

  procedure Set_field_mark( nr: Positive ); -- mark 1 or 2

  -- After type denoter is read:
  procedure Give_variables_a_type;

  procedure Set_type_identifier_mark;
  procedure Associate_new_type_to_denoter;
  
  -- Enter a new declaration.
  procedure Enter(Id: String; K: Idkind; T: Integer);
  
  -- Find the eventual Ada alias of a predefined Pascal identifier
  function Find_alias( Pascal: String ) return String;
  function Find_Postfixed_alias( Pascal: String ) return String;
  
  procedure Give_last_function_its_type;
  
  --------------------
  -- Input / Output --
  --------------------

  -- Load items from a file. They are visible at top level.
  -- Mainly, they correspond to the "interface" of an Unit.
  procedure Load( file_name: String );

  -- Save items to a file, define export limits.
  procedure Stop_Export;  
  procedure Will_save( file_name: String );
  
  -- Sets a "marker" before new, more local definitions,
  -- inside a subprogram.
  procedure Mark;

  -- Forget the definitions created after latest unreleased "Mark".  
  procedure Release;

  -- WITH instruction. Record type (if so !) is of latest selected.
  procedure Add_WITH_variables( prefix: String; is_record: out Boolean );

  -- To be called a begin and end of a type definition part.
  procedure Create_incomplete_types( start: Boolean );
  
  ----------------------------------
  -- Construction of type denoter --
  ----------------------------------

  -- Some denoters will end with no type, for various reasons...
  procedure Clear_type_denoter;
  
  procedure Type_identifier( name: String; incomplete_found: out Boolean; is_method: Boolean:= False );
  procedure Denoter_is_String;

  procedure Open_array_dim( is_first: Boolean );  -- one dimension
  procedure Close_array_def; -- all dimensions

  procedure Open_record_def;
  procedure Close_record_def;
  procedure Link_parent_of_object; -- OO
  
  procedure Open_pointer_def;
  procedure Close_pointer_def;

  procedure Open_file_def;  
  procedure Close_file_def;

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
  procedure Clear_selection;   -- Clear selection on current level
  procedure Stack_selection;   -- Level -> +1
  procedure Destack_selection; -- Level -> -1

  function Lost_in_selection return Boolean; -- at least one undefined type
  
  function Name_of_type_selected return String;

  type File_typing is (no_idea, is_file, is_no_file);

  function Is_type_selected_a_file return File_typing;

  procedure Select_identifier( id: String );
  procedure Select_pointed;

  procedure Select_one_dimension;
  function First_dim_selected return Boolean;
  procedure Close_one_dimension;

  procedure Select_litteral( sort: Character ); -- 'C'har, 'S'tring, 'N'umber
  
end P2Ada_Definition_info;
