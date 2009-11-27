--with GNAT.Traceback.Symbolic, Ada.Exceptions;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with P2Ada_options;                     use P2Ada_options;
with PascalHelp;                        use PascalHelp;

package body P2Ada_Definition_info is

  Pascal_name: Unbounded_String:= To_Unbounded_String("<Standard_Input>");

  procedure Set_Pascal_source(name: String) is
  begin
    Pascal_name:= To_Unbounded_String(name);
  end;

  function  Get_Pascal_source return String is
  begin
    return To_String(Pascal_name);
  end;

  Ada_output: Unbounded_String:= To_Unbounded_String("<Standard_Output>");

  procedure Set_Ada_output(name: String) is
  begin
    Ada_output:= To_Unbounded_String(name);
  end;

  function  Get_Ada_output return String is
  begin
    return To_String(Ada_output);
  end;

  trace: constant Boolean:= False;

  procedure Hep( what: String ) is
  pragma Inline(Hep);
  begin
    if trace then
      Put_Line( Standard_Error, "[DI]: " & what );
    end if;
  end Hep;

  procedure Internal_Error( what: String ) is
  begin
    Put_Line( Standard_Error, "p2ada DI internal error: " & what );
  end;

  procedure Warning( what: String ) is
    ln: constant String:= Integer'Image(PascalHelp.Current_line);
    cn: constant String:= Integer'Image(PascalHelp.Current_column);
  begin
    Put_Line( Standard_Error,
      To_String(Pascal_Name) & ':' &
      ln(ln'First+1..ln'Last) & ":" &
      cn(cn'First+1..cn'Last) & ": " &
      what
    );
  end Warning;

  type Idt( id_length: Natural; kind: Idkind ) is record
    name, cased_name: String(1..id_length);
    typ: Natural;
    nxt: Natural; -- linked list for fields
    Ada_alias: Natural:= 0;
    -- another identifer, hidden, with the Ada name of a simple type
  end record;

  type p_Idt is access Idt;
  procedure Dispose is new Ada.Unchecked_Deallocation(Idt, p_Idt);

  no_ident: constant:= 0;
  idt_stack: array(1..10000) of p_Idt;
  idt_top: Natural:= 0;

  type Tpkind is
    ( Simple, Arrays, Records, Pointer, Incomplete, File,
      Method -- OO
    );

  package TpK_IO is new Enumeration_IO(TpKind); use TpK_IO;
  package IdK_IO is new Enumeration_IO(IdKind); use IdK_IO;

  no_type: constant:= 0;

  type Typ(Kind: Tpkind) is record
    I: Natural:= no_ident; -- redundant info, to search faster.
    case Kind is
      when Simple  =>         null;
      when Arrays  =>         elemtip: Natural:= no_type;
                              -- ^ a priori, element type unknown
                              dim_1  : Boolean:= True;
      when Records =>         field1 : Natural:= no_ident;
                              -- ^ a priori, empty record
                              parent : Natural:= no_type;
                              -- ^ for OO
      when File | Pointer =>  pointed: Natural:= no_type;
                              -- ^ a priori, pointed type unknown
      when Incomplete => null;
      when Method =>        meth_id: Natural:= no_ident;
    end case;
  end record;

  type p_Typ is access Typ;
  procedure Dispose is new Ada.Unchecked_Deallocation(Typ, p_Typ);

  typ_stack: array(1..10000) of p_Typ;
  typ_top: Natural:= 0;

  function Rich_Image_of_type( T: Natural ) return String is
    base: constant String:= "Type #" & integer'image(T);
  begin
    if T = no_type then
      return base;
    elsif typ_stack(T) = null then
      Internal_Error( "Type #" & integer'image(T) & " is NULL" );
    else
      declare
        base2: constant String:= base & " of kind " &
                        TpKind'image(typ_stack(T).kind);
      begin
        if typ_stack(T).I = no_ident then
          return base2 & " (anonymous)";
        elsif idt_stack( typ_stack(T).I ) = null then
          Internal_Error( "Ident Nr" &
            integer'image(typ_stack(T).I) & " is NULL" );
        else
          return base2 & " named " & idt_stack( typ_stack(T).I ).Cased_Name;
        end if;
      end;
    end if;
    return base;
  end Rich_Image_of_type;

  type level_info is record
    idt, typ: Natural;
  end record;

  level_stack: array(1..1000) of level_info;
  lev: Natural:= 0;

  Too_low_level, Too_many_levels,
  Too_many_definitions, Too_many_types,
  Type_too_complicated : exception;

  -- Enter a new declaration.

  procedure Enter(Id: String; K: Idkind; T: Integer) is
  begin
    if idt_top = idt_stack'last then raise Too_many_definitions; end if;
    idt_top:= idt_top+1;
    -- Duplicate identifier NOT checked.
    idt_stack(idt_top):= new Idt'((id'length,k,To_Upper(Id),Id,T,0,0));
    Hep("Enter Ident Nr" & Integer'image(idt_top) & ", '" &
        Id & "' of kind " & Idkind'image(K));
  end Enter;

  procedure Enter_with_Alias(Id,Al,PfAl: String; K: Idkind; T: Integer) is
  begin
    Enter(Id, K, T);
    if Al /= "" then
      Enter(Al, Alias, T);
      idt_stack( idt_top-1 ).Ada_alias:= idt_top;
      if PfAl /= "" then -- Postfixed alias
        Enter(PfAl, Alias, T);
        idt_stack( idt_top-1 ).Ada_alias:= idt_top;
      end if;
    end if;
  end Enter_with_Alias;

  -- Search a definiton

  type IdKind_set is array( IdKind ) of Boolean;

  visible: constant IdKind_set:= ( Field| Alias => False, others=> True);

  function Position(id: String) return Integer is
    u_id: constant String:= To_Upper(id);
  begin
    for i in reverse 1..idt_top loop
      if visible(idt_stack(i).kind) and then
         idt_stack(i).name = u_id then
        return i;
      end if;
    end loop;
    return no_ident;
  end Position;

  function Alias_meaningful return Boolean;

  -- Find the eventual Ada alias of a predefined Pascal identifier
  function Find_alias( Pascal: String ) return String is
    i: constant Natural:= Position( Pascal );
    a: Natural;
  begin
    if i=0 or not Alias_meaningful then      -- identifier not found
      return Pascal;
    else             -- identifier found
      a:= idt_stack(i).Ada_alias;
      if a=0 then    -- no alias
        if P2Ada_options.correct_identifier_casing then -- rectify casing
          return idt_stack(i).cased_name;
        else                                    -- let casing as original
          return Pascal;
        end if;
      else           -- identifer is Adaliased
        return idt_stack(a).cased_name;
      end if;
    end if;
  end Find_alias;

  function Find_Postfixed_alias( Pascal: String ) return String is
    i: constant Natural:= Position( Pascal );
    a: Natural;
  begin
    if i=0 or not Alias_meaningful then      -- identifier not found
      return "";
    else             -- identifier found
      a:= idt_stack(i).Ada_alias;
      if a=0 then    -- no alias
        return "";
      else           -- identifer is Adaliased
        a:= idt_stack(a).Ada_alias; -- Take 2nd alias.
        if a=0 then
          return "";
        else
          return idt_stack(a).cased_name;
        end if;
      end if;
    end if;
  end Find_Postfixed_alias;

  -- Enter a new type element (part of a denoter).

  procedure Enter_type(id: Natural; kind: Tpkind) is
  begin
    if typ_top = typ_stack'last then raise Too_many_types; end if;
    typ_top:= typ_top+1;
    typ_stack(typ_top):= new Typ(kind);
    typ_stack(typ_top).I:= id;
    Hep("Enter Type #" & Integer'image(typ_top) &
        " of kind " & Tpkind'image(kind));
  end Enter_type;

  incompletes, in_pointer_def: Boolean:= False;
  type_mark: Natural:= no_type;

  procedure Create_incomplete_types( start: Boolean ) is
    i,t: Natural;
  begin
    incompletes:= start;
    if start then
      type_mark:= typ_top;
    else -- scan for still incomplete types
      for pt in type_mark .. typ_top loop
      -- we scan forward, complete types are naturally found backwards
        if typ_stack(pt).kind = pointer then -- a pointer type
          t:= typ_stack(pt).pointed;
          if t /= no_type then
            if typ_stack(t).kind = incomplete then
              i:= Position( idt_stack(typ_stack(t).I).name );
              -- the right ident, thnaks to which we will catch the complete type
              if i = no_ident then
                Warning("Undefined type in pointer definition (" &
                        idt_stack(typ_stack(t).I).name & ").");
                typ_stack(pt).pointed:= no_type;
              elsif idt_stack(i).kind /= tipe then
                Warning("Pointer type to a non type " &
                        idt_stack(typ_stack(t).I).name & ") ?");
                typ_stack(pt).pointed:= no_type;
              else
                Hep("For pointer " & Rich_Image_of_type(pt));
                Hep("  > replaced " & Rich_Image_of_type(t));
                Hep("  > with " & Rich_Image_of_type(idt_stack(i).typ));
                typ_stack(pt).pointed:= idt_stack(i).typ;
              end if;
            end if;
          end if;
        end if;
      end loop;
    end if;
  end Create_incomplete_types;

  ----------------------------------
  -- Construction of type denoter --
  ----------------------------------

  type Mark_pair is array(1..2) of Natural; -- 1: start, 2: stop

  type denoter_info is record
    T            : Natural; -- The type
    Array_1st_dim: Boolean; -- Is it the first dimension of an array ?
    ident_mark   : Mark_pair;
    -- ^ To recall the previous identifier mark in nested records
  end record;

  zero_denoter: constant denoter_info:= (no_type,False,(0,0));

  denoter_stack: array(0..100) of Denoter_info:= (others=> zero_denoter);
  denoter_top: Natural:= 0;

  root_of_last_type_denoter: Natural renames denoter_stack(0).T;

  -- Some denoters will end with no type, for various reasons...
  procedure Clear_type_denoter is
  begin
    denoter_stack( denoter_top ) := zero_denoter;
  end;

  procedure Type_identifier( name: String; incomplete_found: out Boolean; is_method: Boolean:= False ) is
    I,T: Natural;
  begin
    I:= Position( name );
    incomplete_found:= False;
    if I = 0 then
      if is_method then
        Enter_type(idt_top, Method);
        T:= typ_top;
      elsif in_pointer_def and incompletes then
        incomplete_found:= True;
        -- Undefined identifier only allowed in this context in Pascal
        Enter(name,alias,0); -- provisory identifier set as alias - harmless!
        Enter_type(idt_top,Incomplete);
        T:= typ_top;
      else
        Warning( "Type identifier " & name & " not found" );
        T:= 0; -- This is a permissive non-compiler
      end if;
    elsif idt_stack(I).kind /= tipe then
      T:= 0; -- We pass...
      Warning( name & " is not the name of a type but of a " &
               idkind'image(idt_stack(I).kind));
    else
      T:= idt_stack(I).typ;
    end if;

    denoter_stack( denoter_top ).T := T;

  end Type_identifier;

  the_Boolean_type, the_Char_type, the_String_type,
  the_Integer_type, the_Real_type: Natural;

  procedure Denoter_is_String is
  begin
    Enter_Type( no_ident, arrays );
    denoter_stack( denoter_top ):= (typ_top, False, (0,0));
    typ_stack( typ_top ).Elemtip:= the_Char_type;
    Hep("Type #" & integer'image(typ_top) & " is a String");
  end Denoter_is_String;

  procedure Nest_denoter is
  begin
    if denoter_top = denoter_stack'last then
      raise Type_too_complicated;
    else
      denoter_top:= denoter_top + 1;
      Clear_type_denoter;
    end if;
  end;

  procedure Unnest_denoter is
  begin
    if denoter_top=0 then
      raise Too_low_level;
    else
      denoter_top:= denoter_top - 1;
    end if;
  end;

  procedure Open_array_dim( is_first: Boolean ) is
  begin
    Enter_Type( no_ident, arrays );
    typ_stack(typ_top).dim_1:= is_first;
    denoter_stack( denoter_top ):= (typ_top, is_first, (0,0));
    Nest_denoter;
  end Open_array_dim;

  procedure Close_array_def is
    a,e: Natural;
  begin
    loop
      e:= denoter_stack( denoter_top ).T; -- element type
      Unnest_denoter;
      -- We complete the 1D array definition by linking it to
      -- its element type.
      a:= denoter_stack( denoter_top ).T; -- the array type
      typ_stack( a ).Elemtip:= e;
      Hep("Type #" & integer'image(a) &
          " is ARRAY of " & Rich_Image_of_type(e));
      exit when denoter_stack( denoter_top ).array_1st_dim;
    end loop;
  end Close_array_def;

  procedure Open_record_def is
  begin
    Enter_Type( no_ident, records );
    denoter_stack( denoter_top ):= (typ_top, False, (0,0));
    Nest_denoter;
  end Open_record_def;

  procedure Close_record_def is
    f,r: Natural;
  begin
    Unnest_denoter;
    r:= denoter_stack( denoter_top ).T; -- the record type
    Hep("Type #" & integer'image(r) &
        " is a RECORD. Fields are:");
    f:= typ_stack(r).field1;
    while f /= 0 loop
      Hep("  Id" & integer'image(f) &
        " named " & idt_stack(f).name &
        " and of " & Rich_Image_of_type(idt_stack(f).typ));
      f:= idt_stack(f).nxt;
    end loop;
    Hep("Parent object is: " & Rich_Image_of_type(typ_stack(r).parent));
  end Close_record_def;

  procedure Open_pointer_def is
  begin
    Enter_Type( no_ident, pointer );
    denoter_stack( denoter_top ):= (typ_top, False, (0,0));
    Nest_denoter;
    in_pointer_def:= True;
  end Open_pointer_def;

  procedure Close_pointer_def is
    ptr,ptd: Natural;
  begin
    in_pointer_def:= False;
    ptd:= denoter_stack( denoter_top ).T; -- the pointed
    Unnest_denoter;
    ptr:= denoter_stack( denoter_top ).T; -- the pointer
    typ_stack(ptr).Pointed:= ptd;
    if ptd = no_type then
      Hep("Type #" & integer'image(ptr) &
          " is a POINTER to an unknown type");
    else
      Hep("Type #" & integer'image(ptr) &
          " is a POINTER to " & Rich_Image_of_type(ptd));
    end if;
  end Close_pointer_def;

  -- +/- a copy of the pointer stuff.

  procedure Open_file_def is
  begin
    Enter_Type( no_ident, file );
    denoter_stack( denoter_top ):= (typ_top, False, (0,0));
    Nest_denoter;
  end Open_file_def;

  procedure Close_file_def is
    ptr,ptd: Natural;
  begin
    ptd:= denoter_stack( denoter_top ).T; -- the pointed
    Unnest_denoter;
    ptr:= denoter_stack( denoter_top ).T; -- the pointer
    typ_stack(ptr).Pointed:= ptd;
    if ptd = no_type then
      Hep("Type #" & integer'image(ptr) &
          " is a FILE of an unknown type");
    else
      Hep("Type #" & integer'image(ptr) &
          " is a FILE of " & Rich_Image_of_type(ptd));
    end if;
  end Close_file_def;

  ----

  identifier_mark: Natural:= 0;
  procedure Set_identifier_mark is
  begin
    identifier_mark:= idt_top;
    -- > The variables with identifiers [identifier_mark+1 .. def_top]
    --   are to be typised when the ':' is reached.
  end Set_identifier_mark;

  procedure Set_field_mark( nr: Positive ) is
  begin
    denoter_stack( denoter_top - 1 ).ident_mark( nr ):= idt_top;
    -- > the RECORD fields are remembered
  end Set_field_mark;

  -- For both VARiables and RECORD fields

  procedure Give_variables_a_type is
    T: constant Natural:= denoter_stack( denoter_top ).T;
    R: p_Typ;
    start, stop : Natural;
    in_record: constant Boolean:= denoter_top > denoter_stack'first;
    f,n: Natural;
    -- We are inside (at least) a record
  begin
    if in_record then
      R:= typ_stack( denoter_stack( denoter_top - 1 ).T);
      start:= denoter_stack( denoter_top - 1 ).ident_mark(1)+1;
      stop := denoter_stack( denoter_top - 1 ).ident_mark(2);
    else -- this is list of variables to type
      start:= identifier_mark + 1;
      stop := idt_top;
      -- Caution, there can be anonymous record fields up to
      -- there, after the variables !
    end if;

    for i in start .. stop loop
      exit when (not in_record) and idt_stack(i).kind = Field;
      Hep(idkind'image(idt_stack(i).kind) & ' ' &
          idt_stack(i).name &
          " is given " & Rich_Image_of_type(T));
      idt_stack(i).typ:= T;

      if in_record then
        if R.kind /= records then
          Internal_Error("Not a record, but a " & tpkind'image(R.kind));
        end if;
        if i=start then
          if R.field1 = no_ident then -- We link to the first
            R.field1:= i;
          else -- Already fields in this record
            f:= R.field1;
            loop
              n:= idt_stack( f ).nxt;
              exit when n = no_ident;
              f:= n;
            end loop;
            idt_stack( f ).nxt:= i; -- we link last to the new ones
          end if;
        end if;
        if i < stop then
          idt_stack(i).nxt:= i + 1;
        else
          idt_stack(i).nxt:= no_ident;
        end if;
      end if; -- in_record

    end loop;
  end Give_variables_a_type;

  -- Definition of a named type

  type_identifier_mark: Natural:= no_ident;
  procedure Set_type_identifier_mark is
  begin
    type_identifier_mark:= idt_top;
    -- > the TYPE with identifier type_identifier_mark is remembered
  end;

  procedure Associate_new_type_to_denoter is
    T: constant Natural:= root_of_last_type_denoter;
  begin
    Hep("Type Ident " & idt_stack(type_identifier_mark).name &
        " is given " & Rich_Image_of_type(T));
    idt_stack(type_identifier_mark).typ:= T;
    if T /= no_type then
      if typ_stack(T).I = no_ident then -- T might be an already named type !
        typ_stack(T).I:= type_identifier_mark;
      end if;
    end if;
  end Associate_new_type_to_denoter;

  ---------------
  -- Selectors --
  ---------------

  type Selector_info is record
    identifier_count: Natural:= 0;
    type_selected: Natural:= No_Type;
  end record;

  selector_stack: array(0..100) of Selector_info;
  selector_top: Natural:= 0;

  -- OO: "TYPE Y = OBJECT(X)" :

  procedure Link_parent_of_object is
    s: Selector_info renames selector_stack(selector_top);
    tparent: constant Natural:= s.type_selected;
    tobject: constant Natural:= denoter_stack( denoter_top-1 ).T;
  begin
    if tparent = no_type then
      Warning("TYPE Y = OBJECT(X), the type X inherited from is unknown");
    else
      if typ_stack( tparent ).kind /= records then
        Warning("The type inherited is not a record type but " &
                Rich_Image_of_type(tparent));
      end if;
      typ_stack( tobject ).parent:= tparent;
      Hep(Rich_Image_of_type(tobject) & " has the parent: " &
          Rich_Image_of_type(tparent));
    end if;
  end Link_parent_of_object;

  -- Inside record fields, "Adalias" are wrong but the fields
  -- not caught as visible identifiers !

  function Alias_meaningful return Boolean is
  begin
    return selector_stack(selector_top).identifier_count <= 1;
  end;

  procedure Clear_Selection is
  begin
    selector_stack(selector_top):= (0, No_Type);
    Hep("Clear_selection");
  end;

  procedure Reset_selection is
  begin
    selector_top:= 0;
    Clear_Selection;
  end;

  procedure Stack_selection is
  begin
    selector_top:= selector_top + 1;
    Clear_Selection;
  end;

  procedure Destack_selection is
  begin
    selector_top:= selector_top - 1;
    Hep("Destacked_selection, now back to " &
        Rich_Image_of_type( selector_stack(selector_top).type_selected ) );
  end;

  function Lost_in_selection return Boolean is -- at least one undefined type
  begin
    return selector_stack(selector_top).type_selected = no_type;
  end;

  procedure Give_last_function_its_type is
    I: constant Natural:= level_stack(lev).idt;   -- identifier of the function
    T: constant Natural:= selector_stack(selector_top).type_selected; -- the type
  begin
    idt_stack( I ).Typ:= T;
    Hep("Function " & idt_stack( I ).name &
        " has the " & Rich_Image_of_type( T ));
  end;

  function Name_of_type_selected return String is
    i,a: Natural;
  begin
    if Lost_in_selection then
      return "";
    else
      i:= typ_stack(  selector_stack(selector_top).type_selected ).I;
      if i = no_ident then
        return "";
      else
        a:= idt_stack(i).Ada_alias;
        if a=no_ident then    -- no alias
          return idt_stack(i).cased_name;
        else           -- identifer is Adaliased
          return idt_stack(a).cased_name;
        end if;
      end if;
    end if;
  end Name_of_type_selected;

  function Is_type_selected_a_file return File_typing is
  begin
    if Lost_in_selection then
      return no_idea;
    elsif typ_stack(  selector_stack(selector_top).type_selected ).Kind = File then
      return is_file;
    else
      return is_no_file;
    end if;
  end Is_type_selected_a_file;

  procedure Add_WITH_variables( prefix: String; is_record: out Boolean ) is
    t: Natural:= selector_stack(selector_top).type_selected;
    f: Natural;
  begin
    is_record:= False;
    if t /= no_type then
      if typ_stack(t).kind /= records then
        Warning("Variable of WITH, of " & Rich_Image_of_type(t) &
                " doesn't seem to be a RECORD");
      else
        is_record:= True;
        f:= typ_stack(t).field1;
        overall_fields: loop
          -- No (more) fields: try to jump to an ancestor with fields
          while f = no_ident and then typ_stack(t).parent /= no_type loop
            t:= typ_stack(t).parent; -- OO : Go on with the parent type
            f:= typ_stack(t).field1;
          end loop;
          exit when f = no_ident;
          while f /= no_ident loop
            -- We open the direct visibility of each field
            -- by adding a fake variable with its name !
            Enter_with_Alias(
              idt_stack( f ).Cased_name,                -- the field name
              prefix & '.' & idt_stack( f ).Cased_name, -- its Adalias
              "",
              Varbl,
              idt_stack( f ).typ
            );
            f:= idt_stack( f ).nxt;
          end loop;
        end loop overall_fields;
      end if;
    end if;
  end Add_WITH_variables;

  procedure Select_identifier( id: String ) is
    i: Natural;
    f: Natural;
    s: Selector_info renames selector_stack(selector_top);
  begin
    s.identifier_count:= s.identifier_count + 1;
    if s.identifier_count = 1 then  -- <---- HEAD identifier
      i:= Position( id );
      if i=0 then
        Hep("Tried to select '" & id & "', nothing found!");
      else
        s.type_selected:= idt_stack( i ).typ;
        Hep("Selected '" & id & "', of " &
            Rich_Image_of_type( s.type_selected )
        );
      end if;
    elsif  s.type_selected = no_type then
      Hep("Tried to select '" & id &
          "', but already lost (no previous type!)");
    elsif typ_stack(  s.type_selected ).kind /= records then
      Warning("Selector/qualifier '" & id & "' not applied to a record type but to " &
              Rich_Image_of_type(s.type_selected));
    else -- This time, we are in a record context
      declare
        uid: constant String:= To_Upper(id);
        t: Natural renames s.type_selected;
      begin
        Hep("In record type " &
            Rich_Image_of_type( t ) &
            ", trying to select " & id &
            ". Parent object is: " &
            Rich_Image_of_type(typ_stack(t).parent)
        );
        f:= typ_stack( t ).field1;
        overall_fields: loop
          -- No (more) fields: try to jump to an ancestor with fields
          while f = no_ident and then typ_stack(t).parent /= no_type loop
            Hep("Selection in object: skipping from " &
                Rich_Image_of_type(t) & " to the parent: " &
                Rich_Image_of_type(typ_stack(t).parent));
            t:= typ_stack(t).parent; -- OO : Go on with the parent type
            f:= typ_stack(t).field1;
          end loop;
          exit when f = no_ident;
          while f /= no_ident loop
            if idt_stack( f ).name = uid then
              t:= idt_stack( f ).typ;
              Hep("\--> Selection successful, field is of type " &
                  Rich_Image_of_type( t ));
              return;
            end if;
            f:= idt_stack( f ).nxt;
          end loop;
        end loop overall_fields;
        Warning("Selector/qualifier '" & id &
                "' is not in record type " &
                Rich_Image_of_type( t ) );
        t:= no_type; -- We are lost
      end;
    end if;
  end Select_identifier;

  procedure Select_pointed is
    s: Selector_info renames selector_stack(selector_top);
  begin
    if  s.type_selected = no_type then
      Hep("Tried to select object (^), but already lost (no previous type!)");
    elsif typ_stack( s.type_selected ).kind = file then
      null; -- Case of file buffer f^ (ISO, "classic" Pascal)
    elsif typ_stack( s.type_selected ).kind /= pointer then
      Warning("Selector/qualifier '^' not applied to a pointer type but to " &
              Rich_Image_of_type(s.type_selected));
    else -- This time, we are in a pointer context
      Hep("In pointer type " &
          Rich_Image_of_type( selector_stack(selector_top).type_selected) &
          "...");
      s.type_selected:= typ_stack(  s.type_selected ).pointed;
      Hep("   ... object type " &
          Rich_Image_of_type( s.type_selected ) &
          " selected.");
    end if;
  end Select_pointed;

  last_was_a_1st_dim: Boolean;

  procedure Select_one_dimension is
    s: Selector_info renames selector_stack(selector_top);
  begin
    if s.type_selected = no_type then
      Hep("Tried to open an array dimension, but already lost (no previous type!)");
      last_was_a_1st_dim:= True;
      -- Pro forma since we are lost, but is checked.
      -- Detected by GNAT's validity checking.
    elsif typ_stack( s.type_selected ).kind /= arrays then
      Warning("Selector/qualifier [...] not applied to an array type but to " &
              Rich_Image_of_type(s.type_selected));
    else -- This time, we are in an array context
      Hep("In array type " &
          Rich_Image_of_type( s.type_selected ) &
          "...");
      last_was_a_1st_dim:= typ_stack( s.type_selected ).dim_1;
      s.type_selected:= typ_stack( s.type_selected ).Elemtip;
      Hep("   ... element type " &
          Rich_Image_of_type( s.type_selected ) &
          " selected.");
    end if;
    Stack_selection;
  end Select_one_dimension;

  function First_dim_selected return Boolean is
  begin
    return last_was_a_1st_dim;
  end;

  procedure Close_one_dimension is
  begin
    Destack_selection;
  end;

  procedure Select_litteral( sort: Character ) is
    s: Selector_info renames selector_stack(selector_top);
  begin
    case sort is
      when 'C' => s.type_selected:= the_Char_type;
      when 'S' => s.type_selected:= the_String_type;
      when 'N' => s.type_selected:= the_Integer_type;
      when others => null;
    end case;
    Hep("Selected litteral of " & Rich_Image_of_type( s.type_selected ));
    if denoter_stack( denoter_top ).T = no_type then    -- 12-Feb-2003: for
      denoter_stack( denoter_top ).T:= s.type_selected; -- untyped constants
      Hep("Type denoter takes type of litteral"); -- whose type is determined
    end if;                                       -- by the litteral
  end;

  --------------------
  -- Input / Output --
  --------------------

  type Def_kind is (idents,types);
  type Def_stack_pointer is array(Def_kind) of Natural;

  -- Load items from a file. They are visible at top level.
  -- Mainly, they correspond to the "interface" of an Unit.

  procedure Load (file_name: String) is
    f: File_Type;
    s: String(1..300);
    l: Natural;
    start_imports, stop_imports, base, offset: Def_stack_pointer;
    tk: TpKind;
    ik: IdKind;

    procedure Relocate(k: Def_kind; idx: in out Natural) is
    begin
      if idx < start_imports(k) then -- reference to someth. before the imports
        if idx >= base(k) then
          Internal_error("Import falls in a gap !");
        end if;
      else
        idx:= idx + offset(k);
      end if;
    end Relocate;

    function Get_ref(k: Def_kind) return Natural is
      n: Natural;
    begin
      Get(f,n); Relocate(k,n); return n;
    end;

  begin
    begin
      Open(f, In_File, file_name);
    exception
      when Name_Error =>
        Put_Line( Standard_Error, "Definition file '" & file_name & "' not found." );
        raise;
    end;
    Skip_Line(f);
    base(types) := typ_top + 1;
    base(idents):= idt_top + 1;
    for k in Def_kind loop
      Get(f,start_imports(k));Skip_Line(f);
      Get(f,stop_imports(k));Skip_Line(f);
      offset(k):= base(k) - start_imports(k);
    end loop;
    Skip_Line(f);
    for t in start_imports(types) .. stop_imports(types) loop
      Get(f,l); -- should be = t
      l:= Get_ref(idents);
      Get(f,tk);
      Enter_type(l,tk);
      case tk is
        when Simple  =>  null;
        when Arrays  =>  typ_stack(typ_top).Elemtip:= Get_ref(types);
        when Records =>  typ_stack(typ_top).Field1 := Get_ref(idents);
                         typ_stack(typ_top).parent := Get_ref(types);
        when Pointer
             | File  =>  typ_stack(typ_top).Pointed:= Get_ref(types);
        when Incomplete => null;
        when Method => typ_stack(typ_top).meth_id:= Get_ref(idents);
      end case;
      Skip_Line(f);
    end loop;
    Skip_Line(f);
    for i in start_imports(idents) .. stop_imports(idents) loop
      Get(f,l); -- should be = i
      Skip_Line(f);
      Get_Line(f,s,l);
      Get(f,ik);
      Enter( s(1..l), ik, no_type);
      idt_stack(idt_top).typ:= Get_ref(types);
      idt_stack(idt_top).nxt:= Get_ref(idents);
      idt_stack(idt_top).Ada_alias:= Get_ref(idents);
      Skip_Line(f);
    end loop;
    Close(f);
  end Load;

  start_exports, stop_exports, predef: Def_stack_pointer;

  type p_String is access String;
  export_name: p_String:= null;
  export_done: Boolean:= False;

  procedure Will_save (file_name: String) is
  begin
    export_name:= new String'(file_name);
  end;

  procedure Stop_export is
    f: File_Type;
  begin
    stop_exports:= (idt_top,typ_top);
    if export_done or export_name = null then return; end if;
    begin
      Open(f, In_File, export_name.all);
      Close(f);
      Put_Line( Standard_Error, "File " & export_name.all &
                " already exists, no exports written." );
      return;
    exception
      when others => null; -- file doesn't exist, we are happy.
    end;
    Create(f, out_file, export_name.all);
    Put_Line(f,"-- Definition exports, file " & export_name.all);
    for k in Def_kind loop
      Put(f,start_exports(k));
      Put_Line(f, " -- first item in " & Def_kind'Image(k));
      Put(f,stop_exports(k));
      Put_Line(f, " -- last  item in " & Def_kind'Image(k));
    end loop;
    Put_Line(f,"-- Types: ");
    for t in start_exports(types) .. stop_exports(types) loop
      Put(f,t); -- redundant info, for human readers of the file
      declare
        tt: Typ renames typ_stack(t).all;
      begin
        Put(f,' '); Put(f,tt.i,0); Put(f,' '); Put(f,tt.kind); Put(f,' ');
        case tt.kind is
          when Simple  =>  null;
          when Arrays  =>  Put(f,tt.Elemtip,0);
          when Records =>  Put(f,tt.Field1,0); Put(f,' '); Put(f,tt.parent,0);
          when Pointer
               | File  =>  Put(f,tt.Pointed,0);
          when Incomplete => null;
          when Method => Put(f,tt.meth_id,0);
        end case;
        New_Line(f);
      end;
    end loop;
    Put_Line(f,"-- Identifiers: ");
    for d in start_exports(idents) .. stop_exports(idents) loop
      Put(f,d,0); -- redundant info, for human readers of the file
      declare
        dd: Idt renames idt_stack(d).all;
      begin
        New_Line(f);
        Put_Line(f,dd.cased_name);
        Put(f,dd.kind);
        Put(f,' '); Put(f,dd.typ,0); Put(f,' '); Put(f,dd.nxt,0);
        Put(f,' '); Put(f,dd.Ada_alias,0);
        New_Line(f);
      end;
    end loop;
    Close(f);
    export_done:= True;
  end Stop_export;

  -- Sets a "marker" before new, more local definitions,
  -- inside a subprogram.
  procedure Mark is
  begin
    if lev = level_stack'last then
      raise Too_many_levels;
    else
      lev:= lev + 1;
      level_stack(lev):= (idt=> idt_top, typ=> typ_top);
      Hep( "-> level" & Natural'image(lev) );
    end if;
  end Mark;

  -- Forget the definitions created after latest unreleased "Mark".
  procedure Release is
  begin
    if lev = 0 then
      raise Too_low_level;
      -- should never happen within P2Ada: the Pascal source
      -- would be malformed...
    else
      for i in level_stack(lev).idt+1 .. idt_top loop
        Dispose(idt_stack(i));
      end loop;
      idt_top:= level_stack(lev).idt;
      for t in level_stack(lev).typ+1 .. typ_top loop
        Dispose(typ_stack(t));
      end loop;
      typ_top:= level_stack(lev).typ;
      lev:= lev - 1;
      Hep( "<- level" & Natural'image(lev) );
    end if;
  end Release;

  procedure Enter_Simple_Type( name: String; alias_name: String:="" ) is
    id: constant Natural:= idt_top + 1; -- place of the Pascal identifier
  begin
    Enter_type( no_ident, Simple ); -- create a type without associated ident.
    -- create an identifier associated to the type + alias
    Enter_with_alias(name, alias_name, "", Tipe, typ_top);
    typ_stack(typ_top).I:= id; -- associate the type to the identifier
    --
    -- If we have THE types Integer or Char, we remember that.
    -- It's for getting the type of Pascal untyped constants with litterals
    -- like CONST A='a'; B='Baaaa'; C=1234;
    --
    if name = "Integer" then
      the_Integer_type:= typ_top;
    elsif name= "Char" then
      the_Char_type:= typ_top;
    elsif name= "Boolean" then
      the_Boolean_type:= typ_top;
    elsif name= "Real" then
      the_Real_type:=  typ_top;
    end if;
  end Enter_Simple_Type;

  procedure Enter_Pointer_or_File_Type(
    name, alias_name: String;
    T: Natural;
    K: TpKind)  is

    id: constant Natural:= idt_top + 1; -- place of the Pascal identifier
  begin
    Enter_type( no_ident, K ); -- create a type without associated ident.
    -- create an identifier associated to the type + alias
    Enter_with_alias(name, alias_name, "", Tipe, typ_top);
    typ_stack(typ_top).I:= id; -- associate the type to the identifier
    typ_stack(typ_top).Pointed:= T;
  end Enter_Pointer_or_File_Type;

  procedure Enter_String_Type( name: String; alias_name: String;
                               char_type: Natural ) is
    id: constant Natural:= idt_top + 1; -- place of the Pascal identifier
  begin
    Enter_type( no_ident, Arrays ); -- create a type without associated ident.
    -- create an identifier associated to the type + alias
    Enter_with_alias(name, alias_name, "", Tipe, typ_top);
    typ_stack(typ_top).I:= id; -- associate the type to the identifier
    typ_stack(typ_top).elemtip:= char_type;
    if name= "String" then
      the_String_type:=  typ_top;
    end if;
  end Enter_String_Type;

  procedure Classic_predefined is
  begin
    Enter_Simple_Type("Integer");
    Enter_with_Alias("Maxint", "Integer'last", "", Konst, the_Integer_type);

    Enter_Simple_Type("Char", "Character");
    Enter_String_Type("String", "", the_Char_type);   -- BP but quite common

    Enter_Simple_Type("Boolean");
    Enter("False", Konst, the_Boolean_type);
    Enter("True", Konst, the_Boolean_type);

    Enter_with_Alias("NIL", "null", "", Konst, 0);
    Enter_Simple_Type("Real", "Float");
    Enter_Pointer_or_File_Type("Text", "Ada.Text_IO.File_Type", the_Char_type, File);

    Enter_with_Alias("Chr",  "Character'Val", "", Funkt, the_Char_type);
    Enter_with_Alias("Ord",  "Character'Pos", "", Funkt, the_Integer_type);
    Enter_with_Alias("EoF",  "End_of_File", "", Funkt, the_Boolean_type);
    Enter_with_Alias("EoLn", "End_of_Line", "", Funkt, the_Boolean_type);
    Enter_with_Alias("Sqr", "(", "**2)", Funkt, the_Real_type);
    Enter_with_Alias("Ln", "Log", "", Funkt, the_Real_type); -- RM95: A.5.1
    Enter_with_Alias("Odd", "(", " mod 2 /= 0)", Funkt, the_Boolean_type);

    Enter_with_Alias("Halt", "raise Program_halted", "", Funkt, 0);
    Enter_with_Alias("Concat", """&""", "", Funkt, the_String_type);
  end Classic_predefined;

  procedure Borland_predefined is
    ac, wc: Natural;
  begin
    -- * Integer types
    Enter_Simple_Type("Shortint", "Integer_8");   -- Delphi 6
    Enter_Simple_Type("Smallint", "Integer_16");  -- Delphi 6
    Enter_Simple_Type("Longint",  "Integer_32");  -- BP
    Enter_with_Alias("MaxLongint", "Integer_32'last", "", Konst, 0);
    Enter_Simple_Type("Int64",    "Integer_64");  -- Delphi 6

    Enter_Simple_Type("Byte",     "Unsigned_8");   -- BP
    Enter_Simple_Type("Word",     "Unsigned_16");  -- BP
    Enter_Simple_Type("Cardinal", "Unsigned_32");  -- Delphi 6
    Enter_Simple_Type("Longword", "Unsigned_32");  -- Delphi 6
    Enter_Simple_Type("DWord",    "Unsigned_32");  -- Delphi 2005
    Enter_Simple_Type("QWord",    "Unsigned_64");  -- Delphi 2005

    -- * Avatars of Boolean type (BP 7 - Delphi 6)
    Enter_Simple_Type("ByteBool", "Boolean");
    Enter_Simple_Type("WordBool", "Boolean");
    Enter_Simple_Type("LongBool", "Boolean");

    -- * Floating point types
    Enter_Simple_Type("Real48");   -- FPU type in Delphi 6
    Enter_Simple_Type("Single");   -- FPU type in BP
    Enter_Simple_Type("Double", "Long_Float"); -- BP and "Standard"
    Enter_Simple_Type("Extended"); -- FPU type in BP
    Enter_Simple_Type("Comp");     -- FPU type in BP

    -- * Fixed point types
    Enter_Simple_Type("Currency"); -- Delphi 6

    -- * Pointer types
    Enter_Pointer_or_File_Type("Pointer", "", no_type, Pointer);  -- BP (untyped pointer!)
    Enter_Pointer_or_File_Type("PChar", "", the_Char_type, Pointer);    -- BP 7

    -- * Characters types - Ada95 RM 3.5.2, Delphi 6 Help
    Enter_Simple_Type("AnsiChar", "Character");
    ac:= typ_top;
    Enter_Simple_Type("WideChar", "Wide_Character");
    wc:= typ_top;

    -- * Strings types - Ada95 RM 3.6.3, Delphi 6 Help
    Enter_String_Type("ShortString", "String", the_Char_type);
    Enter_String_Type("AnsiString", "String", ac);
    Enter_String_Type("WideString", "Wide_String", wc);

    -- * Subprograms

    Enter_with_Alias("Break", "exit", "", Funkt, 0);
    Enter_with_Alias("Exit", "return", "", Funkt, 0);

    Enter_with_Alias("ParamStr", "Argument", "", Funkt, the_String_type);
    Enter_with_Alias("ParamCount", "Argument_Count", "", Funkt, the_Integer_type);

    Enter_with_Alias("Hi",   "((", "/256) mod 256)", Funkt, the_Integer_type);
    Enter_with_Alias("High", "(", "'last)", Funkt, 0);
    Enter_with_Alias("Int",  "Float'Floor", "", Funkt, the_Real_type);
    Enter_with_Alias("Length",  "(", "'length)", Funkt, the_Integer_type);
    Enter_with_Alias("Lo",   "(", " mod 256)", Funkt, the_Integer_type);
    Enter_with_Alias("Low", "(", "'first)", Funkt, 0);
    Enter_with_Alias("LowerCase", "To_Lower", "", Funkt, the_Char_type);
    --    Pos	Returns the index of the first character of a specified substring in a string.
    Enter_with_Alias("Pred", "Integer'Pred", "", Funkt, 0);
    --    Round	Returns the value of a real rounded to the nearest whole number.
    Enter_with_Alias("SizeOf", "(", "'size / 8)", Funkt, the_Integer_type);
    Enter_with_Alias("Succ", "Integer'Succ", "", Funkt, 0);
    Enter_with_Alias("Trunc",    "Integer", "", Funkt, the_Integer_type);
    Enter_with_Alias("UpCase",    "To_Upper", "", Funkt, the_Char_type);
    Enter_with_Alias("UpperCase", "To_Upper", "", Funkt, the_Char_type);
    Enter_with_Alias("StrUpper",  "To_Upper", "", Funkt, the_String_type);
  end Borland_predefined;

  procedure CodeWarrior_predefined is
    -- incomplete_found: Boolean;
  begin
    -- * Integer types
    Enter_Simple_Type("UnsignedByte", "Unsigned_8");
    Enter_Simple_Type("UnsignedWord", "Unsigned_16");
    Enter_Simple_Type("UnsignedLong", "Unsigned_32");
    -- 2007: From Think Pascal:
    Enter_String_Type("Str255", "String", the_Char_type);
    --
    -- This creates an elaboration circularity:
    --
    --    -- Explicit definition of Rect:
    --    --      type Rect = record
    --    --        left, top, right, bottom: ShortInt;
    --    --      end;
    --    Enter( "Rect", Tipe, 0);
    --    Set_type_identifier_mark;
    --    Clear_type_denoter;
    --    Open_record_def;
    --    Set_field_mark(1);
    --    Enter( "left", Field, 0);
    --    Enter( "top", Field, 0);
    --    Enter( "right", Field, 0);
    --    Enter( "bottom", Field, 0);
    --    Set_field_mark(2);
    --    Type_identifier("ShortInt",incomplete_found);
    --    Give_variables_a_type;
    --    Close_record_def;
    --    Associate_new_type_to_denoter;
    --    -- ^^^^^^^^^ end of Rect record
    --
    -- ...and a bit complicated anyway.
    --  Better to go with an import / export solution
    --
  end CodeWarrior_predefined;

  procedure Berkeley_predefined is
  begin
    Enter_with_Alias("Minint", "Integer'first", "", Konst, the_Integer_type);
    Enter_String_Type("Alfa", "String", the_Char_type);
    Enter_with_Alias("Minchar", "Character'first", "", Konst, the_Char_type);
    Enter_with_Alias("Maxchar", "Character'last", "", Konst, the_Char_type);
    Enter_with_Alias("Bell", "ASCII.BEL", "", Konst, the_Char_type);
    Enter_with_Alias("Tab",  "ASCII.HT", "", Konst, the_Char_type);
  end Berkeley_predefined;

  procedure Predefined_Pascal_Identifiers is
  begin
    Classic_predefined;

    -- It doesn't matter if there are more predefined
    -- types than in the considered Pascal dialect:
    -- if an identifier is not predefined in X-Pascal,
    -- a correct program for X-Pascal will define it and
    -- mask our predefined types and their Ada aliases.
    --
    -- So, we put a maximum of predefined items.

    Borland_predefined;
    CodeWarrior_predefined;
    Berkeley_predefined;

    Hep("--------[ End of predefined ]--------");

    predef:=  (idents=> idt_top+1, types=> typ_top+1);
    start_exports:= predef;

  -- exception
  --   when E: others =>
  --     New_Line;
  --     Put_Line("--------------------[ Unhandled exception ]-----------------");
  --     Put_Line(" > Name of exception . . . . .: " &
  --              Ada.Exceptions.Exception_Name(E) );
  --     Put_Line(" > Message for exception . . .: " &
  --              Ada.Exceptions.Exception_Message(E) );
  --     Put_Line(" > Trace-back of call stack: " );
  --     Put_Line( GNAT.Traceback.Symbolic.Symbolic_Traceback(E) );
  end Predefined_Pascal_Identifiers;

  function Typ_Position(id: String) return Integer is
    u_id: constant String:= To_Upper(id);
  begin
    for i in reverse 1..typ_top loop
      if visible(idt_stack(typ_stack(i).i).kind) and then
           idt_stack(typ_stack(i).i).name = u_id then
        return i;
      end if;
    end loop;
    return no_type;
  end Typ_Position;

   procedure Load_Alias (Alias_File_Name : String := "") is
      procedure Analyse (Line : String) is
         IndLine : Natural := Line'First;
         type Item is (Name, Tipe, Pascal, Ada, AdaPost);
         type Indexes is record
            First, Last : Natural:= 0;
         end record;
         Items : array (Item) of Indexes;
         IndItem : Item := Item'First;
         Stop, Text : Boolean := False;
         function Match_Line (From : Natural; Pat : String) return Boolean is
         begin
            if From = 0 then
               return False;
            end if;
            if From + Pat'Length - 1 > Line'Last then
               return False;
            end if;
            return To_Lower (Line(From .. From + Pat'Length - 1)) = Pat;
         end;
         function Slice_Line (From, To : Natural) return String is
         begin
            if From = 0 then
               return "";
            end if;
            return Line(From..To);
         end;
      begin
         if Line'Length /= 0 then
            loop
               case Line(IndLine) is
               when '-' =>
                  if IndLine < Line'last and then Line(Positive'Succ(IndLine)) = '-' then
                     if Items(IndItem).Last = 0 then Items(IndItem).Last := Positive'Pred(IndLine); end if;
                     Stop := True;
                  end if;
               when ',' =>
                  if Items(IndItem).Last = 0 then Items(IndItem).Last := Positive'Pred(IndLine); end if;
                  IndItem := Item'Succ(IndItem);
                  Text := false;
               when ' ' =>
                  if Text and then Items(IndItem).Last = 0 then Items(IndItem).Last := Positive'Pred(IndLine); end if;
               when others =>
                  Text := True;
                  Items(IndItem).Last := 0;
                  if Items(IndItem).First = 0 then Items(IndItem).First := IndLine; end if;
               end case;
               exit when Stop or else IndLine >= Line'Last;
               IndLine := Positive'Succ(IndLine);
            end loop;
            if Items(IndItem).Last = 0 then Items(IndItem).Last := IndLine; end if;
            if Match_Line (Items(Tipe).First, "simple") then
               Enter_Simple_Type (Slice_Line(Items(Name).First,Items(Name).Last),
                                  Slice_Line(Items(Pascal).First,Items(Pascal).Last));
            end if;
            if Match_Line (Items(Tipe).First, "arrays") then
               Enter_String_Type (Slice_Line(Items(Name).First,Items(Name).Last),
                                  Slice_Line(Items(Ada).First,Items(Ada).Last),
                                  Typ_Position (Slice_Line(Items(Pascal).First,Items(Pascal).Last)));
            end if;
            if Match_Line (Items(Tipe).First, "pointer") then
               Enter_Pointer_or_File_Type (Slice_Line(Items(Name).First,Items(Name).Last),
                                           Slice_Line(Items(Ada).First,Items(Ada).Last),
                                           Typ_Position (Slice_Line(Items(Pascal).First,Items(Pascal).Last)),
                                           Pointer);
            end if;
            if Match_Line (Items(Tipe).First, "file") then
               Enter_Pointer_or_File_Type (Slice_Line(Items(Name).First,Items(Name).Last),
                                           Slice_Line(Items(Ada).First,Items(Ada).Last),
                                           Typ_Position (Slice_Line(Items(Pascal).First,Items(Pascal).Last)),
                                           File);
            end if;
            if Match_Line (Items(Tipe).First, "konst") then
               Enter_with_Alias (Slice_Line(Items(Name).First,Items(Name).Last),
                                 Slice_Line(Items(Ada).First,Items(Ada).Last),
                                 Slice_Line(Items(AdaPost).First,Items(AdaPost).Last),
                                 Konst,
                                 Typ_Position (Slice_Line(Items(Pascal).First,Items(Pascal).Last)));
            end if;
            if Match_Line (Items(Tipe).First, "funkt") then
               Enter_with_Alias (Slice_Line(Items(Name).First,Items(Name).Last),
                                 Slice_Line(Items(Ada).First,Items(Ada).Last),
                                 Slice_Line(Items(AdaPost).First,Items(AdaPost).Last),
                                 Funkt,
                                 Typ_Position (Slice_Line(Items(Pascal).First,Items(Pascal).Last)));
            end if;
            if Match_Line (Items(Tipe).First, "proc") then
               Enter_with_Alias (Slice_Line(Items(Name).First,Items(Name).Last),
                                 Slice_Line(Items(Pascal).First,Items(Pascal).Last),
                                 "",
                                 Funkt,
                                 no_type);
            end if;
            if Match_Line(Items(Tipe).First, "with") and then
              not Match_Line(Items(Tipe).First, "withuse") then
               String_List.Append (Default_With_List, "with "
                                   & To_Unbounded_String(Line(Items(Name).First..Items(Name).Last))
                                   & ";");
            end if;
            if Match_Line(Items(Tipe).First, "withuse") then
               String_List.Append (Default_With_List, "with "
                                   & To_Unbounded_String(Line(Items(Name).First..Items(Name).Last))
                                   & ";");
               String_List.Append (Default_With_List, "use "
                                   & To_Unbounded_String(Line(Items(Name).First..Items(Name).Last))
                                   & ";");
            end if;
            if Match_Line(Items(Tipe).First, "option") then
               if Match_Line (Items(Name).First, "var") then
                  translation_of_VAR := VAR_wording'Value (Line(Items(Pascal).First..Items(Pascal).Last));
               end if;
               if Match_Line (Items(Name).First, "eol") then
                  new_line_endings := Line_endings'Value (Line(Items(Pascal).First..Items(Pascal).Last));
               end if;
               if Match_Line (Items(Name).First, "mod") then
                  translation_of_MOD := Mod_Rem'Value(Line(Items(Pascal).First..Items(Pascal).Last));
               end if;
            end if;
         end if;
      end Analyse;
      F : File_Type;
   begin
      if Alias_File_Name = "" then
         Put_Line( Standard_Error, "Internal aliases loaded." );
         Predefined_Pascal_Identifiers;
      else
         begin
            Open (F, In_File, Alias_File_Name);
         exception
            when Name_Error =>
               Put_Line( Standard_Error, "Alias file '" & Alias_File_Name & "' not found, internal aliases loaded." );
               Predefined_Pascal_Identifiers;
               return;
         end;
         Put_Line( Standard_Error, "Aliases loaded from file '" & Alias_File_Name & "'.");
         while not End_Of_File (F) loop
            Analyse (Get_Line (F));
         end loop;
         close (F);
      end if;
   end Load_Alias;

end P2Ada_Definition_info;
