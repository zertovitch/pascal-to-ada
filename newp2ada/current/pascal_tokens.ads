package Pascal_Tokens is


  type const_type is (string_type,character_type,boolean_type,other_type);
  type YYSType is record
     text    : String(1..80);
     length  : Natural := 0;
     vartype : const_type;
  end record;

    YYLVal, YYVal : YYSType;

    type Token is
        (End_Of_Input, Error, Absolute_T, And_T,
         And_Then_T, Ampersand_T, Array_T,
         Arrow_T, Ascii_T, Assign_T,
         At_T, Bar_T, Begin_T,
         Case_T, Char_Const_T, Chr_T,
         Colon_T, Const_T, Constant_T,
         Constructor_T, Comma_T, Dec_T,
         Destructor_T, Div_T, Divide_T,
         Do_T, Doubledot_T, Doublestar_T,
         Downto_T, Else_T, End_T,
         Eof_T, Eoln_T, Equal_T,
         External_T, File_T, For_T,
         Forward_T, Function_T, Ge_T,
         Goto_T, Gt_T, Hexadecimal_T,
         Id_T, If_T, Implementation_T,
         In_T, Inc_T, Inherited_T,
         Interface_T, Label_T, Lbrack_T,
         Le_T, Lparen_T, Lt_T,
         Minus_T, Mod_T, Ne_T,
         New_T, Not_T, Object_T,
         Of_T, Or_T, Or_Else_T,
         Ord_T, Otherwise_T, Override_T,
         Packed_T, Period_T, Plus_T,
         Private_T, Procedure_T, Program_T,
         Protected_T, Public_T, Rbrack_T,
         Record_T, Read_T, Readln_T,
         Repeat_T, Rparen_T, Semicolon_T,
         Set_T, Shl_T, Shr_T,
         Str_T, String_T, Then_T,
         Times_T, To_T, Type_T,
         Unit_T, Until_T, Uparrow_T,
         Uses_T, Var_T, Virtual_T,
         While_T, With_T, Write_T,
         Writeln_T, Xor_T, Try_T,
         Except_T, Finally_T );

    Syntax_Error : exception;

end Pascal_Tokens;
