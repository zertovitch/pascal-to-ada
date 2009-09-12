package P2Ada_options is

  -- Case of keywords.
  type Casing is (lower,upper,neutral);
  keyword_casing: constant Casing:= lower;

  -- When True, P2Ada "rectifies" the casing of a non-masked
  -- predefined identifier with the same name in Pascal as in Ada. 
  correct_identifier_casing: constant Boolean:= False;

  add_translation_comments: constant Boolean:= True;
  
  -- Translating, for subprogram parameters, "var" by "var" instead
  -- of "in out" helps the post-processing (the Ada compiler
  -- stops on each "var"). Idem for Turbo Pascal 7+'s "const".
  type VAR_wording is (var, in_out);
  translation_of_VAR: constant VAR_wording:= in_out;

  -- Line endings  
  type Line_endings is (Unix,DOS,Mac); -- LF,CR/LF,CR
  new_line_endings : constant Line_endings:= DOS;
  
  -- Translation of MOD.
  -- For ISO Pascal, it is MOD, for Think Pascal, it is REM.
  type Mod_Rem is (is_mod, is_rem);
  translation_of_MOD: constant Mod_Rem:= is_mod;
  
end P2Ada_options;