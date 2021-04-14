
with pascal_dfa; use pascal_dfa;
package body pascal_dfa is
--  Warning: This file is automatically generated by AFLEX.
--  *******   It is useless to modify it. Change the ".Y" & ".L" files instead.

      --  Nov 2002. Fixed insufficient buffer size bug causing
      --  damage to comments at about the 1000-th character

   function YYText return String is
      J          : Integer := yytext_ptr;
   begin
      while J <= yy_ch_buf'Last and then yy_ch_buf (J) /= ASCII.NUL loop
         J := J + 1;
      end loop;

      declare
         subtype Sliding_Type is String (1 .. J - yytext_ptr);
      begin
         return Sliding_Type (yy_ch_buf (yytext_ptr .. J - 1));
      end;
   end YYText;

      --  Returns the length of the matched text

   function YYLength return Integer is
   begin
      return yy_cp - yy_bp;
   end YYLength;

      --  Done after the current pattern has been matched and before the
      --  corresponding action - sets up yytext

   procedure YY_DO_BEFORE_ACTION is
   begin
      yytext_ptr   := yy_bp;
      yy_hold_char := yy_ch_buf (yy_cp);
      yy_ch_buf (yy_cp) := ASCII.NUL;
      yy_c_buf_p := yy_cp;
   end YY_DO_BEFORE_ACTION;

end pascal_dfa;
