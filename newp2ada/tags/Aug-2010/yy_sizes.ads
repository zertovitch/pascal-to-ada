-- 14-Jan-2006: this package sets values wich were previously
--              hardcoded in the the ayacc-generated yyparse

package yy_sizes is

   -- the size of the value and state stacks
   --  Affects error 'Stack size exceeded on state_stack'

   stack_size : constant Natural := 8*8192;

end yy_sizes;
