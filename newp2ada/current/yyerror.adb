-- $Id: yyerror.adb,v 2.1 1997/08/24 07:50:27 nestor Rel $
-- $Locker:  $
-- $Log: yyerror.adb,v $
-- Revision 2.1  1997/08/24 07:50:27  nestor
-- Laurent Gasser's correction for MacOS extended Pascal
--
-- Revision 1.1  1997/08/23  08:39:16  nestor
-- Martin C. Carlisle's original version, standard Pascal
--

with Pascal_IO;
with Ada.Text_IO;

procedure YYError (S : in String) is
  syntax_error : exception;
begin
  Ada.Text_IO.Put_Line (S);
  Ada.Text_IO.New_Line;
  Ada.Text_IO.Put_Line
    ("Syntax error in Pascal source, line number:" &
     Pascal_IO.Input_Line'Image);
  --  ^ NB: the function `Pascal_IO.Input_Line` is generated
  --  or is working correctly *only* when AFlex has been called
  --  with the `-E` option.
  raise syntax_error;
end YYError;
