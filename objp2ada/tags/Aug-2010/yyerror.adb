-- $Id: yyerror.adb,v 2.1 1997/08/24 07:50:27 nestor Rel $
-- $Locker:  $
-- $Log: yyerror.adb,v $
-- Revision 2.1  1997/08/24 07:50:27  nestor
-- Laurent Gasser's correction for MacOS extended Pascal
--
-- Revision 1.1  1997/08/23  08:39:16  nestor
-- Martin C. Carlisle's original version, standard Pascal
--
with ada.text_io;
procedure YYError (S : IN String) IS
  syntax_error : exception;
begin
  ada.text_IO.put_line(s);
  raise syntax_error;
end YYError;
