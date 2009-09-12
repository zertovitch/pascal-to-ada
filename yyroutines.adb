-- $Id: yyroutines.adb,v 2.1 1997/08/24 08:03:54 nestor Rel $
-- $Locker:  $
-- $Log: yyroutines.adb,v $
-- Revision 2.1  1997/08/24 08:03:54  nestor
-- Laurent Gasser's correction for MacOS extended Pascal
--
-- Revision 1.1  1997/08/23  08:57:20  nestor
-- Martin C. Carlisle's original version, standard Pascal
--
WITH pascalyylex;
WITH pascal_tokens;
USE pascal_tokens;
PACKAGE BODY yyroutines IS

Lookahead : Token;
HaveLookahead : Boolean := False;
SecondUnYYLex : exception;

FUNCTION YYLex RETURN Token IS
BEGIN
  IF HaveLookahead THEN
    HaveLookahead := False;
    RETURN Lookahead;
  ELSE
    RETURN pascalyylex;
  END IF;
END YYLex;

PROCEDURE UnYYLex(tok : Token) IS
BEGIN
  IF HaveLookahead THEN
    RAISE SecondUnYYLex;
  ELSE
    HaveLookahead := True;
    Lookahead := tok;
  END IF;
END UnYYLex;

END yyroutines;
