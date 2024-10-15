-- $Id: yyroutines.ads,v 2.1 1997/08/24 08:08:55 nestor Rel $
-- $Locker:  $
-- $Log: yyroutines.ads,v $
-- Revision 2.1  1997/08/24 08:08:55  nestor
-- Laurent Gasser's correction for MacOS extended Pascal
--
-- Revision 1.1  1997/08/23  08:53:02  nestor
-- Martin C. Carlisle's original version, standard Pascal
--
WITH pascal_tokens;
USE pascal_tokens;
PACKAGE yyroutines IS

FUNCTION YYLex RETURN Token;

PROCEDURE UnYYLex(tok : Token);

END yyroutines;
