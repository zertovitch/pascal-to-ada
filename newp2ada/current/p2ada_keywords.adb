-- To avoid the problem of Pascal identifiers same as an Ada keyword

-- Converted by (New) P2Ada v. 29-Jan-2003
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;

package body P2Ada_keywords is

  KEY: array (1 .. 73) of String (1 .. 12) ; --  Array of Ada keywords in order
      
  subtype AZ is Character range 'A' .. 'Z';
  Hkey: array ( AZ ) of Integer;

   -- Taken from SmallAda, added Ada95 keywords
   -- added Ada 2005 and Ada 2012 keywords (20130508)
  
  procedure Init_keys is
  begin
    KEY (1)  := "ABORT       ";
    KEY (2)  := "ABS         ";
    KEY (3)  := "ABSTRACT    "; --  Ada 95
    KEY (4)  := "ACCEPT      ";
    KEY (5)  := "ACCESS      ";
    KEY (6)  := "ALIASED     "; --  Ada 95
    KEY (7)  := "ALL         ";
    KEY (8)  := "AND         ";
    KEY (9)  := "ARRAY       ";
    KEY (10) := "AT          ";
    KEY (11) := "BEGIN       ";
    KEY (12) := "BODY        ";
    KEY (13) := "CASE        ";
    KEY (14) := "CONSTANT    ";
    KEY (15) := "DECLARE     ";
    KEY (16) := "DELAY       ";
    KEY (17) := "DELTA       ";
    KEY (18) := "DIGITS      ";
    KEY (19) := "DO          ";
    KEY (20) := "ELSE        ";
    KEY (21) := "ELSIF       ";
    KEY (22) := "END         ";
    KEY (23) := "ENTRY       ";
    KEY (24) := "EXCEPTION   ";
    KEY (25) := "EXIT        ";
    KEY (26) := "FOR         ";
    KEY (27) := "FUNCTION    ";
    KEY (28) := "GENERIC     ";
    KEY (29) := "GOTO        ";
    KEY (30) := "IF          ";
    KEY (31) := "IN          ";
    KEY (32) := "INTERFACE   "; --  Ada 2005
    KEY (33) := "IS          ";
    KEY (34) := "LIMITED     ";
    KEY (35) := "LOOP        ";
    KEY (36) := "MOD         ";
    KEY (37) := "NEW         ";
    KEY (38) := "NOT         ";
    KEY (39) := "NULL        ";
    KEY (40) := "OF          ";
    KEY (41) := "OR          ";
    KEY (42) := "OTHERS      ";
    KEY (43) := "OUT         ";
    KEY (44) := "OVERRIDING  "; --  Ada 2005
    KEY (45) := "PACKAGE     ";
    KEY (46) := "PRAGMA      ";
    KEY (47) := "PRIVATE     ";
    KEY (48) := "PROCEDURE   ";
    KEY (49) := "PROTECTED   "; --  Ada 95
    KEY (50) := "RAISE       ";
    KEY (51) := "RANGE       ";
    KEY (52) := "RECORD      ";
    KEY (53) := "REM         ";
    KEY (54) := "RENAMES     ";
    KEY (55) := "REQUEUE     "; --  Ada 95
    KEY (56) := "RETURN      ";
    KEY (57) := "REVERSE     ";
    KEY (58) := "SELECT      ";
    KEY (59) := "SEPARATE    ";
    KEY (60) := "SOME        "; --  Ada 2012
    KEY (61) := "SUBTYPE     ";
    KEY (62) := "SYNCHRONIZED"; --  Ada 2005
    KEY (63) := "TAGGED      "; --  Ada 95
    KEY (64) := "TASK        ";
    KEY (65) := "TERMINATE   ";
    KEY (66) := "THEN        ";
    KEY (67) := "TYPE        ";
    KEY (68) := "UNTIL       "; --  Ada 95
    KEY (69) := "USE         ";
    KEY (70) := "WHEN        ";
    KEY (71) := "WHILE       ";
    KEY (72) := "WITH        ";
    KEY (73) := "XOR         ";

    for c in AZ loop 
      HKey(c):= Key'last;
      for i in  reverse Key'range loop
        if Key(i)(1) = c then HKey(c):= i; end if;
      end loop;
    end loop;
    
  end Init_keys;

  function Avoid_keyword( id: String ) return String is
    Uid: constant String:= To_Upper(id);
    c: Character;
  begin
    if id /= "" then
      c:= Uid(Uid'first);
      case c is
        when AZ =>
          for i in HKey(c) .. Key'last loop
            if Uid = Trim( Key(i), right ) then
            
              return "P2Ada_no_keyword_" & id;
              
            end if;
            exit when Key(i)(1) /= c;
          end loop;
        when others => null;
      end case;
    end if;
    
    return id;
    
  end Avoid_keyword;

begin
  Init_keys;
end P2Ada_keywords;
-- Converted by (New) P2Ada v. 29-Jan-2003   
