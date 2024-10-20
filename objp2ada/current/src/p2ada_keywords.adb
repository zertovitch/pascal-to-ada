-- To avoid the problem of Pascal identifiers same as an Ada keyword

-- Converted by (New) P2Ada v. 29-Jan-2003
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;

package body P2Ada_keywords is

  KEY: array ( 1..69 ) of String (1 ..10) ; --  Array of Ada keywords in order
      
  subtype AZ is Character range 'A' .. 'Z';
  Hkey: array ( AZ ) of Integer;

  -- Taken from SmallAda, added Ada95 keywords 
  
  procedure Init_keys is
  begin
    KEY ( 1) := "ABORT     " ;
    KEY ( 2) := "ABS       " ;
    KEY ( 3) := "ABSTRACT  " ; --  Ada 95
    KEY ( 4) := "ACCEPT    " ;
    KEY ( 5) := "ACCESS    " ;
    KEY ( 6) := "ALIASED   " ; --  Ada 95
    KEY ( 7) := "ALL       " ;
    KEY ( 8) := "AND       " ;
    KEY ( 9) := "ARRAY     " ;
    KEY (10) := "AT        " ;
    KEY (11) := "BEGIN     " ;
    KEY (12) := "BODY      " ;
    KEY (13) := "CASE      " ;
    KEY (14) := "CONSTANT  " ;
    KEY (15) := "DECLARE   " ;
    KEY (16) := "DELAY     " ;
    KEY (17) := "DELTA     " ;
    KEY (18) := "DIGITS    " ;
    KEY (19) := "DO        " ;
    KEY (20) := "ELSE      " ;
    KEY (21) := "ELSIF     " ;
    KEY (22) := "END       " ;
    KEY (23) := "ENTRY     " ;
    KEY (24) := "EXCEPTION " ;
    KEY (25) := "EXIT      " ;
    KEY (26) := "FOR       " ;
    KEY (27) := "FUNCTION  " ;
    KEY (28) := "GENERIC   " ;
    KEY (29) := "GOTO      " ;
    KEY (30) := "IF        " ;
    KEY (31) := "IN        " ;
    KEY (32) := "IS        " ;
    KEY (33) := "LIMITED   " ;
    KEY (34) := "LOOP      " ;
    KEY (35) := "MOD       " ;
    KEY (36) := "NEW       " ;
    KEY (37) := "NOT       " ;
    KEY (38) := "NULL      " ;
    KEY (39) := "OF        " ;
    KEY (40) := "OR        " ;
    KEY (41) := "OTHERS    " ;
    KEY (42) := "OUT       " ;
    KEY (43) := "PACKAGE   " ;
    KEY (44) := "PRAGMA    " ;
    KEY (45) := "PRIVATE   " ;
    KEY (46) := "PROCEDURE " ;
    KEY (47) := "PROTECTED " ; --  Ada 95
    KEY (48) := "RAISE     " ;
    KEY (49) := "RANGE     " ;
    KEY (50) := "RECORD    " ;
    KEY (51) := "REM       " ;
    KEY (52) := "RENAMES   " ;
    KEY (53) := "REQUEUE   " ; --  Ada 95
    KEY (54) := "RETURN    " ;
    KEY (55) := "REVERSE   " ;
    KEY (56) := "SELECT    " ;
    KEY (57) := "SEPARATE  " ;
    KEY (58) := "SUBTYPE   " ;
    KEY (59) := "TAGGED    " ; --  Ada 95
    KEY (60) := "TASK      " ;
    KEY (61) := "TERMINATE " ;
    KEY (62) := "THEN      " ;
    KEY (63) := "TYPE      " ;
    KEY (64) := "UNTIL     " ; --  Ada 95
    KEY (65) := "USE       " ;
    KEY (66) := "WHEN      " ;
    KEY (67) := "WHILE     " ;
    KEY (68) := "WITH      " ;
    KEY (69) := "XOR       " ;

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