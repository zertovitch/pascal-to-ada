{ Just a test for P2Ada! TP/BP/Delphi Unit -> Ada Package }

(* Difficult comments *)
(* .... ** ....*) { <--- Pascal.l bugfix by C. Carey Nov-2002 }

UNIT NPTest2;

INTERFACE 

  type Bidon = array[1..1] of array[1..1] of char;

  obj_art_type=(txt,box,lin,vec,circ,oval,aux,putaux,bezier,bezvec,
                unitl,spez,beginn,ende1,ende2,point,option);
  ptr_obj_type=^obj_type;
  p_string = ^string;
  obj_type=record {JW,GH}
     x_pos, y_pos, width, height:real;
     next:ptr_obj_type;
     picked:boolean;
  end; {record}

  PROCEDURE ResetText(VAR t:Text; n:String);
  PROCEDURE RewriteText(VAR t:Text; n:String);
  PROCEDURE WritePasString(VAR t:Text; s:String);
  PROCEDURE WriteAdaString(VAR t:Text; s:String);

  procedure Test_External; {external forbidden here}

IMPLEMENTATION

  procedure Test_External; external;
  {$L Test_ext.obj}

  const
    num1 = 1.0e17;
    num2 = + 1.0E17;
    num3 = -1.0e17;

    num101 = 1e1007;
    num102 = 1.0e+1007;
    num103 = 1.0E+1007;
    num104 = 1.0e-1007;
    num105 = -1.0e+1007;
    num106 = +1.0e-1007;

    num201 = 12e-34; { <--- NB: an Ada compiler must gasp on that! }

    { Roman V. Isaev 31-May-2003 }
    num301 =  1.e5;
    num302 = +13.e-5;
    num303 = -13.e+05;

  PROCEDURE ResetText(VAR t:Text; n:String);   BEGIN Assign(t,n);Reset(t) END;
  PROCEDURE RewriteText(VAR t:Text; n:String); BEGIN Assign(t,n);Rewrite(t) END;
  
  PROCEDURE WriteLgString(VAR t:Text; s:String;
              guil,conca:char; asc_a, asc_z:String);
    VAR sd:String; b:Byte;
  BEGIN
    FOR b:= 1 TO Length(s) DO BEGIN
      Str(Ord(s[b]):0,sd);
      Write(t,asc_a,sd,asc_z);
      IF b<Length(s) THEN begin
        Write(t,conca);
        if b mod 3=0 then writeln(t)
      end 
    END
  END;
  
  PROCEDURE WritePasString(VAR t:Text; s:String);
  BEGIN
    WriteLgString(t,s,'''','+','#','')
  END;
  
  PROCEDURE WriteAdaString(VAR t:Text; s:String);
  BEGIN
    WriteLgString(t,s,'"','&','Character''val(',')')
  END;

END.
{--- end of file ---}