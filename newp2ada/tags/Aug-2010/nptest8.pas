{Code Warrior Pascal}

program Test_CWP;

  procedure Short_circuit;
  begin
    if(true&false)|(0=9)then
  end;

  type Ptr = ^Integer;

  procedure Test_univ(a: univ Ptr; var b: univ Ptr; const c: univ Ptr);
  begin
  end;

  procedure Test_C; C; External;

  procedure Test_ext; External;

  procedure Test_exit1;
    procedure Test_exit2;
    begin
      Exit(Test_exit1)
    end;
  begin
    Test_exit2
  end;

begin
  Test_exit1;
end.