{Turbo Pascal 7 (.. Delphi 6)}

(* TP7:

  o Language extensions

    o Open array parameters - Allows safer, more flexible
      variable-length array parameters.
    o New OpenString type - Allows a procedure or function
      to accept any type of string parameter, even in the
      {$V+} state.
    * Constant parameters - Efficient, read-only value
      parameters.
    o Public/private sections - Allows public and private
      sections of objects to be declared in any order.
    - Typed @ operator {$T+/-} - Enforces type-checking when
      using the address (@) operator.
    o Inherited reserved word - Allows references to an inherited
      method without knowing the ancestor's name.

  o New standard procedures/functions

    o Assigned - Returns True if a pointer is not equal to nil
      (especially useful for procedure and function pointers).
    o Include - Adds an item to a set.
    o Exclude - Removes an item from a set.
    o Break & Continue - FOR, WHILE, REPEAT loop control.
    o Low & High - Return the low or high bounds of open array
      parameters or of any scalar type.
*)

program Test_tp7;

  procedure Funny_params(
    const i: integer;
    var j:integer;
    k: integer;
    const l;
    var m);
  begin
    j:= i;
    for k:= 1 to 10 do j:= j+1
  end;

  {Break: *procedure* (not a keyword) that should make exit from a loop...}
  procedure Test_break1;
    var i:Integer;
  begin
    for i:= 1 to 5 do begin
      Break;
      WriteLn('Not seen !')
    end
  end;

  procedure Test_break2;
    var i:Integer;
    procedure Break; begin end; {Masks TP7's "Break" !}
  begin
    for i:= 1 to 5 do begin
      Break;
      WriteLn('After BREAK !')
    end
  end;

  {Exit: *procedure* (not a keyword) that should make return...}
  procedure Test_exit1;
  begin
    Exit;
    WriteLn('Not seen !')
  end;

  procedure Test_exit2;
    procedure exit; begin end; {Masks TP6's "Exit" !}
  begin
    Exit;
    WriteLn('After EXIT !')
  end;

  procedure Test_Halt_1;

    procedure Halt; begin end;

  begin
    Halt;
    WriteLn('After Halt !')
  end;

  procedure Test_Halt_2;
  begin
    Halt;
    WriteLn('After Halt ?')
  end;

begin
  Test_break1;
  Test_break2;
  Test_exit1;
  Test_exit2;
  Test_Halt_1;
  Test_Halt_2;
end.