// Based on FPC test source codes
{ %VERSION=1.1 }
{$mode objfpc}

type
  t1 = record
  end platform;

  t2 = class
  end platform;

  t3 = object
  end platform;

  t4 = record
    w1 : word deprecated;
    w2 : word deprecated
  end;

  t5 = record
    w1 : word deprecated;
  end;

const
  c1 : word = 2;
  c2 = 21312 platform;

var
  v1 : word deprecated library;
  l1,
  l2 : word deprecated;

{procedure p1;platform deprecated;library;
begin
end;

procedure p2;platform;deprecated;
begin
end;}

  type
     parrayobj = ^tarraycla;
     tarraycla = class
       ar : array [1..4] of real;
       constructor create(do_fail : boolean);
       procedure test;virtual;
       destructor done;virtual;
       end;
     pbigarrayobj = ^tbigarraycla;
     tbigarraycla = class(tarraycla)
       ar2 : array [1..10000] of real;
       constructor good_init;
       constructor wrong_init;
       procedure test;virtual;
       end;
  var
    ta1, ta2 : tarraycla;

  constructor tarraycla.create(do_fail : boolean);
    begin
       ar[1]:=1;
       if do_fail then
         fail;
       ar[2]:=2;
    end;

  destructor tarraycla.done;
    begin
    end;

  procedure  tarraycla.test;
    begin
      if ar[1]=1 then
        Writeln('Init called');
      if ar[2]=2 then
        Writeln('Init successful');
    end;

  constructor tbigarraycla.good_init;
    begin
      inherited create(false);
      Writeln('End of tbigarraycla.good_init');
    end;

  constructor tbigarraycla.wrong_init;
    begin
      inherited create(true);
      Writeln('End of tbigarraycla.wrong_init');
    end;

  procedure tbigarraycla.test;
    begin
      Writeln('tbigarraycla.test called');
      Inherited test;
    end;

type
   to1 = class
      destructor destroy;override;
      procedure beforedestruction;override;
   end;

   to2 = class(to1)
      destructor destroy;override;
      procedure beforedestruction;override;
   end;

var
   i : longint;

   destructor to1.destroy;

     begin
        writeln('to1.destroy');
        if i<>2000 then
          halt(1);
        i:=3000;
        inherited destroy;
     end;

   procedure to1.beforedestruction;

     begin
        writeln('to1.beforedestruction');
        if i<>1000 then
          halt(1);
        i:=2000;
     end;

   destructor to2.destroy;

     begin
        writeln('to2.destroy');
        if i<>4000 then
          halt(1);
        i:=2000;
        inherited destroy;
        i:=5000;
     end;

   procedure to2.beforedestruction;

     begin
        writeln('to2.beforedestruction');
        if i<>3000 then
          halt(1);
        i:=4000;
     end;

type
  IMyInterface = interface
    function f : longint;
    procedure p(a : longint);
    property x : longint read f write p;
  end;

var
  fRefCount: Integer = 0;

type
  IA = interface
    ['{81E19F6A-90C2-11D9-8448-00055DDDEA00}']
  end;
  TA = class(TObject, IA, IInterface)
    destructor Destroy; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const iid: TGuid; out obj): HResult; stdcall;
    procedure AfterConstruction; override;
    class function NewInstance: TObject; override;
  end;

class function TA.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  fRefCount := 1;
end;

procedure TA.AfterConstruction;
begin
  InterlockedDecrement(fRefCount);
  inherited AfterConstruction;
end;

function TA._AddRef: Integer; stdcall;
begin
  InterlockedIncrement(fRefCount);
  Result := 0;
end;

function TA._Release: Integer; stdcall;
begin
  InterlockedDecrement(fRefCount);
  if fRefCount = 0 then begin
    Writeln('Destroy');
    Self.Destroy;
  end;

  Result := 0;
end;

function TA.QueryInterface(const iid: TGuid; out obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

var
  gone: Boolean = False;

destructor TA.Destroy;
begin
  gone := True;
  Writeln('gone');
  inherited Destroy;
end;

procedure X;
var
  v: Variant;
  i: IInterface;
begin
  Writeln('start of test');
  (* simple test with nil interface *)
  i := nil;
  v := i;

  i := v;

  v := 3;

  (* complex test with refcounting *)
  Writeln('complex test');

  i := TA.Create;
  assert(fRefCount = 1);
  Writeln('part 1');
  v := i;
  Writeln('part 2');
  //assert(fRefCount = 2);

  i := nil;
  //assert(fRefCount = 1);

  Writeln('part 3');
  i := v;
  //assert(fRefCount = 2);

  Writeln('gone false');
  assert(gone = False);
  i := nil;
  //assert(fRefCount = 1);
  assert(gone = False);
  v := 7; (* TA refcount 0; gone ... note that v := Null doesnt work for some reason *)
  //assert(fRefCount = 0);
  Writeln('goo');
  //assert(gone = True);
  (* "gone" *)

  Writeln('okay');
  //Halt(0);
end;

procedure TestExceptions;
const
  Program_has_errors : boolean = false;
  exception_called   : boolean = false;
  TestNumber : longint = 10000;

procedure test_exception(const s : string);
  begin
    if not(exception_called) then
      begin
        Writeln('Exception not called : ',s);
        Program_has_errors := true;
      end;
  end;

var
   i,j : longint;
   e : extended;
   exception_count,level : longint;
begin
   j:=0;
   i:=100;
   try
   exception_called:=false;
   j := i div j;
   except
     on e : exception do
       begin
         Writeln('First integer exception called ',e.message);
         exception_called:=true;
       end;
   end;
   test_exception('First division by zero for integers');
   try
   exception_called:=false;
   j := i div j;
   except
     on e : exception do
       begin
         Writeln('Second integer exception called ',e.message);
         exception_called:=true;
       end;
   end;
   test_exception('Second division by zero for integers');
   try
   exception_called:=false;
   e:=i/j;
   except
     on e : exception do
       begin
         Writeln('First real exception called ',e.message);
         exception_called:=true;
       end;
   end;
   test_exception('First division by zero for reals');
   try
   exception_called:=false;
   e:=i/j;
   except
     on e : exception do
       begin
         Writeln('Second real exception called ',e.message);
         exception_called:=true;
       end;
   end;
   test_exception('Second division by zero for reals');
   try
   exception_called:=false;
   j := i div j;
   except
     on e : exception do
       begin
         Writeln('exception called ',e.message);
         exception_called:=true;
       end;
   end;
   test_exception('third division by zero for integers');
   exception_count:=0;
   level:=0;
   for j:=1 to TestNumber do
     begin
       try
         i:=0;
         inc(level);
         e:=j/i;
       except
         on e : exception do
           begin
             inc(exception_count);
             if level>1 then
               Writeln('exception overrun');
             dec(level);
           end;
       end;

     end;
   if exception_count<>TestNumber then
     begin
       program_has_errors:=true;
       Writeln('Could not generate ',TestNumber,' consecutive exceptions');
       Writeln('Only ',exception_count,' exceptions were generated');
     end
   else
     begin
       Writeln(TestNumber,' consecutive exceptions generated successfully');
     end;
   try
   exception_called:=false;
   i := -1;
   e := ln(i);
   except
     on e : exception do
       begin
         Writeln('exception called ',e.message);
         exception_called:=true;
       end;
   end;
   test_exception('ln(-1)');
   if program_has_errors then
     Halt(1);
end;


var i : integer;
procedure test3;
  begin
     try
       try
          i:=0;
          raise exception.create('');
       finally
          inc(i);
       end;
     finally
       inc(i);
     end;
     i:=-2;
  end;

var
  FErrno : longint;
function GetROVar:longint;
begin
  GetROVar:=3;
end;
function GetErrno:longint;
begin
  GetErrno:=FErrno;
end;
procedure SetErrno(e:longint);
begin
  FErrno:=e;
end;

property
  Errno:longint read GetErrno write SetErrno;
  ROVar:longint read GetROVar;

type  Tconstexprint=record
        overflow:boolean;
        case signed:boolean of
          false:
            (uvalue:qword);
          true:
            (svalue:int64);
      end;

operator := (const u:qword):Tconstexprint;
begin
  if (u<>high(int64)+100) then
    halt(1);
  result.overflow:=false;
  result.signed:=false;
  result.uvalue:=u;
end;

operator := (const s:int64):Tconstexprint;
begin
  if (s<>-128) then
    halt(2);
  result.overflow:=false;
  result.signed:=true;
  result.svalue:=s;
end;

var
  value : tconstexprint;

type
  op3 = record
    x,y : real;
  end;

operator + (const a,b : op3) c : op3;
begin
  c.x:=a.x+b.x;
  c.y:=a.y+b.y;
end;

procedure test_op3;
var
  a,b,c : op3;
begin
  a.x:=44.0;
  a.y:=67.0;
  b.x:=-34.0;
  b.y:=-57.0;
  c:=a+b;
  if (c.x<>10.0) or (c.y<>10.0) then
    Halt(1);
end;

{$mode objfpc}

type
   generic TList<_T>=class(TObject)
     type
       PListItem = ^TListItem;
       TListItem = record
         data : _T;
         next : PListItem;
       end;
       TIterator = PListItem;
     var
       first : PListItem;

     function GetFirst : TIterator; inline;
     function GetNext(i : TIterator) : TIterator; inline;
     procedure Add(item: _T);
   end;

procedure TList.Add(item: _T);
var
  newitem : PListItem;
begin
  new(newitem);
  newitem^.data:=item;
  newitem^.next:=first;
  first:=newitem;
end;


function TList.GetFirst : TIterator; inline;
  begin
    result:=first;
  end;


function TList.GetNext(i : TIterator) : TIterator; inline;
  begin
    result:=i^.next;
  end;

procedure Test_Gen;
type
  TMyIntList = specialize TList<integer>;
  TMyStringList = specialize TList<string>;
var
  ilist : TMyIntList;
  slist : TMyStringList;
  someInt : integer;
  iterator : TMyIntList.TIterator;
begin
  someInt:=10;
  ilist := TMyIntList.Create;
  ilist.Add(someInt);
  ilist.Add(someInt+1);
  iterator:=ilist.GetFirst;
  writeln(iterator^.data);
  if iterator^.data<>11 then
    halt(1);
  iterator:=ilist.GetNext(iterator);
  writeln(iterator^.data);
  if iterator^.data<>10 then
    halt(1);

  slist := TMyStringList.Create;
  slist.Add('Test1');
  slist.Add('Test2');
  writeln(slist.first^.data);
  if slist.first^.data<>'Test2' then
    halt(1);
  writeln('ok');
end;

var
   o1 : to1;
   o2 : to2;
begin
  // Here it should choose the int64 code instead of qword
  value:=-128;
  // Here it should choose the qword
  value:=high(int64)+100;
    test_op3;
   o1:=to1.create;
   o2:=to2.create;
   i:=1000;
   o1.destroy;
   o2.destroy;
   if i<>5000 then
     halt(1);
   writeln('ok');
  X;
  TestExceptions;
  test3;
  FErrno:=1;
  if Errno<>1 then
    begin
      writeln('Error 1');
      halt(1);
    end;
  Errno:=2;
  if Errno<>2 then
    begin
      writeln('Error 2');
      halt(1);
    end;
  if ROVar<>3 then
    begin
      writeln('Error 3');
      halt(1);
    end;
  writeln(i);
  Test_Gen;
     ta1:=tarraycla.create(false);
     writeln('Call to ta1.test after successful init');
     ta1.test;
     ta2:=tarraycla.create(true);
     writeln('ta2 = ',ptrint(ta2),' after unsuccessful init');
     Writeln('Trying to call ta2.test (should generate a Run Time Error)');
     ta2.test;
end.
