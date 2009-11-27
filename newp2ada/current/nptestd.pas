program NPTestD;

 type

   Emplacement_obj = object
      private
      Z : Boolean;
      public
      X, Y : Integer;
      procedure No_param; virtual;
      constructor Init(InitX, InitY : Integer);
      protected
      function AkiX : Integer;
      function AkiY : Integer;
      private
      destructor Done;
   end;

   Emplacement_obj2 = object(Emplacement_obj)
      procedure No_param; override;
   end;


   Ovide1 = object X: Integer; end;
   Ovide2 = object(Ovide1) end;
   Ovide3 = object(Ovide2) end;
   Ovide4 = object(Ovide3) Y: Integer; end;


   Emplacement_rec = record
      X, Y : Integer;
   end;

      procedure Emplacement_obj.No_param;
      begin
        Y:= 1111;
        X:= Y
      end;

      procedure Emplacement_obj2.No_param;
      begin
        X:= 1234
      end;

      procedure Emplacement_obj.Init(InitX, InitY : Integer);
        begin
        X := InitX;
        Y := InitY;
        end;

      function Emplacement_obj.AkiX : Integer;
        begin
        AkiX := X;
        end;
      function Emplacement_obj.AkiY : Integer;
        begin
        AkiY := Y;
        end;

      procedure Emplacement_obj.Done;
      begin
        X:= 0;
      end;

  procedure Affiche;
  begin
    Writeln('Hello Object Pascal');
  end;

  var Point_o : Emplacement_obj2;
      Point_r : Emplacement_rec;
      ov4     : Ovide4;
begin
  Affiche;
  with point_r do
     Y := 3;
  with point_o do
     Y := 4;
  Point_o.X := 2 + Point_o.AkiX;
  Point_o.Init(2, 3);
  Point_o.No_Param;
  WriteLn(Point_o.X); { Must be 1234 }
  with ov4 do begin
    X:= 8;
    Y:= 12
  end
end.