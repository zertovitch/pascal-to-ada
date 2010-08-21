{> P2ada test - mathematical stuff }

program NPTest6;

  { Mandelbrot set in text-mode. Standard Pascal with "Double" type }
  { G. de Montmollin 2003 }

  procedure Mandelbrot_ASCII(mx,my: Integer);

    procedure Plot_area_absolute( ar,br,ai,bi: Double; itmax: Integer );
      const ncol=6;
      type Complex = record r,i: Double; end;
      var
        x,y: Integer;
        col_table: array[0..ncol] of Char;
        p, area: Complex;

      function Check_in_set( c: Complex ): Integer;
        var
          z: Complex;
          tmp: Double;
          count: Integer;
      begin
        z.r:= 0.0;
        z.i:= 0.0;
        count:= 0;
        repeat
          tmp:= Sqr(z.r) - Sqr(z.i) + c.r;
          z.i:= 2.0 * z.r * z.i + c.i;
          z.r:= tmp;
          count:= count + 1;
        until (count > itmax) or ((Sqr(z.r) + Sqr(z.i)) > 4.0);
        Check_in_set:= count
      end;

    begin
      col_table[0]:= '#';
      col_table[1]:= '%';
      col_table[2]:= 'o';
      col_table[3]:= '*';
      col_table[4]:= ':';
      col_table[5]:= '.';
      col_table[6]:= ' ';
      area.r:= br - ar;
      area.i:= bi - ai;
      for y:= 0 to my-1 do begin
        for x:= 0 to mx-1 do begin
          p.r:= ar + area.r * (x) / (mx); { <--- Add Long_Float(...) }
          p.i:= ai + area.i * (y) / (my);
          Write( col_table[(Check_in_set( p ) * ncol) div itmax] )
        end;
        WriteLn
      end;
      for x:= 0 to mx-9 do Write('-');
      Write('[Return]');
      ReadLn
    end;

  procedure Plot_area( ar,lr,ai,li: Double; itmax: Integer );
  begin
    Plot_area_absolute( ar,ar+lr,ai,ai+li, itmax )
  end;

  begin

    Plot_area_absolute(  -2.2, 1.0, -1.25,  1.25, 20 );
    Plot_area_absolute(  -1.0, 1.0, -1.25,  0.0,  20 );
    Plot_area_absolute(  -0.5, 0.4, -1.20, -0.6,  30 );

    Plot_area(
      -1.2029564421301, 0.0351555010544,
      -0.3096782894352, 0.0116621445959, 150);

    Plot_area(
      -0.6318405970938, 0.1392759537664,
      -0.7106447090334, 0.1356552208384, 200);

    Plot_area(
      -0.5228527433221, 0.0016623236276,
      -0.5988152788281, 0.0011269551612, 300);

    Plot_area(
      -1.2963558632663, 0.0000120813111,
       0.4418487504284, 0.0000057144987, 29);

  end;

  procedure The_Matrix;
    const matmax = 20;

    type Matrix = array[1..matmax,1..matmax] of Double;

    var M1,M2,M3: Matrix;
        i,j: Integer;

    procedure TransposeAB( var A,B: Matrix );
      var i,j: Integer;
    begin
      for i := 1 to matmax do
        for j := 1 to matmax do
          B[j,i]:= A[i,j]
    end;

  begin
    for i := 1 to matmax do
      for j := 1 to matmax do
        if i > j then
          M1[i,j]:= (i*j)  { <- You need to write Long_Float(i*j) }
        else
          M1[i,j]:= 0.0;

    TransposeAB(M1,M2);

    TransposeAB(M2,M3);

    for i := 1 to matmax do
      for j := 1 to matmax do
        if M1[i,j] <> M3[i,j] then WriteLn('Transpose failed!')

  end;

begin
  Mandelbrot_ASCII(78,24);
  The_Matrix;

end.
