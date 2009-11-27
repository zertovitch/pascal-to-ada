@echo off
echo Translation of Numerical Recipes in Pascal
echo ------------------------------------------
echo.
echo  Demos (*.dem) are supposed to be in "Demos" subdirectory.
echo  Recipes (*.pas) are supposed to be in "Recipes" subdirectory.
echo  "modfile.pas" is supposed to be in this directory.
echo.
echo  If the mentioned subdirectories are not there, the present batch file
echo  will create them and copy the Pascal files there
echo.
echo  An "Ada" subdirectory will be created.
echo.
echo  Programs needed: p2ada, gnatchop
echo.
echo Ctrl-Break to stop here.
pause

if not exist Ada\nul md Ada

if exist Demos\nul goto skipdemos
md Demos
copy *.dem Demos
:skipdemos

if exist Recipes\nul goto skiprecipes
md Recipes
copy *.pas Recipes
:skiprecipes

cd Demos
for %%i in (*.dem) do p2ada %%i >>..\Ada\demos.ada
cd..

cd Recipes
rem Recipes are separated procedures, except sfroid.pas and badluk.
ren sfroid.pas sfroid.prg
ren badluk.pas badluk.prg
copy *.pas ..\recipes.src
echo begin end.>>..\recipes.src
ren sfroid.prg sfroid.pas
ren badluk.prg badluk.pas
p2ada sfroid.pas >..\Ada\sfroid.adb
cd..

echo CONST mp1=1;n2m1=1;mp=1;ndatap=1;nyj=1;nyk=1;jmaxp=1;n=1;nvar=1;npolesp=1;ncp=1; >recipes.env
echo       np=1;n2=1;nn=1;nbmax=1;map=1;nci=1;ncj=1;nck=1;m=1;m4=1;nsi=1;nsj=1;ndp=1;nlp=1; >>recipes.env
echo       RzextrImax=11;RzextrNmax=10;RzextrNcol=7;nstepp=200;nfutp=1;ndat2=1;>>recipes.env
echo       PzextrImax=11;PzextrNmax=10;PzextrNcol=7;n1p=1;n2p=1;ncityp=1;nip=1;njp=1;>>recipes.env
echo       MetropJdum: integer = -1; BcucofFlag: boolean = True;nbinsp=1;ndimp=1;ip=1;jp=1;>>recipes.env
echo TYPE RealArrayNP = ARRAY [1..np] OF real; >>recipes.env
echo      DoubleArrayNP = ARRAY [1..np] OF Double; >>recipes.env
echo      RealArray4 = ARRAY [1..4] OF real; >>recipes.env
echo      RealArray4by4 = ARRAY [1..4,1..4] OF real; >>recipes.env
echo      RealArrayN1 = ARRAY [1..n1p] OF real; >>recipes.env
echo      RealArrayN2 = ARRAY [1..n2p] OF real; >>recipes.env
echo      RealArrayN12 = RealArrayNP; >>recipes.env
echo      RealArrayNCITY = ARRAY [1..ncityp] OF real; >>recipes.env
echo      IntegerArrayNCITY = ARRAY [1..ncityp] OF integer; >>recipes.env
echo      RealArrayNPbyNP = ARRAY [1..np,1..np] OF real; >>recipes.env
echo      RealArrayMP = ARRAY [1..mp] OF real; >>recipes.env
echo      RealArrayMPbyNP = ARRAY [1..mp,1..np] OF real; >>recipes.env
echo      RealArrayNPbyMP = ARRAY [1..np,1..mp] OF real; >>recipes.env
echo      RealArray2tN = ARRAY [1..n2] OF real; >>recipes.env
echo      RealArrayNBMAX = ARRAY [1..nbmax] OF real; >>recipes.env
echo      RealArrayNDATA = ARRAY [1..ndatap] OF real; >>recipes.env
echo      RealArrayMA = ARRAY [1..map] OF real; >>recipes.env
echo      RealArrayMAbyMA = ARRAY [1..map,1..map] OF real; >>recipes.env
echo      RealArray2Nm1 = ARRAY [1..n2m1] OF real; >>recipes.env
echo      RealArrayNN = ARRAY [1..nn] OF real; >>recipes.env
echo      RealArray4tM = ARRAY [1..m4] OF real; >>recipes.env
echo      RealArrayNN2 = RealArray4tM; >>recipes.env
echo      IntegerArrayNYJ = ARRAY [1..nyj] OF integer; >>recipes.env
echo      RealArrayNYJ = ARRAY [1..nyj] OF real; >>recipes.env
echo      RealArrayNYJbyNYK = ARRAY [1..nyj,1..nyk] OF real; >>recipes.env
echo      RealArrayNCIbyNCJbyNCK = ARRAY [1..nci,1..ncj,1..nck] OF real; >>recipes.env
echo      RealArrayNSIbyNSJ = ARRAY [1..nsi,1..nsj] OF real; >>recipes.env
echo      DoubleArrayJMAXbyJMAX = ARRAY [1..jmaxp,1..jmaxp] OF double; >>recipes.env
echo      IntegerArrayNP = ARRAY [1..np] OF integer; >>recipes.env
echo      IntegerArrayMP = ARRAY [1..mp] OF integer; >>recipes.env
echo      IntegerArrayN = ARRAY [1..n] OF integer; >>recipes.env
echo      IntegerArrayM = ARRAY [1..m] OF integer; >>recipes.env
echo      RealArrayNVAR = ARRAY [1..nvar] OF real; >>recipes.env
echo      RealArrayNVARbyNVAR = ARRAY [1..nvar,1..nvar] OF real; >>recipes.env
echo      Complex = RECORD r,i: real END; >>recipes.env
echo      ComplexArrayMp1 = ARRAY [1..mp1] OF Complex; >>recipes.env
echo      RealArrayN2byN2 = ARRAY [1..n2p,1..n2p] OF real; >>recipes.env
echo      Immense = RECORD l,r: longint END; >>recipes.env
echo      RealArray65 = ARRAY [1..65] OF real; >>recipes.env
echo      RealArrayNV = ARRAY [1..3] OF real; >>recipes.env
echo      RealArrayNPOLES = ARRAY [1..npolesp] OF real; >>recipes.env
echo      RealArrayNFUT = ARRAY [1..nfutp] OF real; >>recipes.env
echo      IntegerArrayNIbyNJ = ARRAY [1..nip,1..njp] OF integer; >>recipes.env
echo      RealArrayNI = ARRAY [1..nip] OF real; >>recipes.env
echo      RealArrayNJ = ARRAY [1..njp] OF real; >>recipes.env
echo      Great = RECORD l,c,r: word END; >>recipes.env
echo      IntegerArrayMFIT = ARRAY [1..map] OF integer; >>recipes.env
echo      RealArrayNBINS = ARRAY [1..nbinsp] OF real; >>recipes.env
echo      IntegerArrayNDIM = ARRAY [1..ndimp] OF integer; >>recipes.env
echo      RealArrayNDAT2 = ARRAY [1..ndat2] OF real; >>recipes.env
echo      IntegerArrayNC = ARRAY [1..ncp] OF integer; >>recipes.env
echo      IntegerArrayND = ARRAY [1..ndp] OF integer; >>recipes.env
echo      RealArrayNL = ARRAY [1..nlp] OF real; >>recipes.env
echo      RealArrayIPbyJP = ARRAY [1..ip,1..jp] OF real; >>recipes.env
echo      RealArrayMAby1 = ARRAY [1..map,1..1] OF real; >>recipes.env

echo VAR RzextrX: ARRAY [1..RzextrImax] OF real; >>recipes.env
echo     RzextrD: ARRAY [1..RzextrNmax,1..RzextrNcol] OF real; >>recipes.env
echo     MidexpIt, TrapzdIt: integer; >>recipes.env
echo     RkdumbX: ARRAY [1..nstepp] OF real; >>recipes.env
echo     RkdumbY: ARRAY [1..nvar,1..nstepp] OF real; >>recipes.env
echo     Ran4Newkey: boolean; Ran4Inp,Ran4Key: Immense; >>recipes.env
echo     Ran4Pow: RealArray65; >>recipes.env
echo     Ran0Y: real; Ran0V: ARRAY [1..97] OF real; >>recipes.env
echo     Ran1Ix1,Ran1Ix2,Ran1Ix3: longint; Ran1R: ARRAY [1..97] OF real; >>recipes.env
echo     Ran2Iy: longint; Ran2Ir: ARRAY [1..97] OF longint; >>recipes.env
echo     Ran3Inext,Ran3Inextp: integer; Ran3Ma: ARRAY [1..55] OF real; >>recipes.env
echo     Quad3dX,Quad3dY: real; >>recipes.env
echo     PzextrX: ARRAY [1..PzextrImax] OF real; >>recipes.env
echo     PzextrQcol: ARRAY [1..PzextrNmax,1..PzextrNcol] OF real; >>recipes.env
echo     infile,dfile: Text; BcucofWt: ARRAY [1..16,1..16] OF real; >>recipes.env

echo FUNCTION y1(x: real): real; begin end;  >>recipes.env
echo FUNCTION y2(x: real): real; begin end;  >>recipes.env
echo FUNCTION z1(x,y: real): real; begin end;  >>recipes.env
echo FUNCTION z2(x,y: real): real; begin end;  >>recipes.env
echo FUNCTION func(x: real): real; begin end; >>recipes.env
echo FUNCTION fnc(VAR p: RealArrayNP): real; begin end;  >>recipes.env

echo PROCEDURE funcd(x: real; VAR f,df: real); begin end; >>recipes.env
echo PROCEDURE derivs(x: real; VAR v,dvdx: RealArrayNVAR); begin end; >>recipes.env

copy recipes.env+recipes.src recipes.all
p2ada recipes.all >Ada\recipes.adb
rem del recipes.src

copy modfile.pas modfile.src
echo begin end.>>modfile.src
p2ada modfile.src >Ada\modfil.adb
del modfile.src

cd Ada
gnatchop -w demos.ada
if exist d10r1.adb del demos.ada
cd..
