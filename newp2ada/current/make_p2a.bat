@echo off

echo --  Option: [%1]
echo --
echo --  make_p2a: option -f recreates sources from the pascal.y, pascal.l files
echo --
echo --  The option -f is ONLY needed if you make changes to the Pascal
echo --  grammar in pascal.y, pascal.l .
echo --

if not "%1"=="-f" goto comp
rem
echo 1) Compile AYACC/AFLEX files to Ada sources
echo    NB: the ayacc and aflex tools are included with the p2ada distribution.
rem
cd lypascal
echo .
echo 1.1) Running ayacc.exe
ayacc.exe pascal.y off off on off>..\ayacc.log
type ..\ayacc.log
rem
if exist yargla.a del yargla.a
ren pascal.a yargla.a
rem
echo .
echo 1.2) Running aflex.exe
aflex.exe -i -E pascal.l
echo .
rem
echo 2) Glue everything together and let gnatchop find the right filenames
rem
del gnat.ago
copy *.a* allada.tmp
cd ..
call gnatchop -w lypascal\allada.tmp
rem
:comp
echo 3) gnatmake the new P2Ada
rem
call compile
rem
echo 4) New p2ada.exe is built (or should...) !
