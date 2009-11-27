@echo off
echo Cleanup
del *.c
del *.bak
del *.bk.?
del *.def
del *.#*
del *.1
del b~*.*
del nptest?.ad?
del texts.ad*
del lypascal\allada
del lypascal\*.a*
del lypascal\*.bak
del lypascal\*.#*
cd acu
rem cleanacu: see gnatpaqs.zip (cleans ".acu" = {.ali & .o})
cleanacu
cd..