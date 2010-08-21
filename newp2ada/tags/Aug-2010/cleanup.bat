@echo off
echo Cleanup
del *.c
del *.bak
del *.def
del *.#*
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