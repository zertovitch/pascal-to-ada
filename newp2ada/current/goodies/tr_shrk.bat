@echo off
echo Translation of Shrink.pas (.zip archiver with Shrink (LZW) method)
echo ------------------------------------------------------------------
echo.

del *.ada

if exist *.def del *.def

p2ada DOS.int -EDOS.def >DOS.ads
p2ada MEMALLOC.PAS -EMEMALLOC.def >MEMALLOC.Ada
p2ada STRPROCS.PAS -ESTRPROCS.def >STRPROCS.Ada

p2ada SHRINK.PAS -IDOS.def -IMEMALLOC.def -ISTRPROCS.def >Shrink.Ada

for %%i in (*.ada) do gnatchop -w %%i
