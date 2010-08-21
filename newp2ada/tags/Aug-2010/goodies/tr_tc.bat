@echo off
echo Translation of TeXCAD
echo ---------------------
echo.

if exist *.def del *.def

p2ada DOS.int -EDOS.def >DOS.ads
p2ada Graph.int -EGraph.def >Graph.ads
p2ada MOUSE.PAS -IDOS.def >MOUSE.Ada
p2ada TIMERS.PAS -ETimers.def >TIMERS.Ada
p2ada TC_GLOB.PAS -ITimers.def -IDOS.def -IGraph.def -ETC_Glob.def >TC_GLOB.Ada
bp2p  TC_DRAW.PAS | p2ada -ITC_Glob.def >TC_DRAW.Ada
bp2p  TC_EDIT.PAS | p2ada -ITC_Glob.def >TC_EDIT.Ada
bp2p  TC_IO.PAS   | p2ada -ITC_Glob.def -IDOS.def >TC_IO.Ada
p2ada TC_UTIL.PAS -ITC_Glob.def >TC_UTIL.Ada
p2ada TEXCAD.PAS  >TeXCAD.adb

for %%i in (*.ada) do gnatchop -w %%i

del *.ada
