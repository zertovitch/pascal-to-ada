@echo on

if exist nptest2.def del nptest2.def

p2ada  nptest0.pas  >nptest0.adb
bp2p   nptest1.pas  /DCoucou /$Z+ | p2ada >nptest1.adb
p2ada <nptest2.pas  -Enptest2.def >nptest2.ada
bp2p   nptest3.pas  /Dwindows | p2ada >nptest3.adb
p2ada <nptest4.pas  >nptest4.adb
bp2p   nptest5.pas  | p2ada >nptest5.adb
p2ada  nptest6.pas  >nptest6.adb
p2ada  nptest7.pas  >nptest7.adb
p2ada  nptest8.pas  >nptest8.adb
p2ada  nptest9.pas  >nptest9.adb
p2ada  nptesta.pas  >nptesta.adb
p2ada  nptestb.pas  -Inptest2.def >nptestb.adb
p2ada  nptestc.pas  >nptestc.adb
p2ada  nptestd.pas  >nptestd.adb
p2ada  npteste.pas  >npteste.adb

copy nptest?.ad? t.txt

cut t.txt t.txt /C=250 
