goto debug

:debug

gnatmake -i %1 -aOACU -g -gnatwa -gnato -gnatVa -gnatecdebug.pra p2ada 
gnatmake -i %1 -aOACU -g -gnatwa -gnato -gnatVa -gnatecdebug.pra bp2p  

goto end

:release

gnatmake -i %1 -aOACU -g -gnatp -O2 p2ada -largs -s
gnatmake -i %1 -aOACU -g -gnatp -O2 bp2p  -largs -s

:end

del b~*.*