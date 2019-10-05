goto debug

:debug

gnatmake -i %1 -aOACU -g -gnatwa -gnato -gnatVa -gnatecdebug.pra  p2ada  -bargs -Es
gnatmake -i %1 -aOACU -g -gnatwa -gnato -gnatVa -gnatecdebug.pra  bp2p   -bargs -Es

goto end

:release

gnatmake -i %1 -aOACU -g -gnatp -O2  p2ada  -largs -s -bargs -Es
gnatmake -i %1 -aOACU -g -gnatp -O2  bp2p   -largs -s -bargs -Es

:end

del b~*.*