# aflex and ayacc must have been made in their respective directory and copy or link here or adapt following paths
AFLEX_PATH = .
AYACC_PATH = .

VPATH = src:gen:tests

all : p2ada bp2p

test.ada : nptest0.adb nptest1.adb nptest2.adb nptest3.adb nptest4.adb \
nptest5.adb nptest6.adb nptest7.adb nptest8.adb nptest9.adb nptesta.adb \
nptestb.adb nptestc.adb nptestd.adb npteste.adb nptestf.adb
	cd tests; cat nptest?.adb > test.ada

pascal_dfa.adb pascal_dfa.ads pascal_io.adb pascal_io.ads yylex.adb : pascal.l
	cd gen; $(AFLEX_PATH)/aflex -i -E pascal.l
#	./aflex -i -d -E pascal.l
	cd gen; gnatchop -w pascal.ada

pascal_goto.ads pascal_shift_reduce.ads pascal_tokens.ads yyparse.adb : pascal.y
# ayacc file.y C_LEX DEBUG Summary Verbose Error_Recovery
	cd gen; $(AYACC_PATH)/ayacc pascal.y off off
#	./ayacc pascal.y off on on on
	cd gen; gnatchop -w pascal.ada

p2ada : p2ada.adb p2ada_definition_info.adb p2ada_definition_info.ads \
 p2ada_keywords.adb p2ada_keywords.ads p2ada_options.ads \
 pascal_dfa.adb pascal_dfa.ads pascal_goto.ads pascal_io.adb pascal_io.ads \
 pascal_shift_reduce.ads pascal_tokens.ads pascalhelp.adb pascalhelp.ads \
 pascalyylex.ads yy_sizes.ads yyerror.adb yyerror.ads yylex.adb \
 yyparse.adb yyparse.ads yyroutines.adb yyroutines.ads
	gprbuild -P objp2ada p2ada

bp2p : bp2p.adb
	gprbuild -P objp2ada bp2p

clean_p2ada :
	-cd gen; rm pascal_dfa.adb pascal_dfa.ads pascal_io.adb pascal_io.ads yylex.adb pascal.ada
	-cd gen; rm pascal_goto.ads pascal_shift_reduce.ads pascal_tokens.ads yyparse.adb pascal.ada
	-cd bin; rm p2ada bp2p

clean_tests :
	-cd tests; rm nptest?.adb nptest2.* test.ada

nptest0.adb : nptest0.pas
	cd tests; ../bin/p2ada nptest0.pas > nptest0.adb
nptest1.adb : nptest1.pas
# add \$ before $ when option is like -$x+/-
	cd tests; ../bin/bp2p nptest1.pas -dcoucou -\$$z+ | ../bin/p2ada > nptest1.adb
nptest2.adb : nptest2.pas
	-cd tests; rm nptest2.def nptest2.ads
	cd tests; ../bin/p2ada nptest2.pas -enptest2.def > nptest2.adb
	-cd tests; gnatchop -w nptest2.adb
nptest3.adb : nptest3.pas
	cd tests; ../bin/bp2p nptest3.pas -dwindows | ../bin/p2ada > nptest3.adb
nptest4.adb : nptest4.pas
	cd tests; ../bin/p2ada nptest4.pas > nptest4.adb
nptest5.adb : nptest5.pas
	cd tests; ../bin/bp2p nptest5.pas | ../bin/p2ada > nptest5.adb
nptest6.adb : nptest6.pas
	cd tests; ../bin/p2ada nptest6.pas > nptest6.adb
nptest7.adb : nptest7.pas
	cd tests; ../bin/p2ada nptest7.pas > nptest7.adb
nptest8.adb : nptest8.pas
	cd tests; ../bin/p2ada nptest8.pas > nptest8.adb
nptest9.adb : nptest9.pas
	cd tests; ../bin/p2ada nptest9.pas > nptest9.adb
nptesta.adb : nptesta.pas
	cd tests; ../bin/p2ada nptesta.pas > nptesta.adb
nptestb.adb : nptestb.pas nptest2.adb
#	cd tests; ../bin/p2ada nptestb.pas > nptestb.adb
	cd tests; ../bin/p2ada nptestb.pas -inptest2.def > nptestb.adb
nptestc.adb : nptestc.pas
	cd tests; ../bin/p2ada nptestc.pas > nptestc.adb
nptestd.adb : nptestd.pas
	cd tests; ../bin/p2ada nptestd.pas > nptestd.adb
npteste.adb : npteste.pas
	cd tests; ../bin/p2ada npteste.pas > npteste.adb
nptestf.adb : nptestf.pas
	cd tests; ../bin/p2ada nptestf.pas > nptestf.adb
