# Minimal or complete checks
#CFLAGS = -m -gnatf -g
CFLAGS = -m -gnatEfowa -gnatVa -fstack-check -g

# aflex and ayacc must have been made in their respective directory and copy or link here

all : p2ada bp2p

test.ada : nptest0.adb nptest1.adb nptest2.adb nptest3.adb nptest4.adb \
nptest5.adb nptest6.adb nptest7.adb nptest8.adb nptest9.adb nptesta.adb \
nptestb.adb nptestc.adb nptestd.adb npteste.adb nptestf.adb p2ada bp2p
	cat nptest?.adb > test.ada

pascal_dfa.adb pascal_dfa.ads pascal_io.adb pascal_io.ads yylex.adb : pascal.l aflex
	./aflex -i -E pascal.l
#	./aflex -i -d -E pascal.l
	gnatchop -w pascal_io.a pascal_dfa.a pascal.a

pascal_goto.ads pascal_shift_reduce.ads pascal_tokens.ads yyparse.adb : pascal.y ayacc
# ayacc file.y C_LEX DEBUG Summary Verbose Error_Recovery
	./ayacc pascal.y off off
#	./ayacc pascal.y off on on on
	gnatchop -w pascal.a

p2ada : p2ada.adb p2ada_definition_info.adb p2ada_definition_info.ads \
 p2ada_keywords.adb p2ada_keywords.ads p2ada_options.ads \
 pascal_dfa.adb pascal_dfa.ads pascal_goto.ads pascal_io.adb pascal_io.ads \
 pascal_shift_reduce.ads pascal_tokens.ads pascalhelp.adb pascalhelp.ads \
 pascalyylex.ads yy_sizes.ads yyerror.adb yyerror.ads yylex.adb \
 yyparse.adb yyparse.ads yyroutines.adb yyroutines.ads
	gnatmake $(CFLAGS) -D acu p2ada

bp2p : bp2p.adb
	gnatmake $(CFLAGS) -D acu bp2p

nptest0.adb : nptest0.pas p2ada
	./p2ada nptest0.pas > nptest0.adb
nptest1.adb : nptest1.pas p2ada bp2p
# add \$ before $ when option is like -$x+/-
	./bp2p nptest1.pas -dcoucou -\$$z+ | ./p2ada > nptest1.adb
nptest2.adb : nptest2.pas p2ada
	rm -f nptest2.def nptest2.ads
	./p2ada nptest2.pas -enptest2.def > nptest2.adb
	-gnatchop -w nptest2.adb
nptest3.adb : nptest3.pas p2ada bp2p
	./bp2p nptest3.pas -dwindows | ./p2ada > nptest3.adb
nptest4.adb : nptest4.pas p2ada
	./p2ada nptest4.pas > nptest4.adb
nptest5.adb : nptest5.pas p2ada bp2p
	./bp2p nptest5.pas | ./p2ada > nptest5.adb
nptest6.adb : nptest6.pas p2ada
	./p2ada nptest6.pas > nptest6.adb
nptest7.adb : nptest7.pas p2ada
	./p2ada nptest7.pas > nptest7.adb
nptest8.adb : nptest8.pas p2ada
	./p2ada nptest8.pas > nptest8.adb
nptest9.adb : nptest9.pas p2ada
	./p2ada nptest9.pas > nptest9.adb
nptesta.adb : nptesta.pas p2ada
	./p2ada nptesta.pas > nptesta.adb
nptestb.adb : nptestb.pas nptest2.adb p2ada
#	./p2ada nptestb.pas > nptestb.adb
	./p2ada nptestb.pas -inptest2.def > nptestb.adb
nptestc.adb : nptestc.pas p2ada
	./p2ada nptestc.pas > nptestc.adb
nptestd.adb : nptestd.pas p2ada
	./p2ada nptestd.pas > nptestd.adb
npteste.adb : npteste.pas p2ada
	./p2ada npteste.pas > npteste.adb
nptestf.adb : nptestf.pas p2ada
	./p2ada nptestf.pas > nptestf.adb
