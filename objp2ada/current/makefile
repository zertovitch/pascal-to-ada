CFLAGS = -m -gnatf -g
#CFLAGS = -m -gnatEfowa -gnatVa -fstack-check -g

all : p2ada bp2p

test.ada : nptest0.adb nptest1.adb nptest2.adb nptest3.adb nptest4.adb \
nptest5.adb nptest6.adb nptest7.adb nptest8.adb nptest9.adb nptesta.adb \
nptestb.adb nptestc.adb nptestd.adb npteste.adb nptestf.adb p2ada bp2p
	cat nptest?.adb > test.ada

aflex :../aflex95/aflex.adb            ../aflex95/misc.ads \
../aflex95/aflex_scanner.adb           ../aflex95/misc_defs.adb \
../aflex95/aflex_scanner.ads           ../aflex95/misc_defs.ads \
../aflex95/ascan_dfa.adb               ../aflex95/nfa.adb \
../aflex95/ascan_dfa.ads               ../aflex95/nfa.ads \
../aflex95/ascan_io.adb                ../aflex95/parse_goto.ads \
../aflex95/ascan_io.ads                ../aflex95/parse_shift_reduce.ads \
../aflex95/ccl.adb                     ../aflex95/parse_tokens.ads \
../aflex95/ccl.ads                     ../aflex95/parser.adb \
../aflex95/command_line_interface.adb  ../aflex95/parser.ads \
../aflex95/command_line_interface.ads  ../aflex95/scanner.adb \
../aflex95/dfa.adb                     ../aflex95/scanner.ads \
../aflex95/dfa.ads                     ../aflex95/skeleton_manager.adb \
../aflex95/ecs.adb                     ../aflex95/skeleton_manager.ads \
../aflex95/ecs.ads                     ../aflex95/sym.adb \
../aflex95/external_file_manager.adb   ../aflex95/sym.ads \
../aflex95/external_file_manager.ads   ../aflex95/tblcmp.adb \
../aflex95/file_string.ads             ../aflex95/tblcmp.ads \
../aflex95/gen.adb                     ../aflex95/template_manager.adb \
../aflex95/gen.ads                     ../aflex95/template_manager.ads \
../aflex95/int_io.ads                  ../aflex95/tstring.ads \
../aflex95/main_body.adb               ../aflex95/vstrings.adb \
../aflex95/main_body.ads               ../aflex95/vstrings.ads \
../aflex95/misc.adb
	gnatmake $(CFLAGS) -D acu ../aflex95/aflex

ayacc : ../ayacc95/actions_file.adb ../ayacc95/actions_file.ads \
 ../ayacc95/ayacc-initialize-get_arguments.adb \
 ../ayacc95/ayacc-initialize.adb ../ayacc95/ayacc-print_statistics.adb \
 ../ayacc95/ayacc.adb ../ayacc95/ayacc_file_names.adb \
 ../ayacc95/ayacc_file_names.ads ../ayacc95/command_line_interface-read_command_line.adb \
 ../ayacc95/command_line_interface.adb ../ayacc95/command_line_interface.ads \
 ../ayacc95/error_report_file.adb ../ayacc95/error_report_file.ads \
 ../ayacc95/goto_file.adb ../ayacc95/goto_file.ads \
 ../ayacc95/lalr_symbol_info.adb ../ayacc95/lalr_symbol_info.ads \
 ../ayacc95/lexical_analyzer.adb ../ayacc95/lexical_analyzer.ads \
 ../ayacc95/lists.adb ../ayacc95/lists.ads \
 ../ayacc95/lr0_machine.adb ../ayacc95/lr0_machine.ads \
 ../ayacc95/options.adb ../ayacc95/options.ads \
 ../ayacc95/output_file.adb ../ayacc95/output_file.ads \
 ../ayacc95/parse_table.adb ../ayacc95/parse_table.ads \
 ../ayacc95/parse_template_file.adb ../ayacc95/parse_template_file.ads \
 ../ayacc95/parser.adb ../ayacc95/parser.ads \
 ../ayacc95/ragged.adb ../ayacc95/ragged.ads \
 ../ayacc95/rule_table.adb ../ayacc95/rule_table.ads \
 ../ayacc95/set_pack.adb ../ayacc95/set_pack.ads \
 ../ayacc95/shift_reduce_file.adb ../ayacc95/shift_reduce_file.ads \
 ../ayacc95/source_file.adb ../ayacc95/source_file.ads \
 ../ayacc95/stack_pack.adb ../ayacc95/stack_pack.ads \
 ../ayacc95/stack_pkg.adb ../ayacc95/stack_pkg.ads \
 ../ayacc95/str_pack.adb ../ayacc95/str_pack.ads \
 ../ayacc95/string_lists.ads ../ayacc95/string_pkg.adb \
 ../ayacc95/string_pkg.ads ../ayacc95/string_scanner.adb \
 ../ayacc95/string_scanner.ads ../ayacc95/symbol_info.adb \
 ../ayacc95/symbol_info.ads ../ayacc95/symbol_table.adb \
 ../ayacc95/symbol_table.ads ../ayacc95/tokens_file.adb \
 ../ayacc95/tokens_file.ads ../ayacc95/verbose_file.adb \
 ../ayacc95/verbose_file.ads
	gnatmake $(CFLAGS) -D acu ../ayacc95/ayacc

pascal_dfa.adb pascal_dfa.ads pascal_io.adb pascal_io.ads yylex.adb : pascal.l
	./aflex -i -E pascal.l
#	./aflex -i -d -E pascal.l
	gnatchop -w pascal_io.a pascal_dfa.a pascal.a

pascal_goto.ads pascal_shift_reduce.ads pascal_tokens.ads yyparse.adb : pascal.y
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
