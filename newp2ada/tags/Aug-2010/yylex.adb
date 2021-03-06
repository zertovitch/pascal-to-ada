--  Warning: This lexical scanner is automatically generated by AFLEX.
--           It is useless to modify it. Change the ".Y" & ".L" files instead.
with Text_IO; use Text_IO;
with pascal_dfa; use pascal_dfa; 
with pascal_io; use pascal_io; 
--# line 1 "pascal.l"
-- 7-Jan-2007 [GdM]: Added TRY_t, EXCEPT_t, FINALLY_t
-- 26-Dec-2006 [GdM]
--   added eol_in_comment increment (part of line count for input)
-- 15-Nov-2006 [GdM]
-- (B-F) Added //-style comments
-- 31-Dec-2003 [PP]
-- (PO-B) Added constructor, destructor, virtual, private, public, protected tokens
-- (PO-C) Added inherited, override tokens
-- 01-Oct-2003 [PP]
-- (PO) Added OBJECT_t token
-- 31-May-2003 [GdM]
-- (s) figures like "1.e-5" accepted (no '0').
-- 29-Jan-2003 [GdM]
-- (B) Removal of EXIT_t (-> into identifier table)
-- 27-Jan-2003 [GdM]
-- (s) Fixed: record fields can be empty.
-- (s) Removal of CHR_t, ORD_t, EOF_t, EOLN_t (-> into identifier table)
-- 25-Jan-2003 [GdM]
-- (s) Tokens CHAR_t, REAL_t, DOUBLE_t, PARAMSTR_t, PARAMCOUNT_t,
--     UPCASE_t, TRUE_t, FALSE_t, NIL_t
--     removed: Ada aliases of predefined types are now handled
-- 22-Jan-2003 [GdM]
-- (s) (. and .) recognized as [ and ].
-- 14-Jan-2003 [GdM]
--   Token for Absolute (Borland's untyped "renames")
-- 13-Jan-2003 [GdM]
-- * Tokens for Extended Pascal ISO 10206:1990: and_then, or_else
-- * Removed ASM_t : cleanly commented out by BP2P before P2Ada
-- 8,9-Jan-2003 [GdM]
--   Tokens for Borland's ParamCount, ParamStr, UpCase, Inc, Dec
-- 28-Dec-2002 [GdM]
--   LETTER [a-zA-Z_] (was [a-zA-Z])
-- 21,17,15-Dec-2002 [GdM]
--   Tokens for Read, ReadLn, Ord, Chr, New, Sqr, Odd, Eof, Eoln, Double
-- 20-Nov-2002 [GdM]
--   General decimal numbers (integer/float,with optional exponent)
-- 15-Nov-2002 [GACC]
-- - Resolved (* .... ** ....*) bug
-- - Added some exponent litterals (1.2345e7)
-- 16-Jan-2002 [GdM]
-- EXIT_t exit statement (is Ada's return)
-- * GdM xi.1999 *
-- added xor token
-- * GdM 26.viii.1999 *
-- '' handled
-- characters with explicit ascii (#223)
--
-- $Id: Pascal.l,v 2.2 1997/09/20 17:48:31 nestor Exp $
-- $Locker:  $
-- $Log: Pascal.l,v $
-- Revision 2.2  1997/09/20 17:48:31  nestor
-- Correct two problems with comments: remove the original comment
-- delimiters in Ada comments, admit odd number of stars between
-- delimiters in comments of this kind (*****).
--
-- Revision 2.1  1997/08/23  17:16:27  nestor
-- Laurent Gasser's correction for MacOS extended Pascal
-- added tokens: unit, interface, implementation, uses
-- remove reserved keywords in upper-case, use case insensitivity
-- remove definitions of LT [<] and GT [>]
-- add tokens @ and &
-- add hexadecimal notation
--
-- Revision 1.1  1997/08/22  21:00:01  nestor
-- Martin C. Carlisle's original version, standard Pascal
--
-- aflex specification for Pascal
--
-- Martin C. Carlisle, US Air Force Academy
-- mcc@cs.usafa.af.mil
-- http://www.usafa.af.mil/dfcs/bios/carlisle.html
-- November 26, 1996
-- Laurent Gasser, Hirondelle Lumineuse
-- lga@sma.ch
-- June 15, 1997
-- Adaptation to extended MacOS Pascal
--
-- added tokens: unit, interface, implementation, uses
-- remove reserved keywords in upper-case, use case insensitivity
-- remove definitions of LT [<] and GT [>]
-- add tokens @ and &
-- add hexadecimal notation
--
-- usage: aflex -i Pascal.l
--# line 128 "pascal.l"


WITH Pascal_Tokens;
WITH PascalHelp;
USE Pascal_Tokens;

function YYLex return Token is
subtype Short is Integer range -32768..32767;
    yy_act : Integer;
    yy_c   : Short;

-- returned upon end-of-file
YY_END_TOK : constant Integer := 0;
YY_END_OF_BUFFER : constant := 105;
subtype yy_state_type is Integer;
yy_current_state : yy_state_type;
INITIAL : constant := 0;
yy_accept : constant array(0..344) of Short :=
    (   0,
        0,    0,  105,  104,  104,  104,    5,  104,   32,   33,
       26,   18,    9,   19,   12,   27,   98,   16,   13,   23,
       15,   24,    6,    7,    8,   10,   97,   97,   97,   97,
       97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
       97,   97,   97,   97,   97,   97,   97,  104,    4,  101,
      100,    0,  103,    0,    7,   25,   17,    8,   11,    0,
        0,   98,    0,   14,   20,   22,   21,   97,   97,   97,
       97,   97,   97,   97,   97,   97,   66,   97,   97,   97,
       97,   97,   97,   97,   61,   97,   39,   97,   97,   97,
       97,   97,   53,   35,   97,   97,   97,   97,   97,   97,

       97,   97,   97,   97,   40,   97,   97,   97,   97,   97,
       97,   97,   97,   97,   97,    0,    2,    0,  102,    0,
        0,    0,    0,    3,   98,    0,    0,   98,   97,   34,
       97,   97,   97,   97,   77,   97,   28,   97,   97,   44,
       97,   97,   97,   97,   62,   97,   97,   97,   76,   97,
       97,   97,   29,   71,   37,   97,   97,   97,   97,   97,
       97,   97,   97,   97,   97,   97,   55,   30,   31,   70,
       97,   94,   97,   97,   97,   97,   49,   97,   97,   97,
       97,   36,  103,    0,  102,    0,    1,    0,   99,   97,
       97,   97,   97,   54,   97,   97,   97,   68,   97,   97,

       56,   97,   97,   97,   60,   97,   97,   97,   97,   97,
       97,   97,   97,   97,   97,   97,   97,   97,   97,   74,
       97,   97,   67,   48,   78,   97,   79,   97,   97,   45,
       97,  103,    0,   97,   97,   50,   38,   47,   97,   97,
       97,   97,   97,   97,   97,   97,   97,   97,   46,   97,
       97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
       97,   97,   65,   97,   63,   72,   97,   97,   97,   97,
       69,   95,   97,   97,   97,   97,   97,   97,   97,   85,
       97,   97,   97,   52,   97,   97,   97,   97,   90,   75,
       51,   64,   97,   97,   97,   97,   97,   97,   97,   96,

       57,   97,   97,   97,   97,   83,   97,   97,   89,   97,
       41,   97,   88,   73,   84,   82,   97,   97,   42,   59,
       97,   97,   97,   97,   92,   97,   97,   97,   97,   97,
       93,   80,   43,   58,   91,   97,   87,   97,   86,   97,
       97,   97,   81,    0
    ) ;

yy_ec : constant array(ASCII.NUL..Character'Last) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    2,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    3,    4,    1,    5,    6,    7,
        8,    9,   10,   11,   12,   13,   14,   15,   15,   15,
       15,   15,   15,   15,   15,   15,   15,   16,   17,   18,
       19,   20,    1,   21,   26,   27,   28,   29,   30,   31,
       32,   33,   34,   35,   36,   37,   38,   39,   40,   41,
       42,   43,   44,   45,   46,   47,   48,   49,   50,   42,
       22,    1,   23,   24,   25,    1,   26,   27,   28,   29,

       30,   31,   32,   33,   34,   35,   36,   37,   38,   39,
       40,   41,   42,   43,   44,   45,   46,   47,   48,   49,
       50,   42,   51,   52,   53,    1,    1, others=> 1

    ) ;

yy_meta : constant array(0..53) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    2,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    2,    2,    2,    2,    2,    2,
        2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
        2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
        1,    1,    1
    ) ;

yy_base : constant array(0..351) of Short :=
    (   0,
        0,    0,  387,  388,  371,   39,  388,  379,   46,  388,
      375,  388,  388,  363,   48,  368,   45,  362,  388,   43,
      388,  361,  388,  388,  388,  388,    0,   37,  349,   31,
       43,   35,   45,  338,   50,  351,  336,   52,   59,   52,
      345,   63,   67,   55,   71,   78,  334,  320,  388,  357,
       98,  365,  364,  360,  388,  388,  388,  388,  388,  366,
       86,  105,  121,  388,  388,  388,  388,    0,  323,  337,
      322,  332,  319,  323,   59,  314,  312,  315,  329,   87,
      100,  314,  317,  310,    0,  313,  110,  326,  323,  303,
      305,  314,    0,  323,  314,  316,  317,  100,  317,  116,

      298,  104,  299,  311,    0,  290,  298,   85,  308,  294,
      293,  301,  289,  299,  289,  278,  388,  324,  323,  322,
      318,  114,  324,  388,  130,  136,  310,  309,  283,  297,
      295,  286,  289,  274,    0,  272,    0,  277,  285,    0,
      284,  283,  282,  285,  262,  281,  268,  270,    0,  276,
      275,  274,    0,    0,    0,  273,  272,  271,  257,  263,
      251,  121,  260,  267,  255,  264,    0,    0,    0,    0,
      254,    0,  262,  246,  256,  245,    0,  243,  250,  253,
      240,    0,  278,  277,  276,  272,  388,  265,  264,  241,
      232,  226,  236,    0,  229,  230,  227,    0,  230,  227,

        0,  232,  242,  222,    0,  236,  222,  221,  226,  234,
      224,  217,  216,  228,  231,  226,  212,  224,  219,  215,
      208,  224,    0,    0,    0,  212,    0,  202,  217,    0,
      216,  239,  238,  197,  209,    0,    0,  198,  194,  199,
      193,  198,  199,  192,  200,  195,  198,  200,    0,  185,
      185,  180,  193,  197,  180,  195,  197,  194,  193,  181,
      190,  173,    0,  191,    0,  179,  170,  184,  167,  184,
        0,    0,  185,  160,  180,  168,  177,  161,  179,    0,
      174,  169,  173,    0,  171,  154,  161,  153,    0,    0,
        0,    0,  160,  157,  165,  155,  165,  147,  154,    0,

        0,  151,  150,  158,  159,    0,  142,  155,    0,  141,
        0,  153,    0,    0,    0,    0,  137,  141,    0,    0,
      135,  150,  135,  134,    0,  133,  133,  121,  116,  132,
        0,    0,    0,    0,    0,  113,    0,  109,    0,  118,
      110,   75,    0,  388,  166,  107,  168,  170,  172,  174,
      176
    ) ;

yy_def : constant array(0..351) of Short :=
    (   0,
      344,    1,  344,  344,  344,  344,  344,  345,  344,  344,
      344,  344,  344,  344,  344,  344,  344,  344,  344,  344,
      344,  344,  344,  344,  344,  344,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  347,  344,  344,
      344,  348,  344,  349,  344,  344,  344,  344,  344,  350,
      344,  344,  344,  344,  344,  344,  344,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,

      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  347,  344,  348,  344,  348,
      349,  351,  350,  344,  344,  344,  344,  344,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  344,  348,  344,  349,  344,  344,  344,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,

      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  344,  348,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,

      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
      346,  346,  346,    0,  344,  344,  344,  344,  344,  344,
      344
    ) ;

yy_nxt : constant array(0..441) of Short :=
    (   0,
        4,    4,    5,    6,    7,    8,    9,   10,   11,   12,
       13,   14,   15,   16,   17,   18,   19,   20,   21,   22,
       23,   24,   25,   26,   27,   28,   29,   30,   31,   32,
       33,   34,   27,   35,   27,   27,   36,   37,   38,   39,
       40,   27,   41,   42,   43,   44,   45,   46,   47,   27,
       48,   49,    4,   51,   54,   58,   73,   61,   55,   62,
       59,   65,   66,   69,   51,   51,   51,   51,   51,   51,
       74,   78,   75,   79,   63,   70,   76,   97,   81,   71,
       85,   90,   77,   80,   82,   92,  135,   86,   87,   93,
       83,   91,  101,  108,   98,  102,  110,   99,  109,  104,

      125,   94,  136,   95,  111,   96,  105,  103,   68,  106,
      112,  113,   51,  343,  141,  126,  107,   61,  174,   62,
      114,  187,  122,   51,   51,   51,   51,   51,   51,  175,
      127,  142,  127,  161,   63,  128,  143,  149,  144,  162,
      168,  164,  150,  165,  125,  188,  169,  188,  216,  342,
      189,  341,  217,  340,  151,  339,  166,  338,  337,   63,
      336,  335,  334,  333,  332,  218,   52,   52,  116,  116,
      118,  118,  121,  121,  123,  123,  186,  186,  331,  330,
      329,  328,  327,  326,  325,  324,  323,  322,  321,  320,
      319,  318,  317,  316,  315,  314,  313,  312,  311,  310,

      309,  308,  307,  306,  305,  304,  303,  302,  301,  300,
      299,  298,  297,  296,  295,  294,  293,  292,  291,  290,
      289,  288,  287,  286,  285,  284,  283,  282,  281,  280,
      279,  278,  277,  276,  275,  274,  273,  272,  271,  270,
      269,  268,  267,  232,  233,  266,  265,  264,  263,  262,
      261,  260,  259,  258,  257,  256,  255,  254,  253,  252,
      251,  250,  249,  248,  247,  246,  245,  244,  243,  242,
      241,  240,  239,  238,  237,  236,  235,  234,  189,  189,
      122,  233,  232,  184,  231,  230,  229,  228,  227,  226,
      225,  224,  223,  222,  221,  220,  219,  215,  214,  213,

      212,  211,  210,  209,  208,  207,  206,  205,  204,  203,
      202,  201,  200,  199,  198,  197,  196,  195,  194,  193,
      192,  191,  190,  128,  128,  124,  122,  185,  184,  183,
      117,  182,  181,  180,  179,  178,  177,  176,  173,  172,
      171,  170,  167,  163,  160,  159,  158,  157,  156,  155,
      154,  153,  152,  148,  147,  146,  145,  140,  139,  138,
      137,  134,  133,  132,  131,  130,  129,  124,  122,  120,
      119,   50,  117,  115,  100,   89,   88,   84,   72,   67,
       64,   60,   57,   56,   53,   50,  344,    3,  344,  344,
      344,  344,  344,  344,  344,  344,  344,  344,  344,  344,

      344,  344,  344,  344,  344,  344,  344,  344,  344,  344,
      344,  344,  344,  344,  344,  344,  344,  344,  344,  344,
      344,  344,  344,  344,  344,  344,  344,  344,  344,  344,
      344,  344,  344,  344,  344,  344,  344,  344,  344,  344,
      344
    ) ;

yy_chk : constant array(0..441) of Short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    6,    9,   15,   30,   17,    9,   17,
       15,   20,   20,   28,    6,    6,    6,    6,    6,    6,
       30,   32,   31,   32,   17,   28,   31,   40,   33,   28,
       35,   38,   31,   32,   33,   39,   75,   35,   35,   39,
       33,   38,   42,   44,   40,   42,   45,   40,   44,   43,

       61,   39,   75,   39,   45,   39,   43,   42,  346,   43,
       46,   46,   51,  342,   80,   61,   43,   62,  108,   62,
       46,  122,  122,   51,   51,   51,   51,   51,   51,  108,
       63,   80,   63,   98,   62,   63,   81,   87,   81,   98,
      102,  100,   87,  100,  125,  126,  102,  126,  162,  341,
      126,  340,  162,  338,   87,  336,  100,  330,  329,  125,
      328,  327,  326,  324,  323,  162,  345,  345,  347,  347,
      348,  348,  349,  349,  350,  350,  351,  351,  322,  321,
      318,  317,  312,  310,  308,  307,  305,  304,  303,  302,
      299,  298,  297,  296,  295,  294,  293,  288,  287,  286,

      285,  283,  282,  281,  279,  278,  277,  276,  275,  274,
      273,  270,  269,  268,  267,  266,  264,  262,  261,  260,
      259,  258,  257,  256,  255,  254,  253,  252,  251,  250,
      248,  247,  246,  245,  244,  243,  242,  241,  240,  239,
      238,  235,  234,  233,  232,  231,  229,  228,  226,  222,
      221,  220,  219,  218,  217,  216,  215,  214,  213,  212,
      211,  210,  209,  208,  207,  206,  204,  203,  202,  200,
      199,  197,  196,  195,  193,  192,  191,  190,  189,  188,
      186,  185,  184,  183,  181,  180,  179,  178,  176,  175,
      174,  173,  171,  166,  165,  164,  163,  161,  160,  159,

      158,  157,  156,  152,  151,  150,  148,  147,  146,  145,
      144,  143,  142,  141,  139,  138,  136,  134,  133,  132,
      131,  130,  129,  128,  127,  123,  121,  120,  119,  118,
      116,  115,  114,  113,  112,  111,  110,  109,  107,  106,
      104,  103,  101,   99,   97,   96,   95,   94,   92,   91,
       90,   89,   88,   86,   84,   83,   82,   79,   78,   77,
       76,   74,   73,   72,   71,   70,   69,   60,   54,   53,
       52,   50,   48,   47,   41,   37,   36,   34,   29,   22,
       18,   16,   14,   11,    8,    5,    3,  344,  344,  344,
      344,  344,  344,  344,  344,  344,  344,  344,  344,  344,

      344,  344,  344,  344,  344,  344,  344,  344,  344,  344,
      344,  344,  344,  344,  344,  344,  344,  344,  344,  344,
      344,  344,  344,  344,  344,  344,  344,  344,  344,  344,
      344,  344,  344,  344,  344,  344,  344,  344,  344,  344,
      344
    ) ;


-- copy whatever the last rule matched to the standard output

procedure ECHO is
begin
   if Text_IO.is_open(user_output_file) then
     Text_IO.put( user_output_file, yytext );
   else
     Text_IO.put( yytext );
   end if;
end ECHO;

-- enter a start condition.
-- Using procedure requires a () after the ENTER, but makes everything
-- much neater.

procedure ENTER( state : integer ) is
begin
     yy_start := 1 + 2 * state;
end ENTER;

-- action number for EOF rule of a given start state
function YY_STATE_EOF(state : integer) return integer is
begin
     return YY_END_OF_BUFFER + state + 1;
end YY_STATE_EOF;

-- return all but the first 'n' matched characters back to the input stream
procedure yyless(n : integer) is
begin
        yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
        yy_cp := yy_bp + n;
        yy_c_buf_p := yy_cp;
        YY_DO_BEFORE_ACTION; -- set up yytext again
end yyless;

-- redefine this if you have something you want each time.
procedure YY_USER_ACTION is
begin
        null;
end;

-- yy_get_previous_state - get the state just before the EOB char was reached

function yy_get_previous_state return yy_state_type is
    yy_current_state : yy_state_type;
    yy_c : short;
begin
    yy_current_state := yy_start;

    for yy_cp in yytext_ptr..yy_c_buf_p - 1 loop
	yy_c := yy_ec(yy_ch_buf(yy_cp));
	if yy_accept(yy_current_state) /= 0 then
	    yy_last_accepting_state := yy_current_state;
	    yy_last_accepting_cpos := yy_cp;
	end if;
	while yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state loop
	    yy_current_state := yy_def(yy_current_state);
	    if ( yy_current_state >= 345 ) then
		yy_c := yy_meta(yy_c);
	    end if;
	end loop;
	yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
    end loop;

    return yy_current_state;
end yy_get_previous_state;

procedure yyrestart( input_file : file_type ) is
begin
   open_input(Text_IO.name(input_file));
end yyrestart;

begin -- of YYLex
<<new_file>>
        -- this is where we enter upon encountering an end-of-file and
        -- yywrap() indicating that we should continue processing

    if yy_init then
        if yy_start = 0 then
            yy_start := 1;      -- first start state
        end if;

        -- we put in the '\n' and start reading from [1] so that an
        -- initial match-at-newline will be true.

        yy_ch_buf(0) := ASCII.LF;
        yy_n_chars := 1;

        -- we always need two end-of-buffer characters. The first causes
        -- a transition to the end-of-buffer state. The second causes
        -- a jam in that state.

        yy_ch_buf(yy_n_chars) := YY_END_OF_BUFFER_CHAR;
        yy_ch_buf(yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;

        yy_eof_has_been_seen := false;

        yytext_ptr := 1;
        yy_c_buf_p := yytext_ptr;
        yy_hold_char := yy_ch_buf(yy_c_buf_p);
        yy_init := false;
-- UMASS CODES :
--   Initialization
        tok_begin_line := 1;
        tok_end_line := 1;
        tok_begin_col := 0;
        tok_end_col := 0;
        token_at_end_of_line := false;
        line_number_of_saved_tok_line1 := 0;
        line_number_of_saved_tok_line2 := 0;
-- END OF UMASS CODES.
    end if; -- yy_init

    loop                -- loops until end-of-file is reached

-- UMASS CODES :
--    if last matched token is end_of_line, we must
--    update the token_end_line and reset tok_end_col.
    if Token_At_End_Of_Line then
      Tok_End_Line := Tok_End_Line + 1;
      Tok_End_Col := 0;
      Token_At_End_Of_Line := False;
    end if;
-- END OF UMASS CODES.

        yy_cp := yy_c_buf_p;

        -- support of yytext
        yy_ch_buf(yy_cp) := yy_hold_char;

        -- yy_bp points to the position in yy_ch_buf of the start of the
        -- current run.
	yy_bp := yy_cp;
	yy_current_state := yy_start;
	loop
		yy_c := yy_ec(yy_ch_buf(yy_cp));
		if yy_accept(yy_current_state) /= 0 then
		    yy_last_accepting_state := yy_current_state;
		    yy_last_accepting_cpos := yy_cp;
		end if;
		while yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state loop
		    yy_current_state := yy_def(yy_current_state);
		    if ( yy_current_state >= 345 ) then
			yy_c := yy_meta(yy_c);
		    end if;
		end loop;
		yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
	    yy_cp := yy_cp + 1;
if ( yy_current_state = 344 ) then
    exit;
end if;
	end loop;
	yy_cp := yy_last_accepting_cpos;
	yy_current_state := yy_last_accepting_state;

<<next_action>>
	    yy_act := yy_accept(yy_current_state);
            YY_DO_BEFORE_ACTION;
            YY_USER_ACTION;

        if aflex_debug then  -- output acceptance info. for (-d) debug mode
            Text_IO.put( Standard_Error, "--accepting rule #" );
            Text_IO.put( Standard_Error, INTEGER'IMAGE(yy_act) );
            Text_IO.put_line( Standard_Error, "(""" & yytext & """)");
        end if;

-- UMASS CODES :
--   Update tok_begin_line, tok_end_line, tok_begin_col and tok_end_col
--   after matching the token.
        if yy_act /= YY_END_OF_BUFFER and then yy_act /= 0 then
-- Token are matched only when yy_act is not yy_end_of_buffer or 0.
          Tok_Begin_Line := Tok_End_Line;
          Tok_Begin_Col := Tok_End_Col + 1;
          Tok_End_Col := Tok_Begin_Col + yy_cp - yy_bp - 1;
          if yy_ch_buf ( yy_bp ) = ASCII.LF then
            Token_At_End_Of_Line := True;
          end if;
        end if;
-- END OF UMASS CODES.

<<do_action>>   -- this label is used only to access EOF actions
            case yy_act is
		when 0 => -- must backtrack
		-- undo the effects of YY_DO_BEFORE_ACTION
		yy_ch_buf(yy_cp) := yy_hold_char;
		yy_cp := yy_last_accepting_cpos;
		yy_current_state := yy_last_accepting_state;
		goto next_action;



--------------
-- Comments --
--------------
-- Standard Pascal comments:
-- (* ... *)
when 1 => 
--# line 136 "pascal.l"

         PascalHelp.Comment(YYText(3..YYLength-2));
         

-- { ... }
when 2 => 
--# line 140 "pascal.l"
PascalHelp.Comment(YYText(2..YYLength-1));

-- Delphi, FreePascal comments:
when 3 => 
--# line 143 "pascal.l"
 -- Skip rest of line.
                      PascalHelp.Comment(YYText(3..YYLength-1));
                      PascalHelp.eol_in_comment:=
                        PascalHelp.eol_in_comment + 1; -- catch the \n
                    

-- Regular pascal stuff
when 4 => 
--# line 151 "pascal.l"
return BAR_t;

when 5 => 
--# line 152 "pascal.l"
return AMPERSAND_t;

when 6 => 
--# line 153 "pascal.l"
return AT_t;

when 7 => 
--# line 154 "pascal.l"
return LBRACK_t;

when 8 => 
--# line 155 "pascal.l"
return RBRACK_t;

when 9 => 
--# line 156 "pascal.l"
 return COMMA_t; 

when 10 => 
--# line 157 "pascal.l"
 return UPARROW_t; 

when 11 => 
--# line 158 "pascal.l"
 return DOUBLEDOT_t; 

when 12 => 
--# line 159 "pascal.l"
 return PERIOD_t; 

when 13 => 
--# line 160 "pascal.l"
 return SEMICOLON_t; 

when 14 => 
--# line 161 "pascal.l"
return ASSIGN_t;

when 15 => 
--# line 162 "pascal.l"
return EQUAL_t;

when 16 => 
--# line 163 "pascal.l"
 return COLON_t; 

when 17 => 
--# line 164 "pascal.l"
 return ARROW_t; 

when 18 => 
--# line 165 "pascal.l"
 return PLUS_t; 

when 19 => 
--# line 166 "pascal.l"
 return MINUS_t; 

when 20 => 
--# line 167 "pascal.l"
 return LE_t; 

when 21 => 
--# line 168 "pascal.l"
 return GE_t; 

when 22 => 
--# line 169 "pascal.l"
 return NE_t; 

when 23 => 
--# line 170 "pascal.l"
 return LT_t; 

when 24 => 
--# line 171 "pascal.l"
 return GT_t; 

when 25 => 
--# line 172 "pascal.l"
 return DOUBLESTAR_t; 

when 26 => 
--# line 173 "pascal.l"
 return TIMES_t; 

when 27 => 
--# line 174 "pascal.l"
 return DIVIDE_t; 

when 28 => 
--# line 175 "pascal.l"
return DIV_t; 

when 29 => 
--# line 176 "pascal.l"
return MOD_t; 

when 30 => 
--# line 177 "pascal.l"
return SHL_t; 

when 31 => 
--# line 178 "pascal.l"
return SHR_t; 

when 32 => 
--# line 179 "pascal.l"
return LPAREN_t;

when 33 => 
--# line 180 "pascal.l"
return RPAREN_t;

when 34 => 
--# line 181 "pascal.l"
return AND_t;

when 35 => 
--# line 182 "pascal.l"
return OR_t;

when 36 => 
--# line 183 "pascal.l"
return XOR_t;

when 37 => 
--# line 184 "pascal.l"
return NOT_t;

when 38 => 
--# line 185 "pascal.l"
 return BEGIN_t; 

when 39 => 
--# line 186 "pascal.l"
 return IN_t; 

when 40 => 
--# line 187 "pascal.l"
 return TO_t; 

when 41 => 
--# line 188 "pascal.l"
 return PROGRAM_t; 

when 42 => 
--# line 189 "pascal.l"
 return EXTERNAL_t; 

when 43 => 
--# line 190 "pascal.l"
 return OTHERWISE_t; 

when 44 => 
--# line 191 "pascal.l"
 return END_t; 

when 45 => 
--# line 192 "pascal.l"
 return WITH_t; 

when 46 => 
--# line 193 "pascal.l"
 return LABEL_t; 

when 47 => 
--# line 194 "pascal.l"
 return CONST_t; 

when 48 => 
--# line 195 "pascal.l"
 return TYPE_t; 

when 49 => 
--# line 196 "pascal.l"
 return VAR_t; 

when 50 => 
--# line 197 "pascal.l"
 return ARRAY_t; 

when 51 => 
--# line 198 "pascal.l"
 return RECORD_t; 

when 52 => 
--# line 199 "pascal.l"
 return PACKED_t; 

when 53 => 
--# line 200 "pascal.l"
 return OF_t; 

when 54 => 
--# line 201 "pascal.l"
 return CASE_t; 

when 55 => 
--# line 202 "pascal.l"
 return SET_t; 

when 56 => 
--# line 203 "pascal.l"
 return FILE_t; 

when 57 => 
--# line 204 "pascal.l"
 return FORWARD_t; 

when 58 => 
--# line 205 "pascal.l"
 return PROCEDURE_t; 

when 59 => 
--# line 206 "pascal.l"
 return FUNCTION_t; 

when 60 => 
--# line 207 "pascal.l"
 return GOTO_t; 

when 61 => 
--# line 208 "pascal.l"
 return IF_t; 

when 62 => 
--# line 209 "pascal.l"
 return FOR_t; 

when 63 => 
--# line 210 "pascal.l"
 return WHILE_t; 

when 64 => 
--# line 211 "pascal.l"
 return REPEAT_t; 

when 65 => 
--# line 212 "pascal.l"
 return UNTIL_t; 

when 66 => 
--# line 213 "pascal.l"
 return DO_t; 

when 67 => 
--# line 214 "pascal.l"
 return THEN_t; 

when 68 => 
--# line 215 "pascal.l"
 return ELSE_t; 

when 69 => 
--# line 216 "pascal.l"
 return DOWNTO_t; 

when 70 => 
--# line 217 "pascal.l"
 return STR_t; 

when 71 => 
--# line 218 "pascal.l"
 return NEW_t; 

when 72 => 
--# line 219 "pascal.l"
 return WRITE_t; 

when 73 => 
--# line 220 "pascal.l"
 return WRITELN_t; 

when 74 => 
--# line 221 "pascal.l"
 return READ_t; 

when 75 => 
--# line 222 "pascal.l"
 return READLN_t; 

when 76 => 
--# line 223 "pascal.l"
return INC_t;

when 77 => 
--# line 224 "pascal.l"
return DEC_t;

when 78 => 
--# line 225 "pascal.l"
return UNIT_t;

when 79 => 
--# line 226 "pascal.l"
return USES_t;

when 80 => 
--# line 227 "pascal.l"
return INTERFACE_t;

when 81 => 
--# line 228 "pascal.l"
return IMPLEMENTATION_t;

when 82 => 
--# line 229 "pascal.l"
return AND_THEN_t;

when 83 => 
--# line 230 "pascal.l"
return OR_ELSE_t;

when 84 => 
--# line 231 "pascal.l"
return ABSOLUTE_t;

when 85 => 
--# line 232 "pascal.l"
return OBJECT_t;

when 86 => 
--# line 233 "pascal.l"
return CONSTRUCTOR_t;

when 87 => 
--# line 234 "pascal.l"
return DESTRUCTOR_t;

when 88 => 
--# line 235 "pascal.l"
return VIRTUAL_t;

when 89 => 
--# line 236 "pascal.l"
return PRIVATE_t;

when 90 => 
--# line 237 "pascal.l"
return PUBLIC_t;

when 91 => 
--# line 238 "pascal.l"
return PROTECTED_t;

when 92 => 
--# line 239 "pascal.l"
return OVERRIDE_t;

when 93 => 
--# line 240 "pascal.l"
return INHERITED_t;

when 94 => 
--# line 241 "pascal.l"
return TRY_t;

when 95 => 
--# line 242 "pascal.l"
return EXCEPT_t;

when 96 => 
--# line 243 "pascal.l"
return FINALLY_t;

when 97 => 
--# line 245 "pascal.l"
return ID_t;

when 98 => 
--# line 246 "pascal.l"
return CONSTANT_t;

when 99 => 
--# line 247 "pascal.l"
return CONSTANT_t;

when 100 => 
--# line 248 "pascal.l"
return HEXADECIMAL_t;

when 101 => 
--# line 249 "pascal.l"
return ASCII_t;

when 102 => 
--# line 250 "pascal.l"
return CHAR_CONST_t;

when 103 => 
--# line 251 "pascal.l"
return STRING_t;

when 104 => 
--# line 253 "pascal.l"
ECHO;
when YY_END_OF_BUFFER + INITIAL + 1 => 
    return End_Of_Input;
                when YY_END_OF_BUFFER =>
                    -- undo the effects of YY_DO_BEFORE_ACTION
                    yy_ch_buf(yy_cp) := yy_hold_char;

                    yytext_ptr := yy_bp;

                    case yy_get_next_buffer is
                        when EOB_ACT_END_OF_FILE =>
                            begin
                            if yywrap then
                                -- note: because we've taken care in
                                -- yy_get_next_buffer() to have set up yytext,
                                -- we can now set up yy_c_buf_p so that if some
                                -- total hoser (like aflex itself) wants
                                -- to call the scanner after we return the
                                -- End_Of_Input, it'll still work - another
                                -- End_Of_Input will get returned.

                                yy_c_buf_p := yytext_ptr;

                                yy_act := YY_STATE_EOF((yy_start - 1) / 2);

                                goto do_action;
                            else
                                --  start processing a new file
                                yy_init := true;
                                goto new_file;
                            end if;
                            end;
                        when EOB_ACT_RESTART_SCAN =>
                            yy_c_buf_p := yytext_ptr;
                            yy_hold_char := yy_ch_buf(yy_c_buf_p);
                        when EOB_ACT_LAST_MATCH =>
                            yy_c_buf_p := yy_n_chars;
                            yy_current_state := yy_get_previous_state;

                            yy_cp := yy_c_buf_p;
                            yy_bp := yytext_ptr;
                            goto next_action;
                        when others => null;
                        end case; -- case yy_get_next_buffer()
                when others =>
                    Text_IO.put( "action # " );
                    Text_IO.put( INTEGER'IMAGE(yy_act) );
                    Text_IO.new_line;
                    raise AFLEX_INTERNAL_ERROR;
            end case; -- case (yy_act)
        end loop; -- end of loop waiting for end of file
end YYLex;
