package Pascal_Goto is

    type Small_Integer is range -32_000 .. 32_000;

    type Goto_Entry is record
        Nonterm  : Small_Integer;
        Newstate : Small_Integer;
    end record;

  --pragma suppress(index_check);

    subtype Row is Integer range -1 .. Integer'Last;

    type Goto_Parse_Table is array (Row range <>) of Goto_Entry;

    Goto_Matrix : constant Goto_Parse_Table :=
       ((-1,-1)  -- Dummy Entry.
-- State  0
,(-50,8),(-44,5),(-43,4),(-5,3),(-4,2),(-3,1),(-2,9)
-- State  3
,(-10,12),(-9,11),(-8,10),(-6,15)
-- State  6
,(-11,17)
-- State  7
,(-45,18)
-- State  8
,(-49,23),(-26,21),(-25,20),(-15,19)
-- State  11
,(-13,25)
-- State  13
,(-18,26)
-- State  14
,(-14,27)
-- State  15
,(-7,29)
-- State  17
,(-12,30)
-- State  18
,(-420,32),(-46,38)
-- State  19
,(-230,57),(-227,54),(-226,53),(-225,52),(-224,51),(-51,62),(-42,56),(-41,55),(-38,46),(-37,42),(-36,41)
,(-35,40),(-33,45),(-32,44),(-31,43),(-20,39)
-- State  20
,(-26,63)
-- State  22
,(-27,64)
-- State  25
,(-8,65)
-- State  26
,(-26,21),(-25,20),(-15,66)
-- State  27
,(-26,21),(-25,20),(-15,67)
-- State  38
,(-47,70)
-- State  39
,(-59,72)
-- State  40
,(-230,57),(-227,54),(-226,53),(-225,52),(-224,51),(-42,56),(-41,55),(-38,46),(-37,42),(-36,73),(-33,45)
,(-32,44),(-31,43)
-- State  47
,(-62,75),(-60,74)
-- State  48
,(-67,79),(-66,78),(-65,77)
-- State  49
,(-115,80)
-- State  50
,(-218,83),(-217,82),(-216,81)
-- State  57
,(-237,87),(-213,89)
-- State  58
,(-268,90)
-- State  59
,(-263,91)
-- State  60
,(-264,92)
-- State  61
,(-265,93)
-- State  62
,(-52,94)
-- State  64
,(-420,98),(-54,96),(-28,95)
-- State  66
,(-19,99)
-- State  67
,(-16,100)
-- State  70
,(-48,102)
-- State  71
,(-295,103)
-- State  74
,(-61,106)
-- State  77
,(-67,79),(-66,107)
-- State  79
,(-420,32),(-46,108)
-- State  80
,(-118,111),(-117,110),(-116,109)
-- State  81
,(-218,83),(-217,112)
-- State  83
,(-420,32),(-58,114),(-56,113),(-46,115)
-- State  84
,(-230,57),(-227,54),(-226,53),(-225,116),(-42,56),(-41,55)
-- State  85
,(-228,117)
-- State  86
,(-266,118)
-- State  88
,(-238,119)
-- State  90
,(-262,122),(-256,121),(-255,120)
-- State  91
,(-262,123),(-256,121),(-255,120)
-- State  92
,(-262,124),(-256,121),(-255,120)
-- State  93
,(-262,125),(-256,121),(-255,120)
-- State  99
,(-230,57),(-227,54),(-226,53),(-225,52),(-224,51),(-42,56),(-41,55),(-38,46),(-37,42),(-36,41),(-35,40)
,(-33,45),(-32,44),(-31,43),(-20,129)
-- State  100
,(-230,57),(-42,139),(-41,138),(-40,137),(-39,136),(-34,135),(-33,134),(-32,133),(-31,132),(-30,131),(-29,130)
,(-17,140)
-- State  101
,(-420,98),(-54,142),(-53,141)
-- State  103
,(-300,144),(-276,149),(-274,148),(-273,146),(-63,147),(-24,145)
-- State  106
,(-62,151)
-- State  108
,(-68,152)
-- State  109
,(-118,111),(-117,153)
-- State  111
,(-420,32),(-46,154)
-- State  117
,(-235,161),(-234,160),(-232,158),(-231,157),(-229,163)
-- State  118
,(-267,166),(-235,161),(-234,160),(-232,165),(-231,164)
-- State  119
,(-420,32),(-246,173),(-245,172),(-244,171),(-243,170),(-242,169),(-241,168),(-239,167),(-230,57),(-58,114),(-56,174)
,(-46,115),(-42,178),(-41,177)
-- State  120
,(-261,182),(-260,180),(-258,181),(-257,179)
-- State  121
,(-420,32),(-46,183)
-- State  122
,(-269,184)
-- State  128
,(-55,185)
-- State  129
,(-22,186),(-21,188)
-- State  130
,(-230,57),(-42,139),(-41,138),(-40,137),(-39,136),(-34,135),(-33,134),(-32,133),(-31,132),(-30,189)
-- State  143
,(-49,194),(-26,21),(-25,20),(-15,19)
-- State  148
,(-420,98),(-403,222),(-401,220),(-400,213),(-337,215),(-329,207),(-328,206),(-327,205),(-326,204),(-325,203),(-324,202)
,(-280,200),(-279,199),(-278,198),(-275,223),(-171,201),(-102,221),(-54,214)
-- State  149
,(-287,231),(-286,230),(-285,229),(-284,228),(-283,227),(-282,226),(-281,225),(-277,239),(-59,224)
-- State  152
,(-69,242)
-- State  155
,(-57,244)
-- State  156
,(-219,245)
-- State  158
,(-235,247),(-230,57),(-227,54),(-226,53),(-225,52),(-224,51),(-51,248),(-42,56),(-41,55),(-38,46),(-37,42)
,(-36,41),(-35,40),(-33,45),(-32,44),(-31,43),(-20,39)
-- State  162
,(-236,249)
-- State  165
,(-235,247),(-230,57),(-227,54),(-226,53),(-225,52),(-224,51),(-51,250),(-42,56),(-41,55),(-38,46),(-37,42)
,(-36,41),(-35,40),(-33,45),(-32,44),(-31,43),(-20,39)
-- State  175
,(-420,32),(-58,114),(-56,254),(-46,115)
-- State  176
,(-420,32),(-58,114),(-56,255),(-46,115)
-- State  184
,(-237,87),(-213,257)
-- State  185
,(-420,98),(-54,258)
-- State  187
,(-23,259)
-- State  193
,(-420,98),(-54,260)
-- State  196
,(-300,261),(-276,149),(-274,148),(-273,146),(-63,147)
-- State  197
,(-276,149),(-274,148),(-273,262)
-- State  201
,(-291,266),(-290,264),(-289,263),(-288,267)
-- State  208
,(-330,268)
-- State  209
,(-332,269)
-- State  210
,(-333,270)
-- State  211
,(-335,271)
-- State  212
,(-376,272)
-- State  213
,(-339,275)
-- State  214
,(-336,278)
-- State  215
,(-338,279)
-- State  216
,(-352,280)
-- State  232
,(-319,284)
-- State  233
,(-377,285)
-- State  234
,(-303,286)
-- State  235
,(-306,287)
-- State  236
,(-308,288)
-- State  237
,(-315,289)
-- State  238
,(-296,290)
-- State  240
,(-70,291)
-- State  241
,(-72,292)
-- State  243
,(-119,293)
-- State  244
,(-420,32),(-58,294),(-46,115)
-- State  245
,(-420,98),(-125,298),(-121,297),(-120,296),(-73,299),(-54,295)
-- State  246
,(-233,301)
-- State  252
,(-240,303)
-- State  253
,(-247,304)
-- State  254
,(-249,306)
-- State  255
,(-253,308)
-- State  256
,(-259,309)
-- State  259
,(-300,144),(-276,149),(-274,148),(-273,146),(-63,147),(-24,311)
-- State  265
,(-293,312)
-- State  266
,(-339,313),(-292,314)
-- State  268
,(-331,316)
-- State  269
,(-331,317)
-- State  270
,(-334,319)
-- State  271
,(-334,320)
-- State  272
,(-64,322)
-- State  273
,(-356,323)
-- State  274
,(-402,324)
-- State  276
,(-404,325)
-- State  278
,(-339,313),(-292,326)
-- State  279
,(-339,313),(-292,327)
-- State  280
,(-353,329)
-- State  281
,(-360,330)
-- State  282
,(-363,331)
-- State  283
,(-367,332)
-- State  284
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-305,360)
,(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,333),(-54,357)
-- State  285
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-378,362),(-344,334),(-337,359)
,(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,361),(-54,357)
-- State  286
,(-300,144),(-276,149),(-274,148),(-273,146),(-63,147),(-24,363)
-- State  287
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-305,364)
,(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,333),(-54,357)
-- State  288
,(-420,98),(-309,366),(-54,365)
-- State  289
,(-420,98),(-403,222),(-401,220),(-400,213),(-337,359),(-318,368),(-316,367),(-171,369),(-102,221),(-54,357)
-- State  290
,(-300,144),(-276,149),(-274,148),(-273,146),(-63,147),(-24,370)
-- State  291
,(-420,98),(-403,222),(-401,220),(-400,383),(-337,359),(-110,348),(-108,351),(-107,356),(-106,350),(-103,380),(-102,379)
,(-101,377),(-100,376),(-95,375),(-94,374),(-93,373),(-92,372),(-90,371),(-71,384),(-54,378)
-- State  292
,(-420,98),(-125,298),(-121,297),(-120,296),(-73,385),(-54,295)
-- State  293
,(-420,98),(-125,298),(-121,297),(-120,296),(-73,386),(-54,295)
-- State  298
,(-154,397),(-152,396),(-150,395),(-148,394),(-146,393),(-144,391),(-133,390),(-132,389),(-126,399)
-- State  299
,(-220,401)
-- State  303
,(-420,32),(-246,173),(-245,172),(-244,171),(-243,170),(-242,169),(-241,403),(-230,57),(-58,114),(-56,174),(-46,115)
,(-42,178),(-41,177)
-- State  304
,(-420,98),(-251,405),(-250,404),(-248,408),(-120,406),(-54,407)
-- State  305
,(-252,409)
-- State  307
,(-254,410)
-- State  309
,(-420,32),(-46,411)
-- State  310
,(-270,412)
-- State  312
,(-294,413)
-- State  315
,(-340,414)
-- State  318
,(-347,415)
-- State  323
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-359,417),(-357,416),(-344,418)
,(-337,359),(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-54,357)
-- State  324
,(-420,98),(-403,222),(-401,220),(-400,419),(-337,359),(-102,221),(-54,357)
-- State  325
,(-420,98),(-408,421),(-405,420),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334)
,(-337,359),(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,422)
,(-54,357)
-- State  328
,(-354,423)
-- State  330
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,424),(-54,357)
-- State  331
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,425),(-54,357)
-- State  332
,(-420,98),(-403,222),(-401,220),(-400,213),(-337,359),(-171,426),(-102,221),(-54,357)
-- State  335
,(-419,434),(-418,427),(-91,435)
-- State  336
,(-416,436),(-395,443)
-- State  337
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,444),(-337,359),(-171,344),(-110,348),(-108,351)
,(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-54,357)
-- State  338
,(-417,445),(-99,456)
-- State  342
,(-397,457)
-- State  346
,(-398,458)
-- State  347
,(-399,459)
-- State  349
,(-111,460)
-- State  351
,(-109,461)
-- State  352
,(-409,462)
-- State  367
,(-61,469)
-- State  371
,(-419,434),(-418,427),(-91,471)
-- State  373
,(-420,98),(-403,222),(-401,220),(-400,383),(-337,359),(-110,348),(-108,351),(-107,356),(-106,350),(-103,380),(-102,379)
,(-101,377),(-100,376),(-95,375),(-94,374),(-92,475),(-54,378)
-- State  375
,(-417,445),(-99,476)
-- State  381
,(-104,477)
-- State  382
,(-105,478)
-- State  383
,(-339,275)
-- State  385
,(-74,480)
-- State  387
,(-122,482)
-- State  388
,(-124,483)
-- State  389
,(-140,489),(-139,488),(-131,487),(-130,486),(-129,485),(-128,484),(-127,494)
-- State  392
,(-154,397),(-152,396),(-150,395),(-148,394),(-146,393),(-144,495)
-- State  393
,(-147,497)
-- State  394
,(-149,499)
-- State  395
,(-151,501)
-- State  396
,(-153,503)
-- State  398
,(-173,504)
-- State  400
,(-222,505)
-- State  401
,(-221,507)
-- State  405
,(-420,98),(-250,509),(-120,406),(-54,508)
-- State  409
,(-420,98),(-251,405),(-250,404),(-248,510),(-120,406),(-54,407)
-- State  410
,(-420,98),(-251,405),(-250,404),(-248,511),(-120,406),(-54,407)
-- State  412
,(-420,98),(-271,513),(-54,512)
-- State  413
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,514),(-54,357)
-- State  414
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,517),(-343,516),(-341,515)
,(-337,359),(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-54,357)
-- State  415
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-350,519),(-348,518),(-344,520)
,(-337,359),(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-54,357)
-- State  416
,(-61,522)
-- State  419
,(-339,275)
-- State  420
,(-406,526)
-- State  423
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,517),(-343,527),(-337,359)
,(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-54,357)
-- State  424
,(-361,528)
-- State  425
,(-364,529)
-- State  426
,(-368,530)
-- State  435
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,532),(-337,359),(-171,344),(-110,348)
,(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-54,357)
-- State  443
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,533),(-337,359),(-171,344),(-110,348),(-108,351),(-107,356)
,(-106,350),(-103,345),(-102,221),(-101,343),(-54,357)
-- State  444
,(-416,436),(-395,443)
-- State  456
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,534),(-337,359),(-171,344),(-110,348),(-108,351),(-107,356),(-106,350)
,(-103,345),(-102,221),(-101,343),(-54,357)
-- State  457
,(-420,98),(-403,222),(-401,220),(-400,213),(-337,359),(-171,535),(-102,221),(-54,357)
-- State  458
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,536),(-54,357)
-- State  459
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,537),(-337,359),(-171,344),(-110,348),(-108,351),(-107,356),(-106,350)
,(-103,345),(-102,221),(-101,343),(-54,357)
-- State  461
,(-107,539),(-106,538)
-- State  462
,(-420,98),(-414,541),(-412,540),(-410,543),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335)
,(-344,334),(-337,359),(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337)
,(-77,542),(-54,357)
-- State  463
,(-320,544)
-- State  464
,(-379,545)
-- State  465
,(-304,546)
-- State  466
,(-307,547)
-- State  467
,(-310,548)
-- State  468
,(-317,549)
-- State  469
,(-420,98),(-403,222),(-401,220),(-400,213),(-337,359),(-318,550),(-171,369),(-102,221),(-54,357)
-- State  470
,(-297,551)
-- State  471
,(-420,98),(-403,222),(-401,220),(-400,383),(-337,359),(-110,348),(-108,351),(-107,356),(-106,350),(-103,380),(-102,379)
,(-101,377),(-100,376),(-95,375),(-94,374),(-93,373),(-92,372),(-90,552),(-54,378)
-- State  472
,(-96,553)
-- State  473
,(-97,554)
-- State  474
,(-98,555)
-- State  476
,(-420,98),(-403,222),(-401,220),(-400,383),(-337,359),(-110,348),(-108,351),(-107,356),(-106,350),(-103,380),(-102,379)
,(-101,377),(-100,556),(-54,378)
-- State  477
,(-420,98),(-403,222),(-401,220),(-400,383),(-337,359),(-110,348),(-108,351),(-107,356),(-106,350),(-103,380),(-102,379)
,(-101,377),(-100,376),(-95,375),(-94,374),(-93,373),(-92,372),(-90,371),(-71,557),(-54,378)
-- State  478
,(-420,98),(-403,222),(-401,220),(-400,383),(-337,359),(-110,348),(-108,351),(-107,356),(-106,350),(-103,380),(-102,379)
,(-101,377),(-100,558),(-54,378)
-- State  482
,(-420,98),(-113,560),(-112,568),(-108,564),(-107,356),(-106,563),(-93,561),(-54,567)
-- State  483
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,569),(-54,357)
-- State  489
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-141,571),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,570),(-54,357)
-- State  490
,(-210,572)
-- State  491
,(-212,573)
-- State  492
,(-214,574)
-- State  493
,(-142,575)
-- State  496
,(-155,576)
-- State  498
,(-162,577)
-- State  502
,(-207,579)
-- State  504
,(-169,581)
-- State  505
,(-420,98),(-54,582)
-- State  506
,(-223,583)
-- State  515
,(-61,586)
-- State  518
,(-61,589)
-- State  522
,(-358,590)
-- State  526
,(-407,591)
-- State  527
,(-61,592)
-- State  528
,(-365,593),(-362,595),(-61,594)
-- State  529
,(-365,593),(-362,596),(-61,594)
-- State  530
,(-369,598)
-- State  533
,(-417,445),(-99,456)
-- State  540
,(-61,600)
-- State  543
,(-411,602)
-- State  544
,(-301,607),(-276,149),(-274,148),(-273,605),(-272,603),(-63,606)
-- State  545
,(-380,608)
-- State  546
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-305,609)
,(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,333),(-54,357)
-- State  547
,(-301,610),(-276,149),(-274,148),(-273,605),(-272,603),(-63,606)
-- State  548
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-311,612)
,(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,611),(-54,357)
-- State  549
,(-301,613),(-276,149),(-274,148),(-273,605),(-272,603),(-63,606)
-- State  551
,(-300,144),(-276,149),(-274,148),(-273,146),(-63,147),(-24,614)
-- State  553
,(-420,98),(-403,222),(-401,220),(-400,383),(-337,359),(-110,348),(-108,351),(-107,356),(-106,350),(-103,380),(-102,379)
,(-101,377),(-100,376),(-95,615),(-54,378)
-- State  554
,(-420,98),(-403,222),(-401,220),(-400,383),(-337,359),(-110,348),(-108,351),(-107,356),(-106,350),(-103,380),(-102,379)
,(-101,377),(-100,376),(-95,616),(-54,378)
-- State  555
,(-420,98),(-403,222),(-401,220),(-400,383),(-337,359),(-110,348),(-108,351),(-107,356),(-106,350),(-103,380),(-102,379)
,(-101,377),(-100,376),(-95,617),(-54,378)
-- State  559
,(-75,619)
-- State  561
,(-420,98),(-113,620),(-54,567)
-- State  562
,(-114,621)
-- State  564
,(-109,461)
-- State  568
,(-123,622)
-- State  572
,(-420,98),(-211,625),(-120,624),(-54,508)
-- State  573
,(-237,87),(-213,626)
-- State  574
,(-237,87),(-213,627)
-- State  575
,(-420,32),(-58,114),(-56,628),(-46,115)
-- State  577
,(-176,631),(-175,630),(-166,632),(-163,637)
-- State  578
,(-205,638)
-- State  579
,(-208,640)
-- State  580
,(-170,641)
-- State  581
,(-176,631),(-175,630),(-166,632),(-163,642)
-- State  583
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,643),(-54,357)
-- State  586
,(-342,644)
-- State  587
,(-345,645)
-- State  589
,(-349,646)
-- State  590
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-359,647),(-344,418),(-337,359)
,(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-54,357)
-- State  591
,(-420,98),(-408,648),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359)
,(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,422),(-54,357)
-- State  592
,(-355,649)
-- State  594
,(-366,650)
-- State  597
,(-370,653)
-- State  600
,(-413,655)
-- State  601
,(-415,656)
-- State  604
,(-302,658)
-- State  607
,(-322,660),(-321,662)
-- State  608
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-387,664),(-381,663),(-344,334)
,(-337,359),(-201,666),(-196,665),(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343)
,(-93,337),(-77,667),(-54,357)
-- State  612
,(-312,670)
-- State  614
,(-298,671)
-- State  615
,(-417,445),(-99,476)
-- State  616
,(-417,445),(-99,476)
-- State  617
,(-417,445),(-99,476)
-- State  619
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-78,673),(-77,672),(-76,675)
,(-54,357)
-- State  623
,(-143,677)
-- State  629
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-161,682),(-160,681),(-156,680),(-141,684),(-139,683),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221)
,(-101,343),(-93,337),(-77,570),(-54,685)
-- State  632
,(-183,688),(-179,689)
-- State  633
,(-185,690)
-- State  637
,(-164,691)
-- State  638
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-206,693)
,(-171,344),(-161,692),(-141,684),(-139,683),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343)
,(-93,337),(-77,570),(-54,685)
-- State  639
,(-209,694)
-- State  641
,(-420,98),(-403,222),(-401,220),(-400,213),(-337,359),(-171,695),(-102,221),(-54,357)
-- State  642
,(-174,696)
-- State  644
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,517),(-343,697),(-337,359)
,(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-54,357)
-- State  645
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-346,699),(-344,334),(-337,359)
,(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,698),(-54,357)
-- State  646
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-350,700),(-344,520),(-337,359)
,(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-54,357)
-- State  649
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,701),(-54,357)
-- State  650
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,702),(-54,357)
-- State  653
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-371,703),(-344,334),(-337,359)
,(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,704),(-54,357)
-- State  655
,(-420,98),(-414,705),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359)
,(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,542),(-54,357)
-- State  656
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,706),(-54,357)
-- State  658
,(-300,144),(-276,149),(-274,148),(-273,146),(-63,147),(-24,707)
-- State  659
,(-276,149),(-274,148),(-273,708)
-- State  661
,(-323,709)
-- State  663
,(-382,712),(-190,710)
-- State  665
,(-61,713)
-- State  670
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-313,717)
,(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,716),(-54,357)
-- State  674
,(-398,458),(-79,719)
-- State  677
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,721),(-54,357)
-- State  678
,(-215,722)
-- State  680
,(-61,724)
-- State  686
,(-398,458),(-142,575)
-- State  687
,(-177,725)
-- State  688
,(-420,32),(-182,727),(-180,726),(-46,728)
-- State  690
,(-420,98),(-204,731),(-193,730),(-191,729),(-186,733),(-54,732)
-- State  694
,(-420,98),(-158,749),(-153,748),(-151,747),(-149,746),(-147,745),(-145,743),(-140,489),(-139,488),(-138,741),(-136,739)
,(-135,736),(-134,735),(-131,487),(-130,486),(-129,485),(-128,484),(-127,742),(-121,738),(-120,737),(-54,740)
-- State  695
,(-172,750)
-- State  696
,(-168,751),(-167,752),(-166,753)
-- State  709
,(-301,758),(-276,149),(-274,148),(-273,605),(-272,603),(-63,606)
-- State  710
,(-384,759),(-383,760)
-- State  711
,(-386,761)
-- State  713
,(-200,763)
-- State  714
,(-388,764)
-- State  715
,(-202,765)
-- State  719
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-87,772),(-84,770),(-83,769)
,(-82,768),(-81,767),(-80,774),(-78,673),(-77,672),(-76,771),(-54,773)
-- State  722
,(-420,98),(-54,775)
-- State  724
,(-159,777)
-- State  725
,(-178,780),(-176,779),(-166,778)
-- State  744
,(-153,748),(-151,747),(-149,746),(-147,745),(-145,787)
-- State  751
,(-167,789),(-166,753)
-- State  753
,(-230,57),(-42,139),(-41,138),(-40,137),(-39,136),(-34,791)
-- State  754
,(-351,792)
-- State  756
,(-372,793)
-- State  759
,(-385,796)
-- State  760
,(-190,798)
-- State  761
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-387,799),(-344,334),(-337,359)
,(-201,666),(-196,665),(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337)
,(-77,667),(-54,357)
-- State  763
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-201,800)
,(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,667),(-54,357)
-- State  764
,(-301,801),(-276,149),(-274,148),(-273,605),(-272,603),(-63,606)
-- State  765
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,802),(-54,357)
-- State  766
,(-314,803)
-- State  776
,(-157,809)
-- State  777
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-161,682),(-160,810),(-141,684),(-139,683),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343)
,(-93,337),(-77,570),(-54,685)
-- State  778
,(-183,688),(-179,811)
-- State  781
,(-181,812)
-- State  782
,(-184,813)
-- State  783
,(-192,814)
-- State  784
,(-203,815)
-- State  785
,(-187,816)
-- State  786
,(-137,817)
-- State  791
,(-165,820)
-- State  792
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,821),(-54,357)
-- State  793
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,822),(-54,357)
-- State  794
,(-389,823)
-- State  795
,(-391,825)
-- State  803
,(-301,826),(-276,149),(-274,148),(-273,605),(-272,603),(-63,606)
-- State  804
,(-85,827)
-- State  805
,(-86,828)
-- State  806
,(-88,829)
-- State  807
,(-89,830)
-- State  809
,(-420,98),(-158,831),(-153,748),(-151,747),(-149,746),(-147,745),(-145,743),(-140,489),(-139,488),(-138,741),(-136,739)
,(-135,736),(-134,735),(-131,487),(-130,486),(-129,485),(-128,484),(-127,742),(-121,738),(-120,737),(-54,740)
-- State  812
,(-420,32),(-182,832),(-46,728)
-- State  813
,(-420,98),(-153,748),(-151,747),(-149,746),(-147,745),(-145,743),(-140,489),(-139,488),(-138,741),(-136,739),(-135,736)
,(-134,833),(-131,487),(-130,486),(-129,485),(-128,484),(-127,742),(-121,738),(-120,737),(-54,740)
-- State  814
,(-420,98),(-193,835),(-54,834)
-- State  815
,(-420,98),(-204,837),(-54,836)
-- State  816
,(-188,838)
-- State  817
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-77,839),(-54,357)
-- State  823
,(-300,144),(-276,149),(-274,148),(-273,146),(-63,147),(-24,842)
-- State  824
,(-390,843)
-- State  825
,(-300,144),(-276,149),(-274,148),(-273,146),(-63,147),(-24,844)
-- State  827
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-78,673),(-77,672),(-76,845)
,(-54,357)
-- State  828
,(-420,98),(-87,847),(-54,846)
-- State  829
,(-420,98),(-87,848),(-54,846)
-- State  830
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-171,344)
,(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337),(-78,673),(-77,672),(-76,849)
,(-54,357)
-- State  838
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-201,666)
,(-196,852),(-195,851),(-189,850),(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343)
,(-93,337),(-77,667),(-54,357)
-- State  843
,(-300,144),(-276,149),(-274,148),(-273,146),(-63,147),(-24,853)
-- State  850
,(-190,855)
-- State  852
,(-61,713)
-- State  854
,(-194,857)
-- State  856
,(-197,858)
-- State  857
,(-420,98),(-403,222),(-401,220),(-400,213),(-396,341),(-394,338),(-393,336),(-392,335),(-344,334),(-337,359),(-201,666)
,(-196,852),(-195,859),(-171,344),(-110,348),(-108,351),(-107,356),(-106,350),(-103,345),(-102,221),(-101,343),(-93,337)
,(-77,667),(-54,357)
-- State  860
,(-198,861)
-- State  861
,(-176,631),(-175,630),(-166,632),(-163,862)
-- State  862
,(-190,863)
-- State  863
,(-199,864)
);
--  The offset vector
GOTO_OFFSET : array (0.. 865) of Integer :=
(0,
7,7,7,11,11,11,12,13,17,17,17,18,18,19,20,21,21,
22,24,40,41,41,42,42,42,43,46,49,49,49,49,49,49,49,
49,49,49,49,50,51,64,64,64,64,64,64,64,66,69,70,73,
73,73,73,73,73,73,75,76,77,78,79,80,80,83,83,84,85,
85,85,86,87,87,87,88,88,88,90,90,92,95,97,97,101,107,
108,109,109,110,110,113,116,119,122,122,122,122,122,122,137,149,152,
152,158,158,158,159,159,160,162,162,164,164,164,164,164,164,169,174,
188,192,194,195,195,195,195,195,195,196,198,208,208,208,208,208,208,
208,208,208,208,208,208,208,212,212,212,212,212,230,239,239,239,240,
240,240,241,242,242,259,259,259,259,260,260,260,277,277,277,277,277,
277,277,277,277,277,281,285,285,285,285,285,285,285,285,287,289,289,
290,290,290,290,290,290,292,292,292,297,300,300,300,300,304,304,304,
304,304,304,304,305,306,307,308,309,310,311,312,313,313,313,313,313,
313,313,313,313,313,313,313,313,313,313,313,314,315,316,317,318,319,
320,320,321,322,322,323,326,332,333,333,333,333,333,333,334,335,336,
337,338,338,338,344,344,344,344,344,344,345,347,347,348,349,350,351,
352,353,354,354,355,355,357,359,360,361,362,363,385,407,413,435,438,
448,454,474,480,486,486,486,486,486,495,496,496,496,496,509,515,516,
516,517,517,519,520,520,521,521,521,522,522,522,523,523,523,523,523,
545,552,575,575,575,576,576,597,618,626,626,626,629,631,648,650,650,
650,650,651,651,651,651,652,653,653,654,654,655,656,656,656,656,656,
656,656,656,656,656,656,656,656,656,656,657,657,657,657,660,660,677,
677,679,679,679,679,679,679,680,681,682,682,683,683,684,685,692,692,
692,698,699,700,701,702,702,703,703,704,705,705,705,705,709,709,709,
709,715,721,721,724,745,767,789,790,790,790,791,792,792,792,813,814,
815,816,816,816,816,816,816,816,816,816,835,835,835,835,835,835,835,
835,851,853,853,853,853,853,853,853,853,853,853,853,853,868,876,897,
912,912,914,938,939,940,941,942,943,944,953,954,973,974,975,976,976,
990,1010,1024,1024,1024,1024,1032,1053,1053,1053,1053,1053,1053,1075,1076,1077,1078,
1079,1079,1079,1080,1080,1081,1081,1081,1081,1082,1082,1083,1085,1086,1086,1086,1086,
1086,1086,1086,1086,1086,1087,1087,1087,1088,1088,1088,1088,1089,1089,1089,1089,1090,
1091,1094,1097,1098,1098,1098,1100,1100,1100,1100,1100,1100,1100,1101,1101,1101,1102,
1108,1109,1131,1137,1159,1165,1165,1171,1171,1186,1201,1216,1216,1216,1216,1217,1217,
1220,1221,1221,1222,1222,1222,1222,1223,1223,1223,1223,1227,1229,1231,1235,1235,1239,
1240,1241,1242,1246,1246,1267,1267,1267,1268,1269,1269,1270,1291,1313,1314,1314,1315,
1315,1315,1316,1316,1316,1317,1318,1318,1318,1319,1319,1319,1321,1346,1346,1346,1346,
1347,1347,1348,1350,1352,1354,1354,1377,1377,1377,1377,1378,1378,1378,1378,1378,1378,
1404,1404,1404,1406,1407,1407,1407,1407,1408,1433,1434,1434,1442,1443,1443,1464,1486,
1507,1507,1507,1528,1549,1549,1549,1571,1571,1593,1614,1614,1620,1623,1623,1624,1624,
1626,1626,1627,1627,1627,1627,1627,1649,1649,1649,1649,1651,1651,1651,1672,1673,1673,
1674,1674,1674,1674,1674,1674,1676,1677,1681,1681,1687,1687,1687,1687,1708,1709,1712,
1712,1712,1712,1712,1712,1712,1712,1712,1712,1712,1712,1712,1718,1720,1721,1721,1722,
1723,1724,1724,1724,1724,1753,1753,1753,1755,1755,1756,1759,1759,1759,1759,1759,1759,
1759,1759,1759,1759,1759,1759,1759,1759,1759,1759,1759,1759,1759,1764,1764,1764,1764,
1764,1764,1764,1766,1766,1772,1773,1773,1774,1774,1774,1775,1776,1800,1800,1822,1828,
1849,1850,1850,1850,1850,1850,1850,1850,1850,1850,1850,1851,1876,1878,1878,1878,1879,
1880,1881,1882,1883,1884,1884,1884,1884,1884,1885,1906,1927,1928,1929,1929,1929,1929,
1929,1929,1929,1929,1935,1936,1937,1938,1939,1939,1960,1960,1960,1963,1983,1986,1989,
1990,2011,2011,2011,2011,2011,2011,2017,2018,2024,2024,2047,2050,2053,2076,2076,2076,
2076,2076,2076,2076,2076,2101,2101,2101,2101,2101,2107,2107,2107,2107,2107,2107,2107,
2108,2108,2109,2109,2110,2110,2111,2135,2135,2135,2136,2140,2141,2142, 2142);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  636) of Natural := (2,
1,1,3,1,1,1,0,4,2,0,3,0,0,5,0,0,6,1,0,0,3,1,0,2,1,0,4,1,1,0,2,1,1,1,1,1,1,
0,2,1,1,1,1,1,1,1,1,2,2,1,1,0,0,7,0,2,0,4,3,0,3,1,0,4,1,0,4,1,1,2,3,3,1,1,
1,1,2,2,1,0,0,4,0,4,0,0,0,8,1,1,0,4,1,1,1,1,0,4,1,0,4,1,0,4,1,0,4,1,3,1,2,
1,1,0,4,0,4,0,4,3,1,1,1,1,1,0,4,0,3,1,1,1,1,0,3,1,0,2,1,1,1,2,0,2,1,1,1,1,
1,1,1,0,3,2,1,0,0,6,1,0,0,6,0,4,1,1,0,2,1,1,1,1,0,2,1,1,1,1,1,0,4,1,1,1,0,
2,0,4,0,4,1,2,1,2,0,2,0,2,0,2,0,2,1,1,1,1,1,0,0,8,0,4,1,1,1,1,1,1,0,0,5,2,
2,0,1,1,1,0,3,2,1,0,0,0,0,5,0,0,7,1,1,0,0,4,2,2,1,0,0,4,1,1,0,0,5,0,0,0,8,
0,4,1,0,4,1,0,0,0,9,0,4,1,1,0,4,0,4,1,1,1,0,4,1,0,3,0,3,0,0,3,1,0,3,0,0,6,
2,2,1,0,0,8,0,0,3,0,0,3,2,3,1,1,1,0,4,2,1,3,1,0,2,1,0,3,0,2,0,4,0,4,1,1,1,
1,1,1,0,4,3,1,2,1,1,0,3,0,3,0,3,0,1,1,0,2,0,0,4,0,1,1,2,0,3,0,3,0,3,0,4,1,
2,1,2,0,0,0,8,1,0,1,1,3,2,0,0,2,0,2,1,1,1,1,1,1,1,1,1,1,1,2,1,1,0,2,0,0,4,
0,4,0,0,0,8,1,3,1,3,2,0,1,3,1,0,4,0,0,6,0,0,6,0,0,0,11,0,0,6,3,1,1,0,0,7,1,
0,0,3,1,1,1,1,1,1,0,3,0,3,0,3,0,3,0,3,0,3,1,0,0,4,0,0,4,1,1,0,4,0,4,0,0,4,
1,1,1,0,4,0,3,0,0,7,0,4,0,4,1,0,0,7,0,0,7,1,0,0,3,0,0,7,0,3,0,0,4,1,1,1,0,
0,4,0,3,0,0,0,9,3,1,0,0,2,0,1,0,4,1,0,4,0,3,0,4,0,3,1,1,1,1,1,1,1,1,3,1,2,
1,3,1,3,0,3,1,1,1,0,4,0,3,1,1,1,1,1,0,4,1,2,0,5,0,4,1,1,1,1,2,1,1,0,0,5,1,
0,0,4,1,0,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,2,1,1,1,
1,1,1,1,1,1,1);
   Get_LHS_Rule: array (Rule range  0 ..  636) of Nonterminal := (-1,
-2,-2,-3,-6,-6,-6,-12,-5,-7,-13,-10,-14,-16,-9,
-18,-19,-8,-21,-21,-23,-22,-15,-15,-25,-25,-27,-26,-11,
-17,-17,-29,-29,-30,-30,-30,-30,-20,-20,-35,-35,-36,-36,
-36,-36,-36,-34,-34,-39,-40,-4,-4,-45,-47,-43,-50,-44,
-52,-49,-48,-48,-53,-53,-55,-28,-28,-57,-56,-56,-58,-51,
-37,-60,-60,-62,-63,-64,-31,-65,-65,-67,-68,-66,-70,-69,
-72,-74,-75,-69,-76,-76,-79,-78,-80,-80,-81,-81,-85,-82,
-82,-86,-83,-83,-88,-84,-84,-89,-87,-71,-71,-90,-90,-92,
-94,-96,-94,-97,-94,-98,-94,-95,-95,-100,-100,-100,-100,-104,
-100,-105,-100,-106,-107,-107,-108,-109,-108,-101,-111,-101,-101,-101,
-112,-112,-114,-112,-112,-112,-93,-93,-113,-113,-113,-115,-32,-116,
-116,-118,-119,-117,-120,-122,-123,-121,-124,-73,-73,-73,-125,-73,
-127,-127,-127,-127,-132,-126,-126,-134,-134,-134,-134,-137,-135,-136,
-136,-128,-140,-128,-142,-139,-143,-141,-133,-133,-138,-138,-146,-144,
-148,-144,-150,-144,-152,-144,-144,-145,-145,-145,-145,-155,-157,-147,
-159,-156,-156,-160,-161,-161,-161,-158,-162,-164,-149,-165,-165,-165,
-166,-166,-166,-166,-167,-168,-168,-168,-169,-170,-172,-169,-173,-174,
-154,-163,-163,-163,-177,-175,-175,-178,-178,-178,-181,-180,-180,-182,
-183,-184,-179,-185,-187,-188,-176,-192,-186,-186,-194,-189,-189,-197,
-198,-199,-195,-200,-196,-196,-201,-202,-201,-203,-191,-191,-204,-193,
-205,-151,-206,-207,-153,-209,-208,-208,-210,-129,-211,-212,-130,-214,
-215,-131,-33,-216,-216,-218,-219,-217,-220,-222,-220,-221,-223,-221,
-38,-224,-224,-225,-225,-228,-226,-41,-231,-231,-232,-232,-234,-234,
-236,-235,-233,-233,-238,-237,-240,-239,-239,-241,-241,-241,-241,-241,
-247,-242,-243,-248,-248,-250,-251,-252,-249,-249,-244,-254,-253,-253,
-245,-246,-256,-255,-258,-259,-257,-260,-261,-261,-262,-263,-230,-264,
-230,-265,-230,-266,-227,-267,-267,-229,-229,-268,-269,-270,-42,-213,
-213,-271,-272,-272,-272,-272,-274,-273,-276,-273,-275,-275,-275,-277,
-277,-277,-277,-277,-277,-277,-277,-278,-288,-288,-291,-290,-293,-294,
-289,-295,-59,-296,-297,-298,-287,-299,-299,-300,-300,-300,-300,-24,
-24,-301,-302,-301,-303,-304,-283,-306,-307,-284,-308,-310,-314,-285,
-315,-317,-286,-316,-316,-318,-319,-320,-281,-321,-321,-323,-322,-279,
-279,-279,-279,-279,-279,-330,-279,-332,-279,-333,-279,-335,-279,-336,
-324,-338,-325,-292,-292,-340,-331,-331,-342,-341,-341,-343,-345,-343,
-347,-334,-334,-349,-348,-348,-350,-346,-351,-346,-352,-326,-354,-355,
-353,-356,-339,-358,-357,-357,-360,-361,-327,-363,-364,-328,-362,-362,
-366,-365,-367,-368,-329,-370,-369,-369,-372,-371,-371,-359,-373,-373,
-375,-374,-376,-280,-377,-379,-380,-282,-382,-190,-190,-384,-383,-383,
-378,-386,-381,-381,-388,-387,-389,-385,-390,-385,-391,-385,-309,-311,
-312,-312,-313,-305,-77,-344,-344,-392,-392,-393,-393,-394,-394,-397,
-396,-396,-396,-396,-398,-396,-399,-396,-171,-400,-400,-401,-401,-402,
-401,-401,-102,-404,-403,-407,-405,-405,-408,-406,-406,-337,-110,-110,
-409,-411,-103,-410,-410,-413,-412,-412,-415,-414,-414,-395,-416,-416,
-416,-416,-416,-416,-99,-417,-417,-417,-417,-417,-417,-417,-417,-417,
-417,-91,-418,-418,-418,-418,-418,-418,-419,-418,-46,-46,-54,-54,
-420,-420,-420,-420,-420,-61);
end Pascal_Goto;