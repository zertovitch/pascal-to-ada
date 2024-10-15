{  > P2ada test - Example coming from Unzip in Pascal (see Unzip-Ada). }

{  > This Pascal file has successful compilation under TP6 }

{  > Modification for successful translation: added "begin end." at the end. }

{  > Translation works only with the GACC/GdM-19-Nov-2002 version. }

{------------------------------------------------------------------------------------}
{------ unzip.pas include files glued together: -------------------------------------}
{------ globals + tables + huft + unshrink + expand + explode -----------}
{------------------------------------------------------------------------------------}

{$X+} { <- function"="procedure }

const   {Error codes returned by unzip}
  unzip_Ok=0;
  unzip_CRCErr=1;
  unzip_WriteErr=2;
  unzip_ReadErr=3;
  unzip_ZipFileErr=4;
  unzip_UserAbort=5;
  unzip_NotSupported=6;
  unzip_Encrypted=7;
  unzip_InUse=-1;

procedure NEEDBITS(n:byte);
var nb:longint;
begin
end;

procedure DUMPBITS(n:byte);
begin
end;

procedure READBYTE(bytebuf:byte);
begin
end;

function flush(w:word): boolean;
begin
end;

{Include file for unzip.pas: global constants, types and variables}

{C code by info-zip group, translated to pascal by Christian Ghisler}
{based on unz51g.zip}

const   {Variables for output directly to memory}
  global_byteswritten:longint=0;
  global_tomemory:boolean=false;
  global_crconly:boolean=false;
  global_outbuf:pointer=nil;
  global_bufsize:longint=0;

const   {Error codes returned by huft_build}
  huft_complete=0;     {Complete tree}
  huft_incomplete=1;   {Incomplete tree <- sufficient in some cases!}
  huft_error=2;        {bad tree constructed}
  huft_outofmem=3;     {not enough memory}

const wsize=$8000;          {Size of sliding dictionary}
      INBUFSIZ=2048;        {Size of input buffer}

const lbits:shortint=9;
      dbits:shortint=6;

const b_max=16;
      n_max=288;
      BMAX=16;

type push=^ush;
     ush=word;
     pbyte=^byte;
     pushlist=^ushlist;
     ushlist=array[0..32766] of ush;  {only pseudo-size!!}
     pword=^word;
     pwordarr=^twordarr;
     twordarr=array[0..32766] of word;
     iobuf=array[0..inbufsiz] of byte; {!!...-1}
type pphuft=^phuft;
     phuft=^huft;
     phuftlist=^huftlist;
     huft=packed record
       e,             {# of extra bits}
       b:byte;        {# of bits in code}
       v_n:ush;
       v_t:phuftlist; {Linked List}
     end;
     huftlist=array[0..8190] of huft;
type li=record
       lo,hi:word;
     end;

{pkzip header in front of every file in archive}
type
  plocalheader=^tlocalheader;
  tlocalheader=record
    signature:array[0..3] of char;  {'PK'#1#2}
    extract_ver,
    bit_flag,
    zip_type:word;
    file_timedate:longint;
    crc_32,
    compress_size,
    uncompress_size:longint;
    filename_len,
    extra_field_len:word;
  end;

const slide: ^string=nil;      {Sliding dictionary for unzipping}

type short=byte;  thandle=^char;  smallint=integer;

var inbuf:iobuf;            {input buffer}
    inpos:short;            {position in input buffer}
    readpos:integer;        {position read from file}
    dlghandle:thandle;      {optional: handle of a cancel and "%-done"-dialog}
    dlgnotify:short;        {notification code to tell dialog how far the decompression is}

var w:word;                 {Current Position in slide}
    b:longint;              {Bit Buffer}
    k:byte;                 {Bits in bit buffer}
    infile,                 {handle to zipfile}
    outfile:file;           {handle to extracted file}
    compsize,               {comressed size of file}
    reachedsize,            {number of bytes read from zipfile}
    uncompsize:longint;     {uncompressed size of file}
    oldpercent:short;       {last percent value shown}
    crc32val:longint;       {crc calculated from data}
    hufttype:word;          {coding type=bit_flag from header}
    totalabort,             {User pressed abort button, set in showpercent!}
    zipeof:boolean;         {read over end of zip section for this file}

const inuse:boolean=false;    {is unit already in use -> don't call it again!!!}
      lastusedtime:longint=0; {Time of last usage in timer ticks for timeout!}

{include file for unzip.pas: Tables for bit masking, huffman codes and CRC checking}

{C code by info-zip group, translated to Pascal by Christian Ghisler}
{based on unz51g.zip}

{b and mask_bits[i] gets lower i bits out of i}
const mask_bits:array [0..16] of word=
   ($0000,
    $0001, $0003, $0007, $000f, $001f, $003f, $007f, $00ff,
    $01ff, $03ff, $07ff, $0fff, $1fff, $3fff, $7fff, $ffff);

{ Tables for deflate from PKZIP's appnote.txt. }

const border:array[0..18] of byte=   { Order of the bit length code lengths }
        (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);
const cplens:array[0..30] of word=    { Copy lengths for literal codes 257..285 }
        (3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
        35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0);
        { note: see note #13 above about the 258 in this list.}
const cplext:array[0..30] of word =    { Extra bits for literal codes 257..285 }
        (0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
        3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, 99, 99); { 99==invalid }
const cpdist:array[0..29] of word=     { Copy offsets for distance codes 0..29 }
        (1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
        257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
        8193, 12289, 16385, 24577);
const cpdext:array[0..29] of word =    { Extra bits for distance codes }
        (0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
        7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
        12, 12, 13, 13);

{ Tables for explode }

const cplen2:array[0..63] of word = (2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
        18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
        35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
        52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65);
const cplen3:array[0..63] of word= (3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
        19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
        36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
        53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66);
const extra:array[0..63] of word = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        8);
const cpdist4:array[0..63] of word = (1, 65, 129, 193, 257, 321, 385, 449, 513, 577, 641, 705,
        769, 833, 897, 961, 1025, 1089, 1153, 1217, 1281, 1345, 1409, 1473,
        1537, 1601, 1665, 1729, 1793, 1857, 1921, 1985, 2049, 2113, 2177,
        2241, 2305, 2369, 2433, 2497, 2561, 2625, 2689, 2753, 2817, 2881,
        2945, 3009, 3073, 3137, 3201, 3265, 3329, 3393, 3457, 3521, 3585,
        3649, 3713, 3777, 3841, 3905, 3969, 4033);
const cpdist8:array[0..63] of word= (1, 129, 257, 385, 513, 641, 769, 897, 1025, 1153, 1281,
        1409, 1537, 1665, 1793, 1921, 2049, 2177, 2305, 2433, 2561, 2689,
        2817, 2945, 3073, 3201, 3329, 3457, 3585, 3713, 3841, 3969, 4097,
        4225, 4353, 4481, 4609, 4737, 4865, 4993, 5121, 5249, 5377, 5505,
        5633, 5761, 5889, 6017, 6145, 6273, 6401, 6529, 6657, 6785, 6913,
        7041, 7169, 7297, 7425, 7553, 7681, 7809, 7937, 8065);

{************************************ CRC-Calculation ************************************}

const crc_32_tab:array[0..255] of longint=
(
   $00000000,$77073096,$ee0e612c,$990951ba,$076dc419,
   $706af48f,$e963a535,$9e6495a3,$0edb8832,$79dcb8a4,
   $e0d5e91e,$97d2d988,$09b64c2b,$7eb17cbd,$e7b82d07,
   $90bf1d91,$1db71064,$6ab020f2,$f3b97148,$84be41de,
   $1adad47d,$6ddde4eb,$f4d4b551,$83d385c7,$136c9856,
   $646ba8c0,$fd62f97a,$8a65c9ec,$14015c4f,$63066cd9,
   $fa0f3d63,$8d080df5,$3b6e20c8,$4c69105e,$d56041e4,
   $a2677172,$3c03e4d1,$4b04d447,$d20d85fd,$a50ab56b,
   $35b5a8fa,$42b2986c,$dbbbc9d6,$acbcf940,$32d86ce3,
   $45df5c75,$dcd60dcf,$abd13d59,$26d930ac,$51de003a,
   $c8d75180,$bfd06116,$21b4f4b5,$56b3c423,$cfba9599,
   $b8bda50f,$2802b89e,$5f058808,$c60cd9b2,$b10be924,
   $2f6f7c87,$58684c11,$c1611dab,$b6662d3d,$76dc4190,
   $01db7106,$98d220bc,$efd5102a,$71b18589,$06b6b51f,
   $9fbfe4a5,$e8b8d433,$7807c9a2,$0f00f934,$9609a88e,
   $e10e9818,$7f6a0dbb,$086d3d2d,$91646c97,$e6635c01,
   $6b6b51f4,$1c6c6162,$856530d8,$f262004e,$6c0695ed,
   $1b01a57b,$8208f4c1,$f50fc457,$65b0d9c6,$12b7e950,
   $8bbeb8ea,$fcb9887c,$62dd1ddf,$15da2d49,$8cd37cf3,
   $fbd44c65,$4db26158,$3ab551ce,$a3bc0074,$d4bb30e2,
   $4adfa541,$3dd895d7,$a4d1c46d,$d3d6f4fb,$4369e96a,
   $346ed9fc,$ad678846,$da60b8d0,$44042d73,$33031de5,
   $aa0a4c5f,$dd0d7cc9,$5005713c,$270241aa,$be0b1010,
   $c90c2086,$5768b525,$206f85b3,$b966d409,$ce61e49f,
   $5edef90e,$29d9c998,$b0d09822,$c7d7a8b4,$59b33d17,
   $2eb40d81,$b7bd5c3b,$c0ba6cad,$edb88320,$9abfb3b6,
   $03b6e20c,$74b1d29a,$ead54739,$9dd277af,$04db2615,
   $73dc1683,$e3630b12,$94643b84,$0d6d6a3e,$7a6a5aa8,
   $e40ecf0b,$9309ff9d,$0a00ae27,$7d079eb1,$f00f9344,
   $8708a3d2,$1e01f268,$6906c2fe,$f762575d,$806567cb,
   $196c3671,$6e6b06e7,$fed41b76,$89d32be0,$10da7a5a,
   $67dd4acc,$f9b9df6f,$8ebeeff9,$17b7be43,$60b08ed5,
   $d6d6a3e8,$a1d1937e,$38d8c2c4,$4fdff252,$d1bb67f1,
   $a6bc5767,$3fb506dd,$48b2364b,$d80d2bda,$af0a1b4c,
   $36034af6,$41047a60,$df60efc3,$a867df55,$316e8eef,
   $4669be79,$cb61b38c,$bc66831a,$256fd2a0,$5268e236,
   $cc0c7795,$bb0b4703,$220216b9,$5505262f,$c5ba3bbe,
   $b2bd0b28,$2bb45a92,$5cb36a04,$c2d7ffa7,$b5d0cf31,
   $2cd99e8b,$5bdeae1d,$9b64c2b0,$ec63f226,$756aa39c,
   $026d930a,$9c0906a9,$eb0e363f,$72076785,$05005713,
   $95bf4a82,$e2b87a14,$7bb12bae,$0cb61b38,$92d28e9b,
   $e5d5be0d,$7cdcefb7,$0bdbdf21,$86d3d2d4,$f1d4e242,
   $68ddb3f8,$1fda836e,$81be16cd,$f6b9265b,$6fb077e1,
   $18b74777,$88085ae6,$ff0f6a70,$66063bca,$11010b5c,
   $8f659eff,$f862ae69,$616bffd3,$166ccf45,$a00ae278,
   $d70dd2ee,$4e048354,$3903b3c2,$a7672661,$d06016f7,
   $4969474d,$3e6e77db,$aed16a4a,$d9d65adc,$40df0b66,
   $37d83bf0,$a9bcae53,$debb9ec5,$47b2cf7f,$30b5ffe9,
   $bdbdf21c,$cabac28a,$53b39330,$24b4a3a6,$bad03605,
   $cdd70693,$54de5729,$23d967bf,$b3667a2e,$c4614ab8,
   $5d681b02,$2a6f2b94,$b40bbe37,$c30c8ea1,$5a05df1b,
   $2d02ef8d); { end crc_32_tab[] }

{include for unzip.pas: Huffman tree generating and destroying}

{C code by info-zip group, translated to Pascal by Christian Ghisler}
{based on unz51g.zip}

{*************** free huffman tables starting with table where t points to ************}

procedure huft_free(t:phuftlist);

var p,q:phuftlist;
    z:word;

begin
  p:=t;
  while p<>nil do begin
    dec(longint(p),sizeof(huft));
    q:=p^[0].v_t;
    z:=p^[0].v_n;   {Size in Bytes, required by TP ***}
    freemem(p,(z+1)*sizeof(huft));
    p:=q
  end;
end;

{*********** build huffman table from code lengths given by array b^ *******************}    

function huft_build(b:pword;n:word;s:word;d,e:pushlist;t:pphuft;var m:integer):integer;
const b_max1=bmax+1;
var a:word;                        {counter for codes of length k}
    c:array[0..b_max] of word;   {bit length count table}
    f:word;                        {i repeats in table every f entries}
    g,                             {max. code length}
    h:integer;                     {table level}
    i,                             {counter, current code}
    j:word;                        {counter}
    k:integer;                     {number of bits in current code}
    p:pword;                       {pointer into c, b and v}
    q:phuftlist;                   {points to current table}
    r:huft;                        {table entry for structure assignment}
    u:array[0..b_max] of phuftlist;{table stack}
    v:array[0..n_max] of word;     {values in order of bit length}
    w:integer;                     {bits before this table}
    x:array[0..b_max1] of word;   {bit offsets, then code stack} 
    l:array[-1..b_max1] of word;  {l[h] bits in table of level h}
    xp:^word;                      {pointer into x}
    y:integer;                     {number of dummy codes added}
    z:word;                        {number of entries in current table}
    tryagain:boolean;              {bool for loop}
    pt:phuft;                      {for test against bad input}
    el:word;                       {length of eob code=code 256}

  procedure Nestor_1;
    type t = Integer;
    var i: t;
    procedure Nestor_2;
      type t = Integer;
      var i: t;
      procedure Nestor_3;
        type t = Integer;
        var i: t;
        procedure Nestor_4;
          type t = Integer;
          var i: t;
          procedure Nestor_5;
            type t = Integer;
            var i: t;
            procedure Nestor_6;
              type t = Integer;
              var i: t;
              label 1,2,3,4;
            begin
            end; { Nestor_6 }
          begin
            i:= i+1;
            Nestor_6
          end; { Nestor_5 }
        begin
          i:= i+1;
          Nestor_5
        end; { Nestor_4 }
      begin
        i:= i+1;
        Nestor_4
      end; { Nestor_3 }
    begin
      i:= i+1;
      Nestor_3
    end; { Nestor_2 }
  begin
    i:= i+1;
    Nestor_2
  end; { Nestor_1 }

begin
  if n>256 then el:=pword(longint(b)+256*sizeof(word))^
           else el:=BMAX;
  {generate counts for each bit length}
  fillchar(c,sizeof(c),#0);
  p:=b; i:=n;                      {p points to array of word}
  repeat
    if p^>b_max then begin
      t^:=nil;
      m:=0;
      huft_build:=huft_error;
      exit
    end;
    inc(c[p^]);
    inc(longint(p),sizeof(word));   {point to next item}
    dec(i);
  until i=0;
  if c[0]=n then begin
    t^:=nil;
    m:=0;
    huft_build:=huft_complete;
    exit
  end;

  {find minimum and maximum length, bound m by those} 
  j:=1;
  while (j<=b_max) and (c[j]=0) do inc(j);
  k:=j;
  if m<j then m:=j;
  i:=b_max;
  while (i>0) and (c[i]=0) do dec(i);
  g:=i;
  if m>i then m:=i;

  {adjust last length count to fill out codes, if needed}
  y:=1 shl j;
  while j<i do begin
    y:=y-c[j];
    if y<0 then begin
      huft_build:=huft_error;
      exit
    end;
    y:=y shl 1;
    inc(j);
  end;
  dec(y,c[i]);
  if y<0 then begin
    huft_build:=huft_error;
    exit
  end;
  inc(c[i],y);

  {generate starting offsets into the value table for each length}
  x[1]:=0;
  j:=0;
  p:=@c; inc(longint(p),sizeof(word));
  xp:=@x;inc(longint(xp),2*sizeof(word));
  dec(i);
  while i<>0 do begin
    inc(j,p^);
    xp^:=j;
    inc(longint(p),2);
    inc(longint(xp),2);
    dec(i);
  end;

  {make table of values in order of bit length}
  p:=b; i:=0;
  repeat
    j:=p^;
    inc(longint(p),sizeof(word));
    if j<>0 then begin
      v[x[j]]:=i;
      inc(x[j]);
    end;
    inc(i);
  until i>=n;

  {generate huffman codes and for each, make the table entries}
  x[0]:=0; i:=0;
  p:=@v;
  h:=-1;
  l[-1]:=0;
  w:=0;
  u[0]:=nil;
  q:=nil;
  z:=0;

  {go through the bit lengths (k already is bits in shortest code)}
  for k:=k to g do begin
    for a:=c[k] downto 1 do begin
      {here i is the huffman code of length k bits for value p^}
      while k>w+l[h] do begin
        inc(w,l[h]); {Length of tables to this position}
        inc(h);
        z:=g-w;
        if z>m then z:=m;
        j:=k-w;
        f:=1 shl j;
        if f>a+1 then begin
          dec(f,a+1);
          xp:=@c[k];
          inc(j);
          tryagain:=true;
          while (j<z) and tryagain do begin
            f:=f shl 1;
            inc(longint(xp),sizeof(word));
            if f<=xp^ then tryagain:=false
                      else begin
                        dec(f,xp^);
                        inc(j);
                      end;
          end;
        end;
        if (w+j>el) and (w<el) then
          j:=el-w;       {Make eob code end at table}
        if w=0 then begin
          j:=m;  {*** Fix: main table always m bits!}
        end;
        z:=1 shl j;
        l[h]:=j;

        {allocate and link new table}
        getmem(q,(z+1)*sizeof(huft));
        if q=nil then begin
          if h<>0 then huft_free(pointer(u[0]));
          huft_build:=huft_outofmem;
          exit
        end;
        fillchar(q^,(z+1)*sizeof(huft),#0);
        q^[0].v_n:=z;  {Size of table, needed in freemem ***}
        t^:=@q^[1];     {first item starts at 1}
        t:=@q^[0].v_t;
        t^:=nil;
        q:=@q^[1];   {pointer(longint(q)+sizeof(huft));} {???}
        u[h]:=q;
        {connect to last table, if there is one}
        if h<>0 then begin  
          x[h]:=i;
          r.b:=l[h-1];         
          r.e:=16+j;
          r.v_t:=q;
          j:=(i and ((1 shl w)-1)) shr (w-l[h-1]);

          {test against bad input!}
          pt:=phuft(longint(u[h-1])-sizeof(huft));
          if j>pt^.v_n then begin
            huft_free(pointer(u[0]));
            huft_build:=huft_error;
            exit
          end;

          pt:=@u[h-1]^[j];  
          pt^:=r;
        end;
      end;

      {set up table entry in r}
      r.b:=word(k-w);
      r.v_t:=nil;   {Unused}   {***********}
      if longint(p)>=longint(@v[n]) then r.e:=99
      else if p^<s then begin
        if p^<256 then r.e:=16 else r.e:=15;
        r.v_n:=p^;
        inc(longint(p),sizeof(word));
      end else begin
        if (d=nil) or (e=nil) then begin
          huft_free(pointer(u[0]));
          huft_build:=huft_error;
          exit
        end;
        r.e:=word(e^[p^-s]);
        r.v_n:=d^[p^-s];
        inc(longint(p),sizeof(word));
      end;

      {fill code like entries with r}
      f:=1 shl (k-w);
      j:=i shr w;
      while j<z do begin
        q^[j]:=r;
        inc(j,f);
      end;

      {backwards increment the k-bit code i}
      j:=1 shl (k-1);
      while (i and j)<>0 do begin
        {i:=i^j;}
        i:=i xor j;
        j:=j shr 1;
      end;
      i:=i xor j;

      {backup over finished tables}
      while ((i and ((1 shl w)-1))<>x[h]) do begin
        dec(h);
        dec(w,l[h]); {Size of previous table!}
      end;
    end;
  end;
  if (y<>0) and (g<>1) then huft_build:=huft_incomplete
                       else huft_build:=huft_complete;
end;

{*************************** unshrink **********************************}
{Written and NOT copyrighted by Christian Ghisler.
 I have rewritten unshrink because the original
 function was copyrighted by Mr. Smith of Info-zip
 This funtion here is now completely FREE!!!!
 The only right I claim on this code is that
 noone else claims a copyright on it!}


const max_code=8192;
      max_stack=8192;
      initial_code_size=9;
      final_code_size=13;
      write_max=wsize-3*(max_code-256)-max_stack-2;  {Rest of slide=write buffer}
                                                     {=766 bytes}

type prev=array[257..max_code] of smallint;
     pprev=^prev;
     cds=array[257..max_code] of char;
     pcds=^cds;
     stacktype=array[0..max_stack] of char;
     pstacktype=^stacktype;
     writebuftype=array[0..write_max] of char;   {write buffer}
     pwritebuftype=^writebuftype;

var previous_code:pprev;       {previous code trie}
    actual_code:pcds;          {actual code trie}
    stack:pstacktype;          {Stack for output}
    writebuf:pwritebuftype;    {Write buffer}
    next_free,                 {Next free code in trie}
    write_ptr:smallint;        {Pointer to output buffer}

function unshrink_flush:boolean;
begin
  {unshrink_flush:=flush(write_ptr);}  {Outbuf now starts at slide[0]}
end;

function write_char(c:char):boolean;
begin
  writebuf^[write_ptr]:=c;
  inc(write_ptr);
  if write_ptr>write_max then begin
    write_char:=unshrink_flush;
    write_ptr:=0;
  end else write_char:=true;
end;

procedure ClearLeafNodes;
var pc,                    {previous code}
    i,                     {index}
    act_max_code:smallint; {max code to be searched for leaf nodes}
    previous:pprev;        {previous code trie}

begin
  previous:=previous_code;
  act_max_code:=next_free-1;
  for i:=257 to act_max_code do
    previous^[i]:=previous^[i] or $8000;
  for i:=257 to act_max_code do begin
    pc:=previous^[i] and not $8000;
    if pc>256 then
      previous^[pc]:=previous^[pc] and (not $8000);
  end;
  {Build new free list}
  pc:=-1;
  next_free:=-1;
  for i:=257 to act_max_code do
    if previous^[i] and $C000<>0 then begin {Either free before or marked now}
      if pc<>-1 then previous^[pc]:=-i     {Link last item to this item}
                else next_free:=i;
      pc:=i;
    end;
  if pc<>-1 then
    previous^[pc]:=-act_max_code-1;
end;


function unshrink:smallint;

var incode:smallint;           {code read in}
    lastincode:smallint;       {last code read in}
    lastoutcode:char;          {last code emitted}
    code_size:byte;            {Actual code size}
    stack_ptr,                 {Stackpointer}
    new_code,                  {Save new code read}
    code_mask,                 {mask for coding}
    i:smallint;                {Index}
    bits_to_read:longint;



begin
  if compsize=maxlongint then begin   {Compressed Size was not in header!}
    unshrink:=unzip_NotSupported;
    exit
  end;
  inpos:=0;            {Input buffer position}
  readpos:=-1;         {Nothing read}

  {initialize window, bit buffer}
  w:=0;
  k:=0;
  b:=0;

  {Initialize pointers for various buffers}
  {Re-arranged with writebuf first, for flush from slide[0]}

  {writebuf:=slide;}
  {
  previous_code:=@slide[sizeof(writebuftype)];
  actual_code:=@slide[sizeof(prev)+sizeof(writebuftype)];
  stack:=@slide[sizeof(prev)+sizeof(cds)+sizeof(writebuftype)];
  }

  fillchar(slide^,wsize,#0);

  {initialize free codes list}
  for i:=257 to max_code do      
    previous_code^[i]:=-(i+1);
  next_free:=257;
  stack_ptr:=max_stack;
  write_ptr:=0;
  code_size:=initial_code_size;
  code_mask:=mask_bits[code_size];

  NEEDBITS(code_size);
  incode:=b and code_mask;
  DUMPBITS(code_size);

  lastincode:=incode;
  lastoutcode:=char(incode);
  if not write_char(lastoutcode) then begin
    unshrink:=unzip_writeErr;
    exit
  end;

  bits_to_read:=8*compsize-code_size;   {Bits to be read}

  while not totalabort and (bits_to_read>=code_size) do begin
    NEEDBITS(code_size);
    incode:=b and code_mask;
    DUMPBITS(code_size);
    dec(bits_to_read,code_size);
    if incode=256 then begin            {Special code}
      NEEDBITS(code_size);
      incode:=b and code_mask;
      DUMPBITS(code_size);
      dec(bits_to_read,code_size);
      case incode of
        1:begin
          inc(code_size);
          if code_size>final_code_size then begin
            unshrink:=unzip_ZipFileErr;
            exit
          end;
          code_mask:=mask_bits[code_size];
        end;
        2:begin
          ClearLeafNodes;
        end;
      else
        unshrink:=unzip_ZipFileErr;
        exit
      end;
    end else begin
      new_code:=incode;
      if incode<256 then begin          {Simple char}
        lastoutcode:=char(incode);
        if not write_char(lastoutcode) then begin
          unshrink:=unzip_writeErr;
          exit
        end;
      end else begin
        if previous_code^[incode]<0 then begin
          stack^[stack_ptr]:=lastoutcode;
          dec(stack_ptr);
          incode:=lastincode;
        end;
        while incode>256 do begin
          stack^[stack_ptr]:=actual_code^[incode];
          dec(stack_ptr);
          incode:=previous_code^[incode];
        end;
        lastoutcode:=char(incode);
        if not write_char(lastoutcode) then begin
          unshrink:=unzip_writeErr;
          exit
        end;
        for i:=stack_ptr+1 to max_stack do
          if not write_char(stack^[i]) then begin
            unshrink:=unzip_writeErr;
            exit
          end;
        stack_ptr:=max_stack;
      end;
      incode:=next_free;
      if incode<=max_code then begin
        next_free:=-previous_code^[incode];   {Next node in free list}
        previous_code^[incode]:=lastincode;
        actual_code^[incode]:=lastoutcode;
      end;
      lastincode:=new_code;
    end;
  end;
  if totalabort then
    unshrink:=unzip_UserAbort
  else if unshrink_flush then
    unshrink:=unzip_ok
  else
    unshrink:=unzip_WriteErr;
end;

const DLE=144;

type 
  f_array=array[0..255,0..63] of word;        { for followers[256][64] }
  pf_array=^f_array;

procedure LoadFollowers; forward;

{*******************************/
/*  UnReduce Global Variables  */
/*******************************}

var followers:pf_array;
    Slen:array[0..255] of byte;
    factor:integer;

const L_table:array[0..4] of integer=
        (0, $7f, $3f, $1f, $0f);

      D_shift:array[0..4] of integer=
        (0, $07, $06, $05, $04);
      D_mask: array[0..4] of integer=
        (0, $01, $03, $07, $0f);

      B_table:array[0..255] of byte=
(8, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5,
 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6,
 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7,
 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 8, 8, 8);





{*************************/
/*  Function unreduce()  */
/*************************}

function unreduce(compression_method:word):integer;   { expand probabilistically reduced data }
var
    lchar,
    nchar,
    ExState,
    v,
    len:integer;
    s:longint;           { number of bytes left to decompress }
    w:word;              { position in output window slide[]  } 
    u:word;              { true if slide[] unflushed          }
    e,n,d,d1:word;
    mask_bits8:word;
    bitsneeded,follower:integer;

begin
  unreduce:=unzip_ok;
  zipeof:=false;

  b:=0; k:=0; w:=0;

  inpos:=0;            {Input buffer position}
  readpos:=-1;         {Nothing read}

  lchar:=0;
  v:=0;
  Len:=0;
  s:=uncompsize;
  u:=1;
  ExState:=0;

  mask_bits8:=mask_bits[8];

  new(followers);{:=pointer(@slide[$4000]);}
  fillchar(followers^,sizeof(followers^),#0);

  factor:=compression_method - 1;
  LoadFollowers;
  while (s > 0) and not zipeof do begin 
    if (Slen[lchar]=0) then begin
      NEEDBITS(8);
      nchar:=b and mask_bits8;
      DUMPBITS(8);
    end else begin
      NEEDBITS(1);
      nchar:=b and 1;
      DUMPBITS(1);
      if (nchar<>0) then begin
        NEEDBITS(8);
        nchar:=b and mask_bits8;
        DUMPBITS(8);
      end else begin
        bitsneeded:=B_table[Slen[lchar]];
        NEEDBITS(bitsneeded);
        follower:=b and mask_bits[bitsneeded];
        DUMPBITS(bitsneeded);
        nchar:=followers^[lchar,follower];
      end;
    end;
    { expand the resulting byte }
    case ExState of
      0:begin
          if (nchar <> DLE) then begin
            dec(s);
            slide^[w]:=char(nchar);
            inc(w);
            if (w=$4000) then begin
              flush(w);
              w:=0;
              u:=0;
            end;
          end else
            ExState:=1;
        end;
      1:begin
          if (nchar <> 0) then begin
            V:=nchar;
            Len:= V and L_table[factor];
            if (Len=L_table[factor]) then
              ExState:=2
            else
              ExState:=3;
          end else begin
            dec(s);
            slide^[w]:=char(DLE);
            inc(w);
            if (w=$4000) then begin
              flush(w);
              w:=0;
              u:=0;
            end;
            ExState:=0;
          end;
        end;
      2:begin
          inc(Len,nchar);
          ExState:=3;
        end;
      3:begin
          n:=Len+3;                                            {w: Position in slide}
          d:=w-((((V shr D_shift[factor]) and                  {n: zu schreibende Bytes}
                  D_mask[factor]) shl 8) + nchar + 1);         {d: von hier kopieren}
          dec(s,n);                                            {e: zu kopierende Bytes}
          repeat
            d:=d and $3fff;
            if d>w then d1:=d else d1:=w;
            e:=$4000-d1;
            if e>n then e:=n;
            dec(n,e);
            if (u<>0) and (w <= d) then begin
              fillchar(slide^[w],e,#0);
              inc(w,e);
              inc(d,e);
            end else
              if (w - d < e)             { (assume unsigned comparison)   }
              then repeat                { slow to avoid memcpy() overlap }
                 slide^[w]:=slide^[d];
                 inc(w); inc(d); dec(e);
              until e=0
              else begin
                move(slide^[d],slide^[w],e);
                inc(w,e);
                inc(d,e);
              end;
              if (w=$4000) then begin
                flush(w);
                w:=0;
                u:=0;
              end;
          until n=0;
          ExState:=0;
        end;
    end; {case}

    { store character for next iteration }
    lchar:=nchar;
  end;
  flush(w);
  dispose(followers);
end;





{******************************/
/*  Function LoadFollowers()  */
/******************************}

procedure LoadFollowers;
var
  x,i:integer;

begin
  for x:=255 downto 0 do begin
    NEEDBITS(6);
    Slen[x]:=b and mask_bits[6];
    DUMPBITS(6);
    for i:=0  to Slen[x]-1 do begin
      NEEDBITS(8);
      followers^[x,i]:=b and mask_bits[8];
      DUMPBITS(8);
    end;
  end;
end;

{include for unzip.pas: Explode imploded file}

{C code by info-zip group, translated to Pascal by Christian Ghisler}
{based on unz51g.zip}

{************************************* explode ********************************}

{*********************************** read in tree *****************************}
function get_tree(l:pword;n:word):integer;
var i,k,j,b:word;
    bytebuf:byte;

begin
  READBYTE(bytebuf);
  i:=bytebuf;
  inc(i);
  k:=0;
  repeat
    READBYTE(bytebuf);
    j:=bytebuf;  
    b:=(j and $F)+1;
    j:=((j and $F0) shr 4)+1;
    if (k+j)>n then begin
      get_tree:=4;
      exit
    end;
    repeat
      l^:=b;
      inc(longint(l),sizeof(word));
      inc(k);
      dec(j);
    until j=0;
    dec(i);
  until i=0;
  if k<>n then get_tree:=4 else get_tree:=0;
end;

{******************exploding, method: 8k slide, 3 trees ***********************}

function explode_lit8(tb,tl,td:phuftlist;bb,bl,bd:integer):integer;
var s:longint;
    e:word;
    n,d:word;
    w:word;
    t:phuft;
    mb,ml,md:word;
    u:word;

begin
  b:=0; k:=0; w:=0;
  u:=1;
  mb:=mask_bits[bb];
  ml:=mask_bits[bl];
  md:=mask_bits[bd];
  s:=uncompsize;
  while (s>0) and not (totalabort or zipeof) do begin
    NEEDBITS(1);
    if (b and 1)<>0 then begin  {Litteral}
      DUMPBITS(1);
      dec(s);
      NEEDBITS(bb);
      t:=@tb^[(not b) and mb];
      e:=t^.e;
      if e>16 then repeat
        if e=99 then begin
          explode_lit8:=unzip_ZipFileErr;
          exit
        end;
        DUMPBITS(t^.b);
        dec(e,16);
        NEEDBITS(e);
        t:=@t^.v_t^[(not b) and mask_bits[e]];
        e:=t^.e;
      until e<=16;
      DUMPBITS(t^.b);
      slide^[w]:=char(t^.v_n);
      inc(w);
      if w=WSIZE then begin
        if not flush(w) then begin
          explode_lit8:=unzip_WriteErr;
          exit
        end;
        w:=0; u:=0;
      end;
    end else begin
      DUMPBITS(1);
      NEEDBITS(7);
      d:=b and $7F;
      DUMPBITS(7);
      NEEDBITS(bd);
      t:=@td^[(not b) and md];
      e:=t^.e;
      if e>16 then repeat
        if e=99 then begin
          explode_lit8:=unzip_ZipFileErr;
          exit
        end;
        DUMPBITS(t^.b);
        dec(e,16);
        NEEDBITS(e);
        t:=@t^.v_t^[(not b) and mask_bits[e]];
        e:=t^.e;
      until e<=16;
      DUMPBITS(t^.b);

      d:=w-d-t^.v_n;
      NEEDBITS(bl);
      t:=@tl^[(not b) and ml];
      e:=t^.e;
      if e>16 then repeat
        if e=99 then begin
          explode_lit8:=unzip_ZipFileErr;
          exit
        end;
        DUMPBITS(t^.b);
        dec(e,16);
        NEEDBITS(e);
        t:=@t^.v_t^[(not b) and mask_bits[e]];
        e:=t^.e;
      until e<=16;

      DUMPBITS(t^.b);

      n:=t^.v_n;
      if e<>0 then begin
        NEEDBITS(8);
        inc(n,byte(b) and $ff);
        DUMPBITS(8);
      end;
      dec(s,n);
      repeat
        d:=d and pred(WSIZE);
        if d>w then e:=WSIZE-d else e:=WSIZE-w;
        if e>n then e:=n;
        dec(n,e);
        if (u<>0) and (w<=d) then begin
          fillchar(slide^[w],e,#0);
          inc(w,e);
          inc(d,e);
        end else if (w-d>=e) then begin
          move(slide^[d],slide^[w],e);
          inc(w,e);
          inc(d,e);
        end else repeat
          slide^[w]:=slide^[d];
          inc(w);
          inc(d);
          dec(e);
        until e=0;
        if w=WSIZE then begin
          if not flush(w) then begin
            explode_lit8:=unzip_WriteErr;
            exit
          end;
          w:=0; u:=0;
        end;
      until n=0;
    end;
  end;
  if totalabort then explode_lit8:=unzip_userabort
  else
    if not flush(w) then explode_lit8:=unzip_WriteErr
  else
    if zipeof then explode_lit8:=unzip_readErr
  else
    explode_lit8:=unzip_Ok;
end;

{******************exploding, method: 4k slide, 3 trees ***********************}

function explode_lit4(tb,tl,td:phuftlist;bb,bl,bd:integer):integer;
var s:longint;
    e:word;
    n,d:word;
    w:word;
    t:phuft;
    mb,ml,md:word;
    u:word;

begin
  b:=0; k:=0; w:=0;
  u:=1;
  mb:=mask_bits[bb];
  ml:=mask_bits[bl];
  md:=mask_bits[bd];
  s:=uncompsize;
  while (s>0) and not (totalabort or zipeof) do begin
    NEEDBITS(1);
    if (b and 1)<>0 then begin  {Litteral}
      DUMPBITS(1);
      dec(s);
      NEEDBITS(bb);
      t:=@tb^[(not b) and mb];
      e:=t^.e;
      if e>16 then repeat
        if e=99 then begin
          explode_lit4:=unzip_ZipFileErr;
          exit
        end;
        DUMPBITS(t^.b);
        dec(e,16);
        NEEDBITS(e);
        t:=@t^.v_t^[(not b) and mask_bits[e]];
        e:=t^.e;
      until e<=16;
      DUMPBITS(t^.b);
      slide^[w]:=char(t^.v_n);
      inc(w);
      if w=WSIZE then begin
        if not flush(w) then begin
          explode_lit4:=unzip_WriteErr;
          exit
        end;
        w:=0; u:=0;
      end;
    end else begin
      DUMPBITS(1);
      NEEDBITS(6);
      d:=b and $3F;
      DUMPBITS(6);
      NEEDBITS(bd);
      t:=@td^[(not b) and md];
      e:=t^.e;
      if e>16 then repeat
        if e=99 then begin
          explode_lit4:=unzip_ZipFileErr;
          exit
        end;
        DUMPBITS(t^.b);
        dec(e,16);
        NEEDBITS(e);
        t:=@t^.v_t^[(not b) and mask_bits[e]];
        e:=t^.e;
      until e<=16;
      DUMPBITS(t^.b);
      d:=w-d-t^.v_n;
      NEEDBITS(bl);
      t:=@tl^[(not b) and ml];
      e:=t^.e;
      if e>16 then repeat
        if e=99 then begin
          explode_lit4:=unzip_ZipFileErr;
          exit
        end;
        DUMPBITS(t^.b);
        dec(e,16);
        NEEDBITS(e);
        t:=@t^.v_t^[(not b) and mask_bits[e]];
        e:=t^.e;
      until e<=16;

      DUMPBITS(t^.b);
      n:=t^.v_n;
      if e<>0 then begin
        NEEDBITS(8);
        inc(n,b and $ff);
        DUMPBITS(8);
      end;
      dec(s,n);
      repeat
        d:=d and pred(WSIZE);
        if d>w then e:=WSIZE-d else e:=WSIZE-w;
        if e>n then e:=n;
        dec(n,e);
        if (u<>0) and (w<=d) then begin
          fillchar(slide^[w],e,#0);
          inc(w,e);
          inc(d,e);
        end else if (w-d>=e) then begin
          move(slide^[d],slide^[w],e);
          inc(w,e);
          inc(d,e);
        end else repeat
          slide^[w]:=slide^[d];
          inc(w);
          inc(d);
          dec(e);
        until e=0;
        if w=WSIZE then begin
          if not flush(w) then begin
            explode_lit4:=unzip_WriteErr;
            exit
          end;
          w:=0; u:=0;
        end;
      until n=0;
    end;
  end;
  if totalabort then explode_lit4:=unzip_userabort
  else
  if not flush(w) then explode_lit4:=unzip_WriteErr
  else
    if zipeof then explode_lit4:=unzip_readErr
  else explode_lit4:=unzip_Ok;
end;

{******************exploding, method: 8k slide, 2 trees ***********************}

function explode_nolit8(tl,td:phuftlist;bl,bd:integer):integer;
var s:longint;
    e:word;
    n,d:word;
    w:word;
    t:phuft;
    ml,md:word;
    u:word;

begin
  b:=0; k:=0; w:=0;
  u:=1;
  ml:=mask_bits[bl];
  md:=mask_bits[bd];
  s:=uncompsize;
  while (s>0) and not (totalabort or zipeof) do begin
    NEEDBITS(1);
    if (b and 1)<>0 then begin  {Litteral}
      DUMPBITS(1);
      dec(s);
      NEEDBITS(8);
      slide^[w]:=char(b);
      inc(w);
      if w=WSIZE then begin
        if not flush(w) then begin
          explode_nolit8:=unzip_WriteErr;
          exit
        end;
        w:=0; u:=0;
      end;
      DUMPBITS(8);
    end else begin
      DUMPBITS(1);
      NEEDBITS(7);
      d:=b and $7F;
      DUMPBITS(7);
      NEEDBITS(bd);
      t:=@td^[(not b) and md];
      e:=t^.e;
      if e>16 then repeat
        if e=99 then begin
          explode_nolit8:=unzip_ZipFileErr;
          exit
        end;
        DUMPBITS(t^.b);
        dec(e,16);
        NEEDBITS(e);
        t:=@t^.v_t^[(not b) and mask_bits[e]];
        e:=t^.e;
      until e<=16;
      DUMPBITS(t^.b);

      d:=w-d-t^.v_n;
      NEEDBITS(bl);
      t:=@tl^[(not b) and ml];
      e:=t^.e;
      if e>16 then repeat
        if e=99 then begin
          explode_nolit8:=unzip_ZipFileErr;
          exit
        end;
        DUMPBITS(t^.b);
        dec(e,16);
        NEEDBITS(e);
        t:=@t^.v_t^[(not b) and mask_bits[e]];
        e:=t^.e;
      until e<=16;

      DUMPBITS(t^.b);

      n:=t^.v_n;
      if e<>0 then begin
        NEEDBITS(8);
        inc(n,b and $ff);
        DUMPBITS(8);
      end;
      dec(s,n);
      repeat
        d:=d and pred(WSIZE);
        if d>w then e:=WSIZE-d else e:=WSIZE-w;
        if e>n then e:=n;
        dec(n,e);
        if (u<>0) and (w<=d) then begin
          fillchar(slide^[w],e,#0);
          inc(w,e);
          inc(d,e);
        end else if (w-d>=e) then begin
          move(slide^[d],slide^[w],e);
          inc(w,e);
          inc(d,e);
        end else repeat
          slide^[w]:=slide^[d];
          inc(w);
          inc(d);
          dec(e);
        until e=0;
        if w=WSIZE then begin
          if not flush(w) then begin
            explode_nolit8:=unzip_WriteErr;
            exit
          end;
          w:=0; u:=0;
        end;
      until n=0;
    end;
  end;
  if totalabort then explode_nolit8:=unzip_userabort
  else
  if not flush(w) then explode_nolit8:=unzip_WriteErr
  else
    if zipeof then explode_nolit8:=unzip_readErr
  else explode_nolit8:=unzip_Ok;
end;

{******************exploding, method: 4k slide, 2 trees ***********************}

function explode_nolit4(tl,td:phuftlist;bl,bd:integer):integer;
var s:longint;
    e:word;
    n,d:word;
    w:word;
    t:phuft;
    ml,md:word;
    u:word;

begin
  b:=0; k:=0; w:=0;
  u:=1;
  ml:=mask_bits[bl];
  md:=mask_bits[bd];
  s:=uncompsize;
  while (s>0) and not (totalabort or zipeof) do begin
    NEEDBITS(1);
    if (b and 1)<>0 then begin  {Litteral}
      DUMPBITS(1);
      dec(s);
      NEEDBITS(8);
      slide^[w]:=char(b);
      inc(w);
      if w=WSIZE then begin
        if not flush(w) then begin
          explode_nolit4:=unzip_WriteErr;
          exit
        end;
        w:=0; u:=0;
      end;
      DUMPBITS(8);
    end else begin
      DUMPBITS(1);
      NEEDBITS(6);
      d:=b and $3F;
      DUMPBITS(6);
      NEEDBITS(bd);
      t:=@td^[(not b) and md];
      e:=t^.e;
      if e>16 then repeat
        if e=99 then begin
          explode_nolit4:=unzip_ZipFileErr;
          exit
        end;
        DUMPBITS(t^.b);
        dec(e,16);
        NEEDBITS(e);
        t:=@t^.v_t^[(not b) and mask_bits[e]];
        e:=t^.e;
      until e<=16;
      DUMPBITS(t^.b);
      d:=w-d-t^.v_n;
      NEEDBITS(bl);
      t:=@tl^[(not b) and ml];
      e:=t^.e;
      if e>16 then repeat
        if e=99 then begin
          explode_nolit4:=unzip_ZipFileErr;
          exit
        end;
        DUMPBITS(t^.b);
        dec(e,16);
        NEEDBITS(e);
        t:=@t^.v_t^[(not b) and mask_bits[e]];
        e:=t^.e;
      until e<=16;

      DUMPBITS(t^.b);
      n:=t^.v_n;
      if e<>0 then begin
        NEEDBITS(8);
        inc(n,b and $ff);
        DUMPBITS(8);
      end;
      dec(s,n);
      repeat
        d:=d and pred(WSIZE);
        if d>w then e:=WSIZE-d else e:=WSIZE-w;
        if e>n then e:=n;
        dec(n,e);
        if (u<>0) and (w<=d) then begin
          fillchar(slide^[w],e,#0);
          inc(w,e);
          inc(d,e);
        end else if (w-d>=e) then begin
          move(slide^[d],slide^[w],e);
          inc(w,e);
          inc(d,e);
        end else repeat
          slide^[w]:=slide^[d];
          inc(w);
          inc(d);
          dec(e);
        until e=0;
        if w=WSIZE then begin
          if not flush(w) then begin
            explode_nolit4:=unzip_WriteErr;
            exit
          end;
          w:=0; u:=0;
        end;
      until n=0;
    end;
  end;
  if totalabort then explode_nolit4:=unzip_userabort
  else
  if not flush(w) then explode_nolit4:=unzip_WriteErr
  else
    if zipeof then explode_nolit4:=unzip_readErr
  else explode_nolit4:=unzip_Ok;
end;

{****************************** explode *********************************}

function explode:integer;
var r:integer;
    tb,tl,td:phuftlist;
    bb,bl,bd:integer;
    l:array[0..255] of word;

begin
  inpos:=0;
  readpos:=-1;  {Nothing read in}
  bl:=7;
  if compsize>200000 then bd:=8 else bd:=7;
  if hufttype and 4<>0 then begin
    bb:=9;
    r:=get_tree(@l[0],256);
    if r<>0 then begin
      explode:=unzip_ZipFileErr;
      exit
    end;
    r:=huft_build(@l,256,256,nil,nil,@tb,bb);
    if r<>0 then begin
      if r=huft_incomplete then huft_free(tb);
      explode:=unzip_ZipFileErr;
      exit
    end;
    r:=get_tree(@l[0],64);
    if r<>0 then begin
      huft_free(tb);
      explode:=unzip_ZipFileErr;
      exit
    end;
    r:=huft_build(@l,64,0,pushlist(@cplen3),pushlist(@extra),@tl,bl);
    if r<>0 then begin
      if r=huft_incomplete then huft_free(tl);
      huft_free(tb);
      explode:=unzip_ZipFileErr;
      exit
    end;
    r:=get_tree(@l[0],64);
    if r<>0 then begin
      huft_free(tb);
      huft_free(tl);
      explode:=unzip_ZipFileErr;
      exit
    end;
    if hufttype and 2<>0 then begin {8k}
      r:=huft_build(@l,64,0,pushlist(@cpdist8),pushlist(@extra),@td,bd);
      if r<>0 then begin
        if r=huft_incomplete then huft_free(td);
        huft_free(tb);
        huft_free(tl);
        explode:=unzip_ZipFileErr;
        exit
      end;
      r:=explode_lit8(tb,tl,td,bb,bl,bd);
    end else begin
      r:=huft_build(@l,64,0,pushlist(@cpdist4),pushlist(@extra),@td,bd);
      if r<>0 then begin
        if r=huft_incomplete then huft_free(td);
        huft_free(tb);
        huft_free(tl);
        explode:=unzip_ZipFileErr;
        exit
      end;
      r:=explode_lit4(tb,tl,td,bb,bl,bd);
    end;
    huft_free(td);
    huft_free(tl);
    huft_free(tb);
  end else begin       {No literal tree}
    r:=get_tree(@l[0],64);
    if r<>0 then begin
      explode:=unzip_ZipFileErr;
      exit
    end;
    r:=huft_build(@l,64,0,pushlist(@cplen2),pushlist(@extra),@tl,bl);
    if r<>0 then begin
      if r=huft_incomplete then huft_free(tl);
      explode:=unzip_ZipFileErr;
      exit
    end;

    r:=get_tree(@l[0],64);
    if r<>0 then begin
      huft_free(tl);
      explode:=unzip_ZipFileErr;
      exit
    end;
    if hufttype and 2<>0 then begin {8k}
      r:=huft_build(@l,64,0,pushlist(@cpdist8),pushlist(@extra),@td,bd);
      if r<>0 then begin
        if r=huft_incomplete then huft_free(td);
        huft_free(tl);
        explode:=unzip_ZipFileErr;
        exit
      end;
      r:=explode_nolit8(tl,td,bl,bd);
    end else begin
      r:=huft_build(@l,64,0,pushlist(@cpdist4),pushlist(@extra),@td,bd);
      if r<>0 then begin
        if r=huft_incomplete then huft_free(td);
        huft_free(tl);
        explode:=unzip_ZipFileErr;
        exit
      end;
      r:=explode_nolit4(tl,td,bl,bd);
    end;
    huft_free(td);
    huft_free(tl);
  end;
  explode:=r;
end;


begin end.
{--- end of file ---}         