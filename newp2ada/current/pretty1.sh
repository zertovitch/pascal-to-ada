#!/bin/bash

Prettyifier="M:/ada/bin/iccfmt_nt.exe"
Prettyifier="H:/newp2ada/adagide1/reformat.exe"

#set -x  # debug
if [ "X" = "X"$1 -o "X-h" = "X"$1 ]; then
   echo "Prettify Ada 95 programs using irvine.com iccfmt_nt.exe"
else
   for S in $@ ; do
      if [ ! -f $S ]; then
         echo "Error: File not found: \"$S\""
      else
         Outp=$S"y"
         PCmd="$Prettyifier -i 3 -rw Lower_Case $S $Outp"
         echo "Prettifying \"$S\" --> \"$Outp\""
         $PCmd
      fi
   done
fi

# Adagide 6.52 program "reformat.exe"
#  http://www.usafa.af.mil/dfcs/bios/mcc_html/adagide.html
# The RTF and Options Win32 packages can excised and a few places that
#  refer to RTF can be altered (marked out by syntax errors).
#
# Usage h:\newp2ada\reformat.exe [options] infile [outfile]
# options =
#    -id Upper_Case|Lower_Case|Mixed_No_Force_Lower|Mixed_Force_Lower
#    -rw Upper_Case|Lower_Case|Mixed_No_Force_Lower|Mixed_Force_Lower
#    -c Colorize_Only|Colorize|Bold|Bold_Only|No_Colorize
#    -i N
#    -nip
#
# -id specifies case for identifiers
# -rw specifies case for reserved words
# -i specifies amount to indent each level
# -nip specifies not to indent based on parenthesis depth
# -nar specifies not to do advanced reformat
# -nrt specifies not to reformat types
# -c specifies whether to output colorized or bolded RTF, or plain ASCII
#

# -----------------------------------------------------------
# The irvine.com prettifier.
# The positioning of the comments seems to be not good enough (defaults to
#  column (and the source was not sighted)).
#
#         PCmd="$Prettyifier -output=$Outp"
#         PCmd="$PCmd -dotdot -declaration_case=mixed -object_case=mixed"
#         PCmd="$PCmd -unit_case=mixed -comment_column=1 $S"
#         echo "Prettifying \"$S\" --> \"$Outp\""
#
# $ pretty -h
# ICC Ada Source Code Formatter [v2.1.2 May 31, 2001]
# Invocation: iccfmt [qualifiers] [ada source files].
# Qualifers:
#  -ada83                      Recognize Ada83 syntax.                (false)
#  -ada95                      Recognize Ada95 syntax.                (true)
#  -help                       Display command line help.             (false)
#  -output=<arg>               Define output filename.                (<name>.fmt)
#  -quiet                      Execute quietly.                       (false)
# Page Formatting Options:
#  -comment_column=<arg>       Set column for comments.               (40)
#  -context_column=<arg>       Set column for WITH/USE clause.        (1)
#  -dotdot                     Pad '..' on both sides with spaces.    (false)
#  -pad_aggregate              Pad ()s around aggregates with spaces. (false)
#  -page_width=<arg>           Set page width.                        (79)
#  -parameter_align            Align the parameter modes.             (false)
#  -strip_comments             Strip source of all comments.          (false)
# Indentation Options:
#  -indentation=<arg>          Set default indentation.               (3)
#  -if_indent=<arg>            Set indentation for IF statements.     (-indent)
#  -for_indent=<arg>           Set indentation for FOR loops.         (-indent)
#  -loop_indent=<arg>          Set indentation for LOOP blocks.       (-indent)
#  -pragma_indent=<arg>        Set indentation for PRAGMA statements. (-indent)
#  -while_indent=<arg>         Set indentation for WHILE loops.       (-indent)
# Alphabetic Casing Options [upper, mixed, lower, untouched]:
#  -declaration_case=<arg>     Specify casing of declarations.        (untouched)
#  -keyword_case=<arg>         Specify casing of keywords.            (lower)
#  -object_case=<arg>          Specify casing of ada objects.         (untouched)
#  -unit_case=<arg>            Specify casing of unit declarations.   (untouched)
# Format Options:
#  -aggregate_format=<arg>     Format simple aggregate lists.         (paragraph)
#                                [single, paragraph]
#  -enumeration_format=<arg>   Format enumerated declarations.        (multi)
#                                [single, multi, paragraph]
