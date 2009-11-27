#!/bin/bash
# Use Terry's MOD2P.EXE program in 40kb file MOD2PAS.ZIP
# Purpose: convert Modula to Borland Pascal (a 1st step on the
# way to converting to Ada 95)

# Note: Mod2P says "Could not open input file" when the file name
# has over 8 characters in the filename part before the ".".

#set -x # debug
Mod2pas="L:/\$QE_Asir_Qepcad_Mas/MAS_QE/cocor/MOD2PAS/MOD2P.EXE"
if [ "X" = "X"$1 -o "X-h" = "X"$1 ]; then
   echo "Modula to Borland Pascal converter: argument should be \"<Name>.mi\" or \"<Name>.md\""
else
   for S in $@ ; do
      if [ ! -f $S ]; then
         echo "Script Error: File not found: \"$S\""
      #elif [ 1 == 1 ]; then
      #   echo "quitting $S"
      else
         SL=${#S}                # length
         let "SM = $SL - 3"      # length minus 3
         S1=${S:0:SM}            # delete the last 3 characters of S
         S2=${S:$SM:$SL}         # the last 3 characters

         End="BAD"
         if [   "X.mi" = "X"$S2 ]; then
            End=".adbi"
            T=$S1".PAS"
         elif [ "Xmip" = "X"$S2 ]; then
            End="adbip"
            T=$S1"PAS"
         elif [ "X.md" = "X"$S2 ]; then
            End=".adsd"
            T=$S1".PAS"
         fi
         Outp=$S1$End

         if [ $End = "BAD" ]; then
            echo "Script Error: Filetype of \"$S\" should be .mi, .mip, or .md";
         else
            if [ -f $Outp ]; then
               Cmd2="/bin/rm -f $Outp"
               #echo "Deleting $Outp"
               $Cmd2
            fi
            echo "Converting "$S
                 $Mod2pas $S
            if [ -f $T ]; then
               Cmd4="/bin/mv -f $T $Outp"
               echo ""
               echo "Moving \"$T\" to \"$Outp\""
               $Cmd4
            else
               echo "Script Error: Mod2Pas did not produce output for \"$S\" ********"
            fi
            echo ""
         fi
      fi
   done
fi