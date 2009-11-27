#!/bin/bash
#set -x
Pas2Ada="H:/newp2ada/p2ada.exe"
if [ "X" = "X"$1 -o "X-h" = "X"$1 ]; then
   echo "Borland Pascal to Ada converter: argument should be \"<Name>.adbi\" or \"<Name>.adsd\""
else
   for S in $@ ; do
      if [ ! -f $S ]; then
         echo "Script Error: File not found: \"$S\""
      #elif [ 1 == 1 ]; then
      #   echo "quitting $S"
      else
         SL=${#S}                # length
         let "SM = $SL - 5"      # length minus y, y=5
         S1=${S:0:SM}            # delete the last y characters of S
         S2=${S:$SM:$SL}         # the last y characters

         End="BAD"
         if [ "X.adbi" = "X"$S2 ]; then
            End=".adb2"
         elif [ "X.adsd" = "X"$S2 ]; then
            End=".ads2"
         fi
         Outp=$S1$End

         if [ $End = "BAD" ]; then
            echo "Script Error: Filetype of \"$S\" should be .adbi or .adsd";
         else
            echo "Converting \"$S\" and moving to \"$Outp\""
                 $Pas2Ada <$S >| $Outp
            T=$S1".PAS"
            #if [ -f $T ]; then
            #   Cmd4="/bin/mv -f $T $Outp"
            #   echo ""
            #   echo "Moving \"$T\" to \"$Outp\""
            #   echo "Cmd4 = "$Cmd4
            #else
            #   echo "Script Error: Pas2Ada did not produce output for \"$S\" ********"
            #fi
            echo ""
         fi
      fi
   done
fi