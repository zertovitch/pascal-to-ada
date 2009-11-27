TITLE Test_External for P2Ada

DATA   Segment Word Public
ASSUME ds:DATA
DATA   ends

CODE   Segment Byte Public

ASSUME cs:CODE

PUBLIC Test_External

Test_External PROC FAR
       RET
Test_External ENDP

CODE   ENDS

END
