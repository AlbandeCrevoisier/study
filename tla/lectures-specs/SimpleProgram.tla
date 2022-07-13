--------------------------- MODULE SimpleProgram ---------------------------
EXTENDS Integers
VARIABLES i, pc   

Init == (pc = "start") /\ (i = 0)

Pick == /\ pc = "start"  
        /\ i' \in 0..1000
        /\ pc' = "middle"

Add1 == /\ pc = "middle" 
        /\ i' = i + 1
        /\ pc' = "done"  
           
Next == Pick \/ Add1
=============================================================================
\* Modification History
\* Last modified Wed Jul 13 00:39:27 CEST 2022 by ach
\* Created Wed Jul 13 00:38:30 CEST 2022 by ach
