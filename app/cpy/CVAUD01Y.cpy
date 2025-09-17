      ******************************************************************
      * CVAUD01Y.cpy - Parameter structure for audit subprogram       *
      * This copybook defines the input/output parameters for the      *
      * audit trail subprogram                                         *
      ******************************************************************
       01  AUDIT-PARMS.
      * Input Parameters
           05  AUDIT-IN-USER-ID          PIC X(8).
           05  AUDIT-IN-USER-TYPE        PIC X(1).
           05  AUDIT-IN-ACTION-TYPE      PIC X(1).
               88  AUDIT-IN-INSERT       VALUE 'I'.
               88  AUDIT-IN-UPDATE       VALUE 'U'.
               88  AUDIT-IN-DELETE       VALUE 'D'.
           05  AUDIT-IN-LOG-TYPE         PIC X(1).
               88  AUDIT-IN-CUSTOMER     VALUE 'C'.
               88  AUDIT-IN-ACCOUNT      VALUE 'A'.
               88  AUDIT-IN-TRANSACTION  VALUE 'T'.
           05  AUDIT-IN-RECORD-LENGTH    PIC 9(4) COMP.
           05  AUDIT-IN-RECORD-DATA      PIC X(500).
      * Output Parameters
           05  AUDIT-OUT-RETURN-CODE     PIC 9(2) COMP.
               88  AUDIT-SUCCESS         VALUE 00.
               88  AUDIT-WARNING         VALUE 04.
               88  AUDIT-ERROR           VALUE 08.
               88  AUDIT-SEVERE-ERROR    VALUE 12.
           05  AUDIT-OUT-ERROR-MSG       PIC X(80).
           05  AUDIT-OUT-SQLCODE         PIC S9(9) COMP.
           05  AUDIT-OUT-RESP            PIC S9(8) COMP.
           05  AUDIT-OUT-RESP2           PIC S9(8) COMP.