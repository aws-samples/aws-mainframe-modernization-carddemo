      ******************************************************************
      *Working Storage Copybook for DATE related code
      ******************************************************************
           10 WS-EDIT-DATE-CCYYMMDD.
              20 WS-EDIT-DATE-CCYY.
                 25 WS-EDIT-DATE-CC                PIC X(2).
                 25 WS-EDIT-DATE-CC-N REDEFINES    WS-EDIT-DATE-CC
                                                   PIC 9(2).
                    88 THIS-CENTURY                VALUE 20.
                    88 LAST-CENTURY                VALUE 19.
                 25 WS-EDIT-DATE-YY                PIC X(2).
                 25 WS-EDIT-DATE-YY-N REDEFINES    WS-EDIT-DATE-YY
                                                   PIC 9(2).
              20 WS-EDIT-DATE-CCYY-N  REDEFINES
                 WS-EDIT-DATE-CCYY                 PIC 9(4).
              20 WS-EDIT-DATE-MM                   PIC X(2).
              20 WS-EDIT-DATE-MM-N REDEFINES WS-EDIT-DATE-MM
                                                   PIC 9(2).
                 88 WS-VALID-MONTH                 VALUES
                                                   1 THROUGH 12.
                 88 WS-31-DAY-MONTH                VALUES
                                                   1, 3, 5, 7,
                                                   8, 10, 12.
                 88 WS-FEBRUARY                    VALUE 2.
              20 WS-EDIT-DATE-DD                   PIC X(2).
              20 WS-EDIT-DATE-DD-N REDEFINES WS-EDIT-DATE-DD
                                                   PIC 9(2).
                 88 WS-VALID-DAY                   VALUES
                                                   1 THROUGH 31.
                 88 WS-DAY-31                      VALUE 31.
                 88 WS-DAY-30                      VALUE 30.
                 88 WS-DAY-29                      VALUE 29.
                 88 WS-VALID-FEB-DAY               VALUES
                                                   1 THROUGH 28.
           10 WS-EDIT-DATE-CCYYMMDD-N REDEFINES
              WS-EDIT-DATE-CCYYMMDD                PIC 9(8).
           10 WS-EDIT-DATE-BINARY                  PIC S9(9) BINARY.
           10 WS-CURRENT-DATE.
              20 WS-CURRENT-DATE-YYYYMMDD          PIC X(8).
              20 WS-CURRENT-DATE-YYYYMMDD-N REDEFINES
                 WS-CURRENT-DATE-YYYYMMDD          PIC 9(8).
              20 WS-CURRENT-DATE-BINARY            PIC S9(9) BINARY.
           10 WS-EDIT-DATE-FLGS.
               88 WS-EDIT-DATE-IS-VALID            VALUE LOW-VALUES.
               88 WS-EDIT-DATE-IS-INVALID          VALUE '000'.
               20 WS-EDIT-YEAR-FLG                 PIC X(01).
                  88 FLG-YEAR-ISVALID              VALUE LOW-VALUES.
                  88 FLG-YEAR-NOT-OK               VALUE '0'.
                  88 FLG-YEAR-BLANK                VALUE 'B'.
               20 WS-EDIT-MONTH                    PIC X(01).
                  88 FLG-MONTH-ISVALID             VALUE LOW-VALUES.
                  88 FLG-MONTH-NOT-OK              VALUE '0'.
                  88 FLG-MONTH-BLANK               VALUE 'B'.
               20 WS-EDIT-DAY                      PIC X(01).
                  88 FLG-DAY-ISVALID               VALUE LOW-VALUES.
                  88 FLG-DAY-NOT-OK                VALUE '0'.
                  88 FLG-DAY-BLANK                 VALUE 'B'.
          10 WS-DATE-FORMAT                        PIC X(08)
                                                   VALUE 'YYYYMMDD'.
          10 WS-DATE-VALIDATION-RESULT .
               20 WS-SEVERITY                      PIC X(04).
               20 WS-SEVERITY-N                    REDEFINES
                  WS-SEVERITY                      PIC 9(4).
               20 FILLER                           PIC X(11)
                                                   VALUE 'Mesg Code:'.
               20 WS-MSG-NO                        PIC X(04).
               20 WS-MSG-NO-N                      REDEFINES
                  WS-MSG-NO                        Pic 9(4).
               20 FILLER                           PIC X(01)
                                                   VALUE SPACE.
               20 WS-RESULT                        PIC X(15).
               20 FILLER                           PIC X(01)
                                                   VALUE SPACE.
               20 FILLER                           PIC X(09)
                                                   VALUE 'TstDate:'.
               20 WS-DATE                          PIC X(10).
               20 FILLER                           PIC X(01)
                                                   VALUE SPACE.
               20 FILLER                           PIC X(10)
                                                   VALUE 'Mask used:' .
               20 WS-DATE-FMT                      PIC X(10).
               20 FILLER                           PIC X(01)
                                                   VALUE SPACE.
               20 FILLER                           PIC X(03)
                                                   VALUE SPACES.

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:15:59 CDT
      *
