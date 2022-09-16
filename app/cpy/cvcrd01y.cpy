000100 01  CC-WORK-AREAS.                                               00010004
000200    05 CC-WORK-AREA.                                              00020001
000900       10 CCARD-AID                         PIC X(5).             00090001
001000          88  CCARD-AID-ENTER                VALUE 'ENTER'.       00100001
001100          88  CCARD-AID-CLEAR                VALUE 'CLEAR'.       00110001
001200          88  CCARD-AID-PA1                  VALUE 'PA1  '.       00120001
001300          88  CCARD-AID-PA2                  VALUE 'PA2  '.       00130001
001400          88  CCARD-AID-PFK01                VALUE 'PFK01'.       00140001
001500          88  CCARD-AID-PFK02                VALUE 'PFK02'.       00150001
001600          88  CCARD-AID-PFK03                VALUE 'PFK03'.       00160001
001700          88  CCARD-AID-PFK04                VALUE 'PFK04'.       00170001
001800          88  CCARD-AID-PFK05                VALUE 'PFK05'.       00180001
001900          88  CCARD-AID-PFK06                VALUE 'PFK06'.       00190001
002000          88  CCARD-AID-PFK07                VALUE 'PFK07'.       00200001
002100          88  CCARD-AID-PFK08                VALUE 'PFK08'.       00210001
002200          88  CCARD-AID-PFK09                VALUE 'PFK09'.       00220001
002300          88  CCARD-AID-PFK10                VALUE 'PFK10'.       00230001
002400          88  CCARD-AID-PFK11                VALUE 'PFK11'.       00240001
002500          88  CCARD-AID-PFK12                VALUE 'PFK12'.       00250001
002600*      10  CCARD-LAST-PROG                  PIC X(8).             00260001
002700       10  CCARD-NEXT-PROG                  PIC X(8).             00270001
002800*      10  CCARD-RETURN-TO-PROG             PIC X(8).             00280001
003300       10  CCARD-NEXT-MAPSET                PIC X(7).             00330001
003400       10  CCARD-NEXT-MAP                   PIC X(7).             00340001
003500*      10  CCARD-RETURN-FLAG                PIC X(1).             00350001
003600*        88  CCARD-RETURN-FLAG-OFF          VALUE LOW-VALUES.     00360001
003700*        88  CCARD-RETURN-FLAG-ON           VALUE '1'.            00370001
003800       10  CCARD-ERROR-MSG                  PIC X(75).            00380001
003900       10  CCARD-RETURN-MSG                 PIC X(75).            00390001
004000         88  CCARD-RETURN-MSG-OFF           VALUE LOW-VALUES.     00400001
004100*      10  CCARD-FUNCTION                   PIC X(1).             00410001
004200*        88  CCARD-NO-VALUE                  VALUE LOW-VALUES.    00420001
004300*        88  CCARD-GET-DATA                  VALUE '1'.           00430001
004400       10 CC-ACCT-ID                        PIC X(11)             00440005
004500                                            VALUE SPACES.
             10 CC-ACCT-ID-N REDEFINES CC-ACCT-ID PIC 9(11).
004600       10 CC-CARD-NUM                       PIC X(16)             00460005
004700                                            VALUE SPACES.         00470005
             10 CC-CARD-NUM-N REDEFINES CC-CARD-NUM PIC 9(16).
004800       10 CC-CUST-ID                        PIC X(09)             00480005
004900                                            VALUE SPACES.         00490005
004800       10 CC-CUST-ID-N REDEFINES CC-CUST-ID PIC 9(9).             00480005

      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:00 CDT
      *
