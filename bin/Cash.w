&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ics              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

  DEFINE VARIABLE calendr AS COM-HANDLE   NO-UNDO.
  DEFINE VARIABLE tempBill# AS INTEGER     NO-UNDO.
  DEFINE VARIABLE tempaymentID AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwPendingBills

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES bills Payments customer

/* Definitions for BROWSE brwPendingBills                               */
&Scoped-define FIELDS-IN-QUERY-brwPendingBills bills.BillNo bills.bilDate ~
bills.cusName bills.tol bills.paidAmount ~
decimal ( bills.tol - bills.paidAmount) 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwPendingBills 
&Scoped-define QUERY-STRING-brwPendingBills FOR EACH bills NO-LOCK ~
    BY bills.bilDate DESCENDING ~
       BY bills.BillNo DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwPendingBills OPEN QUERY brwPendingBills FOR EACH bills NO-LOCK ~
    BY bills.bilDate DESCENDING ~
       BY bills.BillNo DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwPendingBills bills
&Scoped-define FIRST-TABLE-IN-QUERY-brwPendingBills bills


/* Definitions for BROWSE brwTransHistory                               */
&Scoped-define FIELDS-IN-QUERY-brwTransHistory Payments.PaymentID ~
Payments.Amount Payments.PayMethod Payments.date Payments.stat 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwTransHistory 
&Scoped-define QUERY-STRING-brwTransHistory FOR EACH Payments NO-LOCK, ~
      EACH customer WHERE customer.cusID = Payments.CusID NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwTransHistory OPEN QUERY brwTransHistory FOR EACH Payments NO-LOCK, ~
      EACH customer WHERE customer.cusID = Payments.CusID NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwTransHistory Payments customer
&Scoped-define FIRST-TABLE-IN-QUERY-brwTransHistory Payments
&Scoped-define SECOND-TABLE-IN-QUERY-brwTransHistory customer


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwPendingBills}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSelect btnSelectPayment brwPendingBills ~
tickShowPaid filSearch cmbArea filBillNo cmbCus RECT-16 RECT-17 RECT-18 ~
RECT-20 
&Scoped-Define DISPLAYED-OBJECTS radPayType filAmount tickShowPaid ~
filSearch cmbArea filBillNo cmbCus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.

DEFINE BUTTON btnCancelR 
     LABEL "Cancel" 
     SIZE 15 BY 1.

DEFINE BUTTON btnPay 
     LABEL "Pay" 
     SIZE 15 BY 1.

DEFINE BUTTON btnRevert 
     LABEL "Revert" 
     SIZE 15 BY 1.

DEFINE BUTTON btnSelect 
     LABEL "Select Bill" 
     SIZE 15 BY 1.

DEFINE BUTTON btnSelectPayment 
     LABEL "Select Payment" 
     SIZE 15 BY 1.

DEFINE VARIABLE cmbArea AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Area" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "--Select Here--",0
     DROP-DOWN-LIST
     SIZE 35 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cmbCus AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Customer" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 25
     LIST-ITEM-PAIRS "--Select Here--",0
     DROP-DOWN-LIST
     SIZE 35 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE filAmount AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "Amount" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .88
     BGCOLOR 15 FGCOLOR 1 FONT 10 NO-UNDO.

DEFINE VARIABLE filBillNo AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "BillNo" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88
     BGCOLOR 15 FGCOLOR 0 FONT 10 NO-UNDO.

DEFINE VARIABLE filSearch AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .88
     BGCOLOR 15 FONT 10 NO-UNDO.

DEFINE VARIABLE radPayType AS CHARACTER INITIAL "Cash" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Cash", "Cash",
"Credit", "Credit",
"Cheque", "Cheque"
     SIZE 27.29 BY .85 NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 59 BY 6.46.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  
     SIZE 59 BY 2.23.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  
     SIZE 59 BY 3.31.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 59 BY 1.69.

DEFINE VARIABLE tickShowPaid AS LOGICAL INITIAL no 
     LABEL "Show Paid Bills also" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .77 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwPendingBills FOR 
      bills SCROLLING.

DEFINE QUERY brwTransHistory FOR 
      Payments, 
      customer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwPendingBills
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwPendingBills C-Win _STRUCTURED
  QUERY brwPendingBills NO-LOCK DISPLAY
      bills.BillNo FORMAT "999999":U WIDTH 7
      bills.bilDate FORMAT "99/99/99":U
      bills.cusName COLUMN-LABEL "Customer" FORMAT "x(200)":U WIDTH 50
      bills.tol COLUMN-LABEL "Total" FORMAT "->,>>>,>>>,>>9.99":U
      bills.paidAmount COLUMN-LABEL "Paid Amount" FORMAT "->>>,>>>,>>9.99":U
            WIDTH 16.72
      decimal ( bills.tol - bills.paidAmount) COLUMN-LABEL "To Pay" FORMAT "->,>>>,>>>,>>9.99":U
            COLUMN-FGCOLOR 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 120 BY 12.92
         FONT 10
         TITLE "Pending Bills" ROW-HEIGHT-CHARS .55 FIT-LAST-COLUMN.

DEFINE BROWSE brwTransHistory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwTransHistory C-Win _STRUCTURED
  QUERY brwTransHistory NO-LOCK DISPLAY
      Payments.PaymentID FORMAT ">>>>>>>>>>9":U
      Payments.Amount FORMAT ">,>>>,>>>,>>9.99":U
      Payments.PayMethod COLUMN-LABEL "Pay Method" FORMAT "x(20)":U
            WIDTH 11
      Payments.date FORMAT "99/99/9999":U WIDTH 12
      Payments.stat FORMAT "yes/no":U VIEW-AS TOGGLE-BOX
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 60.43 BY 11.81
         FONT 10
         TITLE "Transaction History" ROW-HEIGHT-CHARS .55 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSelect AT ROW 23.08 COL 7.72 WIDGET-ID 98
     radPayType AT ROW 20.81 COL 17.72 NO-LABEL WIDGET-ID 116
     filAmount AT ROW 21.73 COL 15.72 COLON-ALIGNED WIDGET-ID 102
     btnPay AT ROW 23.08 COL 24.72 WIDGET-ID 100
     btnCancel AT ROW 23.08 COL 41.72 WIDGET-ID 112
     btnSelectPayment AT ROW 24.73 COL 7.72 WIDGET-ID 122
     btnRevert AT ROW 24.73 COL 24.72 WIDGET-ID 120
     btnCancelR AT ROW 24.73 COL 41.72 WIDGET-ID 124
     brwPendingBills AT ROW 1.12 COL 1.43 WIDGET-ID 300
     brwTransHistory AT ROW 14.23 COL 61 WIDGET-ID 400
     tickShowPaid AT ROW 15.42 COL 17.72 WIDGET-ID 94
     filSearch AT ROW 14.42 COL 15.72 COLON-ALIGNED WIDGET-ID 96
     cmbArea AT ROW 17.23 COL 15.72 COLON-ALIGNED WIDGET-ID 80
     filBillNo AT ROW 19.38 COL 15.72 COLON-ALIGNED WIDGET-ID 104
     cmbCus AT ROW 18.31 COL 15.72 COLON-ALIGNED WIDGET-ID 92
     "Bill Date:" VIEW-AS TEXT
          SIZE 7.86 BY .62 AT ROW 16.31 COL 9.57 WIDGET-ID 132
     "Payment Type:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 20.88 COL 4.29 WIDGET-ID 134
     RECT-16 AT ROW 14.19 COL 1.43 WIDGET-ID 126
     RECT-17 AT ROW 20.58 COL 1.43 WIDGET-ID 128
     RECT-18 AT ROW 22.73 COL 1.43 WIDGET-ID 130
     RECT-20 AT ROW 22.73 COL 1.43 WIDGET-ID 136
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120.43 BY 25.35
         FONT 10 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Cache & Credit payments"
         COLUMN             = 12.29
         ROW                = 1.54
         HEIGHT             = 25.19
         WIDTH              = 120.72
         MAX-HEIGHT         = 27.65
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.65
         VIRTUAL-WIDTH      = 146.29
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brwPendingBills btnCancelR DEFAULT-FRAME */
/* BROWSE-TAB brwTransHistory brwPendingBills DEFAULT-FRAME */
/* SETTINGS FOR BROWSE brwTransHistory IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnCancel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnCancelR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnPay IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnRevert IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filAmount IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       filBillNo:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR RADIO-SET radPayType IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwPendingBills
/* Query rebuild information for BROWSE brwPendingBills
     _TblList          = "ics.bills"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "ics.bills.bilDate|no,ics.bills.BillNo|no"
     _FldNameList[1]   > ics.bills.BillNo
"bills.BillNo" ? "999999" "int64" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = ics.bills.bilDate
     _FldNameList[3]   > ics.bills.cusName
"bills.cusName" "Customer" ? "character" ? ? ? ? ? ? no ? no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ics.bills.tol
"bills.tol" "Total" "->,>>>,>>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ics.bills.paidAmount
"bills.paidAmount" "Paid Amount" "->>>,>>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no "16.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"decimal ( bills.tol - bills.paidAmount)" "To Pay" "->,>>>,>>>,>>9.99" ? ? 12 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brwPendingBills */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwTransHistory
/* Query rebuild information for BROWSE brwTransHistory
     _TblList          = "ics.Payments,ics.customer WHERE ics.Payments ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _JoinCode[2]      = "customer.cusID = Payments.CusID"
     _FldNameList[1]   > ics.Payments.PaymentID
"Payments.PaymentID" ? ">>>>>>>>>>9" "int64" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = ics.Payments.Amount
     _FldNameList[3]   > ics.Payments.PayMethod
"Payments.PayMethod" "Pay Method" ? "character" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ics.Payments.date
"Payments.date" ? ? "datetime" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ics.Payments.stat
"Payments.stat" ? ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE brwTransHistory */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 16.23
       COLUMN          = 17.72
       HEIGHT          = .81
       WIDTH           = 20.86
       WIDGET-ID       = 72
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
      CtrlFrame-2:MOVE-AFTER(cmbArea:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Cache  Credit payments */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit.
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Cache  Credit payments */
DO:
  /* This event will close the window and terminate the procedure.  */
    MESSAGE "Conferm to close?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn = YES THEN
    DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwPendingBills
&Scoped-define SELF-NAME brwPendingBills
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwPendingBills C-Win
ON VALUE-CHANGED OF brwPendingBills IN FRAME DEFAULT-FRAME /* Pending Bills */
DO:
    IF AVAILABLE bills THEN
    DO:
        tempBill# = bills.bill#.
        filBillNo = bills.BillNo.
        cmbArea = bills.areaCode.
        RUN cusLoader.
        cmbCus  = bills.cusID.
        calendr:VALUE = bills.bilDate.
    END.
/*     IF AVAILABLE Payments THEN */
/*     DO:                        */
    OPEN QUERY brwTransHistory FOR 
        EACH ics.Payments NO-LOCK,
      EACH ics.customer WHERE 
        customer.cusID = Payments.CusID AND
        bills.bill# = Payments.bill#
            NO-LOCK INDEXED-REPOSITION.
/*     END. */
  

  DISPLAY filBillNo cmbArea cmbCus WITH FRAME {&FRAME-NAME}.

 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwTransHistory
&Scoped-define SELF-NAME brwTransHistory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwTransHistory C-Win
ON VALUE-CHANGED OF brwTransHistory IN FRAME DEFAULT-FRAME /* Transaction History */
DO:
IF AVAILABLE Payments THEN
  tempaymentID = Payments.PaymentID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  DISABLE radPayType filAmount  btnPay btnCancel WITH FRAME {&FRAME-NAME}.

  calendr:ENABLED = TRUE.
  ENABLE btnSelectPayment brwPendingBills btnSelect filSearch tickShowPaid cmbArea cmbCus filBillNo  WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancelR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancelR C-Win
ON CHOOSE OF btnCancelR IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  DISABLE brwTransHistory radPayType filAmount  btnRevert btnCancelR WITH FRAME {&FRAME-NAME}.

  calendr:ENABLED = TRUE.
  ENABLE brwPendingBills btnSelectPayment btnSelect filSearch tickShowPaid cmbArea cmbCus filBillNo  WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPay C-Win
ON CHOOSE OF btnPay IN FRAME DEFAULT-FRAME /* Pay */
DO:
    IF filAmount = 0 THEN
    DO:
        MESSAGE "Enter an amount first." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    MESSAGE "Conferm to Pay this Bill?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn = YES THEN
    DO:
        FIND FIRST bills WHERE bills.bill# = tempBill# EXCLUSIVE-LOCK NO-ERROR.
            IF filAmount > (bills.tol - bills.paidAmount) THEN
            DO:
                MESSAGE "Too large amount to pay." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
                RETURN.
            END.
            ELSE IF filAmount <= (bills.tol - bills.paidAmount) THEN
            DO:
                CREATE Payments.
                 Payments.bill# = bills.bill#.
                 Payments.stat = YES.
                 Payments.PayMethod = radPayType.
                 Payments.date = TODAY.
                 Payments.CusID = cmbCus.
                 Payments.Amount = filAmount.
                 FIND FIRST paramtrs WHERE paramtrs.NAME = "PaymentID" EXCLUSIVE-LOCK NO-ERROR.
                    Payments.PaymentID = INT(paramtrs.val) + 1.
                    paramtrs.val = string(int(paramtrs.val) + 1).
                 RELEASE paramtrs.
                 bills.paidAmount = bills.paidAmount + filAmount.
            END.
        RELEASE bills.
    END.



  DISABLE radPayType filAmount  btnPay btnCancel WITH FRAME {&FRAME-NAME}.
  filAmount = 0.
  radPayType = "Cash".
  calendr:ENABLED = TRUE.
  ENABLE btnSelectPayment brwPendingBills btnSelect filSearch tickShowPaid cmbArea cmbCus filBillNo  WITH FRAME {&FRAME-NAME}.
  RUN filterGrid.
  DISPLAY filAmount radPayType WITH FRAME DEFAULT-FRAME. 
  OPEN QUERY brwPendingBills FOR EACH bills NO-LOCK
    BY bills.bilDate DESCENDING
     BY bills.BillNo DESCENDING.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRevert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRevert C-Win
ON CHOOSE OF btnRevert IN FRAME DEFAULT-FRAME /* Revert */
DO:
  APPLY "VALUE-CHANGED":U TO brwTransHistory.
  FIND FIRST Payments WHERE Payments.PaymentID = tempaymentID NO-ERROR.
  IF AVAILABLE Payments AND Payments.stat = NO THEN
  DO:
      MESSAGE "Already reverted payment." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      RETURN.
  END.

  IF Payments.PayMethod = "Cheque" THEN
  DO:
    {&WINDOW-NAME}:SENSITIVE = FALSE.
    RUN Cheques.w.
    {&WINDOW-NAME}:SENSITIVE = TRUE.
    filAmount = 0.
    radPayType = "Cash".
    calendr:ENABLED = TRUE.
    DISPLAY filAmount radPayType WITH FRAME DEFAULT-FRAME.
    RUN filterGrid.  
    RETURN.
  END.

  MESSAGE "Conferm to revert this transaction ?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
  IF yn = YES THEN
  DO:
      FIND FIRST Payments WHERE Payments.PaymentID = tempaymentID EXCLUSIVE-LOCK NO-ERROR.
        Payments.stat = NO.  
          FIND FIRST bills WHERE bills.bill# = tempBill# EXCLUSIVE-LOCK NO-ERROR.
            bills.paidAmount = bills.paidAmount - Payments.Amount.
          RELEASE bills.
      RELEASE Payments.
    RUN filterGrid.
  END.


  DISABLE brwTransHistory radPayType filAmount  btnRevert btnCancelR WITH FRAME {&FRAME-NAME}.

  calendr:ENABLED = TRUE.
  ENABLE brwPendingBills btnSelect btnSelectPayment filSearch tickShowPaid cmbArea cmbCus filBillNo  WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelect C-Win
ON CHOOSE OF btnSelect IN FRAME DEFAULT-FRAME /* Select Bill */
DO:
  ENABLE  radPayType filAmount btnPay btnCancel WITH FRAME {&FRAME-NAME}.

  calendr:ENABLED = FALSE.
  DISABLE btnSelectPayment brwPendingBills btnSelect filSearch tickShowPaid cmbArea cmbCus filBillNo  WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectPayment
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectPayment C-Win
ON CHOOSE OF btnSelectPayment IN FRAME DEFAULT-FRAME /* Select Payment */
DO:
  

  ENABLE brwTransHistory btnRevert btnCancelR WITH FRAME {&FRAME-NAME}.

  calendr:ENABLED = FALSE.
  DISABLE btnSelect brwPendingBills {&SELF-NAME} filSearch tickShowPaid cmbArea cmbCus filBillNo  WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbArea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbArea C-Win
ON ENTRY OF cmbArea IN FRAME DEFAULT-FRAME /* Area */
DO:
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbArea C-Win
ON LEFT-MOUSE-CLICK OF cmbArea IN FRAME DEFAULT-FRAME /* Area */
DO:
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbArea C-Win
ON VALUE-CHANGED OF cmbArea IN FRAME DEFAULT-FRAME /* Area */
DO:
  ASSIGN {&SELF-NAME}.
  RUN cusLoader.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbCus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbCus C-Win
ON ENTRY OF cmbCus IN FRAME DEFAULT-FRAME /* Customer */
DO:
/*   ASSIGN cmbArea. */
/*   ASSIGN cmbCus.  */
/*   RUN cusLoader.  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbCus C-Win
ON VALUE-CHANGED OF cmbCus IN FRAME DEFAULT-FRAME /* Customer */
DO:
/*     DEFINE VARIABLE tempCusName AS CHARACTER   NO-UNDO.                       */
/*     ASSIGN {&SELF-NAME}.                                                      */
/*     FIND FIRST customer WHERE customer.cusID = {&SELF-NAME} NO-LOCK NO-ERROR. */
/*         tempCusName = customer.cusName.                                       */
/*     RELEASE customer.                                                         */
/*                                                                               */
/*     tickShowPaid = NO.                                                        */
/*     filSearch = tempCusName.                                                  */
/*                                                                               */
/*     DISPLAY filSearch tickShowPaid WITH FRAME {&FRAME-NAME}.                  */
/*                                                                               */
/*     APPLY "VALUE-CHANGED":U TO filSearch.                                     */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-2 C-Win OCX.Click
PROCEDURE CtrlFrame-2.DTPicker.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

calendr:VALUE = STRING(TODAY,"99/99/9999").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filAmount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filAmount C-Win
ON LEAVE OF filAmount IN FRAME DEFAULT-FRAME /* Amount */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filBillNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filBillNo C-Win
ON LEAVE OF filBillNo IN FRAME DEFAULT-FRAME /* BillNo */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filSearch C-Win
ON VALUE-CHANGED OF filSearch IN FRAME DEFAULT-FRAME /* Search */
DO:
  ASSIGN {&SELF-NAME}.
  RUN filterGrid.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME radPayType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL radPayType C-Win
ON VALUE-CHANGED OF radPayType IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&SELF-NAME}.
  IF {&SELF-NAME} = "Cheque" THEN
  DO:
    {&WINDOW-NAME}:SENSITIVE = FALSE.
    RUN Cheques2.w(cmbArea,cmbCus).
    {&WINDOW-NAME}:SENSITIVE = TRUE.
    DISABLE radPayType filAmount  btnPay btnCancel WITH FRAME {&FRAME-NAME}.
    filAmount = 0.
    radPayType = "Cash".
    calendr:ENABLED = TRUE.
    ENABLE btnSelectPayment brwPendingBills btnSelect filSearch tickShowPaid cmbArea cmbCus filBillNo  WITH FRAME {&FRAME-NAME}.
    DISPLAY filAmount radPayType WITH FRAME DEFAULT-FRAME. 

    RUN filterGrid.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tickShowPaid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tickShowPaid C-Win
ON VALUE-CHANGED OF tickShowPaid IN FRAME DEFAULT-FRAME /* Show Paid Bills also */
DO:
  ASSIGN tickShowPaid.
  RUN filterGrid.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwPendingBills
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  DEFINE SHARED VARIABLE session_Path AS CHAR.
  DEFINE SHARED VARIABLE session_icon AS CHAR.
  {&WINDOW-NAME}:TITLE = session_Path.
  {&WINDOW-NAME}:LOAD-ICON(session_icon).

  calendr = chCtrlFrame-2:DTPicker.
  calendr:ENABLED = TRUE.
  calendr:VALUE = TODAY - 1.

  RUN areaLoader.
  APPLY "VALUE-CHANGED":U TO brwPendingBills.

  RUN filterGrid.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE areaLoader C-Win 
PROCEDURE areaLoader :
FOR EACH area BY area.ID.
  cmbArea:ADD-LAST(area.areaCode + " - " + ics.area.descrip,area.ID) IN FRAME {&FRAME-NAME}.
END.
DISPLAY cmbArea WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "Cash.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    CtrlFrame-2:NAME = "CtrlFrame-2":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "Cash.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cusLoader C-Win 
PROCEDURE cusLoader :
DEFINE VARIABLE areaCod AS CHARACTER     NO-UNDO.
DEFINE VARIABLE cmbList AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cnt     AS INTEGER     NO-UNDO.

DISPLAY WITH FRAME DEFAULT-FRAME.

cmbCus = 0.

IF cmbArea = 0 THEN
DO:
    MESSAGE "Select an Area first." VIEW-AS ALERT-BOX INFO BUTTONS OK.
    cmbCus:LIST-ITEM-PAIRS = "--Select Here--,0".
END.
ELSE
DO:
    FIND FIRST area WHERE ID = cmbArea EXCLUSIVE-LOCK NO-ERROR.
    areaCod = area.descrip.
    
    cmbCus:LIST-ITEM-PAIRS = "--Select Here--,0".
    
    FOR EACH customer WHERE CusArea = areaCod BY cusName.
        cmbCus:ADD-LAST(cusName,cusID) NO-ERROR.
        cnt = cnt + 1.
    END.
    IF cnt = 0 THEN
        MESSAGE "No Customers for this Area." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
END.

DISPLAY cmbCus WITH FRAME DEFAULT-FRAME.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  RUN control_load.
  DISPLAY radPayType filAmount tickShowPaid filSearch cmbArea filBillNo cmbCus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnSelect btnSelectPayment brwPendingBills tickShowPaid filSearch 
         cmbArea filBillNo cmbCus RECT-16 RECT-17 RECT-18 RECT-20 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterGrid C-Win 
PROCEDURE filterGrid :
IF tickShowPaid = NO THEN
DO:
    IF AVAILABLE bills THEN
    DO:
        OPEN QUERY brwPendingBills FOR EACH bills WHERE 
          (bills.tol - bills.paidAmount) <> 0 AND (
          string(bills.bill#) BEGINS filSearch OR 
          bills.cusName BEGINS filSearch           )
          NO-LOCK  BY bills.bilDate DESC BY bills.billNo DESC.
    END.
END.
ELSE IF tickShowPaid = YES THEN
DO:
    IF AVAILABLE bills THEN
    DO:
        OPEN QUERY brwPendingBills FOR EACH bills WHERE
          string(bills.bill#) BEGINS filSearch OR 
          bills.cusName BEGINS filSearch
             NO-LOCK BY bills.bilDate DESC BY bills.billNo DESC.
    END.
END.
APPLY "VALUE-CHANGED":U TO brwPendingBills  IN FRAME DEFAULT-FRAME.
APPLY "VALUE-CHANGED":U TO brwTransHistory  IN FRAME DEFAULT-FRAME.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

