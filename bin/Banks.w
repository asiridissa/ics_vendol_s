&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ics              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEFINE SHARED VARIABLE session_Window AS INT.
DEFINE SHARED VARIABLE session_UserType AS CHARACTER.
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE addModify AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ModifyVal AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ModifyCnt AS INTEGER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brw

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Banks

/* Definitions for BROWSE brw                                           */
&Scoped-define FIELDS-IN-QUERY-brw Banks.ID Banks.bankCode Banks.descript ~
Banks.chqCode 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw 
&Scoped-define QUERY-STRING-brw FOR EACH Banks NO-LOCK ~
    BY Banks.bankCode INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brw OPEN QUERY brw FOR EACH Banks NO-LOCK ~
    BY Banks.bankCode INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brw Banks
&Scoped-define FIRST-TABLE-IN-QUERY-brw Banks


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brw}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brw btnAdd btnModify btnDelete 
&Scoped-Define DISPLAYED-OBJECTS filID filCode filchqCode filDescr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     LABEL "Add" 
     SIZE 14 BY 1.

DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 14 BY 1.

DEFINE BUTTON btnDelete 
     LABEL "Delete" 
     SIZE 14 BY 1.

DEFINE BUTTON btnModify 
     LABEL "Modify" 
     SIZE 14 BY 1.

DEFINE BUTTON btnSave 
     LABEL "Save" 
     SIZE 14 BY 1.

DEFINE VARIABLE filchqCode AS INT64 FORMAT ">>>>>>>>>>>>>":U INITIAL 0 
     LABEL "Cheque Code" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filCode AS CHARACTER FORMAT "X(10)":U 
     LABEL "Bank Code" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filDescr AS CHARACTER FORMAT "X(100)":U 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 50 BY .88
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE filID AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "ID" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88
     BGCOLOR 7 FGCOLOR 11  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brw FOR 
      Banks SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw C-Win _STRUCTURED
  QUERY brw NO-LOCK DISPLAY
      Banks.ID FORMAT ">>>9":U WIDTH 5
      Banks.bankCode COLUMN-LABEL "       Code" FORMAT "x(10)":U
      Banks.descript COLUMN-LABEL "                                          Description" FORMAT "x(50)":U
            WIDTH 45
      Banks.chqCode FORMAT ">>>>>>>>>>":U WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80 BY 10.5
         BGCOLOR 15 FGCOLOR 4 FONT 10
         TITLE BGCOLOR 15 FGCOLOR 4 "Bank Details" ROW-HEIGHT-CHARS .66 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brw AT ROW 1 COL 1 WIDGET-ID 200
     filID AT ROW 12.04 COL 19 COLON-ALIGNED WIDGET-ID 16
     filCode AT ROW 13.12 COL 19 COLON-ALIGNED WIDGET-ID 2
     filchqCode AT ROW 13.12 COL 54 COLON-ALIGNED WIDGET-ID 60
     filDescr AT ROW 14.19 COL 19 COLON-ALIGNED WIDGET-ID 4
     btnAdd AT ROW 15.54 COL 3 WIDGET-ID 6
     btnModify AT ROW 15.54 COL 17.29 WIDGET-ID 8
     btnDelete AT ROW 15.54 COL 31.72 WIDGET-ID 10
     btnSave AT ROW 15.54 COL 50 WIDGET-ID 12
     btnCancel AT ROW 15.54 COL 64.29 WIDGET-ID 14
     "*" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 14.19 COL 71.43 WIDGET-ID 58
          FGCOLOR 12 
     "*" VIEW-AS TEXT
          SIZE 1.72 BY .62 AT ROW 13.12 COL 36.43 WIDGET-ID 56
          FGCOLOR 12 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.14 BY 16
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
         TITLE              = "ICS -  Vehicals"
         COLUMN             = 37
         ROW                = 6
         HEIGHT             = 16
         WIDTH              = 80.14
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80.14
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80.14
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
   FRAME-NAME                                                           */
/* BROWSE-TAB brw 1 DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnCancel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSave IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filchqCode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filCode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filDescr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filID IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw
/* Query rebuild information for BROWSE brw
     _TblList          = "ics.Banks"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "ics.Banks.bankCode|yes"
     _FldNameList[1]   > ics.Banks.ID
"Banks.ID" ? ? "integer" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ics.Banks.bankCode
"Banks.bankCode" "       Code" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ics.Banks.descript
"Banks.descript" "                                          Description" ? "character" ? ? ? ? ? ? no ? no no "45" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ics.Banks.chqCode
"Banks.chqCode" ? ? "integer" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brw */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* ICS -  Vehicals */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit.
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* ICS -  Vehicals */
DO:
  /* This event will close the window and terminate the procedure.  */
  MESSAGE "Confrm to close the window?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
  IF yn = YES THEN
    DO:
      session_Window = session_Window - 1.
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN NO-APPLY.
    END.
  ELSE
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brw
&Scoped-define SELF-NAME brw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brw C-Win
ON VALUE-CHANGED OF brw IN FRAME DEFAULT-FRAME /* Bank Details */
DO:
    IF AVAILABLE Banks THEN
        ASSIGN
        filID      = ID
        filCode    = bankCode
        filDescr   = descript
        filchqCode = chqCode.
    DISPLAY filCode filDescr filID filchqCode WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME DEFAULT-FRAME /* Add */
DO:
  ENABLE filCode filDescr btnSave btnCancel filchqCode WITH FRAME {&FRAME-NAME}.
  DISABLE btnAdd btnDelete btnModify brw WITH FRAME {&FRAME-NAME}.
  
  FIND FIRST paramtrs WHERE NAME = "lastBankID".
  IF AVAILABLE paramtrs THEN
      filID    = INT(val) + 1.
      filCode  = "".
      filDescr = "".
      filchqCode = 0.
  addModify = "add".
  DISPLAY filCode filDescr filID filchqCode WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  filID    = 0.
  filCode  = "".
  filDescr = "".
  filchqCode = 0.
  DISPLAY filCode filDescr filID filchqCode WITH FRAME {&FRAME-NAME}.

  DISABLE filCode filDescr filID filchqCode btnSave btnCancel WITH FRAME {&FRAME-NAME}.
    ENABLE btnAdd btnDelete btnModify brw WITH FRAME {&FRAME-NAME}.

    OPEN QUERY brw FOR EACH Banks BY bankCode.
    APPLY "VALUE-CHANGED":U TO brw.
    RUN permissions.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Delete */
DO:
    IF filCode = "" THEN
        MESSAGE "No records to delete." VIEW-AS ALERT-BOX WARNING BUTTONS OK .
    ELSE
    DO:
        MESSAGE "Conferm to save the record?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE yn AS LOGICAL.
        IF yn THEN
        DO:
            FIND FIRST Banks WHERE ID = filID.
            IF AVAILABLE Banks THEN
                DELETE Banks.
            RELEASE Banks.
    
            OPEN QUERY brw FOR EACH Banks BY bankCode.
            APPLY "VALUE-CHANGED":U TO brw.
        END.
    END.
    RUN permissions.
    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnModify
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnModify C-Win
ON CHOOSE OF btnModify IN FRAME DEFAULT-FRAME /* Modify */
DO:
  ENABLE filDescr filchqCode btnSave btnCancel WITH FRAME {&FRAME-NAME}.
  DISABLE btnAdd btnDelete btnModify brw WITH FRAME {&FRAME-NAME}.
  addModify = "modify".
  ModifyVal = filCode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    IF filCode = "" THEN
    DO:
        MESSAGE "Bank Code cannot be a blank." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF LENGTH(filCode) < 4  THEN
    DO:
        MESSAGE "Bank Code must have minimum of 4 charactors." VIEW-AS ALERT-BOX ERROR BUTTONS OK .
        RETURN .
    END.
    IF filDescr = "" THEN
    DO:
        MESSAGE "Description cannot be a blank." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN .
    END.

/********************************************save************************************************/

    IF addModify = "add" THEN
    DO:
        FIND FIRST Banks WHERE bankCode = filCode OR descript  = filDescr EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE Banks THEN
            DO:
                MESSAGE "Bank Code or Description already exists" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                RETURN.
            END.
            ELSE IF NOT AVAILABLE Banks THEN
            DO:
                CREATE Banks.
                  ID       = filID.     
                  bankCode = filCode.   
                  descript = filDescr.  
                  chqCode  = filchqCode.

                FIND FIRST paramtrs WHERE NAME = "lastBankID".
                IF AVAILABLE paramtrs THEN
                    val    = STRING(INT(val) + 1).
                RELEASE paramtrs. 

            END.
    END.
    
    ELSE IF addModify = "modify" THEN
    DO:
        FOR EACH Banks WHERE bankCode = filCode OR descript = filDescr.
            ModifyCnt = ModifyCnt + 1.
        END.
        IF ModifyCnt > 1 THEN
        DO:
            MESSAGE "Bank Code or Description already exists.11" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            ModifyCnt = 0.
            APPLY "ENTRY":U TO filCode.
            RETURN.
        END.
        ELSE IF ModifyCnt = 1 THEN
        DO:
            FIND FIRST Banks WHERE ID = filID EXCLUSIVE-LOCK NO-ERROR.
                ASSIGN
                  ID       = filID     
                  bankCode = filCode  
                  descript = filDescr 
                  chqCode  = filchqCode.
        END.
        
    END.

    RELEASE Banks.

    OPEN QUERY brw FOR EACH Banks BY bankCode.
    APPLY "VALUE-CHANGED":U TO brw.
    RUN permissions.

    DISABLE filCode filDescr filID filchqCode btnSave btnCancel WITH FRAME {&FRAME-NAME}.
    ENABLE btnAdd btnDelete btnModify brw WITH FRAME {&FRAME-NAME}.

    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filchqCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filchqCode C-Win
ON LEAVE OF filchqCode IN FRAME DEFAULT-FRAME /* Cheque Code */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filCode C-Win
ON LEAVE OF filCode IN FRAME DEFAULT-FRAME /* Bank Code */
DO:
  ASSIGN {&SELF-NAME}.
  filCode = CAPS(filCode).
  DISPLAY filCode WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filDescr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filDescr C-Win
ON LEAVE OF filDescr IN FRAME DEFAULT-FRAME /* Description */
DO:
  ASSIGN filDescr.
  filDescr = CAPS(SUBSTRING(filDescr,1,1)) + SUBSTRING(filDescr,2).
  DISPLAY filDescr WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filID C-Win
ON ANY-KEY OF filID IN FRAME DEFAULT-FRAME /* ID */
DO:
  ASSIGN filCode.
  filCode = CAPS(filCode).
  DISPLAY filCode WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filID C-Win
ON LEAVE OF filID IN FRAME DEFAULT-FRAME /* ID */
DO:
  ASSIGN {&SELF-NAME}.
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filID C-Win
ON RETURN OF filID IN FRAME DEFAULT-FRAME /* ID */
DO:
  
    APPLY "CHOOSE":U TO btnSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filID C-Win
ON VALUE-CHANGED OF filID IN FRAME DEFAULT-FRAME /* ID */
DO:
  ASSIGN filCode.
  filCode = CAPS(filCode).
  DISPLAY filCode WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

  session_Window = session_Window + 1.
  RUN permissions.

  APPLY "VALUE-CHANGED":U TO brw.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY filID filCode filchqCode filDescr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brw btnAdd btnModify btnDelete 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE permissions C-Win 
PROCEDURE permissions :
/* IF session_UserType <> "Administrator" OR session_UserType <> "Super Admin" THEN */
/* DO:                                                                              */
/*         DISABLE btnModify btnDelete WITH FRAME {&FRAME-NAME}.                    */
/*     DISPLAY btnModify btnDelete WITH FRAME {&FRAME-NAME}.                        */
/* END.                                                                             */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

