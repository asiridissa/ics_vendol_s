&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          pawning          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/*                                                                */
/* DEFINE INPUT  PARAMETER hndlMainWindow AS HANDLE     NO-UNDO.  */
/* DEFINE INPUT  PARAMETER INPCOL         AS DECIMAL     NO-UNDO. */
/* DEFINE INPUT  PARAMETER INPROW         AS DECIMAL     NO-UNDO. */

DEFINE VARIABLE MOD AS INTEGER    NO-UNDO.
DEFINE VARIABLE mAns AS LOGICAL    NO-UNDO.
DEFINE VARIABLE curr-record  AS RECID.
DEFINE VARIABLE chk AS LOGICAL     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwPaperDetails

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES NewsPaper

/* Definitions for BROWSE brwPaperDetails                               */
&Scoped-define FIELDS-IN-QUERY-brwPaperDetails NewsPaper.PaperCode ~
NewsPaper.Descr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwPaperDetails 
&Scoped-define QUERY-STRING-brwPaperDetails FOR EACH NewsPaper NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwPaperDetails OPEN QUERY brwPaperDetails FOR EACH NewsPaper NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwPaperDetails NewsPaper
&Scoped-define FIRST-TABLE-IN-QUERY-brwPaperDetails NewsPaper


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwPaperDetails}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnAdd btnModify btnDelete brwPaperDetails ~
RECT-11 RECT-12 RECT-9 RECT-13 
&Scoped-Define DISPLAYED-OBJECTS filCode fildesc 

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
     SIZE 14 BY 1
     BGCOLOR 15 FGCOLOR 1 FONT 10.

DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 14 BY 1
     BGCOLOR 15 FGCOLOR 1 FONT 10.

DEFINE BUTTON btnDelete 
     LABEL "Delete" 
     SIZE 14 BY 1
     BGCOLOR 15 FGCOLOR 1 FONT 10.

DEFINE BUTTON btnModify 
     LABEL "Modify" 
     SIZE 14 BY 1
     BGCOLOR 15 FGCOLOR 1 FONT 10.

DEFINE BUTTON btnSave 
     LABEL "Save" 
     SIZE 14 BY 1
     BGCOLOR 15 FGCOLOR 1 FONT 10.

DEFINE VARIABLE filCode AS CHARACTER FORMAT "X(3)":U 
     LABEL "Paper Code" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     BGCOLOR 15 FGCOLOR 1 FONT 10 NO-UNDO.

DEFINE VARIABLE fildesc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88
     BGCOLOR 15 FGCOLOR 1 FONT 10 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 107 BY 3.5.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 44 BY 1.42
     BGCOLOR 3 .

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 30 BY 1.42
     BGCOLOR 3 .

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 107 BY 11.27.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwPaperDetails FOR 
      NewsPaper SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwPaperDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwPaperDetails C-Win _STRUCTURED
  QUERY brwPaperDetails NO-LOCK DISPLAY
      NewsPaper.PaperCode COLUMN-LABEL "Paper Code" FORMAT "x(8)":U
            WIDTH 25.57
      NewsPaper.Descr COLUMN-LABEL "Description" FORMAT "x(50)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 103.57 BY 10.58
         BGCOLOR 15 FGCOLOR 4 FONT 10
         TITLE BGCOLOR 15 FGCOLOR 4 "Paper Code Details" ROW-HEIGHT-CHARS .66 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     filCode AT ROW 16.31 COL 30 COLON-ALIGNED
     fildesc AT ROW 17.38 COL 30 COLON-ALIGNED
     btnSave AT ROW 22.81 COL 77.57
     btnCancel AT ROW 22.81 COL 92
     btnAdd AT ROW 22.81 COL 9.72
     btnModify AT ROW 22.81 COL 24
     btnDelete AT ROW 22.81 COL 38.43
     brwPaperDetails AT ROW 2.27 COL 6.72 WIDGET-ID 100
     "*" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 16.46 COL 44.29 WIDGET-ID 4
          FGCOLOR 12 
     "*" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 17.58 COL 73.14 WIDGET-ID 2
          FGCOLOR 12 
     RECT-11 AT ROW 15.5 COL 5
     RECT-12 AT ROW 22.58 COL 9
     RECT-9 AT ROW 2 COL 4.86
     RECT-13 AT ROW 22.58 COL 76.72 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 115 BY 25
         FONT 10.


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
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "News Paper Details"
         COLUMN             = 50.86
         ROW                = 1
         HEIGHT             = 25.04
         WIDTH              = 115.29
         MAX-HEIGHT         = 26.85
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 26.85
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("adeicon/trans%.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/trans%.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brwPaperDetails btnDelete DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnCancel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnSave IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filCode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fildesc IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwPaperDetails
/* Query rebuild information for BROWSE brwPaperDetails
     _TblList          = "pawning.NewsPaper"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > pawning.NewsPaper.PaperCode
"NewsPaper.PaperCode" "Paper Code" ? "character" ? ? ? ? ? ? no ? no no "25.57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > pawning.NewsPaper.Descr
"NewsPaper.Descr" "Description" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brwPaperDetails */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* News Paper Details */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* News Paper Details */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwPaperDetails
&Scoped-define SELF-NAME brwPaperDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwPaperDetails C-Win
ON VALUE-CHANGED OF brwPaperDetails IN FRAME DEFAULT-FRAME /* Paper Code Details */
DO:
  IF AVAILABLE pawning.NewsPaper  THEN
  DO:
      curr-record = RECID(pawning.NewsPaper).
      ASSIGN filCode = pawning.NewsPaper.PaperCode
             filDesc = pawning.NewsPaper.Descr. 
   END.
  ELSE
      ASSIGN filCode = '' filDesc = ''.
  DISP filCode filDesc WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME DEFAULT-FRAME /* Add */
DO:
  ASSIGN MOD = 1.
  ASSIGN filCode = '' filDesc = ''.
  ENABLE filCode filDesc btnSave btnCancel WITH FRAME {&FRAME-NAME}.
  DISABLE  btnadd btnmodify btnDelete brwPaperDetails WITH FRAME {&FRAME-NAME}.
  
  DISPLAY filCode filDesc WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    ENABLE btnAdd btnModify btnDelete brwPaperDetails WITH FRAME {&FRAME-NAME}.
    DISABLE btncancel btnSave filCode filDesc WITH FRAME {&FRAME-NAME}.
   
    
    OPEN QUERY brwPaperDetails FOR EACH pawning.NewsPaper.
    APPLY 'value-changed' TO brwPaperDetails.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Delete */
DO:

  IF AVAIL (pawning.NewsPaper) THEN
  DO:
      MESSAGE "Confirm to delete the record ?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "InBank Information System" UPDATE mAns.
      IF mAns THEN
      DO:
          FIND FIRST pawning.NewsPaper WHERE pawning.NewsPaper.PaperCode = filCode EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
          IF AVAILABLE pawning.NewsPaper THEN
          DO:
             DELETE pawning.NewsPaper.
             RELEASE pawning.NewsPaper.
          END.
          MESSAGE "Record successfully deleted."     VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "InBank Information System".
      END.
      ELSE
          MESSAGE "Cancelled by user." VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "InBank Information System".
          OPEN QUERY brwPaperDetails FOR EACH pawning.NewsPaper.
          APPLY 'value-changed' TO brwPaperDetails.
   END.
   ELSE
       MESSAGE "No record(s) Available "
           VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "InBank Information System".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnModify
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnModify C-Win
ON CHOOSE OF btnModify IN FRAME DEFAULT-FRAME /* Modify */
DO:
  IF AVAIL (pawning.NewsPaper) THEN
  DO:
      ASSIGN MOD = 2.
      ENABLE  filDesc btnSave btnCancel WITH FRAME {&FRAME-NAME}.
      DISABLE filCode btnModify btnAdd btnDelete brwPaperDetails WITH FRAME {&FRAME-NAME}.
  END.
  ELSE
      MESSAGE "No record(s)  Available" VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "InBank Information System".
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    IF TRIM(filCode) = ""  THEN 
    DO:
        MESSAGE "<" filCode:LABEL "> cannot be blank."    VIEW-AS ALERT-BOX WARNING BUTTONS OK TITLE "InBank Information System".
        APPLY "ENTRY":U TO filCode.
    END.
    ELSE IF NOT chk THEN
    DO:
        MESSAGE  "< " filCode:LABEL "> is invalid."   VIEW-AS ALERT-BOX WARNING BUTTONS OK TITLE "InBank Information System".
/*         APPLY "ENTRY":U TO filCode. */
/*         RETURN NO-APPLY. */
    END.
    ELSE IF TRIM(fildesc) = ""  THEN
    DO:
        MESSAGE "<" fildesc:LABEL "> cannot be blank."    VIEW-AS ALERT-BOX WARNING BUTTONS OK TITLE "InBank Information System".
        APPLY "ENTRY":U TO fildesc.
    END.
    ELSE
    DO:
        IF MOD = 1 THEN
        DO:
            MESSAGE "Confirm to save the record ?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "InBank Information System" UPDATE mAns.
            IF mAns THEN
            DO:
                FIND FIRST pawning.NewsPaper WHERE pawning.NewsPaper.PaperCode = filCode NO-LOCK NO-ERROR NO-WAIT. 
                IF NOT AVAILABLE pawning.NewsPaper THEN
                DO:
                    IF LENGTH(filCode) <> 3 THEN
                    DO:
                        MESSAGE "<Paper Code> must be 3 characters.." VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "InBank Information System".
                        APPLY "ENTRY":U TO filCode.
                        RETURN NO-APPLY.
                    END.
                    ELSE
                    DO:
                        CREATE pawning.NewsPaper.
                        ASSIGN pawning.NewsPaper.PaperCode = filCode
                        pawning.NewsPaper.Descr      = TRIM(fildesc).
                        curr-record = RECID(pawning.NewsPaper).
                        MESSAGE  "Record successfully saved." VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "InBank Information System".
                    END.
                END.
                ELSE
                DO:
                    MESSAGE "<Paper Code> already exist." VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "InBank Information System".
                    APPLY "ENTRY":U TO filCode.
                    RETURN NO-APPLY.
                END.
            END.
            ELSE
            DO:
                MESSAGE "Cancelled by user." VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "InBank Information System".
                RETURN NO-APPLY.
            END.
        END.

        ELSE IF MOD = 2 THEN
        DO:
            MESSAGE "Confirm to modify the record ?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "InBank Information System" UPDATE mAns.
            IF mAns THEN
            DO:
                FIND CURRENT pawning.NewsPaper.
                IF AVAILABLE pawning.NewsPaper THEN
                DO:
                    ASSIGN pawning.NewsPaper.Descr      = TRIM(fildesc).
                    MESSAGE  "Record successfully modified." VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "InBank Information System".
                END.
            END.
            ELSE
            DO:
                MESSAGE "Cancelled by user." VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "InBank Information System".
                RETURN NO-APPLY.
            END.
        END.
   
    
    OPEN QUERY brwPaperDetails FOR EACH pawning.NewsPaper.
    REPOSITION brwPaperDetails TO RECID(curr-record) NO-ERROR.
    APPLY 'value-changed' TO brwPaperDetails.
    DISPLAY filCode filDesc WITH FRAME {&FRAME-NAME}.
    ENABLE btnAdd btnModify btnDelete  brwPaperDetails WITH FRAME {&FRAME-NAME}.
    DISABLE btncancel filCode btnsave filDesc WITH FRAME {&FRAME-NAME}.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filCode C-Win
ON ANY-KEY OF filCode IN FRAME DEFAULT-FRAME /* Paper Code */
DO:
  IF INDEX("`!@#$%^&*()_-+=|\/~{~}[]:;'<>,?~~~"",KEYFUNCTION(LAST-KEY)) > 0 THEN
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filCode C-Win
ON LEAVE OF filCode IN FRAME DEFAULT-FRAME /* Paper Code */
DO:
    ASSIGN filCode filCode = CAPS(filCode).
/*     RUN validate_code.p(filCode,OUTPUT chk). */
    
    DISPLAY fiLCode WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fildesc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fildesc C-Win
ON LEAVE OF fildesc IN FRAME DEFAULT-FRAME /* Description */
DO:
  ASSIGN filDesc fildesc = TRIM(CAPS(SUBSTRING(filDesc,1,1)) + SUBSTRING(filDesc,2)).
  DISPLAY fildesc WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME}
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* ASSIGN CURRENT-WINDOW = hndlMainWindow. */


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
/*                                       */
/*     ASSIGN                            */
/*     FRAME {&FRAME-NAME}:COL = INPCOL  */
/*     FRAME {&FRAME-NAME}:ROW = INPROW. */

  RUN enable_UI.

  OPEN QUERY brwPaperDetails FOR EACH pawning.NewsPaper.
  ENABLE ALL EXCEPT btnSave btnCancel filcode filDesc WITH FRAME {&FRAME-NAME}.
  
  APPLY 'value-changed' TO brwPaperDetails.
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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
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
  DISPLAY filCode fildesc 
      WITH FRAME DEFAULT-FRAME.
  ENABLE btnAdd btnModify btnDelete brwPaperDetails RECT-11 RECT-12 RECT-9 
         RECT-13 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

