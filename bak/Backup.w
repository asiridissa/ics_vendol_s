&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bak              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE VARIABLE selectedDate AS DATE        NO-UNDO.
/* Local Variable Definitions ---                                       */

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
&Scoped-define INTERNAL-TABLES backups

/* Definitions for BROWSE brw                                           */
&Scoped-define FIELDS-IN-QUERY-brw backups.dates 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw 
&Scoped-define QUERY-STRING-brw FOR EACH backups NO-LOCK ~
    BY backups.dates DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brw OPEN QUERY brw FOR EACH backups NO-LOCK ~
    BY backups.dates DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brw backups
&Scoped-define FIRST-TABLE-IN-QUERY-brw backups


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brw}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brw btnBackup btnBackupDel btnRestore ~
btnclose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBackup 
     LABEL "Backup" 
     SIZE 14 BY 2.

DEFINE BUTTON btnBackupDel 
     LABEL "Delete Backup" 
     SIZE 14 BY 2.

DEFINE BUTTON btnclose 
     LABEL "Close" 
     SIZE 14 BY 2.

DEFINE BUTTON btnRestore 
     LABEL "Restore" 
     SIZE 14 BY 2.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brw FOR 
      backups SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw C-Win _STRUCTURED
  QUERY brw NO-LOCK DISPLAY
      backups.dates FORMAT "99/99/9999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 16 BY 9.38
         FONT 10
         TITLE "Backups" ROW-HEIGHT-CHARS .55 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brw AT ROW 1 COL 1 WIDGET-ID 200
     btnBackup AT ROW 1.42 COL 18.14 WIDGET-ID 234
     btnBackupDel AT ROW 3.58 COL 18.14 WIDGET-ID 240
     btnRestore AT ROW 5.77 COL 18.14 WIDGET-ID 236
     btnclose AT ROW 8 COL 18.14 WIDGET-ID 242
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 32.57 BY 9.38
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
         TITLE              = "Error"
         COLUMN             = 46.57
         ROW                = 10.69
         HEIGHT             = 9.38
         WIDTH              = 32.29
         MAX-HEIGHT         = 9.38
         MAX-WIDTH          = 32.57
         VIRTUAL-HEIGHT     = 9.38
         VIRTUAL-WIDTH      = 32.57
         SHOW-IN-TASKBAR    = no
         CONTROL-BOX        = no
         MIN-BUTTON         = no
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw
/* Query rebuild information for BROWSE brw
     _TblList          = "bak.backups"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "bak.backups.dates|no"
     _FldNameList[1]   = bak.backups.dates
     _Query            is OPENED
*/  /* BROWSE brw */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Error */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. 
  IF THIS-PROCEDURE:PERSISTENT THEN  */ RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Error */
DO:
  /* This event will close the window and terminate the procedure.  */
  MESSAGE "Conferm to close?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
  IF yn THEN
  DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brw
&Scoped-define SELF-NAME brw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brw C-Win
ON VALUE-CHANGED OF brw IN FRAME DEFAULT-FRAME /* Backups */
DO:
  selectedDate = bak.backups.dates.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBackup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBackup C-Win
ON CHOOSE OF btnBackup IN FRAME DEFAULT-FRAME /* Backup */
DO:
    FIND FIRST backups WHERE dates = TODAY NO-LOCK NO-ERROR.
    IF NOT AVAILABLE backups THEN
    DO:
        MESSAGE "Conferm to Backup data?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
        IF yn = YES THEN
        DO:
            CREATE backups.
            dates = TODAY.
            
            DEFINE VARIABLE tempDate AS CHARACTER   NO-UNDO.
            tempDate = string(TODAY,"99999999").
            
            OUTPUT TO VALUE("E:\ICS\bak\backup.bat").
                PUT UNFORMAT "@echo off" SKIP.
                PUT UNFORMAT "e:" SKIP.
                PUT UNFORMAT "cd ICS\bak" SKIP.
                PUT UNFORMAT "mkdir " + tempDate SKIP.
                PUT UNFORMAT "xcopy E:\ICS\db E:\ICS\bak\" + tempDate + " /s /q /g /h /r /y" SKIP.
                PUT UNFORMAT "@echo off" SKIP.
                PUT UNFORMAT "exit".

            OUTPUT CLOSE.
            
            DOS SILENT START VALUE("E:\ICS\bak\backup.bat").

            OPEN QUERY brw FOR EACH bak.backups NO-LOCK BY backups.dates DESCENDING.
            MESSAGE "Backup success." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
        END.
    END.
    ELSE
    DO:
      MESSAGE "Backup already available for today." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBackupDel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBackupDel C-Win
ON CHOOSE OF btnBackupDel IN FRAME DEFAULT-FRAME /* Delete Backup */
DO:
  MESSAGE "Conferm to delete Backup data?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
  IF yn = YES THEN
  DO:
      APPLY "VALUE-CHANGED":U TO brw.
      FIND FIRST backups WHERE dates = selectedDate.
        DELETE backups.
      RELEASE backups.
      OPEN QUERY brw FOR EACH bak.backups NO-LOCK BY backups.dates DESCENDING.
      MESSAGE "Backup deleted successfully." VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnclose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnclose C-Win
ON CHOOSE OF btnclose IN FRAME DEFAULT-FRAME /* Close */
DO:
  APPLY "WINDOW-CLOSE":U TO {&WINDOW-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRestore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestore C-Win
ON CHOOSE OF btnRestore IN FRAME DEFAULT-FRAME /* Restore */
DO:
    MESSAGE "Conferm to restore database to selected date?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn AS LOGICAL.
    IF yn = YES THEN
    DO:
/*         MESSAGE "Would you like to Backup current database first?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE yn1 AS LOGICAL. */
/*         IF yn1 = YES THEN                                                                                                           */
/*         DO:                                                                                                                         */
/*             FIND FIRST backups WHERE dates = TODAY EXCLUSIVE-LOCK NO-ERROR.                                                         */
/*             IF AVAILABLE backups THEN                                                                                               */
/*                 DELETE backups.                                                                                                     */
/*             RELEASE backups.                                                                                                        */
/*                                                                                                                                     */
/*             CREATE backups.                                                                                                         */
/*             dates = TODAY.                                                                                                          */
/*                                                                                                                                     */
/*             DEFINE VARIABLE tempDate AS CHARACTER   NO-UNDO.                                                                        */
/*             tempDate = string(TODAY,"99999999").                                                                                    */
/*                                                                                                                                     */
/*             OUTPUT TO VALUE("E:\ICS\bak\backup.bat").                                                                               */
/*                 PUT UNFORMAT "e:" SKIP.                                                                                             */
/*                 PUT UNFORMAT "cd ICS\bak" SKIP.                                                                                     */
/*                 PUT UNFORMAT "mkdir " + tempDate SKIP.                                                                              */
/*                 PUT UNFORMAT "xcopy E:\ICS\db E:\ICS\bak\" + tempDate + " /s /q /g /h /r /y" SKIP.                                  */
/*                 PUT UNFORMAT "exit".                                                                                                */
/*             OUTPUT CLOSE.                                                                                                           */
/*                                                                                                                                     */
/*             DOS SILENT START VALUE("E:\ICS\bak\backup.bat").                                                                        */
/*                                                                                                                                     */
/*             OPEN QUERY brw FOR EACH bak.backups NO-LOCK BY backups.dates DESCENDING.                                                */
/*         END.                                                                                                                        */

        APPLY "VALUE-CHANGED":U TO brw.

        OUTPUT TO VALUE("E:\ICS\bak\restore.bat").
        PUT UNFORMAT "@echo off" SKIP.
            PUT UNFORMAT "xcopy E:\ICS\bak\" + STRING(selectedDate,"99999999") + " E:\ICS\db /s /q /g /h /r /y" SKIP.
            PUT UNFORMAT "@echo off" SKIP.
            PUT UNFORMAT "exit".
        OUTPUT CLOSE.
        DOS SILENT START VALUE("E:\ICS\bak\restore.bat").

        MESSAGE "Restored successfully." VIEW-AS ALERT-BOX INFO BUTTONS OK.

    END.
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

  CONNECT -db E:\ICS\bak\bak -1 NO-ERROR.

  {&WINDOW-NAME}:TITLE = "ICS Backup & Restore".
  {&WINDOW-NAME}:LOAD-ICON("E:\ICS\img\archive.ico").

  OPEN QUERY brw FOR EACH bak.backups NO-LOCK BY backups.dates DESCENDING.

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
  ENABLE brw btnBackup btnBackupDel btnRestore btnclose 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

