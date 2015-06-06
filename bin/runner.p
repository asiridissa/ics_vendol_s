IF TODAY > 12/31/2015 THEN
DO:
    MESSAGE "Your activation period is expired." SKIP
        "Please contact System Administrator to activate."
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
END.
ELSE
DO:
    PROPATH = "E:\ICS_Vendol\bin," + PROPATH.
    PROPATH = REPLACE(PROPATH,"E:\ICS\bin,","").
    CONNECT -db E:\ICS_Vendol\db\ics -1 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        MESSAGE "Program already running." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        QUIT.
    END.
    ELSE
    DO:
        RUN value("login.r").
        DISCONNECT ics.
        QUIT.
    END.
END.


