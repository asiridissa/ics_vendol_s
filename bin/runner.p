IF TODAY > 04/01/2014 THEN
DO:
    MESSAGE "Your activation period is expired." SKIP
        "Please contact System Administrator to activate."
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
END.
ELSE
DO:
    PROPATH = "E:\ICS\bin," + PROPATH.
    CONNECT -db E:\ICS\db\ics -1 NO-ERROR.
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


