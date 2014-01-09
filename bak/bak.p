CONNECT -db E:\ICS\bak\bak -1 NO-ERROR.
PROPATH = "E:\ICS\bak," + PROPATH.
RUN Backup.r.
DISCONNECT bak.
QUIT.
