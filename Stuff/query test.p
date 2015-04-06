        
        DEFINE VARIABLE a AS INT64 NO-UNDO.
        a= ETIME(YES).
        
        DEFINE TEMP-TABLE tt-ldunld
         FIELDS ID       AS INT
         FIELDS SortID   AS INT
         FIELDS vehNo    AS INT
         FIELDS itmID    AS INT
         FIELDS itmName  AS CHAR
         FIELDS Weight   AS DEC
         FIELDS PriceP   AS DEC
         FIELDS PerCase  AS INT
         FIELDS BSC      AS INT
         FIELDS BSP      AS INT
         FIELDS GRRD      AS INT
         FIELDS GRST      AS INT
         FIELDS LDC      AS INT
         FIELDS LDP      AS INT
         FIELDS ULC      AS INT
         FIELDS ULP      AS INT
         FIELDS RDC      AS INT
         FIELDS RDP      AS INT
         FIELDS TolP     AS INT
         FIELDS TOlC     AS INT
         FIELDS Excess   AS INT
         FIELDS BilP     AS INT
         FIELDS Short    AS INT
         FIELDS Amount   AS DEC
         .

        DEFINE TEMP-TABLE tt-itms LIKE itms.
        DEFINE TEMP-TABLE tt-recipts LIKE recipts.
        DEFINE TEMP-TABLE tt-bills LIKE bills.
        DEFINE TEMP-TABLE tt-lorryStock LIKE lorryStock.
        
EMPTY TEMP-TABLE tt-bills.
EMPTY TEMP-TABLE tt-recipts.
EMPTY TEMP-TABLE tt-itms.
EMPTY TEMP-TABLE tt-lorryStock.

FOR EACH bills.
    CREATE tt-bills.
     tt-bills.bill#   = bills.bill#. 
     tt-bills.bilDate = bills.bilDate. 
     tt-bills.BillNo  = bills.BillNo. 
     tt-bills.vehNo   = bills.vehNo.
     tt-bills.bilDate = bills.bilDate.
END.

FOR EACH recipts.
    CREATE tt-recipts.
     tt-recipts.bill#       =   recipts.bill#       .
     tt-recipts.cases       =   recipts.cases       .
     tt-recipts.damageC     =   recipts.damageC     .
     tt-recipts.damP        =   recipts.damP        .
     tt-recipts.expC        =   recipts.expC        .
     tt-recipts.expP        =   recipts.expP        .
     tt-recipts.GRRD        =   recipts.GRRD .
     tt-recipts.GRST        =   recipts.GRST .
     tt-recipts.item#       =   recipts.item#       .
     tt-recipts.pieses      =   recipts.pieses      .
     tt-recipts.reciptID    =   recipts.reciptID    .
END.

FOR EACH itms.
    CREATE tt-itms.
     tt-itms.itmID         = itms.itmID         .
     tt-itms.itmName       = itms.itmName       .
     tt-itms.SortID        = itms.SortID        .
     tt-itms.unitWeightKG  = itms.unitWeightKG  .
     tt-itms.unitsPerCase  = itms.unitsPerCase  .
     tt-itms.unitPriceS    = itms.unitPriceS    .
     tt-itms.stockC        = itms.stockC        .
     tt-itms.stockP        = itms.stockP        .
END.

FOR EACH lorryStock.
    CREATE tt-lorryStock.
     tt-lorryStock.ID      = lorryStock.ID      .
     tt-lorryStock.itmID   = lorryStock.itmID   .
     tt-lorryStock.crDate  = lorryStock.crDate  .
     tt-lorryStock.VehID   = lorryStock.VehID   .
     tt-lorryStock.billedC = lorryStock.billedC .
     tt-lorryStock.billedP = lorryStock.billedP .
     tt-lorryStock.BSC     = lorryStock.BSC     .
     tt-lorryStock.BSP     = lorryStock.BSP     .
     tt-lorryStock.GRRD    = lorryStock.GRRD    .
     tt-lorryStock.GRST    = lorryStock.GRST    .
     tt-lorryStock.Excess  = lorryStock.Excess  .
     tt-lorryStock.LDC     = lorryStock.LDC     .
     tt-lorryStock.LDP     = lorryStock.LDP     .
     tt-lorryStock.RDP     = lorryStock.RDP     .
     tt-lorryStock.RDC     = lorryStock.RDC     .
     tt-lorryStock.Short   = lorryStock.Short   .
     tt-lorryStock.TolC    = lorryStock.TolC    .
     tt-lorryStock.TolP    = lorryStock.TolP    .
     tt-lorryStock.ULC     = lorryStock.ULC     .
     tt-lorryStock.ULP     = lorryStock.ULP     .
END.
        MESSAGE ETIME VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
        DEFINE VARIABLE lastBSDate  AS DATE    NO-UNDO.

        
        a = ETIME(yes).
        SELECT MAX(crDate) INTO lastBSDate FROM lorryStock WHERE VehID = 2 AND crDate < TODAY.

        MESSAGE STRING(ETIME) + " " + STRING(ETIME,"HH:MM:SS:") VIEW-AS ALERT-BOX INFO BUTTONS OK.

        a = ETIME(yes).
        FOR EACH tt-lorryStock WHERE tt-lorryStock.VehID = 2 AND tt-lorryStock.crDate < TODAY.
            ACCUMULATE tt-lorryStock.crDate (MAX).
        END.

        lastBSDate = ACCUM MAX tt-lorryStock.crDate.
        MESSAGE ETIME VIEW-AS ALERT-BOX INFO BUTTONS OK.

        a = ETIME(yes).
        FOR EACH lorryStock WHERE lorryStock.VehID = 2 AND lorryStock.crDate < TODAY.
            ACCUMULATE lorryStock.crDate (MAX).
        END.

        lastBSDate = ACCUM MAX lorryStock.crDate.
        MESSAGE ETIME VIEW-AS ALERT-BOX INFO BUTTONS OK.

        
