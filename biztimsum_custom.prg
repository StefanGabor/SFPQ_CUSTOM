*  Program...........: bizTimsum_Custom
*  Author............: Tom Green
*  Project...........: Human Resource system
*  Created...........: June 25, 2002
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies Inc. 2006
*  Description.......: NOTE: In this vesion of time banks
*                    : module, the AP_EFFDT, and AP_ENDDT
*                    : are always the full year -- even
*                    : if employee starts mid-year, or
*                    : is terminated mid-year.
*                    : 
*                    : 
*                    : 
*  Classes...........: bizTIMSUM as BIZ
*                    : 
*                    : 
*                    : Modifié par Akli pour Adapter
*                    : les engins pour les banques 
*                    : de temp 
*                    : Méthodes ajoutées :
*                    : SetTimsum, Fix_End_TS_Dates
*							: AddRecTimsum
*#########################################################
 define class bizTimsum_Custom ;
 					as bizTimsum of bizTimsum.PRG
*#########################################################


*=========================================================
					**** HOT FIX	*****
*=========================================================
protected procedure GetPlannedAbs_Timedt(pcSwitches, ;
				lnPersid, pcBankid, pdFrom, pdThru, ;
				pnPlanned, pnPlannedD)

*/// TODO - Lots of combinations left to be tested

* Used for both totals and detail.
*   - If /TOTALS, returns pnPlanned and pnPlannedD
*   - If /CURSOR, returns a cursor with details
* plPlanned and pnPlannedD are OUTPUT parameters. 

*** This is very quick and dirty. It does NOT handle
*   ronding, multipliers, etc.  It is here mainly so that
*   ShowTimsumSummary() doesn't crash.

local lcWhere, loCursor, lnSelect, ldFuture, lcUnits

*** Select from TIMEDT starting with pdFrom, or tomorrow
*   if pdFrom is future.

ldFuture = max(date()+1, pdFrom)

if !empty(pdThru) and pdThru <= date()
	lcWhere = "1=2"
else
	lcWhere = "T_PERSID=" + lstr(lnPersid) + ;
				 " and T_EFFDT>=" + tochar(ldFuture, "/DTOS/QUOTES") + ;
				 iif(empty(pdthru), "", ;
				 " and T_EFFDT<=" + tochar(pdThru, "/DTOS/QUOTES"))
	if empty(pcBankid)
		lcWhere = lcWhere + " and inlist(T_DEST, '1P','1W'," + ;
				"'2P','2W','3P','3W','4P','4W','5P','5W'," + ;
				"'6p','6W','7P','7W','8P','8W','9P','9W')"
	else
		lcWhere = lcWhere + " and inlist(T_DEST, '" + pcBankid + "P','" + ;
					 pcBankid + "W')"
	endif
endif

if llTotals
	*** This will not work properly if pcBANKID is empty
	lcUNITS = iif(empty(pcBankid), "H", ;
				 TBLEVAL("TIMEBANK", pcBANKID, "TBLC1"))

	lnSelect = select()
	loCursor = goDataMgr.GetCursor("", ;
				  set("datasession"), "TIMEDT", ;
				  "sum(T_HOURS) as T_HOURS," + ;
				  "sum(T_AMT) as T_AMT", lcWhere)

	do case
	case lcUnits = "$"
		*** Dollars only
		pnPlanned = nvl(T_AMT, 0)
		pnPlannedD = nvl(T_AMT, 0)
	case left(lcUnits,1) = "H"
		*** Hours or Hours+Dollars
		pnPlanned = nvl(T_HOURS, 0)
		pnPlannedD = nvl(T_AMT, 0)
	otherwise
		*** Days or Days+Dollars
		pnPlanned = round(nvl(T_HOURS,0) / lnHrsPdy,4)
		pnPlannedD = nvl(T_AMT, 0)
	endcase
	loCursor = .t.
	select (lnSelect)
else
	loCursor = goDataMgr.GetCursor(pcSwitches, ;
				set("datasession"), "TIMEDT", ;
				"T_PERSID as V_PERSID, " + ;
				"substr(T_DEST,1,1) as V_BANKID, " + ;
				"T_DEST as V_DEST, " + ;
				"T_OPT as V_ETYPE, " + ;
				"T_EFFDT as V_EFFDT, " + ;
				"T_EFFDT as V_ENDDT, " + ;
				"T_HOURS as V_HOURS, " + ;
				"T_AMT as V_TOTAL, " + ;
				"T_NOTES as V_NOTES," + ;
				"T_UNIQID as V_UNIQID", ;
				lcWhere)
	alter table (alias()) ;
			add V_SESS M ;
			add V_UNITS C(2) ;
			add V_QTY N(10,2) ;
			add V_PLAN C(1)

	replace all V_PLAN with "P"
	replace all V_UNITS with tbleval("TIMEBANK", V_BANKID, "TBLC1")
	
	if V_HOURS != 0 and lnHrsPdy != 0
		replace all V_QTY with iif(left(V_UNITS,1) = "D", ;
					round(V_HOURS / lnHrsPdy, 2), val(V_UNITS))
	endif 
endif

return loCursor
endproc


*=========================================================
					**** HOT FIX	*****
*=========================================================
procedure PostAbsence_CalcAccumul
parameters pcSwitches, lnAMT, lnDOLLARS, lcACTION, ;
			  pcTimedtAlias, pcBankId, pcAplanAlias

*** Loads variables lnAMT and lnDOLLARS, to be added to
*   or subtracted from TAKEN(D), EARN(D), ACCRUE(D).

*** Applies rounding formula AP_ADJAMT. 
*   Also loads lcACTION.  Called by PostAbsence_BankAdd/Sub,
*	 and V_DETAIL.

* Pass arguments 2 to 4 by reference!
* Otherwise info is taken from select area <pcTimedtAlias>.T_*
* pcAplanAlias can also be a cursor or a view to APLAN

* NOTE. We call the arguments ln... (not pn...) because
* ROUNDEXP refers to lnAMT.

pcSwitches = iif(vartype(pcSwitches)="C", upper(pcSwitches), "")

*-- Private, not local, so they can be see by rounding expression
private llUSEADJ, lcROUNDEXP, lnHrsPer, llOld, llAdjust
private lcTraceInfo, lcBankid, lcOpt, lcPlanid
private lcDest, lcType, lcSource, lnTAmt, lnTHours
private lcUnitCd, lcUnitrate, lcAplanAlias, lnPersid, lcUnits
local llTimetmp, llDollars
local loBizJobhist, loCsrJobhist
store null to loBizJobhist, loCsrJobhist

*** Extract some values from vTIMEDT.  All names are LC...
llOld = "/OLD" $ pcSwitches
llTimetmp = "/TiMETMP" $ pcSwitches or type(pcTimedtAlias + ".TT_PERSID") = "N"
llDollars = "/DOLLARS" $ pcSwitches

if llTimetmp
	lnPersid = evaluate(pcTimedtAlias + ".TT_PERSID")
	lcDest = evaluate(pcTimedtAlias + ".TT_DEST")
	lcType = evaluate(pcTimedtAlias + ".TT_TYPE")
	lcSource = evx(pcTimedtAlias + ".TT_SOURCE", "F")
	lcOpt = evaluate(pcTimedtAlias + ".TT_OPT")
	lnTAmt = iif(type(pcTimedtAlias + ".TT_AMT") = "N", ;
				evaluate(pcTimedtAlias + ".TT_AMT"), 0)
	lnTHours = evaluate(pcTimedtAlias + ".TT_HOURS")
	lcUnitCd = "H"
	ldEffdt = evx(pcTimedtAlias + ".TT_EFFDT", date())
	llAdjust = .f.
else
	lnPersid = iif(llOld, oldval("T_PERSID", pcTimedtAlias), ;
				evaluate(pcTimedtAlias + ".T_PERSID"))
	lcDest = iif(llOld, oldval("T_DEST", pcTimedtAlias), ;
				evaluate(pcTimedtAlias + ".T_DEST"))
	lcType = iif(llOld, oldval("T_TYPE", pcTimedtAlias), ;
				evaluate(pcTimedtAlias + ".T_TYPE"))
	lcSource = iif(llOld, oldval("T_SOURCE", pcTimedtAlias), ;
				evaluate(pcTimedtAlias + ".T_SOURCE"))
	lcOpt = iif(llOld, oldval("T_OPT", pcTimedtAlias), ;
				evaluate(pcTimedtAlias + ".T_OPT"))
	lnTAmt = iif(llOld, oldval("T_AMT", pcTimedtAlias), ;
				evaluate(pcTimedtAlias + ".T_AMT"))
	lnTHours = iif(llOld, oldval("T_HOURS", pcTimedtAlias), ;
				evaluate(pcTimedtAlias + ".T_HOURS"))
	lcUnitCd = iif(llOld, oldval("T_UNITCD", pcTimedtAlias), ;
				evaluate(pcTimedtAlias + ".T_UNITCD"))
	ldEffdt = iif(llOld, oldval("T_EFFDT", pcTimedtAlias), ;
				evaluate(pcTimedtAlias + ".T_EFFDT"))
	llAdjust = (lcUnitCd = "A")
endif

*** Is this a deposit or withdrawl?
*-- Find next letter after bankid in DEST
lnAT = at(pcBANKID, lcDEST)
lcACTION = ltrim(chrtran(substr(lcDEST,lnAT+1,2),"0123456789", " "))
lcACTION = leftto(lcACTION," ")
*-- lcACTION codes are: D=Deposit, W=Withdrawl
*--                     A=Accrual deposit, B=Accrual withdrawl
*-- lcACTION may be multiple characters. For example: 'BD'
do case
*-- For compatiblity with older (less precise) T_DESTs...
case lcACTION = "P"
	*-- Pay out from bank.  That's a withdrawal
	lcACTION = "W"
case empty(lcACTION)
	*-- No action letter.  Infer action from T_TYPE.
	lcACTION = iif(lcTYPE="A" or "P"$lcDEST, "W", "D")
endcase

*** Initialize results
store 0 to lnAMT, lnDOLLARS

*** Info from the APLAN
lcAplanAlias = iif(type("pcAplanAlias") = "O",;
					pcAplanAlias.Alias, pcAplanAlias)

*** Get rounding rule (ADJAMT expression) from APLAN
if type("llNoRound") = "L" and llNoRound
	*** No rounding
	lcRoundExp = ""
else
	lcRoundExp = evalx(lcAplanAlias + ".AP_ADJAMT", "", "")
	lcROUNDEXP = strtranc(lcRoundExp, "TIMEDT.T_", "ln")
endif


*** START OF _DEBUG -- Stefan 
if !empty(lcROUNDEXP)
	do case 
	case used("jobhist") and type("jobhist.H_SCHEDID")!="U"
	*** do nothing 
	case used("qJobhist") and type("qJobhist.H_SCHEDID")!="U"
		lcROUNDEXP = strtran(lcROUNDEXP,"jobhist","qJobhist")
	case used("vJobhist") and type("vJobhist.H_SCHEDID")!="U"
		lcROUNDEXP = strtran(lcROUNDEXP,"jobhist","vJobhist")
	case used("JH333")
		**** Recalc from aplan level 
		*** The jobhist alias JH333 has no H_SCHEDID field
		*** so we need to get it.
		loBizJobhist = this.oBizmgr.GetBiz("JOBHIST")
		loCsrJobhist = loBizJobhist.GetCurrentJobhist( ;
				"/CURSOR=jobhist", "*", lnPersId, date())

	endcase 
endif 
*** END OF _DEBUG


do case
case left(lcAction,1) = "W" ;
 and !this.lRoundWithdrawals
	lcRoundExp = ""
case left(lcAction,1) = "D" ;
 and !this.lRoundDeposits
	lcRoundExp = ""
endcase

lcUnits = this.aUnitCd[val(pcBankid)]
lcBankid = pcBankid
lcPlanId = trim(evaluate(lcAplanAlias + ".AP_PLANID"))

*** Apply % multiplier from TBLC2 if appropriate
llUSEADJ = evaluate(lcAplanAlias + ;
				iif(lcAction="D", ".AP_USEADJD", ".AP_USEADJ"))
if llUSEADJ and lcTYPE<>"P"
	lcMULT = this.oBizTbl.GETOPT(lcOPT, "TBLC2")
	*** The only multipliers we apply are percentages.
	*** We don't want to apply constant rates, etc.
	if "%"$ lcMULT 
		lnMULT = val(lcMULT) / 100
		if lnMULT = 0
			lnMULT = 1
		endif
	else
		lnMULT = 1
	endif
else
	lnMULT = 1
endif

*** Convert units if required
lcTraceInfo = "Applying APLAN rounding rule while accumulating TIMEDT transactions" + ;
			";lcOpt=" + lcOpt + ;
			";lcUnits=" + lcUnits + ;
			";lcPlanid=" + lcPlanid

if lcUNITS = "$"
	** Bank is in dollars only.  Use T_AMT
	lnAMT = lnTAmt
	lcTraceInfo = lcTraceinfo + ";lnAmt (original)=" + lstr(lnAmt)
	lnAMT = evalx(lcROUNDEXP, lnAMT, lnAMT)
	lnAMT = lnAMT * lnMULT		&& Must do this after adjusting
	*-- 2011dec07 - Tom. Is this right?
	* lnDOLLARS = 0
	lnDollars = lnAmt
else
	** Bank is in hours (and maybe dollars)
	*-- Absence, hours worked, etc.
	*[2014-08-27 - TG] Ignore adjustment hours
	lnAMT = iif(llAdjust, 0, lnTHours)
	lcTraceInfo = lcTraceinfo + ";lnAmt (original)=" + lstr(lnAmt)

*!*	=strtofile("1 - lnPersId, lnAMT = " + transform(lnPersId) + ", "+ ;
*!*				transform(lnAMT)+chr(13)+chr(10), "C:\TEMP\cROUNDEXP.txt", .t.)

	lnAMT = evalx(lcROUNDEXP, lnAMT, lnAMT)

*!*	=strtofile("2 - lnPersId, lnAMT = "  + transform(lnPersId) + ", " + ;
*!*		 transform(lnAMT)+chr(13)+chr(10), "C:\TEMP\cROUNDEXP.txt", .t.)

	lnAMT = lnAMT * lnMULT		&& Must do this after rounding
	*-- Recalculate dollars after rounding and adjusting
	if ! "$" $ lcUNITS and !llDollars
		*** Bank has no dollars
		lnDOLLARS = 0
	else
		*** Transaction in hours
		if empty(lcROUNDEXP) or lnAMT = lnTAmt
			*** Use dollars as presented. Hours were unchanged.
*!*				lnDOLLARS = lnTAmt * lnMULT
			lnDOLLARS = lnTAmt
		else
			*** Recalculate dollars from new hours
			lcUnitrate = iif(llOld, oldval("T_UNITRATE", pcTimedtAlias), ;
						evaluate(pcTimedtAlias + ".T_UNITRATE"))
			lnDOLLARS = round(lcUNITRATE * lnAMT, 2)
		endif
	endif
	if inlist(left(lcUNITS,1), "D", "J")
		*** Convert to days
		lnHrsPdy = evl(hrspdy(lnPERSID, ldEffdt, .t.), C_HRSPDY)
		lnAMT = iif(lnHrsPdy=0, 0, round(lnAMT / lnHrsPdy, 4))
	endif
endif

*** Round to 4 decimals
lnAMT = round(lnAMT, 4)

store null to loBizJobhist, loCsrJobhist
return

*#########################################################
enddefine

