*  Program...........: ShowTimsumSummary
*  Author............: Tom Green
*  Project...........: Human Resources
*  Created...........: October 31, 2006 (Halloween!)
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies 2006
*  Description.......: Shows a window for display only of
*                    : time bank balances.  Called from
*                    : bhvJOBHIST and possibly timesheet
*                    : 
*                    : NOTE that there is no security on
*                    : scTIMSUM.  If you don't want the user
*                    : to see his banks, don't call this prog.
*                    : 
*  Calling Samples...: *** This permits ShowTimsumSummary()
*                    : *   to be overridden in CUSTOM.APP
*                    : lcCommand = ;
*                    :     [ShowTimsumSummary("/MODAL",]+;
*                    :     transform(this.Persid) + [)]
*                    : CustomExecute(lcCommand)
*                    : 
*                    : 
*  Parameter List....: 1. Switches:
*                    :    /MODAL : Show as modal form
*                    :    /TIMETMP : Include unposted time
*                    :        from TIMETMP even if arg.3
*                    :        is not passed.
*                    :    /PROMPT : Ask if should show TIMETMP.
*                    : 
*                    :    NOTE: /TIMETMP implies planned absences
*                    :    as well
*                    : 
*                    : 2. Persid
*                    : 
*                    : 3. TIMETMP alias, view, or cursor
*                    :    If this is passed, unposted time
*                    :    is added in.  TT_DEST and TT_HOURS
*                    :    must exist.  If not passed, only
*                    :    TIMSUM info is used.
*                    : 
*                    : Return: void
*                    : 
*                    : 
*  Status, ToDo......: 
*                    : 
*                    : 
*  Technical Notes...: Uses frmBrowse, with columns defined in
*                    : UIINFO key "TIMSUM_SUMMARY"
*                    : 
*                    : Typically there is only one unposted
*                    : time sheet at a time.  We assume this
*                    : is the case without validation when
*                    : Arg.3 is passed
*                    : 
*                    : There is also the danger that TIMETMP
*                    : includes transactions from different
*                    : years.  However, typically a year will
*                    : not be rolled over until the timesheet
*                    : period is closed.
*                    : 
*  Modification......: September 2014 - S.G.
*                    : The program has been modified to include 
*                    : bank projections for next year entitlement 
*                    : The entitlement hours are saved into TS_ACCRUE.
*                    : 
*  Modification......: October 2015 - S.G.
*                    : Majour changes in terms of display of bank
*                    : balances.
*                    : 
*                    : 
*                    : 
parameters pcSwitches, lnPersid, poTimeTmp

*** Get cursor or banks
local loCsrTimsum, llPrompt, lnReply, loCsrAPlan
local lnSelect, loRS
local lcCarryTxt, lcEarnTxt, lcTakenTxt, lcAccrueTxt, lcBalTxt
local lnBalMin, lnBalMax
local lcPlans, lcPlansIds, lcWhere
local loPERS, loSickBank, lcMOBSEHRCNT
local llDebug, lcMtoHM, lnMtoHM

private lcTimetmpAlias, lnHrsPdy, llModal, llTimetmp, llHasPlanned
private lcDaysAbbrev, lcHoursAbbrev, lcCurrency, lnPersid		&& Not local!
private loBizTimsum, loBizJobhist, ldPlannedFrom, loBizAPlan 

store "" to lcPlans, lcPlansIds, lcWhere, lcMtoHM
store null to loRS, loPERS, loSickBank
store 0 to lnMtoHM

llDebug = .f.								&& set to true to see the cursor 
lcMOBSEHRCNT = "MOBSEHR"				&& MOBSEB counts time taken in hours only
ldSEBEffDT = ctod("2015/10/29")		&& Effective date of new rules SEB 

pcSwitches = iif(vartype(pcSwitches)="C", upper(pcSwitches), "")
llModal = "/MODAL" $ pcSwitches
llTimetmp = "/TIMETMP" $ pcSwitches
llPrompt = "/PROMPT" $ pcSwitches
llHasPlanned = .f.

lnSelect = select()

if llPrompt
	if gcLang = "E"
		lnReply = alert( ;
				"Include unposted timesheet entries?", ;
				0, "\!\<Yes;\<No;\?\<Cancel")
	else
		lnReply = alert( ;
				"Inclure des entrées non-reportées de la feuille de temps ?", ;
				0, "\!\<Oui;\<Non;\?\<Annuler")
	endif
	if lnReply = 3
		return .f.
	endif
	llTimetmp = (lnReply = 1)
endif

waitwind()

loBizTimsum = GetBiz("TIMSUM")
loBizJobhist = GetBiz("JOBHIST")
loBizAplan = GetBiz("APLAN")
loBizSched = GetBiz("SCHED")

*** Get PERS and current JOBHIST info into qPERS77
loCsrJobhist = loBizJobhist.GetPersAndJobhistByPersid (;
					"/cursor=qPers77 /CURRENT", lnPersid)

*** Exclude all [VAR...] plan banks 
lcWhere = MakeFor("", "TS_STAT = 0", ;
					MkInlist("TS_PERSID", transform(lnPersId), "/SQL"), ;
					"LEFT(TS_PLANID,3)<> 'VAR'", lcWhere)

*** Load time bank balances into qTimsum77
loCsrTimsum = loBizTimsum.GetList("/cursor=qTimsum77", ;
					"", lcWhere, "TS_BANKID")

if isnull(loCsrTimsum)
	return 
endif 

select qTimsum77
*** Load APLAN records into qAPlan 
lcPlans = concat("trim(TS_PLANID)+' '")
lcPlansIds = union(lcPlans)
lcWhere = MkInlist("AP_PLANID", lcPlansIds, "/SQL")

loCsrAPlan = goDataMgr.GetCursor( ;
			"/CURSOR=qAPlan", ;
			set("datas*ession"), ;
			"APLAN", ;
			"*", lcWhere)

select qAPlan  
index on AP_PLANID tag AP_PLANID


*** Add planned and display fields
select qTimsum77
alter table qTimsum77 ;
		add BankName C(30) ;
		add PlanName C(30) ;
		add CarryTxt C(40) ;
		add EarnTxt C(40) ;
		add TakenTxt C(40) ;
		add YearTxt C(40) ;
		add BalTxt C(40) ;
		add AccrueTxt C(40) ;
		add CYEntitle N(10,2) ;
		add YTDBalTxt C(40) ;
		add AccBalTxt C(40) ;
		add TS_PLAN N(10,2) ;
		add TS_PLANOK N(10,2) ;
		add TS_PLANPND N(10,2) ;
		add PlanTxt C(40) ;
		add PlanOKTxt C(40) ;
		add PlanPndTxt C(40) ;
		add AvailTxt C(40) ;
		add BalStat N(2) ;
		add PrvPeriod C(2) ;
		add PeriodId C(2) ;
		add cPeriod C(20) ;
		add StartDT C(12) ;
		add EndDT C(12) ;
		add SnNPCYTP N(10,2) ;
		add cSnNPCYTP C(45) ;
		add SnPtCYTP N(10,2) ;
		add cSnPtCYTP C(45) ;
		add BFwdLPTP N(10,2) ;
		add cBFwdLPTP C(45) ;
		add FlexCPTD N(10,2) ;
		add cFlexCPTD C(45) ;
		add SdWkCPTD N(10,2) ;
		add ReWkCPTD N(10,2) ;
		add cWkCPTD C(45) ;
		add WkDysCPTD N(10,2) ;
		add YTDBAL N(10,2) ;
		add cYTDBAL C(55) ;
		add OTCYTD N(10,2) ;
		add cOTCYTD C(55) ;
		add SebSCYTD N(10,2) ;
		add cSebSCYTD C(55) ;
		add cMALSEB C(65) ;
		add cMOBSEB C(65) ;
		add nBalMax N(10,2) ;
		add nMOBSEHR N(10,2) ;
		add nHRSPDY	N(10,2) ;
		add nMtoHM N(10,2) ;
		add Drill1 M ;
		add Drill2 M ;
		add Drill3 M 


index on TS_BANKID tag TS_BANKID

*** Add in unposted time from time sheet
lcTimetmpAlias = GetAlias(poTimetmp)
if !empty(lcTimetmpAlias) or llTimetmp
	loBizTimsum.AccumulateTimetmp("", lnPersid, ;
				lcTimetmpAlias, "qTimsum77")
endif

lcDaysAbbrev = goMsgMgr.GetText("UI.COMMON.UNITS.DAYS_SHORT")
lcHoursAbbrev = goMsgMgr.GetText("UI.COMMON.UNITS.HOURS_SHORT")
lcCurrency = goMsgMgr.GetText("UI.COMMON.CURRENCY_SYMBOL")

*** set step on 
*** Buggy 
lnHrsPdy2 = hrspdy(lnPERSID, date(), .t.)
if !isnull(loBizSched)
	*** Get the average working HOURS/DAY 
	lnHRSPDY = loBizSched.WkCal("AVHRSPDY", ;
				qPers77.H_SCHEDID, date())

	*** Transform in 60 of minutes 
	lcMtoHM = MtoHM(HMtoM(HtoHM(lnHRSPDY, -1)))
	lcMtoHM = chrtran(lcMtoHM, ":", ".")
	lnMtoHM = val(lcMtoHM)
endif 

*!*	wait window "lnHrsPdy2 = " + transform(lnHrsPdy2) + chr(13) + ;
*!*					",  lnHRSPDY = " + transform(lnHRSPDY)

*** Show planned absences starting on this date
ldPlannedFrom = monday(date(), C_WEEKSTRT) - 7

*** set step on 
*** In this program. Creates relation qTimsum77 -> qPlan77
if llTimetmp
	if ShowTimsumSummary_GetPlannedAbs()
		locate for TS_PLAN <> 0
		llHasPlanned = found()
	endif
endif

*** Create the base object
loRS = CreateTBankObject()
loRS.nPersId = nvl(lnPersid,0)

*** set step on 
*** Convert numeric to text
scan
	replace ;
			BankName with tbleval("TIMEBANK", TS_BANKID), ;
			PlanName with tbleval("APLAN", TS_PLANID), ;
			CYEntitle with val(tbleval("_APLAN", TS_PLANID, "TBLC8"))

	replace nHRSPDY	with lnHRSPDY
	replace nMtoHM		with lnMtoHM
			
	replace CarryTxt with ConvertTimsumUnits( ;
				"", TS_Units, TS_Carry, TS_CarryD, lnHrsPdy)

	replace EarnTxt with ConvertTimsumUnits( ;
				"", TS_Units, TS_Earn, TS_EarnD, lnHrsPdy)

	if inlist(TS_BANKID, "1", "3", "5")
		replace TakenTxt with str((TS_Taken/lnHRSPDY),10,2) + ;
					+ space(3) + "jour(s)"
	else
		replace TakenTxt with ConvertTimsumUnits( ;
					"", TS_Units, TS_Taken, TS_TakenD, lnHrsPdy)
	endif
					
	*** Estimates next year entitlement 
	if inlist(TS_BANKID, "3", "4")
		replace TS_Accrue with ;
				GetVacationEntitlement(loBizTimsum, TS_PLANID)
	endif 
	
	replace AccrueTxt with ConvertTimsumUnits( ;
				"/Z", TS_Units, TS_Accrue, TS_AccrueD, lnHrsPdy)

	if inlist(TS_BANKID, "1", "3", "5")
		replace BalTxt with str(((TS_Carry+TS_Earn-TS_Taken)/lnHRSPDY),10,2) + ;
				+ space(3) + "jour(s)"
	else 
		replace BalTxt with ConvertTimsumUnits( ;
					"", TS_Units, ;
					TS_Carry + TS_Earn - TS_Taken, ;
					TS_CarryD + TS_EarnD - TS_TakenD,;
					lnHrsPdy)
	endif 				

	*** Planned absences. Approved and Pending
	replace PlanTxt with ConvertTimsumUnits( ;
				"", "H", ;
				TS_Plan, 0, lnHrsPdy)
	replace PlanOKTxt with ConvertTimsumUnits( ;
				"", "H", ;
				TS_PlanOK, 0, lnHrsPdy)
	replace PlanPndTxt with ConvertTimsumUnits( ;
				"", "H", ;
				TS_PlanPnd, 0, lnHrsPdy)

	if TS_UNITS <> "$"
		replace AvailTxt with ConvertTimsumUnits( ;
				"", chrtran(TS_Units, "$", ""), ;
				TS_Carry + TS_Earn - TS_Taken - TS_Plan, ;
				0, lnHrsPdy)
	endif

	*** These are not displayed in default version, but
	*   are available for custom version.
	replace YearTxt with ConvertTimsumUnits( ;
				"/Z", TS_Units, ;
				TS_Earn - TS_Taken, ;
				TS_EarnD - TS_TakenD,;
				lnHrsPdy)

	replace AccBalTxt with ConvertTimsumUnits( ;
				"/Z", TS_Units, ;
				TS_Carry + TS_Accrue + TS_Earn - TS_Taken, ;
				TS_CarryD + TS_AccrueD + TS_EarnD - TS_TakenD,;
				lnHrsPdy)

	if inlist(TS_BANKID, "1", "3", "5")
		replace YTDBalTxt with str((CYEntitle-(TS_Taken/lnHRSPDY)),10,2)+;
					+ space(3) + "jour(s)"
	else
		replace YTDBalTxt with ConvertTimsumUnits( ;
					"/Z", TS_Units, ;
					(CYEntitle-(TS_Taken/lnHrsPdy))*lnHrsPdy, ;
					0, ;
					lnHrsPdy)	
	endif 				

	*** Check limits
	loLimits = loBizTImsum.GetLimits("", lnPersid, ;
					TS_Bankid, TS_ENDDT, ;
					"qTimsum77")

	lnBalMin = nvl(loLimits.nBalMin,-99999999) - .01
	lnBalMax = nvl(loLimits.nBalMax, 99999999) + .01
	replace nBalMax	with nvl(loLimits.nBalMax,0)

	replace BalStat with icase ( ;
				TS_CARRY+TS_EARN-TS_TAKEN < lnBalMin, -1, ;
				TS_CARRY+TS_EARN-TS_TAKEN > lnBalMax, 1, 0) 

	*** For some reason, slashes don't work. So switches are
	*   done with $-signes instead
	if TS_CARRY<>0 or TS_EARN<>0 or TS_TAKEN<>0
		if llTimetmp
			replace Drill1 with [<a href="show_timsum_trans('$TIMETMP',] + ;
					lstr(lnPersid) + ",'" + TS_BANKID + [')">]
		else
			replace Drill1 with [<a href="show_timsum_trans('',] + ;
					lstr(lnPersid) + ",'" + TS_BANKID + [')">]
		endif
	endif

	*** The is no transaction for this in timeDT 
	if TS_ACCRUE <> 0
		replace Drill2 with [<a href=\"show_timsum_trans('$ACCRUE',] + ;
			lstr(lnPersid) + ",'" + TS_BANKID + [')">]
	endif

	replace Drill3 with [<a href="show_timsum_trans('$PLAN',] + ;
			lstr(lnPersid) + ",'" + TS_BANKID + [')">]

endscan


*** set step on 
try 
	select qTimsum77
	locate for trim(TS_BANKID) = "9" and !eof()
	if !found()
		=GetTBSFPQPeriods( @loRS )
	else 
		scatter name loTBank
		=GetTBRunningTotal(lnHrsPdy, loTBank, @loRS)
		if !isnull(loRS)

			*** Different periods formats 
			replace PeriodId 		with transform(loRS.nPeriodId, "99")
			if loRS.nPeriodId > 1 
				replace PrvPeriod		with transform((loRS.nPeriodId-1), "99")
			endif 	
			replace cPeriod 		with str(year(date()), 4) + " - " + ;
							transform(loRS.nPeriodId, "99")
			
			*** Periods dates 
			replace StartDT 		with dtoc(loRS.dStartDT)
			replace EndDT 			with dtoc(loRS.dEndDT)
			
			*** Signed and Posted CYTP (Current Year To Period) 
			replace SnPtCYTP		with loRS.nSnPtCYTP

			*** Signed and NOT Posted CYTP (Current Year To Period) 
			replace SnNPCYTP 		with loRS.nSnNPCYTP

			*** FLEX bank CPTD (Current Period To Date)
			replace FlexCPTD 		with loRS.nFlexCPTD

			*** Carry Farward Balance (Last Period To Period)  
			replace BFwdLPTP 		with (loRS.nSnPtCYTP+loRS.nSnNPCYTP)

			*** Schedules to Work versus Real time worked
			*** CPTD (Current Period To Date)
			replace SdWkCPTD		with round(loRS.nSdWkCPTD, 2)
			replace ReWkCPTD		with round(loRS.nReWkCPTD, 2)
			replace cWkCPTD	 	with transform((loRS.nSdWkCPTD-loRS.nReWkCPTD), "9999999.99")
			

			*** Number of days 
			replace WkDysCPTD		with round(loRS.nWkDysCPTD, 2)

			*** Monthly variance of hours worked 
			*** CPTD (Current Period To Date)
			*** replace YTDBAL 		with ;
			***		round((BFwdLPTP+FlexCPTD-(loRS.nSdWkCPTD-loRS.nReWkCPTD)),2)
			replace YTDBAL 		with ;
					round((BFwdLPTP-FlexCPTD+(loRS.nReWkCPTD-loRS.nSdWkCPTD)),2)

			*** Signed and Posted CYTP (Current Year To Period) 
			replace cSnPtCYTP 	with ConvertTimsumUnits( ;
												"", TS_Units, SnPtCYTP, TS_EarnD, lnHrsPdy)

			*** Signed and NOT Posted CYTP (Current Year To Period) 
			replace cSnNPCYTP 	with ConvertTimsumUnits( ;
												"", TS_Units, SnNPCYTP, TS_EarnD, lnHrsPdy)

			*** Monthly variance CPTD (Current Period To Date)
			replace cFlexCPTD	with ConvertTimsumUnits( ;
												"", TS_Units, FlexCPTD, TS_EarnD, lnHrsPdy)

			*** Carry Farward Balance (Last Period To Period)  
			replace cBFwdLPTP		with ConvertTimsumUnits( ;
												"", TS_Units, BFwdLPTP, ;
												TS_EarnD, lnHrsPdy)

			*** Monthly variance of hours worked 
			*** CPTD (Current Period To Date)
			replace cYTDBAL	with ConvertTimsumUnits( ;
												"", TS_Units, YTDBAL, TS_EarnD, lnHrsPdy)

		endif 
	endif

	select qTimsum77
	go top in qTimsum77
	scan 
		*** Mobile
		if "5"$TS_BANKID
			*** Get the number of times some time off has been 
			* taken from both tables TIMEDT & TIMETMP based 
			* on the counter [MOBSEHR]. The hours taken 
			* as 1/2 days are already in TS_TAKEN per year.
			loRS.dStartDT = TS_EFFDT
			loRS.dEndDT = TS_ENDDT
			if ldSEBEffDT > loRS.dStartDT
				loRS.dStartDT = ldSEBEffDT
			endif 	
			loRS.nMOBSEHR =GetTimeOffTakenByPersId(lcMOBSEHRCNT, @loRS)

			replace nMOBSEHR	with loRS.nMOBSEHR
			replace cMOBSEB	with 	"Nombre de congés pris en heure(s) : " + ;
											lstr(nMOBSEHR,2)
		endif 

		if "6"$TS_BANKID
			*** Get the number of hours from both table TIMEDT & TIMETMP 
			* based on the counter [TSSEB]
			loRS.nOTCYTD =GetHRSWKByPersId(TS_PLANID, @loRS)

			replace OTCYTD		with loRS.nOTCYTD
			replace cOTCYTD	with "Maximum [ "+lstr(nBalMax,2)+ " ] hres"
		endif 
		
		if "8"$TS_BANKID
			*** Get the number of hours from both table TIMEDT & TIMETMP 
			* based on the counter [SEBSYN] for all employees.
			* This is a TB for all employees.
			loRS.dStartDT = TS_EFFDT
			loRS.dEndDT = TS_ENDDT
			loRS.nSebSynCYTD =GetHRSWKAllPersId(TS_PLANID, @loRS)

			replace SebSCYTD		with loRS.nSebSynCYTD
			replace cSebSCYTD		with 	"Solde actuel / maximum - " + ;
												"[ " + lstr(SebSCYTD,2) + " / " +;
												lstr(nBalMax,2) + " ] hres"
		endif
	endscan 
	store null to loRS, loTBank

catch to loERROR 

	alert("Exception: " + transform(loERROR.Message), ;
					0, "\!\?\<OK")
endtry 

*** DEBUG 
if llDebug 
	select qTimsum77
	go top in qTimsum77
	copy to \TEMP\qTimsum77
	go top in qTimsum77
endif 

*** set step on 
waitwind("","/CLEAR")

*** Show the form
do ShowForm

*** Clean up
use in select("qTimsum77")
select (lnSelect)

return

*=========================================================
procedure ShowTimsumSummary_GetPlannedAbs
***
*
local loCsrPlanned, loBizPayno, ldPayPeriodEnds
local ldPlannedFrom, loBizEvent, lcBankid, lnBankid
local lnMinutes, lnHours, lnQty, ldYearEnd, lcDest

loBizPayno = GetBiz("PAYNO")
loBizEvent = GetBiz("EVENT")

ldPayPeriodEnds = loBizPayno.GetCurrentPayno(;
						qPers77.H_PAYGRP, date(), "PN_ENDDT")
ldPayPeriodEnds = evl(ldPayPeriodEnds, date())
ldPlannedFrom = AddDays(ldPayPeriodEnds,1)

*** Since we are getting records for all banks, we
*   can't specify the bank end date since it might vary.
*   All we know is it must be in the current year.
*   We will remove future years below.

**  Feb 24, 2012 - TCG. I restricted it to EVENT only because
*   records in TIMEDT are alsready in the balance -- I THINK
*   /// TODO - Check above assumption ////
loCsrPlanned = loBizTimsum.GetPlannedAbs(;
				"/cursor /event " + iif(llTimetmp, "/EXCLTIMETMP", ""), ;
				lnPersid, "", ldplannedFrom, ;
				gomonth(date(),13))

if isnull(loCsrPlanned)
	*** Some sort of error. Can't get planned absences
	select qTimsum77
	return .f.
endif

scan
	lcBankid = V_BANKID
	if empty(lcBankid)
		*** I don't think this should be possible
		suspendif(".t.")
		loop
	endif

	if !seek(lcBankid, "qTimsum77", "TS_BANKID")
		*** Has transaction but not bank!
		loop
	endif

	lnBankid = val(lcBankid)
	ldYearEnd = todate(qTimsum77.TS_ENDDT)
	lnMinutes = V_HOURS * 60
	if substr(V_DEST,2) = "D"
		*** Deposited, change sign.  Unlikely.
		lnMinutes = - Minutes
	endif
	lnHours = round(lnMinutes / 60, 2)

	*** Accumulate into qTimsum77
	if V_PLAN = "A"
		*** Awaiting approval (pending)
		replace in qTimsum77 ;
				TS_PLANPND with TS_PLANPND + lnHours
	else
		*** Approved
		replace in qTimsum77 ;
				TS_PLANOK with TS_PLANOK + lnHours
	endif	
	replace in qTimsum77 ;
				TS_PLAN with TS_PLAN + lnHours
endscan

select qTimsum77
return

*=========================================================
procedure ConvertTimsumUnits (pcSwitches, pcUnits, ;
					pnQty, pnDollars, pnHrsPdy)
local lcResult, llBlankIfZero, lnQty
llBlankIfZero = "/Z" $ pcSwitches

lcResult = ""
pnQty = round(pnQty,2)

do case
case pnQty = 0
	if llBlankIfZero
		lcResult = ""
	else
		lcResult = iif(gcLang="E", "None", "Rien")
	endif

case "H" $ pcUnits
	*** Hours
	lcResult = ShowHrs(pnQty)
	if !empty(pnHrsPdy)
		lcResult = lcResult + " (Estimé " + ;
				lstr(pnQty/pnHrsPdy) + " jours)"
	endif

case "J" $ pcUnits or "D" $ pcUnits
	*** Days
	lcResult = lstr(pnQty) + " " + lcDaysAbbrev
	if !empty(pnHrsPdy)
		lcResult = lcResult + " (" + ;
		ShowHrs(pnQty * pnHrsPdy) + ")"
	endif

case pcUnits = "$"
	*** Dollars only. Use pnQTY argument
	lcResult = iif(empty(lcResult), "", ;
					lcResult + ", ") + ;
					lcCurrency + ltrim(str(pnQty,12,2))
endcase

if "$" $ pcUnits and pcUnits <> "$"
	*** Dollars AND days or hours
	if llBlankIfZero and pnDollars = 0 ;
	and (pcUnits = "$" or pnQty = 0)
		*** Dollars = 0 and nothing else in field
	else
		lcResult = iif(empty(lcResult), "", ;
					lcResult + ", ") + ;
					lcCurrency + ltrim(str(pnDollars,12,2))
	endif
endif

return lcResult

*=========================================================
procedure ShowHrs(pnHrs)

local lcHHMM

do case
case abs(pnHrs) = 1
	*** Exactly one hour or -1 hour. Use singular form
	return transform(pnHrs) + " " + ;
			 left(lcHoursAbbrev, len(lcHoursAbbrev)-1)
case int(pnHrs) = pnHrs
	*** Whole hours
	return transform(pnHrs) + " " + lcHoursAbbrev
case abs(pnHrs) < 1
	*** Minutes only
	return transform(round(pnHrs * 60,0)) + " min."
otherwise
	*** Show in hours and minutes
	return iif(C_TDHHMM, ;
		HtoHM(pnHrs, -1, "/DELIM"), lstr(pnHrs)) + ;
		" " + lcHoursAbbrev
endcase

*=========================================================
procedure ShowForm

local lcHTML

*** Prepare HTML
lcHTML = HtmlMerge("/FILE", "TIMSUM_SUMMARY_@.HTM", "qTimsum77")

*** Show the form
if llModal
	*-- Modal version.  Can't use drill-down
	StartHTMLForm( ;
			"/MODAL /ID=TIMSUM_SUMMARY /SAVEPOSITION", ;
			iif(gcLang="E", "Bank Balances", "Soldes des Banques") + ;
			" - " + name(lnPersid), ;
			lcHTML, null, lnPersid)
else
	*-- Non-modal version allows drill-down
	StartHTMLForm( ;
			"/ID=TIMSUM_SUMMARY /ALLOWMULTIPLE " + ;
			"/HEIGHT=550 /WIDTH=530 /SAVEPOSITION" + ;
			"/SUBTYPE=" + lstr(lnPersid), ;
			iif(gcLang="E", "Bank Balances", "Soldes des Banques") + ;
			" - " + name(lnPersid), ;
			lcHTML, null, lnPersid)
endif

return


*=========================================================
procedure GetTBSFPQPeriods( toRS )
*** This method sets the SFPQ periods based on the 
* _FLEXIBLE tbl table.
*
local lnSelect 
local loBizTbl, loCursor 
local lnPeriodId, ldStartDT, ldEndDT

store null to loCursor, loBizTbl
store {} to ldStartDT, ldEndDT
store 0 to lnPeriodId

lnSelect = select()

*** Get SFPQ beging and the end of the pay period.
loBizTbl = GetBiz("TBL")
loCursor = loBizTbl.GetList("/CURSOR=qPeriod", "_FLEXIBLE", ;
				"TBLSORT AS PeriodId, " + ;	
				"rtrim(TBLC1) As TBLC1,rtrim(TBLC2) As TBLC2")

if isnull(loCursor)
	select( lnSelect )
	return 
endif 

select qPeriod
go top in qPeriod
scan
	if between(dtos(date()), trim(TBLC1), trim(TBLC2))

		lnPeriodId= val(PeriodId)
		ldStartDT = ctod(left(trim(TBLC1),4) + "/" + ;
						substr(trim(TBLC1),5,2) + "/" + ;
						substr(trim(TBLC1),7,2))
							
		ldEndDT = ctod(left(trim(TBLC2),4) + "/" + ;
						substr(trim(TBLC2),5,2) + "/" + ;
						substr(trim(TBLC2),7,2))
		
		exit
	endif 	 
endscan 
use in select("qPeriod")
store null to loCursor, loBizTbl

if empty(ldStartDT)
	ldStartDT = date()
endif 	
if empty(ldEndDT)
	ldEndDT = date()
endif 

toRS.nPeriodId = lnPeriodId
toRS.dStartDT = ldStartDT 
toRS.dEndDT = ldEndDT 

select ( lnSelect )
return 

*=========================================================
procedure GetTBRunningTotal(tnHrsPdy, toTBank, loRS )
*** This method gets the running total (DEPOSIT&WITHDRAWLS)
* for the FLEX bank plan.
* The method also calculates the MTD hours worked by SFPQ
* period for a given employe.
* RETURN: an object with all the calculated values 
*
local lnSelect, lcSql, loR, loCursor, lnPersId 
local ldStartDT, ldEndDT, lnSchedHoliHours
local lcDEPOSIT, lcWITHDRAWAL, lnWkScheduleHRS
local loBizJobhist, loBizTbl
local lcPlanId, ldPlanStartDT
local lnPeriodId

store "" to lcSql, lcDEPOSIT, lcWITHDRAWAL
store {} to ldStartDT, ldEndDT
store null to loR, loBizJobhist, loBizTbl
store 0 to lnWkScheduleHRS, lnSchedHoliHours
store 0 to lnPeriodId
store null to loCursor

*** set step on 
if "9"$toTBank.TS_BANKID
	lcDEPOSIT = "DEP9"
	lcWITHDRAWAL = "CFLEXI"
endif 

if isnull(loRS)
	return 
endif 	

lnPersId = loRS.nPersId 
if empty(lnPersId)
	return 
endif 

if isnull(toTBank)
	return 
endif 

lnSelect = select()

*** Get SFPQ custom periods 
=GetTBSFPQPeriods( @loRS )

ldStartDT = loRS.dStartDT
ldEndDT = loRS.dEndDT
if empty(ldEndDT)
	ldEndDT =date()
endif 	

*** Signed & Posted Current Year To Period (CYTP) 
*** ( SnPtCYTP ) object property 
text to lcSql textmerge noshow pretext 1+4
	--Signed & Posted Current Year To Period (CYTP) 
	select a.T_PERSID, a.T_OPT, a.T_EFFDT, a.T_HOURS,
		( SELECT [T_HOURS] = SUM(b.T_HOURS)
			FROM (
				SELECT T_PERSID, T_OPT, T_EFFDT, 
					[T_HOURS] = CASE WHEN T_OPT = '[cWITHDRAWAL]' 
						THEN T_HOURS * (-1) ELSE T_HOURS END
				FROM TimeDT 
				WHERE T_PERSID = [nPERSID] 
				AND T_OPT IN ( '[cWITHDRAWAL]', '[cDEPOSIT]' )
				AND ( T_EFFDT BETWEEN '[dSTARTDT]' AND '[dENDDT]' )
			) b
				WHERE T_PERSID = [nPERSID] 
				AND b.T_OPT IN ('[cWITHDRAWAL]', '[cDEPOSIT]')
				AND b.T_EFFDT <= a.T_EFFDT 
			) As RTotal 
	from timedt a
		WHERE a.T_PERSID = [nPERSID] 
		AND  a.T_OPT in ('[cWITHDRAWAL]', '[cDEPOSIT]')
		AND ( a.T_EFFDT BETWEEN '[dSTARTDT]' AND '[dENDDT]' )
	order by a.T_PERSID, a.T_EFFDT
endtext 

ldPlanStartDT = toTBank.TS_EFFDT
lcSql = strtran(lcSql, "[nPERSID]", transform(lnPersId, "999999"))
lcSql = strtran(lcSql, "[cWITHDRAWAL]", alltrim(lcWITHDRAWAL))
lcSql = strtran(lcSql, "[cDEPOSIT]", alltrim(lcDEPOSIT))
lcSql = strtran(lcSql, "[dSTARTDT]", dtoc(ldPlanStartDT))
lcSql = strtran(lcSql, "[dENDDT]", dtoc(ldStartDT-1))

store null to loCursor
loCursor = goDataMgr.SQLexec("/CURSOR=qTimeDT", set("datasession"), ;
						goDataMgr.ConnectionHandle, lcSql)

if isnull(loCursor)
	return 
endif 	

select qTimeDT
go bottom in qTimeDT 

*** Signed & Posted Current Year To Period (CYTP) 
*** ( SnPtCYTP ) object property 
loRS.nSnPtCYTP = nvl(toTBank.TS_CARRY,0)+nvl(qTimeDT.RTotal, 0)

*** Signed & NOT posted Current Year To Period (CYTP) 
*** ( SnNPCYTP ) object property 
text to lcSql textmerge noshow pretext 1+4
	--Signed & NOT posted Current Year To Period (CYTP) 
	select a.TT_PERSID, a.TT_OPT, a.TT_EFFDT, a.TT_HOURS,
		( SELECT [TT_HOURS] = SUM(b.TT_HOURS)
			FROM (
				SELECT TT_PERSID, TT_OPT, TT_EFFDT, 
					[TT_HOURS] = CASE WHEN TT_OPT = '[cWITHDRAWAL]'
						THEN TT_HOURS * (-1) ELSE TT_HOURS END
				FROM TimeTmp 
				WHERE TT_PERSID = [nPERSID]
				AND TT_OPT IN ( '[cWITHDRAWAL]', '[cDEPOSIT]' )
				AND ( TT_EFFDT BETWEEN '[dSTARTDT]' AND '[dENDDT]' )
					AND (TT_SIGNBY <> '' AND TT_POSTBY = '')
			) b
				WHERE TT_PERSID = [nPERSID]
				AND b.TT_OPT IN ( '[cWITHDRAWAL]', '[cDEPOSIT]' )
				AND b.TT_EFFDT <= a.TT_EFFDT 
			) As RTotal 
	from TimeTmp a
		WHERE a.TT_PERSID = [nPERSID]
		AND  a.TT_OPT in ( '[cWITHDRAWAL]', '[cDEPOSIT]' )
		AND ( a.TT_EFFDT BETWEEN '[dSTARTDT]' AND '[dENDDT]' )
			AND (TT_SIGNBY <> '' AND TT_POSTBY = '')
	order by a.TT_PERSID, a.TT_EFFDT
endtext 
ldPlanStartDT = toTBank.TS_EFFDT
lcSql = strtran(lcSql, "[nPERSID]", transform(lnPersId, "999999"))
lcSql = strtran(lcSql, "[cWITHDRAWAL]", alltrim(lcWITHDRAWAL))
lcSql = strtran(lcSql, "[cDEPOSIT]", alltrim(lcDEPOSIT))
lcSql = strtran(lcSql, "[dSTARTDT]", dtoc(ldPlanStartDT))
lcSql = strtran(lcSql, "[dENDDT]", dtoc(ldStartDT-1))

store null to loCursor
loCursor = goDataMgr.SQLexec("/CURSOR=qTmTmp", set("datasession"), ;
						goDataMgr.ConnectionHandle, lcSql)

if isnull(loCursor)
	return 
endif 	

*** Signed & NOT posted Current Year To Period (CYTP) 
*** ( SnNPCYTP ) object property 
if reccount("qTmTmp") > 0 
	go bottom in qTmTmp
	loRS.nSnNPCYTP = nvl(qTmTmp.RTotal, 0)
endif 	


*** Monthly variance of FLEX time bank
*** CPTD (Current Period To Date)
text to lcSql textmerge noshow pretext 1+4
	--Monthly variance of FLEX time bank
	select a.TT_PERSID, a.TT_OPT, a.TT_EFFDT, a.TT_HOURS,
		( SELECT [TT_HOURS] = SUM(b.TT_HOURS)
			FROM (
				SELECT TT_PERSID, TT_OPT, TT_EFFDT, 
					[TT_HOURS] = CASE WHEN TT_OPT = '[cWITHDRAWAL]'
						THEN TT_HOURS * (-1) ELSE TT_HOURS END
				FROM TimeTmp 
				WHERE TT_PERSID = [nPERSID]
				AND TT_OPT IN ( '[cWITHDRAWAL]' )
				AND ( TT_EFFDT BETWEEN '[dSTARTDT]' AND '[dENDDT]' )
				AND TT_POSTBY = ''
			) b
				WHERE TT_PERSID = [nPERSID]
				AND b.TT_OPT IN ( '[cWITHDRAWAL]' )
				AND b.TT_EFFDT <= a.TT_EFFDT 
			) As RTotal 
	from TimeTmp a
		WHERE a.TT_PERSID = [nPERSID]
		AND  a.TT_OPT in ( '[cWITHDRAWAL]' )
		AND ( a.TT_EFFDT BETWEEN '[dSTARTDT]' AND '[dENDDT]' )
		AND TT_POSTBY = ''
	order by a.TT_PERSID, a.TT_EFFDT
endtext 
lcSql = strtran(lcSql, "[nPERSID]", transform(lnPersId, "999999"))
lcSql = strtran(lcSql, "[cWITHDRAWAL]", alltrim(lcWITHDRAWAL))
lcSql = strtran(lcSql, "[dSTARTDT]", dtoc(ldStartDT))
lcSql = strtran(lcSql, "[dENDDT]", dtoc(ldEndDT))

store null to loCursor
loCursor = goDataMgr.SQLexec("/CURSOR=qTTmp", set("datasession"), ;
						goDataMgr.ConnectionHandle, lcSql)

if isnull(loCursor)
	return 
endif 	

*** Monthly variance of FLEX time bank
*** CPTD (Current Period To Date)
if reccount("qTTmp") > 0 
	go bottom in qTTmp
	loRS.nFlexCPTD = nvl(qTTmp.RTotal, 0)
endif 	


*** Get the number of hours that needs to work based 
* on its schedule for (LPTP)
loBizJobhist = GetBiz("JOBHIST")
lnWkScheduleHRS = loBizJobhist.GetTotalHours( ;
				"/HOURS/INCLINAC/INCLLTD/INCLALD", ;
				lnPersId, ldStartDT, ldEndDT)

lnSchedHoliHours = loBizJobhist.GetTotalHours( ;
		"/FHOURS/INCLINAC/INCLLTD/INCLALD", ;
		lnPersId, ldStartDT, ldEndDT)	

loRS.nSdWkCPTD = round(lnWkScheduleHRS + lnSchedHoliHours, 2)

*** Get the number of hours from both table TIMEDT & TIMETMP 
*** for the entire period
** loRS.dStartDT = ldStartDT 
** loRS.dEndDT = ldEndDT
lcPlanId = alltrim(toTBank.TS_PLANID)
loRS.nReWkCPTD =GetHRSWKByPersId(lcPlanId, @loRS)

*** How many days bases on the schedule
if tnHrsPdy != 0 
	loRS.nWkDysCPTD = round(loRS.nSdWkCPTD / tnHrsPdy, 2)
endif 	

use in select("qPeriod")
use in select("qTimeDT")
use in select("qTmTmp")
use in select("qTTmp")

store null to loCursor
select ( lnSelect )

return 
endproc

*=========================================================
procedure GetHRSWKByPersId(tcCounter, loRS)
*** This method uses the counter "FLEX" to get to total  
* number of hours for posted timesheet transactions [TIMEDT]
* and unposted T/S transactions [TIMETMP].
*
* The counter "FLEX" need to exist.
*
local lnSelect, lcEXPR, lcSql, loCursor
local lnTotHOURS, lnTimeDT_HRS, lnTimeTMP_HRS 

store 0 to lnTotHOURS, lnTimeDT_HRS, lnTimeTMP_HRS
store "" to lcEXPR, lcSql
store null to loCursor

if empty(tcCounter) or isnull(loRS)
	return lnTotHOURS
endif 

tnPersId  = loRS.nPersId
tdStartDT = loRS.dStartDT
tdEndDT   = loRS.dEndDT

lnSelect = select()

*** Get posted timesheet transactions table [TIMEDT]
lnTimeDT_HRS = TCNTVAL("TOT", tcCounter, tnPersId, tdStartDT, tdEndDT)

*** Get unposted timesheet transactions table [TIMETMP]
*** Strip counter to use with timetmp table   
lcEXPR = TCNTVAL("EXPR", tcCounter)
if empty(lcEXPR)
	select (lnSelect )
	return lnTotHOURS
endif 	
lcEXPR = strtran(lcEXPR, "and T_UNITCD='H'","")
lcEXPR = strtran(lcEXPR, "T_","TT_")
lcEXPR = strtran(lcEXPR, "not inlist(TT_OPT,", "TT_OPT not in (")
lcEXPR = strtran(lcEXPR, "inlist(TT_OPT,", "TT_OPT in (")

text to lcSql textmerge noshow pretext 1+4
	select ISNULL(sum( (case when TT_UNITCD='H' and TT_TYPE<>'P' 
					then TT_HOURS else 00000.00 end)), 0) as TOTHRS, 
			CAST(0 as NUMERIC(12,2)) as TOTAMT 
	from TIMETMP
	WHERE TT_PERSID = [nPERSID] 
	AND ( TT_EFFDT BETWEEN '[dSTARTDT]' AND '[dENDDT]' )
	AND TT_POSTBY = ''
	AND [TIMETMP_CNT]
endtext 
lcSql = strtran(lcSql, "[nPERSID]", transform(tnPersId, "999999"))
lcSql = strtran(lcSql, "[dSTARTDT]", dtoc(tdStartDT))
lcSql = strtran(lcSql, "[dENDDT]", dtoc(tdEndDT))
lcSql = strtran(lcSql, "[TIMETMP_CNT]", alltrim(lcEXPR))

loCursor = goDataMgr.SQLexec("/CURSOR=qTTDT", set("datasession"), ;
						goDataMgr.ConnectionHandle, lcSql)

if isnull(loCursor)
	return lnTotHOURS
endif 

lnTimeTMP_HRS = nvl(qTTDT.TOTHRS, 0)

use in select("qTTDT")

if (tdEndDT - date()) = 0
	lnTotHOURS = lnTimeTMP_HRS
else
	lnTotHOURS = lnTimeDT_HRS + lnTimeTMP_HRS
endif 

store null to loCursor 
select( lnSelect )

return lnTotHOURS

*=========================================================
procedure GetHRSWKAllPersId(tcCounter, loRS)
*** This method uses the counter "SYNSEB" to get to total  
* number of hours worked from [TIMEDT] and [TIMETMP].
*
* IMPORTANT: TCNTVAL cannot be used since this counter 
*            needs to count the hours for all employees,
*            so no persid.
*  
local lnSelect, lcEXPR, lcSql
local lnTotHOURS, lnTimeDT_HRS, lnTimeTMP_HRS 
local loCursor 

store 0 to lnTotHOURS, lnTimeDT_HRS, lnTimeTMP_HRS
store "" to lcEXPR, lcSql
store null to loCursor 

if empty(tcCounter) or isnull(loRS)
	return lnTotHOURS
endif 

tnPersId  = loRS.nPersId
tdStartDT = loRS.dStartDT
tdEndDT   = loRS.dEndDT

lnSelect = select()

*** Get posted timesheet transactions table [TIMEDT]
*** for all employees - TCNTVAL cannot be used.
lcEXPR = TCNTVAL("EXPR", tcCounter)
if empty(lcEXPR)
	select (lnSelect )
	return lnTotHOURS
endif 	
lcEXPR = strtran(lcEXPR, "and T_UNITCD='H'","")
lcEXPR = strtran(lcEXPR, "not inlist(T_OPT,", "T_OPT not in (")
lcEXPR = strtran(lcEXPR, "inlist(T_OPT,", "T_OPT in (")

text to lcSql textmerge noshow pretext 1+4
	select ISNULL(sum( (case when T_UNITCD='H' and T_TYPE<>'P' 
					then  T_HOURS else  00000.00 end)), 0) as TOTHRS, 
			CAST(0 as NUMERIC(12,2)) as TOTAMT 
	from TIMEDT
	WHERE ( T_EFFDT BETWEEN '[dSTARTDT]' AND '[dENDDT]' )
	AND [TIMEDT_CNT]
endtext 
lcSql = strtran(lcSql, "[nPERSID]", transform(tnPersId, "999999"))
lcSql = strtran(lcSql, "[dSTARTDT]", dtoc(tdStartDT))
lcSql = strtran(lcSql, "[dENDDT]", dtoc(tdEndDT))
lcSql = strtran(lcSql, "[TIMEDT_CNT]", alltrim(lcEXPR))

store null to loCursor 
loCursor = goDataMgr.SQLexec("/CURSOR=qTimeDT", set("datasession"), ;
						goDataMgr.ConnectionHandle, lcSql)

if isnull(loCursor)
	return lnTotHOURS
endif 
lnTimeDT_HRS = nvl(qTimeDT.TOTHRS, 0)


*** Get unposted timesheet transactions table [TIMETMP]
*** Strip counter to use with timetmp table   
lcEXPR = TCNTVAL("EXPR", tcCounter)
if empty(lcEXPR)
	select (lnSelect )
	return lnTotHOURS
endif 	
lcEXPR = strtran(lcEXPR, "and T_UNITCD='H'","")
lcEXPR = strtran(lcEXPR, "T_","TT_")
lcEXPR = strtran(lcEXPR, "not inlist(TT_OPT,", "TT_OPT not in (")
lcEXPR = strtran(lcEXPR, "inlist(TT_OPT,", "TT_OPT in (")

text to lcSql textmerge noshow pretext 1+4
	select ISNULL(sum( (case when TT_UNITCD='H' and TT_TYPE<>'P' 
					then  TT_HOURS else  00000.00 end)), 0) as TOTHRS, 
			CAST(0 as NUMERIC(12,2)) as TOTAMT 
	from TIMETMP
	WHERE ( TT_EFFDT BETWEEN '[dSTARTDT]' AND '[dENDDT]' )
	AND TT_POSTBY = ''
	AND [TIMETMP_CNT]
endtext 
lcSql = strtran(lcSql, "[nPERSID]", transform(tnPersId, "999999"))
lcSql = strtran(lcSql, "[dSTARTDT]", dtoc(tdStartDT))
lcSql = strtran(lcSql, "[dENDDT]", dtoc(tdEndDT))
lcSql = strtran(lcSql, "[TIMETMP_CNT]", alltrim(lcEXPR))

store null to loCursor 
loCursor = goDataMgr.SQLexec("/CURSOR=qTTDT", set("datasession"), ;
						goDataMgr.ConnectionHandle, lcSql)

if isnull(loCursor)
	return lnTotHOURS
endif 
lnTimeTMP_HRS = nvl(qTTDT.TOTHRS, 0)

use in select("qTimeDT")
use in select("qTTDT")

if (tdEndDT - date()) = 0
	lnTotHOURS = lnTimeTMP_HRS
else
	lnTotHOURS = lnTimeDT_HRS + lnTimeTMP_HRS
endif 

store null to loCursor 
select( lnSelect )

return lnTotHOURS

*=========================================================
procedure GetTimeOffTakenByPersId(tcCounter, toRS)
*** This method uses the counter from cMOBSEBCNT to get 
* the number of how many times an employee has taken 
* some time off. It uses both tables [TIMEDT] and 
* [TIMETMP].
*
* The counter in this.cMOBSEBCNT needs to exist.
*
local lnSelect, lcEXPR, lcSql, loCursor, lnPersId
local lnTotCNT, lnTimeDT_CNT, lnTimeTMP_CNT 
local llInList, llOutList

store 0 to lnTotCNT, lnTimeDT_CNT, lnTimeTMP_CNT
store "" to lcEXPR, lcSql
store null to loCursor

if empty(tcCounter) or isnull(toRS)
	return lnTotCNT
endif 

*** set step on
lnPersId  = toRS.nPersId
ldStartDT = toRS.dStartDT
ldEndDT   = toRS.dEndDT

lnSelect = select()

*** set step on 
lcEXPR = TCNTVAL("EXPR", tcCounter)
if empty(lcEXPR)
	select (lnSelect )
	return lnTotCNT
endif 	

lcEXPR = strtran(lcEXPR, "and T_UNITCD='H'","")
lcEXPR = strtran(lcEXPR, "not inlist(T_OPT,", "T_OPT not in (")
lcEXPR = strtran(lcEXPR, "inlist(T_OPT,", "T_OPT in (")

text to lcSql textmerge noshow pretext 1+4
   --MOBSEB time taken in hours 
	select COUNT(1) as TOTCNT from TIMEDT
	WHERE T_PERSID = [nPERSID] 
	AND ( T_EFFDT BETWEEN '[dSTARTDT]' AND '[dENDDT]' )
	AND [TIMEDT_CNT]
endtext 
lcSql = strtran(lcSql, "[nPERSID]", transform(lnPersId, "999999"))
lcSql = strtran(lcSql, "[dSTARTDT]", dtoc(ldStartDT))
lcSql = strtran(lcSql, "[dENDDT]", dtoc(ldEndDT))
lcSql = strtran(lcSql, "[TIMEDT_CNT]", alltrim(lcEXPR))

loCursor = goDataMgr.SQLexec("/CURSOR=qTmDT", set("datasession"), ;
						goDataMgr.ConnectionHandle, lcSql)
if isnull(loCursor)
	return lnTotCNT
endif 

lnTimeDT_CNT = nvl(qTmDT.TOTCNT,0)
use in select("qTmDT")
store null to loCursor

*** Get unposted timesheet transactions table [TIMETMP]
*** Strip counter to use with timetmp table   
lcEXPR = TCNTVAL("EXPR", tcCounter)
if empty(lcEXPR)
	select (lnSelect )
	return lnTotCNT
endif 	
lcEXPR = strtran(lcEXPR, "and T_UNITCD='H'","")
lcEXPR = strtran(lcEXPR, "T_","TT_")
lcEXPR = strtran(lcEXPR, "not inlist(TT_OPT,", "TT_OPT not in (")
lcEXPR = strtran(lcEXPR, "inlist(TT_OPT,", "TT_OPT in (")

text to lcSql textmerge noshow pretext 1+4
   --MOBSEB time taken in hours 
	select COUNT(1) as TOTCNT from TIMETMP
	WHERE TT_PERSID = [nPERSID] 
	AND ( TT_EFFDT BETWEEN '[dSTARTDT]' AND '[dENDDT]' )
	AND TT_POSTBY = ''
	AND [TIMETMP_CNT]
endtext 
lcSql = strtran(lcSql, "[nPERSID]", transform(lnPersId, "999999"))
lcSql = strtran(lcSql, "[dSTARTDT]", dtoc(ldStartDT))
lcSql = strtran(lcSql, "[dENDDT]", dtoc(ldEndDT))
lcSql = strtran(lcSql, "[TIMETMP_CNT]", alltrim(lcEXPR))

loCursor = goDataMgr.SQLexec("/CURSOR=qTTDT", set("datasession"), ;
						goDataMgr.ConnectionHandle, lcSql)
if isnull(loCursor)
	return lnTotCNT
endif 

lnTimeTMP_CNT = nvl(qTTDT.TOTCNT, 0)
use in select("qTTDT")
store null to loCursor

lnTotCNT = lnTimeDT_CNT + lnTimeTMP_CNT
select( lnSelect )

return lnTotCNT

*=========================================================
procedure GetVacationEntitlement(toBizTimsum, tcPlanId)
*** Calls the entitlement info in order to project  
* the estimates for the next year.
*
local lnSelect, ldAsOf, lnEntitle, lcRetval
local loR

ldAsOf = date()
lnEntitle = 0

store null to loR

if type("toBizTimsum")!="O" or isnull(toBizTimsum)
	return 
endif 

if empty(alltrim(tcPlanId))
	return 
endif 

if !seek(tcPlanId, "qAPlan", "AP_PLANID")
	return 
endif 
	
lnSelect = select()
		
if !empty(qAPlan.AP_YEARN1)

	lcRetval = alltrim(qAPlan.AP_YEARN1)

	if type("lnSenMths")="U"
		public lnSenMths
		loR = toBizTimsum.GetEntitleInfo("", qAPlan.AP_PERIOD, ;
				lnPersId, TS_BANKID, date())
		
		if type("loR")!="O" or isnull(loR)
			store null to loR
			return lnEntitle 
		endif 

		lnSenMths = loR.nSenMths
		store null to loR
	endif 

	lnEntitle =GetEntitleInfoEstimate()
endif

select (lnSelect)

return lnEntitle
endproc

*=========================================================
procedure GetEntitleInfoEstimate()
*** Runs the entilement with different dates interval 
* for vacation banks in order to projects next year 
* estimates.
*
local loVacBank, loVacParm, lnNumOfVacUnits
store null to loVacBank, loVacParm
store 0 to lnNumOfVacUnits

loVacBank = newobject("CVacationBank", "CVacationBank.prg")
if isnull(loVacBank) or type("loVacBank")!="O" 
	loVacBank = null 
	return lnNumOfVacUnits  
endif 

*** Get the object 
*** Set any new properties to the object related 
*** to this specific environment, if need to.
loVacParm = loVacBank.GetVacBankObject(lnSenMths)
if isnull(loVacParm) or type("loVacParm")!="O" 
	loVacParm = null 
	return lnNumOfVacUnits  
endif 

*** Increase the plan date interval so that 
* the vacation entitlement is calculated 
* for the current year (YTD).
loVacParm.dStartDt = gomonth(qAPlan.AP_EFFDT, 12)
loVacParm.dEndDt = gomonth(qAPlan.AP_ENDDT, 12)
loVacParm.dLYStartDt = gomonth(loVacParm.dStartDt, -12)
loVacParm.dLYEndDt = gomonth(loVacParm.dEndDt, -12)

*** Call into the process with the object 
*** to do the entitlement 
loVacBank.DepositBeginningOfYear(@loVacParm, "/VIEWONLY")

*** Get the entitlement calculation 
lnNumOfVacUnits = loVacParm.nTotVacDays
if inlist(loVacParm.cBankUnit, "H")
	lnNumOfVacUnits = loVacParm.nTotVacHH 
endif 

store null to loVacBank, loVacParm
return lnNumOfVacUnits

*=========================================================
procedure CreateTBankObject()
*** Object factory for vacation object 

local loR

loR = createobject("empty")

*** Object to hold the filter parameters 
loR = createobject("empty")
addproperty(loR, "nPersId", 0)
addproperty(loR, "nPeriodId", 0)
addproperty(loR, "dStartDT", {})
addproperty(loR, "dEndDT", {})
addproperty(loR, "nSnPtCYTP", 0)
addproperty(loR, "nSnNPCYTP", 0)
addproperty(loR, "nFlexCPTD", 0)
addproperty(loR, "nSdWkCPTD", 0)
addproperty(loR, "nReWkCPTD", 0)
addproperty(loR, "nWkDysCPTD", 0)
addproperty(loR, "nOTCYTD", 0)
addproperty(loR, "nSebSynCYTD", 0)
addproperty(loR, "nMOBSEHR", 0)

return loR
