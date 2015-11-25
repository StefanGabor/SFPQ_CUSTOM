*========================================================
*  Program...........: bhvTimeTmp1_Custom
*  Author............: Stefan Gabor 
*  Project...........: SFPQ 
*  Created...........: February 12, 2014
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies 2014
*  Description.......: Behavior for TimeTmp1 (Timesheet) form
*                    :
*                    : *** CUSTOM VERSION FOR SFPQ ***
*                    :
*  Technical Notes...: NOTE: This is a subclass of
*                    :       bhvTIMETMP1.
*                    :
*                    :
*  Major modifs......: February - 2014 
*                    : Use the Analyser object to calculate all  
*                    : timesheet details.
*                    : 
*                    :
*  Modification......: August - 2014 
*                    : Remove the EXPENCES from total hours 
*                    :
*  Modification......: September - 2014 
*                    : 1 - Added a new label [FRAIS($)] in the 
*                    : timesheet for expenses. 
*                    : Note expenses are amounts and not hours.
*                    : 2 - Added rules for handling bank 8, 
*                    : [Union Leave], MAX balance validation.
*                    : 
*                    :
*  Modification......: October - 2015 
*                    : 1 - Added a limit for the number of 
*                    : absences taken by SFPQ period (6).
*                    : 2 - Added a limit of how many CFLEX 
*                    : can be taken by SFPQ period (1).
*                    : 
*                    :
*#########################################################
define class bhvTimeTmp1_custom ;
		as bhvTimeTmp1 of bhvTimeTmp1.prg
*#########################################################

*** Properties 
cHALFDAY = "DJ"
csrBankHrs = "qSchedList"
nSchedHours= 0.00
cSchedHours= "" 
cOptType = "" 
oBankHours = null
cSchedId = ""

nKilometerYTD = 0.00
nKmMax = 8000.00
nUnionLeaveYTD = 0.00
l1stWeekPosted = .f. 						&& First week expenses posted
nSEB_UNION_LEAVE_DAYS = 30					&& Union leave SEB 
nAPMCP_UNION_LEAVE_DAYS_25 = 25			&& Union leave APMCP 25 DAYS 
nAPMCP_UNION_LEAVE_DAYS_30 = 30			&& Union leave APMCP 30 DAYS 
nSEBHRSPDAY = 6.50 							&& Average hours par day - SEB 
nAPMCPHRSPDAY = 7.00 						&& Average hours par day - APMCP 

nMaxExpense = 0.00
nEXPUnitRate = 0.00

*** Expenses TT_INOUT 
cPrevINOUT = ""

*** Calendar 
lUseSidebar = .t.

*** timesheet posting 
lAllowPostBeforeAllSigned = .t.
lPostOnlyValidated = .t.

*-- Can employee sign without minimum hours?
lRequireMinHours = .t.

*-- How many periods in the future can employee go to?
nFuturePeriods = Null		&& Null = no restriction

*-- Time BANKIDs can deposit surplus in
cDepositSurplusIntoBankids = "6"
cBank6DepositOpt = ""
cBank9DepositOpt = ""

lShowPlanNames = .T. 
lShowDeficitDialogForSingleBank= .f.
lShowSurplusDialogForSingleBank= .f. 

*-- Time BANKIDs can withdraw deficit from
cWithdrawDeficitFromBankids = "69"		&& Bankids.  E.g. "13"

*** OPTs to use for bank withdrawal transactions.
* Create one per bankid, replacing the Z with the Bankid
cBank1WithdrawalOpt = ""	
cBank2WithdrawalOpt = ""
cBank3WithdrawalOpt = ""
cBank5WithdrawalOpt = ""
cBank6WithdrawalOpt = ""
cBank9WithdrawalOpt = ""

*** Option to reverse 
cRegOptToReverse = "REGU" 
cRegOpts = "REGU"

*-- Page 2.
cPg2Tot1_Caption = "REG"
cPg2Tot2_Caption = "PRIME"
cPg2Tot3_Caption = "ABS"
cPg2Tot4_Caption = "TSUP"
cPg2Tot5_Caption = "N/F"

*-- Holiday Handling
cPreloadHolidaysFor = ".t."				&& Logical expression
cHolidayOpt = "FERIE"

*** Main Form headings, etc.
*-- Page 1.
cPg1TotHrsPaid_Caption = ;
			iif(gcLang="E", "HRS PAID: ", "HRES TOTALES: ")
cPg1TotHrsBanked_Caption = ;
			iif(gcLang="E", "HRS BANKED: ", "MISES EN BANQUE: ")
cPg1TotCurrPeriod_Caption = ;
			iif(gcLang="E", "THIS PERIOD: ", "PÉRIODE COURANTE: ")
cPg1TotUnpaidPeriod_Caption = ;
			iif(gcLang="E", "HRS UNPAID", "SANS SOLDE")
cPg1TotExpensePeriod_Caption = ;
			iif(gcLang="E", "EXPENSES ($): ", "FRAIS ($): ")

dPeriodStDT = ""								&& SFPQ start period 
dPeriodEdDT = ""								&& SFPQ end period 
dSFPQYRStDT = ""								&& SFPQ start year date  
dSFPQYREdDT = ""								&& SFPQ end year date 
cFLEXCOUNTER= "TFLEXI"						&& FLEX time bank counter  
cMOBSEBCNT	= "MOBSEHR"						&& MOBSEB time bank counter  
cFLEXTBANK  = "CFLEXI"						&& IN/OUT default time bank code
nTIMEOFFMAX = 6								&& MAX num. of time off allowed						
nHRSPDY = 6.50									&& Hours / day 
nHRSPDYRND= .50								&& 2 1/2 days round off  
dSEBEffDT= ctod("2015/10/29")				&& Effective date of new rules SEB 
cINLIST  = ""
cOUTLIST = ""

*** Debugging  
cLogStatus = "startlog.txt" 				&& triggers for start logging 
cLogPath = ""		 							&& log folder 
_DEBUG = .T.									&& set .T. for debugging 

*========================================================
procedure Init
parameters loThisform, pcSubType, pnPersId

dodefault(loThisform, pcSubType, pnPersId)

*** Set the log path 
if type("goIni.Temp") = "C" ;
and !empty(goIni.Temp)
	this.cLogPath = addbs(alltrim(goIni.Temp))
endif 

*** Create cursors to hold schedule values 
this.CreateScheduleCursor()

return

*=========================================================
procedure FindObj()
dodefault()

this.oBankHours = findobj(this.oThisform, "BankHours")

return

*=========================================================
procedure ReleaseObjRefs
store null to .oBankHours

dodefault()

return

*=========================================================
procedure Init_CreateVNew()
local lnSelect 

if !dodefault()
	return 
endif 	

lnSelect = select()

if !used("vNew")
	return 
endif 

select vNew 
alter table vNew add column TT__EUNIT N(10,4)	
alter table vNew add column TT__EURATE N(10,2)	
alter table vNew add column TT__ATY M 
alter table vNew add column TT__LOC M

select(lnSelect)
return 

*=========================================================
procedure InitNewPers_Custom()
*** For cusotmization.  Set Plages, etc.
*   Info about the current person is in qJobhist. It
*   contains both PERS and JOBHIST fields.
local lnSelect, loSched, lcPayGRP, ldEffDT
local ldFrom, ldThru, lcWhere
local loCsrTTmp

store null to loSched, loCsrTTmp
store "" to lcPayGRP, ldFrom, ldThru, lcWhere

if !used("qJobhist")
	return 
endif 


lnSelect = select()

with this 

*** .csrPostExp
ldFrom = .dFromDT
ldThru = .dFromDT + 6

lcWhere = ""
lcWhere = Makefor("", lcWhere, ;
				"TT_PERSID=" + lstr(this.PersId), ;
				iif(empty(ldFrom), "", "TT_EFFDT>=" + ;
						tochar(ldFrom, "/SQL/QUOTES")), ;
				iif(empty(ldThru), "", "TT_EFFDT<=" + ;
						tochar(ldThru, "/SQL/QUOTES")))

lcWhere = Makefor("", lcWhere, ;
			"( TT__EUNIT <> 0 OR TT__EURATE <> 0 )", ;
			"( TT_SIGNBY <> '' AND TT_POSTBY = '' )" )

*** We have the cursor with all the dates 
* for which the expenses have been posted.		
loCsrTTmp = .oBizMgr.oBizTimetmp1.GetList( ;
		"/CURSOR=qPostExp", ;
		"TT_PERSID, TT_EFFDT", lcWhere, "TT_EFFDT")

*** We have the cursor with all the dates 
* for which the expenses have been posted.		
if !isnull(loCsrTTmp)
	.l1stWeekPosted = (reccount("qPostExp")>0)
endif 

select (lnSelect)

** set step on
ldEffDT = this.dFromDT
if used("vTimeTmp") and !empty(vTimeTmp.TT_EFFDT)
	go top in vTimeTmp
	ldEffDT = vTimeTmp.TT_EFFDT 
endif 
 
*** Get the hours from the schedule instead of qJobhist 
loSched = .oBizSched.WkCal("OBJECT", qJobhist.H_SCHEDID, ;
		ldEffDT)

.nHRSPDY = .oBizSched.WkCal("AVHRSPDY", ;
							qJobhist.H_SCHEDID, date())

if !isnull(loSched) and type("loSched") = "O" 
	.nMaxRegHoursWeek = nvl(loSched.AVGHRSPER,0)
	.nMinRegHoursWeek = nvl(loSched.AVGHRSPER,0)
else 
	.nMaxRegHoursWeek = nvl(qJobhist.H_HRSPER,0)
	.nMinRegHoursWeek = nvl(qJobhist.H_HRSPER,0)
endif 

lcPayGRP = trim(tbleval("PAYGRP", qJobhist.H_PAYGRP, "TBLC1"))
if "B"$lcPayGRP
	.nMaxRegHoursWeek = .nMaxRegHoursWeek * 2 
	.nMinRegHoursWeek = .nMinRegHoursWeek * 2 
endif 

.WriteLog("InitNewPers_Custom() - " + ;
		"nMaxRegHoursWeek = " + transform(.nMaxRegHoursWeek)+;
		",  nMaxRegHoursWeek = "+transform(.nMinRegHoursWeek))

.InitBankWithdrawal()
.getKilometersYTD()
.getUnionLeaveYTD()
.main()

endwith 

store null to loSched
select (lnSelect)
return 

*==========================================================
procedure cmdGo_EnableDisable(poThis)
**** Here 
if !used("vNew")
	return .f. 
endif 	

if empty(vNew.TT__EUNIT) ;
and empty(vNew.TT__EURATE)
	return dodefault(poThis)
else 
	return this.oThisform.Updmode <> " " ;
			and ( !empty(vNew.TT__EUNIT) ;
			or !empty(vNew.TT__EURATE) )
endif 

*=========================================================
#define SCREEN_LAYOUT 
*** Procedures related to screen layout of controls 
*=========================================================
procedure SetScreen(poThis, tcTranxType)
*** This procedure refreshes the GRID columns 
* base on transaction type  
* OPTIONS:	1 - AMTOPT 
*				2 - ABSENCE - can by taken by 1/2 day  
*				3 - PRIME - Expenses
*
local lcTranxType, llExpenseAccount, lcOPTValue
store "" to lcTranxType, lcOPTValue

*
with this 

lcTranxType = trim(tcTranxType)
if lcTranxType = "P"
	llExpenseAccount=ToLogical(tbleval("PRIME", Optlist.TBLID, "TBLC6"))
endif 

.RefreshScreen(poThis.Parent,lcTranxType,llExpenseAccount)

*** Refresh the EXPENSES 
if llExpenseAccount
	if type("poThis.Parent.cboExpense") = "O" ;
	and poThis.Parent.cboExpense.Visible 
		if (atw("-",poThis.Parent.Opt.DisplayValue)>0)
			lcOPTValue = alltrim(leftto(poThis.Parent.Opt.DisplayValue,"-"))
			poThis.Parent.cboExpense.Requery(lcOPTValue)
		endif 
	endif 
endif 

.oThisform.Refresh()
endwith 

return 

*==========================================================
procedure RefreshScreen(poThis,tcTranxType,tlExpenseAccount)
*** This procedure refreshes the GRID columns 
* base on OPT values 
* OPTIONS:	1 - AMTOPT 
*				2 - PRIME - Expenses
*
local llAbsence 
this.cOptType = upper(trim(tcTranxType))
llAbsence = this.IsTreatedAsAbsence(Optlist.TBLID)

if type("poThis.GrdEmpl") != "O" 
	return 
endif

with poThis

	*** Show regular time input 
	.Hours.visible = (!llAbsence and !tlExpenseAccount)
	*** .Hours.LabelObj.visible = (!llAbsence and !tlExpenseAccount)
	.Hours.LabelObj.visible = (!llAbsence)

	*** Do not show TxtInOut for expenses   
	***.TxtInOut.visible = !tlExpenseAccount

	*** Show absences input 
	.BankHours.visible = (llAbsence and !tlExpenseAccount)
	.BankHours.LabelObj.visible = (llAbsence and !tlExpenseAccount) 

	*** Show expense account input 
	.txtEUnit.visible = tlExpenseAccount
	.txtEUnit.LabelObj.visible = tlExpenseAccount
	.txtExpense.visible = tlExpenseAccount
	.txtExpense.LabelObj.visible = tlExpenseAccount
	.txtActivity.visible = tlExpenseAccount
	.txtActivity.LabelObj.visible = tlExpenseAccount
	.txtLocation.visible = tlExpenseAccount
	.txtLocation.LabelObj.visible = tlExpenseAccount

	.Refresh()	
endwith 

return 

*==========================================================
procedure GrdEmpl_SetProperties(poThis)
local lcDynamicBackColor, lcDynamicForeColor

if !used("vTimetmp")
	return
endif

with poThis

.RecordSource = "vTimeTmp"
.ColumnCount = 9
.RowHeight = 21

*** 234,224,252
lcDynamicBackColor = ;
		"iif((!empty(vTimetmp.TT_SIGNBY) AND vTimetmp.TT_TYPE='P'), " + ;
		"rgb(255,183,183), iif(vTimetmp.TT_EFFDT < thisform.oBhv.dFromDt, " + ;
		"rgb(232,232,255), iif((vTimetmp.TT_HOURS = 0), " + ;
		"rgb(241,250,220), iif((empty(vTimetmp.TT_SIGNBY) AND vTimetmp.TT_TYPE='P')," + ;
		"rgb(255,255,192), rgb(255,255,255)))))"

lcDynamicForeColor = "iif((vTimetmp.TT_OPT='VAREMP'), " + ;
		"rgb(255,255,255), rgb(0,0,0))"

with .Column1
	.ControlSource = [vTimeTmp.vvShowday]
	.Width = 106
	.Alignment = 0		&& Middle Left
	.ReadOnly = .t.
	.SelectOnEntry = .f.
	.DynamicBackcolor = lcDynamicBackColor
	.DynamicForecolor = lcDynamicForeColor
endwith

with .Column2
	.ControlSource = [vTimeTmp.TT_ShowOPT]
	.Width = 150
	.Alignment = 0		&& Middle Left
	.ReadOnly = .t.
	.SelectOnEntry = .f.
	.DynamicBackcolor = lcDynamicBackColor
	.DynamicForecolor = lcDynamicForeColor
endwith

with .Column3
	.ControlSource = [vTimeTmp.vvHHMM]
	.Width = 50
	.Alignment = 1		&& Middle Right
	.Format = "R"
	.InputMask = "99999:99"
	.DynamicBackcolor = lcDynamicBackColor
	.DynamicForecolor = lcDynamicForeColor
endwith

with .Column4
	.Width = 75
	.ControlSource = ;
			[iif(vTimetmp.TT_INOUT="", "", ] + ;
			[transf(left(vTimeTmp.TT_INOUT,5),"@R 99:99") + "-" + ] + ;
			[transf(substr(mline(vTimetmp.TT_INOUT,memlines(vTimetmp.TT_INOUT)),6,4),"@R 99:99"))]
	.ReadOnly = .t.
	.DynamicBackcolor = lcDynamicBackColor
	.DynamicForecolor = lcDynamicForeColor
endwith

*** Add the EXPENSE UNIT column 
with .Column5
	.ControlSource = [vTimeTmp.TT__EUNIT]
	.Width = 60
	.Alignment = 1		&& Middle Right
	.Format = "@Z"
	.InputMask = "999.99"
	.DynamicBackcolor = lcDynamicBackColor
	.DynamicForecolor = lcDynamicForeColor
endwith

*** Add the EXPENSE UNIT column 
with .Column6
	.ControlSource = [vTimeTmp.TT__EURATE]
	.Width = 56
	.Alignment = 1		&& Middle Right
	.Format = "@Z"
	.InputMask = "999.99"
	.DynamicBackcolor = lcDynamicBackColor
	.DynamicForecolor = lcDynamicForeColor
endwith

with .Column7
	.Width = 165
	.ControlSource = [trim(left(vTimeTmp.TT__ATY,127))]
	.ReadOnly = .t.
	.DynamicBackcolor = lcDynamicBackColor
	.DynamicForecolor = lcDynamicForeColor
endwith

with .Column8
	.Width = 125
	.ControlSource = [trim(left(vTimeTmp.TT__LOC,127))]
	.ReadOnly = .t.
	.DynamicBackcolor = lcDynamicBackColor
	.DynamicForecolor = lcDynamicForeColor
endwith

with .Column9
	.Width = 774
	.ControlSource = [trim(left(vTimeTmp.TT_Notes,127))]
	.ReadOnly = .t.
	.DynamicBackcolor = lcDynamicBackColor
	.DynamicForecolor = lcDynamicForeColor
endwith

*** Set read-only prop based on signed status
this.GrdEmpl_EnableDisable(poThis)

endwith
return

*=========================================================
procedure GrdEmpl_InitNewRow
*** Use this to protect/unprotect columns
nodefault

return

*==========================================================
procedure GrdBatch_SetProperties(poThis)
*** Resize the column to match the totals 
dodefault(poThis)

with poThis

*** Wider column range accept for approval 
.Column3.Width = 164
.Column4.Width = 240
.Column5.Width = 70
.Column6.Width = 55
.Column7.Width = 55
.Column8.Width = 55
.Column9.Width = 55

.Column10.Width = 55
.Column10.InputMask = iif(this.lShowMgrPageAsHHMM, "XXXXXX", "9999.99")

.Column11.Width = 55
.Column11.InputMask = iif(this.lShowMgrPageAsHHMM, "XXXXXX", "9999.99")
.Column12.Width = 0
.Column13.Width = 0

endwith 

return 

endproc

*=========================================================
procedure GrdEmpl_DblClick(poThis)
local lnSelect, lnWkHours, ltDateTime  
local lcMaxExpense, lcComm1, lcComm2, lnReply   
local llExpenseAccount, lnSchedCount 

store "" to lcMaxExpense
store 0 to lnReply, lnSchedCount

if trim(gcLang) = "E" 
	lcComm1="The record has been signed and can no longer be modified!"  
	lcComm2="This record has been signed! Would you like to modify it?"
else
	lcComm1="L'enregistrement a été signé et ne peut plus être modifié!"
	lcComm2="Cet enregistrement a été déjà signé! Voulez-vous le modifier?" 
endif 

*** Allow only "S" to modify a sign expense record 
* in the timesheet 
llExpenseAccount=ToLogical(tbleval("PRIME", vTimetmp.TT_OPT, "TBLC6"))
if llExpenseAccount and !empty(vTimetmp.TT_SIGNBY)
	if "M"$this.cRole  
		alert(name(this.PersId)+CRLF+lcComm1, ;
				0, "\!\?\<OK")
		return .f.			
	else 
		lnReply = alert(lcComm2,0, "\!\<Oui;\?\<Non")
		if lnReply <> 1
			return .f.
		endif 	
	endif 
endif 	

dodefault(poThis)

lnSelect = select()
*
with this

	*** Replace ASA the user double click so that 
	* TS sees that the current record is being modified
	ltDateTime = datetimex()
	replace next 1 TT_MODDT with ltDateTime in vTimetmp 
	replace next 1 TT_MODDT with ltDateTime in vNew 

	*** Unsign the expenses 
	if lnReply = 1 
		replace next 1 TT_SIGNBY 	with "" in vTimetmp 
		replace next 1 TT_SIGNDTM 	with {} in vTimetmp 
	endif 

	.nSchedHours = vTimetmp.TT_HOURS
	.cSchedHours = trim(vTimetmp.TT_INOUT)

	*** Get the maximum allowed 
	lcMaxExpense=tbleval("PRIME",trim(vTimetmp.TT_OPT),"TBLC7")
	.nMaxExpense = val(lcMaxExpense)
	.nEXPUnitRate= vNEW.TT__EURATE

	*** Refresh screen 
	.SetScreen(poThis,vTimetmp.TT_TYPE)

	*** Position the record in the cursor 
	if !empty(.cSchedHours)
		select (.csrBankHrs) 
		go top in (.csrBankHrs)
		
		count to lnSchedCount for !deleted()
		if lnSchedCount = 0
			this.FillInScheduleList(vTimetmp.TT_OPT)
		endif 

		go top in (.csrBankHrs)
		locate for trim(cInOut) = .cSchedHours and !eof()

		poThis.Parent.BankHours.DisplayValue = alltrim(cHours)
	endif

	poThis.Parent.txtExpense.Refresh()
endwith 

select(lnSelect)
return 

*==========================================================
procedure txtInOut_Clicked(poThis)
*** Cannot modify an absence entry trough this control
*
local lcComm1, lcValue
store "" to lcComm1, lcValue

if isnull(poThis)
	return 
endif 	

lcValue = alltrim(poThis.Parent.bankHours.DisplayValue)

if trim(gcLang) = "E" 
	lcComm1= "This value [  " + lcValue + ;
		"  ] cannot be modified manually!"+CRLF+;
		"Please use the drop-down list for another selection."  
else
	lcComm1 = "Cette valeur [  " + lcValue + ;
	"  ] ne peut pas être modifié manuellement!"+CRLF+;
	"S.V.P. utiliser la liste déroulente pour une autre sélection." 
endif 

alert(name(this.PersId)+CRLF+lcComm1, ;
		0, "\!\?\<OK")

return 
 
*==========================================================
procedure IsTreatedAsAbsence(tvOptId)
***
*
local lcOptId, llAbsence
llAbsence = (this.cOptType = "A")

if !llAbsence 
	return .f.
endif 

if isnull(tvOptId)
	return .f. 
endif 

if type("tvOptId") = "C" and empty(tvOptId)
	return .f. 
endif 

if type("tvOptId") = "O"
	lcOptId = alltrim(leftto(tvOptId.Parent.Opt.DisplayValue,"-"))
endif 
if type("tvOptId") = "C"
	lcOptId = alltrim(upper(tvOptId))
endif 

llAbsence=!(ToLogical(tbleval("ABSENCE", lcOptId, "TBLC20")))

return llAbsence

*==========================================================
procedure RefreshControl()

with this 

.cSchedHours= "" 
if !empty(.cSchedHours)
	go top in (.csrBankHrs) 
endif 

.oThisform.Pgf1.Page1.BankHours.DisplayValue = ""
.oThisform.Pgf1.Page1.BankHours.Refresh()

endwith 

return 

*=========================================================
#define VALIDATION_METHODS 
*=========================================================
procedure Opt_Valid(poThis)
local lcMaxExpense, lnDAYSPYYEAR, lnUNIONLEAVEDAYS 
local lnFLEXHRSTAKEN, lnMOBSEBTAKENCNT
local lcComm1, lcComm2, lcComm3, lcComm4
local ldFrom, ldThru
*
dodefault(poThis)

store "" to lcMaxExpense 
store 0 to lnDAYSPYYEAR, lnUNIONLEAVEDAYS, lnFLEXHRSTAKEN

ldFrom = this.dFromDT
ldThru = this.dFromDT + 6

if gcLang = "F" 
	lcComm1 = "Le nombre maximal de jours par année autorisé a été atteint : "
	lcComm2 = "S'il vous plaît corriger votre feuille de temps!"
	lcComm3 = "Le nombre maximal d'heures par période autorisé a été atteint : "
	lcComm4 = "Le nombre maximal d'absences par année autorisé a été atteint : "
	lcComm5 = "Vous ne pouvez plus entrer dans les dépenses pour la semaine du: "
else
	lcComm1 = "The maximum number of days/year allowed has been reached: "
	lcComm2 = "Please use a different code."
	lcComm3 = "The maximum number of hours/period allowed has been reached: "
	lcComm4 = "The maximum number of allowed absences per year has been reached: "
	lcComm5 = "You can no longer enter expenses for the week of: "
endif 


*** Validate for UNION LEAVE 25 or 30 days 
if inlist(Optlist.TBLID, "SEBREG", "ACTSYNSE", "ACTSYN25", "ACTSYN30") ;
and inlist(qJobhist.H_UNION, "SEB", "APMC")
	do case 
	case "SEB"$qJobhist.H_UNION 
		lnDAYSPYYEAR = (this.nUnionLeaveYTD / this.nSEBHRSPDAY)
		lnUNIONLEAVEDAYS = this.nSEB_UNION_LEAVE_DAYS
		llUnionLeaveExceeds = (lnDAYSPYYEAR >= this.nSEB_UNION_LEAVE_DAYS)
	case "APMCP"$qJobhist.H_UNION 
		lnDAYSPYYEAR = (this.nUnionLeaveYTD / this.nAPMCPHRSPDAY)
		if inlist(Optlist.TBLID, "ACTSYN25") 
			lnUNIONLEAVEDAYS = this.nAPMCP_UNION_LEAVE_DAYS_25 
			llUnionLeaveExceeds = (lnDAYSPYYEAR >= this.nAPMCP_UNION_LEAVE_DAYS_25)
		endif 
		if inlist(Optlist.TBLID, "ACTSYN30") 
			lnUNIONLEAVEDAYS = this.nAPMCP_UNION_LEAVE_DAYS_30 
			llUnionLeaveExceeds = (lnDAYSPYYEAR >= this.nAPMCP_UNION_LEAVE_DAYS_30)
		endif 
	endcase 

	if llUnionLeaveExceeds
		alert(lcComm1 + transform(lnUNIONLEAVEDAYS, "9999.99") + ;
			CRLF+lcComm2,0,"\!\?\<OK")

		return
	endif 
endif 


*** Get number of hours taken in FLEX bank during one period
lnFLEXHRSTAKEN =this.GetFLEXHRSByPersId(this.cFLEXCOUNTER, this.PersId)
if lnFLEXHRSTAKEN > (this.nHRSPDY + this.nHRSPDYRND) ;
and inlist(Optlist.TBLID, this.cFLEXTBANK)
	alert(lcComm3 + transform((lnFLEXHRSTAKEN/this.nHRSPDY), "9") + ;
			CRLF+lcComm2,0,"\!\?\<OK")

	return
endif 


*** set step on 
*** Get number of paid MOBILE absences taken during one period
lnMOBSEBTAKENCNT =this.GetTimeOffTakenByPersId(this.cMOBSEBCNT, this.PersId)
if lnMOBSEBTAKENCNT > this.nTIMEOFFMAX
	if this.IsCodeAllowedInTS(Optlist.TBLID, Optlist.TT_TYPE)
		alert(lcComm4 + transform(this.nTIMEOFFMAX, "999") + ;
				CRLF+lcComm2,0,"\!\?\<OK")

		return
	endif 
endif 


*** Validate Expences 
llExpenseAccount=ToLogical(tbleval("PRIME",Optlist.TBLID,"TBLC6"))
if llExpenseAccount
	*** Validate how many KM or KME - limit 8000 KM
	if inlist(Optlist.TBLID, "KMC", "KME")
		if this.nKilometerYTD >= this.nKmMax

			alert(lcComm1 + transform(this.nKmMax, "9999.99") + ;
				CRLF+lcComm2,0,"\!\?\<OK")

			return
		endif 
	endif 
	
	*** 1st week of expences has been posted.
	if this.l1stWeekPosted ;
	and between(TT_EFFDT, ldFrom, ldThru)

		alert(lcComm5 + transform(ldFrom) + ;
			CRLF+lcComm2,0,"\!\?\<OK")

		*** Reset to default - validation does not work right
		replace vNEW.TT_OPT with this.cRegOptToReverse in vNEW
		poThis.Parent.Opt.Refresh()
	endif 	
endif 

lcMaxExpense = tbleval("PRIME", Optlist.TBLID, "TBLC7")
this.nMaxExpense = val(lcMaxExpense) 

if !(type("poThis.Parent.txtExpense")="O") 
	return 
endif 

this.nEXPUnitRate = this.nMaxExpense 
poThis.Parent.txtExpense.Value = this.nMaxExpense

poThis.Parent.BankHours.DisplayValue = ""
this.FillInScheduleList(Optlist.TBLID)
this.SetScreen(poThis, Optlist.TT_TYPE)

return

*==========================================================
procedure EmplDay_Valid(poThis)
*
if dodefault(poThis)
	this.FillInScheduleList()
endif 
	
return 

*======================================================================
procedure cmdMod_Valid(poThis)
*
if empty(vTimetmp.TT_SIGNBY)
	dodefault(poThis)
	return 
endif 
	
if !empty(vTimetmp.TT__EUNIT) ;
or !empty(vTimetmp.TT__EURATE)
	*** set step on 
	loGrdEmpl = findobj(this.oThisform, "GrdEmpl")
	this.GrdEmpl_DblClick(loGrdEmpl)
endif 

return

*=========================================================
procedure BankHours_Valid (poThis)
*
if !dodefault(poThis)
	return 
endif 	

with this 

.nSchedHours = qSchedList.nHours 
.cSchedHours = trim(qSchedList.cHours)

replace vNew.TT_HOURS with .nSchedHours in vNew 
replace vNew.TT_INOUT with trim(qSchedList.cInOut) in vNew 

endwith 

return

*=========================================================
procedure cmdGo_Valid(poThis)
***
local lcTTOPT 
store "" to lcTTOPT 

if type("C_TTPREGOPT") != "U" 
	lcTTOPT = alltrim(C_TTPREGOPT)
endif 

if empty(lcTTOPT)
	lcTTOPT = "REGU"
endif 	

if dodefault(poThis)
	*** Refresh screen
	this.SetScreen(poThis, lcTTOPT)
endif 

return 

*==========================================================
procedure cmdClear_Valid()

dodefault()

this.RefreshControl()

return 

*==========================================================
procedure txtInOut_Valid_MinimumLunchBreak
lparameters toAnalysis, lcLunchBreakMM
*** Verify that the total time taken for lunch respects  
* the union agreement. The numeric value it is specified 
* in the TBL and these values need to be specified by 
* the schedule id.
* 
* Ex. SC_SCHEDID = FLE325, TBL = "_SCHEDULE", TBLC8 = 45 
* which means that everyone who has the schedule FLE325 
* need to take at least 45 minutes for lunch break.
*
local loSched, loBizPlanDt, lnTimeInCnt
local lcStartExHM, lcEndExHM
local lnStartExHM, lnEndExHM
local ldEffdt, llOk 

llOk = .t. 
store null to loSched, loBizPlanDt
store "" to lcStartExHM, lcEndExHM
store 0 to lnStartExHM, lnEndExHM, lnTimeInCnt
store {} to ldEffdt 

*** Only regular hours 
if !(alltrim(TT_OPT)$C_TTPREGOPT)
	return 
endif 

ldEffdt = ttod(toAnalysis.StartDTM) 
loBizPlanDt = this.oBizmgr.GetBiz("PLANDT")
if !isnull(loBizPlanDt)
	loSched = loBizPlandt.GetSched("/SCHED /OBJECT", ;
				 this.PersId, ldEffdt, ldEffdt)
endif 

if isnull(loSched)
	store null to loSched, loBizPlanDt
	return llOk
endif 

*** Start & End of lunch time 
lnSchedStartExHM = HMtoM(alltrim(loSched.StartEXHM))
lnSchedEndExHM = HMtoM(alltrim(loSched.EndEXHM))

if empty(this.cSchedId)
	this.cSchedId = trim(loSched.BaseSchedId) 
endif 

if memlines(toAnalysis.UsedHM)<2
	store null to loSched, loBizPlanDt
	return llOk
endif 

*** Get the minimum lunch break from TBL
lcLunchBreakMM = tbleval("_SCHEDULE", this.cSchedId, "TBLC8")
*** debug 
this.WriteLog("txtInOut_Valid_MinimumLunchBreak() - " + ;
		"lcLunchBreakMM = " + transform(lcLunchBreakMM))

if val(lcLunchBreakMM)=0
	return llOk 
endif 

*** Get the total lunch break taken 
lcStartExHM = rightfrom(mline(toAnalysis.UsedHM, 1), "-")
lnStartExHM = HMtoM(trim(lcStartExHM))

lcEndEXHM = leftto(mline(toAnalysis.UsedHM, 2), "-")
lnEndEXHM = HMtoM(trim(lcEndEXHM))

*** No lunch break 
if empty(lcStartExHM) and empty(lcEndEXHM)
	store null to loSched, loBizPlanDt
	return llOk 
endif 

*** Not between lunch time interval  
if !between(lnStartExHM, lnSchedStartExHM, lnSchedEndExHM)
	store null to loSched, loBizPlanDt
	return llOk 
endif

*** Is the lunch break taken higher than the minimum 
* defined by the union 
if lnEndEXHM > lnStartExHM ;
	and castx(lnEndEXHM-lnStartEXHM,"N") < val(lcLunchBreakMM)

	*** Override the end of lunch 
	lnEndEXHM = lnStartExHM + val(lcLunchBreakMM) 
	lcEndEXHM = MtoHM(lnEndEXHM, "/TIME")
	lcEndEXHM = chrtran(lcEndEXHM, ":", "")
	toAnalysis.UsedHM = mline(toAnalysis.UsedHM,1)+CRLF+;
			stuff(mline(toAnalysis.UsedHM,2), 1, 4, lcEndEXHM)

	*** debug 
	this.WriteLog("txtInOut_Valid_MinimumLunchBreak() - " + ;
			"lcEndEXHM = " + transform(lcEndEXHM))
	this.WriteLog("txtInOut_Valid_MinimumLunchBreak() - " + ;
			"UsedHM = " + transform(toAnalysis.UsedHM))
	
	return !llOk 
endif 

store null to loSched, loBizPlanDt

return llOk 
endproc

*=========================================================
procedure txtInOut_Valid_Custom(poThis, loAnalysis)
*** Called from txtInOUt_Valid. Return .f. to suppress 
*   standard alert message. You can still display your own
*
local llOk, lcLunchBreakMM
local lcComm1

store "" to lcLunchBreakMM 
llOk = .t. 

if trim(gcLang) = "E" 
	lcComm1 = "The minimum time required for the lunch break "+;
				"has not been respected: " 
else
	lcComm1 = "Le temps minimum requis pour la pause du dîner "+;
					"n'a pas été respecté: "
endif 

*** set step on 
*** Store the values after they have been rounded 
if empty(loAnalysis.UsedHM)
	return llOk 
endif 

*** Validate minimum amount of time taken for lunch break
llOk = iif(llOk, ;
	this.txtInOut_Valid_MinimumLunchBreak(loAnalysis,@lcLunchBreakMM),.f.)
if !llOk 
	alert(name(this.PersId)+CRLF+lcComm1+;
				trim(lcLunchBreakMM)+ " minutes", 0,;
				"\!\?\<OK")
endif 

replace TT_INOUT with trim(loAnalysis.UsedHM) 

*** Save the current TT_INOUT
* and use it fill hours for EXPENCES 
this.cPrevINOUT = loAnalysis.UsedHM

*** do not show the generic messages 
return iif(llOk, .f., llOk)
endproc

*======================================================================
procedure TxtEUnit_valid(poThis)
*** 
local lcComm1

if gcLang = "E"
	lcComm1 = "The amount should be greater than zero."
else
	lcComm1 = "La quantité doit être supérieure à zéro."
endif

if !used("vNEW")
	return 
endif 	

if !(type("poThis.Parent.txtEUnit")="O")
	return 
endif 

*** Disallow negative amounts 
if vNEW.TT__EUNIT < 0.0 
	alert(lcComm1,0,"\!\<Ok")

	poThis.Parent.txtEUnit.Value = 1.0
	poThis.Parent.txtEUnit.Refresh()
endif 

return   

*======================================================================
procedure TxtExpense_valid(poThis)
*** 
local llExpenseAccount, lcInOut, lcComm1
store "" to lcInOut

if gcLang = "E"
	lcComm1 = "The maximum value has been reached: " 
else
	lcComm1 = "La valeur maximale a été atteinte: " 
endif

if !used("vNEW")
	return 
endif 	

if !(type("poThis.Parent.txtExpense")="O")
	return 
endif 

*** Allow to bypass if the rate = 0.0
if this.nMaxExpense != 0.0 ;
and vNEW.TT__EURATE > this.nMaxExpense
	alert(lcComm1+lstr(this.nMaxExpense,2),0,"\!\<Ok")

	poThis.Parent.txtExpense.Value = this.nMaxExpense
	poThis.Parent.txtExpense.Refresh()
endif

if empty(vNEW.TT_INOUT)
	llExpenseAccount=ToLogical(tbleval("PRIME",vNEW.TT_OPT,"TBLC6"))
	if llExpenseAccount 

		*** Check see if the TT_INOUT has been 
		* entered for the same day: if Yes use it.
		=this.GetTodayInOut(@lcInOut)
		if !empty(lcInOut)
			replace TT_INOUT with lcInOut in vNEW
		else
			if empty(this.cPrevINOUT)
				this.cPrevINOUT = "08:30-11:30" + CRLF +"14:00-17:30"
			endif

			replace TT_INOUT with this.cPrevINOUT in vNEW
		endif 

	endif 
endif 

if dodefault()
	this.oThisform.Pgf1.Page1.cmdGo.EnableDisable()
	this.oThisform.Refresh()
endif 

return   

*======================================================================
procedure cmdExpense_valid(poThis)
*** Signs weekly expenses by updating the fields:
*		- TT_SIGNDTM with datetimex()
*		- TT_SIGNBY  with gcUser
*		
local llOk, loRS, lcComm1

if trim(gcLang) = "F" 
	lcComm1 = "Pour la période: "
	lcComm2 = " -dépenses ont été signés avec succès."  
else
	lcComm1 = "For the period: "
	lcComm2 = " -expenses have been successfully signed." 
endif 

llOk = .f. 
loRS = this.oBiz.PostWeeklyExpenses(this)	
if isnull(loRS)
	return llOk 
endif 
	
llOk = loRS.lOk 
if llOk 
	lcPayPeriod = dtoc(loRS.dFromDt)+" - "+dtoc(loRS.dThruDt)

	alert( lcComm1 + lcPayPeriod + chr(13) + ;
			lstr(loRS.nCount,0) + lcComm2, ;
			0, "\!\?\<OK")
	
	this.oThisform.release()
endif 

return llOk

*=========================================================
procedure ValidateSheet ( llSigning )
*** Does the end validation before singing T/S.
*
local llDefault 
local lnFLEXHRSTAKEN, lnMOBSEBTAKENCNT
local lcComm1, lcComm2, lcComm3, lcComm4

if gcLang = "F" 
	lcComm1 = "Le nombre maximal de jours par année autorisé a été atteint : "
	lcComm2 = "S'il vous plaît corriger votre feuille de temps!"
	lcComm3 = "Le nombre maximal d'heures par période autorisé a été atteint : "
	lcComm4 = "Le nombre maximal d'absences par année autorisé a été atteint : "
else
	lcComm1 = "The maximum number of days/year allowed has been reached: "
	lcComm2 = "Please fix your timesheet!"
	lcComm3 = "The maximum number of hours/period allowed has been reached: "
	lcComm4 = "The maximum number of allowed absences per year has been reached: "
endif 

llDefault =dodefault(llSigning)
if llDefault
	*** Get number of hours taken in FLEX bank during one period
	lnFLEXHRSTAKEN =this.GetFLEXHRSByPersId(this.cFLEXCOUNTER,this.PersId)
	if lnFLEXHRSTAKEN > (this.nHRSPDY + this.nHRSPDYRND)
		alert(lcComm3 + transform((lnFLEXHRSTAKEN/this.nHRSPDY), "9") + ;
				CRLF+lcComm2,0,"\!\?\<OK")
			
		llDefault = .f.
	endif 

	*** Get number of paid MOBILE absences taken during one period
	if llDefault 
		lnMOBSEBTAKENCNT =this.GetTimeOffTakenByPersId(this.cMOBSEBCNT,this.PersId)
		if lnMOBSEBTAKENCNT > this.nTIMEOFFMAX
			alert(lcComm4 + transform(this.nTIMEOFFMAX, "999") + ;
					CRLF+lcComm2,0,"\!\?\<OK")

			llDefault = .f.
		endif 
	endif 
endif 

return llDefault

*=========================================================
#define HELPER_METHODS 
*=========================================================
procedure IsCodeAllowedInTS( tcCodeId, tcType )
*** This method validates that the absence can be taken 
* by the employee based on some counter. Please see: 
* GetTimeOffTakenByPersId() 
*
local llCodeIN 

if empty(this.cInList) and empty(this.cOutList)
	return llCodeIN
endif 

if empty(tcCodeId)
	return llCodeIN
endif 

*** InList is not empty 
if !empty(this.cInList)
	=alines(aInList,this.cInList,4,",")
	
	for lnI = 1 to alen(aInList,1)
		if tcCodeId = alltrim(aInList(lnI))
			llCodeIN = .t.
			exit 
		endif 
	next 
endif 
				
*** OutList is not empty 
if !llCodeIN and !empty(this.cOutList)
	=alines(aOutList,this.cOutList,4,",")
	
	for lnJ = 1 to alen(aOutList,1)
		if tcCodeId = alltrim(aOutList(lnJ))
			llCodeIN = .t.
			exit 
		endif 
	next
	
	llCodeIN = ((llCodeIN=.t.) and tcType = "A")
endif

store null to aInList, aOutList
return llCodeIN

*=========================================================
procedure GetTimeOffTakenByPersId(tcCounter, tnPersId)
*** This method uses the counter from cMOBSEBCNT to get 
* the number of how many times an employee has taken 
* some time off. It uses both tables [TIMEDT] and 
* [TIMETMP].
*
* The counter in this.cMOBSEBCNT needs to exist.
*
local lnSelect, lcEXPR, lcSql, loCursor
local lnTotCNT, lnTimeDT_CNT, lnTimeTMP_CNT 
local llInList, llOutList

store 0 to lnTotCNT, lnTimeDT_CNT, lnTimeTMP_CNT
store "" to lcEXPR, lcSql
store null to loCursor

if empty(tcCounter) or empty(tnPersId)
	return 
endif 

*** set step on
ldStartDT = this.dSFPQYRStDT
if ldStartDT < this.dSEBEffDT
	ldStartDT = this.dSEBEffDT
endif 
ldEndDT = this.dSFPQYREdDT
	
lnSelect = select()

*** set step on 
lcEXPR = TCNTVAL("EXPR", tcCounter)
if empty(lcEXPR)
	select (lnSelect )
	return lnTotCNT
endif 	

*** Set the codes string for validation in T/S. 
llOutList= (atw("not inlist(T_OPT",lcEXPR) > 0)
llInList = (atw("inlist(T_OPT",lcEXPR) > 0)
if llInList
	this.cINLIST = extract(lcEXPR,"inlist(T_OPT,", ")")
	this.cINLIST = chrtran(this.cINLIST,"'","")
endif 
if llOutList
	this.cOUTLIST = extract(lcEXPR,"not inlist(T_OPT,", ")")
	this.cOUTLIST = chrtran(this.cOUTLIST,"'","")
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
lcSql = strtran(lcSql, "[nPERSID]", transform(tnPersId, "999999"))
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
lcSql = strtran(lcSql, "[nPERSID]", transform(tnPersId, "999999"))
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
procedure GetFLEXHRSByPersId(tcCounter, tnPersId)
*** This method uses the counter this.cFLEXCOUNTER 
* to get the to total number of hours from [TIMEDT]
* and [TIMETMP].
*
* The counter this.cFLEXCOUNTER needs to exist.
*
local lnSelect, lcEXPR, lcSql, lnResult 
local lnTotHOURS, lnTimeDT_HRS, lnTimeTMP_HRS 

store 0 to lnResult 
store 0 to lnTotHOURS, lnTimeDT_HRS, lnTimeTMP_HRS
store "" to lcEXPR, lcSql

if empty(tcCounter) or empty(tnPersId)
	return lnTotHOURS
endif 

*** set step on
ldStartDT = this.dPeriodStDT
if ldStartDT < this.dSEBEffDT
	ldStartDT = this.dSEBEffDT
endif 
ldEndDT   = this.dPeriodEdDT

lnSelect = select()

*** Get posted timesheet transactions table [TIMEDT]
lnTimeDT_HRS = TCNTVAL("TOT", tcCounter, tnPersId, ldStartDT, ldEndDT)

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
	WHERE TT_PERSID = [nPERSID] 
	AND ( TT_EFFDT BETWEEN '[dSTARTDT]' AND '[dENDDT]' )
	AND TT_SOURCE = 'F'
	AND TT_POSTBY = ''
	AND [TIMETMP_CNT]
endtext 
lcSql = strtran(lcSql, "[nPERSID]", transform(tnPersId, "999999"))
lcSql = strtran(lcSql, "[dSTARTDT]", dtoc(ldStartDT))
lcSql = strtran(lcSql, "[dENDDT]", dtoc(ldEndDT))
lcSql = strtran(lcSql, "[TIMETMP_CNT]", alltrim(lcEXPR))

loCursor = goDataMgr.SQLexec("/CURSOR=qTTDT", set("datasession"), ;
						goDataMgr.ConnectionHandle, lcSql)
if isnull(loCursor)
	return lnTotHOURS
endif 

lnTimeTMP_HRS = nvl(qTTDT.TOTHRS, 0)

use in select("qTTDT")

if (ldEndDT - date()) = 0
	lnTotHOURS = lnTimeTMP_HRS
else
	lnTotHOURS = lnTimeDT_HRS + lnTimeTMP_HRS
endif 

select( lnSelect )
return lnTotHOURS

*=========================================================
procedure CreateScheduleCursor()
***
*
with this 

if !empty(.csrBankHrs)
	use in select(.csrBankHrs)

	create cursor (.csrBankHrs) ;
		(cHours C(5), cDescr C(30), nHours N(9,4), ;
		 cInOut C(25), cSched C(25) )
endif 					

endwith 

return 

*=========================================================
procedure FillInScheduleList( tcTblId )
***
local loBizPlanDt, lcWkHours, lnN, lcDays
local lcSchedType, lnSelect, ldEffdt, lcDAYTYPE
local lcStartHM, lcStartExHM
private loSched, loRndSchema, lcAMPM 

store null to loBizPlanDt
store 0 to lnWkHours, lnN
store "" to lcDays, lcInOut, lcAMPM, lcDAYTYPE
store "" to lcStartHM, lcStartExHM
store {} to ldEffdt 

with this

if empty(.PersId)
	return 
endif 

do case
case vNew.TT_day = 0
	ldEffdt = {}
case between(vNew.TT_Day, 1, 7)
	ldEffdt = this.dFromDt + vNew.TT_Day - 1
case between(vNew.TT_Day, 9, 15)
	ldEffdt = this.dFromDt + vNew.TT_Day - 2
endcase 

*** set step on 
loBizPlanDt = this.oBizmgr.GetBiz("PLANDT")
loSched = loBizPlandt.GetSched("/SCHED /OBJECT", ;
			 this.PersId, ldEffdt, ldEffdt)

if isnull(loSched) 
	return 
endif 	

lnSelect = select()
this.cSchedId = trim(loSched.BaseSchedId) 

*** set step on 
if reccount(.csrBankHrs) > 0 
	delete all in (.csrBankHrs)
	go top in (.csrBankHrs)
	this.oBankHours.DisplayValue = "" 
	this.oBankHours.Refresh()
endif 	

for lnN = 1 to 4 
	store "" to lcDays, lcInOut
	store 0 to lnWkHours
	do case 
	case lnN = 1
		if empty(loSched.Type)
		
			*** set step on 
			lcDays = iif(gcLang="E", "No schedule", "Pas de calendrier.")
			lnN = 5 
		else 
			lcDays = iif(gcLang="E", "(Unknown)", "(Aucun)")
		endif 
		lnWkHours = 0
	case lnN = 2 
		lcDays = iif(gcLang="E", "Full day", "Journée complète") 
		lnWkHours = loSched.Hours
		if !empty(loSched.EndExHM) and !empty(loSched.EndExHM) 
			lcInOut = trim(loSched.StartHM)+"-"+trim(loSched.StartExHM)+;
			   	CRLF + trim(loSched.EndExHM)+"-"+trim(loSched.EndHM)

			*** For expenses 
			if empty(this.cPrevINOUT)
				this.cPrevINOUT = lcInOut 
			endif 	

		else
			lcInOut = trim(loSched.StartHM)+"-"+trim(loSched.EndHM)
		endif
	case lnN = 3

		lcDays = "AM"

		*** Fill from daytype table 
		if type("tcTblId") = "C" and !empty(tcTblId)
			*** set step on 
			lcDAYTYPE = tbleval("ABSENCE", Optlist.TBLID, "TBLC7")
			if !empty(lcDAYTYPE)
				lcAMPM = tbleval("_DAYTYPE", lcDAYTYPE, "TBLC7")
				lcInOut = lcAMPM

				*** Get hours interval 
				lcStartHM = leftto(lcInOut, "-")
				lcStartExHM = rightFrom(lcInOut, "-")
				lnWkHours = HMtoH(trim(lcStartExHM))-HMtoH(trim(lcStartHM))
			endif 
		endif 

		*** If empty use the schedule  
		if (empty(lcInOut) or empty(lnWkHours))
			if !empty(loSched.StartExHM)
				lcAMPM = trim(loSched.StartHM)+"-"+trim(loSched.StartExHM)
			   lcInOut = trim(loSched.StartHM)+"-"+trim(loSched.StartExHM)
				lnWkHours = HMtoH(trim(loSched.StartExHM)) ;
						   	- HMtoH(trim(loSched.StartHM))
			endif 
		endif 
			
	case lnN = 4

		lcDays = "PM"

		*** Fill from daytype table 
		if type("tcTblId") = "C" and !empty(tcTblId)
			lcDAYTYPE = tbleval("ABSENCE", Optlist.TBLID, "TBLC7")
			if !empty(lcDAYTYPE)
				lcAMPM = tbleval("_DAYTYPE", lcDAYTYPE, "TBLC8")
				lcInOut = lcAMPM

				*** Get hours interval 
				lcStartHM = leftto(lcInOut, "-")
				lcStartExHM = rightFrom(lcInOut, "-")
				lnWkHours = HMtoH(trim(lcStartExHM))-HMtoH(trim(lcStartHM))
			endif 
		endif 
		  
		*** If empty use the schedule  
		if (empty(lcInOut) or empty(lnWkHours))
			if !empty(loSched.EndExHM)
				lcAMPM = trim(loSched.EndExHM)+"-"+trim(loSched.EndHM)
			   lcInOut = trim(loSched.EndExHM)+"-"+trim(loSched.EndHM)		  
				lnWkHours = HMtoH(trim(loSched.EndHM)) ;
						   	- HMtoH(trim(loSched.EndExHM))
			endif 
		endif 

	endcase 

	if !empty(lcDays)
		lcWkHours = HtoHM(lnWkHours, -1)
		lcInOut = chrtran(lcInOut, ":", "")

		insert into (.csrBankHrs) ( cHours, cDescr, nHours, cInOut, cSched);
			values (lcWkHours, lcDays, lnWkHours, lcInOut, lcAMPM)
	endif 
next 


*** wait window "this.cPrevINOUT = " + alltrim(this.cPrevINOUT)
*** select (.csrBankHrs)
*** browse normal 
endwith

select(lnSelect)
store null to loBizPlanDt, loSched

return

*======================================================================
procedure InitBankWithdrawal()
***
*
local lnDifferential, lcVarPlanId

if !used("qJobhist")
	return 
endif 

lnDifferential = MoreC(qJobhist.H_MORE, "DIFFERENT")
lcVarPlanId = alltrim(upper(qJobhist.H_APLAN9))

with this 

.cWithdrawDeficitFromBankids= "9"
.cDepositSurplusIntoBankids = "9"

.cBank9DepositOpt = "DEP9"

if empty(qJobhist.H_APLAN9)
	return 
endif 	

do case 
case inlist(lcVarPlanId, "FLEX")

	if !empty(tbleval("ABSENCE", "CFLEXI"))
		.cBank9WithdrawalOpt = "CFLEXI"
	endif 

case inlist(lcVarPlanId, "VARDIR","VARELU","VARAPMCP")

	if !empty(tbleval("ABSENCE", "VARIA"))
		.cBank9WithdrawalOpt = "VARIA"
	endif
	
case inlist(lcVarPlanId, "VAREMP")

	if !empty(tbleval("ABSENCE", "VAREMP"))
		.cBank9WithdrawalOpt = "VAREMP"
	endif

otherwise 

	*** Default 
	.cBank9WithdrawalOpt = ""

endcase 

.WriteLog("InitBankWithdrawal() = " + ;
		"cBank9WithdrawalOpt = " + ;
			transform(.cBank9WithdrawalOpt))

endwith 
return 

*======================================================================
procedure ValidateSheet_GetExtendedHours(tcSchedId,tdEffDt,tnRegHours)
*** The procedure return the hour's difference between the scheduler
* "general" and "detail" tabs. 
* For example, the "general" tab is configured to work 7 hours per day 
* while the "detail" tab shows that the actual number of hours 
* that the employee needs to work is 5.45.
* The employee is paid for 7 hours and not 5.45. S.G.
*
local lnHMtoHM
local loSched, lnSelect

lnSelect = select()

store null to loSched
store 0 to lnHMtoHM

with this 

*** set step on 
loSched = .oBizSched.WkCal("OBJECT", tcSchedId, tdEffDt)
if isnull(loSched)
	return lnHMtoHM 
endif 

if loSched.AVGHRSPDY > 0 
	lnHMtoHM = loSched.AVGHRSPDY-tnRegHours
endif 

*** Debug 
.WriteLog("ValidateSheet_GetExtendedHours() - " + ;
		"AVGHRSPDY, tnRegHours, lnHMtoHM = " + ;
		transform(loSched.AVGHRSPDY) + ",  " + ;
		transform(tnRegHours) + ",  " + ;
		transform(lnHMtoHM))
endwith 

store null to loSched
select(lnSelect)

return lnHMtoHM

*======================================================================
procedure GetTodayInOut( tcInOut )
*** The expenses do enter the hours interval 
* We need to locate the TT_EFFDT for a given date 
* and use this value in TT_INOUT for expenses records
*
local lnSelect, lnRecNo
store 0 to lnRecNo

if !used("vTimetmp")
	return
endif 

lnSelect = select()
	
select vTimetmp 
count to lnRecNo for !deleted()
if lnRecNo <= 0
	select( lnSelect )
	return 
endif 	

select vTimetmp 
go top in vTimetmp 
locate for vNEW.TT_EFFDT = vTimetmp.TT_EFFDT and !eof()
if found()
	tcInOut = alltrim(nvl(vTimetmp.TT_INOUT, ""))
endif 

if lnRecNo <> 0
	select vTimetmp 
	goto lnRecNo
else 
	go top in vTimetmp 
endif 

select( lnSelect )

return 

*======================================================================
procedure getKilometersYTD()
*** This procedure counts the total number of kilometers that 
* has been entered in the timehseet through expense account 
* by an employee. The method uses a counter to do this.
*
local loCsrTemp 
local lcPayGRP, lcPayNo, lcLastPayNo, ldFrom, ldThru
local lcCNTWhere, lcWhere 

*** set step on 
loCsrTemp = null 
store "" to lcPayGRP, lcPayNo, lcLastPayNo, ldFrom, ldThru
store "" to lcCNTWhere, lcWhere 

if !used("qJobhist")
	return 
endif 	

with this 

*** set step on 
lnPersId = this.PersId  
lcPayGRP = trim(qJobhist.H_PAYGRP)
lcPayNo = lcPayGRP + left(dtos(date()), 4) + "01"

ldFrom = .oBizPayno.GetValueById(lcPayNo, "PN_STARTDT")

lcLastPayNo = .oBizPayno.GetLastPayNo(left(lcPayNo, 8), "/REG")
ldThru = .oBizPayno.GetValueById(lcLastPayNo, "PN_ENDDT")

lcFields = "COALESCE(sum(T__EUNIT),0) AS TOTKMS"
lcCNTWhere = TCNTVAL("EXPR$","KM")

if empty(lcCNTWhere)
	store null to loCsrTemp
	return 
endif 	

lcWhere = Makefor("", lcCNTWhere, ;
				"T_PERSID=" + tochar(lnPersId, "/DTOS/QUOTES"), ;
				iif(empty(ldFrom), "", "T_EFFDT>=" + ;
						tochar(ldFrom, "/SQL/QUOTES")), ;
				iif(empty(ldThru), "", "T_EFFDT<=" + ;
						tochar(ldThru, "/SQL/QUOTES")))

loCsrTemp = .oBizTimedt.GetListByRange (;
				"/CURSOR=qCntKms", lcFields, ;
				lcWhere, .f., "", "", "", "", ;
				"", "")

if !used("qCntKms")
	return 
endif 

.nKilometerYTD = round(nvl(qCntKms.TOTKMS,0),2)

store null to loBizPayno, loBizTimeDt, loCsrTemp
use in select("qCntKms")

endwith 

return
endproc


*======================================================================
procedure getUnionLeaveYTD()
*** This procedure counts the total number of kilometers that 
* has been entered in the timehseet through expense account 
* by an employee. The method uses a counter to do this.
*
local loCsrTemp 
local lcPayGRP, lcPayNo, lcLastPayNo, ldFrom, ldThru
local lcCNTWhere, lcWhere 

*** set step on 
loCsrTemp = null 
store "" to lcPayGRP, lcPayNo, lcLastPayNo, ldFrom, ldThru
store "" to lcCNTWhere, lcWhere 

if !used("qJobhist")
	return 
endif 	

with this 

*** set step on 
lcPayGRP = trim(qJobhist.H_PAYGRP)
lcPayNo = lcPayGRP + left(dtos(date()), 4) + "01"

ldFrom = .oBizPayno.GetValueById(lcPayNo, "PN_STARTDT")

lcLastPayNo = .oBizPayno.GetLastPayNo(left(lcPayNo, 8), "/REG")
ldThru = .oBizPayno.GetValueById(lcLastPayNo, "PN_ENDDT")

lcFields = "SUM(COALESCE(T_HOURS,0)) AS TOTHOURS"
do case 
case "SEB"$qJobhist.H_UNION 
	lcCNTWhere = TCNTVAL("EXPR$", "SYNSEB")
case "APMCP"$qJobhist.H_UNION 
	lcCNTWhere = TCNTVAL("EXPR$", "APMCPSYN")
endcase 

if empty(lcCNTWhere)
	store null to loCsrTemp
	return 
endif 	

*** set step on 
lcWhere = Makefor("", lcCNTWhere, ;
				iif(empty(ldFrom), "", "T_EFFDT>=" + ;
						tochar(ldFrom, "/SQL/QUOTES")), ;
				iif(empty(ldThru), "", "T_EFFDT<=" + ;
						tochar(ldThru, "/SQL/QUOTES")))

loCsrTemp = .oBizTimedt.GetListByRange (;
				"/CURSOR=qUnLeave", lcFields, ;
				lcWhere, .f., "", "", "", "", ;
				"", "")

if !used("qUnLeave")
	return 
endif 

.nUnionLeaveYTD = round(nvl(qUnLeave.TOTHOURS,0),2)

store null to loBizPayno, loBizTimeDt, loCsrTemp
use in select("qUnLeave")

endwith 

return
endproc

*======================================================================
procedure cmdLog_GetLogFile( poThis, pnLogIndex)
*** DEBUG procedure 
local lcLogFileName 
local array laPath[1] 

lcLogFileName = this.cLogPath
do case 
case pnLogIndex = 1 
	lcLogFileName = lcLogFileName + ;
			"_bhvtimetmp1_custom.log" 
case pnLogIndex = 2 
	lcLogFileName = lcLogFileName + ;
			"_biztimetmp1_custom.log" 
case pnLogIndex = 3 
	lcLogFileName = lcLogFileName + ;
			"_cvacationbank.log" 
case pnLogIndex = 4
	lcLogFileName = lcLogFileName + ;
			"_csickbank.log" 
otherwise 
	store "" to lcLogFileName
endcase 

with this 

.WriteLog("lcLogFileName = " + ;
		transform(lcLogFileName))

if !file(lcLogFileName)
	lcLogFileName = ""
endif 

endwith 

return lcLogFileName 

*=========================================================
procedure WriteLog(pcLog)
*** DEBUG procedure 
local lcLogfile, lcStartLog 

if !this._DEBUG or empty(this.cLogPath) 
	return 
endif 
	
pcLog = trim(pcLog) + chr(13) + chr(10) 
lcLogfile = this.cLogPath + "_" + ;
	leftto(lower(trim(transform(program()))),".")+".log"

lcStartLog=this.cLogPath+this.cLogStatus
if file(lcStartLog)
	=strtofile(transform(datetime()) + " - " + ;
			trim(pcLog), lcLogfile, .t.)
endif 

return


*=========================================================
procedure CreateCustomPayObject()
*** Object factory for SFPQ pay period object 

local loR

loR = createobject("empty")

*** Id's 
addproperty(loR, "cTblId", "")
addproperty(loR, "cTblName", "")

*** Date internal
addproperty(loR, "dStartDt", {})
addproperty(loR, "dThruDt", {})

*** Counter for transactions 
addproperty(loR, "cTCNT", "")
addproperty(loR, "cTSTCNT", "")

*** Year To Date deposit 
addproperty(loR, "nSchedHoursToWork", 0)
addproperty(loR, "nSchedHolidayHours", 0)

return loR

*=========================================================
procedure main()
***
*
*** set step on 
loRS = this.CreateCustomPayObject()
this.GetCustomPayPeriod(loRS)
this.GetScheduledWKHours(loRS)

return
endproc

*=========================================================
procedure GetCustomPayPeriod(toRS)
*** This f(x) gets the current SFPQ period 
* start & end dates. It also gets SFPQ period start 
* and end of the year.
* While the 1st set of dates are used by counter TFLEXI,
* the 2nd set are used by the counter MOBSEBHR.
*
local lnSelect, loBizTbl, loCursor 
store null to loBizTbl, loCursor

lnSelect = select()

if isnull(toRS)
	return 
endif 
	
loBizTbl = GetBiz("TBL")
loCursor = loBizTbl.GetList("/cursor=qTbl", "_PERIOD", ;
				"TBLID, TBLNAM" + gcLang + " As TBLNAME, " + ;
				"TBLC7, TBLC8, TBLM1")

if isnull(loCursor)
	return 
endif 

select qTbl 
go top in qTbl 
scan
	
	if !empty(qTbl.TBLC7) and !empty(qTbl.TBLC8)

		toRS.cTblId = qTbl.TBLID 
		toRS.cTblName = qTbl.TBLNAME
		toRS.dStartDt = todate(qTbl.TBLC7)
		toRS.dThruDt = todate(qTbl.TBLC8)
		toRS.cTCNT = alltrim(qTbl.TBLM1)

		exit 
	endif
		
endscan

use in select("qTbl")
store null to loCursor 


*** SET SFPQ periods 
loCursor = loBizTbl.GetList("/cursor=qTFlex", "_FLEXIBLE", ;
				"TBLSORT AS PeriodId, " + ;	
				"rtrim(TBLC1) As TBLC1,rtrim(TBLC2) As TBLC2")

if isnull(toRS)
	return 
endif 

*** 6 MOBSEBHR / YEAR 
*** Get first start of SFPQ period 
go top in qTFlex 
this.dSFPQYRStDT = ctod(left(trim(TBLC1),4) + "/" + ;
				substr(trim(TBLC1),5,2) + "/" + ;
				substr(trim(TBLC1),7,2))

*** Then take the last of SFPQ period 
go bottom in qTFlex 
this.dSFPQYREdDT = ctod(left(trim(TBLC2),4) + "/" + ;
				substr(trim(TBLC2),5,2) + "/" + ;
				substr(trim(TBLC2),7,2))


*** 1 TFLEXI / SFPQ PERIOD  
go top in qTFlex 
scan
	if between(dtos(date()), trim(TBLC1), trim(TBLC2))

		this.dPeriodStDT = ctod(left(trim(TBLC1),4) + "/" + ;
						substr(trim(TBLC1),5,2) + "/" + ;
						substr(trim(TBLC1),7,2))
							
		this.dPeriodEdDT = ctod(left(trim(TBLC2),4) + "/" + ;
						substr(trim(TBLC2),5,2) + "/" + ;
						substr(trim(TBLC2),7,2))
		
		exit
	endif 	 
endscan 

use in select("qTFlex")
store null to loCursor 
select (lnSelect)

return 
endproc

*=========================================================
procedure GetScheduledWKHours(toRS)
***
*
local loBizJobhist, lcWFrom, lcWThru
store null to loBizJobhist
store "" to lcWFrom, lcWThru

*** set step on 
loBizJobhist = this.oBizmgr.GetBiz("JOBHIST")
if isnull(loBizJobhist) 
	return 
endif 	

toRS.nSchedHoursToWork = loBizJobhist.GetTotalHours( ;
		"/HOURS/INCLINAC/INCLLTD/INCLALD", ;
		this.PersId, toRS.dStartDt, toRS.dThruDt)	

toRS.nSchedHolidayHours = loBizJobhist.GetTotalHours( ;
		"/FHOURS/INCLINAC/INCLLTD/INCLALD", ;
		this.PersId, toRS.dStartDt, toRS.dThruDt)	


lcWFrom = iif(empty(toRS.dStartDt), "", ;
		"TT_EFFDT={} or TT_EFFDT>=" + ;
			tochar(toRS.dStartDt, "/SQL/QUOTES"))

lcWThru = iif(empty(toRS.dThruDt), "", ;
		"TT_EFFDT<=" + tochar(toRS.dThruDt, "/SQL/QUOTES"))


*** Strip counter to use timetmp  
lcEXPR = TCNTVAL("EXPR", toRS.cTCNT)
if !empty(lcEXPR)
	lcEXPR = strtran(lcEXPR, "and T_UNITCD='H'","")
	lcEXPR = strtran(lcEXPR, "T_","TT_")
	lcEXPR = lcEXPR + makefor(lcEXPR, lcWFrom, lcWThru) 
endif
toRS.cTSTCNT = lcEXPR

return
endproc

*=========================================================
#define BASE_CLASS_OVERRIDES
*** Only the most inportant methods are shown here 
*=========================================================
procedure ValidateSheet_CheckOneDay(ldEffdt,lnCount,lnRegHrs)
*** We need to credit a number of hours for each day 
* at the signature. For example, an employee works 5.45 hours
* but he gets paid 7 hours. 
* We need to generate a transaction with an absence code
* for the difference in hours on daily basis. - S.G.
*
local lnSelect, lcAbsenceId, lnDeficitHours 
local llERROR, lnEntryMade 

store "" to lcAbsenceId
store 0 to lnDeficitHours, lnEntryMade

if !this.lSigned
	return dodefault(ldEffdt, lnCount, lnRegHrs)
endif

lnSelect = select()

*** Get the absence code from TBL
lcAbsenceId = tbleval("_SCHEDULE",qJobhist.H_SCHEDID,"TBLC7")
if empty(lcAbsenceId) 
	select (lnSelect)
	return dodefault(ldEffdt,lnCount,lnRegHrs)
endif 

*** Was the entry for the day already done?
select vTimetmp
count to lnEntryMade for ;
		TT_EFFDT = ldEffdt and TT_OPT = lcAbsenceId

*** Filter out the dates 
select TT_OPT, TT_EFFDT ;
from vTimetmp with (buffering=.t.) ;
where TT_EFFDT = ldEffdt and TT_OPT<>'**' ;
into cursor qTTmp 

*** Several validations 
llERROR = iif(!llERROR, (lnEntryMade > 0), .t.)
llERROR = iif(!llERROR, (reccount("qTTmp")<1), .t.)
llERROR = iif(!llERROR, (inlist(TT_OPT,lcAbsenceId)), .t.)
llERROR = iif(!llERROR, empty(qJobhist.H_SCHEDID), .t.)

if llERROR 
	this.WriteLog("ValidateSheet_CheckOneDay() - ldEffdt = "+;
			transform(ldEffdt))

	use in select("qTTmp")
	select (lnSelect)

	return dodefault(ldEffdt,lnCount,lnRegHrs)
endif 

*** set step on 
with this 

select vTimeTmp 
locate for vTimeTmp.TT_EFFDT=qTTmp.TT_EFFDT and !eof()
if found()
	lnDeficitHours = .ValidateSheet_GetExtendedHours( ;
					qJobhist.H_SCHEDID, vTimeTmp.TT_EFFDT, lnRegHrs)

	if !empty(lnDeficitHours)
		.AddTrans("/SAVE", vTimetmp.TT_EFFDT, lnDeficitHours, ;
				lcAbsenceId, "A")
	endif 
endif 

endwith 

use in select("qTTmp")

select (lnSelect)
return 

*=========================================================
*** Add the expenses to the timesheet group total 
*
procedure Calc_EmpTot()
*** 
*
local lnN, lnCurrHours, lnCurrMinutes, lnCurrUnits, lnRecno
local lnRegHours, lnOTHours
local lnPastHours, lnPastMinutes, lnPastUnits
local lnPaidHours, lnPaidMinutes
local lnBankedHours, lnBankedMinutes
local lnUnpaidHours, lnExpenseAmt
local lcFromDt, lnColumns, loEngine
local llExpenseAccount

if isnull(this.oLstEmpTot)
	*** Form not yet instantiated
	return
endif

select vTimetmp
lnRecno = iif(eof(), 0, recno())
lnColumns = this.oLstEmpTot.ColumnCount

dimension this.aEMPTOT[10, lnColumns]
this.aEmptot = ""

*--------
** Applies to this entire procedure
*loEngine = Setto("70", "EngineBehavior")
*set enginebehavior 70	&& Applies to this entire procedure
*--------

*** Cursor of current hours only
lcFromDt = tochar(this.dFromDt, "/DTOS/QUOTES")

set enginebehavior 70
SELECT TT_OPT, ;
	 left(alltrim(tt_showopt), 26) As TT_SHOWOPT, ;
	 tt_type, ;
	 sum(round(tt_hours*60,0)) as tt_minutes, ;
	 00000.0000 as TT_HOURS, ;
	 (TT__EUNIT*TT__EURATE) AS TT_ExpAmt, ; 
	 int((todate(tt_effdt) - &lcFromDt)/7) as TT_WEEK ;
FROM vTimetmp ;
	with (Buffering=.t.) ;
	where TT_EFFDT >= this.dFromdt ;
	  and TT_TYPE <> "*" ;
	GROUP BY tt_opt, tt_week, tt_type ;
	INTO CURSOR qCURRTOT readwrite
set enginebehavior 80

*** Total current hours (exclude premiums)
replace all TT_HOURS with TT_MINUTES / 60
calculate sum(tt_minutes) to lnCurrMinutes ;
		for inlist(TT_TYPE, "H", "A")
lnCurrHours = int(lnCurrMinutes) / 60

*** Total current "units" (premiums)
calculate sum(tt_hours) to lnCurrUnits ;
		for TT_TYPE="P"


*** Cursor of past hours only
set enginebehavior 70
SELECT tt_opt, ;
		 left(alltrim(tt_showopt), 26) As TT_SHOWOPT, ;
		 tt_type, ;
		 sum(round(tt_hours*60,0)) as tt_minutes, ;
		 00000.0000 as tt_hours ;
		FROM vTimetmp ;
		with (Buffering=.t.) ;
		where TT_EFFDT < this.dFromdt ;
		  and TT_TYPE <> "*" ;
		GROUP BY tt_opt ;
		INTO CURSOR qPASTTOT readwrite
set enginebehavior 80

*** Total past hours (exclude premiums)
replace all TT_HOURS with TT_MINUTES / 60
calculate sum(tt_minutes) to lnPastMinutes ;
		for inlist(TT_TYPE, "H", "A")
lnPastHours = int(lnPastMinutes) / 60

*** Total past "units" (premiums)
calculate sum(tt_hours) to lnPastUnits ;
		for TT_TYPE="P"

*** Total banked hours
select qCurrTot
store 0 to lnRegHours, lnRegHours1, lnRegHours2
store 0 to lnPaidHours, lnBankedHours, lnUnpaidHours
store 0 to lnExpenseAmt

scan
	if !seek(trim(TT_OPT), "Optlist", "TBLID")
		*** Shouldn't happen
		loop
	endif

	if "#" $ Optlist.TBLC8
		*** Regular hours worked
		lnRegHours = lnRegHours + TT_Hours
		if TT_Week = 0
			lnRegHours1 = lnRegHours1 + TT_Hours
		else
			lnRegHours2 = lnRegHours2 + TT_Hours
		endif
	endif

	do case
	case "P" $ Optlist.TBLC1
		*** Paid
		lnPaidHours = lnPaidHours + TT_Hours

	case "D" $ Optlist.TBLC1
		*** Banked
		lnBankedHours = lnBankedHours + TT_Hours

	otherwise

		*** Add the Expense 	
		llExpenseAccount=ToLogical(tbleval("PRIME", TT_OPT, "TBLC6"))
		if llExpenseAccount 
			lnExpenseAmt = lnExpenseAmt + TT_ExpAmt 
		else 
			lnUnpaidHours = lnUnpaidHours + TT_Hours
		endif 
	endcase

endscan

this.nRegHours = lnRegHours
this.nRegHours1 = lnRegHours1
this.nRegHours2 = lnRegHours2
this.nPaidHours = lnPaidHours
this.nBankedHours = lnBankedHours

*** Load array this.aEmpTot for displaying on bottom
*   of EMPL onglet.
*** Even though the array is 2-dimensional, because of
*   the way VFP handles arrays, we can treat it as
*   1-dimensional.  This is cool.

lnN = 0

*** Total PAID hours
if lnPaidHours <> 0 ;
and !empty(this.cPg1TotHrsPaid_Caption)
	lnN = lnN + 1
	this.aEmptot[lnN] = this.cPg1TotHrsPaid_Caption + ;
				iif(this.lEnterTimeAsHHMM, HtoHM(lnPaidHours,-1), ;
				ltrim(str(lnPaidHours,10,2)))
endif

*** Total BANKED hours
if lnBankedHours <> 0 ;
and !empty(this.cPg1TotHrsBanked_Caption)
	lnN = lnN + 1
	this.aEmptot[lnN] = this.cPg1TotHrsBanked_Caption + ;
				iif(this.lEnterTimeAsHHMM, HtoHM(lnBankedHours,-1),  ;
				ltrim(str(lnBankedHours,10,2)))
endif

*** Total current hours
if lnCurrHours <> 0 ;
and !empty(this.cPg1TotCurrPeriod_Caption)
	lnN = lnN + 1
	this.aEmptot[lnN] = this.cPg1TotCurrPeriod_Caption + ;
				iif(this.lEnterTimeAsHHMM, HtoHM(lnCurrHours,-1), ;
				ltrim(str(lnCurrHours,10,2)))
endif

*** Total EXPENSES amount  
if lnExpenseAmt <> 0 ;
and !empty(this.cPg1TotExpensePeriod_Caption)
	lnN = lnN + 1
	this.aEmptot[lnN] = this.cPg1TotExpensePeriod_Caption + ;
				ltrim(str(lnExpenseAmt,10,2))
endif

*** Total unpaid hours
if lnUnpaidHours <> 0 ;
and !empty(this.cPg1TotUnpaidPeriod_Caption)
	lnN = lnN + 1
	this.aEmptot[lnN] = this.cPg1TotUnpaidPeriod_Caption + ;
				iif(this.lEnterTimeAsHHMM, HtoHM(lnUnpaidHours,-1), ;
				ltrim(str(lnUnpaidHours,10,2)))
endif

*** Total current units
if lnCurrUnits <> 0
	lnN = lnN + 1
	this.aEmptot[lnN] = iif(gcLang = "E", "TOTAL UNITS: ", ;
				"TOTAL (UNITÉS): ") + ;
				iif(this.lEnterTimeAsHHMM, HtoHM(lnCurrUnits,-1), ;
				ltrim(str(lnCurrUnits,5,2)))
endif

*** Total past hours
if lnPastHours <> 0
	lnN = lnN + 1
	this.aEmptot[lnN] = iif(gcLang = "E", "PAST HRS: ", ;
				"HRES ANTÉRIEURES: ") + ;
				iif(this.lEnterTimeAsHHMM, HtoHM(lnPastHours,-1), ;
				ltrim(str(lnPastHours,10,2)))
endif

*** Total past units
if lnPastUnits <> 0
	lnN = lnN + 1
	this.aEmptot[lnN] = iif(gcLang = "E", "PAST UNITS: ", ;
				"UNITÉS ANTÉR.: ") + ;
				iif(this.lEnterTimeAsHHMM, HtoHM(lnPastUnits,-1), ;
				ltrim(str(lnPastUnits,10,2)))
endif

*** qCurrTot is now by week, so get overall totals.
set enginebehavior 70
SELECT tt_opt, ;
		 tt_showopt, ;
		 sum(round(tt_hours*60,0)) as tt_minutes, ;
		 00000.0000 as tt_hours ;
		FROM qCURRTOT ;
		GROUP BY TT_OPT ;
		order by TT_OPT ;
		INTO CURSOR qOptTOT readwrite
set enginebehavior 80
replace all TT_HOURS with TT_MINUTES / 60

scan
	*** Is an expense 	
	llExpenseAccount=ToLogical(tbleval("PRIME", TT_OPT, "TBLC6"))
	if !llExpenseAccount 
		*** Advance to next spot
		lnN = lnN + 1
		this.aEMPTOT[lnN] = trim(tt_showopt) + ": " + ;
				iif(this.lEnterTimeAsHHMM, HtoHM(TT_hours,-1), ;
				ltrim(str(TT_Hours,10,2)))
	endif 
endscan

dimension THIS.aEMPTOT[max(1, ceiling(lnN/lnColumns)), lnColumns]

use in qCurrTot
use in qPastTot
use in qOptTot


*** OT bank totals to display on form
*//// TODO - fix line below ////
*this.nTotOTbank = this.nTimsum1A + lnOThours

*** Force all lines of totals to refresh
this.oLstEmpTot.requery
this.oThisform.pgf1.Page1.refresh

*** Restore environment
select vTimetmp
do GOTO with lnRecno

return

*=========================================================
procedure LoadQBatch(pnPersid)
*** Called from PageActivate() 
*** when batch tab (page 2) activated

local loQBatch1, lcFields, llFirstTime, lnSaveRecno
local loCurTot, lcAlias, lnSelect

lnSelect = select()

select QBATCH
llFirstTime = (reccount() = 0)
lnSaveRecno = iif(eof(), 0, recno())

if llFirstTime
	append from (dbf("qBossed")) for H_PERSID <> 0
	replace all BA_NAME with name(H_Persid)
endif

*** It would have been if this field could have been 
*** dynamically build - Stefan 
lcFields = ;
			"TT_PERSID, TT_OPT, " + ;
		 	"sum(round(tt_hours*60,0)) as tt_minutes, " + ;
		 	"00000.0000 as tt_hours, " + ;
			"0 as TT_EXCEPTN, " + ;
			"max(nvl(TT_SIGNBY,space(20))) as TT_SIGNBY, " + ;
			"max(nvl(TT_VALBY,space(20))) as TT_VALBY, " + ;
			"max(nvl(TT_POSTBY,space(20))) as TT_POSTBY, " + ;
			"max(TT_MOTIF) as TT_MOTIF, " + ;
			"max(TT_ENTITY) as TT_ENTITY, " + ;
			"max(TT_JOBID) as TT_JOBID, " + ;
			"max(TT_STATIS) as TT_STATIS, " + ;
			"max(TT_PROJECT) as TT_PROJECT, " + ;
			"max(left(TT_NOTES,10)) as TT_NOTES, " + ;
			"max(left(TT_MSG1,254)) as TT_MSG1, " + ;
			"'     ' as TT_TBLC8, " + ;
		 	"ROUND(SUM(TT__EUNIT),4) TT__EUNIT, " + ;
		 	"ROUND(SUM(TT__EURATE),4) TT__EURATE," + ;
			"MAX(left(TT__ATY,254)) as TT__ATY, " + ;
			"MAX(left(TT__LOC,254)) as TT__LOC"

			
loQBatch1 = this.oBiz.GetList (;
				"/cursor=qBatch1", ;
				lcFields, ;
				"TT_BATCH='" + trim(this.cBatch) + "'", ;
				"TT_PERSID, TT_OPT", ;
				"TT_PERSID, TT_OPT")

*** Fix up HOURS from minutes
replace all TT_HOURS with TT_MINUTES / 60

*** Fill in TBLC8
set order to TBLID in Optlist
set relation to TT_OPT into Optlist
replace all TT_TBLC8 with Optlist.TBLC8
set relation to
set order to 0 in Optlist

*** Determine what is Exceptional
this.LoadQBatch_FlagExceptions()

*** Summarize totals
this.LoadQBatch_Summarize()

index on TT_PERSID tag TT_PERSID

*** Merge into QBatch
select QBatch
set relation to H_PERSID into qBatch2
replace all ;
		BA_TOT1 with qbatch2.TT_TOT1, ;
		BA_TOT2 with qbatch2.TT_TOT2, ;
		BA_TOT3 with qbatch2.TT_TOT3, ;
		BA_TOT4 with qbatch2.TT_TOT4, ;
		BA_TOT5 with qbatch2.TT_TOT5, ;
		BA_SIGNED with qbatch2.TT_SIGNED, ;
		BA_VALED with qbatch2.TT_VALED, ;
		BA_EXCEPTN with qbatch2.TT_EXCEPTN=1, ;
		BA_POSTED with qbatch2.TT_POSTED

* BA_TOT65 - expenses
* It should not be included in the totals.
replace all ;
		BA_Total with BA_TOT1+BA_TOT2+BA_TOT3+BA_TOT4 && +BA_TOT5

replace all ;
		BA_TOT1HM  with HtoHM(BA_TOT1, 3, "/DELIM"), ;
		BA_TOT2HM  with HtoHM(BA_TOT2, 3, "/DELIM"), ;
		BA_TOT3HM  with HtoHM(BA_TOT3, 3, "/DELIM"), ;
		BA_TOT4HM  with HtoHM(BA_TOT4, 3, "/DELIM"), ;
		BA_TOT5HM  with HtoHM(BA_TOT5, 3, "/DELIM"), ;
		BA_TOTALHM with HtoHM(BA_TOTAL, 3, "/DELIM")

release loQBatch1
use in select("qBatch2")

select QBatch
index on BA_NAME tag BA_NAME

*** Calculer les totaux des totaux
SELECT qBatch
SUM ba_tot1 TO this.oThisform.nBatchTot1
SUM ba_tot2 TO this.oThisform.nBatchTot2
SUM ba_tot3 TO this.oThisform.nBatchTot3
SUM ba_tot4 TO this.oThisform.nBatchTot4
SUM ba_tot5 TO this.oThisform.nBatchTot5
if type("this.oThisform.nBatchTotal") = "N"
	SUM ba_Total TO this.oThisform.nBatchTotal
endif

*** Set filter based on what user has checked
this.QBatch_SetFilter()

if llFirstTime or lnSaveRecno = 0
	if !empty(pnPersid)
		locate for H_PERSID = pnPersid
	else
		this.InitNewPers()
	endif
else
	do GOTO with lnSaveRecno, "qBatch" 
endif

select (lnSelect)
return

*=========================================================
procedure LoadQBatch_Summarize()
*** A separate method to facilitate overriding

set enginebehavior 70
select TT_PERSID, ;
		 (TT_SIGNBY<>"") as TT_SIGNED, ;
		 (TT_VALBY<>"") as TT_VALED, ;
		 (TT_POSTBY<>"") as TT_POSTED, ;
		 max(TT_EXCEPTN) as TT_EXCEPTN, ;
		 sum(iif("1"$TT_TBLC8, TT_HOURS, 0) + 000.00) as TT_TOT1, ;
		 sum(iif("2"$TT_TBLC8, TT_HOURS, 0) + 000.00) as TT_TOT2, ;
		 sum(iif("3"$TT_TBLC8, TT_HOURS, 0) + 000.00) as TT_TOT3, ;
		 sum(iif("4"$TT_TBLC8, TT_HOURS, 0) + 000.00) as TT_TOT4, ;
       sum(nvl(TT__EUNIT,0)*nvl(TT__EURATE,0)) as TT_TOT5, ;
		 max(TT_MOTIF) as TT_MOTIF, ;
		 max(TT_ENTITY) as TT_ENTITY, ;
		 max(TT_JOBID) as TT_JOBID, ;
		 max(TT_STATIS) as TT_STATIS, ;
		 max(TT_PROJECT) as TT_PROJECT, ;
		 max(left(TT_NOTES,10)) as TT_NOTES, ;
		 max(left(TT_MSG1, 254)) as TT_MSG1, ;
		 MAX(left(TT__ATY,254)) as TT__ATY, ;
		 MAX(left(TT__LOC,254)) as TT__LOC ;
	from qBatch1 ;
	group by TT_PERSID ;
	into cursor qBatch2
set enginebehavior 80

return

*==========================================================
procedure PrintRec()
*** Timesheet reports 
*
local lcList, lcList2, lcRptAccess, lnRptNo, lnI  
local loBizRepolist 

loBizRepolist = null 
store "" to lcList, lcList2

local array laReportList[3]
laReportList[1] = 9001
laReportList[2] = 9002 
laReportList[3] = 9003 

dimension OPTIONS[4]
OPTIONS[1] = .t.
OPTIONS[2] = this.dFromDt
OPTIONS[3] = this.dThruDt
OPTIONS[4] = trim(this.cBatch)

*** set step on 
with this 

.odset.save()

if isnull(loBizRepolist)
	loBizRepolist = .oBizMgr.GetBiz("REPOLIST")
endif 

for lnI = 1 to alen(laReportList, 1)
	lnRptNo = laReportList[lnI]

	lcRptAccess = loBizRepolist.GetValue("RP_RPTNO="+;
			transform(lnRptNo), "RP_ACCESS")
			
	if lcRptAccess$gcBelongs or empty(lcRptAccess)
		do case 
		case lnI = 1
			lcList = lcList + lstr(lnRptNo,0) +" "
		case lnI = 2
			lcList2 = lcList2 + lstr(lnRptNo,0) +" "
		otherwise 
			lcList = lcList + lstr(lnRptNo,0) +" "
			lcList2 = lcList2 + lstr(lnRptNo,0) +" "
		endcase 
	endif 
next 

if .oThisform.pgf1.activepage = 1
    do quickprint_custom with "/DEST=PDF", lcList, ;
    		"between(TT_BATCH,'"+left(.cBatch, 9) + ;
    		"','"+left(.cBatch, 9)+"~')"+;
    		" and TT_PERSID="+transform(.PersId), ;
    		OPTIONS
else 
	do quickprint_custom with "/DEST=PDF", lcList2, ;
			"TT_BATCH='"+.cBatch+"'", OPTIONS
endif

endwith 

laReportList = null 
return 

*=========================================================
procedure AddTrans(pcSwitches, pdEffdt, pnHours, pcOpt, pcType)
*** Used primarily to add hours (+ or -) before signing.
* The base class has been overrideen in order to add value 
* to field TT_INOUT - Stefan G.
*
pcSwitches = iif(vartype(pcSwitches)="C", upper(pcSwitches),"")

private all like TT_*

select vTimetmp
scatter memvar memo blank

m.TT_PERSID = this.Persid
m.TT_NAME = this.PersName
m.TT_NOTES = iif(gcLang="E", "Created upon signing", ;
				"Créé à la signature")
m.TT_SOURCE = "A"
m.TT_EFFDT = pdEffdt
m.vvShowDay = left(this.DateToText(pdEffdt), 16)

m.TT_UNITCD = "H"
m.TT_USER = gcUser
m.TT_BATCH = this.cBatch
m.TT_ENTERDT = datetimex()

m.TT_OPT = pcOpt
m.TT_TYPE = pcType
m.TT_SHOWOPT = tbleval("OPT", pcOpt)
m.TT_DEST = tbleval("OPT", pcOpt, "TBLC1")
m.TT_TBLCTL = tbleval("OPT", pcOpt, "TBLC8")
m.TT_HOURS = pnHours
m.vvHHMM = HtoHM(pnHours, len(vTimetmp.vvHHMM)-2)

m.TT_INOUT = "2359-2400"

insert into vTimetmp from Memvar

if "/SAVE" $ pcSwitches
	*** Save immediately
	this.BeforeSave()
	this.oDset.Save()

	*** Update totals
	this.Calc_EmpTot()

	*** Refresh grid
	this.oThisform.Refresh
endif

return


enddefine
*Eof######################################################
