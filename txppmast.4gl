###############################################################################
#+ <SOURCEFILE name="txppmast.4gl" author="mcyr" date="03-08-2004">
#+    <summary>
#+     This program allows maintenance to personal properties, both current
#+     and tax.  The primary table is txppmast.  Current and tax variations
#+     are identified by txpp_mt_sw where 0 (zero) is the current (master)
#+     and 1 with txpp_year of the current assessment year is tax or in the
#+     case of VA, mt_sw 9's are the tax records. Older tax
#+     years are also in the database and each year is identified by the tax
#+     year (txpp_year). Many tables are used to retain PP information such
#+     as valuations, charge definitions, exemptions, etc.  Options are
#+     provided to maintain just the current data or tax.  When the tax d
#+     is updated, the current master record will also be updated with the
#+     same changes.  This option is presented as a menu option and sets the
#+     access mode.
#+    </summary>
#+    <notes>
#+     An argument of V can be passed by the Munis menu that is available for
#+     Virginia sites only that will activate certain "Filing" functions/
#+     conditions and hide other functions.  These functions and conditions
#+     effect the master screen.
#+
#+     An argument of P can be passed by the Munis menu and will only be used
#+     by VA sites.  This indicates only Personal Property records that have
#+     a 'Type' of 'P' can be accessed.  When the program is called with this
#+     parameter, the program will force the value of ct_sw to 'T'ax.  The only
#+     available ring menu options will be Find, Next, Prev, Browse, Values,
#+     and Exit.  The purpose of this option is to allow the user to maintain
#+     tax values for Public Service Corporations and/or PSC bills that have
#+     already been generated.  Any changes to values that are already part of
#+     a bill must be accompanied by adjustments to the bill via Bill Detail
#+     Maintenance.
#+
#+     When an argument of L is passed (called from Business License Master
#+     Maintenance), the program will function as a PP maintenance program.
#+     The initial reason for calling PP Master from BL Master is to allow
#+     the user to view the PP Master record for the Property ID entered on
#+     the BL Master record.  If there is no Property ID, then PP Master
#+     will allow the user to add a Master record and return the Property ID
#+     to BL Master.  Besides Passing the argument of 'L', BL Master will also
#+     pass blms_prop_id, blms_bus_cid, blms_bus_addr_no, blms_id and a link
#+     number that will be used in cooperation with sppasval to return the
#+     prop ID to BL Master if a PP record was added.  When the Property ID is
#+     passed, an Update on the record will be allowed; however, the options
#+     Find, Next, Prev, Browse, Add and Delete will not be available.  The
#+     user has the ability to delete the record they have just added and then
#+     Add a new record.  They also have the ability to Update that record.
#+     Location info will default in on the PP Master record from the BL Master
#+     record on an Add.  The user can not Add more than one record.
#+
#+     An "I" can be passed from the Munis menu to signify Inquiry-only mode.
#+     This mode will hide and disable all maintenance functions.
#+
#+     An "E" is passed when called from Listing Entry (NC) to allow the addition
#+     of a tax record at the same time that a current record is added.
#+
#+    </notes>
#+    <warnings>
#+       <warning></warning>
#+    </warnings>
#+    <appnotes></appnotes>
#+    <revisions>
#+       <rev date="03-08-2004" author="mcyr" wo="none" version="">
#+          -Added file header.
#+       </rev>
#+    </revisions>
#+ </SOURCEFILE>
###############################################################################
#############################################################################
#                                  MUNIS
#                        Computer Center Software
#
#  Copyright (C) 1996 The Computer Center, Inc., Falmouth, Maine US
#  Use, modification, duplication, and/or distribution of this software is
#  limited to the terms of the software agreement.
#
#  Module Name:   txppmast.4gl
#  Date Written:  11/16/96
#  Written By:    TCC
#
#  Attachment Button Enabled
###############################################################################
import fgl Attachments
import fgl IdleProcessor
import fgl CustomerInterface
import fgl CustomerManager
import fgl ExceptionState
import fgl ExceptionFactory
import fgl ExceptionHandler
import fgl archgobj
import fgl arcommon
import fgl arspclnk
import fgl cploclib
import fgl mefavort
import fgl mvaclassio
import fgl mvamvmstwr
import fgl spapplnk
import fgl spaskrtn
import fgl spcntrec
import fgl spconfig
import fgl spdomlib
import fgl sperrorm
import fgl spexcmnt
import fgl spfeatur
import fgl spgetarg_wrappers
import fgl spgettmp
import fgl sphlpdoc
import fgl spinform
import fgl spinitpm
import fgl spmdebug
import fgl spmuhelp
import fgl spnotlib
import fgl spoffice
import fgl spoutput
import fgl sppasval
import fgl sprelnot
import fgl sprolapi
import fgl sprunpgm
import fgl spschmvw
import fgl spsetwin
import fgl sptiklnk
import fgl sptxtedt
import fgl sptylrcm
import fgl txcalchg
import fgl txchgdefwr
import fgl txchgmnt
import fgl txclascdwr
import fgl txcsttagwr
import fgl txexmmnt
import fgl txinitem
import fgl txmigratbu
import fgl txmiscfm
import fgl txownerswr
import fgl txownmnt
import fgl txparamsio
import fgl txppmast_adlops
import fgl txppmast_browse
import fgl txppmastio
import fgl txppmastwr
import fgl txproptybu
import fgl txprpxmlbu
import fgl txrpfind
import fgl txspcondbu
import fgl txvalmnt
import fgl txvalueswr
import fgl txyrparmio
import fgl txyrparmwr

&include "SchemaMunis.inc"
&include "ar/archgobj.inc"
&include "sp/TylerNotifyInterface.inc"

globals "txppmast_global.4gl"
#Tyler Notify
#   Module-scoped variables for Tyler Notify integration
define g_notifyParams                  TylerNotifyDialogParameters,
       g_notifyReturns                 TylerNotifyDialogReturnValues,
       notifyRecordArray               dynamic array of TylerNotifyRecord, # Array to hold contact/message record to submit to Tyler Notify
       notifyFetchArray                dynamic array of integer,           # Array to cross-reference notifyRecordArray entries with a tmpcutof scroll cursor
       g_notify                        char(1),                            # Define check box variable to indicate whether to automatically submit notifications after a report
       g_notify_complete               char(1),                            # Indicates notifications have been submitted for a given define set
       searchString                    string,
       cancelled                       boolean

main

   define lv_inputInfo char(300),
          lv_initialSearch,
          lv_initialFindSpecific,
          lv_initialAdd boolean

   defer interrupt
   whenever error continue

   let ST_USER = spconfig_getEnv("LOGNAME")
   call spinitpm(ST_USER) returning ST_CAN_RUN, ST_CLIENT_NAME, ST_TRANS

   let lv_initialSearch = FALSE
   let lv_initialFindSpecific = FALSE
   let lv_initialAdd = FALSE

# Arguments may be passed in.  When calling this program
# from another program using sprunpgm separate each argument with a "/".
#
# An example string would be: let arg_string =
#          "I/", id_no using "&&&&&&&&&&",  "/", year using "&&&&",
#          "/"list_no using "&&&&&&&&", "/", mt_sw using "&&"
#  then call sprunpgm("I", "txppmast", arg_string)

   if (spgetarg_numArgs() != 0) then
      let lv_inputInfo = spgetarg_argVal(1)
      let pass_let   = lv_inputInfo[1]

      if (pass_let NOT MATCHES '[-IELNS]') then
         let err_msg = "Invalid argument passed.  Valid arguments: I, N, L, E, or S."
         call sperrorm(err_msg, 0, 2)
         exit program
      end if

      if pass_let = "L" then
         let pass_prop  = spgetarg_argVal(2)
         let pass_lno   = spgetarg_argVal(3)
         let pass_bcid  = spgetarg_argVal(4)
         let pass_badr  = spgetarg_argVal(5)
         let pass_blid  = spgetarg_argVal(6)
         if (pass_prop != "BLANK") then
            let gr_txppmast.txpp_id_no   = pass_prop clipped
            let gr_txppmast.txpp_year    = 0
            let gr_txppmast.txpp_list_no = 0
            let gr_txppmast.txpp_mt_sw   = 0

            let lv_initialFindSpecific = TRUE
         else
            let lv_initialFindSpecific = FALSE
         end if
      else
         if pass_let = "I" then
            if (length(lv_inputInfo) > 1) then
               let gr_txppmast.txpp_id_no   = lv_inputInfo[3,12]
               let gr_txppmast.txpp_year    = lv_inputInfo[14,17]
               let gr_txppmast.txpp_list_no = lv_inputInfo[19,26]
               let gr_txppmast.txpp_mt_sw   = lv_inputInfo[28,29]
               if (gr_txppmast.txpp_mt_sw is NULL) then
                  let gr_txppmast.txpp_mt_sw = 1
               end if
               let lv_initialFindSpecific = TRUE
            else
               if spgetarg_numArgs() > 1 then
                  let gr_txppmast.txpp_id_no   = spgetarg_argVal(2)
                  let gr_txppmast.txpp_year    = spgetarg_argVal(3)
                  let gr_txppmast.txpp_list_no = spgetarg_argVal(4)
                  let gr_txppmast.txpp_mt_sw   = spgetarg_argVal(5)
                  let lv_initialFindSpecific = TRUE
               else
                  let lv_initialFindSpecific = FALSE
               end if
            end if
         else
            if (length(lv_inputInfo) > 1) then
               case
                  when (lv_inputInfo = "--Add")
                     let lv_initialAdd = TRUE
                  when (lv_inputInfo = "--Search")
                     let lv_initialSearch = TRUE
                     let searchString = spgetarg_argVal(2)
                  otherwise
                     let gr_txppmast.txpp_id_no   = lv_inputInfo[3,12]
                     let gr_txppmast.txpp_year    = lv_inputInfo[14,17]
                     let gr_txppmast.txpp_list_no = lv_inputInfo[19,26]
                     let gr_txppmast.txpp_mt_sw   = lv_inputInfo[28,29]
                     if (gr_txppmast.txpp_mt_sw is NULL) then
                        let gr_txppmast.txpp_mt_sw = 1
                     end if
                     let lv_initialFindSpecific = TRUE
               end case
            end if
         end if
      end if

   else
      let pass_let = " "
   end if
   call main_init()
   if (NOT ST_CAN_RUN) then
      let err_msg = "Initialization error(s) occurred.  ",
                    "You cannot continue."
      call sperrorm(err_msg, 0, 2)
   end if

   if (ST_CAN_RUN) then

      if (pass_let = "I" or lv_initialSearch) then
         let reptitle = "Personal Property Inquiry"
      else
         let reptitle = "Personal Property"
         if (ST_CLIENT_STATE = "VA") then
            if (pass_let = "V") then
                let reptitle = "Personal Property Filing"
            end if
         end if
      end if

      let whdl_main = spsetwin_new("txppmast01",
                                    reptitle,
                                    "appmain",
                                    "ATTACH")
      call spsetwin_navbar(0,0,0," ")

      let gr_txppmast.txpp_resident = "N"
      display by name gr_txppmast.txpp_resident

      call hide_groups()
      if (NOT ST_CAN_RUN) then
         call spsetwin_hdlClose(whdl_main)
         call spinitpm_finalize()
      end if

      # hide fields that are state specific
      call hide_fields()
      call change_field_attr()
      call set_styles()

      let disable_find_option = FALSE
      case
         when (lv_initialFindSpecific)
            let disable_find_option = TRUE
            call find_routine(6)
            if (record_count) then
               call display_record(TRUE)
            else
               let ST_CAN_RUN = FALSE
            end if
         when (lv_initialAdd)
            call add_routine()
         when (lv_initialSearch)
            call find_routine(7)
            if (record_count) then
               call display_record(TRUE)
            else
               let ST_CAN_RUN = FALSE
            end if
            # Override the run mode to "I"nquiry
            let pass_let = "I"
      end case

   end if

   if (ST_CAN_RUN) then
      call main_menu()
   end if

   call spsetwin_hdlClose(whdl_main)
   call spinitpm_finalize()

end main
###########################################################################
function main_init()
   define prepchar                     string,
          tmp_filler                   like txparams.txpm_filler

   let ST_CAN_RUN       = TRUE
   let ST_TODAY         = TODAY
   let ZERO             = 0
   let int_flag         = FALSE
   let WHDL_CLOSE       = FALSE
   let SPACE            = " "

   let addl_no          = null
   let bl_add           = FALSE
   let confid_msg       = 'CONFIDENTIAL'
   let cust_acct        = 0
   let disp_special     = TRUE
   let default_choice   = " "
   let gv_customerSSN   = " "

#  Tyler Notify
#
#  NOTE: Intentionally leaving this out of saved report options to
#        force the user to choose to notify every time, thus forcing
#        them to review the notification options every time.
   let g_notify = "N"


   initialize nl_txppmast.* TO NULL
   initialize nl_spspccom.* TO NULL

   let ST_IDLE = IdleProcessor.ProgramTimeoutGet()

   let ar_user_viewSSN = arcommon_SSN_viewPermission(ST_USER)
   let ar_user_enterSSN = arcommon_SSN_entryPermission(ST_USER)

#Tyler Notify
#  Initialize the module-scoped notify dialog parameter record
#  used for batch processing. Do this once at during init to
#  avoid having to changes these on a re-define.
   call spnotlib_initializeNotifyDialog(g_notifyParams.*)
      returning g_notifyParams.*

   let g_notifyParams.moduleID = "TX"
   let g_notifyParams.hostID = "TXPersonalProperty"
   let g_notifyParams.filterOptionsMode = FilterOptionsModeRecordType
   let g_notifyParams.filterRecordType = RecordTypeMunisCustomer
   let g_notifyParams.windowTitle = "Personal Property Notification"


# get client state

   select spsysrec.client_state
     into ST_CLIENT_STATE
     from spsysrec
    where spsysrec.day_key = 9
   if (sqlca.sqlcode != 0) then
      call spsyserr("Select spsysrec failed.", "Select spsysrec - 82.")
      let ST_CAN_RUN = FALSE
      return
   end if

# get user permissions

   let updt_inact = sprolapi_getUserPermission(ST_USER, "TAX", "txid_updt_inact")
   if (updt_inact IS NULL) then
      call sperrorm("Permission not found. See error log for more information.", 0, 2)
      let ST_CAN_RUN = FALSE
   end if

   let public_access = sprolapi_getUserPermission(ST_USER, "SYSTEM", "sp_pub_access")
   if (public_access IS NULL) then
      call sperrorm("Permission not found. See error log for more information.", 0, 2)
      let ST_CAN_RUN = FALSE
   end if
   if (public_access != "N") then
      let public_access = "Y"
   end if

   let cust_mnt = sprolapi_getUserPermission(ST_USER, "ACCT_RCV", "arid_cust_mnt")
   if (cust_mnt IS NULL) then
      call sperrorm("Permission not found. See error log for more information.", 0, 2)
      let ST_CAN_RUN = FALSE
   end if

#Get permissions to view special conditions/notes...
   let glob_spec_view_notes = sprolapi_getUserPermission(ST_USER,"ACCT_RCV","arid_view_scnotes")
   if (glob_spec_view_notes IS NULL) or (glob_spec_view_notes = " ") then
      let glob_spec_view_notes = "N"
   end if

# get tax parameter info

   select txpm_assm_year, txpm_coll_year, txpm_next_mvno,
          txpm_act_pp_year, txpm_act_pp_cycle, txpm_filler
     into assm_year, coll_year, hold_use_cycle,
          act_pp_year, act_pp_cycle, tmp_filler
     from txparams
    where txparams.txpm_code = "P"
   if (sqlca.sqlcode) then
      call spsyserr("Select txparams failed.","Select txparams - 108")
      let ST_CAN_RUN = FALSE
      return
   end if
   if (tmp_filler[2] is NULL or tmp_filler[2] = " ") then
      let tmp_filler[2] = "Y"
   end if
   let updt_curr = tmp_filler[2]
   call spmdebug_say("TXPPMAST: main_init: updt_curr: ", updt_curr)

  if (assm_year < 1991) then
     call sperrorm("Tax settings assessment year must be > 1990.", 0, 2)
     let ST_CAN_RUN = FALSE
     return
  end if

  if (coll_year < 1991) then
     call sperrorm("Tax settings collection year must be > 1990.", 0, 2)
     let ST_CAN_RUN = FALSE
     return
  end if

  if (coll_year > assm_year) then
     let err_msg = "Tax settings collection year cannot ",
                    "be greater than assessment year."
     call sperrorm(err_msg, 0, 2)
     let ST_CAN_RUN = FALSE
     return
  end if

   if (ST_CLIENT_STATE != "MA" and
       ST_CLIENT_STATE != "MD" and
       ST_CLIENT_STATE != "NY" and
       ST_CLIENT_STATE != "CT" and
       ST_CLIENT_STATE != "NH" and
       ST_CLIENT_STATE != "MI" and
       ST_CLIENT_STATE != "RI" and
       ST_CLIENT_STATE != "LA" and
       ST_CLIENT_STATE != "VT") then
      if (act_pp_year is not NULL and act_pp_year > 0) then
         let use_year = act_pp_year
         let use_cycle = act_pp_cycle
      else
         let use_year = assm_year
         let use_cycle = 0
      end if
   else
      if ST_CLIENT_STATE = "NY" then
         let use_year = assm_year
         let use_cycle = hold_use_cycle
      else
         let use_year = assm_year
         let use_cycle = 0
      end if
   end if

# get tax year parameter info

  select txyp_proc_stat, txyp_status, txyp_val_pct, txyp_instmt_meth
    into proc_stat, pp_stat, val_pct, instmt_meth
    from txyrparm
   where txyp_year = use_year
     and txyp_ar_cat = 25
     and txyp_cycle_comm = use_cycle
  if (sqlca.sqlcode != 0) then
     let err_msg = "Tax year settings not found for ",
                       assm_year using "####","."
     call sperrorm(err_msg, 0, 2)
     let ST_CAN_RUN = FALSE
     return
  end if
  if (val_pct < 60) then
     let val_pct = 100
  end if

   let ct_sw = "C"
   let use_mt_sw = 0

   if (pass_let = "S") then
      let ct_sw = "T"
      let use_mt_sw = gr_txppmast.txpp_mt_sw
   end if

# txvalues_cursor

   let prepchar = "select txva_amt, txva_sq_ft from txvalues ",
                   "where txva_mt_sw   = ? ",
                     "and txva_ar_cat  = 25 ",
                     "and txva_prop_id = ? ",
                     "and txva_year    = ? ",
                     "and txva_list_no = ? "
   prepare sel_txvalues from prepchar
   if (sqlca.sqlcode != 0) then
       call spsyserr("Prepare txvalues failed.",
             "Prepare txvalues - 205")
       let ST_CAN_RUN = FALSE
       return
   end if
   declare txvalues_cursor cursor for sel_txvalues

# txppmast_cursor

   let prepchar = "select * from txppmast ",
                   "where txpp_mt_sw = ? ",
                     "and txpp_id_no = ? ",
                     "and txpp_year = ?  ",
                     "and txpp_list_no = ?"
   prepare get_txppmast from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Prepare txppmast_cursor failed.",
            "Prepare txppmast - 135")
      let ST_CAN_RUN = FALSE
      return
   end if
   declare txppmast_cursor cursor for get_txppmast

# lock_txch_j

   let prepchar = "select * ",
                  "  from txchgdef ",
                  " where txch_mt_sw = ? ",
                  "   and txch_ar_cat = ? ",
                  "   and txch_prop_id = ? ",
                  "   and txch_year = ? ",
                  "   and txch_list_no = ? ",
                  "   and txch_seq = ? ",
                  "   for update ",
                  spinitpm_getNoWait()
   prepare prep_txch_j from prepchar
   if (sqlca.sqlcode != 0) then
      call spsyserr("Prepare lock_txch_j failed.","Prepare txchgdef - 2830")
      let ST_CAN_RUN = FALSE
      return
   end if
   declare lock_txch_j cursor for prep_txch_j

# delete_txch_j

   let prepchar = "delete from txchgdef where current of lock_txch_j"
   prepare delete_txch_j from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Prepare delete_txch_j failed.",
                    "txchgdef:  Prepare error 207.")
      let ST_CAN_RUN = FALSE
      return
   end if

# lock_txpm

   let prepchar = "select * ",
                  "  from txparams ",
                  " where txpm_code = 'P' ",
                  "   for update ",
                  spinitpm_getNoWait()
   prepare prep_txpm from prepchar
   if (sqlca.sqlcode != 0) then
      call spsyserr("Prepare lock_txpm failed.","Prepare txparams - 1270")
      let ST_CAN_RUN = FALSE
      return
   end if
   declare lock_txpm cursor for prep_txpm

# update_txparams

   let prepchar = "update txparams ",
                  "   set txpm_next_ppid = ? ",
                  " where current of lock_txpm"
   prepare update_txparams from prepchar
   if (sqlca.sqlcode != 0) then
      call spsyserr("Prepare txparams update failed.",
                    "Prepare txparams update - 1480")
      let ST_CAN_RUN = FALSE
      return
   end if

# bilhdr_csr

   let prepchar = "select arbilhdr.arbh_ar_cat, arbilhdr.arbh_year, ",
                        " arbilhdr.arbh_bill, arbilhdr.arbh_bill_type ",
                  "  from arbilhdr ",
                  " where arbh_prop_cd = ? "
   prepare get_bilhdr from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Error creating bilhdr_csr.",
            "txppmast:  Prepare error .")
      let ST_CAN_RUN = FALSE
      return
   end if
   declare bilhdr_csr cursor for get_bilhdr

   let prepchar = "SELECT TOP 1 Bills.BillNumber ",
                  "  FROM AccountsReceivableDataAccess.Bills ",
                  " WHERE Bills.AccountIdentifier = ? ",
                  "   AND Bills.BillYear = ? ",
                  "   AND Bills.BillCategory = 25 ",
                  "   AND Bills.BillClassificationCode = 'R' "
   prepare getBillNumber from prepchar
   if sqlca.sqlcode then
      call spsyserr("Prepare getBillNumber failed.", "Prepare, Line " || __LINE__)
      let ST_CAN_RUN = FALSE
   end if

# owner_cursor

   let prepchar = " select txon_acct, txon_addr_no from txowners ",
                   " where txon_mt_sw = ? ",
                      "and txon_ar_cat  = 25  ",
                      "and txon_prop_id = ? ",
                      "and txon_year    = ? ",
                      "and txon_list_no = ? ",
                      "and txon_own     = 'P' "
   prepare get_owner from prepchar
   if (sqlca.sqlcode != 0) then
      call spsyserr("Error creating owner_cursor.",
                    "Prepare error 223.")
      let ST_CAN_RUN = FALSE
      return
   end if
   declare owner_cursor cursor for get_owner

# juriscd_chgdef

   let prepchar = "select txjc_chg_def, txjc_base_pct ",
                    "from txjurchg where txjc_code = ? "
   prepare get_juriscd from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Error creating txjurchg_cursor.",
                    "txppmast:  Prepare error 263.")
      let ST_CAN_RUN = FALSE
      return
   end if
   declare juriscd_chgdef cursor for get_juriscd

# cust_tag_cursor

   let prepchar = "select txtg_acct, txtg_bill_addr_no ",
                    "from txcsttag ",
                   "where txtg_ar_cat  = 25 ",
                    " and txtg_own     = 'P' ",
                    " and txtg_prop_id = ? "
   prepare get_custtag from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Error creating cust_tag_cursor.",
                    "txppmast: Prepare error 276.")
      let ST_CAN_RUN = FALSE
      return
   end if
   declare cust_tag_cursor cursor for get_custtag

# audit_count
   # current_audit_count will be for all audits regardless of version or ctb_sw.
   let prepchar = "SELECT ",
                  "   (SELECT COUNT(*) ",
                  "      FROM  txppmaud ",
                  "     WHERE txpa_pp_id = ? ",
                  "   ) + ",
                  "   (SELECT COUNT(*) ",
                  "     FROM mvaaudit ",
                  "    WHERE mvaa_prop_id = ? ",
                  "      and mvaa_table in ('mvamvmst','mvamvadd','MASTER') ",
                  "   ) "
   prepare get_curr_aud_cnt from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Error creating get_curr_aud_cnt.",
                    sfmt("txppmaud:  Prepare error %1.",__LINE__))
      let ST_CAN_RUN = FALSE
      return
   end if
   declare current_audit_count cursor for get_curr_aud_cnt

   # tax_audit_count will include audits with a ctb_sw of 'T' but will only select the
   # active version for mvaaudit. If the client wants to see VA MV audits for all versions they can
   # pull up the current record or go to the Motor Vehicles program for the audits.
   let prepchar = "SELECT ",
                  "   (SELECT COUNT(*) ",
                  "      FROM  txppmaud ",
                  "     WHERE txpa_pp_id = ? ",
                  "       and txpa_ctb_sw = 'T' ",
                  "   ) + ",
                  "   (SELECT COUNT(*) ",
                  "     FROM mvaaudit ",
                  "    WHERE mvaa_prop_id = ? ",
                  "      and mvaa_table in ('mvamvmst','mvamvadd','MASTER') ",
                  "      and mvaa_mt_sw = ? ",
                  "   ) "
   prepare get_tax_aud_cnt from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Error creating get_tax_aud_cnt.",
                    sfmt("txppmaud:  Prepare error %1.",__LINE__))
      let ST_CAN_RUN = FALSE
      return
   end if
   declare tax_audit_count cursor for get_tax_aud_cnt

# audit_find

   # current_audit_find will be for all audits regardless of version or ctb_sw.
   let prepchar = " SELECT ",
                  "   a_date, ",
                  "   a_time, ",
                  "   a_clerk, ",
                  "   a_action, ",
                  "   a_file, ",
                  "   a_table, ",
                  "   t_seq, ",
                  "   a_field, ",
                  "   a_ser, ",
                  "   a_old_val, ",
                  "   a_new_val, ",
                  "   mv_year, ",
                  "   mv_vin ",
                  " FROM ( ",
                  " SELECT txpa_date     a_date,  ",
                  "        txpa_time     a_time, ",
                  "        txpa_clerk    a_clerk,  ",
                  "        txpa_action   a_action,  ",
                  "        txpa_ctb_sw   a_file,  ",
                  "        txpa_table    a_table,  ",
                  "        txpa_seq      t_seq,  ",
                  "        txpa_field    a_field,  ",
                  "        txpa_ser      a_ser,  ",
                  "        txpa_old_val  a_old_val, ",
                  "        txpa_new_val  a_new_val, ",
                  "        NULL          mv_year, ",
                  "        ' '           mv_vin ",
                  " FROM txppmaud  ",
                  " WHERE txpa_pp_id = ? ",
                  "             ",
                  " UNION ALL ",
                  "           ",
                  " SELECT mvaa_date            a_date, ",
                  "        mvaa_time            a_time, ",
                  "        mvaa_clerk           a_clerk, ",
                  "        mvaa_action          a_action, ",
                  "        CASE mvaa_mt_sw ",
                  "           WHEN 0 THEN 'C' ",
                  "           WHEN 1 THEN 'T' ",
                  "           WHEN 9 THEN 'W' ",
                  "           ELSE ' ' ",
                  "        END                  a_file, ",
                  "        CASE mvaa_table ",
                  "           WHEN 'MASTER'  ",
                  "              THEN 'mvamvmst' ",
                  "        ELSE mvaa_table ",
                  "        END                  a_table, ",
                  "        0                    t_seq, ",
                  "        mvaa_field           a_field, ",
                  "        mvaa_ser             a_ser, ",
                  "        mvaa_old_val         a_old_val, ",
                  "        mvaa_new_val         a_new_val, ",
                  "        mvaa_year            mv_year, ",
                  "        mvaa_vin             mv_vin ",
                  "         ",
                  " FROM mvaaudit  ",
                  " WHERE mvaa_prop_id = ? ",
                  "   and mvaa_table in ('mvamvmst','mvamvadd','MASTER')) T  ",
                  " ORDER BY a_date desc, ",
                  "          a_time desc, ",
                  "          a_table, ",
                  "          a_ser desc "
   prepare get_curr_aud_find from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Error creating get_curr_aud_find.",
                    sfmt("txbrowse:  Prepare error %1.",__LINE__))
      let ST_CAN_RUN = FALSE
      return
   end if
   declare current_audit_find scroll cursor for get_curr_aud_find

   # tax_audit_find will only be for audits with a ctb_sw of 'T' from txppmaud and for the active mt_sw for mvaaudit.
   let prepchar = " SELECT ",
                  "   a_date, ",
                  "   a_time, ",
                  "   a_clerk, ",
                  "   a_action, ",
                  "   a_file, ",
                  "   a_table, ",
                  "   t_seq, ",
                  "   a_field, ",
                  "   a_ser, ",
                  "   a_old_val, ",
                  "   a_new_val, ",
                  "   mv_year, ",
                  "   mv_vin ",
                  " FROM ( ",
                  " SELECT txpa_date     a_date,  ",
                  "        txpa_time     a_time, ",
                  "        txpa_clerk    a_clerk,  ",
                  "        txpa_action   a_action,  ",
                  "        txpa_ctb_sw   a_file,  ",
                  "        txpa_table    a_table,  ",
                  "        txpa_seq      t_seq,  ",
                  "        txpa_field    a_field,  ",
                  "        txpa_ser      a_ser,  ",
                  "        txpa_old_val  a_old_val, ",
                  "        txpa_new_val  a_new_val, ",
                  "        NULL          mv_year, ",
                  "        ' '           mv_vin ",
                  " FROM txppmaud  ",
                  " WHERE txpa_pp_id = ? ",
                  "   and txpa_ctb_sw = 'T' ",
                  "             ",
                  " UNION ALL ",
                  "           ",
                  " SELECT mvaa_date            a_date, ",
                  "        mvaa_time            a_time, ",
                  "        mvaa_clerk           a_clerk, ",
                  "        mvaa_action          a_action, ",
                  "        CASE mvaa_mt_sw ",
                  "           WHEN 0 THEN 'C' ",
                  "           WHEN 1 THEN 'T' ",
                  "           WHEN 9 THEN 'W' ",
                  "           ELSE ' ' ",
                  "        END                  a_file, ",
                  "        CASE mvaa_table ",
                  "           WHEN 'MASTER'  ",
                  "              THEN 'mvamvmst' ",
                  "        ELSE mvaa_table ",
                  "        END                  a_table, ",
                  "        0                    t_seq, ",
                  "        mvaa_field           a_field, ",
                  "        mvaa_ser             a_ser, ",
                  "        mvaa_old_val         a_old_val, ",
                  "        mvaa_new_val         a_new_val, ",
                  "        mvaa_year            mv_year, ",
                  "        mvaa_vin             mv_vin ",
                  "         ",
                  " FROM mvaaudit  ",
                  " WHERE mvaa_mt_sw = ? ",
                  "   and mvaa_prop_id = ? ",
                  "   and mvaa_table in ('mvamvmst','mvamvadd','MASTER')) T  ",
                  " ORDER BY a_date desc, ",
                  "          a_time desc, ",
                  "          a_table, ",
                  "          a_ser desc "
   prepare get_tax_aud_find from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Error creating get_tax_aud_find.",
                    sfmt("txbrowse:  Prepare error %1.",__LINE__))
      let ST_CAN_RUN = FALSE
      return
   end if
   declare tax_audit_find scroll cursor for get_tax_aud_find

   # sumAssessmentByPropYear
   let prepchar = "SELECT COUNT(*) ",
                  "      ,ISNULL(SUM(txvalues.txva_amt),0) ",
                  "      ,ISNULL(txvalues.txva_list_no,0) ",
                  "  FROM txvalues ",
                  " WHERE txvalues.txva_mt_sw = ? ",
                  "   AND txvalues.txva_ar_cat = 25 ",
                  "   AND txvalues.txva_prop_id = ? ",
                  "   AND txvalues.txva_year = ? ",
                  "GROUP BY txvalues.txva_list_no "
   prepare sumAssessmentByPropYear from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Error preparing sumAssessmentByPropYear. Status:", "Prepare 814 - txvalues")
      let ST_CAN_RUN = FALSE
      return
   end if

   # sumAssessmentByPropYearNumber
   let prepchar = "SELECT ISNULL(SUM(txvalues.txva_amt),0) ",
                  "  FROM txvalues ",
                  " WHERE txvalues.txva_mt_sw = 1 ",
                  "   AND txvalues.txva_ar_cat = 25 ",
                  "   AND txvalues.txva_prop_id = ? ",
                  "   AND txvalues.txva_year = ? ",
                  "   AND txvalues.txva_list_no = ? "
   prepare sumAssessmentByPropYearNumber from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Error preparing sumAssessmentByPropYearNumber. Status:", "Prepare 829 - txvalues")
      let ST_CAN_RUN = FALSE
      return
   end if

   # sumExemptionByProperty
   let prepchar = " SELECT ISNULL(SUM(txcstexm.txce_amt), 0) ",
                  "   FROM txcstexm ",
                  " INNER JOIN txcsttag ON txcsttag.txtg_acct = txcstexm.txce_acct ",
                  "                    AND txcsttag.txtg_ar_cat = txcstexm.txce_ar_cat ",
                  "                    AND txcsttag.txtg_own = 'P' ",
                  "                    AND txcsttag.txtg_prop_id = txcstexm.txce_prop_id ",
                  "  WHERE txcstexm.txce_ar_cat = 25 ",
                  "    AND txcstexm.txce_prop_id = ? "
   prepare sumExemptionByProperty from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Error preparing sumExemptionByProperty. Status:", "Prepare 845 - txcstexm")
      let ST_CAN_RUN = FALSE
      return
   end if

   # sumExemptionByYearAndNumber
   let prepchar = " SELECT ISNULL(SUM(txexemps.txxm_amt),0) ",
                  "   FROM txexemps ",
                  " INNER JOIN txowners ON txowners.txon_mt_sw = txexemps.txxm_mt_sw ",
                  "                    AND txowners.txon_ar_cat = txexemps.txxm_ar_cat ",
                  "                    AND txowners.txon_prop_id = txexemps.txxm_prop_id ",
                  "                    AND txowners.txon_year = txexemps.txxm_year ",
                  "                    AND txowners.txon_list_no = txexemps.txxm_list_no ",
                  "                    AND txowners.txon_acct = txexemps.txxm_acct ",
                  "                    AND txowners.txon_own = 'P' ",
                  "  WHERE txexemps.txxm_mt_sw = 1 ",
                  "    AND txexemps.txxm_ar_cat = 25 ",
                  "    AND txexemps.txxm_year = ? ",
                  "    AND txexemps.txxm_list_no = ? ",
                  "    AND txexemps.txxm_prop_id = ? "
   prepare sumExemptionByYearAndNumber from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Error preparing sumExemptionByYearAndNumber. Status:", "Prepare 867 - txcstexm")
      let ST_CAN_RUN = FALSE
      return
   end if

   # sumMotorVehicleAssessmenByYearAndNumber
   let prepchar = "SELECT ISNULL(SUM(mvamvmst.mvam_assessment),0) ",
                  "  FROM mvamvmst ",
                  " WHERE mvamvmst.mvam_mt_sw = ? ",
                  "   AND mvamvmst.mvam_ar_cat = 25 ",
                  "   AND mvamvmst.mvam_tax_yr = ? ",
                  "   AND mvamvmst.mvam_list_no = ? ",
                  "   AND mvamvmst.mvam_prop_id = ? "
   prepare sumMotorVehicleAssessmenByYearAndNumber from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Error preparing sumMotorVehicleAssessmenByYearAndNumber. Status:", "Prepare 912 - mvamvmst")
      let ST_CAN_RUN = FALSE
      return
   end if

   # getBillType
   let prepchar = "SELECT ISNULL(arbilhdr.arbh_bill_type, ' ') ",
                  "  FROM arbilhdr ",
                  " WHERE arbilhdr.arbh_ar_cat = 25 ",
                  "   AND arbilhdr.arbh_year = ? ",
                  "   AND arbilhdr.arbh_bill = ? "
   prepare getBillType from prepchar
   if sqlca.sqlcode then
      call spsyserr("Error preparing getBillType. Status:", "Prepare 880 - arbilhdr")
      let ST_CAN_RUN = FALSE
      return
   end if

   # getAllPriorBillsCursor
   let prepchar = "SELECT ISNULL(arbilhdr.arbh_year, 0) ",
                  "      ,ISNULL(arbilhdr.arbh_bill, 0) ",
                  "      ,ISNULL(arbilhdr.arbh_bill_type, ' ') ",
                  "  FROM arbilhdr ",
                  " WHERE arbilhdr.arbh_ar_cat = 25 ",
                  "   AND arbilhdr.arbh_year != ? ",
                  "   AND arbilhdr.arbh_prop_cd = ? "
   prepare getAllPriorBills from prepchar
   if sqlca.sqlcode then
      call spsyserr("Error preparing getAllPriorBills. Status:", "Prepare 895 - arbilhdr")
      let ST_CAN_RUN = FALSE
      return
   else
      declare getAllPriorBillsCursor cursor with hold for getAllPriorBills
   end if

# select_txchgs

   let prepchar = "select txch_chg_def, txch_net_assmt, ",
                         "txch_count, txch_amt ",
                    "from txchgdef ",
                   "where txch_mt_sw   = ? ",
                     "and txch_ar_cat  = 25 ",
                     "and txch_prop_id = ? ",
                     "and txch_year    = ? ",
                     "and txch_list_no = ?"
   prepare get_taxdef from prepchar
   if (sqlca.sqlcode !=0) then
      let err_msg = "Error preparing select from txchgdef. Status:"
      call spsyserr(err_msg, "Prepare 670 - txchgdef")
      let ST_CAN_RUN = FALSE
      return
   else
      declare select_txchgs cursor for get_taxdef
   end if

# select_txchgs_j

   let prepchar = "select * ",
                    "from txchgdef ",
                   "where txch_mt_sw   = ? ",
                     "and txch_ar_cat  = 25 ",
                     "and txch_prop_id = ? ",
                     "and txch_year    = ? ",
                     "and txch_list_no = ? ",
                     "and txch_chg_def = ?"
   prepare get_taxdef_j from prepchar
   if (sqlca.sqlcode !=0) then
      let err_msg = "Error preparing select from txchgdef. Status:"
      call spsyserr(err_msg, "Prepare 6701 - txchgdef")
      let ST_CAN_RUN = FALSE
      return
   else
      declare select_txchgs_j cursor for get_taxdef_j
   end if

# select_txem1

  let prepchar = "select txxm_exem, txxm_amt from txexemps ",
                  "where txxm_mt_sw   = ? ",
                    "and txxm_ar_cat  = 25 ",
                    "and txxm_prop_id = ? ",
                    "and txxm_acct    = ? ",
                    "and txxm_year    = ? ",
                    "and txxm_list_no = ?"
   prepare get_txem1 from prepchar
   if (sqlca.sqlcode != 0) then
      let err_msg = "Error preparing select from txexemps. Status:"
      call spsyserr(err_msg, "Prepare 683 - txexemps")
      let ST_CAN_RUN = FALSE
      return
   else
      declare select_txem1 cursor for get_txem1
   end if

# select_txce1

  let prepchar = "select txce_exem, txce_amt from txcstexm ",
                  "where txce_acct    = ? ",
                    "and txce_ar_cat  = 25 ",
                    "and txce_prop_id = ?"
   prepare get_txce1 from prepchar
   if (sqlca.sqlcode !=0) then
      let err_msg = "Error preparing select from txcstexm. Status:"
      call spsyserr(err_msg, "Prepare 696 - txcstexm")
      let ST_CAN_RUN = FALSE
      return
   else
      declare select_txce1 cursor for get_txce1
   end if

# select_txval

   let prepchar = "select txva_class, txva_amt from txvalues ",
                   "where txva_mt_sw   = ? ",
                     "and txva_ar_cat  = 25 ",
                     "and txva_prop_id = ? ",
                     "and txva_year    = ? ",
                     "and txva_list_no = ?"
   prepare get_txval from prepchar
   if (sqlca.sqlcode !=0) then
      let err_msg = "Error preparing select from txvalues. Status:"
      call spsyserr(err_msg, "Prepare 712 - txvalues")
      let ST_CAN_RUN = FALSE
      return
   else
      declare select_txval cursor for get_txval
   end if

   # select_ppval

   let prepchar = "select txpv_purch_yr, txpv_purch_amt, txpv_pct_good, txpv_curr_value, txpv_acquisitions, txpv_removals ",
                  "from txppvals ",
                  "where txpv_mt_sw = ? ",
                  "and txpv_ar_cat = 25 ",
                  "and txpv_prop_id = ? ",
                  "and txpv_year = ? ", 
                  "and txpv_list_no = ? ",
                  "AND txpv_seq = ? "
   prepare get_ppval from prepchar
   if (sqlca.sqlcode != 0) then
      let err_msg = "Error preparing select from txvalues for ppvalues. Status:"
      call spsyserr(err_msg, "Prepare "||__LINE__||" - txvalues")
      let ST_CAN_RUN = FALSE
      return
   else
      declare select_ppval cursor for get_ppval
   end if

# update_mvamjurs

   let prepchar = "update mvamvmst ",
                     "set mvam_psd = ? ",
                   "where mvam_tax_yr  = ? ",
                     "and mvam_prop_id = ? ",
                     "and mvam_list_no = ?"
  prepare update_mvamjurs from prepchar
  if (sqlca.sqlcode != 0) then
     call spsyserr("Prepare update_mvamjurs failed.",
                   "Prepare 765")
     let ST_CAN_RUN = FALSE
     return
  end if

#spspccom cursor(s)
   let prepchar = " select * ",
                  " from spspccom ",
                  "  where spcc_cust = ? "
   prepare sel_sp1 from prepchar
   if (sqlca.sqlcode != 0) then
      call spsyserr("Prepare (sel_sp1) failed.", "Prepare 879.")
      let ST_CAN_RUN = FALSE
   end if
   declare sp_cursor1 cursor for sel_sp1

   let prepchar = " select * ",
                  " from spspccom ",
                  " where spcc_ar_cat = ? ",
                  "  and spcc_year = ? ",
                  "  and spcc_bill = ? "
   prepare sel_sp2 from prepchar
   if (sqlca.sqlcode != 0) then
      call spsyserr("Prepare (sel_sp2) failed.", "Prepare 891.")
      let ST_CAN_RUN = FALSE
   end if
   declare sp_cursor2 cursor for sel_sp2

   let prepchar = " select * ",
                  " from spspccom ",
                  " where spcc_ar_cat = ? ",
                  "  and spcc_prop_id = ? "
   prepare sel_sp3 from prepchar
   if (sqlca.sqlcode != 0) then
      call spsyserr("Prepare (sel_sp3) failed.", "Prepare 902.")
      let ST_CAN_RUN = FALSE
   end if
   declare sp_cursor3 cursor for sel_sp3

   create temp table tmphist
      (
      gl_year     smallint,
      yd          char(4),
      h_type      char(1),
      list_no     integer,
      assmt       decimal(11,0),
      exem        decimal(11,0),
      net         decimal(11,0)
      ) with no log
   if (sqlca.sqlcode != 0) then
      let err_msg = "Error creating temp table. Status:"
      call spsyserr(err_msg, "Create 641 - tmphist")
      let ST_CAN_RUN = FALSE
      return
   end if

# insert_tmphist

   let prepchar = "insert into tmphist values (?,?,?,?,?,?,?) "
   prepare insert_tmphist from prepchar
   if (sqlca.sqlcode) then
      let err_msg = "Error preparing insert to temp table. Status:"
      call spsyserr(err_msg, "Prepare 712 - tmphist")
      let ST_CAN_RUN = FALSE
      return
   end if

   let prepchar = "select count(*) from txppmast ",
                   "where txppmast.txpp_mt_sw != 0 ",
                     "and txppmast.txpp_id_no = ? "
   prepare cnt_others from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Prepare cnt_others failed.", "Prepare cnt_others 1145")
      let ST_CAN_RUN = FALSE
      return
   else
      declare count_others cursor for cnt_others
   end if

   # AccountsReceivable.CustomerTypes cursor(s)
   let prepchar = "SELECT CustomerTypes.Id, CustomerTypes.Description ",
                    "FROM AccountsReceivable.CustomerTypes "
   prepare sel_CustomerTypes from prepchar
   if (sqlca.sqlcode) then
      call spsyserr("Prepare getCustomerTypeDescriptions failed.", "Prepare getCustomerTypeDescriptions, Line: " || __LINE__)
      let ST_CAN_RUN = FALSE
      return
   else
      declare getCustomerTypeDescriptions cursor for sel_CustomerTypes
   end if

   call browse_init()
   call addl_init()

   if (ST_CLIENT_STATE = "MA" or
       ST_CLIENT_STATE = "MD" or
       ST_CLIENT_STATE = "NY" or
       ST_CLIENT_STATE = "CT" or
       ST_CLIENT_STATE = "NH" or
       ST_CLIENT_STATE = "MI" or
       ST_CLIENT_STATE = "RI" or
       ST_CLIENT_STATE = "LA" or
       ST_CLIENT_STATE = "VT") then
      if (NOT prep_calchg(ST_USER)) then
         let ST_CAN_RUN = FALSE
      end if
   end if

   call txinitem_dummy()

end function
###########################################################################
private function checkViewPermissions()

   call spdomlib_hideAllByTag("VA_SSN_only", not ar_user_viewSSN)

end function
###########################################################################
private function checkEntryPermissions()

   call spdomlib_hideAllByTag("VA_SSN_only", not ar_user_enterSSN)

end function
###########################################################################
function hide_groups()

   select txyp_use_loc
     into use_cen_addr
     from txyrparm
    where txyp_ar_cat = 25
      and txyp_year = assm_year
      and txyp_cycle_comm = 0
   if (sqlca.sqlcode or use_cen_addr IS NULL) then
      let use_cen_addr = "N"
   end if

   if (use_cen_addr = "N") then
      call spdomlib_hideAllByTag("cal_grp", TRUE)
      call spdomlib_hideAllByTag("rl_grp", FALSE)
   else
      call spdomlib_hideAllByTag("cal_grp", FALSE)
      call spdomlib_hideAllByTag("rl_grp", TRUE)
   end if

end function
###########################################################################
function main_menu()
   define char1,
          char2,
          char3,
          char4,
          char5,
          char6 char(1),
          lv_ssnOrFid char(14),
          lv_answer,
          lv_errorMessage,
          lv_message string,
          lv_customerDeliveryAddressStatusActive,
          lv_isPerson boolean,
          tmp_changes_made,
          i,
          lv_recordNotFound,
          lv_recordNumber,
          lv_success smallint,
          tmp_count integer,
          tmp_txcsttag record like txcsttag.*,
          tmp_txowners record like txowners.*,
          tmp_val_array dynamic array of record
             invalid_field string,
             invalid_reason string
          end record

   define attachParams Attachments.InterfaceParameters

   # Initialize local variable(s)...
   let lv_answer = " "
   let lv_errorMessage = " "
   let lv_isPerson = TRUE
   let lv_message = " "
   let lv_recordNotFound = FALSE
   let lv_ssnOrFid = " "
   let lv_success = TRUE
   
   # Initialize global variable(s)...
   let prog_timeout = FALSE

   if (NOT g_disp_values_array.getLength()) then
      let g_disp_values_array[1].classCode = ""
      let g_disp_values_array[1].classDescription = ""
      let g_disp_values_array[1].count = 0
      let g_disp_values_array[1].currentYearAssessment = 0
      let g_disp_values_array[1].lastYearAssessment = 0
   end if

   display array g_disp_values_array to sa_disp_values.*
         attributes (count = g_disp_values_array.getLength(), ACCEPT = FALSE, CANCEL = FALSE)

      before display
         call spdomlib_decorateAction("assessment", "Assessment", "Displays a history of assessments.", "", "A", "auto")
         call spdomlib_decorateAction("bill_inquiry", "Bill Inquiry", "Runs Bill Inquiry.", "", "B", "auto")
         call spdomlib_decorateAction("charges", "Charges", "Displays a list of charge code records for the selected data record.", "", "C", "auto")
         call spdomlib_decorateAction("motor_vehicles", "Motor Vehicles", "Finds motor vehicles for the selected data record.", "", "M", "auto")
         call spdomlib_decorateAction("make_tax", "Make Tax", "Creates tax year table entries for the current property.", "", "T", "auto")
         call spdomlib_decorateAction("make_working", "Make Working", "Creates a working record from the current record.", "", "W", "auto")
         call spdomlib_decorateAction("misc_fields", "Miscellaneous Fields", "Shows miscellaneous fields associated with the property.", "", "F", "auto")
         call spdomlib_decorateAction("ownership", "Ownership", "Displays tag records for the current data record.", "", "O", "auto")
         call spdomlib_decorateAction("values", "Values", "Displays assessed values records for the selected data record.", "", "V", "auto")
         call spdomlib_decorateAction("text", "Text", "Enter notes on this personal property record.", "", "E", "auto")
         call spdomlib_decorateAction("view_audit", "View Audit", "Shows audit records for selected property.", "", "I", "auto")
         call spdomlib_decorateAction("xml_output", "XML Output", "Output selected records in XML format.", "", "X", "auto")

           #Tyler Notify
           #   Use spnotlib_hideNotify to determine if the Notify
           #   option should be completely hidden from the user.
           #   Currently not hiding it to make users aware of this new feature
           if (spnotlib_hideNotify(ST_USER)) then
              call DIALOG.setActionActive("mu_notify", FALSE)
              call DIALOG.setActionHidden("mu_notify", TRUE)
           end if

         if (public_access = "Y") then
            call spdomlib_setActionActive("note_sp_r_btn", 0)
         else
            call spdomlib_setActionActive("note_sp_r_btn", 1)
         end if
         if (pass_let = "N") then
            # If entering through listing entry's listing mode, take the user directly to the values screen...
            if (record_count) then
               call txvalmnt(gr_txppmast.txpp_mt_sw, 25,
                           gr_txppmast.txpp_id_no,
                           gr_txppmast.txpp_year,
                           gr_txppmast.txpp_list_no, pass_let, " ") returning tmp_changes_made
               call display_record(FALSE)
               if (prog_timeout) then
                  exit display
               end if
            end if
         else
            call spdomlib_setBtnDataImage("adjust_r_btn", 0)
            call spdomlib_setBtnDataImage("note_sp_r_btn", 0)
            call spdomlib_setBtnDataImage("exem_r_btn", 0)
         end if

         #Check to see if TCM installed.
         call Dialog.setActionActive("image",(sptylrcm_installed()==1))

         if (pass_let = "E") then
            # If entering through listing entry's "Add PP" button, go directly into the add routine...
            call add_routine()
            # Immediately make a working record...
            if (NOT cancelled) then
               if (ST_CLIENT_STATE != "MA" and
                   ST_CLIENT_STATE != "MD" and
                   ST_CLIENT_STATE != "NY" and
                   ST_CLIENT_STATE != "CT" and
                   ST_CLIENT_STATE != "NH" and
                   ST_CLIENT_STATE != "MI" and
                   ST_CLIENT_STATE != "RI" and
                   ST_CLIENT_STATE != "LA" and
                   ST_CLIENT_STATE != "VT") then
                  call make_working()
               else
                  call make_tax()
               end if
            end if
            exit display
         end if

         if (pass_let = "S") then
# Hide add and delete when coming from corrections/subsequent...
            call dialog.setActionActive("insert", FALSE)
            call dialog.setActionActive("delete", FALSE)
         end if

         if (pass_let = "L") then
# Conditionally hide add and delete when coming from BL Master...
            if (pass_prop = "BLANK") then
               call add_routine()
               if (WHDL_CLOSE) then
                  let WHDL_CLOSE = FALSE
                  exit display
               end if
            else
               call dialog.setActionActive("insert", FALSE)
               call dialog.setActionActive("delete", FALSE)
            end if
         end if

         if (disable_find_option) then
# Hide the find and fetch options if a single record was already found...
            call dialog.setActionActive("find", FALSE)
            call dialog.setActionActive("nav_find", FALSE)
            call dialog.setActionActive("nav_first", FALSE)
            call dialog.setActionActive("nav_next", FALSE)
            call dialog.setActionActive("nav_last", FALSE)
            call dialog.setActionActive("nav_previous", FALSE)
         end if

         if (ST_CLIENT_STATE != "VA") then
            # Show MVs only for VA...
            call dialog.setActionActive("motor_vehicles", FALSE)
            call dialog.setActionHidden("motor_vehicles", TRUE)
         else
            # Don't show exemptions for VA...
            call dialog.setActionActive("exem_r_btn", FALSE)
            call dialog.setActionHidden("exem_r_btn", TRUE)
         end if

         if ((ST_CLIENT_STATE != "CT") and (proc_stat < 1 or proc_stat > 8)) or
            ((ST_CLIENT_STATE = "CT") and  (proc_stat < 1 or proc_stat > 2)) then
# Only show the make tax/working buttons if it's allowed...
            call dialog.setActionActive("make_tax", FALSE)
            call dialog.setActionHidden("make_tax", TRUE)
            call dialog.setActionActive("make_working", FALSE)
            call dialog.setActionHidden("make_working", TRUE)
         end if

         if ((ST_CLIENT_STATE != "CT") and
             (proc_stat = 0) and
             (instmt_meth = 2) and
             (pp_stat < 3)) then
            call dialog.setActionActive("make_tax", TRUE)
            call dialog.setActionHidden("make_tax", FALSE)
         end if

         if (pass_let = "I") then
# Hide options for inquiry mode...
            call dialog.setActionActive("make_tax", FALSE)
            call dialog.setActionHidden("make_tax", TRUE)
            call dialog.setActionActive("make_working", FALSE)
            call dialog.setActionHidden("make_working", TRUE)
            call dialog.setActionActive("insert", FALSE)
            call dialog.setActionActive("update", FALSE)
            call dialog.setActionActive("delete", FALSE)
            # Only hide the View Audit button if the user is restricted to "public access only"...
            if (public_access = "Y") then
               call dialog.setActionActive("view_audit", FALSE)
               call dialog.setActionHidden("view_audit", TRUE)
            end if
         else
            call dialog.setActionActive("bill_inquiry", FALSE)
            call dialog.setActionHidden("bill_inquiry", TRUE)
         end if

         if (ST_CLIENT_STATE != "MA" and
             ST_CLIENT_STATE != "MD" and
             ST_CLIENT_STATE != "NY" and
             ST_CLIENT_STATE != "CT" and
             ST_CLIENT_STATE != "NH" and
             ST_CLIENT_STATE != "MI" and
             ST_CLIENT_STATE != "RI" and
             ST_CLIENT_STATE != "LA" and
             ST_CLIENT_STATE != "VT") then
# mt_sw=1 states get Make Tax...
            call dialog.setActionActive("make_tax", FALSE)
            call dialog.setActionHidden("make_tax", TRUE)
         else
# mt_sw=9 states get Make Working...
            call dialog.setActionActive("make_working", FALSE)
            call dialog.setActionHidden("make_working", TRUE)
         end if

      before row
         let lv_recordNumber = arr_curr()

      on idle ST_IDLE
         if (IdleProcessor.TimeoutExpired()) then
            call close_cursor()
            let int_flag = FALSE
            exit display
         end if

      on action image
         call sptylrcm_openTCM()

      on action find
         call sperrorm_blankmess()
         if (pass_let = "I") then
            call find_routine(5)
            if (WHDL_CLOSE) then
               let WHDL_CLOSE = FALSE
               exit display
            end if
         else
            call find_routine(1)
            if (WHDL_CLOSE) then
               let WHDL_CLOSE = FALSE
               exit display
            end if
         end if
         if record_count then
            call display_record(TRUE)
         else
            call spdomlib_setBtnDataImage("adjust_r_btn", 0)
            call spdomlib_setBtnDataImage("note_sp_r_btn", 0)
            call spdomlib_setBtnDataImage("exem_r_btn", 0)
         end if

      on action nav_find
         call sperrorm_blankmess()
         if (pass_let = "I") then
            call find_routine(5)
            if (WHDL_CLOSE) then
               let WHDL_CLOSE = FALSE
               exit display
            end if
         else
            call find_routine(1)
            if (WHDL_CLOSE) then
               let WHDL_CLOSE = FALSE
               exit display
            end if
         end if
         if record_count then
            call display_record(TRUE)
         else
            call spdomlib_setBtnDataImage("adjust_r_btn", 0)
            call spdomlib_setBtnDataImage("note_sp_r_btn", 0)
            call spdomlib_setBtnDataImage("exem_r_btn", 0)
         end if

      on action nav_first
         call sperrorm_blankmess()
         if (record_count) then
            let fetchno = 1
            call fetch_record()
            call display_record(TRUE)
         else
            call spnodata()
         end if

      on action nav_next
         call sperrorm_blankmess()
         if (record_count) then
            if (fetchno < record_count) then
               let fetchno = fetchno + 1
               call fetch_record()
               call display_record(TRUE)
            else
               call spnonext()
            end if
         else
            call spnodata()
         end if

      on action nav_previous
         call sperrorm_blankmess()
         if (record_count) then
            if (fetchno > 1) then
               let fetchno = fetchno - 1
               call fetch_record()
               call display_record(TRUE)
            else
               call spnoprev()
            end if
         else
            call spnodata()
         end if

      on action nav_last
         call sperrorm_blankmess()
         if (record_count) then
            let fetchno = record_count
            call fetch_record()
            call display_record(TRUE)
         else
            call spnodata()
         end if

      on action browse
         call sperrorm_blankmess()
         if (record_count) then
            if (pass_let != 'I') then
               let fetchno = browse_routine(1, record_count)
            else
               let fetchno = browse_routine(3, record_count)
            end if
         else
            call spnodata()
         end if

      on action nav_browse
         call sperrorm_blankmess()
         if (record_count) then
            if (pass_let != 'I') then
               let fetchno = browse_routine(1, record_count)
            else
               let fetchno = browse_routine(3, record_count)
            end if
         else
            call spnodata()
         end if

      on action exchange
         call sperrorm_blankmess()
         if (NOT find_cursor_open) then
            call spnodata()
         else
            if (exchange_routine()) then
               call spsyserr("'spexcmnt' link Error. Unable to maintain appointments.", " ")
            end if
         end if

      on action link_email
         call sperrorm_blankmess()
         if (NOT find_cursor_open) then
            call spnodata()
         else
            call email_routine()
         end if

      on action accept
         call sperrorm_blankmess()
         if get_record() then
            call sperrorm("No value information available since property has been deleted.", 0, 1)
         else
            if (record_count) then
               # Determine which row is selected...
               if (g_disp_values_array.getLength()) then
                  for i = 1 to g_disp_values_array.getLength()
                     if (DIALOG.isRowSelected("sa_disp_values", i)) then
                        exit for
                     end if
                  end for
               end if

               # Determine the class code of the row selected...
               if (g_disp_values_array[i].classDescription == "Last year value(s) removed") then
                  # Display the last year's value(s) removed...
                  call txvalmnt(gr_txppmast.txpp_mt_sw, 25, gr_txppmast.txpp_id_no, gr_txppmast.txpp_year, gr_txppmast.txpp_list_no, "P", " ") 
                      returning tmp_changes_made
               else
                  # Display the current year's values...
                  call txvalmnt(gr_txppmast.txpp_mt_sw, 25, gr_txppmast.txpp_id_no, gr_txppmast.txpp_year, gr_txppmast.txpp_list_no, pass_let, " ") 
                      returning tmp_changes_made
               end if
               call display_record(FALSE)
            else
               call spnodata()
            end if
            if (prog_timeout) then
               exit display
            end if
         end if

      on action insert
         if (bl_add = TRUE and pass_prop = "BLANK") then
            let dlg_str = "You have already added a property for \n",
                          "this business."
            let user_ansr = spaskrtn_dialog("Property Already Added",
                                            dlg_str,
                                            "OK",
                                            "info",
                                            "OK")
            call spnooption()
            continue display
         end if
         call sperrorm_blankmess()
         if (ct_sw = "T" and pass_let != "E") then

            if use_mt_sw = 9 then
               let dlg_str = " Adding working records is not allowed.\n ",
                             "Would you like to add a current record?"
            else
               let dlg_str = " Adding tax records is not allowed.\n ",
                             "Would you like to add a current record?"
            end if
            let user_ansr = spaskrtn_dialog("Add Current Record",
                                            dlg_str,
                                            "Yes|No",
                                            "question",
                                            "Y")
            if (user_ansr = "Yes") then
               let ct_sw = "C"
               call add_routine()
               if (WHDL_CLOSE) then
                  let WHDL_CLOSE = FALSE
                  exit display
               end if
               if (record_count) then
                  call display_record(FALSE)
               end if
            end if
         else
            call add_routine()
            if (pass_let = "E") then
               if (ST_CLIENT_STATE != "MA" and
                   ST_CLIENT_STATE != "MD" and
                   ST_CLIENT_STATE != "NY" and
                   ST_CLIENT_STATE != "CT" and
                   ST_CLIENT_STATE != "NH" and
                   ST_CLIENT_STATE != "MI" and
                   ST_CLIENT_STATE != "RI" and
                   ST_CLIENT_STATE != "LA" and
                   ST_CLIENT_STATE != "VT") then
                  call make_working()
               else
                  call make_tax()
               end if
               exit display
            end if
            if (WHDL_CLOSE) then
               let WHDL_CLOSE = FALSE
               exit display
            end if
            if (record_count) then
               call display_record(FALSE)
            end if
         end if

      on action update
         call sperrorm_blankmess()
         if (record_count and NOT get_record()) then
            if (check_permit()) then
               call update_routine()
               if (WHDL_CLOSE) then
                  let WHDL_CLOSE = FALSE
                  exit display
               end if
               call display_record(FALSE)
            end if
         else
            call spnodata()
         end if

      on action delete
         call sperrorm_blankmess()
         if (record_count) then
            if (check_permit()) then
               call delete_routine()
               call display_record(FALSE)
            end if
         else
            call spnodata()
         end if

      on action word
         if (spfeatur("GROUP1")) then
            call sperrorm_blankmess()
            if (NOT find_cursor_open) then
               call find_routine(1)
               if (WHDL_CLOSE) then
                  exit display
               end if
               if (NOT find_cursor_open) then
                  call spnodata()
                  continue display
               end if
               call display_record(TRUE)
            end if
            call print_export("Word")
         end if

      on action excel
         if (spfeatur("GROUP1")) then
            call sperrorm_blankmess()
            if (NOT find_cursor_open) then
               call find_routine(1)
               if (WHDL_CLOSE) then
                  exit display
               end if
               if (NOT find_cursor_open) then
                  call spnodata()
                  continue display
               end if
               call display_record(TRUE)
            end if
            call print_export("Excel")
         end if

      on action attach
         call sperrorm_blankmess()
         if (NOT find_cursor_open) then
            call spnodata()
            continue display
         end if
         initialize attachParams.* to null
         let attachParams.RecordAttachment.Context = "txppmast"
         # all attachments will be held on the current record now
         let attachParams.RecordAttachment.Keys[1] = "0", # mt_sw
                                                     active_id_no using "&&&&&&&&&&",
                                                     "0000", # year
                                                     "000000000" # list_no
         call Attachments.Dialog(attachParams.*)
         call Attachments.ShowCount(attachParams.*)

      on action nav_attach
         call sperrorm_blankmess()
         if (NOT find_cursor_open) then
            call spnodata()
            continue display
         end if
         initialize attachParams.* to null
         let attachParams.RecordAttachment.Context = "txppmast"
         # all attachments will be held on the current record now
         let attachParams.RecordAttachment.Keys[1] = "0", # mt_sw
                                                     active_id_no using "&&&&&&&&&&",
                                                     "0000", # year
                                                     "000000000" # list_no
         call Attachments.Dialog(attachParams.*)
         call Attachments.ShowCount(attachParams.*)

      # Tyler Notify
      #   Add a Notify option to the menu.
      #   Requirements for executing are similar to those of the Export option,
      #   which can be used to generate notices as well.
      #
      #   spnotlib_notifyAvailable function indicates whether Tyler Notify is configured and
      #   available to the current user.  If not available, a dialog box is popped up unless silent
      #   mode (second) parameter is true.
      on action mu_notify
         call sperrorm_blankmess()
         if (NOT spnotlib_notifyAvailable(ST_USER, false)) then
            continue display
         end if
         if (NOT find_cursor_open) then
            call spnodata()
            continue display
         end if
         if (g_notify_complete == "Y") then
            let notify_err_msg = "You have already submitted notifications for this data set.\n",
                                 "Do you really want to go through the notifications process again?\n"
            call spaskrtn_dialog("***WARNING***",
                                 err_msg clipped,
                                 "Yes|No",
                                 "warning", "") returning user_ansr
            if (user_ansr != "Yes") then
               continue display
            end if
         end if
         call notify_routine("menu")

      on action assessment
         call sperrorm_blankmess()
         if get_record() then
            call sperrorm("No assessment information available since property has been deleted.", 0, 1)
         else
            if (record_count) then
               call assmt_history_routine()
            else
               call spnodata()
            end if
            if (prog_timeout) then
               exit display
            end if
         end if

      on action bill_inquiry
         call sperrorm_blankmess()
         if (gr_txppmast.txpp_list_no = 0) then
            call sperrorm("No bills exist for this record.",0,1)
            continue display
         end if
         if get_record() then
            call sperrorm("No billing information is retrievable since property has been deleted.", 0, 1)
         else
            if (record_count) then
               let err_msg = ST_TODAY using "mmddyyyy",
                       gr_txppmast.txpp_year using "####", "025",
                             gr_txppmast.txpp_list_no using "&&&&&&&&"
               call sprunpgm("I", "arbilinq", err_msg)
            else
               call spnodata()
            end if
            if (prog_timeout) then
               exit display
            end if
         end if

      on action charges
         call sperrorm_blankmess()
         if get_record() then
            call sperrorm("No charge information available since property has been deleted.", 0, 1)
         else
            if (record_count) then
               call txchgmnt(gr_txppmast.txpp_mt_sw, 25,
                             gr_txppmast.txpp_id_no,
                             gr_txppmast.txpp_year,
                             gr_txppmast.txpp_list_no, pass_let) returning tmp_changes_made
            else
               call spnodata()
            end if
            if (prog_timeout) then
               exit display
            end if
         end if

      on action motor_vehicles
         call sperrorm_blankmess()
         if get_record() then
            call sperrorm("No motor vehicle information available since property has been deleted.", 0, 1)
         else
            if (record_count) then
               call browse_routine(6,0) returning i
               call display_record(false)
            else
               call spnodata()
            end if
            if (prog_timeout) then
               exit display
            end if
         end if

      on action make_tax
         call sperrorm_blankmess()
         if get_record() then
            call sperrorm("Table entries cannot be created since property has been deleted.", 0, 1)
         else
            if (record_count) then
               if (ct_sw = "C") then
                  call make_tax()
               else
                  let err_msg = "Tax records can only be created from current ",
                                "records."
                  call sperrorm(err_msg, 0, 2)
               end if
            else
               call spnodata()
            end if
         end if

      on action make_working
         call sperrorm_blankmess()
         if get_record() then
            call sperrorm("Make working can't be performed since parcel has been delete.", 0, 1)
         else
            call make_working()
         end if

      on action misc_fields
         call sperrorm_blankmess()
         if get_record() then
            call sperrorm("No miscellaneous fields information available since property has been deleted.", 0, 1)
         else
            if (record_count) then
               call txmiscfm(25,
                             gr_txppmast.txpp_mt_sw,
                             gr_txppmast.txpp_id_no,
                             gr_txppmast.txpp_year,
                             gr_txppmast.txpp_list_no,
                             pass_let) returning tmp_changes_made
            else
               call spnodata()
            end if
            if (prog_timeout) then
               exit display
            end if
         end if

      on action ownership
         call sperrorm_blankmess()
         if get_record() then
            call sperrorm("No ownership information available since property has been deleted.", 0, 1)
         else
            if (record_count) then
               call txownmnt(gr_txppmast.txpp_mt_sw, 25,
                             gr_txppmast.txpp_id_no,
                             gr_txppmast.txpp_year,
                             gr_txppmast.txpp_list_no, pass_let) returning tmp_changes_made
               # The following logic may have to exist in txownmnt to account for all instances of where this routine is called...
               if (tmp_changes_made) then
                  call get_customer(gr_txppmast.txpp_id_no, gr_txppmast.txpp_year, gr_txppmast.txpp_list_no) returning lv_customerDeliveryAddressStatusActive
                  if (lv_customerDeliveryAddressStatusActive) then
                     call spdomlib_hideNodeByTag("Label", "cur_customer_status_lbl_tag", true)
                  else
                     call spdomlib_hideNodeByTag("Label", "cur_customer_status_lbl_tag", false)
                  end if

                  # Obtain personal data...
                  call CustomerManager.getDeliveryAddressSSNorFID(cust_acct, addl_no, 1) returning lv_ssnOrFid, lv_isPerson
                  if (ExceptionState.hasException()) then
                     if (NOT ExceptionState.hasNotFoundSQLException()) then
                        call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_WRITESYSTEMLOG + ExceptionHandler.c_DISPLAYERRORLINE, "Error calling getDeliveryAddressSSNorFID() for Customer ID: "||cust_acct||" and delivery addr: "||addl_no||". Line: "||__LINE__)
                     end if
                  end if
                  # Defensive programming...
                  if (lv_ssnOrFid is NULL) then
                     let lv_ssnOrFid = " "
                  end if

                  # Check to see if the SSN listed on the property differs from the SSN of the primary owner's customer record...
                  if (ST_CLIENT_STATE == "VA") then
                     if (gv_customerSSN != lv_ssnOrFid) then
                        let lv_message = "The SSN/FID (" || gv_customerSSN clipped || ") listed on the property no longer matches\n",
                                         "the SSN/FID (" || lv_ssnOrFid clipped || ") of the primary owner's customer record.\n\n",
                                         "Would you like to update the property to match the customer?"
                        let lv_answer = spaskrtn_dialog("Customer SSN/FID", lv_message, "Yes|No", "question", "Yes")
                        if (lv_answer == "Yes") then
                           let gr_txppmast.txpp_old_id = lv_ssnOrFid
                        
                           # Need to update txppmast record...
                           call txppmastwr_update(gr_txppmast.*, TRUE, TRUE, tmp_val_array, TRUE, TRUE)
                              returning lv_success, lv_errorMessage
                           if (NOT lv_success) then
                              call spsyserr(lv_errorMessage, "txppmastwr_update, Line: " || __LINE__)
                           end if
                        end if
                     end if
                  end if
               end if
               call display_record(FALSE)
            else
               call spnodata()
            end if
            if (prog_timeout) then
               exit display
            end if
         end if

      on action values
         call sperrorm_blankmess()
         if get_record() then
            call sperrorm("No value information available since property has been deleted.", 0, 1)
         else
            if (record_count) then
               call txvalmnt(gr_txppmast.txpp_mt_sw, 25,
                             gr_txppmast.txpp_id_no,
                             gr_txppmast.txpp_year,
                             gr_txppmast.txpp_list_no, pass_let, " ") returning tmp_changes_made
               call display_record(FALSE)
            else
               call spnodata()
            end if
            if (prog_timeout) then
               exit display
            end if
         end if

      on action text
         call sperrorm_blankmess()
         if get_record() then
            call sperrorm("No text information available since property has been deleted.", 0, 1)
         else
            if (NOT record_count) then
               call spnodata()
               continue display
            else
               call text_routine()
            end if
         end if

      on action view_audit
         call sperrorm_blankmess()
         if get_record() then
            call sperrorm("No audit information available since property has been deleted.", 0, 1)
         else
            if (record_count) then
            let tmp_count = 0
            if (ct_sw = "C") then
               open current_audit_count using active_id_no,
                                              char_id_no
               if (sqlca.sqlcode) then
                  call spsyserr(sfmt("Error opening audit count for current. Line %1 ",__LINE__), sfmt("Open current_audit_count failed - %1",__LINE__))
               else
                  fetch current_audit_count into tmp_count
                  if (sqlca.sqlcode) then
                     call spsyserr(sfmt("Error fetching audit count for current. Line %1 ",__LINE__), sfmt("Fetch current_audit_count failed - %1",__LINE__))
                  end if
               end if
            else
               open tax_audit_count using active_id_no,
                                          char_id_no,
                                          active_mt_sw
               if (sqlca.sqlcode) then
                  call spsyserr(sfmt("Error opening audit count for tax. Line %1 ",__LINE__), sfmt("Open tax_audit_count failed - %1",__LINE__))
               else
                  fetch tax_audit_count into tmp_count
                  if (sqlca.sqlcode) then
                     call spsyserr(sfmt("Error fetching audit count for tax. Line %1 ",__LINE__), sfmt("Fetch tax_audit_count failed - %1",__LINE__))
                  end if
               end if
            end if



               if (check_int(tmp_count)) then
                  let tmp_count = browse_routine(2, tmp_count)
               else
                  call sperrorm("No audit records exist for this property.",0,2)
               end if
            else
               call spnodata()
            end if
            if (prog_timeout) then
               exit display
            end if
         end if

      on action xml_output
         call sperrorm_blankmess()
         if get_record() then
            call sperrorm("Create XML export file unavailable since parcel has been deleted.", 0, 1)
         else
            if record_count then
               call outputXML_main_menu()
            else
               call spnodata()
            end if
            if (prog_timeout) then
               exit display
            end if
         end if

      on action note_sp_r_btn
         call sperrorm_blankmess()
         if get_record() then
            call sperrorm("No special condition/notes information available since property has been deleted.", 0, 1)
         else
            if (record_count) then
               if (ct_sw = "C") then
                  # Select the primary owner...
                  call spmdebug_say("TXPPMAST: main_menu: gr_txppmast.txpp_id_no: ", gr_txppmast.txpp_id_no)
                  call txcsttagwr_selectPrimaryOwner(25, gr_txppmast.txpp_id_no)
                                           returning lv_success, lv_errorMessage, lv_recordNotFound, tmp_txcsttag.*

                  if (NOT lv_success) then
                     call spsyserr(lv_errorMessage, "txcsttagwr_selectForPropertry 1549")
                  else
                     # Check to see if a primary owner exists...
                     if (lv_recordNotFound) then
                        let lv_errorMessage = "No primary owner record found for property: ", gr_txppmast.txpp_id_no using "<<<<<<<<<&", "."
                        call sperrorm(lv_errorMessage, 0, 1)
                     end if
                  end if

                  case
                     # If the program is called in 'I'nquiry mode...
                     when (pass_let = "I")
                        # Call the arspclnk routine in Inquiry mode and exclude all notes...
                        call arspclnk("Y", "Y", "N",
                                      tmp_txcsttag.txtg_acct,
                                      tmp_txcsttag.txtg_ar_cat, 0, 0,
                                      tmp_txcsttag.txtg_prop_id, ' ', ST_TODAY)
                            returning char1, char2, char3, char4, char5, char6

                     # If the user is unable to view/maintain special conditions/notes...
                     when (glob_spec_view_notes = "N")
                        # Call the arspclnk routine in Inquiry mode and exclude all notes...
                        call arspclnk("Y", "Y", "N",
                                      tmp_txcsttag.txtg_acct,
                                      tmp_txcsttag.txtg_ar_cat, 0, 0,
                                      tmp_txcsttag.txtg_prop_id, ' ', ST_TODAY)
                            returning char1, char2, char3, char4, char5, char6

                     # If user is unable to update inactive records AND the record is inactive...
                     when (updt_inact = "N" and gr_txppmast.txpp_status = "I")
                        let err_msg = "You do not have permission to maintain inactive properties."
                        call sperrorm(err_msg, 0, 2)
                        # Call the arspclnk routine in Inquiry mode, but display all notes/special conditions...
                        call arspclnk("Y", "Y", "X",
                                      tmp_txcsttag.txtg_acct,
                                      tmp_txcsttag.txtg_ar_cat, 0, 0,
                                      tmp_txcsttag.txtg_prop_id, ' ', ST_TODAY)
                            returning char1, char2, char3, char4, char5, char6

                     otherwise
                        # Call the arspclnk routine in normal maintenance mode...
                        call arspclnk("Y", "Y", "Y",
                                      tmp_txcsttag.txtg_acct,
                                      tmp_txcsttag.txtg_ar_cat, 0, 0,
                                      tmp_txcsttag.txtg_prop_id, ' ', ST_TODAY)
                            returning char1, char2, char3, char4, char5, char6
                  end case
                  let disp_special = FALSE
                  call special_condition()
               end if
               if (ct_sw = "T") then
                  # Select the primary owner...
                  call spmdebug_say("TXPPMAST: main_menu: gr_txppmast.txpp_mt_sw: ", gr_txppmast.txpp_mt_sw)
                  call spmdebug_say("TXPPMAST: main_menu: gr_txppmast.txpp_id_no: ", gr_txppmast.txpp_id_no)
                  call spmdebug_say("TXPPMAST: main_menu: gr_txppmast.txpp_year: ", gr_txppmast.txpp_year)
                  call spmdebug_say("TXPPMAST: main_menu: gr_txppmast.txpp_list_no: ", gr_txppmast.txpp_list_no)
                  call txownerswr_selectPrimaryOwner(gr_txppmast.txpp_mt_sw,
                                                     25,
                                                     gr_txppmast.txpp_id_no,
                                                     gr_txppmast.txpp_year,
                                                     gr_txppmast.txpp_list_no)
                                           returning lv_success, lv_errorMessage, lv_recordNotFound, tmp_txowners.*
                  if (NOT lv_success) then
                     call spsyserr(lv_errorMessage, "txownerswr_selectForParent 1620")
                  else
                     # Check to see if a primary owner exists...
                     if (lv_recordNotFound) then
                        let lv_errorMessage = "No primary owner record found for property: ", gr_txppmast.txpp_id_no using "<<<<<<<<<&", "."
                        call sperrorm(lv_errorMessage, 0, 1)
                     end if
                  end if

                  case
                     # If the program is called in 'I'nquiry mode...
                     when (pass_let = "I")
                        # Call the arspclnk routine in Inquiry mode and exclude all notes...
                        call arspclnk("Y", "Y", "N",
                                      tmp_txowners.txon_acct,
                                      tmp_txowners.txon_ar_cat,
                                      tmp_txowners.txon_year,
                                      tmp_txowners.txon_list_no,
                                      tmp_txowners.txon_prop_id, ' ', ST_TODAY)
                            returning char1, char2, char3, char4, char5, char6

                     # If the user is unable to view/maintain special conditions/notes...
                     when (glob_spec_view_notes = "N")
                        # Call the arspclnk routine in Inquiry mode and exclude all notes...
                        call arspclnk("Y", "Y", "N",
                                      tmp_txowners.txon_acct,
                                      tmp_txowners.txon_ar_cat,
                                      tmp_txowners.txon_year,
                                      tmp_txowners.txon_list_no,
                                      tmp_txowners.txon_prop_id, ' ', ST_TODAY)
                            returning char1, char2, char3, char4, char5, char6

                     # If user is unable to update inactive records AND the record is inactive...
                     when (updt_inact = "N" and gr_txppmast.txpp_status = "I")
                        let err_msg = "You do not have permission to maintain inactive properties."
                        call sperrorm(err_msg, 0, 2)
                        # Call the arspclnk routine in Inquiry mode, but display all notes/special conditions...
                        call arspclnk("Y", "Y", "X",
                                      tmp_txowners.txon_acct,
                                      tmp_txowners.txon_ar_cat,
                                      tmp_txowners.txon_year,
                                      tmp_txowners.txon_list_no,
                                      tmp_txowners.txon_prop_id, ' ', ST_TODAY)
                            returning char1, char2, char3, char4, char5, char6

                     otherwise
                        # Call the arspclnk routine in normal maintenance mode...
                        call arspclnk("Y", "Y", "Y",
                                      tmp_txowners.txon_acct,
                                      tmp_txowners.txon_ar_cat,
                                      tmp_txowners.txon_year,
                                      tmp_txowners.txon_list_no,
                                      tmp_txowners.txon_prop_id, ' ', ST_TODAY)
                            returning char1, char2, char3, char4, char5, char6
                  end case
                  let disp_special = FALSE
                  call special_condition()
               end if
            else
               call spnodata()
            end if
            if (prog_timeout) then
               exit display
            end if
         end if

      on action exem_r_btn
         call sperrorm_blankmess()
         if get_record() then
            call sperrorm("No exemption information available since property has been deleted.", 0, 1)
         else
            if (record_count) then
               call txexmmnt(gr_txppmast.txpp_mt_sw,
                             25,
                             gr_txppmast.txpp_id_no,
                             gr_txppmast.txpp_year,
                             gr_txppmast.txpp_list_no,
                             pass_let) returning tmp_changes_made
               call display_record(FALSE)
            else
               call spnodata()
            end if
            if (prog_timeout) then
               exit display
            end if
         end if

      on action adjust_r_btn
         call sperrorm_blankmess()
         if get_record() then
            call sperrorm("No adjustment information available since property has been deleted.", 0, 1)
         else
            if (adjust_cnt > 0) then
               call inquire_adj_routine()
            else
               call sperrorm("No adjustments exists for this personal property.", 0, 1)
            end if
            if (prog_timeout) then
               exit display
            end if
         end if

      on action close
         let int_flag = FALSE
         if (spaskrtn_closeWindow(DIALOG)) then
            exit display
         else
            continue display
         end if


      &include "StandardMainDialogActions.inc"

   end display

end function
######################################################################
function text_routine()

   define tmp_success   smallint,
          tmp_err_msg   string,
          tmp_not_found smallint,
          lck_txppmast  record like txppmast.*

   if (NOT sp_begin_work(7844)) then
      return
   end if

   call txppmastio_lock(active_mt_sw, active_id_no,
                        active_year, active_list_no)
                     returning tmp_success, tmp_err_msg, tmp_not_found,
                               lck_txppmast.*
   if (tmp_not_found) then
      let tmp_err_msg = "Cannot find personal property record to update."
      let tmp_success = FALSE
   end if
   if (NOT tmp_success) then
      call sperrorm(tmp_err_msg, 0, 2)
      if (NOT sp_rollback_work(2525)) then
      end if
      return
   end if

   let arg_string = active_mt_sw using "&&", " ",
                    active_id_no using "&&&&&&&&&&", " ",
                    active_year using "&&&&", " ",
                    active_list_no using "&&&&&&&&"

   if pass_let = "I" then
      call sptxtedt_textEditor("txppmast", arg_string,
                               "Personal Property Text",
                               " ", " ", "V", "N")
   else
      if (check_permit()) then
         call sptxtedt_textEditor("txppmast", arg_string,
                                  "Personal Property Text",
                                  " ", " ", "V", "Y")
      else
         call sptxtedt_textEditor("txppmast", arg_string,
                                  "Personal Property Text",
                                  " ", " ", "V", "N")
      end if
   end if

   if (NOT sp_commit_work(8987)) then
      return
   end if

end function
######################################################################
function lock_txch_rec(pass_mt_sw, pass_cat, pass_prop, pass_year,
                       pass_bill, pass_seq)
   define success                   smallint,
          pass_mt_sw                smallint,
          pass_cat                  smallint,
          pass_year                 smallint,
          pass_prop                 char(30),
          pass_bill                 integer,
          pass_seq                  like txchgdef.txch_seq

   let success = TRUE
   open lock_txch_j using pass_mt_sw,
                          pass_cat,
                          pass_prop,
                          pass_year,
                          pass_bill,
                          pass_seq
   if (sqlca.sqlcode = 0) then
      fetch lock_txch_j
   end if

   case
      when (sqlca.sqlcode = NOTFOUND)
         call splockdelete()
         let success = FALSE
         exit case
      when (sperrorm_busy())
         call splockbusy()
         let success = FALSE
         exit case
      when (sqlca.sqlcode != 0)
         call splockunknown()
         let success = FALSE
         exit case
   end case

   if (NOT success) then
      close lock_txch_j
      return FALSE
   end if

   return TRUE

end function
######################################################################
function lock_txpm_rec()
   define success smallint

   let success = TRUE

   open lock_txpm
   if (sqlca.sqlcode = 0) then
      fetch lock_txpm
   end if

   case
      when (sqlca.sqlcode = NOTFOUND)
         call splockdelete()
         let success = FALSE
         exit case
      when (sperrorm_busy())
         call splockbusy()
         let success = FALSE
         exit case
      when (sqlca.sqlcode)
         call splockunknown()
         let success = FALSE
         exit case
   end case

   if (NOT success) then
      close lock_txpm
   end if

   return(success)

end function
######################################################################
function find_routine(find_option)
   define query_line, where_clause STRING,
          find_option smallint,
          tmp_success   smallint,
          tmp_tables    string

   # Initialize global variable(s)...
   let int_flag = 0

   # Initialize global array(s)...
   call g_disp_values_array.clear()

   call close_cursor()

   if (pass_let != "V" and
       pass_let != "I" and
       pass_let != "L" and
       pass_let != "N") then

      if ((ST_CLIENT_STATE != "CT") and (proc_stat > 0 and proc_stat < 9)) or
         ((ST_CLIENT_STATE = "CT") and (proc_stat > 0 and proc_stat < 3)) or
         ((ST_CLIENT_STATE != "CT") and (proc_stat = 0 and instmt_meth = 2 and pp_stat < 3)) then
         if (find_option != 3 and find_option != 6) then
             call set_access()   -- found in txppmast_adlops.4gl
             if (int_flag) then
                let int_flag = FALSE
                return
             end if
         end if
      else
         let ct_sw = "C"
         let use_mt_sw = 0
         let use_year = 0
      end if
   else
      if (pass_let = "I" or pass_let = "L") then
         let ct_sw = "C"
         let use_mt_sw = 0
      end if

      if (pass_let = "B" or pass_let = "N") then
         let ct_sw = "T"
         if (ST_CLIENT_STATE != "MA" and
             ST_CLIENT_STATE != "MD" and
             ST_CLIENT_STATE != "NY" and
             ST_CLIENT_STATE != "CT" and
             ST_CLIENT_STATE != "NH" and
             ST_CLIENT_STATE != "MI" and
             ST_CLIENT_STATE != "RI" and
             ST_CLIENT_STATE != "LA" and
             ST_CLIENT_STATE != "VT") then
            if (proc_stat != 0) then
               let use_mt_sw = 9
               let use_year = act_pp_year
            else
               let use_mt_sw = 1
               let use_year = assm_year
            end if
         else
            let use_mt_sw = 1
            let use_year = assm_year
         end if
      end if
   end if

   case
      # called from the main screen when NOT in inquiry mode
      when (find_option = 1)
         call txrpfind(use_mt_sw, 25, use_year)
                        returning tmp_success, query_line, tmp_tables

         if (tmp_success) then

            let select_line = "select count(*) from ", tmp_tables clipped,
                              " where ", query_line clipped

            call spmdebug_say("TXPPMAST: find_routine: spcounter(select_line): ", select_line)
            let record_count = spcounter(select_line, 100)

            if (record_count <= 0) then
               let find_cursor_open = FALSE
               let record_count = 0
               return
            end if

            let select_line = "select txpp_mt_sw, txpp_id_no, ",
                                    "txpp_year, txpp_list_no from ",
                                 tmp_tables clipped, " where ",
                                 query_line clipped,
                                 " order by txpp_mt_sw, txpp_id_no "
            call spmdebug_say("TXPPMAST: find_routine: select_line: ", select_line)
         else
            return
         end if

      # only called after an ADD has been done
       when (find_option = 3)
          let where_clause = " where txpp_mt_sw = ",
                                        gr_txppmast.txpp_mt_sw,
                               " and txpp_id_no = '",
                                        gr_txppmast.txpp_id_no,
                              "' and txpp_year = ",
                                        gr_txppmast.txpp_year,
                               " and txpp_list_no = ",
                                        gr_txppmast.txpp_list_no

          let find_cursor_open = TRUE

          let select_line = "select count(*) from txppmast", where_clause
          let record_count = spcounter(select_line, 0)

          let select_line = "select txpp_mt_sw, txpp_id_no, ",
                                   "txpp_year, txpp_list_no ",
                              "from txppmast ", where_clause clipped

      when (find_option = 5)  # This option will be used when pass_let = "I" (Inquiry).
         call txrpfind(99, 25, 99) returning tmp_success, query_line, tmp_tables
         if (NOT tmp_success) then
            call spsetwin_clearForm(whdl_main)
            return
         end if
         let select_line = "select count(*) from ", tmp_tables clipped,
                           " where ", query_line clipped

         call spmdebug_say("TXPPMAST: find_routine: spcounter(select_line): ", select_line)
         let record_count = spcounter(select_line, 100)
         if (record_count <= 0) then
            let find_cursor_open = FALSE
            let record_count = 0
            return
         else
            let find_cursor_open = TRUE
            let select_line = "select txpp_mt_sw, txpp_id_no, ",
                              " txpp_year, txpp_list_no from ",
                              tmp_tables clipped, " where ", query_line clipped,
                              " order by txpp_id_no, txpp_year, txpp_mt_sw, txpp_list_no "
            call spmdebug_say("TXPPMAST: find_routine: select_line: ", select_line)
         end if

      when (find_option = 6)# This option is used when txppmast is called from another
                            # program which passes parameters to find.
         select count(*) into record_count from txppmast
          where txpp_mt_sw = gr_txppmast.txpp_mt_sw and
                txpp_id_no = gr_txppmast.txpp_id_no and
                txpp_year = gr_txppmast.txpp_year and
                txpp_list_no = gr_txppmast.txpp_list_no
         if (record_count = 0) then
            call sperrorm("Property not found",0,2)
            return
         end if

         let select_line = "select txpp_mt_sw, txpp_id_no, txpp_year, ",
                                  "txpp_list_no from txppmast ",
                            "where txpp_mt_sw = ", gr_txppmast.txpp_mt_sw, " and ",
                                "txpp_id_no = ",
                                      gr_txppmast.txpp_id_no, " and ",
                                "txpp_year = ", gr_txppmast.txpp_year, " and ",
                                "txpp_list_no = ", gr_txppmast.txpp_list_no

      when (find_option = 7)  # find from Action Launcher
         let select_line = "WITH tmpCount AS ",
                           "(SELECT DISTINCT ",
                              "txppmast.txpp_mt_sw, ",
                              "txppmast.txpp_id_no, ",
                              "txppmast.txpp_year, ",
                              "txppmast.txpp_list_no ",
                           "FROM txppmast ",
                           "LEFT JOIN txowners ",
                                  "ON txowners.txon_prop_id = CAST(txppmast.txpp_id_no AS char(30)) ",
                                  "AND txppmast.txpp_mt_sw = 1 ",
                           "LEFT JOIN AccountsReceivableDataAccess.Customers ",
                                  "ON AccountsReceivableDataAccess.Customers.CustomerNumber = txowners.txon_acct ",
                           "WHERE txppmast.txpp_mt_sw = 1 ",
                           "AND txppmast.txpp_id_no LIKE '%",searchString,"%'  COLLATE SQL_Latin1_General_CP1_CI_AS ",
                           "OR AccountsReceivableDataAccess.Customers.PrimaryName like '%",searchString,"%' COLLATE SQL_Latin1_General_CP1_CI_AS) ",
                           "SELECT COUNT(*) ",
                           "FROM tmpCount "
         let record_count = spcounter(select_line, 100)
         case
            when (record_count <= 0)
               let record_count = 0
               let find_cursor_open = FALSE
               return
            otherwise
               let find_cursor_open = TRUE
         end case

         let select_line = "SELECT txppmast.txpp_mt_sw, txppmast.txpp_id_no, txppmast.txpp_year, txppmast.txpp_list_no ",
                           "FROM txppmast ",
                           "LEFT JOIN txowners ",
                                  "ON txowners.txon_prop_id = CAST(txppmast.txpp_id_no AS char(30)) ",
                                 "AND txppmast.txpp_mt_sw = 1 ",
                           "LEFT JOIN AccountsReceivableDataAccess.Customers ",
                                  "ON AccountsReceivableDataAccess.Customers.CustomerNumber = txowners.txon_acct ",
                           "WHERE txppmast.txpp_mt_sw = 1 ",
                           "AND txppmast.txpp_id_no LIKE '%",searchString,"%'  COLLATE SQL_Latin1_General_CP1_CI_AS ",
                           "OR AccountsReceivableDataAccess.Customers.PrimaryName like '%",searchString,"%' COLLATE SQL_Latin1_General_CP1_CI_AS ",
                           "GROUP BY txppmast.txpp_mt_sw, txppmast.txpp_id_no, txppmast.txpp_year, txppmast.txpp_list_no ",
                           "ORDER BY txppmast.txpp_year ASC "

   end case

   prepare sl1 from select_line
   if (sqlca.sqlcode) then
      call spsyserr("Prepare find_cursor failed", "Prepare, Line " || __LINE__)
      return
   end if
   declare find_cursor scroll cursor with hold for sl1

   open find_cursor
   if (sqlca.sqlcode) then
      call spsyserr("Open find_cursor failed", "Open, Line " || __LINE__)
      let find_cursor_open = FALSE
      let record_count = 0
      return
   end if
   let find_cursor_open = TRUE

   let fetchno = 1
   call fetch_record()

end function
#########################################################################
function fetch_record()

   fetch absolute fetchno find_cursor into active_mt_sw,
                                           active_id_no,
                                           active_year,
                                           active_list_no
   if (sqlca.sqlcode) then
      call spsyserr("Error fetching record.",
                    "Fetch error 240.")
   else
      let char_id_no = active_id_no using "<<<<<<<<<<"
   end if

   if (active_mt_sw = 0) then
      let ct_sw = "C"
   else
      let ct_sw = "T"
   end if

   let disp_special = TRUE

end function
###########################################################################
function fetch_key(fetchit)
   define fetchit smallint,
          key_mt_sw     like txppmast.txpp_mt_sw,
          key_id_no     like txppmast.txpp_id_no,
          key_year      like txppmast.txpp_year,
          key_list_no   like txppmast.txpp_list_no

   if (fetchit <= record_count) then
      fetch absolute fetchit find_cursor into key_mt_sw,
                                              key_id_no,
                                              key_year,
                                              key_list_no

      return key_mt_sw, key_id_no, key_year,
             key_list_no, sqlca.sqlcode
   else
      return ZERO, SPACE, ZERO, ZERO, ZERO
   end if

end function
###########################################################################
function add_routine()
   define success smallint,
          ret_success smallint,
          retval  smallint,
          tmp_val_array dynamic array of record
             invalid_field string,
             invalid_reason string
          end record

   let int_flag = FALSE
   let cancelled = FALSE
   call close_cursor()
   call spsetwin_clearForm(whdl_main)

   let gr_txppmast.* = nl_txppmast.*
   call initial_values()
   if (pass_let = "L" and pass_prop = "BLANK") then
      select blms_loc_no, blms_loc_no_suff,
             blms_loc_street, blms_loc_ap
        into gr_txppmast.txpp_loc_no, gr_txppmast.txpp_loc_no_suff,
             gr_txppmast.txpp_loc_street, gr_txppmast.txpp_loc_apt
        from blmaster
       where blms_id = pass_blid
      if (sqlca.sqlcode) then
         let gr_txppmast.txpp_loc_no       = 0
         let gr_txppmast.txpp_loc_no_suff  = " "
         let gr_txppmast.txpp_loc_street   = " "
         let gr_txppmast.txpp_loc_apt      = " "
      end if
      display by name gr_txppmast.txpp_loc_no,
                      gr_txppmast.txpp_loc_no_suff,
                      gr_txppmast.txpp_loc_street,
                      gr_txppmast.txpp_loc_apt
   end if

   if (find_cursor_open) then
      call close_cursor()
   end if

   if (ST_CLIENT_STATE = "VA") then
      call va_input_routine(0)
   else
      call input_routine(0)
   end if

   if (int_flag) then
      let cancelled = TRUE
      let int_flag = FALSE
      call spsetwin_clearForm(whdl_main)
      let gr_txppmast.* = nl_txppmast.*
      call spaddcanc()
      return
   end if

   if (NOT sp_begin_work(1609)) then
      return
   end if

   let char_id_no = gr_txppmast.txpp_id_no using "<<<<<<<<<<"

   if (ST_CLIENT_STATE = "VA" and gr_txppmast.txpp_mt_sw = 0) then
      let gr_txppmast.txpp_comply_date = NULL
   end if

   if (ST_CLIENT_STATE = "NC") then
      if (gr_txppmast.txpp_mt_sw != 0) then
         if (gr_txppmast.txpp_list_status != "U") then
            let gr_txppmast.txpp_comply_id = ST_USER
            let gr_txppmast.txpp_comply_date = ST_TODAY
            let gr_txppmast.txpp_comply_dttm = current hour to minute
         end if
      end if
   end if

   let ret_success = TRUE
   call txppmastwr_insert(gr_txppmast.*, FALSE, tmp_val_array, TRUE, TRUE)
                                    returning ret_success, err_msg

   case
      when (NOT ret_success)
         call sperrorm(err_msg, 0, 2)
         call sp_rollback_work(539) returning retval
         let int_flag = TRUE
      when (NOT insert_custtag())
         call sp_rollback_work(540) returning retval
         let int_flag = TRUE
      when (NOT sp_commit_work(539))
         let int_flag = TRUE
   end case

   if (NOT insert_chgdefs(1, gr_txppmast.*)) then
      call sp_rollback_work(541) returning retval
      let int_flag = TRUE
   end if

   if (int_flag) then
      let int_flag = FALSE
      return
   end if
   if (pass_let = "L" and pass_prop = "BLANK") then
      if (bl_add = FALSE) then
         call sppasval_put(pass_lno, char_id_no) returning success
         let bl_add = TRUE
      end if
   end if

   call find_routine(3)
   call display_record(FALSE)
   call sprecordadd()

end function
###############################################################################
function update_routine()
   define retval string,
          tmp_err_msg string,
          tmp_not_found smallint,
          tmp_success smallint,
          tmp_upd_current smallint,
          lck_txppmast record like txppmast.*,
          cur_txppmast record like txppmast.*

   # Initialize local variable(s)...
   let retval = " "
   let tmp_err_msg = " "
   let tmp_not_found = FALSE
   let tmp_success = TRUE
   let tmp_upd_current = FALSE

   # Initialize global variable(s)...
   let hold_juriscd = gr_txppmast.txpp_juris_cd
   let hold_customerSSN = gv_customerSSN

   let old_txppmast.* = gr_txppmast.*    -- for audit records

   if (ST_CLIENT_STATE = "VA") then
      if (gr_txppmast.txpp_status = "I") then
         let prev_status = "I"
         let retval = spaskrtn_dialog("Inactive Property", "Warning - This property is inactive.", "OK", "warning", "OK")
      end if
      call va_input_routine(1)
   else
      call input_routine(1)
   end if

   if (int_flag) then
      let int_flag = 0
      call spupdcanc()
      return
   end if

   if (sp_begin_work(303)) then
      call txppmastio_lock(active_mt_sw, active_id_no, active_year, active_list_no)
                           returning tmp_success, tmp_err_msg,
                                     tmp_not_found, lck_txppmast.*
      if (tmp_not_found) then
         let tmp_err_msg = "Error updating personal property record.  Record not found."
         let tmp_success = FALSE
      end if
      if (NOT tmp_success) then
         call sperrorm(tmp_err_msg, 0, 2)
         call sp_rollback_work(303) returning retval
         return
      end if
      let gr_txppmast.txpp_last_updt_by = ST_USER
      let gr_txppmast.txpp_last_date = ST_TODAY
      let gr_txppmast.txpp_last_time = current hour to minute
      if (ST_CLIENT_STATE = "VA" and gr_txppmast.txpp_mt_sw = 0) then
         let gr_txppmast.txpp_comply_date = NULL
      end if

      if (ST_CLIENT_STATE = "NC") then
         if (gr_txppmast.txpp_mt_sw != 0) then
            if (gr_txppmast.txpp_list_status != old_txppmast.txpp_list_status) then
               if (gr_txppmast.txpp_list_status = "U") then
                  let gr_txppmast.txpp_comply_id = SPACE
                  let gr_txppmast.txpp_comply_date = NULL
                  let gr_txppmast.txpp_comply_dttm = SPACE
               else
                  let gr_txppmast.txpp_comply_id = ST_USER
                  let gr_txppmast.txpp_comply_date = ST_TODAY
                  let gr_txppmast.txpp_comply_dttm = current hour to minute
               end if
            end if
         end if
      end if

      call update_record(gr_txppmast.*)

      if (sqlca.sqlcode) then
         call spsyserr("Unexpected error updating record.",
                       "txppmast:   Update error 731.")
         call sp_rollback_work(335) returning retval
         return
      end if

      if (ST_CLIENT_STATE = "NC") then
         if (NOT insert_chgdefs(2, gr_txppmast.*)) then
            if (NOT sp_rollback_work(2344)) then
            end if
            return
         end if
      end if

      if (ct_sw = "T") then
         if (gr_txppmast.txpp_mt_sw = 1 or gr_txppmast.txpp_mt_sw = 9) then
            let tmp_upd_current = FALSE
            if (ST_CLIENT_STATE = "VA") then
               let dlg_str = "Would you like to update the current record?"
               let user_ansr = spaskrtn_dialog("Update Current Record", dlg_str, "Yes|No", "question", "Y")
               if (user_ansr = "Yes") then
                  let tmp_upd_current = TRUE
               end if
            else
               let tmp_upd_current = TRUE
            end if

# If NC, only update current records if permitted to do so from Additional Settings of Tax Settings...
            if (ST_CLIENT_STATE = "NC" and updt_curr = "N") then
               let tmp_upd_current = FALSE
            end if

            if (tmp_upd_current = TRUE) then
               let cur_txppmast.* = gr_txppmast.*
               let cur_txppmast.txpp_mt_sw = 0
               let cur_txppmast.txpp_year = 0
               let cur_txppmast.txpp_list_no = 0
               let cur_txppmast.txpp_list_status = " "
               let cur_txppmast.txpp_comply_id = " "
               let cur_txppmast.txpp_comply_date = NULL
               let cur_txppmast.txpp_comply_dttm = SPACE
               let cur_txppmast.txpp_comply_no = 0

               call txppmastio_lock(cur_txppmast.txpp_mt_sw,
                                    cur_txppmast.txpp_id_no,
                                    cur_txppmast.txpp_year,
                                    cur_txppmast.txpp_list_no)
                                 returning tmp_success, tmp_err_msg,
                                           tmp_not_found, lck_txppmast.*
               if (tmp_not_found) then
                  let tmp_err_msg = "Current record not found to update."
                  let tmp_success = FALSE
               end if
               if (NOT tmp_success) then
                  call sperrorm(tmp_err_msg, 0, 2)
               else
                  call update_record(cur_txppmast.*)
               end if

               if (ST_CLIENT_STATE = "NC") then
                  if (NOT insert_chgdefs(2, cur_txppmast.*)) then
                     if (NOT sp_rollback_work(5667)) then
                     end if
                     return
                  end if
               end if
            end if
         end if
      end if

      call sp_commit_work(335) returning retval

      call sprecordupd()

   end if

end function
###############################################################################
function delete_routine()
   define tmp_bills_found smallint,
          tmp_ar_cat like arbilhdr.arbh_ar_cat,
          tmp_year like arbilhdr.arbh_year,
          tmp_bill like arbilhdr.arbh_bill,
          tmp_bill_type like arbilhdr.arbh_bill_type,
          tmp_success smallint,
          tmp_err_msg string,
          tmp_rec_notfound smallint,
          tmp_count integer,
          tmp_txppmast record like txppmast.*,
          tmp_user_answer char(3),
          tmp_val_array dynamic array of record
             invalid_field string,
             invalid_reason string
          end record

   let tmp_success = TRUE

# Don't allow records which have a list number assigned to be deleted...
   if (gr_txppmast.txpp_list_no != 0) then
      call sperrorm("Record has been assigned a bill number.  You cannot delete.", 0, 2)
      let tmp_success = FALSE
   end if

# Don't allow records where the property ID has a bill (in any year) to be deleted...
   if (tmp_success) then
      if (gr_txppmast.txpp_mt_sw = 0) then
         let tmp_bills_found = FALSE
         open bilhdr_csr using char_id_no
         while (TRUE)
            fetch bilhdr_csr into tmp_ar_cat,
                                  tmp_year,
                                  tmp_bill,
                                  tmp_bill_type
            if (sqlca.sqlcode) then
               exit while
            else
               if (tmp_ar_cat = 25) then
                  let tmp_bills_found = TRUE
                  exit while
               end if
            end if
         end while
         close bilhdr_csr

         if (tmp_bills_found) then
            call sperrorm("Property has bills attached.  You cannot delete.", 0, 2)
            let tmp_success = FALSE
         end if
      end if
   end if

# If deleting a current record, make sure a corresponding mt_sw!=0 record doesn't exist..
   if (tmp_success) then
      if (gr_txppmast.txpp_mt_sw = 0) then
         open count_others using active_id_no
         if (sqlca.sqlcode) then
            call spsyserr("Error opening count_others.", "Open count_others 2935")
            let tmp_success = FALSE
         else
            fetch count_others into tmp_count
            if (sqlca.sqlcode) then
               call spsyserr("Error fetching count_others.", "Fetch count_others 2940")
               let tmp_success = FALSE
            else
               if (tmp_count is NULL) then
                  let tmp_count = 0
               end if
               if (tmp_count) then
                  call sperrorm("This property cannot be deleted because another version of this property exists.", 0, 2)
                  let tmp_success = FALSE
               end if
            end if
            close count_others
         end if
      end if
   end if

   if (tmp_success) then
# Begin a transaction...
      if (NOT sp_begin_work(1374)) then
         let tmp_success = FALSE
      end if
   end if

   if (tmp_success) then
# Make sure the record can be locked...
      call txppmastio_lock(active_mt_sw, active_id_no, active_year, active_list_no)
                 returning tmp_success, tmp_err_msg, tmp_rec_notfound, tmp_txppmast.*
      if (tmp_rec_notfound) then
         let tmp_success = FALSE
         let tmp_err_msg = "Property to be deleted not found."
      end if
      if (NOT tmp_success) then
         call sperrorm(tmp_err_msg, 0, 2)
      end if

# Ask the user if he really want to delete the record...
      if (tmp_success) then
         if (NOT spaskrtn_interrupt("del")) then
            let tmp_success = FALSE
         end if

         if (tmp_success) then
            if (ST_CLIENT_STATE = "VA") then
# See if MV records are attached...
               call mvamvmstwr_countForParent(active_mt_sw, active_id_no, active_year, active_list_no)
                                    returning tmp_success, tmp_err_msg, tmp_count
               if (NOT tmp_success) then
                  call sperrorm(tmp_err_msg, 0, 2)
               else
                  if (tmp_count) then
                     let dlg_str = "There are motor vehicle records associated \n",
                                   "with this property, which will also be deleted. \n",
                                   "Do you still want to delete this property and \n",
                                   "its associated records?"
                     let user_ansr = spaskrtn_dialog("Motor Vehicles Found", dlg_str,
                                                     "Yes|No", "question", "No")
                     if (user_ansr != "Yes") then
                        let tmp_success = FALSE
                     end if
                  end if
               end if
            end if
         end if

# Delete the property...
         if (tmp_success) then
            call txproptybu_removeProperty(active_mt_sw, 25, active_id_no, active_year,
                                           active_list_no, FALSE, FALSE, TRUE)
                                 returning tmp_success, tmp_err_msg
            if (NOT tmp_success) then
               call sperrorm(tmp_err_msg, 0, 2)
            end if
         end if

# Release the lock...
         call txppmastio_release()
      end if

      if (tmp_success) then
         if (active_mt_sw != 0) then
# See if a corresponding current record exists...
            call txppmastio_select(0, active_id_no, 0, 0)
                         returning tmp_success, tmp_err_msg, tmp_rec_notfound, tmp_txppmast.*
            if (NOT tmp_success) then
               call sperrorm(tmp_err_msg, 0, 2)
            else
               if (NOT tmp_rec_notfound) then
# Ask the user if he would like to inactivate current...
                  call spaskrtn_dialog("Inactivate Current",
                                       "Would you like to inactivate the corresponding current record?",
                                       "Yes|No", "question", "Yes") returning tmp_user_answer
                  if (tmp_user_answer = "Yes") then
                     let tmp_txppmast.txpp_status = "I"
                     let tmp_txppmast.txpp_inact_date = ST_TODAY
                     let tmp_txppmast.txpp_full_exem_cd = " "

                     call txppmastwr_update(tmp_txppmast.*, FALSE, TRUE, tmp_val_array, TRUE, TRUE)
                                  returning tmp_success, tmp_err_msg
                     if (NOT tmp_success) then
                        call sperrorm(tmp_err_msg, 0, 2)
                     end if
                  end if
               end if
            end if
         end if
      end if

# Finish the transaction...
      if (NOT tmp_success) then
         if (NOT sp_rollback_work(1413)) then
         end if
      else
         if (NOT sp_commit_work(1419)) then
            let tmp_success = FALSE
         end if
      end if
   end if

   if (pass_let = "L" and pass_prop = "BLANK") then
      let bl_add = FALSE
   end if

end function
###############################################################################
function display_record(show_warning)
   define tmp_loc_city_st_zip,
          tmp_loc_desc string,
          removed,
          retval,
          show_warning smallint,
          lv_customerDeliveryAddressStatusActive boolean,
          attachParams Attachments.InterfaceParameters

   # Initialize local variable(s)...
   let tmp_loc_desc = " "

   let removed = get_record()

   if removed = 2 then
      return
   end if

   call get_customer(char_id_no, active_year, active_list_no) returning lv_customerDeliveryAddressStatusActive
   if (lv_customerDeliveryAddressStatusActive) then
      call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", true)
   else
      call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", false)
   end if
   call get_tot_sqft()
   let cynet = calc_net_value()

   # get code descriptions for display
   let jur_desc = " "
   select txju_desc into jur_desc from txjurisd
      where txju_code = gr_txppmast.txpp_juris_cd
   if (sqlca.sqlcode) then
      let jur_desc = " "
   end if

   let class_desc = " "
   select txcl_desc into class_desc from txclascd
      where txcl_class   = gr_txppmast.txpp_class_cd
        and txcl_ar_cat  = 25
        and txcl_lnpg_sw = 'G'
   if (sqlca.sqlcode) then
      let class_desc = " "
   end if

   let sub_desc = " "
   if (length(gr_txppmast.txpp_subdiv) > 0) then
      select txsd_desc into sub_desc from txsubdiv
         where txsd_code = gr_txppmast.txpp_subdiv
      if (sqlca.sqlcode) then
         let sub_desc = " "
      end if
   end if

   let lender_name = " "
   if (length(gr_txppmast.txpp_nature_own) > 0) then
      if (ST_CLIENT_STATE = "VA") then
         select arlr_name into lender_name from arlender
            where arlr_code = gr_txppmast.txpp_nature_own
      else
         select txno_desc into lender_name from txnatown
            where txno_code = gr_txppmast.txpp_nature_own
      end if
      if (sqlca.sqlcode) then
         let lender_name = " "
      end if
   end if

   let exem_desc = " "
   if (length(gr_txppmast.txpp_full_exem_cd) > 0) then
      select ared_desc into exem_desc from arexdisc
         where ared_code  = gr_txppmast.txpp_full_exem_cd
           and ared_total = "Y"
      if (sqlca.sqlcode) then
         let exem_desc = " "
      end if
   end if

   if (ST_CLIENT_STATE = "VA") then
      let gv_customerSSN = nvl(gr_txppmast.txpp_old_id, " ")
   else
      let gv_oldID = nvl(gr_txppmast.txpp_old_id, " ")
   end if

   if (ST_CLIENT_STATE = "VA") then
      call checkViewPermissions()
      display by name gr_txppmast.txpp_id_no,
                      gr_txppmast.txpp_year,
                      gr_txppmast.txpp_list_no,
                      gr_txppmast.txpp_mt_sw,
                      gr_txppmast.txpp_dba,
                      gr_txppmast.txpp_parc_id,
                      cust_acct,
                      addl_no,
                      gv_customerSSN,
                      cust_name,
                      cust_name2,
                      cust_addr,
                      cust_addr2,
                      cust_city,
                      cust_state,
                      cust_zip,
                      cust_country,
                      gr_txppmast.txpp_status,
                      gr_txppmast.txpp_inact_date,
                      gr_txppmast.txpp_full_exem_cd,
                      exem_desc,
                      gr_txppmast.txpp_abst_type,
                      gr_txppmast.txpp_business,
                      gr_txppmast.txpp_list_status,
                      gr_txppmast.txpp_juris_cd,
                      jur_desc,
                      gr_txppmast.txpp_class_cd,
                      class_desc,
                      gr_txppmast.txpp_subdiv,
                      sub_desc,
                      gr_txppmast.txpp_nature_own,
                      lender_name,
                      gr_txppmast.txpp_formal_lst,
                      gr_txppmast.txpp_comply_no,
                      gr_txppmast.txpp_list_status,
                      gr_txppmast.txpp_comply_date,
                      tot_sqft,
                      gr_txppmast.txpp_create_date,
                      gr_txppmast.txpp_resident,
                      gr_txppmast.txpp_date_field,
                      gr_txppmast.txpp_who_field,
                      gr_txppmast.txpp_date_desk,
                      gr_txppmast.txpp_who_desk,
                      gr_txppmast.txpp_comply,
                      gr_txppmast.txpp_fml_lst_yr,
                      gr_txppmast.txpp_audit,
                      gr_txppmast.txpp_audit_yr,
                      gr_txppmast.txpp_pos_audit,
                      gr_txppmast.txpp_assmt_ly
   else
      display by name gr_txppmast.txpp_id_no,
                      gr_txppmast.txpp_year,
                      gr_txppmast.txpp_list_no,
                      gr_txppmast.txpp_mt_sw,
                      gr_txppmast.txpp_dba,
                      gr_txppmast.txpp_parc_id,
                      gv_oldID,
                      cust_acct,
                      addl_no,
                      cust_name,
                      cust_name2,
                      cust_addr,
                      cust_addr2,
                      cust_city,
                      cust_state,
                      cust_zip,
                      cust_country,
                      gr_txppmast.txpp_status,
                      gr_txppmast.txpp_inact_date,
                      gr_txppmast.txpp_full_exem_cd,
                      exem_desc,
                      gr_txppmast.txpp_abst_type,
                      gr_txppmast.txpp_business,
                      gr_txppmast.txpp_list_status,
                      gr_txppmast.txpp_juris_cd,
                      jur_desc,
                      gr_txppmast.txpp_class_cd,
                      class_desc,
                      gr_txppmast.txpp_subdiv,
                      sub_desc,
                      gr_txppmast.txpp_nature_own,
                      lender_name,
                      gr_txppmast.txpp_formal_lst,
                      gr_txppmast.txpp_comply_no,
                      gr_txppmast.txpp_comply_date,
                      tot_sqft,
                      gr_txppmast.txpp_create_date,
                      gr_txppmast.txpp_resident,
                      gr_txppmast.txpp_date_field,
                      gr_txppmast.txpp_who_field,
                      gr_txppmast.txpp_date_desk,
                      gr_txppmast.txpp_who_desk,
                      gr_txppmast.txpp_comply,
                      gr_txppmast.txpp_fml_lst_yr,
                      gr_txppmast.txpp_audit,
                      gr_txppmast.txpp_audit_yr,
                      gr_txppmast.txpp_pos_audit,
                      gr_txppmast.txpp_assmt_ly
   end if

   initialize attachParams.* to null
   let attachParams.RecordAttachment.Context = "txppmast"
   let attachParams.RecordAttachment.Keys[1] = active_mt_sw using "&",
                                               active_id_no using "&&&&&&&&&&",
                                               active_year using "&&&&",
                                               active_list_no using "&&&&&&&&&"
   call Attachments.ShowCount(attachParams.*)

   call load_values_array()

   let adjust_cnt = 0
   if (use_mt_sw = 0) then
      select count(*)
        into adjust_cnt
        from txadjblh
       where txah_prop_id = char_id_no
         and txah_ar_cat = 25
      if (adjust_cnt IS NULL) then
         let adjust_cnt = 0
      end if
   else
      if active_mt_sw = 9 and gr_txppmast.txpp_list_no = 0 then
         let adjust_cnt = 0
      else
         select count(*)
           into adjust_cnt
           from txadjblh
          where txah_ar_cat = 25
            and txah_year = active_year
            and txah_bill = active_list_no
      end if
      if (adjust_cnt IS NULL) then
         let adjust_cnt = 0
      end if
   end if
   call spdomlib_setBtnDataImage("adjust_r_btn", adjust_cnt)

# Special condition section
   if ((public_access = "N") and (cust_acct != 0)) then
      call special_condition()
   end if

   let exem_count = 0
   if (gr_txppmast.txpp_mt_sw = 0) then #CURRENT
      select count(*)
        into exem_count
        from txcstexm
       where txce_acct in (select txtg_acct from txcsttag
                            where txtg_ar_cat = 25
                              and txtg_prop_id = char_id_no)
         and txce_ar_cat = 25
         and txce_prop_id = char_id_no
      if (exem_count IS NULL) then
         let exem_count = 0
      end if
      call spdomlib_setBtnDataImage("exem_r_btn", exem_count)
   else  # TAX
      select count(*)
        into exem_count
        from txexemps
       where txxm_mt_sw = active_mt_sw
         and txxm_ar_cat = 25
         and txxm_prop_id = char_id_no
         and txxm_year = active_year
         and txxm_list_no = active_list_no
      if (exem_count IS NULL) then
         let exem_count = 0
      end if
      call spdomlib_setBtnDataImage("exem_r_btn", exem_count)
   end if

#333
# THIS CODE HAS BEEN COMMENTED OUT (MUN-30501) BECAUSE THE SWITCHING OF WHAT
# IS BEING SHOWN AND HIDDEN SHOULD NOT CHANGE THE SIZE OF THE SCREEN AND THIS
# CODE WOULD DO THAT. THIS CODE SHOULD NOT BE DELETED BECAUSE IT MAY BE
# IMPLEMENTED IN THE FUTURE.

#  if (gr_txppmast.txpp_year != 0) then
#     let use_cen_addr = "N"
#     select txyp_use_loc
#       into use_cen_addr
#       from txyrparm
#      where txyp_ar_cat = 25
#        and txyp_year = gr_txppmast.txpp_year
#        and txyp_cycle_comm = 0
#     if (sqlca.sqlcode) then
#        let err_msg = "Error selecting location flag from txyrparm."
#        call spsyserr(err_msg, "Select txyrparm - 2893")
#     end if

#     if (use_cen_addr IS NULL) then
#        let use_cen_addr = "N"
#     end if

#     if (use_cen_addr = "N") then
#        call spdomlib_hideAllByTag("cal_grp", TRUE)
#        call spdomlib_hideAllByTag("rl_grp", FALSE)
#     else
#        call spdomlib_hideAllByTag("cal_grp", FALSE)
#        call spdomlib_hideAllByTag("rl_grp", TRUE)
#     end if
#  end if
# END OF CODE COMMENTED OUT (MUN-30501)

   if (use_cen_addr = "Y") then
      display by name gr_txppmast.txpp_loc_id

      if (gr_txppmast.txpp_loc_id IS NULL or
          gr_txppmast.txpp_loc_id = " ") then
         let loc_seq = " "
      else
         let loc_seq = "Seq: ", gr_txppmast.txpp_loc_seq using "<<<<&"
      end if
      display by name loc_seq

      if (gr_txppmast.txpp_loc_id IS NOT NULL and
          gr_txppmast.txpp_loc_id != " ") then
         call cploclib_formatShortLoc(gr_txppmast.txpp_loc_id,
                                      gr_txppmast.txpp_loc_seq,
                                      " ",
                                      0,
                                      " ",
                                      " ",
                                      " ",
                                      " ",
                                      " ",
                                      " ",
                                      " ",
                                      " ")
                            returning tmp_loc_desc,
                                      tmp_loc_city_st_zip,
                                      retval,
                                      err_msg
         if (retval) then
            let format_loc = tmp_loc_desc clipped, "\n",
                             tmp_loc_city_st_zip
            display by name format_loc
         end if

         if (retval < 0) then
            let format_loc = " "
            display by name format_loc
         end if
      else
         let format_loc = " "
         display by name format_loc
      end if
   else
      display by name gr_txppmast.txpp_loc_no,
                      gr_txppmast.txpp_loc_no_suff,
                      gr_txppmast.txpp_loc_street,
                      gr_txppmast.txpp_loc_apt
   end if

   if (find_cursor_open) then
      if (removed) then
         call spsetwin_navbar(0, fetchno, record_count, "D")
      else
         call spsetwin_navbar(0, fetchno, record_count, " ")
      end if
   end if

   if (ct_sw = "T" and show_warning) then
      call check_update()
   end if

end function
###########################################################################
function get_record()

   open txppmast_cursor using active_mt_sw,
                              active_id_no,
                              active_year,
                              active_list_no
   fetch txppmast_cursor into gr_txppmast.*

   case
      when (sqlca.sqlcode = 100)
         let gr_txppmast.* = nl_txppmast.*
         let gr_txppmast.txpp_mt_sw = active_mt_sw
         let gr_txppmast.txpp_id_no = active_id_no
         let gr_txppmast.txpp_year = active_year
         let gr_txppmast.txpp_list_no = active_list_no
         let id_10 = " "
         return 1
      when (sqlca.sqlcode != 0)
         call spsyserr("Error fetching record.",
                       "Fetch error 275.")
         return 2
   end case

   let id_10 = gr_txppmast.txpp_id_no using "<<<<<<<<<<"

   return 0

end function
###########################################################################
function get_customer(in_parcel, in_year, in_list_no)
   define in_parcel char(20),
          in_year smallint,
          in_list_no integer,
          ret_customerDeliveryAddressStatusActive boolean

   if (ct_sw = "T") or (in_year > 0) then
      open owner_cursor using active_mt_sw,
                              in_parcel,
                              in_year,
                              in_list_no
      fetch owner_cursor into cust_acct,
                              addl_no
      if (sqlca.sqlcode != 0) then
         let cust_acct = 0
         let addl_no = null
      end if
   else
      open cust_tag_cursor using in_parcel
      fetch cust_tag_cursor into cust_acct,
                                 addl_no
      if (sqlca.sqlcode) then
         let cust_acct = 0
          let addl_no = null
      end if
   end if
   call get_address(cust_acct, addl_no) returning cid_error, ret_customerDeliveryAddressStatusActive

   return ret_customerDeliveryAddressStatusActive

end function
###########################################################################
function get_address(in_acct, in_addr_no)
   define lv_success,
          lv_isActive,
          lv_isConfidential,
          ret_customerDeliveryAddressStatusActive,
          ret_error boolean,
          in_acct,
          in_addr_no integer,
          lv_FormattedNameAndAddress CustomerManager.FormattedNameAndAddress,
          lv_FormattedContactInformation CustomerManager.FormattedContactInformation

   call spmdebug_say("TXPPMAST: get_address: Begin: ", current hour to fraction)
   call spmdebug_say("TXPPMAST: get_address: in_acct: ", in_acct)
   call spmdebug_say("TXPPMAST: get_address: in_addr_no: ", in_addr_no)

   # Initialize local variable(s)...
   let lv_success = true
   let ret_error = false
   let ret_customerDeliveryAddressStatusActive = true
   
   call spmdebug_say("TXPPMAST: get_address: public_access: ", public_access)
   if ((in_acct is null) or
       (in_acct is not null and in_acct = 0) or
       (in_addr_no is null) or
       (in_addr_no is not null and in_addr_no < 0)) then
      # This would be the case when the record is deleted...
      let cust_name = " "
      let cust_name2 = " "
      let cust_addr = " "
      let cust_addr2 = " "
      let cust_city = " "
      let cust_state = " "
      let cust_zip = " "
      let cust_country = " "
      let trash_phone = " "
      let fax = " "
      let email = " "
   else
      if (public_access = "Y") then
         let cust_name = confid_msg
         let cust_name2 = confid_msg
         let cust_addr = confid_msg
         let cust_addr2 = confid_msg
         let cust_city = confid_msg
         let cust_state = confid_msg
         let cust_zip = confid_msg
         let cust_country = confid_msg
         let trash_phone = confid_msg
         let fax = confid_msg
         let email = confid_msg
      else
         call CustomerManager.isCustomerConfidential(in_acct) returning lv_isConfidential
         if (ExceptionState.hasException()) then
            call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_DISPLAYDIALOG, "Error selecting owner's confidential indicator for Customer ID: "|| in_acct ||". Line: "|| __LINE__)
            let lv_success = false
         end if
         if (lv_success) then
            if (public_access = "Y" and lv_isConfidential) then
               let cust_name = confid_msg
               let cust_name2 = confid_msg
               let cust_addr = confid_msg
               let cust_addr2 = confid_msg
               let cust_city = confid_msg
               let cust_state = confid_msg
               let cust_zip = confid_msg
               let cust_country = confid_msg
               let trash_phone = confid_msg
               let fax = confid_msg
               let email = confid_msg
            else
               # Verify that the CID/Addr# is valid/active
               call CustomerManager.isDeliveryAddressActive(in_acct, in_addr_no) returning lv_isActive
               if (ExceptionState.hasException()) then
                  if (ExceptionState.hasNotFoundSQLException()) then
                     call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_DISPLAYDIALOG, sfmt("Customer not found for ID %1 and delivery address %2. Line %3.", in_acct, in_addr_no, __LINE__))
                  else
                     call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_DISPLAYDIALOG, sfmt("Error selecting customer status for customer ID %1 and delivery address %2. Line %3.", in_acct, in_addr_no, __LINE__))
                  end if
                  let lv_success = false
               else
                  if (not lv_isActive) then
                     let ret_customerDeliveryAddressStatusActive = false
                  end if
               end if

               # Get the formatted name, address, and contact info...
               if (lv_success) then
                  call CustomerManager.getFormattedNameAndAddress(in_acct, in_addr_no) returning lv_FormattedNameAndAddress.*
                  if (ExceptionState.hasException()) then
                     call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_DISPLAYDIALOG, sfmt("Error selecting owner name/address for Customer ID '%1' and delivery addr '%2'. Line %3. Error code", in_acct, in_addr_no, __LINE__))
                     let lv_success = false
                  end if
               end if

               if (lv_success) then
                  call CustomerManager.getFormattedContactInformation(in_acct, in_addr_no) returning lv_FormattedContactInformation.*
                  if (ExceptionState.hasException()) then
                     call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_DISPLAYDIALOG, sfmt("Error selecting owner contact info for Customer ID '%1' and delivery addr '%2'. Line %3. Error code", in_acct, in_addr_no, __LINE__))
                     let lv_success = false
                  end if
               end if

               call spmdebug_say("TXPPMAST: get_address: lv_success: ", lv_success)
               if (lv_success) then
                  let cust_name = lv_FormattedNameAndAddress.FormattedName1
                  let cust_name2 = lv_FormattedNameAndAddress.FormattedName2
                  let cust_addr = lv_FormattedNameAndAddress.Line1
                  let cust_addr2 = lv_FormattedNameAndAddress.Line2
                  let cust_city = lv_FormattedNameAndAddress.City
                  let cust_state = lv_FormattedNameAndAddress.State
                  let cust_zip = lv_FormattedNameAndAddress.ZipCode
                  let cust_country = lv_FormattedNameAndAddress.Country
                  let trash_phone = lv_FormattedContactInformation.PhoneNumber
                  let fax = lv_FormattedContactInformation.FaxNumber
                  let email = lv_FormattedContactInformation.EmailAddress
               end if
            end if
         end if
      end if
   end if

   call spmdebug_say("TXPPMAST: get_address: cust_name: ", cust_name)
   call spmdebug_say("TXPPMAST: get_address: cust_name2: ", cust_name2)
   call spmdebug_say("TXPPMAST: get_address: cust_addr: ", cust_addr)
   call spmdebug_say("TXPPMAST: get_address: cust_addr2: ", cust_addr2)
   call spmdebug_say("TXPPMAST: get_address: cust_city: ", cust_city)
   call spmdebug_say("TXPPMAST: get_address: cust_state: ", cust_state)
   call spmdebug_say("TXPPMAST: get_address: cust_zip: ", cust_zip)
   call spmdebug_say("TXPPMAST: get_address: cust_country: ", cust_country)
   call spmdebug_say("TXPPMAST: get_address: trash_phone: ", trash_phone)
   call spmdebug_say("TXPPMAST: get_address: fax: ", fax)
   call spmdebug_say("TXPPMAST: get_address: email: ", email)
   call spmdebug_say("TXPPMAST: get_address: lv_success: ", lv_success)
   if (not lv_success) then
      let ret_error = true
      let ret_customerDeliveryAddressStatusActive = false
   end if

   call spmdebug_say("TXPPMAST: get_address: ret_error: ", ret_error)
   call spmdebug_say("TXPPMAST: get_address: ret_customerDeliveryAddressStatusActive: ", ret_customerDeliveryAddressStatusActive)
   call spmdebug_say("TXPPMAST: get_address: Complete: ", current hour to fraction)
   return ret_error, ret_customerDeliveryAddressStatusActive

end function
###########################################################################
function check_update()
   define check_date date,
          check_clerk like txppmast.txpp_last_updt_by

   select txpp_last_date, txpp_last_updt_by
     into check_date, check_clerk
     from txppmas
     where txpp_mt_sw = 0
       and txpp_id_no = active_id_no
       and txpp_year = 0
       and txpp_list_no = 0

   if (gr_txppmast.txpp_last_date < check_date) then
      let dlg_str = "Warning - Current property was updated on \n",
                    check_date using "mm/dd/yyyy", " by ", check_clerk, ". \n",
                    "Tax property was updated on ",
                    gr_txppmast.txpp_last_date using "mm/dd/yyyy", " by ",
                    gr_txppmast.txpp_last_updt_by, "."
      call spaskrtn_msgBoxChk("Property Updated", dlg_str, 2971, FALSE, "txppmast")
   end if

end function
###########################################################################
function check_null(entered_no)
   define entered_no decimal(12,3)

   if (entered_no IS NULL) then
      return 0
   else
      return entered_no
   end if

end function
###########################################################################
function close_cursor()

   call spsetwin_clearForm(whdl_main)

   if (find_cursor_open) then
      let find_cursor_open = FALSE
      close find_cursor
   end if

   let record_count   = 0
   let fetchno        = 0
   let active_mt_sw   = 0
   let active_id_no   = 0
   let char_id_no     = SPACE
   let active_year    = 0
   let active_list_no = 0

end function
###########################################################################
function get_tot_sqft()
   define amount like txvalues.txva_amt,
          square_feet like txvalues.txva_sq_ft

   let tot_sqft = 0
   let gross_assmt = 0

   open txvalues_cursor using active_mt_sw,
                              char_id_no,
                              active_year,
                              active_list_no
   fetch txvalues_cursor into amount, square_feet
   while (NOT sqlca.sqlcode)
      let gross_assmt = gross_assmt + amount
      let tot_sqft = tot_sqft + check_int(square_feet)
      fetch txvalues_cursor into amount, square_feet
   end while

end function
###########################################################################
function calc_net_value()

   if (active_mt_sw = 0) then  #CURRENT
      select sum(txcstexm.txce_amt) into ex_amt
         from txcstexm
         where txcstexm.txce_acct = cust_acct
           and txcstexm.txce_ar_cat = 25
           and txcstexm.txce_prop_id = char_id_no
   else     # TAX
      select sum(txexemps.txxm_amt) into ex_amt
         from txexemps
         where txexemps.txxm_mt_sw = active_mt_sw
           and txexemps.txxm_ar_cat = 25
           and txexemps.txxm_prop_id = char_id_no
           and txexemps.txxm_year = active_year
           and txexemps.txxm_list_no = active_list_no
   end if

   if (ex_amt is NULL) then
      let ex_amt = 0
   end if

   let cynet = gross_assmt - ex_amt

   if (cynet < 0) then
      let cynet = 0
   end if

   return cynet

end function
#########################################################################
function check_permit()

   if (updt_inact = "N" and gr_txppmast.txpp_status = "I") then
      let err_msg = "You do not have permission to maintain ",
                    "inactive properties."
      call sperrorm(err_msg, 0, 2)
      return FALSE
   else
      return TRUE
   end if

end function
###########################################################################
function menu_scrn9(audit_fetch, audit_count)
   define audit_fetch,
          audit_count   integer

   let whdl_audit = spsetwin_new("txppmast12",
                                 "Audit Detail",
                                 "appsub_nomenu", "")

   call spsetwin_navbar(0,0,0," ")
   call display_scrn9(audit_fetch, audit_count)

   menu ""
      before menu
      if (ST_CLIENT_STATE != "VA") then
         call spdomlib_hideAllByTag("VA_only", TRUE)
      end if

      on action nav_first
         call sperrorm_blankmess()
         if (audit_count) then
            let audit_fetch = 1
            call display_scrn9(audit_fetch, audit_count)
         else
            call spnodata()
         end if

      on action nav_next
         call sperrorm_blankmess()
         if (audit_fetch < audit_count) then
            let audit_fetch = audit_fetch + 1
            call display_scrn9(audit_fetch, audit_count)
         else
            call spnonext()
         end if

      on action nav_previous
         call sperrorm_blankmess()
         if (audit_fetch > 1) then
            let audit_fetch = audit_fetch - 1
            call display_scrn9(audit_fetch, audit_count)
         else
            call spnoprev()
         end if

      on action nav_last
         call sperrorm_blankmess()
         if (audit_count) then
            let audit_fetch = audit_count
            call display_scrn9(audit_fetch, audit_count)
         else
            call spnodata()
         end if

      on action close
         let int_flag = FALSE
         if (spaskrtn_closeWindow(DIALOG)) then
            exit menu
         else
            continue menu
         end if


      &include "StandardSubDialogActions.inc"

   end menu

   call spsetwin_hdlClose(whdl_audit)

   return audit_fetch

end function
#################################################################
function display_scrn9(audit_fetch, audit_count)
   define audit_count                  integer,
          audit_fetch                  integer,
          lr_txppmaud                  record like txppmaud.*

   if (ct_sw = 'C') then
      open current_audit_find using active_id_no,
                                    char_id_no
      fetch absolute audit_fetch current_audit_find into gr_audit.*
      if (sqlca.sqlcode != 0) then
         call spsyserr("Error fetching current audit record", sfmt("Fetch current_audit_find %1.", __LINE__))
         close current_audit_find
         initialize gr_audit.* to NULL
         return
      end if
   else
      open tax_audit_find using active_id_no,
                                active_mt_sw,
                                char_id_no
      fetch absolute audit_fetch tax_audit_find into gr_audit.*
      if (sqlca.sqlcode != 0) then
         call spsyserr("Error fetching tax audit record", sfmt("Fetch tax_audit_find %1.", __LINE__))
         initialize gr_audit.* to NULL
         close tax_audit_find
         return
      end if
   end if

   let lr_txppmaud.txpa_action = gr_audit.a_action
   let lr_txppmaud.txpa_date = gr_audit.a_date
   let lr_txppmaud.txpa_time = gr_audit.a_time
   let lr_txppmaud.txpa_clerk = gr_audit.a_clerk
   let lr_txppmaud.txpa_ctb_sw = gr_audit.a_file
   let lr_txppmaud.txpa_table = gr_audit.a_table
   let lr_txppmaud.txpa_seq = gr_audit.t_seq
   let lr_txppmaud.txpa_field = gr_audit.a_field
   display by name lr_txppmaud.txpa_action,
                   lr_txppmaud.txpa_date,
                   lr_txppmaud.txpa_time,
                   lr_txppmaud.txpa_clerk,
                   lr_txppmaud.txpa_ctb_sw,
                   lr_txppmaud.txpa_table,
                   lr_txppmaud.txpa_seq,
                   gr_audit.mv_year,
                   gr_audit.mv_vin,
                   lr_txppmaud.txpa_field,
                   gr_audit.a_old_val,
                   gr_audit.a_new_val

   call spsetwin_navbar(0, audit_fetch, audit_count, " ")

end function
###############################################################################
function initial_values()

   let gr_txppmast.txpp_mt_sw        = 0
   let gr_txppmast.txpp_id_no        = 0
   let gr_txppmast.txpp_year         = 0
   let gr_txppmast.txpp_list_no      = 0
   let gr_txppmast.txpp_parc_id      = " "
   let gr_txppmast.txpp_old_id       = " "
   let gr_txppmast.txpp_status       = "A"
   let gr_txppmast.txpp_full_exem_cd = " "
   let gr_txppmast.txpp_create_date  = ST_TODAY
   let gr_txppmast.txpp_inact_date   = NULL
   let gr_txppmast.txpp_ar_cat       = 25
   let gr_txppmast.txpp_class_cd     = " "
   let gr_txppmast.txpp_loc_no       = 0
   let gr_txppmast.txpp_loc_no_suff  = " "
   let gr_txppmast.txpp_loc_street   = " "
   let gr_txppmast.txpp_loc_apt      = " "
   let gr_txppmast.txpp_juris_cd     = " "
   let gr_txppmast.txpp_last_updt_by = ST_USER
   let gr_txppmast.txpp_last_date    = ST_TODAY
   let gr_txppmast.txpp_last_time    = current hour to minute
   let gr_txppmast.txpp_subdiv       = " "
   let gr_txppmast.txpp_nature_own   = " "
   let gr_txppmast.txpp_business     = " "
   let gr_txppmast.txpp_dba          = " "
   let gr_txppmast.txpp_date_field   = NULL
   let gr_txppmast.txpp_who_field    = " "
   let gr_txppmast.txpp_date_desk    = NULL
   let gr_txppmast.txpp_who_desk     = " "
   let gr_txppmast.txpp_purch_date   = NULL
   let gr_txppmast.txpp_assmt_ly     = 0
   let gr_txppmast.txpp_ma           = " "
   let gr_txppmast.txpp_resident     = "Y"
   if (ST_CLIENT_STATE = "VA") then
      let gr_txppmast.txpp_formal_lst = "Y"
   else
      let gr_txppmast.txpp_formal_lst = "N"
   end if
   let gr_txppmast.txpp_fml_lst_yr   = 0
   let gr_txppmast.txpp_filed        = "N"
   let gr_txppmast.txpp_nc           = " "
   let gr_txppmast.txpp_abst_type    = " "
   let gr_txppmast.txpp_list_status  = " "
   let gr_txppmast.txpp_abst_chng    = " "
   let gr_txppmast.txpp_comply_date  = NULL
   let gr_txppmast.txpp_comply_no    = 0
   let gr_txppmast.txpp_comply_id    = " "
   let gr_txppmast.txpp_comply_dttm  = " "
   let gr_txppmast.txpp_ag_value     = 0
   let gr_txppmast.txpp_ag_tax       = 0
   let gr_txppmast.txpp_ct           = " "
   let gr_txppmast.txpp_sect         = " "
   let gr_txppmast.txpp_comply       = "N"
   let gr_txppmast.txpp_audit        = "N"
   let gr_txppmast.txpp_audit_yr     = 0
   let gr_txppmast.txpp_pos_audit    = "N"
   let gr_txppmast.txpp_detail_date  = NULL
   let gr_txppmast.txpp_loc_id       = " "
   let gr_txppmast.txpp_loc_seq      = 0
   let loc_seq                       = " "
   let format_loc                    = " "

   let cust_acct      = 0
   let addl_no        = null
   let cust_name      = " "
   let cust_name2     = " "
   let cust_addr      = " "
   let cust_addr2     = " "
   let cust_city      = " "
   let cust_state     = " "
   let cust_zip       = " "
   let cust_country   = " "
   let gv_customerSSN = " "
   let gv_oldID       = " "
   let lender_name    = " "
   let tot_sqft       = 0

   display by name gr_txppmast.txpp_id_no,
                   gr_txppmast.txpp_year,
                   gr_txppmast.txpp_list_no,
                   gr_txppmast.txpp_mt_sw,
                   gr_txppmast.txpp_dba,
                   gr_txppmast.txpp_parc_id,
                   gv_oldID,
                   cust_acct,
                   addl_no,
                   gv_customerSSN,
                   cust_name,
                   cust_name2,
                   cust_addr,
                   cust_addr2,
                   cust_city,
                   cust_state,
                   cust_zip,
                   cust_country,
                   gr_txppmast.txpp_loc_no,
                   gr_txppmast.txpp_loc_no_suff,
                   gr_txppmast.txpp_loc_street,
                   gr_txppmast.txpp_loc_apt,
                   gr_txppmast.txpp_status,
                   gr_txppmast.txpp_abst_type,
                   gr_txppmast.txpp_business,
                   gr_txppmast.txpp_list_status,
                   gr_txppmast.txpp_full_exem_cd,
                   gr_txppmast.txpp_juris_cd,
                   gr_txppmast.txpp_class_cd,
                   gr_txppmast.txpp_subdiv,
                   tot_sqft,
                   gr_txppmast.txpp_resident,
                   gr_txppmast.txpp_date_field,
                   gr_txppmast.txpp_who_field,
                   gr_txppmast.txpp_date_desk,
                   gr_txppmast.txpp_who_desk,
                   gr_txppmast.txpp_comply,
                   gr_txppmast.txpp_fml_lst_yr,
                   gr_txppmast.txpp_audit,
                   gr_txppmast.txpp_audit_yr,
                   gr_txppmast.txpp_pos_audit,
                   gr_txppmast.txpp_assmt_ly

   if (ST_CLIENT_STATE = "VA") then
      call checkViewPermissions()
      display by name gr_txppmast.txpp_nature_own,
                      lender_name
   else
      display by name gr_txppmast.txpp_date_field,
                      gr_txppmast.txpp_who_field,
                      gr_txppmast.txpp_date_desk,
                      gr_txppmast.txpp_who_desk,
                      gr_txppmast.txpp_comply,
                      gr_txppmast.txpp_fml_lst_yr,
                      gr_txppmast.txpp_audit,
                      gr_txppmast.txpp_audit_yr,
                      gr_txppmast.txpp_pos_audit

   end if

end function
###########################################################################
function insert_custtag()

   define tmp_val_array       dynamic array of record
             invalid_field      string,
             invalid_reason     string
                              end record,
          tmp_success         smallint,
          tmp_err_msg         string,
          retval              smallint,
          gr_txcsttag         record like txcsttag.*

   let retval = TRUE

   let char_id_no = gr_txppmast.txpp_id_no using "<<<<<<<<<&"

   select * into gr_txcsttag.*
     from txcsttag
    where txtg_acct = cust_acct
      and txtg_ar_cat = 25
      and txtg_own = "P"
      and txtg_prop_id = char_id_no

   if (sqlca.sqlcode) then
      let gr_txcsttag.txtg_acct         = cust_acct
      let gr_txcsttag.txtg_ar_cat       = 25
      let gr_txcsttag.txtg_own          = "P"
      let gr_txcsttag.txtg_prop_id      = char_id_no
      let gr_txcsttag.txtg_pct_own      = 100
      let gr_txcsttag.txtg_status       = "N"
      let gr_txcsttag.txtg_aka_seq      = 0
      let gr_txcsttag.txtg_bill_addr_no = addl_no
      let gr_txcsttag.txtg_x_acct       = 0
      let gr_txcsttag.txtg_x_addr_no    = 0
      call txcsttagwr_insert(gr_txcsttag.*, FALSE, tmp_val_array, TRUE, TRUE)
                              returning tmp_success, tmp_err_msg
      if (NOT tmp_success) then
         call sperrorm(tmp_err_msg, 0, 2)
         let retval = FALSE
      else
         let retval = TRUE
      end if
   end if

   return retval

end function
###############################################################################
function insert_chgdefs(pick_method, in_txppmast)
   define activity char(4),
          charge_def char(6),
          char_id_no char(30),
          tmp_err_msg string,
          pick_method,
          retval,
          tmp_multijuris,
          tmp_success smallint,
          check_chgs integer,
          base_pct decimal(9,6),
          rate decimal(11,6),
          charge ChargeType,
          lr_txchgdef record like txchgdef.*,
          in_txppmast record like txppmast.*,
          tmp_val_array dynamic array of record
             invalid_field string,
             invalid_reason string
          end record

   # Initialize local variable(s)...
   let retval = TRUE

   # Initialize local table(s)...
   initialize lr_txchgdef.* to NULL

   call txyrparmwr_getMultiJurisFlag(in_txppmast.txpp_year, 25, use_cycle)
         returning tmp_success, tmp_multijuris

   if (NOT tmp_multijuris) then
      if (pick_method = 1) then
         open juriscd_chgdef using in_txppmast.txpp_juris_cd

         while TRUE
            fetch juriscd_chgdef into charge_def,
                                      base_pct
            if (sqlca.sqlcode) then
               exit while
            end if

            let charge = archgobj_open(25, assm_year, charge_def)
            if charge IS NULL then
               continue while
            else
               let rate = archgobj_getRateAmount(charge)
               let activity = archgobj_getActivity(charge)
               call archgobj_free(charge)
            end if

            let char_id_no = in_txppmast.txpp_id_no

            let lr_txchgdef.txch_mt_sw        = in_txppmast.txpp_mt_sw
            let lr_txchgdef.txch_ar_cat       = 25            
            let lr_txchgdef.txch_prop_id      = char_id_no
            let lr_txchgdef.txch_year         = in_txppmast.txpp_year
            let lr_txchgdef.txch_list_no      = in_txppmast.txpp_list_no
            let lr_txchgdef.txch_chg_def      = charge_def
            let lr_txchgdef.txch_net_assmt    = 0
            let lr_txchgdef.txch_pct_base     = base_pct
            let lr_txchgdef.txch_count        = 0
            let lr_txchgdef.txch_rate         = rate
            let lr_txchgdef.txch_amt          = 0
            let lr_txchgdef.txch_actvty       = activity
            let lr_txchgdef.txch_prelim_amt12 = 0
            let lr_txchgdef.txch_prelim_amt3  = 0
            let lr_txchgdef.txch_re_gross     = 0
            let lr_txchgdef.txch_re_defer     = 0
            let lr_txchgdef.txch_re_exem      = 0
            let lr_txchgdef.txch_pp_gross     = 0
            let lr_txchgdef.txch_pp_exem      = 0
            let lr_txchgdef.txch_seq          = 0
            let lr_txchgdef.txch_levy_year    = lr_txchgdef.txch_year

            call txchgdefwr_insert(lr_txchgdef.*, FALSE, tmp_val_array, TRUE, TRUE)
                         returning tmp_success, tmp_err_msg
            if (NOT tmp_success) then
               call sperrorm(tmp_err_msg, 0, 2)
               let retval = FALSE
               exit while
            end if
         end while
      end if

      if (pick_method = 2) then
         if (old_txppmast.txpp_juris_cd != in_txppmast.txpp_juris_cd) then
            let char_id_no = in_txppmast.txpp_id_no
            open juriscd_chgdef using old_txppmast.txpp_juris_cd

            while TRUE
               fetch juriscd_chgdef into charge_def,
                                         base_pct
               if (sqlca.sqlcode) then
                  exit while
               end if

               open select_txchgs_j using in_txppmast.txpp_mt_sw,
                                          char_id_no,
                                          in_txppmast.txpp_year,
                                          in_txppmast.txpp_list_no,
                                          charge_def
               fetch select_txchgs_j into lr_txchgdef.*
               if (sqlca.sqlcode) then
                  if (sqlca.sqlcode = NOTFOUND) then
                     continue while
                  else
                     let err_msg = "Error trying to select from txchgdef."
                     call spsyserr(err_msg, "select_txchgs_j - 9238")
                     close select_txchgs_j
                     if (NOT sp_rollback_work(2837)) then
                     end if
                     return FALSE
                  end if
               end if

               if (NOT lock_txch_rec(lr_txchgdef.txch_mt_sw,
                                     lr_txchgdef.txch_ar_cat,
                                     lr_txchgdef.txch_prop_id,
                                     lr_txchgdef.txch_year,
                                     lr_txchgdef.txch_list_no,
                                     lr_txchgdef.txch_seq)) then
                  if (NOT sp_rollback_work(2333)) then
                  end if
                  return FALSE
               end if

               execute delete_txch_j
               if (sqlca.sqlcode or sqlca.sqlerrd[3] = 0) then
                  let err_msg = "Error on delete from txchgdef."
                  call spsyserr(err_msg, "Delete txchgdef - 2982")
                  if (NOT sp_rollback_work(3924)) then
                  end if
                  close lock_txch_j
                  return FALSE
               end if

            end while

            open juriscd_chgdef using in_txppmast.txpp_juris_cd

            while TRUE
               fetch juriscd_chgdef into charge_def,
                                         base_pct
               if (sqlca.sqlcode) then
                  exit while
               end if

               if (in_txppmast.txpp_mt_sw = 0) then
                  let charge = archgobj_open(25, assm_year, charge_def)
               else
                  let charge = archgobj_open(25, in_txppmast.txpp_year, charge_def)
               end if

               if (charge IS NULL) then
                  continue while
               else
                  let activity = archgobj_getActivity(charge)
                  let rate = archgobj_getRateAmount(charge)
                  call archgobj_free(charge)
               end if

               let check_chgs = 0
               select count(*)
                 into check_chgs
                 from txchgdef
                where txch_mt_sw = in_txppmast.txpp_mt_sw
                  and txch_ar_cat = 25
                  and txch_prop_id = char_id_no
                  and txch_year = in_txppmast.txpp_year
                  and txch_list_no = in_txppmast.txpp_list_no
                  and txch_chg_def = charge_def
               if (check_chgs > 0) then
                  continue while
               end if

               let lr_txchgdef.txch_mt_sw        = in_txppmast.txpp_mt_sw
               let lr_txchgdef.txch_ar_cat       = 25
               let lr_txchgdef.txch_prop_id      = char_id_no
               let lr_txchgdef.txch_year         = in_txppmast.txpp_year
               let lr_txchgdef.txch_list_no      = in_txppmast.txpp_list_no
               let lr_txchgdef.txch_chg_def      = charge_def
               let lr_txchgdef.txch_net_assmt    = 0
               let lr_txchgdef.txch_pct_base     = base_pct
               let lr_txchgdef.txch_count        = 0
               let lr_txchgdef.txch_rate         = rate
               let lr_txchgdef.txch_amt          = 0
               let lr_txchgdef.txch_actvty       = activity
               let lr_txchgdef.txch_prelim_amt12 = 0
               let lr_txchgdef.txch_prelim_amt3  = 0
               let lr_txchgdef.txch_re_gross     = 0
               let lr_txchgdef.txch_re_defer     = 0
               let lr_txchgdef.txch_re_exem      = 0
               let lr_txchgdef.txch_pp_gross     = 0
               let lr_txchgdef.txch_pp_exem      = 0
               let lr_txchgdef.txch_seq          = 0
               let lr_txchgdef.txch_levy_year    = lr_txchgdef.txch_year

               call txchgdefwr_insert(lr_txchgdef.*, FALSE, tmp_val_array, TRUE, TRUE)
                            returning tmp_success, tmp_err_msg
               if (NOT tmp_success) then
                  call sperrorm(tmp_err_msg, 0, 2)
                  let retval = FALSE
                  exit while
               end if
            end while
         end if
      end if
   end if

   return retval

end function
###############################################################################
function init_combos(cb)
   define form_name char(10),
          fldValue char(20),
          txtStr1 char(40),
          lv_description varchar(50),
          comboDis,
          prepchar string,
          lv_ID integer,
          cb ui.ComboBox

   # Initialize local variable(s)...
   let lv_description = " "
   let lv_ID = 0

   let form_name = spsetwin_getFormName()

   case

      when (cb.getTag() = "txpp_abst_type")

         if (ST_CLIENT_STATE != "VA") then
            if (form_name = "txppmast01") then
               call cb.clear()
               let prepchar = "select prfld_val, ",
                              "       prfld_desc ",
                              "  from prfldval ",
                              " where prfld_fun = 'tx' ",
                              "   and prfld_fld = 'abst_type' ",
                              " order by prfld_val"
               prepare sel_abst_tp from prepchar
               if (sqlca.sqlcode) then
                  call cb.addItem(0, "Error Populating Combo")
                  exit case
               end if
               declare pop_abst_tp cursor for sel_abst_tp
               if (sqlca.sqlcode) then
                  call cb.addItem(0, "Error Populating Combo")
                  exit case
               end if
               open pop_abst_tp
               while TRUE
                  fetch pop_abst_tp into fldValue,
                                         txtStr1
                  if (sqlca.sqlcode) then
                     exit while
                  end if
                  let comboDis = fldValue clipped, " - ", txtStr1 clipped
                  call cb.addItem(fldValue clipped, comboDis clipped)
               end while
            end if
         else
            if (form_name = "txppmast01") then
               call cb.clear()
               let prepchar = "select prfld_val, ",
                              "       prfld_desc ",
                              "  from prfldval ",
                              " where prfld_fun = 'tx' ",
                              "   and prfld_fld = 'va_abst_type' ",
                              " order by prfld_val"
               prepare sel_va_abst_tp from prepchar
               if (sqlca.sqlcode) then
                  call cb.addItem(0, "Error Populating Combo")
                  exit case
               end if
               declare pop_va_abst_tp cursor for sel_va_abst_tp
               if (sqlca.sqlcode) then
                  call cb.addItem(0, "Error Populating Combo")
                  exit case
               end if
               open pop_va_abst_tp
               while TRUE
                  fetch pop_va_abst_tp into fldValue,
                                            txtStr1
                  if (sqlca.sqlcode) then
                     exit while
                  end if
                  let comboDis = fldValue clipped, " - ", txtStr1 clipped
                  call cb.addItem(fldValue clipped, comboDis clipped)
               end while
            end if
         end if

      when (cb.getTag() = "txpp_list_status")
         if (ST_CLIENT_STATE != "VA") then
            if (form_name = "txppmast01") then
               call cb.clear()
               let prepchar = "select prfld_val, ",
                              "       prfld_desc ",
                              "  from prfldval ",
                              " where prfld_fun = 'tx' ",
                              "   and prfld_fld = 'list_stat' ",
                              " order by prfld_val"
               prepare sel_list_st from prepchar
               if (sqlca.sqlcode) then
                  call cb.addItem(0, "Error Populating Combo")
                  exit case
               end if
               declare pop_list_st cursor for sel_list_st
               if (sqlca.sqlcode) then
                  call cb.addItem(0, "Error Populating Combo")
                  exit case
               end if
               open pop_list_st
               while TRUE
                  fetch pop_list_st into fldValue,
                                         txtStr1
                  if (sqlca.sqlcode) then
                     exit while
                  end if
                  let comboDis = fldValue clipped, " - ", txtStr1 clipped
                  call cb.addItem(fldValue clipped, comboDis clipped)
               end while
            end if
         else
            call cb.clear()
            call cb.addItem("L", "L - Filed")
            call cb.addItem("P", "P - Late Filed")
            call cb.addItem("U", "U - Unfiled")
            call cb.addItem("", " ")
         end if

      when (cb.getTag() = "txpp_mt_sw")
         call cb.addItem(0, "0 - Current")
         call cb.addItem(1, "1 - Tax")
         if (ST_CLIENT_STATE != "MA" and
             ST_CLIENT_STATE != "MD" and
             ST_CLIENT_STATE != "NY" and
             ST_CLIENT_STATE != "CT" and
             ST_CLIENT_STATE != "NH" and
             ST_CLIENT_STATE != "MI" and
             ST_CLIENT_STATE != "RI" and
             ST_CLIENT_STATE != "LA" and
             ST_CLIENT_STATE != "VT") then
            call cb.addItem(7, "7 - Subsequent")
            call cb.addItem(8, "8 - Correction")
            call cb.addItem(9, "9 - Working")
            call cb.addItem(10, "10 - Original")
         end if

      when (cb.getTag() = "txpp_business")
         # Obtain business type descriptions...
         open getCustomerTypeDescriptions
         while (TRUE)
            fetch getCustomerTypeDescriptions into lv_ID, lv_description
            if (sqlca.sqlcode) then
               if (sqlca.sqlcode != 100) then
                  call spsyserr("Error fetching getCustomerTypeDescriptions.", "Fetch getCustomerTypeDescriptions, Line: " || __LINE__)
               end if
               exit while
            end if

            call cb.addItem(lv_ID, lv_description clipped)
         end while
         close getCustomerTypeDescriptions

   end case

end function
###############################################################################
function hide_fields()

   if (ST_CLIENT_STATE != "CT" AND
       ST_CLIENT_STATE != "NC" AND
       ST_CLIENT_STATE != "RI" AND
       ST_CLIENT_STATE != "TN" AND
       ST_CLIENT_STATE != "VA") then
      call spdomlib_hideAllByTag("filing_only", TRUE)
   end if

   if (ST_CLIENT_STATE == "CT") then
      call spdomlib_hideAllByTag("txpp_formal_lst", TRUE)
      call spdomlib_hideAllByTag("txpp_comply_no", TRUE)

      call spdomlib_setLabelText("comp_dt_lbl", "Notice of change date")
      call spdomlib_setTextByTag("filing_only","Notice of change")
   else
      call spdomlib_hideAllByTag("street_cd", TRUE)
   end if

   if (ST_CLIENT_STATE == "VA") then
      call spdomlib_hideAllByTag("exem_r_btn", TRUE)
      call spdomlib_hideAllByTag("filing_status", FALSE)
      #call spdomlib_setNoEntry("txpp_comply_date",TRUE)
      # Not showing label of Filing date for VA to eliminate confusion when it is really Late Filing Date
      #call spdomlib_setLabelText("comp_dt_lbl", " ")
      call spdomlib_hideAllByTag("VA_SSN_only", FALSE)
   else
      call spdomlib_hideAllByTag("NOT_VA", FALSE)
   end if

   if (ST_CLIENT_STATE == "NC") then
      call spdomlib_hideAllByTag("txpp_formal_lst", TRUE)
      call spdomlib_hideAllByTag("cmpl_date", TRUE)
      call spdomlib_hideAllByTag("txpp_comply_date", TRUE)
      call spdomlib_setNoEntry("txpp_comply_no",TRUE)
   end if

   if (ST_CLIENT_STATE == "TN") then
      call spdomlib_hideAllByTag("txpp_formal_lst", TRUE)
      call spdomlib_hideAllByTag("txpp_comply_no", TRUE)
      call spdomlib_hideAllByTag("txpp_list_status", TRUE)
   end if

   if (ST_CLIENT_STATE != "VA" and
       ST_CLIENT_STATE != "NC" and
       ST_CLIENT_STATE != "TN" and
       ST_CLIENT_STATE != "MO") then
      call spdomlib_hideAllByTag("txpp_abst_type", TRUE)
   end if

   if (ST_CLIENT_STATE != "NC" and ST_CLIENT_STATE != "VA") then
      call spdomlib_hideAllByTag("txpp_list_status", TRUE)
   end if

   if (ST_CLIENT_STATE == "NC" or ST_CLIENT_STATE == "TN") then
      call spdomlib_hideAllByTag("not_NC_or_TN", TRUE)
   end if

end function
###############################################################################
function change_field_attr()

   case ST_CLIENT_STATE

      when "CT"
         call spdomlib_setComment("txpp_comply_date", "Date the last notice of change printed.")

      when "NC"
         call spdomlib_setLabelText("comp_no_lbl", "Filing number")
         call spdomlib_setComment("txpp_comply_no", "Filing number.")

      when "VA"
         call spdomlib_setLabelText("list_lbl", "Filing status")
         call spdomlib_setComment("txpp_list_status", "Filing status.")

      when "MO"
         call spdomlib_setLabelText("lend_lbl", "County")
         call spdomlib_setComment("txpp_nature_own", "County code. Click for help.")

   end case
end function
###############################################################################
function set_styles()

   if (pass_let = "I") then
      call spdomlib_setFldStyle("type_lbl",       "reql", "D")
      call spdomlib_setFldStyle("txpp_abst_type", "reqf", "D")
      call spdomlib_setFldStyle("prop_id_lbl",   "reql", "D")
      call spdomlib_setFldStyle("txpp_id_no",    "reqf", "D")
      call spdomlib_setFldStyle("bus_lbl",       "reql", "D")
      call spdomlib_setFldStyle("cust_acct",     "reqf", "D")
      call spdomlib_setFldStyle("jur_lbl",       "reql", "D")
      call spdomlib_setFldStyle("txpp_juris_cd", "reqf", "D")
      call spdomlib_setFldStyle("clas_lbl",      "reql", "D")
      call spdomlib_setFldStyle("txpp_class_cd", "reqf", "D")
   else
      if (ST_CLIENT_STATE != "NC" and
          ST_CLIENT_STATE != "TN" and
          ST_CLIENT_STATE != "VA" and
          ST_CLIENT_STATE != "MO") then
         call spdomlib_setFldStyle("type_lbl",       "reql", "D")
         call spdomlib_setFldStyle("txpp_abst_type", "reqf", "D")
      end if
   end if

end function
###############################################################################
function input_routine(update_flag)
   define update_flag         integer,
          retcode_jur         like txjurisd.txju_code,
          retdesc_jur         like txjurisd.txju_desc,
          retcode_cls         like txclascd.txcl_class,
          retdesc_cls         like txclascd.txcl_desc,
          retcode_sub         like txsubdiv.txsd_code,
          retdesc_sub         like txsubdiv.txsd_desc,
          retcode_st          like txstreet.txst_code,
          retdesc_st          like txstreet.txst_name,
          retcode_ex          like arexdisc.ared_code,
          retdesc_ex          like arexdisc.ared_desc,
          retcode_lnd         like arlender.arlr_code,
          retdesc_lnd         like arlender.arlr_name,
          retcode_own         like txnatown.txno_code,
          retdesc_own         like txnatown.txno_desc,
          tmp_num             integer,
          tmp_str             string,
          retval              smallint,
          tmp_loc_desc        string,
          tmp_loc_city_st_zip string,
          hold_loc_id         like txppmast.txpp_loc_id,
          lv_customerDeliveryAddressStatusActive boolean,
          lv_returnAction,
          lv_customerNumber integer,
          lv_returnDeliveryAddress CustomerManager.DeliveryAddress,
          lv_createOrUpdateRestrictions CustomerManager.CreateOrUpdateRestrictions,
          lv_prepopulatedMatchingCriteria CustomerManager.MatchingCriteria

   let int_flag   = FALSE
   let WHDL_CLOSE = FALSE

   let street_cd = " "

   options input no wrap

   input by name gr_txppmast.txpp_id_no,
                 gr_txppmast.txpp_parc_id,
                 gv_oldID,
                 cust_acct,
                 addl_no,
                 gr_txppmast.txpp_dba,
                 gr_txppmast.txpp_loc_no,
                 gr_txppmast.txpp_loc_no_suff,
                 street_cd,
                 gr_txppmast.txpp_loc_street,
                 gr_txppmast.txpp_loc_apt,
                 gr_txppmast.txpp_loc_id,
                 gr_txppmast.txpp_status,
                 gr_txppmast.txpp_inact_date,
                 gr_txppmast.txpp_full_exem_cd,
                 gr_txppmast.txpp_abst_type,
                 gr_txppmast.txpp_business,
                 gr_txppmast.txpp_juris_cd,
                 gr_txppmast.txpp_class_cd,
                 gr_txppmast.txpp_subdiv,
                 gr_txppmast.txpp_nature_own,
                 gr_txppmast.txpp_list_status,
                 gr_txppmast.txpp_formal_lst,
                 gr_txppmast.txpp_comply_no,
                 gr_txppmast.txpp_comply_date,
                 gr_txppmast.txpp_pos_audit,
                 gr_txppmast.txpp_resident,
                 gr_txppmast.txpp_date_field,
                 gr_txppmast.txpp_who_field,
                 gr_txppmast.txpp_date_desk,
                 gr_txppmast.txpp_who_desk,
                 gr_txppmast.txpp_comply,
                 gr_txppmast.txpp_fml_lst_yr,
                 gr_txppmast.txpp_audit,
                 gr_txppmast.txpp_audit_yr,
                 gr_txppmast.txpp_assmt_ly
                 without defaults attribute(unbuffered)

      before input
         if (update_flag) then
            call Dialog.setFieldActive("txpp_id_no", FALSE)
            call Dialog.setFieldActive("cust_acct", FALSE)
            call Dialog.setFieldActive("addl_no", FALSE)
         end if
         if (ct_sw != "T") then
            let gr_txppmast.txpp_list_status = " "
            display by name gr_txppmast.txpp_list_status
            call Dialog.setFieldActive("txpp_list_status", FALSE)
         end if
         if (gr_txppmast.txpp_status != "E") then
            let gr_txppmast.txpp_full_exem_cd = " "
            let exem_desc = " "
            display by name exem_desc
            call Dialog.setFieldActive("txpp_full_exem_cd", FALSE)
         end if
         if (gr_txppmast.txpp_status != "I") then
            let gr_txppmast.txpp_inact_date = null
            call Dialog.setFieldActive("txpp_inact_date", FALSE)
         end if
         if (gr_txppmast.txpp_comply = "N") then
            let gr_txppmast.txpp_fml_lst_yr = " "
            call Dialog.setFieldActive("txpp_fml_lst_yr", FALSE)
         end if
         if (gr_txppmast.txpp_audit = "N") then
            let gr_txppmast.txpp_audit_yr = 0
            call Dialog.setFieldActive("txpp_audit_yr", FALSE)
         end if
         if (ST_CLIENT_STATE == "CT") then
            call Dialog.setFieldActive("txpp_comply_date", FALSE)
         end if
         if (ST_CLIENT_STATE == "VA") then
            call checkEntryPermissions()
         end if

      before field txpp_parc_id
         if (NOT update_flag) then
            if (NOT verify_field("txpp_id_no", update_flag)) then
               next field txpp_id_no
            end if
         end if

      before field gv_oldID
         if (NOT verify_field("txpp_parc_id", update_flag)) then
            next field txpp_parc_id
         end if

      before field cust_acct
         if (NOT verify_field("gv_oldID", update_flag)) then
            next field gv_oldID
         end if
      after field cust_acct
         if ((addl_no is null) or
             (addl_no is not null and addl_no < 0)) then
            # Set the default delivery address number as the primary one and display the corresponding name and address info
            let addl_no = CustomerManager.getPrimaryDeliveryAddressSequence(cust_acct)
            if (ExceptionState.hasException()) then
               call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_DISPLAYDIALOG, sfmt("Error selecting the primary delivery address for acct %1. Line %2.", cust_acct, __LINE__))
               let addl_no = null
            end if
         end if

      before field addl_no
         if (NOT verify_field("cust_acct", update_flag)) then
            next field cust_acct
         end if
         if (addl_no is not null and addl_no >= 0) then
            call get_address(cust_acct, addl_no) returning cid_error, lv_customerDeliveryAddressStatusActive
            display by name cust_name, cust_name2, cust_addr, cust_addr2, cust_city, cust_state, cust_zip, cust_country
            if (lv_customerDeliveryAddressStatusActive) then
               call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", true)
            else
               call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", false)
            end if
         end if

      after field addl_no
         # Display the name and address info for the customer and delivery address
         call get_address(cust_acct, addl_no) returning cid_error, lv_customerDeliveryAddressStatusActive
         display by name cust_name, cust_name2, cust_addr, cust_addr2, cust_city, cust_state, cust_zip, cust_country
         if (lv_customerDeliveryAddressStatusActive) then
            call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", true)
         else
            call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", false)
         end if
         if (pass_let = "L" and pass_prop = "BLANK") then
            if (pass_bcid = cust_acct and pass_badr != addl_no) then
               let dlg_str = "Warning: This address number does not \n",
                             "match the business owner address number \n",
                             pass_badr using "<<<<&", "."
               let user_ansr = spaskrtn_dialog("Business Owner Address Number", dlg_str, "OK", "warning", "OK")
            end if
         end if

      before field txpp_dba
         if (NOT verify_field("gv_oldID", update_flag)) then
            next field gv_oldID
         end if
         if (not update_flag) then
            if (NOT verify_field("addl_no", update_flag)) then
               next field addl_no
            end if
         end if

      before field txpp_loc_no
         if (NOT verify_field("txpp_dba", update_flag)) then
            next field txpp_dba
         end if

      before field txpp_loc_no_suff
         if (NOT verify_field("txpp_loc_no", update_flag)) then
            next field txpp_loc_no
         end if

      before field street_cd
         if (NOT verify_field("txpp_loc_no_suff", update_flag)) then
            next field txpp_loc_no_suff
         end if

      before field txpp_loc_street
         if (ST_CLIENT_STATE = "CT") then
            if (NOT verify_field("street_cd", update_flag)) then
               next field street_cd
            end if
         else
            if (NOT verify_field("txpp_loc_no_suff", update_flag)) then
               next field txpp_loc_no_suff
            end if
         end if

      before field txpp_loc_apt
         if (NOT verify_field("txpp_loc_street", update_flag)) then
            next field txpp_loc_street
         end if

      before field txpp_loc_id
         if (NOT verify_field("txpp_dba", update_flag)) then
            next field txpp_dba
         end if

      before field txpp_status
         if (use_cen_addr = "N") then
            if (NOT verify_field("txpp_loc_apt", update_flag)) then
               next field txpp_loc_apt
            end if
         else
            if (NOT verify_field("txpp_loc_id", update_flag)) then
               next field txpp_loc_id
            end if
         end if
      on change txpp_status
         if (gr_txppmast.txpp_status != "E") then
            let gr_txppmast.txpp_full_exem_cd = " "
            let exem_desc = " "
            display by name exem_desc
            call Dialog.setFieldActive("txpp_full_exem_cd", FALSE)
         else
            call Dialog.setFieldActive("txpp_full_exem_cd", TRUE)
         end if
         if (gr_txppmast.txpp_status != "I") then
            let gr_txppmast.txpp_inact_date = NULL
            call Dialog.setFieldActive("txpp_inact_date", FALSE)
         else
            call Dialog.setFieldActive("txpp_inact_date", TRUE)
         end if

      # note that the next few fields have "repeated" verification to handle
      # the times when certain fields will be hidden
      before field txpp_inact_date
         if (NOT verify_field("txpp_status", update_flag)) then
            next field txpp_status
         end if

      before field txpp_full_exem_cd
         if (NOT verify_field("txpp_status", update_flag)) then
            next field txpp_status
         end if

      before field txpp_abst_type
         case
            when (gr_txppmast.txpp_status == "E")
               if (NOT verify_field("txpp_full_exem_cd", update_flag)) then
                  next field txpp_full_exem_cd
               end if
            when (gr_txppmast.txpp_status == "I")
               if (NOT verify_field("txpp_inact_date", update_flag)) then
                  next field txpp_inact_date
               end if
            otherwise
               if (NOT verify_field("txpp_status", update_flag)) then
                  next field txpp_status
               end if
         end case
         if (gr_txppmast.txpp_abst_type = "B" OR gr_txppmast.txpp_abst_type = "P") then
            call Dialog.setFieldActive("txpp_business", TRUE)
         else
            let gr_txppmast.txpp_business = " "
            display by name gr_txppmast.txpp_business
            call Dialog.setFieldActive("txpp_business", FALSE)
         end if

      on change txpp_abst_type
         if (gr_txppmast.txpp_abst_type = "B" OR gr_txppmast.txpp_abst_type = "P") then
            call Dialog.setFieldActive("txpp_business", TRUE)
         else
            let gr_txppmast.txpp_business = " "
            display by name gr_txppmast.txpp_business
            call Dialog.setFieldActive("txpp_business", FALSE)
         end if

      before field txpp_business
         if (NOT verify_field("txpp_abst_type", update_flag)) then
            next field txpp_abst_type
         end if

      before field txpp_juris_cd
         if (NOT verify_field("txpp_business", update_flag)) then
            next field txpp_business
         end if

      before field txpp_class_cd
         if (NOT verify_field("txpp_juris_cd", update_flag)) then
            next field txpp_juris_cd
         end if

      before field txpp_subdiv
         if (NOT verify_field("txpp_class_cd", update_flag)) then
            next field txpp_class_cd
         end if

      before field txpp_nature_own
         if (NOT verify_field("txpp_subdiv", update_flag)) then
            next field txpp_subdiv
         end if

      before field txpp_list_status
         if (NOT verify_field("txpp_nature_own", update_flag)) then
            next field txpp_nature_own
         end if

      before field txpp_pos_audit
         if (NOT verify_field("txpp_list_status", update_flag)) then
            next field txpp_list_status
         end if

      before field txpp_resident
         if (NOT verify_field("txpp_pos_audit", update_flag)) then
            next field txpp_pos_audit
         end if

      before field txpp_date_field
         if (NOT verify_field("txpp_resident", update_flag)) then
            next field txpp_resident
         end if
      after field txpp_date_field
         if (gr_txppmast.txpp_date_field IS NULL) then
            let gr_txppmast.txpp_who_field = " "
            display by name gr_txppmast.txpp_who_field
            call Dialog.setFieldActive("txpp_who_field", FALSE)
         else
            call Dialog.setFieldActive("txpp_who_field", TRUE)
         end if

      before field txpp_who_field
         if (NOT verify_field("txpp_date_field", update_flag)) then
            next field txpp_date_field
         end if

      before field txpp_date_desk
         if (gr_txppmast.txpp_date_field IS NOT NULL) then
            if (NOT verify_field("txpp_who_field", update_flag)) then
               next field txpp_who_field
            end if
         end if
      after field txpp_date_desk
         if (gr_txppmast.txpp_date_desk IS NULL) then
            let gr_txppmast.txpp_who_desk = " "
            display by name gr_txppmast.txpp_who_desk
            call Dialog.setFieldActive("txpp_who_desk", FALSE)
         else
            call Dialog.setFieldActive("txpp_who_desk", TRUE)
         end if

      before field txpp_who_desk
         if (NOT verify_field("txpp_date_desk", update_flag)) then
            next field txpp_date_desk
         end if

      before field txpp_comply
         if (NOT verify_field("txpp_who_desk", update_flag)) then
            next field txpp_who_desk
         end if

      on change txpp_comply
         if (gr_txppmast.txpp_comply = "N") then
            let gr_txppmast.txpp_fml_lst_yr = " "
            call Dialog.setFieldActive("txpp_fml_lst_yr", FALSE)
         else
            call Dialog.setFieldActive("txpp_fml_lst_yr", TRUE)
         end if

      before field txpp_fml_lst_yr
         if (NOT verify_field("txpp_comply", update_flag)) then
            next field txpp_comply
         end if

      before field txpp_audit
         if (gr_txppmast.txpp_comply != "N") then
            if (NOT verify_field("txpp_fml_lst_yr", update_flag)) then
               next field txpp_fml_lst_yr
            end if
         end if
      on change txpp_audit
         if (gr_txppmast.txpp_audit = "N") then
            let gr_txppmast.txpp_audit_yr = 0
            call Dialog.setFieldActive("txpp_audit_yr", FALSE)
         else
            call Dialog.setFieldActive("txpp_audit_yr", TRUE)
         end if

      before field txpp_audit_yr
         if (NOT verify_field("txpp_audit", update_flag)) then
            next field txpp_audit
         end if

      before field txpp_assmt_ly
         if (NOT verify_field("txpp_audit_yr", update_flag)) then
            next field txpp_audit_yr
         end if
         if (gr_txppmast.txpp_audit != "N") then
            if (NOT verify_field("txpp_audit_yr", update_flag)) then
               next field txpp_audit_yr
            end if
         end if

      before field txpp_formal_lst
         if (NOT verify_field("txpp_assmt_ly", update_flag)) then
            next field txpp_assmt_ly
         end if

      before field txpp_comply_no
         if (NOT verify_field("txpp_formal_lst", update_flag)) then
            next field txpp_formal_lst
         end if

      before field txpp_comply_date
         if (NOT verify_field("txpp_comply_no", update_flag)) then
            next field txpp_comply_no
         end if

      on action bte_getnext
         case
            when (infield(txpp_id_no))
               call get_next_ppid() returning gr_txppmast.txpp_id_no
               if gr_txppmast.txpp_id_no then
                  display by name gr_txppmast.txpp_id_no
               else
                  let int_flag = FALSE
                  exit input
               end if
         end case

      on action bte_help
         case
            when (infield(cust_acct))
               let lv_createOrUpdateRestrictions.Category = 25
               initialize lv_prepopulatedMatchingCriteria.* to null
               initialize lv_returnDeliveryAddress.* to null
               if (cust_mnt = "Y") then
                  call CustomerInterface.displaySearch("Find an Owner's Customer ID and Delivery Address", CustomerInterface.c_COMMANDSEARCH_ReturnsDeliveryAddresses + CustomerInterface.c_COMMANDSEARCH_AllowCreate + CustomerInterface.c_COMMANDSEARCH_PreventSelectionOfInaccessibleDepartment + CustomerInterface.c_COMMANDSEARCH_ModalDialog + CustomerInterface.c_COMMANDSEARCH_AdvancedSearch + CustomerInterface.c_COMMANDSEARCH_DisplayPostSelectionActions + CustomerInterface.c_COMMANDSEARCH_IncludeTaxFields, lv_createOrUpdateRestrictions.*, lv_prepopulatedMatchingCriteria.*, "search1")
                                             returning lv_returnAction, lv_customerNumber, lv_returnDeliveryAddress.*
               else
                  call CustomerInterface.displaySearch("Find an Owner's Customer ID and Delivery Address", CustomerInterface.c_COMMANDSEARCH_ReturnsDeliveryAddresses + CustomerInterface.c_COMMANDSEARCH_PreventSelectionOfInaccessibleDepartment + CustomerInterface.c_COMMANDSEARCH_ModalDialog + CustomerInterface.c_COMMANDSEARCH_AdvancedSearch + CustomerInterface.c_COMMANDSEARCH_IncludeTaxFields, lv_createOrUpdateRestrictions.*, lv_prepopulatedMatchingCriteria.*, "search1")
                                             returning lv_returnAction, lv_customerNumber, lv_returnDeliveryAddress.*
               end if
               if (ExceptionState.hasException()) then
                  call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_DISPLAYDIALOG, "Error occurred selecting the owner's customer record.")
               else
                  if (lv_returnAction == CustomerInterface.c_RETURNACTION_Selected) then
                     let cust_acct = lv_customerNumber
                     let addl_no = lv_returnDeliveryAddress.AddressNumber
                     display by name cust_acct, addl_no
                     call get_address(cust_acct, addl_no) returning cid_error, lv_customerDeliveryAddressStatusActive
                     display by name cust_name, cust_name2, cust_addr, cust_addr2, cust_city, cust_state, cust_zip, cust_country
                     if (lv_customerDeliveryAddressStatusActive) then
                        call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", true)
                     else
                        call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", false)
                     end if
                  end if
               end if

            when (infield(addl_no))
               let lv_createOrUpdateRestrictions.Category = 25
               let lv_prepopulatedMatchingCriteria.CustomerNumber = cust_acct
               initialize lv_returnDeliveryAddress.* to null
               call CustomerInterface.displaySearch("Find an Owner's Delivery Address", CustomerInterface.c_COMMANDSEARCH_ReturnsDeliveryAddresses + CustomerInterface.c_COMMANDSEARCH_AllowCreate + CustomerInterface.c_COMMANDSEARCH_PreventSelectionOfInaccessibleDepartment + CustomerInterface.c_COMMANDSEARCH_ModalDialog + CustomerInterface.c_COMMANDSEARCH_AutomaticallySearch, lv_createOrUpdateRestrictions.*, lv_prepopulatedMatchingCriteria.*, "search1")
                                          returning lv_returnAction, lv_customerNumber, lv_returnDeliveryAddress.*
               if (ExceptionState.hasException()) then
                  call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_WRITESYSTEMLOG + ExceptionHandler.c_DISPLAYDIALOG, "Error occurred selecting the owner's delivery address number." ||__LINE__)
               else
                  if (lv_returnAction == CustomerInterface.c_RETURNACTION_Selected) then
                     let cust_acct = lv_customerNumber
                     let addl_no = lv_returnDeliveryAddress.AddressNumber
                     display by name cust_acct, addl_no
                     call get_address(cust_acct, addl_no) returning cid_error, lv_customerDeliveryAddressStatusActive
                     display by name cust_name, cust_name2, cust_addr, cust_addr2, cust_city, cust_state, cust_zip, cust_country
                     if (lv_customerDeliveryAddressStatusActive) then
                        call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", true)
                     else
                        call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", false)
                     end if
                  end if
               end if

            when (infield(txpp_juris_cd))
               let select_line = "select txju_code, txju_desc from txjurisd "
               call spmuhelp_setColumnWidths(4,30,0,0,0,0,0,0,0,0)
               call spmuhelp(select_line, "Code|Description", " ", "Jurisdiction Code Help")
                             returning retcode_jur, retdesc_jur
               if (retcode_jur is NOT NULL) then
                  let gr_txppmast.txpp_juris_cd = retcode_jur
                  let jur_desc = retdesc_jur
               end if
               display by name jur_desc

            when (infield(txpp_class_cd))
               let select_line = "select txcl_class, txcl_desc from txclascd ",
                                 " where txcl_ar_cat  = 25 ",
                                 "   and txcl_lnpg_sw = 'G' "
               call spmuhelp_setColumnWidths(4,30,0,0,0,0,0,0,0,0)
               call spmuhelp(select_line, "Code|Description", " ", "Class Code Help")
                             returning retcode_cls, retdesc_cls
               if (retcode_cls is NOT NULL) then
                  let gr_txppmast.txpp_class_cd = retcode_cls
                  let class_desc = retdesc_cls
               end if
               display by name class_desc

            when (infield(txpp_subdiv))
               let select_line = "select txsd_code, txsd_desc from txsubdiv "
               call spmuhelp_setColumnWidths(6,30,0,0,0,0,0,0,0,0)
               call spmuhelp(select_line, "Code|Description", " ", "Subdivision Code Help")
                             returning retcode_sub, retdesc_sub
               if (retcode_sub is NOT NULL) then
                  let gr_txppmast.txpp_subdiv = retcode_sub
                  let sub_desc = retdesc_sub
               end if
               display by name sub_desc

            when (infield(txpp_nature_own))
               if (ST_CLIENT_STATE == "MO") then
                  let select_line = "select txno_code, txno_desc from txnatown "
                  call spmuhelp_setColumnWidths(4,40,0,0,0,0,0,0,0,0)
                  call spmuhelp(select_line, "Code|Name", " ", "County Code Help")
                                returning retcode_own, retdesc_own
                  if (retcode_own is NOT NULL) then
                     let gr_txppmast.txpp_nature_own = retcode_own
                     let lender_name = retdesc_own
                  end if
               else
                  let select_line = "select arlr_code, arlr_name from arlender "
                  call spmuhelp_setColumnWidths(4,40,0,0,0,0,0,0,0,0)
                  call spmuhelp(select_line, "Code|Name", " ", "Lender Code Help")
                                returning retcode_lnd, retdesc_lnd
                  if (retcode_lnd is NOT NULL) then
                     let gr_txppmast.txpp_nature_own = retcode_lnd
                     let lender_name = retdesc_lnd
                  end if
               end if
               display by name lender_name

            when (infield(street_cd))
               let select_line = "select txst_code, txst_name from txstreet "
               call spmuhelp_setColumnWidths(4,25,0,0,0,0,0,0,0,0)
               call spmuhelp(select_line, "Code|Name", " ", "Street Code Help")
                             returning retcode_st, retdesc_st
               if (retcode_st is NOT NULL) then
                  let street_cd = retcode_st
                  let gr_txppmast.txpp_loc_street = retdesc_st
               end if

            when (infield(txpp_full_exem_cd))
               let select_line = "select ared_code, ared_desc from arexdisc ",
                                 " where ared_total = 'Y' "
               call spmuhelp_setColumnWidths(6,30,0,0,0,0,0,0,0,0)
               call spmuhelp(select_line, "Code|Description", " ", "Exemption Code Help")
                             returning retcode_ex, retdesc_ex
               if (retcode_ex is NOT NULL) then
                  let gr_txppmast.txpp_full_exem_cd = retcode_ex
                  let exem_desc = retdesc_ex
               end if
               display by name exem_desc

            when (infield(txpp_loc_id))
               if (use_cen_addr = "Y") then
                  let hold_loc_id = gr_txppmast.txpp_loc_id
                  call cploclib_locHelp(" ",
                                        "N",
                                        " ",
                                        0,
                                        " ",
                                        " ",
                                        " ",
                                        " ",
                                        " ",
                                        " ",
                                        " ",
                                        " ")
                            returning gr_txppmast.txpp_loc_id,
                                      gr_txppmast.txpp_loc_seq,
                                      tmp_str,
                                      tmp_num,
                                      tmp_str,
                                      tmp_str,
                                      tmp_str,
                                      tmp_str,
                                      tmp_str,
                                      tmp_str,
                                      tmp_str,
                                      tmp_str

                  if (length(gr_txppmast.txpp_loc_id) = 0) then
                     let gr_txppmast.txpp_loc_id = hold_loc_id
                  end if

                  display by name gr_txppmast.txpp_loc_id

                  let loc_seq = "Seq: ",
                                gr_txppmast.txpp_loc_seq using "<<<<&"
                  display by name loc_seq

                  if (gr_txppmast.txpp_loc_id IS NOT NULL and
                      gr_txppmast.txpp_loc_id != " ") then
                      call cploclib_formatShortLoc(gr_txppmast.txpp_loc_id,
                                                   gr_txppmast.txpp_loc_seq,
                                                   " ",
                                                   0,
                                                   " ",
                                                   " ",
                                                   " ",
                                                   " ",
                                                   " ",
                                                   " ",
                                                   " ",
                                                   " ")
                                    returning tmp_loc_desc,
                                              tmp_loc_city_st_zip,
                                              retval,
                                              err_msg
                     if (retval) then
                        let format_loc = tmp_loc_desc clipped, "\n",
                                         tmp_loc_city_st_zip
                        display by name format_loc
                     end if

                     if (retval < 0) then
                        let format_loc = " "
                        display by name format_loc
                        call sperrorm(err_msg, 0, 1)
                        next field txpp_loc_id
                     end if
                  else
                     let format_loc = " "
                     display by name format_loc
                  end if

               else
                  call spnohelp()
               end if

         end case

      on action cancel
         if (spaskrtn_interrupt("")) then
            let int_flag = TRUE
            exit input
         else
            let int_flag = FALSE
            continue input
         end if

      on action close
         let int_flag = FALSE
         if (spaskrtn_closeWindow(DIALOG)) then
            let int_flag   = TRUE
            let WHDL_CLOSE = TRUE
            exit input
         else
            continue input
         end if

      after input
         if (NOT update_flag) then
            if (NOT verify_field("txpp_id_no", update_flag)) then
               next field txpp_id_no
            end if
         end if
         if (NOT verify_field("txpp_dba", update_flag)) then
            next field txpp_dba
         end if
         if (NOT verify_field("txpp_parc_id", update_flag)) then
            next field txpp_parc_id
         end if
         if (NOT verify_field("gv_oldID", update_flag)) then
            next field gv_oldID
         end if
         if (NOT update_flag) then
            if (NOT verify_field("cust_acct", update_flag)) then
               next field cust_acct
            end if
            if (NOT verify_field("addl_no", update_flag)) then
               next field addl_no
            end if
         end if
         if (use_cen_addr = "N") then
            if (NOT verify_field("txpp_loc_no", update_flag)) then
               next field txpp_loc_no
            end if
            if (NOT verify_field("txpp_loc_no_suff", update_flag)) then
               next field txpp_loc_no_suff
            end if
            if (NOT verify_field("street_cd", update_flag)) then
               next field street_cd
            end if
            if (NOT verify_field("txpp_loc_street", update_flag)) then
               next field txpp_loc_street
            end if
            if (NOT verify_field("txpp_loc_apt", update_flag)) then
               next field txpp_loc_apt
            end if
         else
            if (NOT verify_field("txpp_loc_id", update_flag)) then
               next field txpp_loc_id
            end if
         end if
         if (NOT verify_field("txpp_status", update_flag)) then
            next field txpp_status
         end if
         if (gr_txppmast.txpp_status = "I") then
            if(NOT verify_field("txpp_inact_date", update_flag)) then
               next field txpp_inact_date
            end if
         end if
         if (NOT verify_field("txpp_abst_type", update_flag)) then
            next field txpp_abst_type
         end if
         if(gr_txppmast.txpp_abst_type = "B" OR gr_txppmast.txpp_abst_type = "P") then
            if (NOT verify_field("txpp_business", update_flag)) then
               next field txpp_business
            end if
         end if
         if (gr_txppmast.txpp_status = "E") then
            if (NOT verify_field("txpp_full_exem_cd", update_flag)) then
               next field txpp_full_exem_cd
            end if
         end if
         if (NOT verify_field("txpp_list_status", update_flag)) then
            next field txpp_list_status
         end if
         if (NOT verify_field("txpp_juris_cd", update_flag)) then
            next field txpp_juris_cd
         end if
         if (NOT verify_field("txpp_class_cd", update_flag)) then
            next field txpp_class_cd
         end if
         if (NOT verify_field("txpp_subdiv", update_flag)) then
            next field txpp_subdiv
         end if
         if (NOT verify_field("txpp_nature_own", update_flag)) then
            next field txpp_nature_own
         end if
         if (NOT verify_field("txpp_pos_audit", update_flag)) then
            next field txpp_pos_audit
         end if
         if (NOT verify_field("txpp_resident", update_flag)) then
            next field txpp_resident
         end if
         if (NOT verify_field("txpp_date_field", update_flag)) then
            next field txpp_date_field
         end if
         if (gr_txppmast.txpp_date_field IS NOT NULL) then
            if (NOT verify_field("txpp_who_field", update_flag)) then
               next field txpp_who_field
            end if
         end if
         if (NOT verify_field("txpp_date_desk", update_flag)) then
            next field txpp_date_desk
         end if
         if (gr_txppmast.txpp_date_desk IS NOT NULL) then
            if (NOT verify_field("txpp_who_desk", update_flag)) then
               next field txpp_who_desk
            end if
         end if
         if (ST_CLIENT_STATE != "NC" and
             ST_CLIENT_STATE != "TN") then
            if (NOT verify_field("txpp_comply", update_flag)) then
               next field txpp_comply
            end if
            if (gr_txppmast.txpp_comply != "N") then
               if (NOT verify_field("txpp_fml_lst_yr", update_flag)) then
                  next field txpp_fml_lst_yr
               end if
            end if
            if (NOT verify_field("txpp_audit", update_flag)) then
               next field txpp_audit
            end if
            if (gr_txppmast.txpp_audit != "N") then
               if (NOT verify_field("txpp_audit_yr", update_flag)) then
                  next field txpp_audit_yr
               end if
            end if
         end if
         if (NOT verify_field("txpp_assmt_ly", update_flag)) then
            next field txpp_assmt_ly
         end if
         if (NOT verify_field("txpp_formal_lst", update_flag)) then
            next field txpp_formal_lst
         end if
         if (NOT verify_field("txpp_comply_no", update_flag)) then
            next field txpp_comply_no
         end if
         if (NOT verify_field("txpp_comply_date", update_flag)) then
            next field txpp_comply_date
         end if

      &include "StandardEntryDialogActions.inc"

   end input

end function
#############################################################################
function va_input_routine(update_flag)
   define lv_ssnOrFid char(14),
          lv_answer,
          lv_message,
          tmp_loc_city_st_zip,
          tmp_loc_desc,
          tmp_str string,
          lv_isPerson boolean,
          retval smallint,
          lv_customerNumber,
          lv_returnAction,
          tmp_num,
          update_flag integer,
          retcode_jur like txjurisd.txju_code,
          retdesc_jur like txjurisd.txju_desc,
          retcode_cls like txclascd.txcl_class,
          retdesc_cls like txclascd.txcl_desc,
          retcode_sub like txsubdiv.txsd_code,
          retdesc_sub like txsubdiv.txsd_desc,
          retcode_ex like arexdisc.ared_code,
          retdesc_ex like arexdisc.ared_desc,
          retcode_lnd like arlender.arlr_code,
          retdesc_lnd like arlender.arlr_name,
          hold_loc_id like txppmast.txpp_loc_id,
          lv_customerDeliveryAddressStatusActive boolean,
          lv_returnDeliveryAddress CustomerManager.DeliveryAddress,
          lv_createOrUpdateRestrictions CustomerManager.CreateOrUpdateRestrictions,
          lv_prepopulatedMatchingCriteria CustomerManager.MatchingCriteria

   # Initialize global variable(s)...
   let int_flag = FALSE
   let WHDL_CLOSE = FALSE

   # Initialize local variable(s)...
   let lv_answer = " "
   let lv_isPerson = TRUE
   let lv_message = " "
   let lv_ssnOrFid = " "

   options input no wrap

   input by name gr_txppmast.txpp_id_no,
                 gr_txppmast.txpp_parc_id,
                 cust_acct,
                 addl_no,
                 gv_customerSSN,
                 gr_txppmast.txpp_dba,
                 gr_txppmast.txpp_loc_no,
                 gr_txppmast.txpp_loc_no_suff,
                 gr_txppmast.txpp_loc_street,
                 gr_txppmast.txpp_loc_apt,
                 gr_txppmast.txpp_loc_id,
                 gr_txppmast.txpp_status,
                 gr_txppmast.txpp_inact_date,
                 gr_txppmast.txpp_full_exem_cd,
                 gr_txppmast.txpp_abst_type,
                 gr_txppmast.txpp_business,
                 gr_txppmast.txpp_juris_cd,
                 gr_txppmast.txpp_class_cd,
                 gr_txppmast.txpp_subdiv,
                 gr_txppmast.txpp_nature_own,
                 gr_txppmast.txpp_formal_lst,
                 gr_txppmast.txpp_comply_date,
                 gr_txppmast.txpp_resident,
                 gr_txppmast.txpp_comply,
                 gr_txppmast.txpp_fml_lst_yr,
                 gr_txppmast.txpp_pos_audit, 
                 gr_txppmast.txpp_audit,
                 gr_txppmast.txpp_audit_yr,
                 gr_txppmast.txpp_date_field,
                 gr_txppmast.txpp_who_field,
                 gr_txppmast.txpp_date_desk,
                 gr_txppmast.txpp_who_desk
                 without defaults attribute(unbuffered)

      before input
         if (update_flag) then
            call Dialog.setFieldActive("txpp_id_no", FALSE)
            call Dialog.setFieldActive("cust_acct", FALSE)
            call Dialog.setFieldActive("addl_no", FALSE)
            call Dialog.setFieldActive("gv_customerSSN", FALSE)
            call Dialog.setFieldActive("txpp_date_field", TRUE)
         end if
         if (gr_txppmast.txpp_status != "E") then
            let gr_txppmast.txpp_full_exem_cd = " "
            let exem_desc = " "
            display by name exem_desc
            call Dialog.setFieldActive("txpp_full_exem_cd", FALSE)
         end if
         if (gr_txppmast.txpp_status != "I") then
            let gr_txppmast.txpp_inact_date = null
            call Dialog.setFieldActive("txpp_inact_date", FALSE)
         end if
         if (gr_txppmast.txpp_comply = "N") then
            let gr_txppmast.txpp_fml_lst_yr = " "
            call Dialog.setFieldActive("txpp_fml_lst_yr", FALSE)
         end if
         call checkEntryPermissions()

      before field txpp_parc_id
         if (NOT update_flag) then
            if (NOT verify_field("txpp_id_no", update_flag)) then
               next field txpp_id_no
            end if
         end if

      before field cust_acct
         if (NOT verify_field("txpp_parc_id", update_flag)) then
            next field txpp_parc_id
         end if

      before field addl_no
         if (NOT verify_field("cust_acct", update_flag)) then
            next field cust_acct
         end if
         if ((addl_no is null) or
             (addl_no is not null and addl_no < 0)) then
            # Set the default delivery address number as the primary one and display the corresponding name and address info
            let addl_no = CustomerManager.getPrimaryDeliveryAddressSequence(cust_acct)
            if (ExceptionState.hasException()) then
               call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_DISPLAYDIALOG, sfmt("Error selecting the primary delivery address for acct %1. Line %2.", cust_acct, __LINE__))
               let addl_no = null
            end if
         end if
         if (addl_no is not null and addl_no >= 0) then
            call get_address(cust_acct, addl_no) returning cid_error, lv_customerDeliveryAddressStatusActive
            display by name cust_name, cust_name2, cust_addr, cust_addr2, cust_city, cust_state, cust_zip, cust_country
            if (lv_customerDeliveryAddressStatusActive) then
               call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", true)
            else
               call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", false)
            end if
         end if

      after field addl_no
         # Display the name and address info for the customer and delivery address
         call get_address(cust_acct, addl_no) returning cid_error, lv_customerDeliveryAddressStatusActive
         display by name cust_name, cust_name2, cust_addr, cust_addr2, cust_city, cust_state, cust_zip, cust_country
         if (lv_customerDeliveryAddressStatusActive) then
            call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", true)
         else
            call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", false)
         end if

      before field gv_customerSSN
         if (not update_flag) then
            if (NOT verify_field("addl_no", update_flag)) then
               next field addl_no
            end if
         end if

      after field gv_customerSSN
         if (update_flag) then
            if (hold_customerSSN == gv_customerSSN) then
               # Obtain personal data...
               call CustomerManager.getDeliveryAddressSSNorFID(cust_acct, addl_no, 1) returning lv_ssnOrFid, lv_isPerson
               if (ExceptionState.hasException()) then
                  if (NOT ExceptionState.hasNotFoundSQLException()) then
                     call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_WRITESYSTEMLOG + ExceptionHandler.c_DISPLAYERRORLINE, "Error calling getDeliveryAddressSSNorFID() for Customer ID: "||cust_acct||" and delivery addr: "||addl_no||". Line: "||__LINE__)
                  end if
               end if
               # Defensive programming...
               if (lv_ssnOrFid is NULL) then
                  let lv_ssnOrFid = " "
               end if

               # Check to see if the SSN listed on the property differs from the SSN of the primary owner's customer record...
               if (gv_customerSSN != lv_ssnOrFid) then
                  let lv_message = "The SSN/FID (" || gv_customerSSN clipped || ") listed on the property does not match\n",
                                   "the SSN/FID (" || lv_ssnOrFid clipped || ") of the primary owner's customer record.\n\n",
                                   "Would you like to update the property to match the customer?"
                  let lv_answer = spaskrtn_dialog("Customer SSN/FID", lv_message, "Yes|No", "question", "Yes")
                  if (lv_answer == "Yes") then
                     let gv_customerSSN = lv_ssnOrFid
                  end if
               end if
            end if
            let gr_txppmast.txpp_old_id = gv_customerSSN
         end if

      before field txpp_dba
         if (NOT verify_field("gv_customerSSN", update_flag)) then
            next field gv_customerSSN
         end if

      before field txpp_loc_no
         if (NOT verify_field("txpp_dba", update_flag)) then
            next field txpp_dba
         end if

      before field txpp_loc_no_suff
         if (NOT verify_field("txpp_loc_no", update_flag)) then
            next field txpp_loc_no
         end if

      before field txpp_loc_street
         if (NOT verify_field("txpp_loc_no_suff", update_flag)) then
            next field txpp_loc_no_suff
         end if

      before field txpp_loc_apt
         if (NOT verify_field("txpp_loc_street", update_flag)) then
            next field txpp_loc_street
         end if

      before field txpp_loc_id
         if (NOT verify_field("txpp_dba", update_flag)) then
            next field txpp_dba
         end if

      before field txpp_status
         if (use_cen_addr = "N") then
            if (NOT verify_field("txpp_loc_apt", update_flag)) then
               next field txpp_loc_apt
            end if
         else
            if (NOT verify_field("txpp_loc_id", update_flag)) then
               next field txpp_loc_id
            end if
         end if

      on change txpp_status
         if (gr_txppmast.txpp_status != "E") then
            let gr_txppmast.txpp_full_exem_cd = " "
            let exem_desc = " "
            display by name exem_desc
            call Dialog.setFieldActive("txpp_full_exem_cd", FALSE)
         else
            call Dialog.setFieldActive("txpp_full_exem_cd", TRUE)
         end if
         if (gr_txppmast.txpp_status != "I") then
            let gr_txppmast.txpp_inact_date = NULL
            call Dialog.setFieldActive("txpp_inact_date", FALSE)
         else
            call Dialog.setFieldActive("txpp_inact_date", TRUE)
         end if
         if (prev_status = "I") then
            if (gr_txppmast.txpp_status = "A" and
                gr_txppmast.txpp_formal_lst = "N") then
               let dlg_str = "Do you want the Print Return flag to be \n",
                             "set to 'Y'?"
               let user_ansr = spaskrtn_dialog("Print Returns",
                                               dlg_str,
                                               "Yes|No",
                                               "question",
                                               "Yes")
               if (user_ansr = "Yes") then
                  let gr_txppmast.txpp_formal_lst = "Y"
                  display by name gr_txppmast.txpp_formal_lst
               end if
               let prev_status = " "
            end if
         end if

      # note that some of the fields have "repeated" verification logic
      # in order to account for fields that may be hidden or set no entry
      before field txpp_inact_date
         if (NOT verify_field("txpp_status", update_flag)) then
            next field txpp_status
         end if

      before field txpp_full_exem_cd
         if (NOT verify_field("txpp_status", update_flag)) then
            next field txpp_status
         end if
         if (gr_txppmast.txpp_status = "I") then
            if (NOT verify_field("txpp_inact_date", update_flag)) then
               next field txpp_inact_date
            end if
         end if

      before field txpp_abst_type
         case
            when (gr_txppmast.txpp_status == "E")
               if (NOT verify_field("txpp_full_exem_cd", update_flag)) then
                  next field txpp_full_exem_cd
               end if
            when (gr_txppmast.txpp_status == "I")
               if (NOT verify_field("txpp_inact_date", update_flag)) then
                  next field txpp_inact_date
               end if
            otherwise
               if (NOT verify_field("txpp_status", update_flag)) then
                  next field txpp_status
               end if
         end case
         if (gr_txppmast.txpp_abst_type = "B" OR gr_txppmast.txpp_abst_type = "P") then
            call Dialog.setFieldActive("txpp_business", TRUE)
         else
            let gr_txppmast.txpp_business = " "
            display by name gr_txppmast.txpp_business
            call Dialog.setFieldActive("txpp_business", FALSE)
         end if
      on change txpp_abst_type
         if (gr_txppmast.txpp_abst_type = "B" OR gr_txppmast.txpp_abst_type = "P") then
            call Dialog.setFieldActive("txpp_business", TRUE)
         else
            let gr_txppmast.txpp_business = " "
            display by name gr_txppmast.txpp_business
            call Dialog.setFieldActive("txpp_business", FALSE)
         end if

      before field txpp_business
         if (NOT verify_field("txpp_abst_type", update_flag)) then
            next field txpp_abst_type
         end if

      before field txpp_juris_cd
         if (gr_txppmast.txpp_business == " ") then
            if (NOT verify_field("txpp_abst_type", update_flag)) then
               next field txpp_abst_type
            end if
         end if
         if (NOT verify_field("txpp_business", update_flag)) then
            next field txpp_business
         end if

      before field txpp_class_cd
         if (NOT verify_field("txpp_juris_cd", update_flag)) then
            next field txpp_juris_cd
         end if

      before field txpp_subdiv
         if (NOT verify_field("txpp_class_cd", update_flag)) then
            next field txpp_class_cd
         end if

      before field txpp_nature_own
         if (NOT verify_field("txpp_subdiv", update_flag)) then
            next field txpp_subdiv
         end if

      before field txpp_formal_lst
         if (NOT verify_field("txpp_nature_own", update_flag)) then
            next field txpp_nature_own
         end if

      before field txpp_comply_date
         if (NOT verify_field("txpp_formal_lst", update_flag)) then
            next field txpp_formal_lst
         end if
         
      before field txpp_resident
         if (NOT verify_field("txpp_comply_date", update_flag)) then
            next field txpp_comply_date
         end if

      on change txpp_comply
         if (gr_txppmast.txpp_comply = "N") then
            let gr_txppmast.txpp_fml_lst_yr = " "
            call Dialog.setFieldActive("txpp_fml_lst_yr", FALSE)
         else
            call Dialog.setFieldActive("txpp_fml_lst_yr", TRUE)
         end if

      before field txpp_fml_lst_yr
         if (NOT verify_field("txpp_comply", update_flag)) then
            next field txpp_comply
         end if
      
      before field txpp_pos_audit
         if (gr_txppmast.txpp_comply = "N") then
            if (not verify_field("txpp_resident", update_flag)) then
               next field txpp_resident
            end if
         else
            if (NOT verify_field("txpp_fml_lst_yr", update_flag)) then
               next field txpp_fml_lst_yr
            end if
         end if

      before field txpp_audit_yr
         if (NOT verify_field("txpp_pos_audit", update_flag)) then
            next field txpp_pos_audit
         end if

      before field txpp_date_field
         call Dialog.setFieldActive("txpp_who_field", TRUE)
         if (NOT verify_field("txpp_resident", update_flag)) then
            next field txpp_audit_yr
         end if
      after field txpp_date_field
         if (gr_txppmast.txpp_date_field IS NULL) then
            let gr_txppmast.txpp_who_field = " "
            display by name gr_txppmast.txpp_who_field
            call Dialog.setFieldActive("txpp_who_field", FALSE)
         else
            call Dialog.setFieldActive("txpp_who_field", TRUE)
         end if

      before field txpp_who_field
         if (NOT verify_field("txpp_date_field", update_flag)) then
            next field txpp_date_field
         end if

      before field txpp_date_desk
         if (gr_txppmast.txpp_date_field IS NOT NULL) then
            if (NOT verify_field("txpp_who_field", update_flag)) then
               next field txpp_who_field
            end if
         end if
      after field txpp_date_desk
         if (gr_txppmast.txpp_date_desk IS NULL) then
            let gr_txppmast.txpp_who_desk = " "
            display by name gr_txppmast.txpp_who_desk
            call Dialog.setFieldActive("txpp_who_desk", FALSE)
         else
            call Dialog.setFieldActive("txpp_who_desk", TRUE)
         end if

      before field txpp_who_desk
         if (NOT verify_field("txpp_date_desk", update_flag)) then
            next field txpp_date_desk
         end if

      on action bte_getnext
         case
            when (infield(txpp_id_no))
               call get_next_ppid() returning gr_txppmast.txpp_id_no
               if gr_txppmast.txpp_id_no then
                  display by name gr_txppmast.txpp_id_no
               else
                  let int_flag = FALSE
                  exit input
               end if
         end case

      on action bte_help
         case
            when (infield(cust_acct))
               let lv_createOrUpdateRestrictions.Category = 25
               initialize lv_prepopulatedMatchingCriteria.* to null
               initialize lv_returnDeliveryAddress.* to null
               if (cust_mnt = "Y") then
                  call CustomerInterface.displaySearch("Find an Owner's Customer ID and Delivery Address", CustomerInterface.c_COMMANDSEARCH_ReturnsDeliveryAddresses + CustomerInterface.c_COMMANDSEARCH_AllowCreate + CustomerInterface.c_COMMANDSEARCH_PreventSelectionOfInaccessibleDepartment + CustomerInterface.c_COMMANDSEARCH_ModalDialog + CustomerInterface.c_COMMANDSEARCH_AdvancedSearch + CustomerInterface.c_COMMANDSEARCH_DisplayPostSelectionActions + CustomerInterface.c_COMMANDSEARCH_IncludeTaxFields, lv_createOrUpdateRestrictions.*, lv_prepopulatedMatchingCriteria.*, "search1")
                                             returning lv_returnAction, lv_customerNumber, lv_returnDeliveryAddress.*
               else
                  call CustomerInterface.displaySearch("Find an Owner's Customer ID and Delivery Address", CustomerInterface.c_COMMANDSEARCH_ReturnsDeliveryAddresses + CustomerInterface.c_COMMANDSEARCH_PreventSelectionOfInaccessibleDepartment + CustomerInterface.c_COMMANDSEARCH_ModalDialog + CustomerInterface.c_COMMANDSEARCH_AdvancedSearch + CustomerInterface.c_COMMANDSEARCH_IncludeTaxFields, lv_createOrUpdateRestrictions.*, lv_prepopulatedMatchingCriteria.*, "search1")
                                             returning lv_returnAction, lv_customerNumber, lv_returnDeliveryAddress.*
               end if
               if (ExceptionState.hasException()) then
                  call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_DISPLAYDIALOG, "Error occurred selecting the owner's customer record.")
               else
                  if (lv_returnAction == CustomerInterface.c_RETURNACTION_Selected) then
                     let cust_acct = lv_customerNumber
                     let addl_no = lv_returnDeliveryAddress.AddressNumber
                     display by name cust_acct, addl_no
                     call get_address(cust_acct, addl_no) returning cid_error, lv_customerDeliveryAddressStatusActive
                     display by name cust_name, cust_name2, cust_addr, cust_addr2, cust_city, cust_state, cust_zip, cust_country
                     if (lv_customerDeliveryAddressStatusActive) then
                        call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", true)
                     else
                        call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", false)
                     end if
                  end if
               end if

            when (infield(addl_no))
               let lv_createOrUpdateRestrictions.Category = 25
               let lv_prepopulatedMatchingCriteria.CustomerNumber = cust_acct
               initialize lv_returnDeliveryAddress.* to null
               call CustomerInterface.displaySearch("Find an Owner's Delivery Address", CustomerInterface.c_COMMANDSEARCH_ReturnsDeliveryAddresses + CustomerInterface.c_COMMANDSEARCH_AllowCreate + CustomerInterface.c_COMMANDSEARCH_PreventSelectionOfInaccessibleDepartment + CustomerInterface.c_COMMANDSEARCH_ModalDialog + CustomerInterface.c_COMMANDSEARCH_AutomaticallySearch, lv_createOrUpdateRestrictions.*, lv_prepopulatedMatchingCriteria.*, "search1")
                                          returning lv_returnAction, lv_customerNumber, lv_returnDeliveryAddress.*
               if (ExceptionState.hasException()) then
                  call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_WRITESYSTEMLOG + ExceptionHandler.c_DISPLAYDIALOG, "Error occurred selecting the owner's delivery address number." ||__LINE__)
               else
                  if (lv_returnAction == CustomerInterface.c_RETURNACTION_Selected) then
                     let cust_acct = lv_customerNumber
                     let addl_no = lv_returnDeliveryAddress.AddressNumber
                     display by name cust_acct, addl_no
                     call get_address(cust_acct, addl_no) returning cid_error, lv_customerDeliveryAddressStatusActive
                     display by name cust_name, cust_name2, cust_addr, cust_addr2, cust_city, cust_state, cust_zip, cust_country
                     if (lv_customerDeliveryAddressStatusActive) then
                        call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", true)
                     else
                        call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", false)
                     end if
                  end if
               end if

            when (infield(txpp_juris_cd))
               let select_line = "select txju_code, txju_desc from txjurisd "
               call spmuhelp_setColumnWidths(4,30,0,0,0,0,0,0,0,0)
               call spmuhelp(select_line, "Code|Description", " ", "Jurisdiction Code Help")
                             returning retcode_jur, retdesc_jur
               if (retcode_jur is NOT NULL) then
                  let gr_txppmast.txpp_juris_cd = retcode_jur
                  let jur_desc = retdesc_jur
               end if
               display by name jur_desc

            when (infield(txpp_class_cd))
               let select_line = "select txcl_class, txcl_desc from txclascd ",
                                 " where txcl_ar_cat  = 25 ",
                                 "   and txcl_lnpg_sw = 'G' "
               call spmuhelp_setColumnWidths(4,30,0,0,0,0,0,0,0,0)
               call spmuhelp(select_line, "Code|Description", " ", "Class Code Help")
                             returning retcode_cls, retdesc_cls
               if (retcode_cls is NOT NULL) then
                  let gr_txppmast.txpp_class_cd = retcode_cls
                  let class_desc = retdesc_cls
               end if
               display by name class_desc

            when (infield(txpp_subdiv))
               let select_line = "select txsd_code, txsd_desc from txsubdiv "
               call spmuhelp_setColumnWidths(6,30,0,0,0,0,0,0,0,0)
               call spmuhelp(select_line, "Code|Description", " ", "Subdivision Code Help")
                             returning retcode_sub, retdesc_sub
               if (retcode_sub is NOT NULL) then
                  let gr_txppmast.txpp_subdiv = retcode_sub
                  let sub_desc = retdesc_sub
               end if
               display by name sub_desc

            when (infield(txpp_full_exem_cd))
               let select_line = "select ared_code, ared_desc from arexdisc ",
                                 " where ared_total = 'Y' "
               call spmuhelp_setColumnWidths(6,30,0,0,0,0,0,0,0,0)
               call spmuhelp(select_line, "Code|Description", " ", "Exemption Code Help")
                             returning retcode_ex, retdesc_ex
               if (retcode_ex is NOT NULL) then
                  let gr_txppmast.txpp_full_exem_cd = retcode_ex
                  let exem_desc = retdesc_ex
               end if
               display by name exem_desc

            when (infield(txpp_nature_own))
               let select_line = "select arlr_code, arlr_name from arlender "
               call spmuhelp_setColumnWidths(4,40,0,0,0,0,0,0,0,0)
               call spmuhelp(select_line, "Code|Name", " ", "Lender Code Help")
                             returning retcode_lnd, retdesc_lnd
               if (retcode_lnd is NOT NULL) then
                  let gr_txppmast.txpp_nature_own = retcode_lnd
                  let lender_name = retdesc_lnd
               end if
               display by name lender_name

            when (infield(txpp_loc_id))
               if (use_cen_addr = "Y") then
                  let hold_loc_id = gr_txppmast.txpp_loc_id
                  call cploclib_locHelp(" ",
                                        "N",
                                        " ",
                                        0,
                                        " ",
                                        " ",
                                        " ",
                                        " ",
                                        " ",
                                        " ",
                                        " ",
                                        " ")
                            returning gr_txppmast.txpp_loc_id,
                                      gr_txppmast.txpp_loc_seq,
                                      tmp_str,
                                      tmp_num,
                                      tmp_str,
                                      tmp_str,
                                      tmp_str,
                                      tmp_str,
                                      tmp_str,
                                      tmp_str,
                                      tmp_str,
                                      tmp_str

                  if (length(gr_txppmast.txpp_loc_id) = 0) then
                     let gr_txppmast.txpp_loc_id = hold_loc_id
                  end if

                  display by name gr_txppmast.txpp_loc_id

                  let loc_seq = "Seq: ",
                                gr_txppmast.txpp_loc_seq using "<<<<&"
                  display by name loc_seq

                  if (gr_txppmast.txpp_loc_id IS NOT NULL and
                      gr_txppmast.txpp_loc_id != " ") then
                      call cploclib_formatShortLoc(gr_txppmast.txpp_loc_id,
                                                   gr_txppmast.txpp_loc_seq,
                                                   " ",
                                                   0,
                                                   " ",
                                                   " ",
                                                   " ",
                                                   " ",
                                                   " ",
                                                   " ",
                                                   " ",
                                                   " ")
                                    returning tmp_loc_desc,
                                              tmp_loc_city_st_zip,
                                              retval,
                                              err_msg
                     if (retval) then
                        let format_loc = tmp_loc_desc clipped, "\n",
                                         tmp_loc_city_st_zip
                        display by name format_loc
                     end if

                     if (retval < 0) then
                        let format_loc = " "
                        display by name format_loc
                        call sperrorm(err_msg, 0, 1)
                        next field txpp_loc_id
                     end if
                  else
                     let format_loc = " "
                     display by name format_loc
                  end if

               else
                  call spnohelp()
               end if

         end case

      on action cancel
         if (spaskrtn_interrupt("")) then
            let int_flag = TRUE
            exit input
         else
            let int_flag = FALSE
            continue input
         end if

      on action close
         let int_flag = FALSE
         if (spaskrtn_closeWindow(DIALOG)) then
            let int_flag   = TRUE
            let WHDL_CLOSE = TRUE
            exit input
         else
            continue input
         end if

      after input
         if (NOT update_flag) then
             if (NOT verify_field("txpp_id_no", update_flag)) then
                next field txpp_id_no
             end if
         end if
         if (NOT verify_field("txpp_dba", update_flag)) then
            next field txpp_dba
         end if
         if (NOT verify_field("txpp_parc_id", update_flag)) then
            next field txpp_parc_id
         end if
         if (NOT update_flag) then
            if (NOT verify_field("cust_acct", update_flag)) then
               next field cust_acct
            end if
            if (NOT verify_field("addl_no", update_flag)) then
               next field addl_no
            end if
            if (NOT verify_field("gv_customerSSN", update_flag)) then
               next field gv_customerSSN
            end if
         end if
         if (use_cen_addr = "N") then
            if (NOT verify_field("txpp_loc_no", update_flag)) then
               next field txpp_loc_no
            end if
            if (NOT verify_field("txpp_loc_no_suff", update_flag)) then
               next field txpp_loc_no_suff
            end if
            if (NOT verify_field("txpp_loc_street", update_flag)) then
               next field txpp_loc_street
            end if
            if (NOT verify_field("txpp_loc_apt", update_flag)) then
               next field txpp_loc_apt
            end if
         else
            if (NOT verify_field("txpp_loc_id", update_flag)) then
               next field txpp_loc_id
            end if
         end if
         if (NOT verify_field("txpp_status", update_flag)) then
            next field txpp_status
         end if
         if (gr_txppmast.txpp_status = "I") then
            if (NOT verify_field("txpp_inact_date", update_flag)) then
               next field txpp_inact_date
            end if
         end if
         if (NOT verify_field("txpp_abst_type", update_flag)) then
            next field txpp_abst_type
         end if
         if (NOT verify_field("txpp_business", update_flag)) then
            next field txpp_business
         end if
         if (gr_txppmast.txpp_status = "E") then
            if (NOT verify_field("txpp_full_exem_cd", update_flag)) then
               next field txpp_full_exem_cd
            end if
         end if
         if (NOT verify_field("txpp_juris_cd", update_flag)) then
            next field txpp_juris_cd
         end if
         if (NOT verify_field("txpp_class_cd", update_flag)) then
            next field txpp_class_cd
         end if
         if (NOT verify_field("txpp_subdiv", update_flag)) then
            next field txpp_subdiv
         end if
         if (NOT verify_field("txpp_nature_own", update_flag)) then
            next field txpp_nature_own
         end if
         if (NOT verify_field("txpp_resident", update_flag)) then
            next field txpp_resident
         end if
         if (NOT verify_field("txpp_date_field", update_flag)) then
            next field txpp_date_field
         end if
         if (gr_txppmast.txpp_date_field IS NOT NULL) then
            if (NOT verify_field("txpp_who_field", update_flag)) then
               next field txpp_who_field
            end if
         end if
         if (NOT verify_field("txpp_date_desk", update_flag)) then
            next field txpp_date_desk
         end if
         if (gr_txppmast.txpp_date_desk IS NOT NULL) then
            if (NOT verify_field("txpp_who_desk", update_flag)) then
               next field txpp_who_desk
            end if
         end if
         if (NOT verify_field("txpp_formal_lst", update_flag)) then
            next field txpp_formal_lst
         end if
         if (gr_txppmast.txpp_mt_sw != 0) then
            if (NOT verify_field("txpp_comply_date", update_flag)) then
               next field txpp_comply_date
            end if
         end if

      &include "StandardEntryDialogActions.inc"

   end input

end function
#############################################################################
function verify_field(fieldname, update_flag)
   define abst_type char(1),
          lv_customerTypeCode char(8),
          lv_customerTypeShortDescription char(10),
          lv_ssnOrFid char(14),
          fieldname,
          tmp_type_desc char(20),
          lv_customerTypeDescription char(40),
          lv_message,
          tmp_loc_city_st_zip,
          tmp_loc_desc string,
          lv_customerDeliveryAddressStatusActive,
          lv_isPerson boolean,
          lv_customerExists,
          update_flag,
          retval smallint

   # Initialize local variable(s)...
   let lv_customerTypeCode = " "
   let lv_customerTypeShortDescription = " "
   let lv_customerTypeDescription = " "
   let lv_isPerson = TRUE
   let lv_message = " "
   let lv_ssnOrFid = " "
   let retval = TRUE

   case
      when(fieldname = "txpp_id_no")
         if (gr_txppmast.txpp_id_no is NULL) then
            let gr_txppmast.txpp_id_no = 0
         end if
         if gr_txppmast.txpp_id_no <= 0 then
            call sperrormx("ID number must be greater than 0.")
            let retval = FALSE
         else
            select * from txppmast
              where txpp_mt_sw = gr_txppmast.txpp_mt_sw
                and txpp_id_no = gr_txppmast.txpp_id_no
                and txpp_year = gr_txppmast.txpp_year
                and txpp_list_no = gr_txppmast.txpp_list_no
            if NOT sqlca.sqlcode then
               call sperrormx("This record already exists.")
               let retval = FALSE
            end if
         end if

      when(fieldname = "txpp_dba")
         if (gr_txppmast.txpp_dba IS NULL) then
            let gr_txppmast.txpp_dba = " "
         end if

      when(fieldname = "txpp_parc_id")
         if length(gr_txppmast.txpp_parc_id) then
            select txre_parcel_id from txremast
              where txre_mt_sw = 0
                and txre_parcel_id = gr_txppmast.txpp_parc_id
                and txre_year = 0
                and txre_list_no = 0
            if sqlca.sqlcode then
               call sperrormx("Warning - Parcel not found.")
            end if
         end if

      when(fieldname = "gv_oldID")
         if (gv_oldID is NULL) then
            let gv_oldID = " "
         end if
         # Ensure the txppmast record is updated...
         if (ST_CLIENT_STATE != "VA") then
            let gr_txppmast.txpp_old_id = gv_oldID
         end if

      when(fieldname = "cust_acct")
         if (NOT cust_acct) then
            call sperrorm("Owner is a required field.", 0, 1)
            let retval = false
            exit case
         else
            let lv_customerExists = false
            call CustomerManager.customerExists(cust_acct) returning lv_customerExists
            if (ExceptionState.hasException()) then
               call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_DISPLAYDIALOG, sfmt("Error selecting Customer ID: %1. Line: %2", cust_acct, __LINE__))
               let retval = false
               exit case
            else
               if (not lv_customerExists) then
                  call sperrorm("Customer does not exist.", 0, 1)
                  let retval = false
                  exit case
               end if
            end if
         end if
         # TODO: determine how this value is now being set (if at all) 
         #       value maps to AccountsReceivable.Customers.Class and included in type CustomerManager.Customer
         if (ST_CLIENT_STATE = "NC") or (ST_CLIENT_STATE = "TN") then
            select arcs_cust_class into abst_type
               from arcstcid
               where arcs_acct = cust_acct
            if (length(gr_txppmast.txpp_abst_type) = 0) then
               let gr_txppmast.txpp_abst_type = abst_type
               display by name gr_txppmast.txpp_abst_type
            end if
         end if

      when(fieldname = "addl_no")
         if ((addl_no is null) or
             (addl_no is not null and addl_no < 0)) then
            call sperrorm("Address number cannot be less than zero.", 0, 1)
            let retval = FALSE
         else
            call get_address(cust_acct, addl_no) returning cid_error, lv_customerDeliveryAddressStatusActive
            if (cid_error) then
               call sperrorm("Delivery address number not found.", 0, 1)
               let retval = FALSE
               exit case
            end if

            # If adding a record, attempt to populate the SSN/FID field on the property...
            if (NOT update_flag) then
               # Obtain personal data...
               call CustomerManager.getDeliveryAddressSSNorFID(cust_acct, addl_no, 1) returning lv_ssnOrFid, lv_isPerson
               if (ExceptionState.hasException()) then
                  if (NOT ExceptionState.hasNotFoundSQLException()) then
                     call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_WRITESYSTEMLOG + ExceptionHandler.c_DISPLAYERRORLINE, "Error calling getDeliveryAddressSSNorFID() for Customer ID: "||cust_acct||" and delivery addr: "||addl_no||". Line: "||__LINE__)
                  end if
               end if
               # Defensive programming...
               if (lv_ssnOrFid is NULL) then
                  let lv_ssnOrFid = " "
               end if
               let gv_customerSSN = lv_ssnOrFid
               let gr_txppmast.txpp_old_id = lv_ssnOrFid
            end if

            if (lv_customerDeliveryAddressStatusActive) then
               call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", true)
            else
               call spdomlib_hideNodeByTag("Label", "customer_status_lbl_tag", false)
               call sperrorm("Owner's delivery address is inactive.  Please choose an active address.", 0, 1)
               let retval = false
               exit case
            end if
            display by name gv_customerSSN, cust_name ,cust_name2, cust_addr, cust_addr2, cust_city, cust_state, cust_zip, cust_country
         end if

      when(fieldname = "gv_customerSSN")
         if (gv_customerSSN is NULL) then
            let gv_customerSSN = " "
         end if
         # Ensure the txppmast record is updated...
         if (ST_CLIENT_STATE == "VA") then
            let gr_txppmast.txpp_old_id = gv_customerSSN
         end if

      when(fieldname = "txpp_loc_no")
         if (gr_txppmast.txpp_loc_no IS NULL) then
             let gr_txppmast.txpp_loc_no = 0
             display by name gr_txppmast.txpp_loc_no
         end if

      when(fieldname = "txpp_loc_no_suff")
         if (gr_txppmast.txpp_loc_no_suff IS NULL) then
             let gr_txppmast.txpp_loc_no_suff = " "
         end if

      when(fieldname = "street_cd")
         if (street_cd is NULL) then
            let street_cd = " "
         end if
         if (length(street_cd) > 0) then
            select txst_name into gr_txppmast.txpp_loc_street
             from txstreet
             where txstreet.txst_code = street_cd
            case
               when (sqlca.sqlcode = NOTFOUND)
                  let err_msg = "Street code does not exist."
                  call sperrorm(err_msg, 0, 1)
                  let gr_txppmast.txpp_loc_street = " "
                  let retval = FALSE
               when (sqlca.sqlcode != 0)
                  let err_msg = "Unexpected error selecting from Street Code. ",
                                "Status:"
                  call spsyserr(err_msg, "Select 4756 - txstreet")
                  let gr_txppmast.txpp_loc_street = " "
                  let retval = FALSE
            end case
         end if

      when(fieldname = "txpp_loc_street")
         if (gr_txppmast.txpp_loc_street IS NULL) then
             let gr_txppmast.txpp_loc_street = " "
         end if

      when(fieldname = "txpp_loc_apt")
         if (gr_txppmast.txpp_loc_apt IS NULL) then
             let gr_txppmast.txpp_loc_apt = " "
         end if

      when(fieldname = "txpp_loc_id")
         if (gr_txppmast.txpp_loc_id IS NULL) then
            let gr_txppmast.txpp_loc_id = " "
         end if

         if (gr_txppmast.txpp_loc_id != " ") then
            call cploclib_formatShortLoc(gr_txppmast.txpp_loc_id,
                                         gr_txppmast.txpp_loc_seq,
                                         " ",
                                         0,
                                         " ",
                                         " ",
                                         " ",
                                         " ",
                                         " ",
                                         " ",
                                         " ",
                                         " ")
                               returning tmp_loc_desc,
                                         tmp_loc_city_st_zip,
                                         retval,
                                         err_msg
            if (retval) then
               let format_loc = tmp_loc_desc clipped, "\n",
                                tmp_loc_city_st_zip
               let loc_seq = "Seq: ",
                             gr_txppmast.txpp_loc_seq using "<<<<&"
               display by name loc_seq,
                               format_loc
            end if

            if (retval < 0) then
               let format_loc = " "
               let loc_seq = " "
               display by name loc_seq,
                               format_loc
               call sperrorm(err_msg, 0, 1)
               let retval = FALSE
               exit case
            end if
         else
            let loc_seq = " "
            let format_loc = " "
            display by name loc_seq,
                            format_loc
         end if

      when(fieldname = "txpp_status")
         if length(gr_txppmast.txpp_status) then
            if (NOT gr_txppmast.txpp_status matches "[AIEH]") then
               call sperrormx("Status must be an active, inactive, exempt or historical.")
               let retval = FALSE
            end if
         else
            call sperrormx("Status is a required field.")
            let retval = FALSE
         end if

      when(fieldname = "txpp_inact_date")
         if gr_txppmast.txpp_inact_date is NULL then
            call sperrormx("Inactive date is required when status is inactive.")
            let retval = FALSE
            exit case
         end if
         if (gr_txppmast.txpp_inact_date < gr_txppmast.txpp_create_date) then
             let err_msg = "Inactive date cannot be less than ",
                      gr_txppmast.txpp_create_date using "mm/dd/yyyy", "."
             call sperrorm(err_msg, 0, 1)
             let retval = FALSE
         end if

      when(fieldname = "txpp_abst_type")
         if (ST_CLIENT_STATE = "VA" or ST_CLIENT_STATE = "NC" or
             ST_CLIENT_STATE = "TN" or ST_CLIENT_STATE = "MO") then
            if (length(gr_txppmast.txpp_abst_type) = 0) then
               let gr_txppmast.txpp_abst_type = " "
               let err_msg = "Abstract type is a required field"
               call sperrorm(err_msg, 0, 1)
               let retval = FALSE
               exit case
            end if
         end if

         if ST_CLIENT_STATE = "VA" then
            select prfld_desc into tmp_type_desc from prfldval
               where prfld_fun = "tx"
                 and prfld_fld = "va_abst_type"
                 and prfld_val = gr_txppmast.txpp_abst_type
            if (sqlca.sqlcode != 0) then
               call sperrormx("Invalid abstract type.")
               let retval = FALSE
            end if
         else
            if (ST_CLIENT_STATE = "NC" or
                ST_CLIENT_STATE = "TN" or
                ST_CLIENT_STATE = "MO") then
               select prfld_desc into tmp_type_desc from prfldval
                  where prfld_fun = "tx"
                    and prfld_fld = "abst_type"
                    and prfld_val = gr_txppmast.txpp_abst_type
               if (sqlca.sqlcode != 0) then
                  call sperrormx("Invalid abstract type.")
                  let retval = FALSE
               end if
            end if
         end if

      when(fieldname = "txpp_full_exem_cd")
         if (length(gr_txppmast.txpp_full_exem_cd) > 0) then
            select ared_desc into exem_desc from arexdisc
             where ared_total = "Y"
               and ared_code = gr_txppmast.txpp_full_exem_cd
            if (sqlca.sqlcode = NOTFOUND) then
               call sperrormx("Invalid exemption code.")
               let retval = FALSE
               let exem_desc = " "
            end if
         else
            call sperrormx("Total exempt code is required when status is exempt.")
            let retval = FALSE
            let exem_desc = " "
         end if
         display by name exem_desc

      when(fieldname = "txpp_list_status")
         if (length(gr_txppmast.txpp_list_status) > 0) then
             select prfld_val, prfld_desc
                from prfldval
                  where prfld_fun = "tx"
             and prfld_fld = "list_stat"
             and prfld_val = gr_txppmast.txpp_list_status
             if (sqlca.sqlcode != 0) then
                 call sperrormx("Invalid list status.")
                 let retval = FALSE
             end if
         end if
         if (ct_sw = "C") then
            let gr_txppmast.txpp_list_status = " "
            display by name gr_txppmast.txpp_list_status
         end if

      when(fieldname = "txpp_juris_cd")
         if (length(gr_txppmast.txpp_juris_cd) = 0) then
            let jur_desc = " "
            display by name jur_desc
            call sperrormx("Jurisdiction code is required.")
            let retval = FALSE
         else
            select txju_desc into jur_desc from txjurisd
             where txju_code = gr_txppmast.txpp_juris_cd
            if (sqlca.sqlcode) then
               let err_msg = "Invalid jurisdiction code."
               let jur_desc = " "
               display by name jur_desc
               call sperrorm(err_msg, 0, 1)
               let retval = FALSE
               exit case
            end if
         end if
         display by name jur_desc

      when(fieldname = "txpp_class_cd")
         if (length(gr_txppmast.txpp_class_cd) = 0) then
            let class_desc = " "
            display by name class_desc
            call sperrormx("Class code is required.")
            let retval = FALSE
         else
            select txcl_desc into class_desc from txclascd
             where txcl_class   = gr_txppmast.txpp_class_cd
               and txcl_ar_cat  = 25
               and txcl_lnpg_sw = "G"
            if (sqlca.sqlcode) then
               let class_desc = " "
               display by name class_desc
               let err_msg = "Invalid class code."
               call sperrorm(err_msg, 0, 1)
               let retval = FALSE
               exit case
            end if
         end if
         display by name class_desc

      when(fieldname = "txpp_subdiv")
         if length(gr_txppmast.txpp_subdiv) = 0 then
            let gr_txppmast.txpp_subdiv = " "
            let sub_desc = " "
            display by name sub_desc
         else
            select txsd_desc into sub_desc from txsubdiv
             where txsd_code = gr_txppmast.txpp_subdiv
            if (sqlca.sqlcode) then
               let sub_desc = " "
               display by name sub_desc
               let err_msg = "Invalid subdivision code."
               call sperrorm(err_msg, 0, 1)
               let retval = FALSE
               exit case
            end if
         end if
         display by name sub_desc

      when(fieldname = "txpp_nature_own")
         if (length(gr_txppmast.txpp_nature_own) = 0) then
            let gr_txppmast.txpp_nature_own = " "
            let lender_name = " "
            display by name lender_name
         else
            if (ST_CLIENT_STATE = "VA") then
               select arlr_name into lender_name from arlender
                where arlr_code = gr_txppmast.txpp_nature_own
               if (sqlca.sqlcode) then
                  let err_msg = "Invalid lender code."
                  let lender_name = " "
                  display by name lender_name
                  call sperrorm(err_msg, 0, 1)
                  let retval = FALSE
                  exit case
               end if
            else
               select txno_desc into lender_name from txnatown
                where txno_code = gr_txppmast.txpp_nature_own
               if (sqlca.sqlcode) then
                  let lender_name = " "
               end if
            end if
         end if
         display by name lender_name

      when(fieldname = "txpp_pos_audit")
         if (ST_CLIENT_STATE != "NC" and
             ST_CLIENT_STATE != "TN") then
            if (gr_txppmast.txpp_pos_audit is NULL) or
               ((gr_txppmast.txpp_pos_audit != "Y") and
                (gr_txppmast.txpp_pos_audit != "N")) then
               call sperrorm("Enter Y (Yes) or N (No) only.",0,1)
               let retval = FALSE
             end if
         end if

      when(fieldname = "txpp_resident")
         if (gr_txppmast.txpp_resident != "Y" and
             gr_txppmast.txpp_resident != "N") then
            call sperrorm("Check for 'Yes' or uncheck for 'No'.", 0, 1)
            let retval = FALSE
         end if

      when(fieldname = "txpp_date_field")
      when(fieldname = "txpp_who_field")
      when(fieldname = "txpp_date_desk")
      when(fieldname = "txpp_who_desk")

      when(fieldname = "txpp_business")
         if (length(gr_txppmast.txpp_business) > 0) then
            call CustomerManager.getLegacyCustomerTypeForId(gr_txppmast.txpp_business)
               returning lv_customerTypeCode, lv_customerTypeShortDescription, lv_customerTypeDescription
            if (ExceptionState.hasException()) then
               if (NOT ExceptionState.HasNotFoundSQLException()) then
                  call ExceptionHandler.processException(ExceptionState.getException(),
                                                         ExceptionHandler.c_WRITESYSTEMLOG + ExceptionHandler.c_DISPLAYERRORLINE,
                                                         "Error retrieving customer type information.")
               end if

               let err_msg = "Invalid business code."
               call sperrorm(err_msg, 0, 1)
               let retval = FALSE
               exit case
            end if
         else
            let gr_txppmast.txpp_business = " "
         end if

      when(fieldname = "txpp_comply")
         if ((gr_txppmast.txpp_comply is NULL) or
             NOT(gr_txppmast.txpp_comply matches "[YNL]")) then
            call sperrorm("Enter Y (Yes), N (No), or L (Late).",0,1)
            let retval = FALSE
         end if

      when(fieldname = "txpp_fml_lst_yr")
         if (gr_txppmast.txpp_fml_lst_yr is NULL) then
            let gr_txppmast.txpp_fml_lst_yr = 0
         end if
         if ((gr_txppmast.txpp_fml_lst_yr <= 1970) and
             (gr_txppmast.txpp_fml_lst_yr != 0)) then
            let err_msg = "Last compliance year must be greater than 1970."
            call sperrorm(err_msg,0,1)
            let retval = FALSE
        end if

      when(fieldname = "txpp_audit")
         if (gr_txppmast.txpp_audit is NULL) or
            ((gr_txppmast.txpp_audit != "Y") and
             (gr_txppmast.txpp_audit != "N")) then
            call sperrorm("Enter Y (Yes) or N (No) only.",0,1)
            let retval = FALSE
          end if

      when(fieldname = "txpp_audit_yr")
         if (gr_txppmast.txpp_audit_yr is NULL) then
            let gr_txppmast.txpp_audit_yr = 0
         end if
         if ((gr_txppmast.txpp_audit_yr <= 1970) and
             (gr_txppmast.txpp_audit_yr != 0)) then
            call sperrormx("Audit year must be greater than 1970.")
            let retval = FALSE
        end if

      when(fieldname = "txpp_assmt_ly")
         if (gr_txppmast.txpp_assmt_ly is NULL) then
            let gr_txppmast.txpp_assmt_ly = 0
         end if
         if (gr_txppmast.txpp_assmt_ly is NULL) or
            (gr_txppmast.txpp_assmt_ly < 0) then
            call sperrorm("Value must be 0 or greater.",0,1)
            let retval = FALSE
         end if

      when(fieldname = "txpp_formal_lst")
         if (ST_CLIENT_STATE = "VA") then
            if (gr_txppmast.txpp_formal_lst != "Y" and
                gr_txppmast.txpp_formal_lst != "N") then
               call sperrorm("Check for 'Yes' or uncheck for 'No'.", 0, 1)
               let retval = FALSE
            end if
         end if

      when(fieldname = "txpp_comply_no")
         if (gr_txppmast.txpp_comply_no IS NULL) then
            let gr_txppmast.txpp_comply_no = 0
         end if

      when(fieldname = "txpp_comply_date")

   end case

   return retval

end function
###########################################################################
function get_next_ppid()
   define new_ppid,
          tmp_ppid         like txparams.txpm_next_ppid,
          continue_search  smallint,
          sel_line         string

   let continue_search = TRUE

   select txpm_next_ppid into tmp_ppid
     from txparams
    where txpm_code = "P"

   if (tmp_ppid IS NULL) then
      let tmp_ppid = 1
   end if

   while continue_search

      let sel_line = "select count(*) from txppmast",
                             " where txpp_mt_sw = 0 ",
                               " and txpp_id_no = ",
                                 tmp_ppid using "##########",
                               " and txpp_year = 0 ",
                               " and txpp_list_no = 0"

      if (spcounter(sel_line,0)) then
         let tmp_ppid = tmp_ppid + 1
      else
         let continue_search = FALSE
      end if

   end while

   let new_ppid = tmp_ppid

   let tmp_ppid = tmp_ppid + 1

   if (NOT sp_begin_work(2799)) then
      return 0
   end if

   if (NOT lock_txpm_rec()) then
      if (NOT sp_rollback_work(2800)) then
         close lock_txpm
         return 0
      end if
      return 0
   end if

   execute update_txparams using tmp_ppid
   if (sqlca.sqlcode != 0 or sqlca.sqlerrd[3] = 0) then
      let err_msg = "Error updating txparams."
      call spsyserr(err_msg, "Update txparams - 2805")
      if (NOT sp_rollback_work(2810)) then
         close lock_txpm
         return 0
      end if
      close lock_txpm
      return 0
   end if
   close lock_txpm

   if (NOT sp_commit_work(2815)) then
      return 0
   end if

   return new_ppid

end function
###########################################################################
function update_record(in_txppmast)

   define in_txppmast   record like txppmast.*,
          tmp_success   smallint,
          tmp_err_msg   string,
          tmp_val_array dynamic array of record
             invalid_field string,
             invalid_reason string
          end record

   if (ST_CLIENT_STATE = "VA") then
      if (in_txppmast.txpp_juris_cd != hold_juriscd) then
         execute update_mvamjurs using in_txppmast.txpp_juris_cd,
                                       in_txppmast.txpp_year,
                                       char_id_no,
                                       in_txppmast.txpp_list_no
      end if
   end if
   call txppmastwr_update(in_txppmast.*, FALSE, FALSE, tmp_val_array, TRUE, TRUE)
                                 returning tmp_success, tmp_err_msg
   if (NOT tmp_success) then
      call sperrorm(tmp_err_msg, 0, 2)
   end if

end function
###############################################################################
function check_int(entered_no)
   define entered_no integer

   if entered_no is NULL then
      return ZERO
   else
      return entered_no
   end if

end function
###############################################################################
function inquire_adj_routine()
   define max_keyno   integer,
          cmdstr      char(500)

   select max(spcmdarg.spca_keyno) into max_keyno from spcmdarg
      where spcmdarg.spca_userid = ST_USER

   if (max_keyno is NULL or max_keyno > 999999999) then
      let max_keyno = 0
   end if

   let max_keyno = max_keyno + 1

   let cmdstr[1,30]   = gr_txppmast.txpp_id_no using "<<<<<<<<<<"
   let cmdstr[31,32]  = 25
   let cmdstr[33,36]  = gr_txppmast.txpp_year using "&&&&"
   let cmdstr[37,44]  = gr_txppmast.txpp_list_no using "&&&&&&&&"

   insert into spcmdarg values (ST_USER, max_keyno, cmdstr)

   call sprunpgm("I", "txadjinq", max_keyno)

end function
###############################################################################
function assmt_history_routine()
   define lv_billType char(1),
          i,
          lv_billCategory,
          lv_billYear,
          lv_version smallint,
          lv_billNumber,
          lv_count integer,
          lv_assessment,
          lv_exemption,
          lv_motorVehicleAssessment,
          lv_totalAssessment decimal(11,0)

   define lr_tmphist record
            gl_year smallint,
            yd char(4),
            h_type char(1),
            list_no integer,
            assmt decimal(11,0),
            exem decimal(11,0),
            net decimal(11,0)
          end record

   # Initialize local variable(s)...
   let lv_assessment = 0
   let lv_exemption = 0
   let lv_billCategory = 0
   let lv_billNumber = 0
   let lv_billType = " "
   let lv_billYear = 0
   let lv_count = 0
   let lv_motorVehicleAssessment = 0
   let lv_totalAssessment = 0
   let lv_version = 0

   # Initialize global variable(s)...
   let err_flag = FALSE

   # Remove any existing records...
   delete from tmphist where 1=1
   if (sqlca.sqlcode != 0) then
      call spsyserr("Unexpected error deleting from temp table. Status:", "Delete tmphist, line: " || __LINE__)
      let err_flag = TRUE
      return
   end if

   #region Obtain assessment amount for current
   let lv_billNumber = 0
   let lv_billYear = 0
   let lv_version = 0

   # Sum the assessment amount (txva_amt) from all value records (txvalues) by property and year...
   execute sumAssessmentByPropYear using lv_version, id_10, lv_billYear into lv_count, lv_assessment, lv_billNumber
   if ((sqlca.sqlcode != 0) AND (sqlca.sqlcode != NOTFOUND)) then
      call spsyserr("Unexpected error executing sumAssessmentByPropYear.", "Execute sumAssessmentByPropYear, line: " || __LINE__)
      let err_flag = TRUE
      return
   end if

   if (ST_CLIENT_STATE == "VA") then
      # Sum the motor vehicle assessment amount (mvam_assessment) from all motor vehicle records (mvamvmst) by property and year...
      execute sumMotorVehicleAssessmenByYearAndNumber using lv_version, lv_billYear, lv_billNumber, id_10 into lv_motorVehicleAssessment
      if ((sqlca.sqlcode != 0) AND (sqlca.sqlcode != NOTFOUND)) then
         call spsyserr("Unexpected error executing sumMotorVehicleAssessmenByYearAndNumber.", "Execute sumMotorVehicleAssessmenByYearAndNumber, line: " || __LINE__)
         let err_flag = TRUE
         return
      end if
   end if

   # Combine the regular assessment amount and the motor vehicle assessment amount to obtain the total assessment amount...
   let lv_totalAssessment = lv_assessment + lv_motorVehicleAssessment

   # Sum the exemption amount (txce_amt) from all customer exemption records (txcsttag) by property...
   execute sumExemptionByProperty using id_10 into lv_exemption
   if ((sqlca.sqlcode != 0) AND (sqlca.sqlcode != NOTFOUND)) then
      call spsyserr("Unexpected error executing sumExemptionByProperty.", "Execute sumExemptionByProperty, line: " || __LINE__)
      let err_flag = TRUE
      return
   end if

   # Populate the temporary history table...
   let lr_tmphist.gl_year = 9999
   let lr_tmphist.yd = "CURR"
   let lr_tmphist.h_type = " "
   let lr_tmphist.list_no = 0
   let lr_tmphist.assmt = lv_totalAssessment
   let lr_tmphist.exem = lv_exemption
   let lr_tmphist.net = iif(lv_totalAssessment >= lv_exemption, lv_totalAssessment - lv_exemption, 0)

   # Insert the temporary history record...
   execute insert_tmphist using lr_tmphist.*
   if ((sqlca.sqlcode != 0) AND (sqlca.sqlcode != NOTFOUND)) then
      call spsyserr("Unexpected error inserting into temp table.", "Execute insert_tmphist, line: " || __LINE__)
      let err_flag = TRUE
      return
   end if
   #endregion

   #region Obtain assessment amount for the assessment year
   let lv_billNumber = 0
   let lv_billYear = assm_year
   let lv_count = 0
   let lv_version = 1

   # Re-initialize local variable(s)...
   let lv_assessment = 0
   let lv_exemption = 0
   let lv_motorVehicleAssessment = 0
   let lv_totalAssessment = 0

   # Sum the assessment amount (txva_amt) from all value records (txvalues) by property and year...
   execute sumAssessmentByPropYear using lv_version, id_10, lv_billYear into lv_count, lv_assessment, lv_billNumber
   if ((sqlca.sqlcode != 0) AND (sqlca.sqlcode != NOTFOUND)) then
      call spsyserr("Unexpected error executing sumAssessmentByPropYear.", "Execute sumAssessmentByPropYear, line: " || __LINE__)
      let err_flag = TRUE
      return
   end if

   if (ST_CLIENT_STATE == "VA") then
      # Sum the motor vehicle assessment amount (mvam_assessment) from all motor vehicle records (mvamvmst) by property and year...
      execute sumMotorVehicleAssessmenByYearAndNumber using lv_version, lv_billYear, lv_billNumber, id_10 into lv_motorVehicleAssessment
      if ((sqlca.sqlcode != 0) AND (sqlca.sqlcode != NOTFOUND)) then
         call spsyserr("Unexpected error executing sumMotorVehicleAssessmenByYearAndNumber.", "Execute sumMotorVehicleAssessmenByYearAndNumber, line: " || __LINE__)
         let err_flag = TRUE
         return
      end if
   end if

   # Combine the regular assessment amount and the motor vehicle assessment amount to obtain the total assessment amount...
   let lv_totalAssessment = lv_assessment + lv_motorVehicleAssessment

   if (lv_count) then
      # Sum the exemption amount (txva_amt) from all exemptions (txexemps) by property, year, and bill...
      execute sumExemptionByYearAndNumber using lv_billYear, lv_billNumber, id_10 into lv_exemption
      if ((sqlca.sqlcode != 0) AND (sqlca.sqlcode != NOTFOUND)) then
         call spsyserr("Unexpected error executing sumExemptionByYearAndNumber.", "Execute sumExemptionByYearAndNumber, line: " || __LINE__)
         let err_flag = TRUE
         return
      end if

      # If a bill exists, select the bill type...
      if (lv_billNumber) then
         execute getBillType using lv_billYear, lv_billNumber into lv_billType
         if ((sqlca.sqlcode != 0) AND (sqlca.sqlcode != NOTFOUND)) then
            call spsyserr("Unexpected error executing getBillType.", "Execute getBillType, line: " || __LINE__)
            let err_flag = TRUE
            return
         end if
      end if

      # Populate the temporary history table...
      let lr_tmphist.gl_year = lv_billYear
      let lr_tmphist.yd = lv_billYear using "&&&&"
      let lr_tmphist.h_type = lv_billType
      let lr_tmphist.list_no = lv_billNumber
      let lr_tmphist.assmt = lv_totalAssessment
      let lr_tmphist.exem = lv_exemption
      let lr_tmphist.net = iif(lv_totalAssessment >= lv_exemption, lv_totalAssessment - lv_exemption, 0)

      # Insert the temporary history record...
      execute insert_tmphist using lr_tmphist.*
      if ((sqlca.sqlcode != 0) AND (sqlca.sqlcode != NOTFOUND)) then
         call spsyserr("Unexpected error inserting into temp table.", "Execute insert_tmphist, line: " || __LINE__)
         let err_flag = TRUE
         return
      end if
   end if
   #endregion

   #region Obtain assessment amount for all non assessment years
   let lv_billNumber = 0
   let lv_billYear = 0
   let lv_version = 0

   foreach getAllPriorBillsCursor using assm_year, id_10 into lv_billYear, lv_billNumber, lv_billType
      if (sqlca.sqlcode != 0) then
         if (sqlca.sqlcode != NOTFOUND) then
            call spsyserr("Unexpected error executing getAllPriorBillsCursor.", "Execute getAllPriorBillsCursor, line: " || __LINE__)
            let err_flag = TRUE
         end if
         exit foreach
      end if

      # Re-initialize bill level variable(s)...
      let lv_assessment = 0
      let lv_exemption = 0
      let lv_motorVehicleAssessment = 0
      let lv_totalAssessment = 0

      # Sum the assessment amount (txva_amt) from all value records (txvalues) by property and year...
      execute sumAssessmentByPropYearNumber using id_10, lv_billYear, lv_billNumber into lv_assessment
      if ((sqlca.sqlcode != 0) AND (sqlca.sqlcode != NOTFOUND)) then
         call spsyserr("Unexpected error executing sumAssessmentByPropYearNumber.", "Execute sumAssessmentByPropYearNumber, line: " || __LINE__)
         let err_flag = TRUE
         exit foreach
      end if

      if (ST_CLIENT_STATE == "VA") then
         # Sum the motor vehicle assessment amount (mvam_assessment) from all motor vehicle records (mvamvmst) by property and year...
         let lv_version = 1
         execute sumMotorVehicleAssessmenByYearAndNumber using lv_version, lv_billYear, lv_billNumber, id_10 into lv_motorVehicleAssessment
         if ((sqlca.sqlcode != 0) AND (sqlca.sqlcode != NOTFOUND)) then
            call spsyserr("Unexpected error executing sumMotorVehicleAssessmenByYearAndNumber.", "Execute sumMotorVehicleAssessmenByYearAndNumber, line: " || __LINE__)
            let err_flag = TRUE
            exit foreach
         end if
      end if

      # Combine the regular assessment amount and the motor vehicle assessment amount to obtain the total assessment amount...
      let lv_totalAssessment = lv_assessment + lv_motorVehicleAssessment

      # Sum the exemption amount (txva_amt) from all exemptions (txexemps) by property, year, and bill...
      execute sumExemptionByYearAndNumber using lv_billYear, lv_billNumber, id_10 into lv_exemption
      if ((sqlca.sqlcode != 0) AND (sqlca.sqlcode != NOTFOUND)) then
         call spsyserr("Unexpected error executing sumExemptionByYearAndNumber.", "Execute sumExemptionByYearAndNumber, line: " || __LINE__)
         let err_flag = TRUE
         exit foreach
      end if

      # Populate the temporary history table...
      let lr_tmphist.gl_year = lv_billYear
      let lr_tmphist.yd = lv_billYear using "&&&&"
      let lr_tmphist.h_type = lv_billType
      let lr_tmphist.list_no = lv_billNumber
      let lr_tmphist.assmt = lv_totalAssessment
      let lr_tmphist.exem = lv_exemption
      let lr_tmphist.net = iif(lv_totalAssessment >= lv_exemption, lv_totalAssessment - lv_exemption, 0)

      # Insert the temporary history record...
      execute insert_tmphist using lr_tmphist.*
      if ((sqlca.sqlcode != 0) AND (sqlca.sqlcode != NOTFOUND)) then
         call spsyserr("Unexpected error inserting into temp table.", "Execute insert_tmphist, line: " || __LINE__)
         let err_flag = TRUE
         exit foreach
      end if
   end foreach
   #endregion

   if (err_flag) then
      return
   end if

   select count(*) 
     into hist_count
     from tmphist
   if ((sqlca.sqlcode != 0) AND (sqlca.sqlcode != NOTFOUND)) then
      call spsyserr("Unexpected error selecting from temp table. Status:", "Select count, line: " || __LINE__)
      return
   end if

   if (hist_count) then
      let i = browse_routine(10, hist_count)
   else
      call sperrorm("No history files exist for this parcel.", 0, 1)
   end if

end function
##############################################################################
function print_export(prog)
   define charge ChargeType,
          tmp_class_cd char(4),
          tmp_chg_def char(6),
          tmp_exem_cd char(6),
          prog,
          lv_customerTypeCode char(8),
          lv_customerTypeShortDescription char(10),
          tmp_exm_desc,
          char_conv char(30),
          tmp_chg_desc,
          lv_customerTypeDescription char(40),
          hyperLinkArgs,
          st_comply_no,
          st_comply_dt string,
          lv_isConfidential boolean,
          success,
          tmp_chg_cnt,
          lv_addressSeq,
          i smallint,
          j,
          firstrec,
          lastrec,
          lv_customerId,
          tmp_ppval_purch_yr,
          tmp_ppval_pct_good,
          prfetchno integer,
          tmp_exem_amt dec(10,2),
          tmp_net_assmt,
          tmp_net_value,
          tmp_val_amt,
          tmp_tot_exem,
          tmp_tot_value dec(11,0),
          tmp_tot_tax,
          tmp_chg_amt,
          tmp_penalty,
          tmp_prin_tax,
          tmp_ppval_purch_amt,
          tmp_ppval_curr_value,
          tmp_ppval_acquisitions,
          tmp_ppval_removals,
          tmp_spec_assmt dec(11,2),
          tmp_chg_rate dec(11,6),
          er_txppmast record like txppmast.*,
          lv_customerAddress CustomerManager.NameAndAddress

   # Initialize local variable(s)...
   let lv_customerTypeCode = " "
   let lv_customerTypeShortDescription = " "

# Set comply date and number field names to match labels for various states
   case
      when (ST_CLIENT_STATE = "VA")
         let st_comply_no = "RETURN NO"
         let st_comply_dt = "FILING DATE"
      when (ST_CLIENT_STATE = "NC")
         let st_comply_no = "FILING NO"
         let st_comply_dt = "COMPLY DATE"
      when (ST_CLIENT_STATE = "CT")
         let st_comply_no = "COMPLY NO"
         let st_comply_dt = "NOTICE OF CHANGE DATE"
      otherwise
         let st_comply_no = "COMPLY NO"
         let st_comply_dt = "COMPLY DATE"
   end case

   let firstrec = 1
   let lastrec  = record_count

   call spoffice_beginExport(prog, "txppmast", " ", "Personal Property Inquiry", "txppmast", "")
        returning success, err_msg
   if (NOT success) then
      call sperrorm(err_msg, 0, 2)
      return
   end if

   for prfetchno = firstrec to lastrec
      fetch absolute prfetchno find_cursor into active_mt_sw,
                                                active_id_no,
                                                active_year,
                                                active_list_no
      if (sqlca.sqlcode = -213) then
         exit for
      end if
      if (sqlca.sqlcode != 0) then
         continue for
      end if

      let char_conv = active_id_no

      select * into er_txppmast.* from txppmast
       where txppmast.txpp_mt_sw = active_mt_sw
         and txppmast.txpp_id_no = active_id_no
         and txppmast.txpp_year = active_year
         and txppmast.txpp_list_no = active_list_no
      if (sqlca.sqlcode = -213) then
         exit for
      end if
      if (sqlca.sqlcode != 0) then
         continue for
      end if

      # Need to get the customer id, and address sequence number.
      if (active_mt_sw = 1 or active_mt_sw = 9) then
         select txon_acct, txon_addr_no into lv_customerId, lv_addressSeq from txowners
          where txon_mt_sw = active_mt_sw
            and txon_ar_cat = 25
            and txon_prop_id = char_conv
            and txon_year = active_year
            and txon_list_no = active_list_no
            and txon_own = "P"

         if (sqlca.sqlcode = -213) then
            exit for
         end if
         if (sqlca.sqlcode != 0) then
            continue for
         end if
      else
         select txtg_acct, txtg_bill_addr_no into lv_customerId, lv_addressSeq from txcsttag
          where txtg_ar_cat = 25
            and txtg_prop_id = char_conv
            and txtg_own = "P"

         if (sqlca.sqlcode = -213) then
            exit for
         end if
         if (sqlca.sqlcode != 0) then
            let lv_customerId = 0
         end if
      end if

      if (lv_customerId = 0) then
         let lv_customerAddress.Name1.Full = "UNKNOWN"
         let lv_customerAddress.Name2.Full = " "
         let lv_customerAddress.Address.Line1 = " "
         let lv_customerAddress.Address.Line2 = " "
         let lv_customerAddress.Address.City = " "
         let lv_customerAddress.Address.State = " "
         let lv_customerAddress.Address.ZipCode = " "
      else
         call CustomerManager.isCustomerConfidential(lv_customerId) returning lv_isConfidential
         if (ExceptionState.hasException()) then
            call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_DISPLAYDIALOG, sfmt("Error selecting owner's confidential indicator for Customer ID '%1'. Line %2. Error code", lv_customerId, __LINE__))
            continue for
         end if
         if (public_access = "Y" and lv_isConfidential) then
            let lv_customerAddress.Name1.Full = confid_msg
            let lv_customerAddress.Name2.Full = confid_msg
            let lv_customerAddress.Address.Line1 = confid_msg
            let lv_customerAddress.Address.Line2 = confid_msg
            let lv_customerAddress.Address.City = confid_msg
            let lv_customerAddress.Address.State = confid_msg
            let lv_customerAddress.Address.ZipCode = confid_msg
         else
            call CustomerManager.getNameAndAddress(lv_customerId, lv_addressSeq) returning lv_customerAddress.*
            if (ExceptionState.hasException()) then
               if (ExceptionState.hasNotFoundSQLException()) then
                  let lv_customerAddress.Name1.Full = "UNKNOWN"
                  let lv_customerAddress.Name2.Full = " "
                  let lv_customerAddress.Address.Line1 = " "
                  let lv_customerAddress.Address.Line2 = " "
                  let lv_customerAddress.Address.City = " "
                  let lv_customerAddress.Address.State = " "
                  let lv_customerAddress.Address.ZipCode = " "
               else
                  call ExceptionHandler.processException(ExceptionState.getException(), ExceptionHandler.c_WRITESYSTEMLOG + ExceptionHandler.c_DISPLAYERRORLINE, sfmt('Error selecting customer with id: %1, and address id: %2', lv_customerId, lv_addressSeq))
                  continue for
               end if
            end if
         end if
      end if

      if (length(er_txppmast.txpp_business) > 0) then
         call CustomerManager.getLegacyCustomerTypeForId(er_txppmast.txpp_business)
            returning lv_customerTypeCode, lv_customerTypeShortDescription, lv_customerTypeDescription
         if (ExceptionState.hasException()) then
            if (NOT ExceptionState.HasNotFoundSQLException()) then
               call ExceptionHandler.processException(ExceptionState.getException(),
                                                      ExceptionHandler.c_WRITESYSTEMLOG + ExceptionHandler.c_DISPLAYERRORLINE,
                                                      "Error retrieving customer type information.")
            end if
            let lv_customerTypeDescription = "UNKNOWN"
         end if
      else
         let lv_customerTypeDescription = " "
      end if

      select sum(txch_amt) into tmp_prin_tax from txchgdef
       where txch_mt_sw = active_mt_sw
         and txch_ar_cat = 25
         and txch_prop_id = char_conv
         and txch_year = active_year
         and txch_list_no = active_list_no
         and txch_actvty = "PRIN"
      if (sqlca.sqlcode = -213) then
         exit for
      end if
      if (sqlca.sqlcode != 0) then
         let tmp_prin_tax = 0
      end if

#       (ACCUMULATE SPECIAL ASSESSMENT TAX AMOUNT)
      select sum(txch_amt) into tmp_spec_assmt from txchgdef
       where txch_mt_sw = active_mt_sw
         and txch_ar_cat = 25
         and txch_prop_id = char_conv
         and txch_year = active_year
         and txch_list_no = active_list_no
         and txch_actvty in ("SPAP", "SPAI")
      if (sqlca.sqlcode = -213) then
         exit for
      end if
      if(sqlca.sqlcode != 0) then
         let tmp_spec_assmt = 0
      end if

#       (ACCUMULATE PENALTY CHARGE AMOUNT)
      select sum(txch_amt) into tmp_penalty from txchgdef
       where txch_mt_sw = active_mt_sw
         and txch_ar_cat = 25
         and txch_prop_id = char_conv
         and txch_year = active_year
         and txch_list_no = active_list_no
         and txch_actvty = "LL"
      if (sqlca.sqlcode = -213) then
         exit for
      end if
      if (sqlca.sqlcode != 0) then
         let tmp_penalty = 0
      end if

    #   (ACCUMULATE TOTAL TAX AMOUNT)
      select sum(txch_amt) into tmp_tot_tax from txchgdef
       where txch_mt_sw = active_mt_sw
         and txch_ar_cat = 25
         and txch_prop_id = char_conv
         and txch_year = active_year
         and txch_list_no = active_list_no
      if (sqlca.sqlcode = -213) then
         exit for
      end if
      if (sqlca.sqlcode != 0) then
         let tmp_tot_tax = 0
      end if

     #  (ACCUMULATE TOTAL VALUATION AMOUNT)
      select sum(txva_amt) into tmp_tot_value from txvalues
       where txva_mt_sw = active_mt_sw
         and txva_ar_cat = 25
         and txva_prop_id = char_conv
         and txva_year = active_year
         and txva_list_no = active_list_no
      if (sqlca.sqlcode = -213) then
         exit for
      end if
      if (sqlca.sqlcode != 0) then
         let tmp_tot_value = 0
      end if

#       (ACCUMULATE TOTAL EXEMPTION AMOUNT)
      if (active_mt_sw = 1 or active_mt_sw = 9) then
         select sum(txxm_amt) into tmp_tot_exem from txexemps
          where txxm_mt_sw = active_mt_sw
            and txxm_ar_cat = 25
            and txxm_prop_id = char_conv
            and txxm_acct = lv_customerId
            and txxm_year = active_year
            and txxm_list_no = active_list_no
            and txxm_at_sw = "A"
         if (sqlca.sqlcode = -213) then
            exit for
         end if
         if (sqlca.sqlcode != 0) then
            let tmp_tot_exem = 0
         end if
      else
         select sum(txce_amt) into tmp_tot_exem from txcstexm
          where txce_acct = lv_customerId
            and txce_ar_cat = 25
            and txce_prop_id = char_conv
         if (sqlca.sqlcode = -213) then
            exit for
         end if
         if (sqlca.sqlcode != 0) then
            let tmp_tot_exem = 0
         end if
      end if

  #     (CALCULATE NET ASSESSMENT AMOUNT)
      if (tmp_tot_exem > tmp_tot_value) then
         let tmp_tot_exem = tmp_tot_value
         let tmp_net_value = 0
      else
         let tmp_net_value = tmp_tot_value - tmp_tot_exem
      end if

#       (LOAD CHARGE ARRAY)
      for i = 1 to 6
         let chg_array[i].chg    = SPACE
         let chg_array[i].desc1  = SPACE
         let chg_array[i].rate   = ZERO
         let chg_array[i].assmt  = ZERO
         let chg_array[i].cnt    = ZERO
         let chg_array[i].amount = ZERO
      end for

      let i = 0

      let tmp_chg_amt = 0
      open select_txchgs using active_mt_sw,
                               char_conv,
                               active_year,
                               active_list_no
      if (sqlca.sqlcode = -213) then
         exit for
      end if
      while true
         if (i = 6) then
            exit while
         end if

         fetch select_txchgs into tmp_chg_def, tmp_net_assmt, tmp_chg_cnt, tmp_chg_amt
         if (sqlca.sqlcode = -213) then
            exit for
         end if
         if (sqlca.sqlcode != 0) then
            exit while
         end if

         let i = i + 1

         if (gr_txppmast.txpp_mt_sw = 0) then
            let charge = archgobj_open(25,
                                       assm_year,
                                       tmp_chg_def)
         else
            let charge = archgobj_open(25,
                                       active_year,
                                       tmp_chg_def)
         end if
         if charge IS NULL then
            let tmp_chg_desc = spbaddesc2(0, "NOTFOUND")
            let tmp_chg_rate = 0
         else
            let tmp_chg_desc = archgobj_getDescription1(charge)
            let tmp_chg_rate   = archgobj_getRateAmount(charge)
            call archgobj_free(charge)
         end if

         let chg_array[i].chg    = tmp_chg_def
         let chg_array[i].desc1  = tmp_chg_desc
         let chg_array[i].rate   = tmp_chg_rate using "###.&&&&"
         let chg_array[i].assmt  = tmp_net_assmt
         let chg_array[i].cnt    = tmp_chg_cnt
         let chg_array[i].amount = tmp_chg_amt

      end while
      close select_txchgs

      for i = 1 to 3
        let exem_array[i].exem = SPACE
        let exem_array[i].desc1 = SPACE
        let exem_array[i].amount = ZERO
      end for

      let i = 0

      if (active_mt_sw = 1 or active_mt_sw = 9) then
         open select_txem1 using active_mt_sw,
                                 char_conv,
                                 lv_customerId,
                                 active_year,
                                 active_list_no
      else
         open select_txce1 using lv_customerId,
                                 char_conv
      end if
      if (sqlca.sqlcode = -213) then
         exit for
      end if

      while true
         if (i = 3) then
            exit while
         end if
         if (active_mt_sw = 1 or active_mt_sw = 9) then
            fetch select_txem1 into tmp_exem_cd, tmp_exem_amt
            if (sqlca.sqlcode = -213) then
               exit for
            end if
            if (sqlca.sqlcode != 0) then
               exit while
            end if
         else
            fetch select_txce1 into tmp_exem_cd, tmp_exem_amt
            if (sqlca.sqlcode = -213) then
               exit for
            end if
            if (sqlca.sqlcode != 0) then
               exit while
            end if
         end if

         let i = i + 1

         select ared_desc into tmp_exm_desc from arexdisc
          where ared_code = tmp_exem_cd
         if (sqlca.sqlcode = -213) then
            exit for
         end if
         if (sqlca.sqlcode!=0) then
            let tmp_exm_desc = "UNKNOWN"
         end if

         let exem_array[i].exem = tmp_exem_cd
         let exem_array[i].desc1 = tmp_exm_desc
         let exem_array[i].amount = tmp_exem_amt
      end while

      if (active_mt_sw = 1 or active_mt_sw = 9) then
         close select_txem1
      else
         close select_txce1
      end if

      for i = 1 to 10
        let val_array[i].class = SPACE
        let val_array[i].desc1 = SPACE
        let val_array[i].amount = ZERO
        let j = 0
        for j = 1 to 10
           let val_array[i].ppval_array[j].ppval_yr = 0
           let val_array[i].ppval_array[j].ppval_pct_good = 0
           let val_array[i].ppval_array[j].ppval_purch_amt = 0
           let val_array[i].ppval_array[j].ppval_curr_value = 0
           let val_array[i].ppval_array[j].ppval_acquisitions = 0
           let val_array[i].ppval_array[j].ppval_removals = 0
        end for
      end for

      let i = 0
      open select_txval using active_mt_sw,
                              char_conv,
                              active_year,
                              active_list_no
      if (sqlca.sqlcode = -213) then
         exit for
      end if
      while true
         if i = 10 then
            exit while
         end if

         fetch select_txval into tmp_class_cd, tmp_val_amt
         if (sqlca.sqlcode = -213) then
            exit for
         end if
         if (sqlca.sqlcode != 0) then
            exit while
         end if

         let i = i + 1

         select txcl_desc into class_desc from txclascd
          where txcl_class = tmp_class_cd
            and txcl_ar_cat = 25
            and txcl_lnpg_sw = "P"
         if (sqlca.sqlcode = -213) then
            exit for
         end if
         if (sqlca.sqlcode) then
            let class_desc = "UNKNOWN"
         end if

         let val_array[i].class = tmp_class_cd
         let val_array[i].desc1 = class_desc
         let val_array[i].amount = tmp_val_amt

         # Populate PP Values
         let j = 1
         open select_ppval using active_mt_sw, 
                                 char_conv, 
                                 active_year,
                                 active_list_no,
                                 i #txpv_seq
         if (sqlca.sqlcode) then
            call sperrorm("Error opening select_ppval cursor - "||__LINE__, 0, 2)
         else
            while true
               
               # End of Record Check
               if j = 11 then
                  exit while
               end if

               # Gather Data
               fetch select_ppval into tmp_ppval_purch_yr,
                                       tmp_ppval_purch_amt,
                                       tmp_ppval_pct_good,
                                       tmp_ppval_curr_value,
                                       tmp_ppval_acquisitions,
                                       tmp_ppval_removals
               if (sqlca.sqlcode) then
                  if(sqlca.sqlcode != NOTFOUND) then
                     call sperrorm("Error fetching select_ppval data - "||__LINE__, 0, 2)
                  end if
                  exit while
               end if
               
               # Populate pp value array
               let val_array[i].ppval_array[j].ppval_yr = tmp_ppval_purch_yr
               let val_array[i].ppval_array[j].ppval_purch_amt = tmp_ppval_purch_amt
               let val_array[i].ppval_array[j].ppval_pct_good = tmp_ppval_pct_good
               let val_array[i].ppval_array[j].ppval_curr_value = tmp_ppval_curr_value
               let val_array[i].ppval_array[j].ppval_acquisitions = tmp_ppval_acquisitions
               let val_array[i].ppval_array[j].ppval_removals = tmp_ppval_removals

               # Increment
               let j = j + 1
            end while
         end if
      end while
      close select_txval

      call spoffice_addRow()
      let hyperLinkArgs = "I ",
                          er_txppmast.txpp_id_no using "<<<<<<<<<<&", " ",
                          er_txppmast.txpp_year using "&&&&", " ",
                          er_txppmast.txpp_list_no using "&&&&&&&&", " ",
                          er_txppmast.txpp_mt_sw using "&&"
      call spoffice_addLink(spapplnk_getHyperLink("txppmast", hyperLinkArgs))
      call spoffice_addData("PROP ID"        ,  30, "String"  , "NONE"         , "", char_conv)
      call spoffice_addData("YEAR"           ,   4, "Number"  , "DEC0N"        , "", active_year)
      call spoffice_addData("BILL"           ,  10, "Number"  , "DEC0N"        , "", active_list_no)
      call spoffice_addData("PARCEL"         ,   1, "String"  , "NONE"         , "", er_txppmast.txpp_parc_id)
      call spoffice_addData("OLD ID"         ,   1, "String"  , "NONE"         , "", er_txppmast.txpp_old_id)
      call spoffice_addData("STATUS"         ,   1, "String"  , "NONE"         , "", er_txppmast.txpp_status)
      call spoffice_addData("CLASS"          ,   1, "String"  , "NONE"         , "", er_txppmast.txpp_class_cd)
      call spoffice_addData("LOC NO"         ,   1, "String"  , "NONE"         , "", er_txppmast.txpp_loc_no)
      call spoffice_addData("LOC NO SUFF"    ,   1, "String"  , "NONE"         , "", er_txppmast.txpp_loc_no_suff)
      call spoffice_addData("LOC STREET"     ,   1, "String"  , "NONE"         , "", er_txppmast.txpp_loc_street)
      call spoffice_addData("LOC APT"        ,   1, "String"  , "NONE"         , "", er_txppmast.txpp_loc_apt)
      call spoffice_addData("JURIS"          ,   1, "String"  , "NONE"         , "", er_txppmast.txpp_juris_cd)
      call spoffice_addData("SUBDIV"         ,   1, "String"  , "NONE"         , "", er_txppmast.txpp_subdiv)
      call spoffice_addData("DBA"            ,   1, "String"  , "NONE"         , "", er_txppmast.txpp_dba)
      call spoffice_addData("PROPTYPE"       ,   1, "String"  , "NONE"         , "", er_txppmast.txpp_abst_type)
      call spoffice_addData(st_comply_no     ,  10, "Number"  , "DEC0N"        , "", er_txppmast.txpp_comply_no)
      call spoffice_addData(st_comply_dt     ,  10, "DateTime", "DTE_MED"      , "", er_txppmast.txpp_comply_date)
      call spoffice_addData("BUSINESS TYPE"  ,   1, "String"  , "NONE"         , "", er_txppmast.txpp_business)
      call spoffice_addData("BUSINESS DESC"  ,   1, "String"  , "NONE"         , "", lv_customerTypeDescription)
      call spoffice_addData("ACCOUNT"        ,  10, "Number"  , "DEC0N"        , "", lv_customerId)
      call spoffice_addData("NAME1"          ,   1, "String"  , "NONE"         , "", lv_customerAddress.Name1.Full)
      call spoffice_addData("NAME2"          ,   1, "String"  , "NONE"         , "", lv_customerAddress.Name2.Full)
      call spoffice_addData("ADDR1"          ,   1, "String"  , "NONE"         , "", lv_customerAddress.Address.Line1)
      call spoffice_addData("ADDR2"          ,   1, "String"  , "NONE"         , "", lv_customerAddress.Address.Line2)
      call spoffice_addData("CITY"           ,   1, "String"  , "NONE"         , "", lv_customerAddress.Address.City)
      call spoffice_addData("STATE"          ,   1, "String"  , "NONE"         , "", lv_customerAddress.Address.State)
      call spoffice_addData("ZIP"            ,   1, "String"  , "NONE"         , "", lv_customerAddress.Address.ZipCode)
      call spoffice_addData("PERS-ENT"       ,   1, "String"  , "NONE"         , "", IIF(lv_customerAddress.Name1.IsPerson == 0, 'E', 'P')) # 0 = 'E', 1 = 'P'. For some reason CustomerManager.c_PERSONENTITY_Entity is 2, and _Person is 1?
      call spoffice_addData("PRINTAX"        ,  11, "Number"  , "DEC2"         , "", tmp_prin_tax)
      call spoffice_addData("SPEC ASSMT"     ,  11, "Number"  , "DEC2"         , "", tmp_spec_assmt)
      call spoffice_addData("PENALTY"        ,  11, "Number"  , "DEC2"         , "", tmp_penalty)
      call spoffice_addData("TOTAL TAX"      ,  11, "Number"  , "DEC2"         , "", tmp_tot_tax)
      call spoffice_addData("TOTAL VALUE"    ,  11, "Number"  , "DEC0"         , "", tmp_tot_value)
      call spoffice_addData("EXEM VALUE"     ,  11, "Number"  , "DEC0"         , "", tmp_tot_exem)
      call spoffice_addData("NET VALUE"      ,  11, "Number"  , "DEC0"         , "", tmp_net_value)
      
      
      # Charges
      # Prints 6 sets of charge data (Prints 36 rows)
      for i = 1 to 6
         call spoffice_addData("CHARGE"    ||i ,   6, "String"  , "NONE"       , "", chg_array[i].chg)
         call spoffice_addData("DESC"      ||i ,  40, "String"  , "NONE"       , "", chg_array[i].desc1)
         call spoffice_addData("RATE"      ||i ,  11, "Number"  , "DEC6"       , "", chg_array[i].rate)
         call spoffice_addData("ASSMT"     ||i ,  11, "Number"  , "DEC0"       , "", chg_array[i].assmt)
         call spoffice_addData("COUNT"     ||i ,   5, "Number"  , "DEC0"       , "", chg_array[i].cnt)
         call spoffice_addData("AMOUNT"    ||i ,  11, "Number"  , "DEC2"       , "", chg_array[i].amount)
      end for
      
      # Exemptions
      # Prints 3 sets of exemption data (Prints 9 Rows)
      for i = 1 to 3
         call spoffice_addData("EXEM CODE" ||i ,   6, "String"  , "NONE"       , "", exem_array[i].exem)
         call spoffice_addData("EXEM DESC" ||i ,  30, "String"  , "NONE"       , "", exem_array[i].desc1)
         call spoffice_addData("EXEM VALUE"||i ,  10, "Number"  , "DEC2"       , "", exem_array[i].amount)
      end for

      # Values and PP Values
      # Prints 10 sets of value data with each set containing 10 sets of pp value data (Prints 630 Rows)
      for i = 1 to 10 
         call spoffice_addData("VAL CLASS" ||i ,   4, "String"  , "NONE"       , "", val_array[i].class)
         call spoffice_addData("VAL DESC"  ||i ,  30, "String"  , "NONE"       , "", val_array[i].desc1)
         call spoffice_addData("VALUE"     ||i ,  11, "Number"  , "DEC0"       , "", val_array[i].amount)
         for j = 1 to 10 
            call spoffice_addData("PPVAL_YEAR_PURCHASED_"    ||i||"_"||j ,   4, "Number"  , "DEC0N" , "", val_array[i].ppval_array[j].ppval_yr)
            call spoffice_addData("PPVAL_PURCHASED_VALUE"    ||i||"_"||j ,  11, "Number"  , "DEC0"  , "", val_array[i].ppval_array[j].ppval_purch_amt)
            call spoffice_addData("PPVAL_ACQUISITIONS_"      ||i||"_"||j ,  11, "Number"  , "DEC0"  , "", val_array[i].ppval_array[j].ppval_acquisitions)
            call spoffice_addData("PPVAL_REMOVALS_"          ||i||"_"||j ,  11, "Number"  , "DEC0"  , "", val_array[i].ppval_array[j].ppval_removals)
            call spoffice_addData("PPVAL_PERCENT_GOOD_"      ||i||"_"||j ,   4, "Number"  , "DEC0N" , "", val_array[i].ppval_array[j].ppval_pct_good)
            call spoffice_addData("PPVAL_DEPRECIATED_VALUE_" ||i||"_"||j ,  11, "Number"  , "DEC0"  , "", val_array[i].ppval_array[j].ppval_curr_value)
         end for
      end for
   end for

   # Execute the Office Export...
   call spoffice_executeExport() returning success, err_msg
   if (NOT success) then
      call sperrorm(err_msg, 0, 2)
   end if

end function
###############################################################################
function make_working()
   define tmp_success        smallint,
          tmp_err_msg        string,
          tmp_msg            string,
          tmp_ans            string,
          tmp_rec_notfound   smallint,
          tmp_cnt            integer,
          tmp_txparams       record like txparams.*,
          tmp_txyrparm       record like txyrparm.*,
          tmp_to_versions    dynamic array of record
             mt_sw              smallint,
             tax_year           smallint,
             list_no            integer,
             cycle_comm         smallint
                             end record

   let tmp_success = TRUE

   if (tmp_success) then
      if (NOT record_count) then
         let tmp_success = FALSE
         call spnodata()
      end if
   end if

   if (tmp_success) then
      if (gr_txppmast.txpp_mt_sw != 0) then
         let tmp_success = FALSE
         call sperrorm("Working records can only be created from current records.", 0, 2)
      end if
   end if

   if (tmp_success) then
      if (gr_txppmast.txpp_status = "I") then
         let tmp_success = FALSE
         call sperrorm("Property is inactive.", 0, 2)
      end if
   end if

   if (tmp_success) then
      call txparamsio_select("P") returning tmp_success, tmp_err_msg, tmp_rec_notfound, tmp_txparams.*
      if (tmp_rec_notfound) then
         let tmp_success = FALSE
         let tmp_err_msg = "Tax settings not found."
      end if
      if (NOT tmp_success) then
         call sperrorm(tmp_err_msg, 0, 2)
      else
         if (tmp_txparams.txpm_act_pp_year = 0) then
            let tmp_success = FALSE
            call sperrorm("An active personal property bill run does not exist at this time.", 0, 2)
         end if
      end if
   end if

   if (tmp_success) then
      call txyrparmio_select(tmp_txparams.txpm_act_pp_year, 25, tmp_txparams.txpm_act_pp_cycle)
                   returning tmp_success, tmp_err_msg, tmp_rec_notfound, tmp_txyrparm.*
      if (tmp_rec_notfound) then
         let tmp_success = FALSE
         let tmp_err_msg = "Tax year settings not found."
      end if
      if (NOT tmp_success) then
         call sperrorm(tmp_err_msg, 0, 2)
      else
         if (tmp_txyrparm.txyp_proc_stat <= 0 or tmp_txyrparm.txyp_proc_stat >= 7) then
            let tmp_success = FALSE
            call sperrorm("The process status will not allow for working records to be created at this time.", 0, 2)
         end if
      end if
   end if

   if (tmp_success) then
      let tmp_cnt = 0
      select count(*) into tmp_cnt from txppmast
       where txpp_mt_sw = 9
         and txpp_id_no = gr_txppmast.txpp_id_no
         and txpp_year = tmp_txyrparm.txyp_year
      if (sqlca.sqlcode) then
         let tmp_success = FALSE
         call spsyserr("Error selecting from txppmast.",
                       "Select txppmast 814")
      end if
      if (tmp_cnt is NULL) then
         let tmp_cnt = 0
      end if
      if (tmp_cnt > 0) then
         let tmp_success = FALSE
         call sperrorm("This property already exists as a working record.", 0, 2)
      end if
   end if

   if (tmp_success) then
      let tmp_msg = "Proceed to create a ", tmp_txyrparm.txyp_year using "<<<&",
                    " working record for property ", gr_txppmast.txpp_id_no using "<<<<<<<<<&", "?"
      call spaskrtn_dialog("Make Working", tmp_msg, "Yes|No", "question", "Yes")
                 returning tmp_ans
      if (tmp_ans != "Yes") then
         let tmp_success = FALSE
      end if
   end if

   if (tmp_success) then
      call tmp_to_versions.clear()
      let tmp_to_versions[1].mt_sw = 9
      let tmp_to_versions[1].tax_year = tmp_txyrparm.txyp_year
      let tmp_to_versions[1].list_no = 0
      let tmp_to_versions[1].cycle_comm = tmp_txyrparm.txyp_cycle_comm

      call txmigratbu_migrateProperty(gr_txppmast.txpp_mt_sw, 25,
                                      gr_txppmast.txpp_id_no,
                                      gr_txppmast.txpp_year,
                                      gr_txppmast.txpp_list_no,
                                      tmp_to_versions, "C", "A",
                                      TRUE, TRUE)
                            returning tmp_success, tmp_err_msg
      if (NOT tmp_success) then
         call sperrorm(tmp_err_msg, 0, 2)
      end if
   end if

   if (tmp_success) then
      call sperrorm_message("Working record created successfully.")
   end if

end function
###############################################################################
function exchange_routine()
   define exchg_err_msg   string,
          rec_index       string,
          hyperLinkArgs   string,
          hyperLink       string,
          hyperLinkText   string,
          out_idle        boolean,
          exchg_success   boolean

   let exchg_err_msg = ""
   let out_idle = FALSE


   # Set a unique index for use within Exchange...
   let rec_index = active_id_no using "<<<<<<<<<<&", " ",
                   active_year using "&&&&", " ",
                   active_list_no using "&&&&&&&&", " ",
                   active_mt_sw using "&&"

   # Set the hyper-link back to the MUNIS record...
   let hyperLinkArgs = "I ", active_id_no using "<<<<<<<<<<&", " ",
                             active_year using "&&&&", " ",
                             active_list_no using "&&&&&&&&", " ",
                             active_mt_sw using "&&"

   let hyperLink = spapplnk_getHyperlink("txppmast", hyperLinkArgs)

   # Set the text to display in the hyper-link...
   let hyperLinkText = "Personal Property Inquiry -\n",
                       "   Personal property id: ", active_id_no using "<<<<<<<<<<&", "\n",
                       "   Year:                 ", active_year using "&&&&", "\n",
                       "   Bill number:          ", active_list_no using "<<<<<<<<<<&", "\n"

   # Call the central routine to maintain exchange appointments...
   call spexcmnt_maintainAppBrief("Personal Property Inquiry",         # Window title
                                  "txppmast",                          # Tyler Type
                                  rec_index,                           # Tyler ID
                                  ST_USER,                             # User
                                  "Personal Property Inquiry",         # Default subject
                                  hyperLinkText,                       # Default description
                                  hyperLink)                           # Include hyper-link
      returning exchg_success, exchg_err_msg, out_idle

   # Provide feedback to the user...
   if (NOT exchg_success) then
      call sperrorm(exchg_err_msg, 0, 1)
   end if

   return out_idle

end function
##############################################################################
function email_routine()
   define hyperLinkArgs                string,
          hyperLinkText                string

   # Set the command line argument string...
   let hyperLinkArgs = "I ", active_id_no using "<<<<<<<<<<&", " ",
                             active_year using "&&&&", " ",
                             active_list_no using "&&&&&&&&", " ",
                             active_mt_sw using "&&"

   # Set the text to display in the hyper-link...
   let hyperLinkText = "Personal Property Inquiry -\n",
                       "   Personal property id: ", active_id_no using "<<<<<<<<<<&", "\n",
                       "   Year:                 ", active_year using "&&&&", "\n",
                       "   Bill number:          ", active_list_no using "<<<<<<<<<<&", "\n"

   # Send the email...
   call spapplnk_execMailto("", hyperLinkArgs, hyperLinkText)

end function
###############################################################################
function special_condition()
   define tmp_count          smallint,
          x                  smallint,
          tmp_spcond_found   smallint,
          tmp_spcond_found1  smallint,
          tmp_success        smallint,
          tmp_rec_notfound   smallint,
          tmp_err_msg        string,
          tmp_spspccom       record like spspccom.*,
          tmp_spspccom_array dynamic array of record like spspccom.*,
          tmp_txowners       record like txowners.*,
          tmp_txcsttag       record like txcsttag.*

   call spmdebug_say("TXPPMAST: special_condition: Begin: ", current hour to fraction)

   # Initialize local variable(s)...
   let tmp_count = 1
   let tmp_spcond_found = 0
   let tmp_spcond_found1 = 0
   let x = 1

   # Initialize local temp table(s)...
   initialize tmp_spspccom.* to NULL
   initialize tmp_txowners.* to NULL
   initialize tmp_txcsttag.* to NULL

   # Initialize local temp array(s)...
   call tmp_spspccom_array.clear()

   # Current record...
   if (ct_sw = "C") then
      # Select the primary owner...
      call spmdebug_say("TXPPMAST: special_condition: gr_txppmast.txpp_id_no: ", gr_txppmast.txpp_id_no)
      call txcsttagwr_selectPrimaryOwner(25, gr_txppmast.txpp_id_no)
                               returning tmp_success, tmp_err_msg, tmp_rec_notfound, tmp_txcsttag.*
      if (NOT tmp_success) then
         call spsyserr(tmp_err_msg, "txcsttagwr_selectForPropertry 8243")
      else
         # Check to see if a primary owner exists...
         if (tmp_rec_notfound) then
            let tmp_err_msg = "No primary owner record found for property: ", gr_txppmast.txpp_id_no using "<<<<<<<<<&", "."
            call sperrorm(tmp_err_msg, 0, 1)
         end if
      end if

      # Check if a special condition exists for CID...
      if (tmp_txcsttag.txtg_acct != 0) then
         # Update tmp_spspccom table with special condition data using tmp_acct...
         open sp_cursor1 using tmp_txcsttag.txtg_acct
         while (TRUE)
            fetch sp_cursor1 into tmp_spspccom.*
            if (sqlca.sqlcode) then
               exit while
            end if

            # Check to see if any records exist in tmp_spspccom_array...
            if (tmp_spspccom_array.getLength()) then
               # Loop through all records in the array...
               for x = 1 to tmp_spspccom_array.getLength()
                  # Skip record if the special condition record already exists...
                  if (tmp_spspccom_array[x].spcc_serial = tmp_spspccom.spcc_serial) then
                     continue while
                  end if
               end for
            end if

            # Skip record if user is NOT allowed to view notes and a special condition is nonexistent...
            if (glob_spec_view_notes = "N" and length(tmp_spspccom.spcc_sp_cond = 0)) then
               continue while
            end if

            # Skip record if start date has yet to come or if the end date has surpassed...
            if (tmp_spspccom.spcc_start_date > ST_TODAY or tmp_spspccom.spcc_end_date < ST_TODAY) then
               continue while
            end if

            # Add record to tmp_spspccom_array...
            let tmp_spspccom_array[tmp_count].* = tmp_spspccom.*
            let tmp_count = tmp_count + 1
         end while
         close sp_cursor1
      end if
      let tmp_spspccom.* = nl_spspccom.*

      # Check if a special condition exists for tmp_txcsttag.txtg_prop_id...
      if (length(tmp_txcsttag.txtg_prop_id) > 0) then
         # Update tmp_spspccom table with special condition data using tmp_txcsttag.txtg_prop_id...
         open sp_cursor3 using tmp_txcsttag.txtg_ar_cat,
                               tmp_txcsttag.txtg_prop_id
         while (TRUE)
            fetch sp_cursor3 into tmp_spspccom.*
            if (sqlca.sqlcode) then
               exit while
            end if

            # Check to see if any records exist in tmp_spspccom_array...
            if (tmp_spspccom_array.getLength()) then
               # Loop through all records in the array...
               for x = 1 to tmp_spspccom_array.getLength()
                  # Skip record if the special condition record already exists...
                  if (tmp_spspccom_array[x].spcc_serial = tmp_spspccom.spcc_serial) then
                     continue while
                  end if
               end for
            end if

            # Skip record if user is NOT allowed to view notes and a special condition is nonexistent...
            if (glob_spec_view_notes = "N" and length(tmp_spspccom.spcc_sp_cond = 0)) then
               continue while
            end if

            # Skip record if start date has yet to come or if the end date has surpassed...
            if (tmp_spspccom.spcc_start_date > ST_TODAY or tmp_spspccom.spcc_end_date < ST_TODAY) then
               continue while
            end if

            # Use tmp_spspccom table to update tmp_spspccom_array...
            let tmp_spspccom_array[tmp_count].* = tmp_spspccom.*
            let tmp_count = tmp_count + 1
         end while
         close sp_cursor3
      end if
      let tmp_spspccom.* = nl_spspccom.*

#      # Check if a special condition exists for tmp_txcsttag.txtg_prop_id...
#      if (length(tmp_txcsttag.txtg_prop_id) > 0) then
#         # Update tmp_spspccom table with special condition data using tmp_txcsttag.txtg_prop_id...
#         open sp_cursor4 using tmp_txcsttag.txtg_prop_id
#         while (TRUE)
#            fetch sp_cursor4 into tmp_spspccom.*
#            if (sqlca.sqlcode) then
#               exit while
#            end if
#
#            # Check to see if any records exist in tmp_spspccom_array...
#            if (tmp_spspccom_array.getLength()) then
#               # Loop through all records in the array...
#               for x = 1 to tmp_spspccom_array.getLength()
#                  # Skip record if the special condition record already exists...
#                  if (tmp_spspccom_array[x].spcc_serial = tmp_spspccom.spcc_serial) then
#                     continue while
#                  end if
#               end for
#            end if
#
#            # Skip record if user is NOT allowed to view notes and a special condition is nonexistent...
#            if (glob_spec_view_notes = "N" and length(tmp_spspccom.spcc_sp_cond = 0)) then
#               continue while
#            end if
#
#            # Skip record if start date has yet to come or if the end date has surpassed...
#            if (tmp_spspccom.spcc_start_date > ST_TODAY or tmp_spspccom.spcc_end_date < ST_TODAY) then
#               continue while
#            end if
#
#            # Use tmp_spspccom table to update tmp_spspccom_array...
#            let tmp_spspccom_array[tmp_count].* = tmp_spspccom.*
#            let tmp_count = tmp_count + 1
#         end while
#         close sp_cursor4
#      end if
#      let tmp_spspccom.* = nl_spspccom.*

      let tmp_spcond_found = tmp_spspccom_array.getLength()

      if (tmp_spcond_found and disp_special) then
         call txspcondbu_auto_disp_spccond(tmp_txcsttag.txtg_acct, 0,
                                           tmp_txcsttag.txtg_ar_cat, 0,
                                           tmp_txcsttag.txtg_prop_id, ' ',
                                           pass_let)
                                 returning tmp_spcond_found1
         let disp_special = FALSE
         call spmdebug_say("TXPPMAST: special_condition: tmp_spcond_found1 = ", tmp_spcond_found1)
         call spdomlib_setBtnDataImage("note_sp_r_btn", tmp_spcond_found1)
      else
         call spmdebug_say("TXPPMAST: special_condition: tmp_spcond_found = ", tmp_spcond_found)
         call spdomlib_setBtnDataImage("note_sp_r_btn", tmp_spcond_found)
      end if
   end if

   # Tax record...
   if (ct_sw = "T") then
      # Select the primary owner...
      call spmdebug_say("TXPPMAST: special_condition: gr_txppmast.txpp_mt_sw: ", gr_txppmast.txpp_mt_sw)
      call spmdebug_say("TXPPMAST: special_condition: gr_txppmast.txpp_id_no: ", gr_txppmast.txpp_id_no)
      call spmdebug_say("TXPPMAST: special_condition: gr_txppmast.txpp_year: ", gr_txppmast.txpp_year)
      call spmdebug_say("TXPPMAST: special_condition: gr_txppmast.txpp_list_no: ", gr_txppmast.txpp_list_no)
      call txownerswr_selectPrimaryOwner(gr_txppmast.txpp_mt_sw,
                                         25,
                                         gr_txppmast.txpp_id_no,
                                         gr_txppmast.txpp_year,
                                         gr_txppmast.txpp_list_no)
                               returning tmp_success, tmp_err_msg, tmp_rec_notfound, tmp_txowners.*
      if (NOT tmp_success) then
         call spsyserr(tmp_err_msg, "txownerswr_selectForParent 1620")
      else
         # Check to see if a primary owner exists...
         if (tmp_rec_notfound) then
            let tmp_err_msg = "No primary owner record found for property: ", gr_txppmast.txpp_id_no using "<<<<<<<<<&", "."
            call sperrorm(tmp_err_msg, 0, 1)
         end if
      end if

      # Check if a special condition exists for CID...
      if (tmp_txowners.txon_acct != 0) then
         # Update tmp_spspccom table with special condition data using tmp_acct...
         open sp_cursor1 using tmp_txowners.txon_acct
         while (TRUE)
            fetch sp_cursor1 into tmp_spspccom.*
            if (sqlca.sqlcode) then
               exit while
            end if

            # Check to see if any records exist in tmp_spspccom_array...
            if (tmp_spspccom_array.getLength()) then
               # Loop through all records in the array...
               for x = 1 to tmp_spspccom_array.getLength()
                  # Skip record if the special condition record already exists...
                  if (tmp_spspccom_array[x].spcc_serial = tmp_spspccom.spcc_serial) then
                     continue while
                  end if
               end for
            end if

            # Skip record if user is NOT allowed to view notes and a special condition is nonexistent...
            if (glob_spec_view_notes = "N" and length(tmp_spspccom.spcc_sp_cond = 0)) then
               continue while
            end if

            # Skip record if start date has yet to come or if the end date has surpassed...
            if (tmp_spspccom.spcc_start_date > ST_TODAY or tmp_spspccom.spcc_end_date < ST_TODAY) then
               continue while
            end if

            # Add record to tmp_spspccom_array...
            let tmp_spspccom_array[tmp_count].* = tmp_spspccom.*
            let tmp_count = tmp_count + 1
         end while
         close sp_cursor1
      end if
      let tmp_spspccom.* = nl_spspccom.*

      # Check if a special condition exists for tmp_txowners.txon_list_no...
      if (tmp_txowners.txon_list_no != 0) then
         # Update tmp_spspccom table with special condition data using tmp_txowners.txon_list_no...
         open sp_cursor2 using tmp_txowners.txon_ar_cat,
                               tmp_txowners.txon_year,
                               tmp_txowners.txon_list_no
         while (TRUE)
            fetch sp_cursor2 into tmp_spspccom.*
            if (sqlca.sqlcode) then
               exit while
            end if

            # Check to see if any records exist in tmp_spspccom_array...
            if (tmp_spspccom_array.getLength()) then
               # Loop through all records in the array...
               for x = 1 to tmp_spspccom_array.getLength()
                  # Skip record if the special condition record already exists...
                  if (tmp_spspccom_array[x].spcc_serial = tmp_spspccom.spcc_serial) then
                     continue while
                  end if
               end for
            end if

            # Skip record if user is NOT allowed to view notes and a special condition is nonexistent...
            if (glob_spec_view_notes = "N" and length(tmp_spspccom.spcc_sp_cond = 0)) then
               continue while
            end if

            # Skip record if start date has yet to come or if the end date has surpassed...
            if (tmp_spspccom.spcc_start_date > ST_TODAY or tmp_spspccom.spcc_end_date < ST_TODAY) then
               continue while
            end if

            # Use tmp_spspccom table to update tmp_spspccom_array...
            let tmp_spspccom_array[tmp_count].* = tmp_spspccom.*
            let tmp_count = tmp_count + 1
         end while
         close sp_cursor2
      end if
      let tmp_spspccom.* = nl_spspccom.*

      # Check if a special condition exists for tmp_txowners.txon_prop_id...
      if (length(tmp_txowners.txon_prop_id) > 0) then
         # Update tmp_spspccom table with special condition data using tmp_txowners.txon_prop_id...
         open sp_cursor3 using tmp_txowners.txon_ar_cat,
                               tmp_txowners.txon_prop_id
         while (TRUE)
            fetch sp_cursor3 into tmp_spspccom.*
            if (sqlca.sqlcode) then
               exit while
            end if

            # Check to see if any records exist in tmp_spspccom_array...
            if (tmp_spspccom_array.getLength()) then
               # Loop through all records in the array...
               for x = 1 to tmp_spspccom_array.getLength()
                  # Skip record if the special condition record already exists...
                  if (tmp_spspccom_array[x].spcc_serial = tmp_spspccom.spcc_serial) then
                     continue while
                  end if
               end for
            end if

            # Skip record if user is NOT allowed to view notes and a special condition is nonexistent...
            if (glob_spec_view_notes = "N" and length(tmp_spspccom.spcc_sp_cond = 0)) then
               continue while
            end if

            # Skip record if start date has yet to come or if the end date has surpassed...
            if (tmp_spspccom.spcc_start_date > ST_TODAY or tmp_spspccom.spcc_end_date < ST_TODAY) then
               continue while
            end if

            # Use tmp_spspccom table to update tmp_spspccom_array...
            let tmp_spspccom_array[tmp_count].* = tmp_spspccom.*
            let tmp_count = tmp_count + 1
         end while
         close sp_cursor3
      end if
      let tmp_spspccom.* = nl_spspccom.*

#      # Check if a special condition exists for tmp_txowners.txon_prop_id...
#      if (length(tmp_txowners.txon_prop_id) > 0) then
#         # Update tmp_spspccom table with special condition data using tmp_txowners.txon_prop_id...
#         open sp_cursor4 using tmp_txowners.txon_prop_id
#         while (TRUE)
#            fetch sp_cursor4 into tmp_spspccom.*
#            if (sqlca.sqlcode) then
#               exit while
#            end if
#
#            # Check to see if any records exist in tmp_spspccom_array...
#            if (tmp_spspccom_array.getLength()) then
#               # Loop through all records in the array...
#               for x = 1 to tmp_spspccom_array.getLength()
#                  # Skip record if the special condition record already exists...
#                  if (tmp_spspccom_array[x].spcc_serial = tmp_spspccom.spcc_serial) then
#                     continue while
#                  end if
#               end for
#            end if
#
#            # Skip record if user is NOT allowed to view notes and a special condition is nonexistent...
#            if (glob_spec_view_notes = "N" and length(tmp_spspccom.spcc_sp_cond = 0)) then
#               continue while
#            end if
#
#            # Skip record if start date has yet to come or if the end date has surpassed...
#            if (tmp_spspccom.spcc_start_date > ST_TODAY or tmp_spspccom.spcc_end_date < ST_TODAY) then
#               continue while
#            end if
#
#            # Use tmp_spspccom table to update tmp_spspccom_array...
#            let tmp_spspccom_array[tmp_count].* = tmp_spspccom.*
#            let tmp_count = tmp_count + 1
#         end while
#         close sp_cursor4
#      end if
#      let tmp_spspccom.* = nl_spspccom.*

      let tmp_spcond_found = tmp_spspccom_array.getLength()

      if (tmp_spcond_found and disp_special) then
         call txspcondbu_auto_disp_spccond(tmp_txowners.txon_acct,
                                           tmp_txowners.txon_ar_cat,
                                           tmp_txowners.txon_year,
                                           tmp_txowners.txon_list_no,
                                           tmp_txowners.txon_prop_id, ' ',
                                           pass_let)
                                 returning tmp_spcond_found1
         let disp_special = FALSE
         call spmdebug_say("TXPPMAST: special_condition: tmp_spcond_found1 = ", tmp_spcond_found1)
         call spdomlib_setBtnDataImage("note_sp_r_btn", tmp_spcond_found1)
      else
         call spmdebug_say("TXPPMAST: special_condition: tmp_spcond_found = ", tmp_spcond_found)
         call spdomlib_setBtnDataImage("note_sp_r_btn", tmp_spcond_found)
      end if
   end if

   call spmdebug_say("TXPPMAST: special_condition: Complete: ", current hour to fraction)

end function
###############################################################################
#Tyler Notify
#   New routine to submit notifications to Tyler Notify, it does the following.
#   1. Walks through the tmpcutof table to populate the notifyRecordArray.
#   2. Calls spnotlib_notifyDialog to allow the user to select a notification template and filter notification methods (Phone, Email, etc).
#   3. Upon a successful return from the dialog, processes the selected notification records to generate history records and update the record key.
#   4. Calls spnotlib_submitNotifications to submit the array of notifications to Tyler Notify.
###############################################################################
function notify_routine(ui_mode)
   define retmsg,
          ui_mode                      string,
          success                      boolean,
          tmp_rec_notfound             smallint,
          firstrec,
          lastrec,
          prfetchno,
          retval                       integer,
          notify_active_id_no          like txppmast.txpp_id_no,
          notify_active_list_no        like txppmast.txpp_list_no,
          notify_active_mt_sw          like txppmast.txpp_mt_sw,
          notify_active_year           like txppmast.txpp_year,
          submitReturns                TylerNotifySubmitNotificationsReturnValues

   let success = true
   call notifyRecordArray.clear()
   call notifyFetchArray.clear()
   let prfetchno = 0
   let firstrec = 1
   let lastrec  = record_count

   if (success) then
      call spmdebug_say("TXPPMAST: notify_routine: firstrec: ", firstrec)
      call spmdebug_say("TXPPMAST: notify_routine: lastrec: ", lastrec)
      for prfetchno = firstrec to lastrec
         fetch absolute prfetchno find_cursor into notify_active_mt_sw,
                                                   notify_active_id_no,
                                                   notify_active_year,
                                                   notify_active_list_no
         call spmdebug_say("TXPPMAST: notify_routine: sqlca.sqlcode: ", sqlca.sqlcode)
         if (sqlca.sqlcode = -213) then
            let success = FALSE
            exit for
         end if
         if (sqlca.sqlcode != 0) then
            continue for
         end if

# Select the associated property record...
         call txppmastio_select(notify_active_mt_sw, notify_active_id_no, notify_active_year, notify_active_list_no)
                      returning success, notify_err_msg, tmp_rec_notfound, notify_txppmast.*
         if (tmp_rec_notfound) then
            let notify_err_msg = "Property record not found for property: ", notify_active_id_no clipped, "."
            let success = FALSE
         end if
         if (NOT success) then
            call sperrorm(notify_err_msg, 0, 2)
            exit for
         end if

         call spmdebug_say("TXPPMAST: notify_routine: call: ", "write_notify_record")
         call write_notify_record(prfetchno, ui_mode)
            returning retval,
                      retmsg
         if (retval != 0) then
            let success = false
            exit for
         end if
      end for
   end if

   if (success) then
      if (ui_mode == "menu") then
         let g_notifyParams.dialogMode = TylerNotifyDialogModeFull
         call spnotlib_notifyDialog(g_notifyParams.*, notifyRecordArray)
            returning g_notifyReturns.*
         if (g_notifyReturns.errorCode != 0) then
            let success = false
            if (length(g_notifyReturns.errorMessage) > 0) then
               call sperrorm(g_notifyReturns.errorMessage, 0, 2)
            end if
         end if
      end if
   end if

   call spmdebug_say("TXPPMAST: notify_routine: success: ", success)
   if (success) then
#Tyler Notify
#     User has accepted the notifications to be submitted and history records have been generated.
#     Submit the notifications array to Tyler Notify using spnotlib_submitNotifications. Only
#     "Selected" records will be submitted. The return record from spnotlib_notifyDialog contains
#     the submit parameter record with the values preset.
      call spnotlib_submitNotifications(g_notifyReturns.submitParameters.*, notifyRecordArray)
         returning submitReturns.*
      call spmdebug_say("TXPPMAST: notify_routine: submitReturns.errorCode: ", submitReturns.errorCode)
      if (submitReturns.errorCode == 0) then
         let g_notify_complete = "Y"
         if (spaskrtn_dialog("Notifications Submitted",
                             "Records successfully submitted for notification.",
                             "OK",
                             "info",
                             "OK")) then
         end if
      else
         call sperrorm(submitReturns.errorMessage, 0, 2)
      end if
   end if

#Tyler Notify
#  Saving notify options for next run
   if (success and
       ui_mode == "menu") then
      if (NOT spnotlib_saveNotifySettings(g_notifyReturns.*, " ", "utcutoff", "notify")) then
      end if
   end if

end function
###############################################################################
function write_notify_record(fetchno, ui_mode)

   define
      fetchno              integer,
      ui_mode              string

   define
      indx                 integer,
      jndx                 integer,
      notifyRecord         TylerNotifyRecord,
      buildParams          TylerNotifyBuildMunisContactsParameters,
      retRecordArray       dynamic array of TylerNotifyRecord,
      formatted_addr1      string,
      formatted_addr2      string,
      formatted_city       string,
      formatted_state      string,
      formatted_zip        string
   define
      errorStatus          integer,
      errorMessage         string,
      lv_customerDeliveryAddressStatusActive boolean

   define lv_customerTypeCode char(8),
          lv_customerTypeShortDescription char(10),
          lv_customerTypeDescription char(40)

   # Initialize local variable(s)...
   let errorStatus = 0
   let lv_customerTypeCode = " "
   let lv_customerTypeShortDescription = " "

   let char_id_no = notify_txppmast.txpp_id_no using "<<<<<<<<<<"
   call spmdebug_say("TXPPMAST: write_notify_record: char_id_no: ", char_id_no)
   call get_customer(char_id_no, notify_active_year, notify_active_list_no) returning lv_customerDeliveryAddressStatusActive
   call spmdebug_say("TXPPMAST: write_notify_record: cust_city: ", cust_city)

   let formatted_addr1 = cust_addr
   let formatted_addr2 = cust_addr2
   let formatted_city = cust_city
   let formatted_state = cust_state
   let formatted_zip = cust_zip

#Tyler Notify
#     Write the base notifyRecord for each record in the set, then use spnotlib_buildMunisContacts
#     to generate a notification record for each contact type available to a Munis entity (currently
#     for MunisCustomer, but MunisEmployee and MunisUser to be added in the future).
#     spnotlib_buildMunisContacts returns an array of notification records that must be added to
#     module-scoped array. Each call to spnotlib_buildMunisContacts must be preceded by a call to
#     spnotlib_initializeBuildMunisContactsParameters.
#
#     NOTE: Selected flag must be set to true to ensure the record is submitted to Tyler Notify.
#     The flag may be set to false by the spnotlib_notifyDialog.
   let notifyRecord.RecordKey = notify_txppmast.txpp_id_no
   let notifyRecord.LanguagePreference = TylerNotifyLanguagePreferenceUnknown
   let notifyRecord.ServAddress = formatted_addr1
   let notifyRecord.ServAddress2 = formatted_addr2
   let notifyRecord.ServCity = formatted_city
   let notifyRecord.ServState = formatted_state
   let notifyRecord.ServZip = formatted_zip
   let notifyRecord.AmountDue = 0
   let notifyRecord.Selected = true
   let notifyRecord.DetailFields[1].fieldname = "id_no"
   let notifyRecord.DetailFields[1].fieldValue = notify_txppmast.txpp_id_no

   let notifyRecord.DetailFields[2].fieldname = "year"
   let notifyRecord.DetailFields[2].fieldValue = notify_txppmast.txpp_year

   let notifyRecord.DetailFields[3].fieldname = "list_no"
   let notifyRecord.DetailFields[3].fieldValue = notify_txppmast.txpp_list_no

   let notifyRecord.DetailFields[4].fieldname = "parc_id"
   let notifyRecord.DetailFields[4].fieldValue = notify_txppmast.txpp_parc_id

   let notifyRecord.DetailFields[5].fieldname = "status"
   case
      when notify_txppmast.txpp_status = "A"
          let notifyRecord.DetailFields[5].fieldValue = "Active"
      when notify_txppmast.txpp_status = "I"
          let notifyRecord.DetailFields[5].fieldValue = "Inactive"
      when notify_txppmast.txpp_status = "E"
          let notifyRecord.DetailFields[5].fieldValue = "Exempt"
      when notify_txppmast.txpp_status = "H"
          let notifyRecord.DetailFields[5].fieldValue = "Hold"
   end case

   let notifyRecord.DetailFields[6].fieldname = "class_cd"
   select txcl_desc into class_desc from txclascd
      where txcl_class = notify_txppmast.txpp_class_cd
        and txcl_ar_cat = 25
        and txcl_lnpg_sw = "G"
   let notifyRecord.DetailFields[6].fieldValue = class_desc

   let notifyRecord.DetailFields[7].fieldname = "loc_no"
   let notifyRecord.DetailFields[7].fieldValue = notify_txppmast.txpp_loc_no

   let notifyRecord.DetailFields[8].fieldname = "loc_no_suff"
   let notifyRecord.DetailFields[8].fieldValue = notify_txppmast.txpp_loc_no_suff

   let notifyRecord.DetailFields[9].fieldname = "loc_street"
   let notifyRecord.DetailFields[9].fieldValue = notify_txppmast.txpp_loc_street

   let notifyRecord.DetailFields[10].fieldname = "loc_apt"
   let notifyRecord.DetailFields[10].fieldValue = notify_txppmast.txpp_loc_apt

   let notifyRecord.DetailFields[11].fieldname = "juris_cd"
   select txju_desc into jur_desc from txjurisd
      where txju_code = notify_txppmast.txpp_juris_cd
   if sqlca.sqlcode != 0 then
      let jur_desc = "UNKNOWN"
   end if
   let notifyRecord.DetailFields[11].fieldValue = jur_desc

   let notifyRecord.DetailFields[12].fieldname = "subdiv"
   select txsd_desc into sub_desc from txsubdiv
      where txsd_code = notify_txppmast.txpp_subdiv
   if sqlca.sqlcode != 0 then
      let sub_desc = "UNKNOWN"
   end if
   let notifyRecord.DetailFields[12].fieldValue = sub_desc

   let notifyRecord.DetailFields[13].fieldname = "business"
   call CustomerManager.getLegacyCustomerTypeForId(notify_txppmast.txpp_business)
      returning lv_customerTypeCode, lv_customerTypeShortDescription, lv_customerTypeDescription
   if (ExceptionState.hasException()) then
      if (NOT ExceptionState.HasNotFoundSQLException()) then
         call ExceptionHandler.processException(ExceptionState.getException(),
                                                ExceptionHandler.c_WRITESYSTEMLOG + ExceptionHandler.c_DISPLAYERRORLINE,
                                                "Error retrieving customer type information.")
      end if
      let lv_customerTypeDescription = " "
   end if
   let notifyRecord.DetailFields[13].fieldValue = lv_customerTypeDescription

   let notifyRecord.DetailFields[14].fieldname = "dba"
   let notifyRecord.DetailFields[14].fieldValue = notify_txppmast.txpp_dba

   let notifyRecord.DetailFields[15].fieldname = "purch_date"
   let notifyRecord.DetailFields[15].fieldValue = notify_txppmast.txpp_purch_date

   let notifyRecord.DetailFields[16].fieldname = "assmt_ly"
   let notifyRecord.DetailFields[16].fieldValue = notify_txppmast.txpp_assmt_ly

   let notifyRecord.DetailFields[17].fieldname = "comply_date"
   let notifyRecord.DetailFields[17].fieldValue = notify_txppmast.txpp_comply_date

   let notifyRecord.DetailFields[18].fieldname = "comply_no"
   let notifyRecord.DetailFields[18].fieldValue = notify_txppmast.txpp_comply_no

   let notifyRecord.DetailFields[19].fieldname = "comply_id"
   let notifyRecord.DetailFields[19].fieldValue = notify_txppmast.txpp_comply_id
#Tyler Notify
#     Use spnotlib_buildMunisContacts to generate a notification record for each contact type
#     available to a Munis entity (currently for MunisCustomer, but MunisEmployee and MunisUser
#     to be added in the future).  spnotlib_buildMunisContacts returns an array of notification
#     records that must be added to module-scoped array. Each call to spnotlib_buildMunisContacts
#     must be preceded by a call to spnotlib_initializeBuildMunisContactsParameters.
   call spnotlib_initializeBuildMunisContactsParameters(buildParams.*)
   returning buildParams.*

   let buildParams.munisRecordType = "MunisCustomer"
   let buildParams.munisRecordID = cust_acct
   if (ui_mode == "menu") then
      let buildParams.retrieveAutomatic = true
   #else
   #  TylerNotifyInterface had a change with the retreiveOptions and includeOptions fields.
   #  It seems that since txremast and txppmast only use a ui_mode of menu that this should not be needed.
   #
   #   let buildParams.retrieveAutomatic = false
   #   let buildParams.retrieveOptions.* = g_notifyReturns.includeOptions.*
   end if
   let buildParams.setName = true
   call spnotlib_buildMunisContacts(buildParams.*, notifyRecord.*)
      returning
         retRecordArray,
         errorStatus,
         errorMessage
   if (errorStatus == 0) then
      for jndx = 1 to retRecordArray.getLength()
         let indx = notifyRecordArray.getLength() + 1
#Tyler Notify
# spnotlib_appendNotificationsRecord provided to add records to the end of the array
# This is provided as a work-around to a FourJs bug. Simply assigning records with
# sub-arrays causes an issue.
         call spnotlib_appendNotificationRecord(notifyRecordArray, retRecordArray[jndx].*)
         let notifyFetchArray[indx] = fetchno
      end for
   end if

   return
      errorStatus,
      errorMessage

end function
###############################################################################
function outputXML_main_menu()
   define tmp_err_msg string

   call spmdebug_say("TXPPMAST: outputXML_main_menu: BEGIN: ", current hour to fraction)

# Initialize local variable(s)...
   let tmp_err_msg = " "

# Initialize global variable(s)...
   let define_done = FALSE

   let whdl_xml = spsetwin_new("txremast15", "XML Output", "appsub", "")

   let file_form = TRUE
   let file_name = ".xml"
   let file_attr = "N"
   display by name file_name,
                   file_form,
                   file_attr

   menu ""

      command "Define" "Define the XML output settings."
         call sperrorm_blankmess()
         call outputXML_define_routine()

      command "Export" "Export XML file."
         call sperrorm_blankmess()
         if (define_done) then
            call outputXML_export_routine()
         else
            let tmp_err_msg = "A successful DEFINE must be executed prior to EXPORT."
            call sperrorm(tmp_err_msg, 0, 1)
         end if

      on action print
         call sperrorm_blankmess()
         if (define_done) then
            call outputXML_print_routine(1025)
         else
            let tmp_err_msg = "A successful DEFINE must be executed prior to output."
            call sperrorm(tmp_err_msg, 0, 1)
         end if

      on action preview
         call sperrorm_blankmess()
         if (define_done) then
            call outputXML_print_routine(1026)
         else
            let tmp_err_msg = "A successful DEFINE must be executed prior to output."
            call sperrorm(tmp_err_msg, 0, 1)
         end if

      on action spool
         call sperrorm_blankmess()
         if (define_done) then
            call outputXML_print_routine(1028)
         else
            let tmp_err_msg = "A successful DEFINE must be executed prior to output."
            call sperrorm(tmp_err_msg, 0, 1)
         end if

      on action pdf
         call sperrorm_blankmess()
         if (define_done) then
            call outputXML_print_routine(1029)
         else
            let tmp_err_msg = "A successful DEFINE must be executed prior to output."
            call sperrorm(tmp_err_msg, 0, 1)
         end if

      on action output
         call sperrorm_blankmess()
         if (define_done) then
            call outputXML_print_routine(31)
         else
            let tmp_err_msg = "A successful DEFINE must be executed prior to output."
            call sperrorm(tmp_err_msg, 0, 1)
         end if

      on idle ST_IDLE
         if (IdleProcessor.TimeoutExpired()) then
            let int_flag = FALSE
            exit menu
         end if

      on action close
         let int_flag = FALSE
         if (spaskrtn_closeWindow(DIALOG)) then
            exit menu
         else
            continue menu
         end if

      &include "StandardMainDialogActions.inc"

   end menu

# Close the window...
   call spsetwin_hdlClose(whdl_xml)

   call spmdebug_say("TXPPMAST: outputXML_main_menu: COMPLETE: ", current hour to fraction)
end function
###############################################################################
function outputXML_define_routine()

   call spmdebug_say("TXPPMAST: outputXML_define_routine: BEGIN: ", current hour to fraction)

# Initialize global variable(s)...
   let define_done = FALSE
   let int_flag = FALSE
   let file_form = TRUE
   let file_name = ".xml"
   let file_attr = "N"

   display by name file_name,
                   file_form,
                   file_attr

# Input from the user to make sure export formatting is correct...
   options input no wrap
   input by name file_name,
                 file_form,
                 file_attr without defaults attribute (unbuffered)

      before field file_form
         if (NOT outputXML_verify_field("file_name")) then
            next field file_name
         end if

      before field file_attr
         if (NOT outputXML_verify_field("file_form")) then
            next field file_form
         end if

      on action cancel
         if (spaskrtn_interrupt("")) then
            let int_flag = TRUE
            exit input
         else
            let int_flag = FALSE
            continue input
         end if

      after input
         if (NOT outputXML_verify_field("file_name")) then
            next field file_name
         end if
         if (NOT outputXML_verify_field("file_form")) then
            next field file_name
         end if
         if (NOT outputXML_verify_field("file_attr")) then
            next field file_attr
         end if

      &include "StandardEntryDialogActions.inc"

   end input

   if (NOT int_flag) then
      let define_done = TRUE
   else
      let define_done = FALSE
   end if

   call spmdebug_say("TXPPMAST: outputXML_define_routine: COMPLETE: ", current hour to fraction)

end function
###############################################################################
function outputXML_verify_field(in_fieldname)
   define in_fieldname char(20),
          out_valid smallint

   let out_valid = TRUE

   case
      when (in_fieldname = "file_name")
         if (file_name is NULL) then
            let file_name = ""
         end if
         if (length(file_name) <= 0) then
            call sperrorm("A valid field name is required.", 0, 1)
            let out_valid = FALSE
            exit case
         end if

      when (in_fieldname = "file_form")
         if (file_form is NULL) then
            call sperrorm("A valid selection is required.", 0, 1)
            let out_valid = FALSE
            exit case
         end if

      when (in_fieldname = "file_attr")
         if ((file_attr is NULL) or
             (file_attr != "Y" and file_attr != "N")) then
            call sperrorm("A valid category is required.", 0, 1)
            let out_valid = FALSE
            exit case
         end if

   end case

   return out_valid

end function
###############################################################################
function outputXML_export_routine()
   define firstrec integer,
          lastrec integer,
          prfetchno integer,
          tmp_exp_command string,
          tmp_err_msg string,
          tmp_file_name string,
          tmp_rec_notfound smallint,
          tmp_success smallint,
          tmp_txppmast record like txppmast.*

   call spmdebug_say("TXPPMAST: outputXML_export_routine: BEGIN: ", current hour to fraction)

# Initialize local variable(s)...
   let firstrec = 1
   let lastrec = record_count
   let tmp_file_name = "txppmastXML"
   let tmp_success = TRUE

   if (length(file_name) > 0) then
      let tmp_file_name = " "
      let tmp_file_name = file_name clipped
   end if
   call spmdebug_say("TXPPMAST: outputXML_export_routine: file_name: ", file_name)
   call spmdebug_say("TXPPMAST: outputXML_export_routine: tmp_file_name: ", tmp_file_name)

   let tmp_exp_command = spconfig_getPath("export"), tmp_file_name
   call spmdebug_say("TXPPMAST: outputXML_export_routine: tmp_exp_command: ", tmp_exp_command)

   if (tmp_success) then
# Create XML file...
      call txprpxmlbu_beginXMLFile(tmp_exp_command) returning tmp_success, tmp_err_msg
      if (NOT tmp_success) then
         call sperrorm(tmp_err_msg, 0, 2)
      end if
   end if

   if (tmp_success) then
      for prfetchno = firstrec to lastrec
         fetch absolute prfetchno find_cursor into active_mt_sw,
                                                   active_id_no,
                                                   active_year,
                                                   active_list_no
         if (sqlca.sqlcode = -213) then
            exit for
         end if
         if (sqlca.sqlcode != 0) then
            continue for
         end if

# Select the associated property record...
         initialize tmp_txppmast.* to NULL
         call txppmastio_select(active_mt_sw, active_id_no, active_year, active_list_no)
                      returning tmp_success, tmp_err_msg, tmp_rec_notfound, tmp_txppmast.*
         if (tmp_rec_notfound) then
            let tmp_err_msg = "Property record not found for property: ", active_id_no clipped, "."
            let tmp_success = FALSE
         end if
         if (NOT tmp_success) then
            call sperrorm(tmp_err_msg, 0, 2)
            exit for
         end if

# Build a new property element into XML file...
         call txprpxmlbu_buildPropertyXML(25,
                                          tmp_txppmast.txpp_mt_sw,
                                          tmp_txppmast.txpp_id_no,
                                          tmp_txppmast.txpp_year,
                                          tmp_txppmast.txpp_list_no,
                                          file_form,
                                          file_attr)
                                returning tmp_success, tmp_err_msg
          if (NOT tmp_success) then
            call sperrorm(err_msg, 0, 2)
            exit for
         end if
      end for
   end if

   if (tmp_success) then
# Close and clean and save XML file...
      call txprpxmlbu_endXMLFile() returning tmp_success, tmp_err_msg
      if (NOT tmp_success) then
         call sperrorm(tmp_err_msg, 0, 2)
      end if
   end if

   if (tmp_success) then
      let tmp_err_msg = "Export file \"", tmp_exp_command clipped,"\" ", "has been created in your munis export directory."
      call spaskrtn_msgBox("Process complete", tmp_err_msg)
   end if

   call spmdebug_say("TXPPMAST: outputXML_export_routine: COMPLETE: ", current hour to fraction)

end function
###############################################################################
function outputXML_print_routine(optint)
   define background smallint,
          current_only smallint,
          firstrec integer,
          landscape smallint,
          lastrec integer,
          nightrun smallint,
          optint smallint,
          output_command string,
          output_type smallint,
          prfetchno integer,
          tmp_err_msg string,
          tmp_rec_notfound smallint,
          tmp_success smallint,
          tmp_use_file string,
          tmp_txppmast record like txppmast.*

   call spmdebug_say("TXPPMAST: outputXML_print_routine: BEGIN: ", current hour to fraction)

# Initialize local variable(s)...
   let firstrec = 1
   let lastrec = record_count
   let tmp_err_msg = " "
   let tmp_rec_notfound = FALSE
   let tmp_success = TRUE

   let reptitle = "PERSONAL PROPERTY XML EXPORT"
   call spoutput_reportStyle("predefinedForm")
   call spoutput_rpttitle(ST_USER, optint, "txppmast", reptitle)
                returning output_type,
                          output_command,
                          current_only,
                          landscape,
                          background,
                          nightrun,
                          reptitle

   case
      when (output_type = 0)
         let tmp_success = FALSE
      when (output_type = 1 or output_type = 2)
         start report export_properties to pipe output_command
      when (output_type = 3)
         start report export_properties to output_command
      when (output_type = 5)
         start report export_properties to printer
   end case

   { options sql interrupt on # NOTE: See https://tylerjira.tylertech.com/browse/MUN-360785 }
   let int_flag = FALSE

   call sperrorm_message("Creating XML file...")

# Determine the location for the file to be created...
   if (output_type != 3) then
      let tmp_use_file = spgettmp_tempFile("TEMP")
   else
      let tmp_use_file = output_command
   end if

   call spmdebug_isay("TXPPMAST: outputXML_print_routine: Creating XML file here: ", tmp_use_file)

   if (tmp_success) then
# Create XML file...
      call txprpxmlbu_beginXMLFile(tmp_use_file) returning tmp_success, tmp_err_msg
      if (NOT tmp_success) then
         call sperrorm(tmp_err_msg, 0, 2)
      end if
   end if

   if (tmp_success) then
      for prfetchno = firstrec to lastrec
         fetch absolute prfetchno find_cursor into active_mt_sw,
                                                   active_id_no,
                                                   active_year,
                                                   active_list_no
         if (sqlca.sqlcode = -213) then
            exit for
         end if
         if (sqlca.sqlcode != 0) then
            continue for
         end if

# Select the associated property record...
         initialize tmp_txppmast.* to NULL
         call txppmastio_select(active_mt_sw, active_id_no, active_year, active_list_no)
                        returning tmp_success, tmp_err_msg, tmp_rec_notfound, tmp_txppmast.*
         if (tmp_rec_notfound) then
            let tmp_err_msg = "Property record not found for property: ", active_id_no clipped, "."
            let tmp_success = FALSE
         end if
         if (NOT tmp_success) then
            call sperrorm(tmp_err_msg, 0, 2)
            exit for
         end if

# Build a new property element into XML file...
         call txprpxmlbu_buildPropertyXML(25,
                                          tmp_txppmast.txpp_mt_sw,
                                          tmp_txppmast.txpp_id_no,
                                          tmp_txppmast.txpp_year,
                                          tmp_txppmast.txpp_list_no,
                                          file_form,
                                          file_attr)
                                 returning tmp_success, tmp_err_msg
            if (NOT tmp_success) then
            call sperrorm(err_msg, 0, 2)
            exit for
         end if
      end for
   end if

   if (tmp_success) then
      call sperrorm_message("Completing XML file...")
      if (output_type != 3) then
# Close and clean and save XML file into a string...
         call txprpxmlbu_endXMLString() returning tmp_success, tmp_err_msg, xml_string

# Output the XML file to the report...
         call sperrorm_message("Outputting XML file...")
         output to report export_properties()
      else
# Close and clean and save XML file...
         call txprpxmlbu_endXMLFile() returning tmp_success, tmp_err_msg
         if (NOT tmp_success) then
            call sperrorm(tmp_err_msg, 0, 2)
         end if
      end if
   end if

   call sperrorm_blankmess()

   finish report export_properties
   { options sql interrupt off # NOTE: See https://tylerjira.tylertech.com/browse/MUN-360785 }

   if (int_flag) then
      let int_flag = FALSE
   end if

   if (NOT spoutput_finishSpoolEntry(reptitle, lastrec, "A")) then
      call sperrorm("Error updating saved file record.", 0, 1)
   end if

   call spmdebug_say("TXPPMAST: outputXML_print_routine: COMPLETE: ", current hour to fraction)

end function
###############################################################################
report export_properties()

   output
      right margin 0
      left margin 0
      top margin 0
      bottom margin 0
      page length 1

   format
      on every row
         print column 001, xml_string clipped

end report
###############################################################################
private function load_values_array()
   define lv_classDescription char(30),
          lv_errorMessage string,
          lv_exists boolean,
          lv_length1,
          lv_length2,
          lv_recordNotFound,
          lv_success,
          lv_year smallint,
          i, j,
          lv_bill integer,
          lv_mvaclass record like mvaclass.*,
          la_mvamvmst dynamic array of record like mvamvmst.*,
          la_txvalues dynamic array of record like txvalues.*

   define la_displayValues dynamic array of record
            classCode like txclascd.txcl_class,
            classDescription like txclascd.txcl_desc,
            count integer,
            currentYearAssessment like txvalues.txva_amt,
            lastYearAssessment like txvalues.txva_amt
          end record

   # Initialize local variable(s)...
   let lv_bill = 0
   let lv_errorMessage = " "
   let lv_exists = FALSE
   let lv_success = TRUE
   let lv_year = 0

   # Initialize temporary array(s)...
   call la_displayValues.clear()
   call la_mvamvmst.clear()
   call la_txvalues.clear()

   # Initialize global array(s)...
   call g_disp_values_array.clear()

   # Obtain all current year's associated txvalues records...
   call la_txvalues.clear()
   call txvalueswr_selectForParent(gr_txppmast.txpp_mt_sw, gr_txppmast.txpp_ar_cat, gr_txppmast.txpp_id_no, gr_txppmast.txpp_year, gr_txppmast.txpp_list_no, la_txvalues)
                         returning lv_success, lv_errorMessage
   if (NOT lv_success) then
      call sperrorm(lv_errorMessage, 0, 2)
   else
      # Cycle through la_txvalues...
      let lv_length1 = la_txvalues.getLength()
      if (lv_length1) then
         for i=1 to lv_length1
            # Check to see if value class code has already been added la_displayValues...
            let lv_exists = FALSE
            let lv_length2 = la_displayValues.getLength()
            if (lv_length2) then
               for j=1 to lv_length2
                  if (la_txvalues[i].txva_class == la_displayValues[j].classCode) then
                     # Value class code has already been added to la_displayValues, add assessment to it...
                     let la_displayValues[j].count = la_displayValues[j].count + la_txvalues[i].txva_count
                     let la_displayValues[j].currentYearAssessment = la_displayValues[j].currentYearAssessment + la_txvalues[i].txva_amt

                     let lv_exists = TRUE
                     exit for
                  end if
               end for
            end if

            if (NOT lv_exists) then
               # Value class code has not been added to la_displayValues...
               # Obtain the value class code description...
               let lv_classDescription = " "
               call txclascdwr_getDescription(la_txvalues[i].txva_class, la_txvalues[i].txva_ar_cat, la_txvalues[i].txva_type)
                                    returning lv_success, lv_errorMessage, lv_classDescription
               if (NOT lv_success) then
                  call sperrorm(lv_errorMessage, 0, 2)
               end if

               # Add value to la_displayValues...
               call la_displayValues.appendElement()
               let j = la_displayValues.getLength()
               let la_displayValues[j].classCode = la_txvalues[i].txva_class
               let la_displayValues[j].classDescription = lv_classDescription
               let la_displayValues[j].count = la_txvalues[i].txva_count
               let la_displayValues[j].currentYearAssessment = la_txvalues[i].txva_amt
            end if
         end for
      end if
   end if

   # Obtain all of last year's associated txvalues records...
   if (gr_txppmast.txpp_mt_sw == 0) then
      let lv_year = coll_year
   else
      let lv_year = gr_txppmast.txpp_year - 1
   end if

   execute getBillNumber using char_id_no, lv_year into lv_bill
   if (sqlca.sqlcode) then
      if (sqlca.sqlcode != NOTFOUND) then
         let lv_errorMessage = "Unexpected error selecting from Bill Header. Status:"
         call spsyserr(lv_errorMessage, "Fetch getBillNumber, Line: " || __LINE__)
      end if
   end if

   call la_txvalues.clear()
   call txvalueswr_selectForParent(1, 25, gr_txppmast.txpp_id_no, lv_year, lv_bill, la_txvalues)
                         returning lv_success, lv_errorMessage
   if (NOT lv_success) then
      call sperrorm(lv_errorMessage, 0, 2)
   else
      # Cycle through la_txvalues...
      let lv_length1 = la_txvalues.getLength()
      if (lv_length1) then
         for i=1 to lv_length1
            # Check to see if value class code has already been added la_displayValues...
            let lv_exists = FALSE
            let lv_length2 = la_displayValues.getLength()
            if (lv_length2) then
               for j=1 to lv_length2
                  if (la_txvalues[i].txva_class == la_displayValues[j].classCode) then
                     # Value class code has already been added to la_displayValues, add last year assessment to it...
                     if (la_displayValues[j].lastYearAssessment is NULL) then
                        let la_displayValues[j].lastYearAssessment = la_txvalues[i].txva_amt
                     else
                        let la_displayValues[j].lastYearAssessment = la_displayValues[j].lastYearAssessment + la_txvalues[i].txva_amt
                     end if

                     let lv_exists = TRUE
                     exit for
                  end if
               end for
            end if

            if (NOT lv_exists) then
               # Value class code has not been added to la_displayValues...
               # Cycle thru la_displayValues to see if a blank class code already exists...
               let lv_length2 = la_displayValues.getLength()
               if (lv_length2) then
                  for j=1 to lv_length2
                     if ((la_displayValues[j].classCode IS NULL) or (la_displayValues[j].classCode == "")) then
                        # Blank class code already exists, add last year assessment to it...
                        if (la_displayValues[j].lastYearAssessment is NULL) then
                           let la_displayValues[j].lastYearAssessment = la_txvalues[i].txva_amt
                        else
                           let la_displayValues[j].lastYearAssessment = la_displayValues[j].lastYearAssessment + la_txvalues[i].txva_amt
                        end if

                        let lv_exists = TRUE
                        exit for
                     end if
                  end for
               end if

               if ((NOT lv_exists) and (la_txvalues[i].txva_amt) and (gr_txppmast.txpp_mt_sw != 7)) then
                  # Add value to la_displayValues...
                  call la_displayValues.appendElement()
                  let j = la_displayValues.getLength()
                  let la_displayValues[j].classCode = ""
                  let la_displayValues[j].classDescription = "Last year value(s) removed"
                  let la_displayValues[j].lastYearAssessment = la_txvalues[i].txva_amt
               end if
            end if
         end for
      end if
   end if

   if (ST_CLIENT_STATE = "VA" and gr_txppmast.txpp_ar_cat = 25) then
      # Obtain all associated mvamvmst records...
      call la_mvamvmst.clear()
      call mvamvmstwr_selectForParent(gr_txppmast.txpp_mt_sw, gr_txppmast.txpp_id_no, gr_txppmast.txpp_year, gr_txppmast.txpp_list_no, la_mvamvmst)
                            returning lv_success, lv_errorMessage
      if (NOT lv_success) then
         call sperrorm(lv_errorMessage, 0, 2)
      else
         # Cycle through la_mvamvmst...
         let lv_length1 = la_mvamvmst.getLength()
         if (lv_length1) then
            for i=1 to lv_length1
               # Check to see if motor vehicle class code has already been added la_displayValues...
               let lv_exists = FALSE
               let lv_length2 = la_displayValues.getLength()
               if (lv_length2) then
                  for j=1 to lv_length2
                     if (la_mvamvmst[i].mvam_class == la_displayValues[j].classCode) then
                        # Motor vehicle class code has already been added to la_displayValues, add assessment to it...
                        let la_displayValues[j].count = la_displayValues[j].count + 1
                        if (la_displayValues[j].currentYearAssessment is NULL) then
                           let la_displayValues[j].currentYearAssessment = la_mvamvmst[i].mvam_assessment
                        else
                           let la_displayValues[j].currentYearAssessment = la_displayValues[j].currentYearAssessment + la_mvamvmst[i].mvam_assessment
                        end if

                        let lv_exists = TRUE
                        exit for
                     end if
                  end for
               end if

               if (NOT lv_exists) then
                  # Motor vehicle class code has not been added to la_displayValues...
                  # Obtain the motor vehicle class code description...
                  initialize lv_mvaclass.* to NULL
                  call mvaclassio_select(la_mvamvmst[i].mvam_class) 
                               returning lv_success, lv_errorMessage, lv_recordNotFound, lv_mvaclass.*
                  if (NOT lv_success) then
                     call sperrorm(lv_errorMessage, 0, 2)
                  end if

                  # Add motor vehicle to la_displayValues...
                  call la_displayValues.appendElement()
                  let j = la_displayValues.getLength()
                  let la_displayValues[j].classCode = la_mvamvmst[i].mvam_class
                  let la_displayValues[j].classDescription = lv_mvaclass.mvac_desc clipped
                  let la_displayValues[j].count = 1
                  let la_displayValues[j].currentYearAssessment = la_mvamvmst[i].mvam_assessment
               end if
            end for
         end if
      end if
   end if

   # Finally, populate the g_disp_values_array array...
   call g_disp_values_array.clear()
   let lv_length1 = la_displayValues.getLength()
   for i=1 to lv_length1
      call g_disp_values_array.appendElement()
      let j = g_disp_values_array.getLength()
      let g_disp_values_array[j].classCode = la_displayValues[i].classCode
      let g_disp_values_array[j].classDescription = la_displayValues[i].classDescription
      let g_disp_values_array[j].count = la_displayValues[i].count
      let g_disp_values_array[j].currentYearAssessment = la_displayValues[i].currentYearAssessment
      if (la_displayValues[i].currentYearAssessment is NULL) then 
         let g_disp_values_array[j].currentYearAssessment = 0 
      end if
      let g_disp_values_array[j].lastYearAssessment = la_displayValues[i].lastYearAssessment
      if (la_displayValues[i].lastYearAssessment is NULL) then 
         let g_disp_values_array[j].lastYearAssessment = 0
      end if
   end for

   let lv_length1 = g_disp_values_array.getLength()
   if (lv_length1) then
      # Bubble sort (re-order) the g_disp_values_array array by class code...
      for i = 1 to lv_length1
         for j = lv_length1 to i + 1 step -1
            if ((g_disp_values_array[j-1].classCode > g_disp_values_array[j].classCode) or (g_disp_values_array[j-1].classCode == "")) then
               call g_disp_values_array.insertElement(j+1)
               let g_disp_values_array[j+1].* = g_disp_values_array[j-1].*
               call g_disp_values_array.deleteElement(j-1)
            end if
         end for
      end for
   end if

end function
###############################################################################
###############################################################################
#