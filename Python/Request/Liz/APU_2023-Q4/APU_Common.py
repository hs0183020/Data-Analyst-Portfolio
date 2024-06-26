from typing import TYPE_CHECKING
from rich import print
from datetime import datetime, timedelta

class Config:
  def __init__(self):

    #APU or OR reporting cycle? This should be APU or OR.
    self.cycle = "APU"

    #These you need to modify each reporting cycle
    #Basic year and quarter info
    self.cmn_yyyy = "2023"
    self.cmn_q = 4
    self.cmn_yyyy_hosp = "2024"
    self.cmn_q_hosp = 1

    self.cmn_qtr_start = "01OCT" + self.cmn_yyyy
    self.cmn_qtr_end = "31DEC" + self.cmn_yyyy

    print("[red]Note: cmn_qtr_start= " + self.cmn_qtr_start +
          " cmn_qtr_end= " + self.cmn_qtr_end
          + "[/]")

    """
    Report start = entire duration of report. 
	  Will typically end on qtr end date.
	  For APU, will typically start on Jan 1
	  For OR, typically same as quarter start date
    """
    if self.cycle == "APU":
      self.cmn_report_start	= "01JAN" + self.cmn_yyyy
    else:
      self.cmn_report_start	= self.cmn_qtr_start
    
    self.cmn_report_end		= self.cmn_qtr_end;

    print("[red]Note: cmn_report_start= " + self.cmn_report_start +
          " cmn_report_end= " + self.cmn_report_end
          + "[/]")

    #Same macros but hospice is one quarter off
    #common quarter start for hosp should always reflect JAN due to how it is used downstream
    self.cmn_qtr_start_hosp		= "01JAN" + self.cmn_yyyy_hosp
    self.cmn_qtr_end_hosp		= "31MAR" + self.cmn_yyyy_hosp
    self.cmn_report_start_hosp	= "01JAN" + self.cmn_yyyy_hosp
    self.cmn_report_end_hosp = self.cmn_qtr_end_hosp

    print("[red]Note: cmn_qtr_start_hosp= " + self.cmn_qtr_start_hosp +
          " cmn_qtr_end_hosp= " + self.cmn_qtr_end_hosp
          + "[/]")

    print("[red]Note: cmn_report_start_hosp= " + self.cmn_report_start_hosp +
          " cmn_report_end_hosp= " + self.cmn_report_end_hosp
          + "[/]")

    #Submission cutoff date
    self.cmn_submit_cutoff	= "15MAY2024"

    #Which PACs to run?
    self.cmn_run_irf = 1
    self.cmn_run_ltch = 1
    self.cmn_run_snf = 1
    self.cmn_run_hosp = 1

    #Alternate formats for year and quarter	
    self.cmn_yy = self.cmn_yyyy[-2:]
    print("[red]Note: cmn_yy= " + self.cmn_yy + "[/]")
    self.cmn_qq = "Q" + str(self.cmn_q)
    print("[red]Note: cmn_qq= " + self.cmn_qq + "[/]")
    self.cmn_yy_hosp = self.cmn_yyyy_hosp[-2:]
    print("[red]Note: cmn_yy_hosp= " + self.cmn_yy_hosp + "[/]")
    self.cmn_qq_hosp = "Q" + str(self.cmn_q_hosp)
    print("[red]Note: cmn_qq_hosp= " + self.cmn_qq_hosp + "[/]")

    #Workbench folder for this reporting cycle
    self.cmn_folder = self.cmn_yyyy + "-" + self.cmn_qq + " " + self.cycle
    print("[red]Note: cmn_folder= " + self.cmn_folder + "[/]")

    #Data extract start dates in Hive format
    self.cmn_qtr_start_hive = datetime.strptime(self.cmn_qtr_start, "%d%b%Y").strftime("%Y-%m-%d")
    print("[red]Note: cmn_qtr_start_hive= " + self.cmn_qtr_start_hive + "[/]")
    self.cmn_qtr_end_hive = datetime.strptime(self.cmn_qtr_end, "%d%b%Y").strftime("%Y-%m-%d")
    print("[red]Note: cmn_qtr_end_hive= " + self.cmn_qtr_end_hive + "[/]")
    
    #hospice pulls from beginning of reporting year to end of current quarter
    self.cmn_qtr_start_hive_hosp = datetime.strptime(self.cmn_report_start_hosp, "%d%b%Y").strftime("%Y-%m-%d")
    print("[red]Note: cmn_qtr_start_hive_hosp= " + self.cmn_qtr_start_hive_hosp + "[/]")
    self.cmn_qtr_end_hive_hosp = datetime.strptime(self.cmn_qtr_end_hosp, "%d%b%Y").strftime("%Y-%m-%d")
    print("[red]Note: cmn_qtr_end_hive_hosp= " + self.cmn_qtr_end_hive_hosp + "[/]")
    
    #Submission cutoff date
    self.cmn_submit_cutoff_hive = datetime.strptime(self.cmn_submit_cutoff, "%d%b%Y").strftime("%Y-%m-%d")
    print("[red]Note: cmn_submit_cutoff_hive= " + self.cmn_submit_cutoff_hive + "[/]")
    
    #Tiein cutoff -- providers with tiein >= cutoff are OPD
    self.cmn_tiein_cutoff = (datetime.strptime(self.cmn_report_end, "%d%b%Y") + timedelta(days=1)).strftime("%d%b%Y").upper()
    print("[red]Note: cmn_tiein_cutoff= " + self.cmn_tiein_cutoff + "[/]")
    #Note that we don't need a tiein cutoff for Hospice...
    
    
    
Config_obj = Config()
