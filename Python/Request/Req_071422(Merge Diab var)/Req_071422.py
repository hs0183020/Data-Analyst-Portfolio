import pandas as pd
import datacompy
# import numpy as np


def main(Excel_Sheet,cchc):
    #keep rrid and labid from CCHC for compare
    cchc_v1=cchc[['RRID','LABID','ADA2010_DM']]
    cchc_v1.columns= cchc_v1.columns.str.lower()
    cchc_v1 = cchc_v1.sort_values(by=['labid'], ascending=True)
    
    
    Excel_Sheet.columns= Excel_Sheet.columns.str.lower()
    Excel_Sheet = Excel_Sheet.sort_values(by=['labid'], ascending=True)
    
    #clean up DXA_LOG information
    Excel_Sheet['labid'] = Excel_Sheet['labid'].str.upper()
    Excel_Sheet['rrid'] = Excel_Sheet['rrid'].str.upper()
    Excel_Sheet = Excel_Sheet.sort_values(by=['labid'], ascending=True)
    #find duplicates in DXA_LOG
    Excel_Sheet_dups = Excel_Sheet[Excel_Sheet.duplicated(['labid'], keep=False)]
    #compare CCHC and DXA_LOG
    compare = datacompy.Compare(cchc_v1,Excel_Sheet,join_columns=['labid','rrid'])
    #Not in CCHC but in DXA_LOG
    notInCCHC=compare.df2_unq_rows
    
    #left join to add diabates variable to excel sheet
    output = pd.merge(Excel_Sheet, cchc_v1, how="left",on=["labid", "rrid"])
    
    return output,Excel_Sheet_dups,notInCCHC

if __name__ == "__main__":
    #Bring in information from SAS covid and DRS datasets
    Excel_Sheet = pd.read_excel(r'C:\Users\hsoriano\Documents\Python Scripts\Req071422\randomized_Q-02751_SampleManifest_LIMS.xlsx',sheet_name='randomized_Q-02751_SampleManife')
    cchc=pd.read_sas(r'\\Uthouston.edu\uthsc\SPH\Research\Studies Data\All_CCHC\cchc.sas7bdat',encoding='latin1')
    
    output=main(Excel_Sheet,cchc)
    
    #output to excel file
    output[0].to_excel(r'C:\Users\hsoriano\Documents\Python Scripts\Req071422\randomized_Q-02751_SampleManifest_LIMS_withDiab.xlsx', index = False)
    
