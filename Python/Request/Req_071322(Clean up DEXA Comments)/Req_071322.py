import pandas as pd
import datacompy
# import numpy as np

def comment_code(a):
    if "OSTEOPOROSIS" in a.upper():
        return 1
    elif "HIP REPLACEMENT" in a.upper():
        return 2
    elif "CANCER" in a.upper():
        return 3
    elif "HYSTERECTOMY" in a.upper():
        return 4   
    elif "HERNIATED DISC" in a.upper():
        return 5   
    elif "HEART ATTACK" in a.upper():
        return 6   
    elif "ARTHRITIS" in a.upper():
        return 7  
    elif "METAL" in a.upper():
        return 8  
    elif "KNEE REPLACEMENT" in a.upper():
        return 9 
    else:
        return 0
    
def main(DXA_Log,cchc):
    #keep rrid and labid from CCHC for compare
    cchc_v1=cchc[['RRID','LABID']]
    cchc_v1.columns= cchc_v1.columns.str.lower()
    cchc_v1 = cchc_v1.sort_values(by=['rrid'], ascending=True)
    
    #Grab rrid labid comments1
    DXA_Log.columns= DXA_Log.columns.str.lower()
    DXA_Log = DXA_Log.sort_values(by=['labid'], ascending=True)
    DXA_Log_v1=DXA_Log[['rrid','labid','comments1']]
    #clean up DXA_LOG information
    DXA_Log_v1 = DXA_Log_v1[DXA_Log_v1['comments1'].notna()]
    DXA_Log_v1 = DXA_Log_v1[DXA_Log_v1['labid'].notna()]
    DXA_Log_v1['labid'] = DXA_Log_v1['labid'].str.upper()
    DXA_Log_v1 = DXA_Log_v1[DXA_Log_v1['rrid'].notna()]
    DXA_Log_v1['rrid'] = DXA_Log_v1['rrid'].str.upper()
    DXA_Log_v1 = DXA_Log_v1.sort_values(by=['labid'], ascending=True)
    #correcting values
    DXA_Log_v1.loc[(DXA_Log_v1["labid"] == "BA0508/UL0071") & (DXA_Log_v1["rrid"] == "BD3523"), "labid"] = "BA0508"
    DXA_Log_v1.loc[(DXA_Log_v1["labid"] == "10Y0361") & (DXA_Log_v1["rrid"] == "BD1131"), "labid"] = "10Y0367"
    DXA_Log_v1.loc[(DXA_Log_v1["labid"] == "15Y0188") & (DXA_Log_v1["rrid"] == "BD1379"), "labid"] = "15Y0186"
    DXA_Log_v1.loc[(DXA_Log_v1["labid"] == "BD0601") & (DXA_Log_v1["rrid"] == "BD3653"), "labid"] = "BA0601"
    DXA_Log_v1.loc[(DXA_Log_v1["labid"] == "BD0668") & (DXA_Log_v1["rrid"] == "BD3735"), "labid"] = "BA0668"
    #find duplicates in DXA_LOG
    DXA_Log_dups = DXA_Log_v1[DXA_Log_v1.duplicated(['labid'], keep=False)]
    DXA_Log_v1 = DXA_Log_v1.drop([724]) # removed dup
    DXA_Log_dups = DXA_Log_v1[DXA_Log_v1.duplicated(['labid'], keep=False)] 
    #compare CCHC and DXA_LOG
    compare = datacompy.Compare(cchc_v1,DXA_Log_v1,join_columns=['labid','rrid'])
    #Not in CCHC but in DXA_LOG
    notInCCHC=compare.df2_unq_rows
    
    #cleaning up comments remove follow ups
    drop_rows = DXA_Log_v1['comments1'].str.contains("FOLLOW UP")
    DXA_Log_v1 = DXA_Log_v1[~drop_rows]
    
    DXA_Log_v1["comments_code"] = DXA_Log_v1.comments1.apply(lambda x: comment_code(x))
    
    return DXA_Log_v1,DXA_Log_dups,notInCCHC



if __name__ == "__main__":
    #Bring in information from SAS covid and DRS datasets
    DXA_Log = pd.read_excel(r'\\uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\Data Request\Python\Req_071322\DXA & BONE TURN OVER  spreadsheet for Dr. Nahid.xlsx',sheet_name='DXA log')
    cchc=pd.read_sas(r'\\Uthouston.edu\uthsc\SPH\Research\Studies Data\All_CCHC\cchc.sas7bdat',encoding='latin1')

    Returned_Values=main(DXA_Log,cchc)
    Comments=Returned_Values[0]
    Dups=Returned_Values[1]
    NotInCCHC=Returned_Values[2]
    
    Comments.to_excel(r'C:\Users\hsoriano\Documents\Python Scripts\comments.xlsx', index = False)
    
