import pandas as pd
import datacompy
import numpy as np


if __name__ == "__main__":
    #Bring in information from both excel files
    df_HCV = pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\LABORATORY_RAHC\HCV\final_hcv.sas7bdat',encoding='latin1')
    df_CCHC = pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\All_CCHC\cchc.sas7bdat',encoding='latin1')
    df_EXCEL = pd.read_excel(r'C:\Users\hsoriano\Documents\Python\Data Analyst Portfolio\Python\Request\Marcela\Req_050423(HCV)\Participants with missing HCV_050223.xlsx',sheet_name='Sheet1')

    df_CCHC.columns=df_CCHC.columns.str.lower()
    df_HCV.columns=df_HCV.columns.str.lower()
    df_EXCEL.columns=df_EXCEL.columns.str.lower()
    
    df_CCHC=df_CCHC[['labid','rrid','visit']]
    
    df_HCV1=df_HCV.merge(df_CCHC,how="left",on=['labid'])
    
    df_HCV2=df_HCV1.loc[~df_HCV1.rrid.isna()]
    df_HCV3=df_HCV2.loc[df_HCV2.rrid.isin(df_EXCEL.rrid)]
    
    

