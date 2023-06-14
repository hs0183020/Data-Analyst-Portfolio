import pandas as pd
import datacompy
import numpy as np


if __name__ == "__main__":
    #Bring in information from both excel files
    DXA_Isela = pd.read_excel(r'C:\Users\hsoriano\Documents\Python\Data Analyst Portfolio\Python\Request\Isela\Req_050223\DXA analytic sample NA_050223.xlsx',sheet_name='Sheet1')
    DXA_Log = pd.read_excel(r'\\uthouston.edu\uthsc\SPH\Research\CRU\DXA and BONETURNOVER\DXA & BONE TURN OVER  spreadsheet for Dr. Nahid.xlsx',sheet_name='DXA log')

    DXA_Isela.columns=DXA_Isela.columns.str.lower()
    DXA_Log.columns=DXA_Log.columns.str.lower()
    
    Columns=list(DXA_Log.columns)
    DXA_Log.rename(columns={'comments pt with metal implant, missing a leg or arm, etc  ':'comments','study id or lab id':'labid'},inplace=True)

    DXA_Log=DXA_Log[['labid','comments']]
    
    DXA_Log=DXA_Log.loc[~DXA_Log.labid.isna()]
    
    DXA_Final=DXA_Isela.merge(DXA_Log,how="left",on=['labid'])

