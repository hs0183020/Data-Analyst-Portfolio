import pandas as pd
import datacompy
import numpy as np
import pyodbc as pyodbc
from functools import reduce
from datetime import date



if __name__ == "__main__":
    #bring in all of the datasets into python
    df_cchc=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\All_CCHC\cchc.sas7bdat',encoding='latin1')
    df_excel = pd.read_csv(r"\\uthouston.edu\uthsc\SPH\Users\hsoriano\Data Request\Rocio's\CleanUpFreezerPro\SamplesReport_1680628949_125.csv")

    df_cchc.columns= df_cchc.columns.str.lower()

    df_cchc1=df_cchc[['rrid','labid']]
    df_excel['Name']=df_excel['Name'].str.replace("\(Copy\)",'')
    df_excel['Name']=df_excel['Name'].str.replace(r"\(Derived_[0-9]\)",'')
    df_excel['Name']=df_excel['Name'].str.strip()
    df_excel['Name']=df_excel['Name'].str.upper()
    
    df_excel['Patient ID']=df_excel['Patient ID'].str.strip()
    df_excel['Patient ID']=df_excel['Patient ID'].str.upper()
    
    df_cchc_rna=df_cchc[['rrid','labid','rna','has_pax','town']]
    
    
    df_cchc_rna_har=df_cchc_rna.loc[df_cchc_rna['town']==2]
    df_cchc_rna_har1=df_cchc_rna_har.loc[(df_cchc_rna_har['rna'].isin([1,3])) | (df_cchc_rna_har['has_pax']==1)]
    
    df_excel_har_5Y = df_excel.loc[df_excel['Name'].str.contains('5YH', regex=True, na=True)]
    df_excel_har = df_excel.loc[df_excel['Name'].str.contains('HA', regex=True, na=True)]
    df_excel_har_pedi = df_excel.loc[df_excel['Name'].str.contains('HP', regex=True, na=True)]
    
    df_excel_har=pd.concat([df_excel_har_5Y,df_excel_har,df_excel_har_pedi],ignore_index=True)
    df_excel_har=df_excel_har[['Name','Patient ID']]
    df_excel_har.columns=[['labid','rrid']]

    
    df_har_rna_merged=pd.concat([df_cchc_rna_har1,df_excel_har],ignore_index=True)

    
    

    writer = pd.ExcelWriter(r"\\uthouston.edu\uthsc\SPH\Users\hsoriano\Data Request\Rocio's\Req_040323(Harlingen PAXGENE inventory)\Har_Rna.xlsx", engine='xlsxwriter')


    df_har_rna_merged.to_excel(writer, sheet_name='Har_RNA',index=False)
    
   
    # Close the Pandas Excel writer and output the Excel file.
    writer.save()
