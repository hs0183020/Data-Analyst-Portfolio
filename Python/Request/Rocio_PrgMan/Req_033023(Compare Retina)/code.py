import pandas as pd
import datacompy
import numpy as np
import pyodbc as pyodbc
from functools import reduce
from datetime import date



if __name__ == "__main__":
    #bring in all of the datasets into python
    df_cchc=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\All_CCHC\cchc.sas7bdat',encoding='latin1')
    df_excel = pd.read_excel(r'\\uthouston.edu\uthsc\SPH\Research\CRU\ULTRASOUND RETINAL SCANS\RETINAL and OCT WORKSHEET.xlsx',sheet_name='Retina worksheet')
    df_ultrasound = pd.read_excel(r'\\Uthouston.edu\uthsc\SPH\Research\CRU\LABORATORY CRU\LAB CRU Inventory\ALIQUOTS INVENTORY ALL studies\ULTRASOUNDS\ULTRASOUND.xlsx',sheet_name='Sheet2')
    df_dapc = pd.read_excel(r'\\uthouston.edu\uthsc\SPH\Users\hsoriano\Python\Cleanup_Retinal\32723CCHC_T2Dpt_DRscore HTNMaxDM DAPC DR.xlsx',sheet_name='202303_merged_DM status byDR')
 
    
    df_ultrasound.columns= df_ultrasound.columns.str.lower()
    df_ultrasound=df_ultrasound.loc[df_ultrasound['ul #'].notnull()]

    #Clean up Retina Worksheet
    df_excel.columns= df_excel.columns.str.lower()
    df_excel.columns = ["retina_num", "rrid", "labid","retina_date","gender","male","female","retina_exmnrid","retina_followup_date","retina_followup",
                        "interpret_dr_reddy","oct_date","oct","oct_tech","oct_followup_date","oct_followup_tech",
                        "oct_followup","name","results","comments"]
    
    df_excel = df_excel.loc[~df_excel.labid.isna()]
    df_excel = df_excel[~df_excel['labid'].isin(['Date'])]
    df_excel = df_excel[~df_excel['rrid'].isin(['RETINAL '])]
    df_excel = df_excel[~df_excel['retina_num'].isin(['not done','re-done','need to repeat','unable to do'])]
    
    #cleaning up wrong labids
    df_excel.loc[(df_excel['rrid'] == 'BD2065') & (df_excel['labid'] == '5Y1121 / UL0072')  ,'labid'] = '5Y1121'
    df_excel.loc[(df_excel['rrid'] == 'BD3522') & (df_excel['labid'] == 'BA0507 / UL0070')  ,'labid'] = 'BA0507'
    df_excel.loc[(df_excel['rrid'] == 'BD0755') & (df_excel['labid'] == '15Y0265')  ,'labid'] = '15Y0256'
    df_excel.loc[(df_excel['rrid'] == 'BD0246') & (df_excel['labid'] == '10Y0012')  ,'labid'] = '20Y0012'
    df_excel.loc[(df_excel['rrid'] == 'BD3800') & (df_excel['labid'] == 'BD0728')  ,'labid'] = 'BA0729'
    df_excel.loc[(df_excel['rrid'] == 'BD0594') & (df_excel['labid'] == '15Y')  ,'labid'] = '15Y0275'
    
    df_excel['rrid'] = df_excel['rrid'].str.strip()
    df_excel['rrid'] = df_excel['rrid'].str.upper()
    
    df_excel['labid'] = df_excel['labid'].str.strip()
    df_excel['labid'] = df_excel['labid'].str.upper()
    
    #replacing ultrasound labids with correct labids
    df_excel_ul = df_excel.loc[df_excel['labid'].str.contains('UL', regex=True, na=True)]
    df_excel_ul['UL_labid'] =  df_excel_ul['labid'] 
    
    df_excel_noul=df_excel.loc[~df_excel['labid'].isin(df_excel_ul['labid'])]
    
    df_excel_ul['labid'] = df_excel_ul['labid'].map(df_ultrasound.set_index('ul #')['last lab id'])
    
    df_excel=pd.concat([df_excel_ul,df_excel_noul],ignore_index=True)
    
    #fixing retina date
    df_excel['retina_date'] = pd.to_datetime(df_excel['retina_date'],errors = 'coerce').dt.date
    df_excel['retina_followup_date'] = pd.to_datetime(df_excel['retina_followup_date'],errors = 'coerce').dt.date

    df_excel['retina_date'] = np.where(~df_excel['retina_followup_date'].isna(),df_excel['retina_followup_date'],df_excel['retina_date'])

    #fixing oct date
    df_excel['oct_date'] = pd.to_datetime(df_excel['oct_date'],errors = 'coerce').dt.date
    df_excel['oct_followup_date'] = pd.to_datetime(df_excel['oct_followup_date'],errors = 'coerce').dt.date

    df_excel['oct_date'] = np.where(~df_excel['oct_followup_date'].isna(),df_excel['oct_followup_date'],df_excel['oct_date'])
    df_excel['oct_date'] = pd.to_datetime(df_excel['oct_date'],errors = 'coerce').dt.date

    #fixing oct
    df_excel['oct'] = np.where(~df_excel['oct_followup'].isna(),df_excel['oct_followup'],df_excel['oct'])
    df_excel['oct'] = pd.to_numeric(df_excel['oct'], errors='coerce')
    
    #fixing oct_tech
    df_excel['oct_tech'] = np.where(~df_excel['oct_followup_tech'].isna(),df_excel['oct_followup_tech'],df_excel['oct_tech'])
    df_excel['oct_tech'] = pd.to_numeric(df_excel['oct_tech'], errors='coerce')
    
    df_excel['retina_exmnrid'] = pd.to_numeric(df_excel['retina_exmnrid'], errors='coerce')
    
    df_excel=df_excel[['rrid','labid','retina_date','retina_exmnrid','oct_date','oct','oct_tech','results','comments']]
    
    df_cchc.columns= df_cchc.columns.str.lower()
    df_cchc=df_cchc[['rrid','labid']]
    output = datacompy.Compare(df_cchc,df_excel,join_columns=['LABID', 'RRID'])
    
    notInCCHC=output.df2_unq_rows
    inBoth=output.intersect_rows
    

    writer = pd.ExcelWriter(r'\\uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\TEMPDATA\ULTRASOUND DATA\RETINA\RETINAL WORKSHEET_032823.xlsx', engine='xlsxwriter')


    df_final.to_excel(writer, sheet_name='Sheet1',index=False)
    
   
    # Close the Pandas Excel writer and output the Excel file.
    writer.save()
