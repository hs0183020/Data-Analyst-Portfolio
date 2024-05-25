import pandas as pd
import datacompy
import pyodbc as pyodbc



if __name__ == "__main__":
    #df_covid=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\Covid\covid.sas7bdat',encoding='latin1')

    df_FAST = pd.read_excel(r'U:\Research\BrownsvilleSD\public\Diabetes_Core\Data Request\McCormick\Req_021023(FAST Variable)\Req_021023(FAST Variable).xlsx',sheet_name='Req_021023(FAST Variable)')

    df_Sheet1 = pd.read_excel(r'U:\Users\hsoriano\Data Request\Marcela\Req_020223(Add FAST)\Aliquot Manifest.xlsx',sheet_name='TS01587919 Plate 1 ')
    df_Sheet2 = pd.read_excel(r'U:\Users\hsoriano\Data Request\Marcela\Req_020223(Add FAST)\Aliquot Manifest.xlsx',sheet_name='TS01587928 Plate 2')
    df_Sheet3 = pd.read_excel(r'U:\Users\hsoriano\Data Request\Marcela\Req_020223(Add FAST)\Aliquot Manifest.xlsx',sheet_name='TS01614066 Plate 3')

    df_FAST = df_FAST[['RRID','LABID','FAST = (ln e)^X/(1-(ln e)^X)']]

    df_FAST.rename(columns={'FAST = (ln e)^X/(1-(ln e)^X)': 'FAST'}, inplace=True)
    
    df_Sheet1_FAST=df_Sheet1.merge(df_FAST, left_on='LABID', right_on='LABID')
    df_Sheet1_FAST=df_Sheet1.merge(df_FAST, how='left', on='LABID')
    
    df_notinlist=df_Sheet1.loc[~df_Sheet1.LABID.isin(df_Sheet1_FAST['LABID'])]
    
    df_Sheet2_FAST=df_Sheet2.merge(df_FAST, left_on='LABID', right_on='LABID')
    df_Sheet2_FAST=df_Sheet2.merge(df_FAST, how='left', on='LABID')
    
    df_notinlist=df_Sheet2.loc[~df_Sheet2.LABID.isin(df_Sheet2_FAST['LABID'])]
    
    df_Sheet3_FAST=df_Sheet3.merge(df_FAST, left_on='LABID', right_on='LABID')
    df_Sheet3_FAST=df_Sheet3.merge(df_FAST, how='left', on='LABID')
    
    df_notinlist=df_Sheet3.loc[~df_Sheet3.LABID.isin(df_Sheet3_FAST['LABID'])]
    
    
    #output to excel file
    writer = pd.ExcelWriter(r'U:\Users\hsoriano\Data Request\Marcela\Req_020223(Add FAST)\Aliquot Manifest_FAST.xlsx', engine='xlsxwriter')


    df_Sheet1_FAST.to_excel(writer, sheet_name='TS01587919 Plate 1 ',index=False)
    
    df_Sheet2_FAST.to_excel(writer, sheet_name='TS01587928 Plate 2',index=False)
    
    df_Sheet3_FAST.to_excel(writer, sheet_name='TS01614066 Plate 3',index=False)
    #worksheet = writer.sheets['Baseline 75<Trig<=150']

    #worksheet.write(0, 0, 'Baseline 75<Trig<=150')
    
   
    # Close the Pandas Excel writer and output the Excel file.
    writer.save()
    