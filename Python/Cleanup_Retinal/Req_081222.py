import pandas as pd
import datacompy
import numpy as np
import pyodbc as pyodbc
from functools import reduce
from datetime import date

def extract(database):
    # SQL query to extract RRID and LabID from IMPACTLAB_A table
    query = "SELECT rrid, labid FROM IMPACTLAB_A"
    # Establishing connection to the database
    cnxn = pyodbc.connect(database)
    # Reading SQL query results into a DataFrame
    df = pd.read_sql(query, cnxn)
    # Closing the database connection
    cnxn.close()
    return df

def main(df_cchc, df_covid, df_syv, df_drs, df_excel):
    # Selecting necessary columns from various DataFrames and performing required data cleaning
    
    # Selecting required columns from COVID, SYV, and DRS DataFrames
    df_covid = df_covid[['rrid', 'labid']]
    df_syv = df_syv[['rrid', 'labid']]
    # Changing column headers to lowercase in DRS DataFrame
    df_drs.columns = df_drs.columns.str.lower()
    # Selecting required columns from DRS DataFrame
    df_drs = df_drs[['rrid', 'labid']]
    # Combining all DataFrames into one
    df_cchc = pd.concat([df_cchc, df_covid, df_syv, df_drs], sort=True, ignore_index=True)
    
    # Cleaning RRID and LabID columns in Excel DataFrame
    df_excel['rrid'] = df_excel['rrid'].str.strip().str.upper()
    df_excel['labid'] = df_excel['labid'].str.strip().str.upper()
    # Dropping rows with null LabID values from Excel DataFrame
    df_excel = df_excel.loc[df_excel.labid.notnull()]
    
    # Comparing data between CCHC DataFrame and Excel DataFrame using datacompy
    compare = datacompy.Compare(df_cchc, df_excel, join_columns=['labid', 'rrid'],
                                df1_name='Original', df2_name='New')
    # Storing rows present in Excel but not in CCHC
    notInCCHC = compare.df2_unq_rows
    
    # Returning combined DataFrame, unmatched rows, and comparison results
    return df_cchc, notInCCHC, compare

if __name__ == "__main__":
    # Establishing database connections and reading necessary Excel files and SAS datasets
    
    # Database connection strings
    constr_bro = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\BrownsvilleSD\public\Diabetes_Core\New_CCHC\Server\BD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    constr_har = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\New_CCHC\HRL\CCHC_HD_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    constr_lar = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\New_CCHC\LD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;'     
    
    # Reading SAS datasets
    df_covid = pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\Covid\covid.sas7bdat', encoding='latin1')
    df_syv = pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\SYV_Covid\syv_covid.sas7bdat', encoding='latin1')
    df_drs = pd.read_sas(r'\\uthouston.edu\uthsc\sph\research\brownsvillesd\public\Diabetes_Core\MSAccess\TEMPDATA\DRS\crldata.sas7bdat', encoding='latin1')
    
    # Reading Excel files
    df_excel = pd.read_excel(r'\\uthouston.edu\uthsc\SPH\Users\hsoriano\Python\Cleanup_Retinal\RETINAL and OCT WORKSHEET.xlsx', sheet_name='Retina worksheet')
    df_ultrasound = pd.read_excel(r'\\Uthouston.edu\uthsc\SPH\Research\CRU\LABORATORY CRU\LAB CRU Inventory\ALIQUOTS INVENTORY ALL studies\ULTRASOUNDS\ULTRASOUND.xlsx', sheet_name='Sheet2')
    df_dapc = pd.read_excel(r'\\uthouston.edu\uthsc\SPH\Users\hsoriano\Python\Cleanup_Retinal\32723CCHC_T2Dpt_DRscore HTNMaxDM DAPC DR.xlsx', sheet_name='202303_merged_DM status byDR')
    
    # Cleaning columns in the ultrasound DataFrame
    df_ultrasound.columns = df_ultrasound.columns.str.lower()
    df_ultrasound = df_ultrasound.loc[df_ultrasound['ul #'].notnull()]

    # Cleaning columns in the Retina Worksheet DataFrame
    df_excel.columns = df_excel.columns.str.lower()
    df_excel.columns = ["retina_num", "rrid", "labid","retina_date","gender","male","female","retina_exmnrid","retina_followup_date","retina_followup",
                        "interpret_dr_reddy","oct_date","oct","oct_tech","oct_followup_date","oct_followup_tech",
                        "oct_followup","name","results","comments"]
    
    df_excel = df_excel.loc[~df_excel.labid.isna()]
    df_excel = df_excel[~df_excel['labid'].isin(['Date'])]
    df_excel = df_excel[~df_excel['rrid'].isin(['RETINAL '])]
    df_excel = df_excel[~df_excel['retina_num'].isin(['not done','re-done','need to repeat','unable to do'])]
    
    # Cleaning incorrect labids
    df_excel.loc[(df_excel['rrid'] == 'BD2065') & (df_excel['labid'] == '5Y1121 / UL0072')  ,'labid'] = '5Y1121'
    df_excel.loc[(df_excel['rrid'] == 'BD3522') & (df_excel['labid'] == 'BA0507 / UL0070')  ,'labid'] = 'BA0507'
    df_excel.loc[(df_excel['rrid'] == 'BD0755') & (df_excel['labid'] == '15Y0265')  ,'labid'] = '15Y0256'
    df_excel.loc[(df_excel['rrid'] == 'BD0246') & (df_excel['labid'] == '10Y0012')  ,'labid'] = '20Y0012'
    df_excel.loc[(df_excel['rrid'] == 'BD3800') & (df_excel['labid'] == 'BD0728')  ,'labid'] = 'BA0729'
    df_excel.loc[(df_excel['rrid'] == 'BD0594') & (df_excel['labid'] == '15Y')  ,'labid'] = '15Y0275'
    
    # Replacing ultrasound labids with correct labids
    df_excel_ul = df_excel.loc[df_excel['labid'].str.contains('UL', regex=True, na=True)]
    df_excel_ul['UL_labid'] =  df_excel_ul['labid'] 
    
    df_excel_noul = df_excel.loc[~df_excel['labid'].isin(df_excel_ul['labid'])]
    
    df_excel_ul['labid'] = df_excel_ul['labid'].map(df_ultrasound.set_index('ul #')['last lab id'])
    
    df_excel = pd.concat([df_excel_ul, df_excel_noul], ignore_index=True)
    
    # Fixing date formats
    df_excel['retina_date'] = pd.to_datetime(df_excel['retina_date'], errors='coerce').dt.date
    df_excel['retina_followup_date'] = pd.to_datetime(df_excel['retina_followup_date'], errors='coerce').dt.date

    df_excel['retina_date'] = np.where(~df_excel['retina_followup_date'].isna(), df_excel['retina_followup_date'], df_excel['retina_date'])

    df_excel['oct_date'] = pd.to_datetime(df_excel['oct_date'], errors='coerce').dt.date
    df_excel['oct_followup_date'] = pd.to_datetime(df_excel['oct_followup_date'], errors='coerce').dt.date

    df_excel['oct_date'] = np.where(~df_excel['oct_followup_date'].isna(), df_excel['oct_followup_date'], df_excel['oct_date'])
    df_excel['oct_date'] = pd.to_datetime(df_excel['oct_date'], errors='coerce').dt.date

    # Fixing numeric data
    df_excel['oct'] = np.where(~df_excel['oct_followup'].isna(), df_excel['oct_followup'], df_excel['oct'])
    df_excel['oct'] = pd.to_numeric(df_excel['oct'], errors='coerce')

    df_excel['oct_tech'] = np.where(~df_excel['oct_followup_tech'].isna(), df_excel['oct_followup_tech'], df_excel['oct_tech'])
    df_excel['oct_tech'] = pd.to_numeric(df_excel['oct_tech'], errors='coerce')

    df_excel['retina_exmnrid'] = pd.to_numeric(df_excel['retina_exmnrid'], errors='coerce')

    df_excel = df_excel[['rrid', 'labid', 'retina_date', 'retina_exmnrid', 'oct_date', 'oct', 'oct_tech', 'results', 'comments']]
    
    # Extracting RRID and LABID from DAPC dataset and combining
    df_cchc = pd.concat([extract(constr_bro), extract(constr_har), extract(constr_lar)], sort=True, ignore_index=True)
     
    output = main(df_cchc, df_covid, df_syv, df_drs, df_excel)
    
    # Cleaning DAPC dataset
    df_dapc.columns = df_dapc.columns.str.lower()
    df_dapc1 = df_dapc[['dr?', 'rrid', 'labid', 'dapc comments']]

    df_dapc1 = df_dapc1[~df_dapc1.labid.str.contains("LABID")]
    df = df_dapc1['dr?'].unique()
    
    # Categorizing column 'dr?'
    # 0: No, 1: Yes, 2: Possible, 3: Unable, 4: Missing Image, 5: Other look at dapc_dr_other
    df_dapc1.loc[(df_dapc1['dr?'] == 'y'), 'dr?'] = 1
    df_dapc1.loc[(df_dapc1['dr?'] == 'n'), 'dr?'] = 0
    df_dapc1.loc[(df_dapc1['dr?'] == 'p'), 'dr?'] = 2
    df_dapc1.loc[(df_dapc1['dr?'] == 'unable'), 'dr?'] = 3
    df_dapc1.loc[(df_dapc1['dr?'] == 'missing'), 'dr?'] = 4   
    df_dapc1.loc[(~df_dapc1['dr?'].isin([0, 1, 2, 3, 4])) & (~df_dapc1['dr?'].isna()), 'dapc_dr_other'] = df_dapc1['dr?']
    df_dapc1.loc[(~df_dapc1['dr?'].isin([0, 1, 2, 3, 4])) & (~df_dapc1['dr?'].isna()), 'dr?'] = 5
    
    df_dapc1['dr?'] = pd.to_numeric(df_dapc1['dr?'], errors='coerce')
    
    df_dapc2 = df_dapc1.drop(df_dapc1[(df_dapc1['rrid'] == "BD1375") & (df_dapc1['labid'] == "5Y1054") & (df_dapc1['dr?'] == 5)].index)

    df_dups = df_dapc2.loc[df_dapc2.duplicated(subset='labid', keep=False)]
    
    # Comparing Excel and DAPC DataFrames
    output = main(df_cchc, df_covid, df_syv, df_drs, df_dapc2)
    
    compare = datacompy.Compare(df_excel, df_dapc2, join_columns=['labid', 'rrid'],
                                df1_name='Original', df2_name='New')
    
    # Merging Excel and DAPC DataFrames
    df_final = pd.merge(df_excel, df_dapc2, how="left", on=["rrid", "labid"])

    df_final.columns = ['rrid', 'labid', 'retina_date', 'retina_exmnrid', 'spectralis_date', 'has_spectralis_oct', 'spectralis_tech',
                        'retina_results', 'retina_comments', 'dapc_dr', 'dapc_comments', 'dapc_dr_other']
    
    # Writing final DataFrame to an Excel file
    writer = pd.ExcelWriter(r'\\uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\TEMPDATA\ULTRASOUND DATA\RETINA\RETINAL WORKSHEET_032823.xlsx', engine='xlsxwriter')
    df_final.to_excel(writer, sheet_name='Sheet1', index=False)
    
    # Saving the Excel file
    writer.save()
