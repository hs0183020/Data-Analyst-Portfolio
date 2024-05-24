import pandas as pd
import datacompy
import pyodbc as pyodbc


def extract(database):
    query = "SELECT rrid,labid FROM IMPACTLAB_A"
    cnxn = pyodbc.connect(database)
    df = pd.read_sql(query, cnxn)
    cnxn.close()
    return df
    

def main(df_cchc,df_covid,df_syv,df_drs,df_excel):
    
    df_covid=df_covid[['rrid','labid']]
    df_syv=df_syv[['rrid','labid']]
    #change column headers to lowercase
    df_drs.columns= df_drs.columns.str.lower()
    #keep rrid and labid from DRS dataset
    df_drs=df_drs[['rrid','labid']]
    df_cchc=pd.concat([df_cchc,df_covid,df_syv,df_drs],sort=True,ignore_index=True)
    
    df_excel['rrid'] = df_excel['rrid'].str.strip()
    df_excel['rrid'] = df_excel['rrid'].str.upper()
    
    df_excel['labid'] = df_excel['labid'].str.strip()
    df_excel['labid'] = df_excel['labid'].str.upper()
    
    
    compare = datacompy.Compare(df_cchc,df_excel,join_columns=['LABID', 'RRID'], 

          
        # Optional, defaults to 'df1'
        df1_name = 'Original',
          
        # Optional, defaults to 'df2'
        df2_name = 'New' 
        )
    #Not in CCHC but in EXCEL
    notInCCHC=compare.df2_unq_rows
    
    return df_cchc,notInCCHC,compare

if __name__ == "__main__":
    #bring in all of the datasets 
    constr_bro = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\New_CCHC\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    constr_har = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\New_CCHC\HRL\CCHC_HD_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    constr_lar = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\New_CCHC\LD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;'     
    df_covid=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\Covid\covid.sas7bdat',encoding='latin1')
    df_syv=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\SYV_Covid\syv_covid.sas7bdat',encoding='latin1')
    df_drs=pd.read_sas(r'\\uctnascifs.uthouston.edu\uthsc\sph\research\brownsvillesd\public\Diabetes_Core\MSAccess\TEMPDATA\DRS\crldata.sas7bdat',encoding='latin1')
    
    df_excel_cchc = pd.read_excel(r'H:\Python\Clean_Up_Covid_File\Specimen Collected since Re-opening the CRU RU.xlsx',sheet_name='Cohort COVID')
    df_excel_syv = pd.read_excel(r'H:\Python\Clean_Up_Covid_File\Specimen Collected since Re-opening the CRU RU.xlsx',sheet_name='Salud y Vida COVID')
    df_excel_com = pd.read_excel(r'H:\Python\Clean_Up_Covid_File\Specimen Collected since Re-opening the CRU RU.xlsx',sheet_name='COMUNITY')
    df_excel_hos = pd.read_excel(r'H:\Python\Clean_Up_Covid_File\Specimen Collected since Re-opening the CRU RU.xlsx',sheet_name='Hospital COVID')
    df_excel_SC = pd.read_excel(r'H:\Python\Clean_Up_Covid_File\Specimen Collected since Re-opening the CRU RU.xlsx',sheet_name='Su Clinica')
    
    #create one dataframe from all excel sheets 
    df_excel=pd.concat([df_excel_cchc[['rrid','labid']],df_excel_syv[['rrid','labid']],df_excel_com[['rrid','labid']],df_excel_hos[['rrid','labid']],df_excel_SC[['rrid','labid']]],sort=True,ignore_index=True)
    #remove null rows
    df_excel=df_excel.loc[df_excel.labid.notnull()]
    #find duplicates in excel
    df_dups = df_excel[df_excel.duplicated(['labid'], keep=False)]
    #drop duplicates
    df_excel=df_excel.drop_duplicates(subset=['labid'])
    #extract only RRID and LABID from these datasets and combine
    df_cchc=pd.concat([extract(constr_bro),extract(constr_har),extract(constr_lar)],sort=True,ignore_index=True)
     

    output=main(df_cchc,df_covid,df_syv,df_drs,df_excel)
    

    #output to excel file
    output[0].to_excel(r'H:\Python\Clean_Up_Covid_File\Correct.xlsx', index = True)
    output[1].to_excel(r'H:\Python\Clean_Up_Covid_File\Errors.xlsx', index = True)
    
