import pandas as pd
import datacompy
import pyodbc as pyodbc
import numpy as np
from functools import reduce

def extract(database):
    """
    Createds a dataframe from access table IMPACTLAB_A with RRID and LABID.

    Parameters
    ----------
    database : Dataframe
        Access dataframe.

    Returns
    -------
    df : Dataframe
        Returns extracted dataframe from table IMPACTLAB_A with rrid and labid.

    """
    query = "SELECT rrid,labid,todaydat FROM IMPACTLAB_A"
    cnxn = pyodbc.connect(database)
    df = pd.read_sql(query, cnxn)
    cnxn.close()
    return df

def extractHlcc(database):
    """
    Createds a dataframe from access table IMPACTLAB_A with RRID and LABID.

    Parameters
    ----------
    database : Dataframe
        Access dataframe.

    Returns
    -------
    df : Dataframe
        Returns extracted dataframe from table IMPACTLAB_A with rrid and labid.

    """
    query = "SELECT cpid,labid,todaydat FROM IMPACTLAB_A"
    cnxn = pyodbc.connect(database)
    df = pd.read_sql(query, cnxn)
    cnxn.close()
    return df

def create_main(df_cchc,df_covid,df_syv,df_drs,df_drs1):
    """
    Function that creates the main CCHC dataset

    Parameters
    ----------
    df_cchc : Dataframe
        Main access dataset for Brownsvill, Harlingen and Laredo.
    df_covid : Dataframe
        Covid dataset.
    df_syv : Dataframe
        Salud y Vida dataset.
    df_drs : Dataframe
        DRS dataset.

    Returns
    -------
    df_cchc : Dataframe
        Returns the combined dataset called main CCHC dataset.

    """
    
    df_cchc['labid'] = df_cchc['labid'].str.strip()
    df_cchc['labid'] = df_cchc['labid'].str.upper()
    
    df_cchc['rrid'] = df_cchc['rrid'].str.strip()
    df_cchc['rrid'] = df_cchc['rrid'].str.upper()

    df_cchc.rename(columns={'todaydat': 'interview_date'}, inplace=True)
    
    df_covid.columns= df_covid.columns.str.lower()
    df_syv.columns= df_syv.columns.str.lower()
    df_drs.columns= df_drs.columns.str.lower()

    df_covid=df_covid[['rrid','labid','interview_date']]
    df_syv=df_syv[['rrid','labid','interview_date']]
    #change column headers to lowercase
    df_drs.columns= df_drs.columns.str.lower()
    df_drs1.columns= df_drs1.columns.str.lower()
    
    df_drs1=df_drs1[['bdvisit','h_date']]
    
    df_drs=pd.merge(df_drs, df_drs1, how="left", on=["bdvisit"])
    
    df_drs.rename(columns={'h_date': 'interview_date'}, inplace=True)

    #keep rrid and labid from DRS dataset
    df_drs=df_drs[['rrid','labid','interview_date']]
    
    df_cchc=pd.concat([df_cchc,df_covid,df_syv,df_drs],sort=True,ignore_index=True)
    
    
    return df_cchc

if __name__ == "__main__":
    #bring in all of the datasets into python
    constr_bro = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\Uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\New_CCHC\Server\BD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    constr_har = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\New_CCHC\HRL\CCHC_HD_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    constr_lar = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\New_CCHC\LD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;'     
    constr_hlc = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\HLCC\HLCC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;'     

    df_covid=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\Covid\covid.sas7bdat',encoding='latin1')
    df_syv=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\SYV_Covid\syv_covid.sas7bdat',encoding='latin1')
    df_drs=pd.read_sas(r'\\uctnascifs.uthouston.edu\uthsc\sph\research\brownsvillesd\public\Diabetes_Core\MSAccess\TEMPDATA\DRS\crldata.sas7bdat',encoding='latin1')
    df_drs1=pd.read_sas(r'\\uctnascifs.uthouston.edu\uthsc\sph\research\brownsvillesd\public\Diabetes_Core\MSAccess\TEMPDATA\DRS\hemat_a.sas7bdat',encoding='latin1')
    
    df_hlcc=extractHlcc(constr_hlc)
    df_hlcc.rename(columns={'cpid': 'rrid'}, inplace=True)
    
    #extract only RRID and LABID from these datasets and combine
    df_cchc=pd.concat([extract(constr_bro),extract(constr_har),extract(constr_lar),df_hlcc],sort=True,ignore_index=True)
    
    df_cchc=create_main(df_cchc,df_covid,df_syv,df_drs,df_drs1)
    df_cchc=df_cchc.drop_duplicates(subset=['labid'])

    df_echo= pd.read_excel(r'\\Uthouston.edu\uthsc\SPH\Users\hsoriano\Data Request\Emma\Cleanup_Echo\EchoDataSpredsheet Oct 2022.xlsx',sheet_name='Sheet5')
    df_ultrasound = pd.read_excel(r'\\Uthouston.edu\uthsc\SPH\Research\CRU\LABORATORY CRU\LAB CRU Inventory\ALIQUOTS INVENTORY ALL studies\ULTRASOUNDS\ULTRASOUND.xlsx',sheet_name='Sheet2')
    
    df_cchc.columns= df_cchc.columns.str.lower()
    df_echo.columns= df_echo.columns.str.lower()
    df_ultrasound.columns= df_ultrasound.columns.str.lower()
    
    df_echo.loc[(df_echo['rrid'] == 'BD0332') & (df_echo['labid'] == '10Y0181')  ,'rrid'] = 'BD0532'
    df_echo.loc[(df_echo['rrid'] == 'BD0172') & (df_echo['labid'] == '10Y0198')  ,'rrid'] = 'BD1292'
    df_echo.loc[(df_echo['rrid'] == 'HD0002') & (df_echo['labid'] == '5HY0005')  ,'labid'] = '5YH0005'
    df_echo.loc[(df_echo['rrid'] == 'BD2231') & (df_echo['labid'] == '5Y1033')  ,'labid'] = '5Y1032'
    
    df_echo.loc[(df_echo['rrid'] == 'BD3076') & (df_echo['labid'] == '5Y1378')  ,'rrid'] = 'BD3078'
    df_echo.loc[(df_echo['rrid'] == 'BD3347') & (df_echo['labid'] == '5Y1497')  ,'rrid'] = 'BD3348'
    df_echo.loc[(df_echo['rrid'] == 'HD0021') & (df_echo['labid'] == '5YH0021')  ,'labid'] = '5YH0024'
    df_echo.loc[(df_echo['rrid'] == 'HD0487') & (df_echo['labid'] == '5YH0823')  ,'labid'] = '5YH0023'
    df_echo.loc[(df_echo['rrid'] == 'BD3013') & (df_echo['labid'] == 'BA0107')  ,'rrid'] = 'BD3014'
    df_echo.loc[(df_echo['rrid'] == 'BD3247') & (df_echo['labid'] == 'BA0297')  ,'rrid'] = 'BD3273'
    df_echo.loc[(df_echo['rrid'] == 'BD3612') & (df_echo['labid'] == 'BA0581')  ,'rrid'] = 'BD3616'
    df_echo.loc[(df_echo['rrid'] == 'BD3573') & (df_echo['labid'] == 'BA0883')  ,'labid'] = 'BA0553'
    df_echo.loc[(df_echo['rrid'] == 'BD3100') & (df_echo['labid'] == 'BD0163')  ,'labid'] = 'BA0163'
    df_echo.loc[(df_echo['rrid'] == 'BD3103') & (df_echo['labid'] == 'BD0166')  ,'labid'] = 'BA0166'
    df_echo.loc[(df_echo['rrid'] == 'BD3081') & (df_echo['labid'] == 'BD3081')  ,'labid'] = 'BA0148'
    df_echo.loc[(df_echo['rrid'] == 'BD3096') & (df_echo['labid'] == 'BD3096')  ,'labid'] = 'BA0159'
    df_echo.loc[(df_echo['rrid'] == 'BD2861') & (df_echo['labid'] == 'BD6861')  ,'labid'] = 'BA0016'
    df_echo.loc[(df_echo['rrid'] == 'BD2618') & (df_echo['labid'] == 'CV0072')  ,'labid'] = 'CV0076'
    df_echo.loc[(df_echo['rrid'] == 'HD0511') & (df_echo['labid'] == 'HA0191')  ,'rrid'] = 'HD0521'
    df_echo.loc[(df_echo['rrid'] == 'BD2974') & (df_echo['labid'] == 'UL9935')  ,'labid'] = 'BA0086'
    df_echo.loc[(df_echo['rrid'] == 'CP0015') & (df_echo['labid'] == 'CL0015')  ,'rrid'] = 'CP0006'
    df_echo.loc[(df_echo['rrid'] == 'CF0004.2') & (df_echo['labid'] == 'CL0006')  ,'rrid'] = 'CF0004'


    df_echo=df_echo.loc[df_echo.labid.notnull()]
    df_ultrasound=df_ultrasound.loc[df_ultrasound['ul #'].notnull()]
    
    df_echo_ul = df_echo.loc[df_echo['labid'].str.contains('UL', regex=True, na=True)]
    df_echo_ul['UL_labid'] =  df_echo_ul['labid'] 
    
    df_echo_noul=df_echo.loc[~df_echo['labid'].isin(df_echo_ul['labid'])]
    
    df_echo_ul['labid'] = df_echo_ul['labid'].map(df_ultrasound.set_index('ul #')['last lab id'])
    
    df_echo_final=pd.concat([df_echo_noul,df_echo_ul],ignore_index=True)
    
    df_compare=df_cchc[['rrid','labid']]
    
    
    compare = datacompy.Compare(df_compare,df_echo_final,join_columns=['LABID', 'RRID'], 

          
        # Optional, defaults to 'df1'
        df1_name = 'Original',
          
        # Optional, defaults to 'df2'
        df2_name = 'New' 
        )
    #Not in CCHC but in EXCEL
    notInCCHC=compare.df2_unq_rows
    
    
    df_echo_final=pd.merge(df_echo_final, df_cchc, how="left", on=["labid","rrid"])

    df_echo_final = df_echo_final.sort_values(['rrid','interview_date'],ascending=True)
    df_echo_final['visit'] = np.where(df_echo_final['interview_date'].notna(), df_echo_final.groupby(['rrid']).cumcount().add(1), 1)

    df_dups= df_echo_final[df_echo_final.duplicated(['rrid'], keep=False)]
    df_dups = df_dups[['rrid','labid','interview_date','visit']].sort_values(['rrid','interview_date'],ascending=True)
    

    #output to excel file
    writer = pd.ExcelWriter(r'\\Uthouston.edu\uthsc\SPH\Users\hsoriano\Data Request\Emma\Cleanup_Echo\Labid_Cleanup.xlsx', engine='xlsxwriter')

    #df_Trig75_baseline.to_excel(writer, sheet_name='Baseline Trig<=75',startrow=1,index=False)

    #workbook  = writer.book
    #worksheet = writer.sheets['Baseline Trig<=75']

    #worksheet.write(0, 0, 'Baseline Trig<=75')
    
    notInCCHC.to_excel(writer, sheet_name='Needs_Cleanup',index=False)

    df_echo_final.to_excel(writer, sheet_name='With_visit',index=False)
    #worksheet = writer.sheets['Baseline 75<Trig<=150']

    #worksheet.write(0, 0, 'Baseline 75<Trig<=150')
    
   
    # Close the Pandas Excel writer and output the Excel file.
    writer.save()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
   