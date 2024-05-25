import pandas as pd
import datacompy
import pyodbc as pyodbc


def extract(database):
    query = "SELECT rrid,labid FROM IMPACTLAB_A"
    cnxn = pyodbc.connect(database)
    df = pd.read_sql(query, cnxn)
    cnxn.close()
    return df
    

def createMain(df_cchc,df_covid,df_syv,df_drs):
    
    df_covid=df_covid[['rrid','labid']]
    df_syv=df_syv[['rrid','labid']]
    #change column headers to lowercase
    df_drs.columns= df_drs.columns.str.lower()
    #keep rrid and labid from DRS dataset
    df_drs=df_drs[['rrid','labid']]
    df_cchc=pd.concat([df_cchc,df_covid,df_syv,df_drs],sort=True,ignore_index=True)
    
    

    return df_cchc    

def compareMainRetna(df_cchc,df_excel):
    
    df_excel['rrid'] = df_excel['rrid'].str.strip()
    df_excel['rrid'] = df_excel['rrid'].str.upper()
    
    df_excel['labid'] = df_excel['labid'].str.strip()
    df_excel['labid'] = df_excel['labid'].str.upper()
    
    #df_excel=df_excel.loc[df_excel.labid.notnull()]
    
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
    #bring in all of the datasets into python
    constr_bro = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\BrownsvilleSD\public\Diabetes_Core\New_CCHC\Server\BD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    constr_har = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\New_CCHC\HRL\CCHC_HD_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    constr_lar = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\New_CCHC\LD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;'     
    df_covid=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\Covid\covid.sas7bdat',encoding='latin1')
    df_syv=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\SYV_Covid\syv_covid.sas7bdat',encoding='latin1')
    df_drs=pd.read_sas(r'\\uctnascifs.uthouston.edu\uthsc\sph\research\brownsvillesd\public\Diabetes_Core\MSAccess\TEMPDATA\DRS\crldata.sas7bdat',encoding='latin1')
    
    df_retina = pd.read_excel(r'U:\Users\hsoriano\Data Request\Dr Padovani\Req_021023\RETINAL and OCT WORKSHEET.xlsx',sheet_name='Retina worksheet')

    df_retina.rename(columns={'BD': 'rrid', 'LAB ID': 'labid'}, inplace=True)
    #remove null rows
    df_retina=df_retina.loc[df_retina.labid.notnull()]
    #find duplicates in excel
    df_dups = df_retina[df_retina.duplicated(['labid'], keep=False)]
    #drop duplicates
    df_retina1=df_retina.drop_duplicates(subset=['labid'])
    #extract only RRID and LABID from these datasets and combine
    df_cchc=pd.concat([extract(constr_bro),extract(constr_har),extract(constr_lar)],sort=True,ignore_index=True)
     
    
    #corrections
    #df_retina1.loc[(df_retina1['rrid'] == "3BD1776"), "rrid"] = "BD1776"
    df_retina1.loc[(df_retina1['rrid'] == "BD3088") & (df_retina1['labid'] == "UL0056"), "labid"] = "BA0154"
    df_retina1.loc[(df_retina1['rrid'] == "BD2808") & (df_retina1['labid'] == "UL0061"),"labid" ] = "BD6808"
    df_retina1.loc[(df_retina1['rrid'] == "BD2811") & (df_retina1['labid'] == "UL0062"),"labid" ] = "BD6811"
    df_retina1.loc[(df_retina1['rrid'] == "BD2868") & (df_retina1['labid'] == "UL0065"),"labid" ] = "BA0023"
    df_retina1.loc[(df_retina1['rrid'] == "BD2998") & (df_retina1['labid'] == "UL0069"),"labid" ] = "BA0098"
    df_retina1.loc[(df_retina1['rrid'] == "BD2065") & (df_retina1['labid'] == "5Y1121 / UL0072"),"labid" ] = "5Y1121"
    df_retina1.loc[(df_retina1['rrid'] == "BD3522") & (df_retina1['labid'] == "BA0507 / UL0070"),"labid" ] = "BA0507"
    df_retina1.loc[(df_retina1['rrid'] == "BD1470") & (df_retina1['labid'] == "UL0072"),"labid" ] = "10Y0216"
    
    df_retina1.loc[(df_retina1['rrid'] == "BD0359") & (df_retina1['labid'] == "10Y1018"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD1318") & (df_retina1['labid'] == "10Y0230"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD1942") & (df_retina1['labid'] == "10Y0343"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD3625") & (df_retina1['labid'] == "BA0858"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD0394") & (df_retina1['labid'] == "10Y407"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD2500") & (df_retina1['labid'] == "10Y0520"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD3087") & (df_retina1['labid'] == "55Y1403"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD3337") & (df_retina1['labid'] == "BA0666"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD1379") & (df_retina1['labid'] == "15Y0188"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD1734") & (df_retina1['labid'] == "15Y0211"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD1877") & (df_retina1['labid'] == "15Y0255"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD2773") & (df_retina1['labid'] == "10Y0585"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD0547") & (df_retina1['labid'] == "15Y0261"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD0331") & (df_retina1['labid'] == "20Y0011"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD1161") & (df_retina1['labid'] == "15Y0269"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD1250") & (df_retina1['labid'] == "15Y0268"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD2685") & (df_retina1['labid'] == "10Y0601"),"labid" ] = ""
    df_retina1.loc[(df_retina1['rrid'] == "BD2576") & (df_retina1['labid'] == "10Y0600"),"labid" ] = ""

    indexNames = df_retina1[df_retina1['labid'].isin(['DATE'])].index
    indexNames1 = df_retina1[df_retina1['rrid'].isin(['RETINAL'])].index
    df_retina1 = df_retina1.drop(indexNames)
    df_retina1 = df_retina1.drop(indexNames1)
    
    df_cchc=createMain(df_cchc,df_covid,df_syv,df_drs)
    output=compareMainRetna(df_cchc,df_retina1)
    

    #output to excel file
    df_cchc.to_excel(r'U:\Users\hsoriano\Data Request\Dr Padovani\Req_021023\cchc.xlsx', index = False)
    df_dups.to_excel(r'U:\Users\hsoriano\Data Request\Dr Padovani\Req_021023\dups.xlsx', index = True)
    df_retina1.to_excel(r'U:\Users\hsoriano\Data Request\Dr Padovani\Req_021023\Correct.xlsx', index = True)
    output[1].to_excel(r'U:\Users\hsoriano\Data Request\Dr Padovani\Req_021023\Errors.xlsx', index = True)
    
