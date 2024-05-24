import pandas as pd
import datacompy


def main(df_Fibro,df_Fibro_Log1,df_Fibro_Log2,df_Fibro_Log3):
    

    #change column headers to lowercase
    df_Fibro_Log3.columns= df_Fibro_Log3.columns.str.lower()
    df_Fibro_Log2.columns= df_Fibro_Log2.columns.str.lower()
    df_Fibro_Log1.columns= df_Fibro_Log1.columns.str.lower()
    df_Fibro.columns= df_Fibro.columns.str.lower()
    #keep rrid and labid from DRS dataset

    
    df_Fibro_Log1['rrid'] = df_Fibro_Log1['rrid'].str.strip()
    df_Fibro_Log1['rrid'] = df_Fibro_Log1['rrid'].str.upper()
    
    df_Fibro_Log1['labid'] = df_Fibro_Log1['labid'].str.strip()
    df_Fibro_Log1['labid'] = df_Fibro_Log1['labid'].str.upper()
    
    df_Fibro_Log1=df_Fibro_Log1.loc[df_Fibro_Log1.labid.notnull()]
    
    df_Fibro_Log2['rrid'] = df_Fibro_Log2['rrid'].str.strip()
    df_Fibro_Log2['rrid'] = df_Fibro_Log2['rrid'].str.upper()
    
    df_Fibro_Log2['labid'] = df_Fibro_Log2['labid'].str.strip()
    df_Fibro_Log2['labid'] = df_Fibro_Log2['labid'].str.upper()
    
    df_Fibro_Log2=df_Fibro_Log2.loc[df_Fibro_Log2.labid.notnull()]
    
    df_Fibro_Log3=df_Fibro_Log3.loc[df_Fibro_Log2.labid.notnull()]
    
    df_Fibro_Log3=df_Fibro_Log3.loc[(df_Fibro_Log3['1st time fibroscan yes = 1 '] == 1) | (df_Fibro_Log3['follow up fibroscan     yes = 1'] == 1)]
    
    df_Fibro_Log=pd.concat([df_Fibro_Log1,df_Fibro_Log2,df_Fibro_Log3],sort=True,ignore_index=True)
    
    compare = datacompy.Compare(df_Fibro_Log,df_Fibro,join_columns=['LABID', 'RRID'], 

          
        # Optional, defaults to 'df1'
        df1_name = 'Original',
          
        # Optional, defaults to 'df2'
        df2_name = 'New' 
        )
    #Not in CCHC but in EXCEL
    notInLog=compare.df2_unq_rows
    notInFibro=compare.df1_unq_rows
    return notInLog,notInFibro,compare

if __name__ == "__main__":
    #bring in all of the datasets into python
    df_Fibro=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\FINAL DATA\ULTRASOUND DATA\FIBROSCAN\final_te.sas7bdat',encoding='latin1')
    df_Fibro_Log1 = pd.read_excel(r'U:\Research\BrownsvilleSD\public\Diabetes_Core\Data Request\McCormick\Req 032122( Plasma_DNA_RNA_URINE)\fibroscan\Liver_ultrasound participation_ Drs.JJ_Fallon.xlsx',sheet_name='Fibroscan=1')
    df_Fibro_Log2 = pd.read_excel(r'U:\Research\BrownsvilleSD\public\Diabetes_Core\Data Request\McCormick\Req 032122( Plasma_DNA_RNA_URINE)\fibroscan\Liver_ultrasound participation_ Drs.JJ_Fallon.xlsx',sheet_name='Followup Fibroscan=1')
    df_Fibro_Log3 = pd.read_excel(r'U:\Research\BrownsvilleSD\public\Diabetes_Core\Data Request\McCormick\Req 032122( Plasma_DNA_RNA_URINE)\fibroscan\ALL ULTRASOUND for PEDI SPREADSHEET.xlsx',sheet_name='Liver, Carotid & DXA')
    
    output=main(df_Fibro,df_Fibro_Log1,df_Fibro_Log2,df_Fibro_Log3)
    

    #output to excel file
    output[0].to_excel(r'U:\Research\BrownsvilleSD\public\Diabetes_Core\Data Request\McCormick\Req 032122( Plasma_DNA_RNA_URINE)\fibroscan\notInLog.xlsx', index = True)
    output[1].to_excel(r'U:\Research\BrownsvilleSD\public\Diabetes_Core\Data Request\McCormick\Req 032122( Plasma_DNA_RNA_URINE)\fibroscan\notInFibro.xlsx', index = True)
    
