import pandas as pd
import datacompy
import numpy as np


if __name__ == "__main__":
    #Bring in information from both excel files
    df_CCHC = pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\All_CCHC\cchc.sas7bdat',encoding='latin1')
    df_drs_inv= pd.read_excel(r'U:\Research\CRU\LABORATORY CRU\LAB CRU Inventory\ALIQUOTS INVENTORY ALL studies\DRS\DRS INVENTORY.xlsx', sheet_name='Sheet1')
    df_excel_hea= pd.read_excel(r'C:\Users\hsoriano\Documents\Python\Data Analyst Portfolio\Python\Request\Heather\Reg_051523\LABIDsDKD.xlsx', sheet_name='LABIDsDKD')
    df_excel_marc= pd.read_excel(r'C:\Users\hsoriano\Documents\Python\Data Analyst Portfolio\Python\Request\Heather\Reg_051523\URINE SENT TO CRL 11-16-15 MM.xlsx', sheet_name='Sheet1')

    df_CCHC.columns=df_CCHC.columns.str.lower()
    df_excel_hea.columns=df_excel_hea.columns.str.lower()
    df_drs_inv.columns=df_drs_inv.columns.str.lower()
    
    column_list=list(df_drs_inv.columns)  
    df_drs_inv.rename(columns={"dr #": "labid", "bd#": "rrid",
                                 "plasma      1.1":"plasma_1.1","plasma 1.2 ":"plasma_1.2",
                                 "plasma 1.3":"plasma_1.3","serum      1 ":"serum_1",
                                 "serum     2":"serum_2","serum     3":"serum_3",
                                 "serum     4":"serum_4",
                                 "plasma 2.1":"plasma_2.1","plasma 2.2":"plasma_2.2",
                                 "plasma 2.3":"plasma_2.3","plasma 2.4":"plasma_2.4",
                                 "plasma 2.5":"plasma_2.5","plasma 2.6":"plasma_2.6",
                                 "plasma 2.7":"plasma_2.7","plasma 2.8":"plasma_2.8",
                                 "plasma 3.1 ":"plasma_3.1","plasma 3.2":"plasma_3.2",
                                 "plasma 3.3":"plasma_3.3"},
                        inplace=True)
    
    

    df_samples=df_CCHC[['rrid','labid','urinsamp','visit']]

    
    df_excel_marc=df_excel_marc.loc[~df_excel_marc['RRID'].isna()]
    df_excel_marc=df_excel_marc[['RRID']]
    
    df_excel_marc["Used_urine"] =1
    df_cchc_labids_marc1=df_samples.loc[df_samples.rrid.isin(df_excel_marc.RRID)]
    df_cchc_labids_marc2=df_samples.loc[df_samples.labid.isin(df_excel_marc.RRID)]
    
    df_samples['updated_urinsamp'] = df_samples['urinsamp']
    df_samples.loc[df_samples.labid.isin(df_excel_marc.RRID),'updated_urinsamp']=2
    
    df_final=df_excel_hea.merge(df_samples,how="left",on=['labid','rrid'])
    

    
    

