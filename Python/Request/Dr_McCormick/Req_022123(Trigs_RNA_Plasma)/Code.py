import pandas as pd
import datacompy
from functools import reduce



if __name__ == "__main__":
    df_cchc=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\All_CCHC\cchc.sas7bdat',encoding='latin1')
    df_rna = pd.read_excel(r'\\uthouston.edu\uthsc\SPH\Users\hsoriano\Python\Req_082522(Overlapping_Geno_Pro_Lip_RNA)\Request_Isela_122222.xlsx',sheet_name='Sheet1')
    df_excel_Trig75 = pd.read_excel(r'\\uthouston.edu\uthsc\SPH\Users\hsoriano\Data Request\DR. McC\Req_022123(Trigs_RNA_Plasma)\Triglyceride extremes.xlsx',sheet_name='Trig<75')
    df_excel_Trig225 = pd.read_excel(r'\\uthouston.edu\uthsc\SPH\Users\hsoriano\Data Request\DR. McC\Req_022123(Trigs_RNA_Plasma)\Triglyceride extremes.xlsx',sheet_name='Trig>225')
    
    df_cchc.columns= df_cchc.columns.str.lower()
    
    df_excel_Trig75_nodups=df_excel_Trig75.drop_duplicates(subset=['rrid']).reset_index(drop=True)
    df_excel_Trig225_nodups=df_excel_Trig225.drop_duplicates(subset=['rrid']).reset_index(drop=True)
    
    df_rna=df_rna[['rrid','has_rnaseq']]
    
    df_excel_Trig75_rna=pd.merge(df_excel_Trig75, df_rna, how="left", on=["rrid"])
    df_excel_Trig225_rna=pd.merge(df_excel_Trig225, df_rna, how="left", on=["rrid"])
    
    df_cchc1=df_cchc[['rrid','labid','edta1','edta2','edta3','edta4','rna','has_pax']]
    
    df_cchc1.loc[(df_cchc1["edta1"].isin([1,3]) ) | ( df_cchc1["edta2"].isin([1,3]) )
                        | (df_cchc1["edta3"].isin([1,3])) | (df_cchc1["edta4"].isin([1,3])), "has_plasma"] = 1
    
    df_cchc1.loc[(df_cchc1["rna"].isin([1,3]) ) | ( df_cchc1["has_pax"].isin([1]) ), "has_rna"] = 1
    
    df_cchc2=df_cchc1[['rrid','labid','has_plasma','has_rna']]
    
    df_excel_Trig75_final=pd.merge(df_excel_Trig75_rna, df_cchc2, how="left", on=["rrid","labid"])
    df_excel_Trig225_final=pd.merge(df_excel_Trig225_rna, df_cchc2, how="left", on=["rrid","labid"])
    

    
    #output to excel file
    writer = pd.ExcelWriter(r'\\uthouston.edu\uthsc\SPH\Users\hsoriano\Data Request\DR. McC\Req_022123(Trigs_RNA_Plasma)\Req_022123(Trg_Extrems_rna_plasma).xlsx', engine='xlsxwriter')

    #df_Trig75_baseline.to_excel(writer, sheet_name='Baseline Trig<=75',startrow=1,index=False)
    df_excel_Trig75_final.to_excel(writer, sheet_name='Trig<75',index=False)

    #workbook  = writer.book
    #worksheet = writer.sheets['Baseline Trig<=75']

    #worksheet.write(0, 0, 'Baseline Trig<=75')
    
    df_excel_Trig225_final.to_excel(writer, sheet_name='Trig>225',index=False)
    
    writer.save()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
   