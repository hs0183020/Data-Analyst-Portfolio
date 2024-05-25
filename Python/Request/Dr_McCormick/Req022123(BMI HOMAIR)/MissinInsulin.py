import pandas as pd
import datacompy
from functools import reduce



if __name__ == "__main__":
    df_cchc=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\All_CCHC\cchc.sas7bdat',encoding='latin1')
    df_insulin=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\LABORATORY_RAHC\INSULIN\CCHC\final_insulin.sas7bdat',encoding='latin1')

    df_cchc.columns= df_cchc.columns.str.lower()
    df_insulin.columns= df_insulin.columns.str.lower()
    
    df_homair=df_cchc[['rrid','homa_ir','ins','labid','mfbg']]
    
    #has homa-ir
    df_homair1=df_homair.loc[df_homair.homa_ir.notnull()]
    #missing homa-ir
    df_missingHomaIr=df_homair.loc[df_homair.homa_ir.isnull()]
    #missing because of missing ins
    df_missingIns=df_homair.loc[df_homair.ins.isnull()]
    #missing because of mfbg
    df_missingInsMfbg=df_homair.loc[(df_homair.ins.isnull()) & (df_homair.mfbg.isnull())]
    
    

    
    #output to excel file
    writer = pd.ExcelWriter(r'U:\Users\hsoriano\Data Request\DR. McC\Req022123(BMI HOMAIR)\Missing_HomaIR.xlsx', engine='xlsxwriter')

    #df_Trig75_baseline.to_excel(writer, sheet_name='Baseline Trig<=75',startrow=1,index=False)
    df_missingHomaIr.to_excel(writer, sheet_name='MissingHomaIR',index=False)

    #workbook  = writer.book
    #worksheet = writer.sheets['Baseline Trig<=75']

    #worksheet.write(0, 0, 'Baseline Trig<=75')
    
    df_missingIns.to_excel(writer, sheet_name='MissingIns',index=False)
    df_missingInsMfbg.to_excel(writer, sheet_name='MissingMFBG',index=False)

    #worksheet = writer.sheets['Baseline 75<Trig<=150']

    #worksheet.write(0, 0, 'Baseline 75<Trig<=150')
    
   
    # Close the Pandas Excel writer and output the Excel file.
    writer.save()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
   