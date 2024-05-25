import pandas as pd
import datacompy
from functools import reduce

#cleans up data sheet from regular fibroscan file 
def createBaseFollowupDF(df,df_main):
    
    df_baseline=df[df['study'].isin(['BASELINE','PEDIATRIC BASELINE'])]
    df_main_baseline=df_main.loc[df_main.rrid.isin(df_baseline['rrid'])]
    df_dups=df_main_baseline.loc[df_main_baseline.duplicated(subset='rrid', keep=False)]
    
    df_followup=df_dups.groupby('rrid').filter(lambda x: any(x.study.isin(['BASELINE','PEDIATRIC BASELINE'])))
    
    return df_baseline,df_followup

if __name__ == "__main__":
    df_cchc=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\All_CCHC\cchc.sas7bdat',encoding='latin1')

    df_cchc.columns= df_cchc.columns.str.lower()
    
    df_cchc.bmi1.fillna(df_cchc.pd_bmi, inplace=True)
    df_cchc['interview_date'] = pd.to_datetime(df_cchc['interview_date']).dt.date
    
    df_homair_bmi=df_cchc[['rrid','labid','study','age_at_visit','gender','bmi1','homa_ir','mets_idf','ada2010_cat']]

    df_homair_bmi_1=df_homair_bmi.loc[( df_homair_bmi['homa_ir'] <= 1.75 ) & ( df_homair_bmi['bmi1'] >= 30 )]
    df_homair_bmi_2=df_homair_bmi.loc[( df_homair_bmi['homa_ir'] >= 4.8 ) & ( df_homair_bmi['bmi1'] < 28 )]

    
    output=createBaseFollowupDF(df_homair_bmi_1,df_homair_bmi)
    df_homair_bmi_1_baseline=output[0]
    df_homair_bmi_1_followup=output[1]
    
    output=createBaseFollowupDF(df_homair_bmi_2,df_homair_bmi)
    df_homair_bmi_2_baseline=output[0]
    df_homair_bmi_2_followup=output[1]
    

    
    #output to excel file
    writer = pd.ExcelWriter(r'U:\Users\hsoriano\Data Request\DR. McC\Req022123(BMI HOMAIR)\Req_HOMAIR_BMI_022123.xlsx', engine='xlsxwriter')

    #df_Trig75_baseline.to_excel(writer, sheet_name='Baseline Trig<=75',startrow=1,index=False)
    df_homair_bmi_1_baseline.to_excel(writer, sheet_name='Baseline HOMAIR<=1.75 BMI>=30',index=False)

    #workbook  = writer.book
    #worksheet = writer.sheets['Baseline Trig<=75']

    #worksheet.write(0, 0, 'Baseline Trig<=75')
    
    df_homair_bmi_1_followup.to_excel(writer, sheet_name='Followup HOMAIR<=1.75 BMI>=30',index=False)
    df_homair_bmi_2_baseline.to_excel(writer, sheet_name='Baseline HOMAIR>=4.8 BMI<28',index=False)
    df_homair_bmi_2_followup.to_excel(writer, sheet_name='Followup HOMAIR>=4.8 BMI<28',index=False)
    #worksheet = writer.sheets['Baseline 75<Trig<=150']

    #worksheet.write(0, 0, 'Baseline 75<Trig<=150')
    
   
    # Close the Pandas Excel writer and output the Excel file.
    writer.save()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
   