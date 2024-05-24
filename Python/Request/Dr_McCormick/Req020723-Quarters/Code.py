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
    df_cchc.trig.fillna(df_cchc.trigs, inplace=True)
    df_cchc['interview_date'] = pd.to_datetime(df_cchc['interview_date']).dt.date
    
    df_trigs=df_cchc[['rrid','labid','study','age_at_visit','gender','bmi1','trig','homa_ir','mets_idf','ada2010_cat']]
    df_Trig75=df_trigs.loc[( df_trigs['trig'] <= 75 )]
    df_Trig75_150=df_trigs.loc[( df_trigs['trig'] > 75 ) & ( df_trigs['trig'] <= 150 )]
    df_Trig150_225=df_trigs.loc[( df_trigs['trig'] > 150) & ( df_trigs['trig'] <= 225 )]
    df_Trig225=df_trigs.loc[( df_trigs['trig'] > 225) ]
    
    output=createBaseFollowupDF(df_Trig75,df_trigs)
    df_Trig75_baseline=output[0]
    df_Trig75_followup=output[1]
    
    output=createBaseFollowupDF(df_Trig75_150,df_trigs)
    df_Trig75_150_baseline=output[0]
    df_Trig75_150_followup=output[1]
    
    output=createBaseFollowupDF(df_Trig150_225,df_trigs)
    df_Trig150_225_baseline=output[0]
    df_Trig150_225_followup=output[1]
    
    output=createBaseFollowupDF(df_Trig225,df_trigs)
    df_Trig225_baseline=output[0]
    df_Trig225_followup=output[1]
    

    df_ldl=df_cchc[['rrid','labid','study','age_at_visit','gender','bmi1','ldlcalc','homa_ir','mets_idf','ada2010_cat']]
    df_ldl50=df_ldl.loc[( df_ldl['ldlcalc'] <= 50 )]
    df_ldl50_100=df_ldl.loc[( df_ldl['ldlcalc'] > 50 ) & ( df_ldl['ldlcalc'] <= 100 )]
    df_ldl100_150=df_ldl.loc[( df_ldl['ldlcalc'] > 100) & ( df_ldl['ldlcalc'] <= 150 )]
    df_ldl150=df_ldl.loc[( df_ldl['ldlcalc'] > 150) ]
    
    output=createBaseFollowupDF(df_ldl50,df_ldl)
    df_ldl50_baseline=output[0]
    df_ldl50_followup=output[1]
    
    output=createBaseFollowupDF(df_ldl50_100,df_ldl)
    df_ldl50_100_baseline=output[0]
    df_ldl50_100_followup=output[1]
    
    output=createBaseFollowupDF(df_ldl100_150,df_ldl)
    df_ldl100_150_baseline=output[0]
    df_ldl100_150_followup=output[1]
    
    output=createBaseFollowupDF(df_ldl150,df_ldl)
    df_ldl150_baseline=output[0]
    df_ldl150_followup=output[1]
    
    
    #output to excel file
    writer = pd.ExcelWriter(r'U:\Users\hsoriano\Python\Dr_McCormick\Req020723-Quarters\Request_Quarter_Trig_LDL_020723v2.xlsx', engine='xlsxwriter')

    #df_Trig75_baseline.to_excel(writer, sheet_name='Baseline Trig<=75',startrow=1,index=False)
    df_Trig75_baseline.to_excel(writer, sheet_name='Baseline Trig<=75',index=False)

    #workbook  = writer.book
    #worksheet = writer.sheets['Baseline Trig<=75']

    #worksheet.write(0, 0, 'Baseline Trig<=75')
    
    df_Trig75_150_baseline.to_excel(writer, sheet_name='Baseline 75<Trig<=150',index=False)
    df_Trig150_225_baseline.to_excel(writer, sheet_name='Baseline 150<Trig<=225',index=False)
    df_Trig225_baseline.to_excel(writer, sheet_name='Baseline Trig>225',index=False)
    #worksheet = writer.sheets['Baseline 75<Trig<=150']

    #worksheet.write(0, 0, 'Baseline 75<Trig<=150')
    
    df_Trig75_followup.to_excel(writer, sheet_name='Followup Trig<=75',index=False)
    df_Trig75_150_followup.to_excel(writer, sheet_name='Followup 75<Trig<=150',index=False)
    df_Trig150_225_followup.to_excel(writer, sheet_name='Followup 150<Trig<=225',index=False)
    df_Trig225_followup.to_excel(writer, sheet_name='Followup Trig>225',index=False)
    
    df_ldl50_baseline.to_excel(writer, sheet_name='Baseline LDL<= 50 ',index=False)
    df_ldl50_100_baseline.to_excel(writer, sheet_name='Baseline 50<LDL<=100 ',index=False)
    df_ldl100_150_baseline.to_excel(writer, sheet_name='Baseline 100<LDL<=150 ',index=False)
    df_ldl150_baseline.to_excel(writer, sheet_name='Baseline LDL>150 ',index=False)
    
    df_ldl50_followup.to_excel(writer, sheet_name='Followup LDL<= 50 ',index=False)
    df_ldl50_100_followup.to_excel(writer, sheet_name='Followup 50<LDL<=100 ',index=False)
    df_ldl100_150_followup.to_excel(writer, sheet_name='Followup 100<LDL<=150 ',index=False)
    df_ldl150_followup.to_excel(writer, sheet_name='Followup LDL>150 ',index=False)
    # Close the Pandas Excel writer and output the Excel file.
    writer.save()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
   