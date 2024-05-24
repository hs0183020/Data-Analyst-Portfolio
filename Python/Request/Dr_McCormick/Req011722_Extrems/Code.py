import pandas as pd
import datacompy
from functools import reduce




if __name__ == "__main__":
    df_cchc=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\All_CCHC\cchc.sas7bdat',encoding='latin1')

    df_cchc.columns= df_cchc.columns.str.lower()
    
    df_cchc.bmi1.fillna(df_cchc.pd_bmi, inplace=True)
    df_cchc.trig.fillna(df_cchc.trigs, inplace=True)
    df_cchc['interview_date'] = pd.to_datetime(df_cchc['interview_date']).dt.date
    
    HiBMIlowTrig75=df_cchc[['rrid','labid','visit','age_at_visit','gender','bmi1','trig','interview_date']]
    HiBMIlowTrig75=HiBMIlowTrig75.loc[(HiBMIlowTrig75['bmi1'] > 32) & ( HiBMIlowTrig75['trig'] <= 75 )]
    
    HiBMIlowTrig75_dups=HiBMIlowTrig75.loc[HiBMIlowTrig75.duplicated(subset='rrid', keep=False)]
    
    HiBMIhighHDL60=df_cchc[['rrid','labid','visit','age_at_visit','gender','bmi1','hdlc','interview_date']]
    HiBMIhighHDL60=HiBMIhighHDL60.loc[(HiBMIhighHDL60['bmi1'] > 32) & ( HiBMIhighHDL60['hdlc'] >= 60 )]
    
    HiBMIhighHDL60_dups=HiBMIhighHDL60.loc[HiBMIhighHDL60.duplicated(subset='rrid', keep=False)]
    
    HiBMIhlowLDL50=df_cchc[['rrid','labid','visit','age_at_visit','gender','bmi1','ldlcalc','interview_date']]
    HiBMIhlowLDL50=HiBMIhlowLDL50.loc[(HiBMIhlowLDL50['bmi1'] > 32) & ( HiBMIhlowLDL50['ldlcalc'] <= 50 )]
    
    HiBMIhlowLDL50_dups=HiBMIhlowLDL50.loc[HiBMIhlowLDL50.duplicated(subset='rrid', keep=False)]
    
    LowBMILowHDL40=df_cchc[['rrid','labid','visit','age_at_visit','gender','bmi1','hdlc','interview_date']]
    LowBMILowHDL40=LowBMILowHDL40.loc[(LowBMILowHDL40['bmi1'] < 28) & ( LowBMILowHDL40['hdlc'] < 40 )]
    
    LowBMILowHDL40_dups=LowBMILowHDL40.loc[LowBMILowHDL40.duplicated(subset='rrid', keep=False)]
    
    LowBMIHiLDL150=df_cchc[['rrid','labid','visit','age_at_visit','gender','bmi1','ldlcalc','interview_date']]
    LowBMIHiLDL150=LowBMIHiLDL150.loc[(LowBMIHiLDL150['bmi1'] < 28) & ( LowBMIHiLDL150['ldlcalc'] > 150 )]
    
    LowBMIHiLDL150_dups=LowBMIHiLDL150.loc[LowBMIHiLDL150.duplicated(subset='rrid', keep=False)]
    
    LowBMIHiTrig300=df_cchc[['rrid','labid','visit','age_at_visit','gender','bmi1','trig','interview_date']]
    LowBMIHiTrig300=LowBMIHiTrig300.loc[(LowBMIHiTrig300['bmi1'] < 26) & ( LowBMIHiTrig300['trig'] > 300 )]
    
    LowBMIHiTrig300_dups=LowBMIHiTrig300.loc[LowBMIHiTrig300.duplicated(subset='rrid', keep=False)]
    
    
    BMI30HOMA_IR3=df_cchc[['rrid','labid','visit','age_at_visit','gender','bmi1','homa_ir','interview_date']]
    BMI30HOMA_IR3=BMI30HOMA_IR3.loc[(BMI30HOMA_IR3['bmi1'] < 30) & ( BMI30HOMA_IR3['homa_ir'] >= 3 )]
    
    BMI30HOMA_IR3_dups=BMI30HOMA_IR3.loc[BMI30HOMA_IR3.duplicated(subset='rrid', keep=False)]
    
    BMI30HOMA_IR2=df_cchc[['rrid','labid','visit','age_at_visit','gender','bmi1','homa_ir','interview_date']]
    BMI30HOMA_IR2=BMI30HOMA_IR2.loc[(BMI30HOMA_IR2['bmi1'] >= 30) & ( BMI30HOMA_IR2['homa_ir'] < 2 )]
    
    BMI30HOMA_IR2_dups=BMI30HOMA_IR2.loc[BMI30HOMA_IR2.duplicated(subset='rrid', keep=False)]
    
    HOMA_IR3FBG100=df_cchc[['rrid','labid','visit','age_at_visit','gender','mfbg','homa_ir','interview_date']]
    HOMA_IR3FBG100=HOMA_IR3FBG100.loc[(HOMA_IR3FBG100['mfbg'] < 100) & ( HOMA_IR3FBG100['homa_ir'] >= 3 )]
    
    HOMA_IR3FBG100_dups=HOMA_IR3FBG100.loc[HOMA_IR3FBG100.duplicated(subset='rrid', keep=False)]
    
    HOMA_IR2FBG100=df_cchc[['rrid','labid','visit','age_at_visit','gender','mfbg','homa_ir','interview_date']]
    HOMA_IR2FBG100=HOMA_IR2FBG100.loc[(HOMA_IR2FBG100['mfbg'] >= 100) & ( HOMA_IR2FBG100['homa_ir'] <= 2 )]
    
    HOMA_IR2FBG100_dups=HOMA_IR2FBG100.loc[HOMA_IR2FBG100.duplicated(subset='rrid', keep=False)]
    
    METS_IDF_IR3=df_cchc[['rrid','labid','visit','age_at_visit','gender','mets_idf','homa_ir','interview_date']]
    METS_IDF_IR3=METS_IDF_IR3.loc[(METS_IDF_IR3['mets_idf'] == 0) & ( METS_IDF_IR3['homa_ir'] >= 3 )]
    
    METS_IDF_IR3_dups=METS_IDF_IR3.loc[METS_IDF_IR3.duplicated(subset='rrid', keep=False)]
    
    METS_IDF_IR2=df_cchc[['rrid','labid','visit','age_at_visit','gender','mets_idf','homa_ir','interview_date']]
    METS_IDF_IR2=METS_IDF_IR2.loc[(METS_IDF_IR2['mets_idf'] == 1) & ( METS_IDF_IR2['homa_ir'] <= 2 )]
    
    METS_IDF_IR2_dups=METS_IDF_IR2.loc[METS_IDF_IR2.duplicated(subset='rrid', keep=False)]
    
    #output to excel file
    writer = pd.ExcelWriter(r'U:\Users\hsoriano\Python\Dr_McCormick\Req011722_Extrems\Extremes of BMI and lipids multiple visit _Req011723.xlsx', engine='xlsxwriter')

    HiBMIlowTrig75.to_excel(writer, sheet_name='Hi BMI low trig<=75',index=False)
    HiBMIlowTrig75_dups.to_excel(writer, sheet_name='Hi BMI low trig<=75_dups',index=False)
    HiBMIhighHDL60.to_excel(writer, sheet_name='Hi BMI high HDL>=60',index=False)
    HiBMIhighHDL60_dups.to_excel(writer, sheet_name='Hi BMI high HDL>=60_dups',index=False)
    HiBMIhlowLDL50.to_excel(writer, sheet_name='Hi BMI low LDL<=50',index=False)
    HiBMIhlowLDL50_dups.to_excel(writer, sheet_name='Hi BMI low LDL<=50_dups',index=False)
    LowBMILowHDL40.to_excel(writer, sheet_name='LowBMI Low HDL<40',index=False)
    LowBMILowHDL40_dups.to_excel(writer, sheet_name='LowBMI Low HDL<40_dups',index=False)
    LowBMIHiLDL150.to_excel(writer, sheet_name='lowBMI HI LDL>150',index=False)
    LowBMIHiLDL150_dups.to_excel(writer, sheet_name='lowBMI HI LDL>150_dups',index=False)
    LowBMIHiTrig300.to_excel(writer, sheet_name='LowBMI <=26 Hitrig>300',index=False)
    LowBMIHiTrig300_dups.to_excel(writer, sheet_name='LowBMI <=26 Hitrig>300_dups',index=False)
    
    BMI30HOMA_IR3.to_excel(writer, sheet_name='IR>=3.0 & BMI<30',index=False)
    BMI30HOMA_IR3_dups.to_excel(writer, sheet_name='IR>=3.0 & BMI<30_dups',index=False)
    BMI30HOMA_IR2.to_excel(writer, sheet_name='IR<=2 & BMI>=30',index=False)
    BMI30HOMA_IR2_dups.to_excel(writer, sheet_name='IR<=2 & BMI>=30_dups',index=False)
    HOMA_IR3FBG100.to_excel(writer, sheet_name='IR>=3 & FBG<100',index=False)
    HOMA_IR3FBG100_dups.to_excel(writer, sheet_name='IR>=3 & FBG<100_dups',index=False)
    HOMA_IR2FBG100.to_excel(writer, sheet_name='IR<=2 & FBG>=100',index=False)
    HOMA_IR2FBG100_dups.to_excel(writer, sheet_name='IR<=2 & FBG>=100_dups',index=False)
    METS_IDF_IR3.to_excel(writer, sheet_name='MS=0 & IR>=3',index=False)
    METS_IDF_IR3_dups.to_excel(writer, sheet_name='MS=0 & IR>=3_dups',index=False)
    METS_IDF_IR2.to_excel(writer, sheet_name='MS=1 & IR<2',index=False)
    METS_IDF_IR2_dups.to_excel(writer, sheet_name='MS=1 & IR<2_dups',index=False)
    # Close the Pandas Excel writer and output the Excel file.
    writer.save()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
   