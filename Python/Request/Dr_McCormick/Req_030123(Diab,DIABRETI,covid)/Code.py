#######################################################################################
# Request 03/01/23
# Author-H.Soriano
# Could you work on this spreadsheet to provide the numbers.  There are three separate tabs
# Tab 1 is diabetes and seeks to know how many people have gone from a baseline of no diabetes or pre-diabetes to diabetes based on our definition. It also wants to count how many people developed pre-diabetes, and finally how many people never developed either after baseline.
# Tab 2 is about diabetic retinopathy and seeks to count how many people we have with diabetic retinopathy at baseline and how many people developed retinopathy at any time in follow-up.
# Tab 3 is about COVID. All of the baseline from the CCHC should be no COVID because we have visits prior to COVID.  We then want to know how many people were infected with SARS Cov 2 virus based on their having antibody to the Nucleocapsid protein of SARS-Cov 2 virus.  Those will be after baseline.
# We also want to know who never developed antibody to Nucelocapsid so never encountered COVID disease.
#######################################################################################

import pandas as pd
import numpy as np
import datacompy
from functools import reduce

#function creates followup dataset from CCHC dataset given specific dataset
#returns baseline and duplicates which are followup visits
def createBaseFollowupDF(df,df_main):
    
    df_baseline=df[df['study'].isin(['BASELINE','PEDIATRIC BASELINE'])]
    df_main_baseline=df_main.loc[df_main.rrid.isin(df_baseline['rrid']) ]
    df_dups=df_main_baseline.loc[df_main_baseline.duplicated(subset='rrid', keep=False)]
    df_dups1=df_dups.loc[~df_dups['study'].isin(['BASELINE','PEDIATRIC BASELINE'])]
    
    #df_followup=df_dups.groupby('rrid').filter(lambda x: any(x.study.isin(['BASELINE','PEDIATRIC BASELINE'])))
    
    return df_baseline,df_dups1

#creates followup dataset from Covid dataset given specific dataset
#returns baseline and duplicates which are followup visits
def createBaseFollowupDF_covid(df,df_main):
    
    df_baseline=df[df['visit_corrected'].isin([1])]
    df_main_baseline=df_main.loc[df_main.rrid.isin(df_baseline['rrid']) ]
    df_dups=df_main_baseline.loc[df_main_baseline.duplicated(subset='rrid', keep=False)]
    df_dups1=df_dups.loc[~df_dups['visit_corrected'].isin([1])]
    
    #df_followup=df_dups.groupby('rrid').filter(lambda x: any(x.study.isin(['BASELINE','PEDIATRIC BASELINE'])))
    
    return df_baseline,df_dups1



if __name__ == "__main__":
    #brings in CCHC sas dataset
    df_cchc=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\All_CCHC\cchc.sas7bdat',encoding='latin1')

    df_cchc.columns= df_cchc.columns.str.lower()
    df_diabetic=df_cchc[['rrid','labid','study','age_at_visit','gender','ada2010_cat']]

    #----------------------------------------Tab 1-----------------------------------------------
    #seperates dataset into diabetic, prediabetic, and normal
    df_diabetic_1=df_diabetic.loc[( df_diabetic['ada2010_cat'] == 1)]
    df_diabetic_2=df_diabetic.loc[( df_diabetic['ada2010_cat'] == 2)]
    df_diabetic_3=df_diabetic.loc[( df_diabetic['ada2010_cat'] == 3)]

    #create datasets baseline diabetic to followup non-diabetic participants at any point
    output=createBaseFollowupDF(df_diabetic_3,df_diabetic)
    df_diabetic_3_baseline=output[0]
    df_diabetic_3_followup=output[1]
    df_diabetic_followup_nondia=df_diabetic_3_followup.loc[( df_diabetic_3_followup['ada2010_cat'].isin([1,2]))]
    df_diabetic_followup_nondia=df_diabetic_followup_nondia.drop_duplicates(subset=['rrid']).reset_index(drop=True)
    
    #create datasets baseline pre-diabetic to followup diabetic participants at any point
    output=createBaseFollowupDF(df_diabetic_2,df_diabetic)
    df_diabetic_2_baseline=output[0]
    df_diabetic_2_followup=output[1]
    df_predia_followup_dia=df_diabetic_2_followup.loc[( df_diabetic_2_followup['ada2010_cat'].isin([1]))]
    df_predia_followup_dia=df_predia_followup_dia.drop_duplicates(subset=['rrid']).reset_index(drop=True)
    
    #create datasets baseline non-diabetic to followup diabetic participants at any point
    output=createBaseFollowupDF(df_diabetic_1,df_diabetic)
    df_diabetic_1_baseline=output[0]
    df_diabetic_1_followup=output[1]
    df_no_pre_or_diab_followup_dia=df_diabetic_1_followup.loc[( df_diabetic_1_followup['ada2010_cat'].isin([3]))]
    df_no_pre_or_diab_followup_dia=df_no_pre_or_diab_followup_dia.drop_duplicates(subset=['rrid']).reset_index(drop=True)
    
    #create datasets baseline non diabetic to followup pre-diabetic participants at any point
    output=createBaseFollowupDF(df_diabetic_1,df_diabetic)
    df_no_diab_to_followup_pre=output[1]
    df_no_diab_to_followup_pre=df_no_diab_to_followup_pre.loc[( df_no_diab_to_followup_pre['ada2010_cat'].isin([2]))]
    df_no_diab_to_followup_pre=df_no_diab_to_followup_pre.drop_duplicates(subset=['rrid']).reset_index(drop=True)
    
    
    #creates dataset to display diabetic numbers on excel sheet
    df_diabetes_sheet = pd.DataFrame(columns=['baseline characteristic','number of people at baseline','follow up at any point','Number of people'])
    df_diabetes_sheet.loc[len(df_diabetes_sheet.index)] = ['diabetes',df_diabetic_3_baseline.shape[0] ,'no diabetes at any point',df_diabetic_followup_nondia.shape[0]] 
    df_diabetes_sheet.loc[len(df_diabetes_sheet.index)] = ['pre-diabetes',df_diabetic_2_baseline.shape[0] ,'diabetes at any point',df_predia_followup_dia.shape[0]] 
    df_diabetes_sheet.loc[len(df_diabetes_sheet.index)] = ['no diabetes  no pre-diabetes',df_diabetic_1_baseline.shape[0] ,'diabetes at any point',df_no_pre_or_diab_followup_dia.shape[0]] 
    df_diabetes_sheet.loc[len(df_diabetes_sheet.index)] = ['no diabetes  no pre-diabetes',df_diabetic_1_baseline.shape[0] ,'prediabetes at any point',df_no_diab_to_followup_pre.shape[0]] 


    #----------------------------------------Tab 2-----------------------------------------------

    #creates covid dataset
    df_covid=df_cchc[['rrid','labid','visit','interview_date','study','cov_ncp_result','has_covid_study']]

    df_covid=df_covid.loc[(df_covid['has_covid_study'] == 1)]

    #correcting visit number based on visit date
    df_covid['interview_date'] = pd.to_datetime(df_covid['interview_date'])
    df_covid1 = df_covid.sort_values(['rrid','interview_date'], ignore_index=True)
    df_covid1['visit_corrected'] = np.where(df_covid1['interview_date'].notna(), df_covid1.groupby('rrid').cumcount().add(1), 1)

    #creates baseline no-covid dataset
    df_nocovid_baseline=df_covid1.loc[(df_covid1['visit_corrected']==1) & (df_covid1['cov_ncp_result'] == 0)]
    #df_dups=df_nocovid_baseline.loc[df_nocovid_baseline.duplicated(subset='rrid', keep=False)]
    df_nocovid_baseline=df_nocovid_baseline.drop_duplicates(subset=['rrid']).reset_index(drop=True)

    #creates baseline covid dataset
    df_covid_baseline=df_covid1.loc[(df_covid1['visit_corrected']==1) & (df_covid1['cov_ncp_result'] == 1)]
    df_covid_baseline=df_covid_baseline.drop_duplicates(subset=['rrid']).reset_index(drop=True)

    #creates baseline borderline covid dataset
    df_borderline_covid_baseline=df_covid1.loc[(df_covid1['visit_corrected']==1) & (df_covid1['cov_ncp_result'] == 2)]
    df_borderline_covid_baseline=df_borderline_covid_baseline.drop_duplicates(subset=['rrid']).reset_index(drop=True)

    #creates followup covid dataset
    df_covid_anypoint=df_covid1.loc[(df_covid1['cov_ncp_result'] == 1)]
    df_covid_anypoint=df_covid_anypoint.drop_duplicates(subset=['rrid']).reset_index(drop=True)

    #creates followup no-covid dataset
    df_nocovid_anypoint=df_covid1.loc[(df_covid1['cov_ncp_result'] == 0) & (~df_covid1['cov_ncp_result'].isna())]
    df_nocovid_anypoint=df_nocovid_anypoint.drop_duplicates(subset=['rrid']).reset_index(drop=True)
    
    #creates followup borderline covid dataset
    df_borderline_covid_anypoint=df_covid1.loc[(df_covid1['cov_ncp_result'] == 2) & (~df_covid1['cov_ncp_result'].isna())]
    df_borderline_covid_anypoint=df_borderline_covid_anypoint.drop_duplicates(subset=['rrid']).reset_index(drop=True)
    
    #1. creates followup dataset given no-covid on baseline dataset
    output=createBaseFollowupDF_covid(df_nocovid_baseline,df_covid1)
    
    #creates dataset no-covid given the followup dataset from step 1
    df_nocovid_followup=output[1].loc[(output[1]['cov_ncp_result']==0)]
    df_nocovid_followup1=df_nocovid_followup.drop_duplicates(subset=['rrid']).reset_index(drop=True)

    #creates dataset covid given the followup dataset from step 1
    df_covid_followup=output[1].loc[(output[1]['cov_ncp_result']==1)]
    df_covid_followup1=df_covid_followup.drop_duplicates(subset=['rrid']).reset_index(drop=True)
    
    #creates dataset borderline covid given the followup dataset from step 1
    df_borderline_covid_followup=output[1].loc[(output[1]['cov_ncp_result']==2)]
    df_borderline_covid_followup1=df_borderline_covid_followup.drop_duplicates(subset=['rrid']).reset_index(drop=True)
    
    df_covid_noresults=df_covid1.loc[df_covid1['cov_ncp_result'].isna()].reset_index(drop=True)

    #creates dataset to display covid numbers on excel sheet
    df_covid_sheet = pd.DataFrame(columns=['baseline','number of people','COVID infection(antibody to N protein) at any point','Number of people','no COVID infection (Neg antibody to N protein) after all follow-up','Number of people'])
    df_covid_sheet.loc[len(df_covid_sheet.index)] = ['COVID',df_covid_baseline.shape[0],'COVID',df_covid_anypoint.shape[0],'COVID',df_covid_followup1.shape[0]] 
    df_covid_sheet.loc[len(df_covid_sheet.index)] = ['no COVID',df_nocovid_baseline.shape[0],'no COVID',df_nocovid_anypoint.shape[0],'no Covid',df_nocovid_followup1.shape[0]] 
    df_covid_sheet.loc[len(df_covid_sheet.index)] = ['Borderline',df_borderline_covid_baseline.shape[0],'Borderline',df_borderline_covid_anypoint.shape[0],'Borderline',df_borderline_covid_followup1.shape[0]] 


    #----------------------------------------Tab 3-----------------------------------------------
    #work in progress
    
    df_retina=df_cchc[['rrid','labid','visit','interview_date','study','retina_results','has_retina_scan']]
    
    #adding additional values
    df_retina.loc[(df_retina['has_retina_scan'] == 1) & (df_retina['rrid'] == "BD0525") & (df_retina['labid'] == "15Y0165"),'diabetic_retinopathy']=1
    df_retina.loc[(df_retina['has_retina_scan'] == 1) & (df_retina['rrid'] == "BD1263") & (df_retina['labid'] == "10Y0284"),'diabetic_retinopathy']=1
    df_retina.loc[(df_retina['has_retina_scan'] == 1) & (df_retina['rrid'] == "BD1724") & (df_retina['labid'] == "5Y0211"),'diabetic_retinopathy']=1
    df_retina.loc[(df_retina['has_retina_scan'] == 1) & (df_retina['rrid'] == "BD1848") & (df_retina['labid'] == "15Y0219"),'diabetic_retinopathy']=1
    df_retina.loc[(df_retina['has_retina_scan'] == 1) & (df_retina['rrid'] == "BD1877") & (df_retina['labid'] == "15Y0225"),'diabetic_retinopathy']=1
    df_retina.loc[(df_retina['has_retina_scan'] == 1) & (df_retina['rrid'] == "BD3454") & (df_retina['labid'] == "BA0443"),'diabetic_retinopathy']=1


    df_retina_results=df_retina.loc[~df_retina['retina_results'].isna()].reset_index(drop=True)
    df_retina_results.loc[(df_retina_results['retina_results'].str.contains("retinopathy", case=False)),"diabetic_retinopathy"] = 1
    df_retina_results.loc[(df_retina_results['retina_results'].str.contains("NPDR", case=False)),"diabetic_retinopathy"] = 1

    df_retinopathy=df_retina_results.loc[(df_retina_results['diabetic_retinopathy'] == 1)].reset_index(drop=True)
    
    df_retinopathy1=df_retina.loc[(df_retina['diabetic_retinopathy'] == 1)].reset_index(drop=True)
    
    df_retinopathy2=df_retina.loc[(df_retina['has_retina_scan'] == 1) & ( df_retina['rrid'].isin(['BD0063',
    'BD0331',
    #'BD0525',
    'BD1134',
    'BD1161',
    'BD1303',
    'BD1521',
    'BD1546',
    'BD1586',
   # 'BD1877',
    'BD1902',
    'BD1964',
    'BD2000',
    'BD2259',
    'BD2276',
    'BD2282',
    'BD2369',
    'BD2430',
    'BD2724',
    'BD2815',
    'BD2873',
    'BD2874',
    'BD2912',
    'BD2980',
    'BD3062',
    'BD3441',
    'BD3513',
    'BD3590',
    'BD3707',
    'BD3738',
    'BD3759',
    'BD0760',
    'BD0761',
    'BD1112',
   # 'BD1263',
    'BD1327',
    'BD1345',
    'BD1628',
  #  'BD1724',
  #  'BD1848',
  #  'BD3454',
    'BD0669',
    'BD0202'
    ]))].reset_index(drop=True)
    
    df_retinopathy2.loc[(df_retinopathy2['has_retina_scan'] == 1),'diabetic_retinopathy']=1
    

    df_retinopathy3=pd.concat([df_retinopathy,df_retinopathy1,df_retinopathy2], ignore_index=True)
    df_retinopathy3=df_retinopathy3.drop_duplicates(subset=['labid']).reset_index(drop=True)
    
    df_retinopathy_final=df_retinopathy3[['rrid','labid','diabetic_retinopathy']]
    
    df_diab_retinopathy=pd.merge(df_diabetic_3, df_retinopathy_final, how="left", on=["rrid","labid"])
    
    df_diab_no_retinopathy=df_diab_retinopathy.loc[df_diab_retinopathy.diabetic_retinopathy.isna()]

    #diabetic and no retinopathy at any point
    output=createBaseFollowupDF(df_diab_no_retinopathy,df_diab_retinopathy)
    df_diabetic_no_retinopathy_baseline=output[0]
    df_diabetic_no_retinopathy_followup=output[1]
    df_diabetic_followup_retinopathy=df_diabetic_no_retinopathy_followup.loc[( df_diabetic_no_retinopathy_followup['diabetic_retinopathy'] == 1 )]
    df_diabetic_followup_retinopathy=df_diabetic_followup_retinopathy.drop_duplicates(subset=['rrid']).reset_index(drop=True)
    

    df_diab_retinopathy_sub1=df_diab_retinopathy.loc[df_diab_retinopathy['diabetic_retinopathy'] == 1]

    #diabetic and retinopathy at any point
    output=createBaseFollowupDF(df_diab_retinopathy_sub1,df_diab_retinopathy)
    df_diabetic_retinopathy_baseline=output[0]
    df_diabetic_retinopathy_followup=output[1]
    

    df_prediab_retinopathy=pd.merge(df_diabetic_2, df_retinopathy_final, how="left", on=["rrid","labid"])

    df_prediab_no_retinopathy=df_prediab_retinopathy.loc[df_prediab_retinopathy.diabetic_retinopathy.isna()]

    #prediabetic and no retinopathy at any point
    output=createBaseFollowupDF(df_prediab_no_retinopathy,df_prediab_retinopathy)
    df_prediabetic_no_retinopathy_baseline=output[0]
    df_prediabetic_no_retinopathy_followup=output[1]
    df_prediabetic_no_retinopathy_followup=df_prediabetic_no_retinopathy_followup.loc[( df_prediabetic_no_retinopathy_followup['diabetic_retinopathy'] == 1 )]
    df_prediabetic_no_retinopathy_followup=df_prediabetic_no_retinopathy_followup.drop_duplicates(subset=['rrid']).reset_index(drop=True)
    
    
    df_nobiab=pd.concat([df_diabetic_2,df_diabetic_1], ignore_index=True)
    df_nobiab=df_nobiab.drop_duplicates(subset=['labid']).reset_index(drop=True)
    
    df_nodiab_retinopathy=pd.merge(df_nobiab, df_retinopathy_final, how="left", on=["rrid","labid"])

    df_nodiab_noretinopathy=df_nodiab_retinopathy.loc[df_nodiab_retinopathy.diabetic_retinopathy.isna()]

    #no diabetic and no retinopathy at any point
    output=createBaseFollowupDF(df_nodiab_noretinopathy,df_nodiab_retinopathy)
    df_nodiabetic_noretinopathy_baseline=output[0]
    df_nodiabetic_noretinopathy_followup=output[1]    
    df_nodiabetic_noretinopathy_followup=df_nodiabetic_noretinopathy_followup.loc[( df_nodiabetic_noretinopathy_followup['diabetic_retinopathy'] == 1 )]
    df_nodiabetic_noretinopathy_followup=df_nodiabetic_noretinopathy_followup.drop_duplicates(subset=['rrid']).reset_index(drop=True)
    
    
    
    df_retinopathy_sheet = pd.DataFrame(columns=['baseline','number of people','followup at any point','Number of people'])
    df_retinopathy_sheet.loc[len(df_retinopathy_sheet.index)] = ['diabetes no retinopath',df_diabetic_no_retinopathy_baseline.shape[0],'retinopathy',df_diabetic_followup_retinopathy.shape[0]] 
    df_retinopathy_sheet.loc[len(df_retinopathy_sheet.index)] = ['diabetes retinopath',df_diabetic_retinopathy_baseline.shape[0],'retinopathy',''] 
    df_retinopathy_sheet.loc[len(df_retinopathy_sheet.index)] = ['pre-diabetes and no retinopathy',df_prediabetic_no_retinopathy_baseline.shape[0],'retinopathy',df_prediabetic_no_retinopathy_followup.shape[0]] 

    
    #output to dataset sheets to excel file
    writer = pd.ExcelWriter(r'U:\Users\hsoriano\Data Request\DR. McC\Req_030123(Diab,DIABRETI,covid)\Req_031123(Ddiabetes retinopathy and COVID infection).xlsx', engine='xlsxwriter')


    df_diabetes_sheet.to_excel(writer, sheet_name='diabetes',index=False)
    
    df_covid_sheet.to_excel(writer, sheet_name='covid',index=False)
    #df_retinopathy_sheet.to_excel(writer, sheet_name='retinopathy',index=False)
   
    # Close the Pandas Excel writer and output the Excel file.
    writer.save()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
   