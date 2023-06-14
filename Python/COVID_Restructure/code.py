import pandas as pd
import datacompy
import config as cfg
import sqlalchemy
from sqlalchemy import text
import logging
import numpy as np


# Logger initialization
logging.basicConfig(format='[%(levelname)s]: %(message)s', level=logging.DEBUG)

def extract_table(table):
    """
    Function to extract table from sql database
    Given table name
    """
    connexion_str = sqlalchemy.engine.URL.create(
        "mssql+pyodbc",
        username=cfg.sql['user'],
        host=cfg.sql['server'],
        database=cfg.sql['db'],
        query={
            "driver": cfg.sql['driver'],
            "authentication": cfg.sql['authentication'],
        },
        )
        

    engine = sqlalchemy.create_engine(connexion_str)

    query = 'SELECT * from [dbo].[' + table + ']'
    df = pd.read_sql_query(sql=text(query), con=engine.connect())
    
    df.columns= df.columns.str.lower()


    return df

def extract_columns(table):
    """
    Function to extract table from sql database
    Given table name
    """
    connexion_str = sqlalchemy.engine.URL.create(
        "mssql+pyodbc",
        username=cfg.sql['user'],
        host=cfg.sql['server'],
        database=cfg.sql['db'],
        query={
            "driver": cfg.sql['driver'],
            "authentication": cfg.sql['authentication'],
        },
        )
        

    engine = sqlalchemy.create_engine(connexion_str)

    query = 'SELECT column_name,ordinal_position FROM INFORMATION_SCHEMA.COLUMNS where table_name=\'' + table + '\' order by 2'
    df = pd.read_sql_query(sql=text(query), con=engine.connect())
    
    df.columns= df.columns.str.lower()
    col_list = df.column_name.values.tolist()
    col_list = [x.lower() for x in col_list ]

    return col_list

def load_data(df,table):
    """
    Function that allows loading data into sql database
    Given dataframe and table name
    """
    connexion_str = sqlalchemy.engine.URL.create(
        "mssql+pyodbc",
        username=cfg.sql['user'],
        host=cfg.sql['server'],
        database=cfg.sql['db'],
        query={
            "driver": cfg.sql['driver'],
            "authentication": cfg.sql['authentication'],
        },
        )
        

    engine = sqlalchemy.create_engine(connexion_str)
    engine.connect()

    insp = sqlalchemy.inspect(engine)
    if not insp.has_table(table):
        logging.info('Please create table')
        return
    
    try:
        df.to_sql(table, engine, index=False, if_exists='append')
    except Exception as e:
        logging.info('Something went wrong' + e)
    except:
        logging.info('Data already exists in the database')

    logging.info('Close database successfully')
    return


def getFormSection(Start,End,df_covid):
    """
    Function that returns sections
    Given start column, end column, and dataframe
    """
    A_index = column_list.index(Start)
    B_index = column_list.index(End)
    df_covid_Sec = df_covid.loc[:, list(df_covid.columns[0:5]) + list(df_covid.columns[A_index:B_index+1])]
    
    return df_covid_Sec

if __name__ == '__main__':
    
    df_covid=pd.read_csv(r'C:\Users\hsoriano\Documents\Python\Data Analyst Portfolio\Python\COVID_Restructure\COHORTCOVID19_DATA_2023-04-21_0933.csv')

    df_covid=df_covid.sort_values(by=['study_id', 'cov_rrid'])
    df_covid['cov_rrid'] = df_covid.groupby('study_id')['cov_rrid'].transform('first')
    
    #extracting individual tables from REDcap output based on columns
    column_list = list(df_covid.columns)
    df_covid_CS = getFormSection('cov_com','contact_information_complete',df_covid)
    df_covid_CS1 = df_covid_CS.loc[(df_covid_CS['redcap_event_name'] =='enrollment_arm_1') & (df_covid_CS['redcap_repeat_instrument'] =='contact_information') ]

    df_covid_DM = getFormSection('cov_verbal_consent','demographic_and_medical_history_complete',df_covid)
    df_covid_DM1 = df_covid_DM.loc[(df_covid_DM['redcap_event_name'] =='enrollment_arm_1') & (df_covid_DM['redcap_repeat_instrument'].isna()) ]

    df_covid_CON = getFormSection('cov_verbal_consent_v2_v2','phase_2_consent_covid19_complete',df_covid)
    df_covid_CON1 = df_covid_CON.loc[(df_covid_DM['redcap_event_name'] =='enrollment_arm_1') & (df_covid_DM['redcap_repeat_instrument'] == 'phase_2_consent_covid19') ]

    df_covid_CPS = getFormSection('last_result','current_participation_status_complete',df_covid)
    df_covid_CPS1 = df_covid_CPS.loc[(df_covid_CPS['redcap_event_name'] =='enrollment_arm_1') & (df_covid_CPS['redcap_repeat_instrument'].isna()) ]

    df_covid_SSC = getFormSection('cov_basedate','medical_history_7017_complete',df_covid)
    df_covid_SSC1 = df_covid_SSC.loc[(df_covid_SSC['redcap_event_name'] =='baseline_arm_1') & (df_covid_SSC['redcap_repeat_instrument'].isna()) ]

    df_covid_FUSSC = getFormSection('cov_rrid_fup','followup_screening_and_symptoms_covid19_complete',df_covid)
    df_covid_FUSSC1 = df_covid_FUSSC.loc[(df_covid_FUSSC['redcap_event_name'] =='followups_arm_1') & (df_covid_FUSSC['redcap_repeat_instrument'] == 'followup_screening_and_symptoms_covid19') ]

    df_covid_WFUSSC = getFormSection('cov_labid_wk','weekly_followup_screening_and_symptoms_covid19_complete',df_covid)
    df_covid_WFUSSC1 = df_covid_WFUSSC.loc[(df_covid_WFUSSC['redcap_event_name'] =='followups_arm_1') & (df_covid_WFUSSC['redcap_repeat_instrument'] == 'weekly_followup_screening_and_symptoms_covid19') ]

    df_covid_QFUSSC = getFormSection('cov_fuvisit_mth3','weekly_followup_screening_and_symptoms_covid19_312b44_complete',df_covid)
    df_covid_QFUSSC1 = df_covid_QFUSSC.loc[(df_covid_QFUSSC['redcap_event_name'] =='followups_arm_1') & (df_covid_QFUSSC['redcap_repeat_instrument'] == 'weekly_followup_screening_and_symptoms_covid19_312b44') ]

    df_covid_ANTH = getFormSection('cov_visit_anth','anthropometrics_complete',df_covid)
    df_covid_ANTH1 = df_covid_ANTH.loc[(df_covid_ANTH['redcap_event_name'].str.contains(r'^visit[0-9]_arm_[0-9]')) ]

    #------------------------------Transforms CS Tables and loads it into SQL-----------------------------------
    df_covid_CS1.loc[df_covid_CS1['cov_rrid']=='BDO800','cov_rrid']='BD0800'
    df_covid_CS1.loc[df_covid_CS1['cov_rrid']=='BDO845','cov_rrid']='BD0845'
    df_covid_CS1.loc[df_covid_CS1['cov_rrid']=='HD00182','cov_rrid']='HD0182'
    
    df_covid_CS1.loc[df_covid_CS1['cov_result_code'].isin([8,9]),'cov_result_code']=1
    df_covid_CS1.loc[df_covid_CS1['cov_result_code'].isin([10]),'cov_result_code']=11
    df_covid_CS1.loc[df_covid_CS1['cov_result_code'].isin([6]),'cov_result_code']=8
    df_covid_CS1.loc[df_covid_CS1['cov_result_code'].isin([7]),'cov_result_code']=9
    
    df_covid_CS1.loc[df_covid_CS1['cov_type_contact'].isin([1]),'cov_type_contact']="FACE"
    df_covid_CS1.loc[df_covid_CS1['cov_type_contact'].isin([2]),'cov_type_contact']="PHONE"
    df_covid_CS1.loc[df_covid_CS1['cov_type_contact'].isin([3]),'cov_type_contact']="OTHER"
    
    df_covid_CS1['cov_interv_id'] = pd.to_numeric(df_covid_CS1['cov_interv_id'], errors='coerce')

    
    df_covid_CS2=df_covid_CS1.loc[~df_covid_CS1['cov_rrid'].isna()]
    df_covid_CS3=df_covid_CS2.sort_values(by=['cov_rrid','redcap_repeat_instance'])

    df_ref=df_covid_CS3.loc[~df_covid_CS3['cov_reason'].isna()]
    df_dups= df_ref[df_ref.duplicated(['cov_rrid'], keep=False)]
    
    df_examdate=df_covid_CS3.loc[~df_covid_CS3['cov_dt_examdate'].isna()]
    
    df_covid_CS3.rename(columns={"cov_rrid": "rrid", "cov_interv_id": "id",
                                 "cov_type_contact":"face","cov_tc_other":"other",
                                 "cov_result_code":"res","cov_rc_other2":"comm"},
                        inplace=True)
    
    df_covid_CS3['date'] = pd.to_datetime(df_covid_CS3['cov_c_date']).dt.date
    df_covid_CS3['time'] = pd.to_datetime(df_covid_CS3['cov_c_date']).dt.time
    
    df_covid_CS3.drop(columns=['study_id', 'redcap_event_name',
                               'redcap_repeat_instrument','contact_information_complete',
                               'cov_com','cov_reason','cov_dt_examdate',
                               'cov_c_date'],inplace=True)
    

    df_covid_CS4 = df_covid_CS3.pivot(index='rrid', 
                                      columns='redcap_repeat_instance', 
                                      values=['id', 'face','other','res','comm','date','time'])
    
    df_covid_CS4.columns =["co" + str(int(c2)) + c1  for (c1,c2) in df_covid_CS4.columns.tolist()]
    
    df_ref1=df_ref[['cov_rrid','cov_reason']]
    
    df_ref1.rename(columns={"cov_rrid": "rrid","cov_reason":"rsnref"},
                        inplace=True)
    
    df_covid_CS5=df_covid_CS4.merge(df_ref1,how="left",on=['rrid'])

    df_sql_CS=extract_table('ImpactContactScheduling_A')
    column_list=list(df_sql_CS)
    
    
    #lets restructure bdvisit and visit for all drs dataset
    df_sql_CS1=df_sql_CS.loc[(df_sql_CS['rrid'].isin(df_covid_CS5['rrid']))]
    df_sql_CS1=df_sql_CS1.sort_values(['bdvisit'])
    
    df_sql_CS1=df_sql_CS1.groupby("rrid").last()
    df_sql_CS1=df_sql_CS1.reset_index()
    
    df_covid_CS5=df_covid_CS5.sort_values(['rrid'])
    
    df_covid_CS5['visit_count'] = df_covid_CS5.assign().groupby('rrid').cumcount() + 1
    df_covid_CS5['old_visit'] = df_covid_CS5['rrid'].map(df_sql_CS1.set_index('rrid')['visit'])
    df_covid_CS5.loc[df_covid_CS5['old_visit'].isna(),'old_visit']=0
    df_covid_CS5['old_visit'] = df_covid_CS5['old_visit'].astype(int)
    df_covid_CS5['new_visit'] =  df_covid_CS5['old_visit'] + df_covid_CS5['visit_count']
    df_covid_CS5['new_bdvisit'] = df_covid_CS5['rrid'] + df_covid_CS5['new_visit'].astype(str).str.zfill(2)
    
    df_covid_CS5.drop(columns =['old_visit','visit_count'], inplace=True)
    df_covid_CS5.rename(columns ={'new_visit':'visit','new_bdvisit':'bdvisit'}, inplace=True)

    df_keys=df_sql_CS1[['rrid','key7','key8']]
    
    df_covid_CS6=df_covid_CS5.merge(df_keys,how="left",on=['rrid'])
    
    df_covid_CS7=df_covid_CS6.loc[~df_covid_CS6['key8'].isna()]
    df_covid_missing=df_covid_CS6.loc[df_covid_CS6['key8'].isna()]
    df_covid_CS7['study']="COVID"
    
    #df_covid_CS7['rsnref_sz'] = df_covid_CS7.rsnref.str.len()
    
    #loads data into the CS table
    load_data(df_covid_CS7,'ImpactContactScheduling_A')
    
    #------------------------------Adding Missing Key8s and loading into Cluster,Household,Members,Address, and CS----------------

    df_covid_DM2=df_covid_DM1.loc[~df_covid_DM1['cov_rrid'].isna()]
    df_dups= df_covid_DM2[df_covid_DM2.duplicated(['cov_rrid'], keep=False)]
    df_covid_missing_DM=df_covid_DM1.loc[df_covid_DM1['cov_rrid'].isna()]
    
    df_covid_DM3=df_covid_DM2.loc[df_covid_DM2.cov_rrid.isin(df_covid_missing.rrid)]
    
    Temp_Key6="01999.999999ABC1A"
    data = {
      "key6":[Temp_Key6],
      "town": [1],
      "tract": [999.99],
      "block":[9999],
      "subdiv":["ABC"],
      "inout":[1],
      "section":['A']
    }
    
    #load data into a DataFrame object:
    df_cluster = pd.DataFrame(data)
    
    load_data(df_cluster,'Cluster_A')
    
    df_household=df_covid_DM3[['cov_rrid','cov_str_add','cov_city','cov_zip_code']].reset_index(drop=True)
    df_household['ID'] = df_household.index + 1
    df_household['key7'] = Temp_Key6 + df_household['ID'].astype(str).str.zfill(3)
    df_household['key6'] = Temp_Key6
    
    df_household.rename(columns={"cov_rrid": "rrid", "cov_str_add": "address",
                                 "cov_city":"city","cov_zip_code":"zip"},
                        inplace=True)
    
    df_household1=df_household.drop(columns=['ID','rrid'])
    df_household1['address'] = df_household1['address'].str.upper()
    df_household1['city'] = df_household1['city'].str.upper()
    df_household1['zip'] = pd.to_numeric(df_household1['zip'], errors='coerce')
    
    load_data(df_household1,'Household_A')
    
    df_member=df_covid_DM3[['cov_rrid','cov_first_name','cov_mid_name','cov_last_name'
                               ,'cov_gender','cov_age']].reset_index(drop=True)

    df_member['cov_first_name'] = df_member['cov_first_name'].str.upper()
    df_member['cov_mid_name'] = df_member['cov_mid_name'].str.upper()
    df_member['cov_last_name'] = df_member['cov_last_name'].str.upper()
    
    df_member['fgname']= df_member['cov_first_name'].fillna('') + " "  + df_member['cov_mid_name'].fillna('')
    df_member['sgname']= df_member['cov_last_name'].fillna('')
    
    df_member.rename(columns={"cov_rrid": "complete", "cov_gender": "gender",
                                 "cov_age":"age",},
                        inplace=True)
    
    df_member.drop(columns=['cov_first_name', 'cov_last_name',
                               'cov_mid_name'],inplace=True)
    
    df_member2=df_member.merge(df_household[['rrid','key7']],how="left",left_on='complete', right_on='rrid')

    df_member2.drop(columns=['rrid'],inplace=True)
    df_member2['key8'] = df_member2['key7'] + '01'

    load_data(df_member2,'Member_A')
    
    df_missing_add=df_member2[['complete','key8','key7','fgname','sgname']]
    
    df_temp=df_household1.merge(df_cluster,how="left",on='key6')
    df_missing_add2=df_missing_add.merge(df_temp,how="left",on='key7')
    
    df_missing_add2.rename(columns={"fgname": "given", "sgname": "patsur","complete":"rrid"},
                        inplace=True)
    
    df_missing_add2.drop(columns=['key6'],inplace=True)
    
    load_data(df_missing_add2,'Addressbook_A')

    
    df_covid_missing2=df_covid_missing.merge(df_member2[['complete','key7','key8']],how="left",left_on='rrid', right_on='complete')

    df_covid_missing2.rename(columns={"key7_y": "key7", "key8_y": "key8"
                               },
                        inplace=True)
    
    df_covid_missing2.drop(columns=['complete', 'key7_x',
                               'key8_x'],inplace=True)
    
    df_covid_missing2['study']="COVID"
    
    load_data(df_covid_missing2,'ImpactContactScheduling_A')
    
    #-----------------------------------------------Anthropometrics TABLE---------------------------
    output=extract_table('ImpactContactScheduling_A')
    
    df_drs_form=df_drs[df_drs.columns.intersection(column_list)]
    
    column_list_sas=list(df_covid_DM3)
    not_list=list(set(column_list) - set(column_list_sas))
    
    df_drs_form1 = df_drs_form.replace('  .', np.nan)
    df_drs_form2 = df_drs_form1.replace('.', np.nan)
    #loads data into the sql database
    load_data(df_drs_form2,'ImpactIdd_A')
    
    #------------------------------IDD TABLE-----------------------------------
    output=extract_table('ImpactIdd_A')
    
    df_sql=output[0]
    column_list=output[1]
    
    df_drs_form=df_drs[df_drs.columns.intersection(column_list)]
    
    column_list_sas=list(df_drs_form)
    not_list=list(set(column_list) - set(column_list_sas))
    
    df_drs_form1 = df_drs_form.replace('  .', np.nan)
    df_drs_form2 = df_drs_form1.replace('.', np.nan)
    #loads data into the sql database
    load_data(df_drs_form2,'ImpactIdd_A')
    
    #------------------------------Anthropometrics TABLE-----------------------------------
    output=extract_table('ImpactAnth_A')
    
    df_sql=output[0]
    column_list=output[1]
    
    df_drs_form=df_drs[df_drs.columns.intersection(column_list)]
    
    column_list_sas=list(df_drs_form)
    not_list=list(set(column_list) - set(column_list_sas))
    
    #loads data into the sql database
    load_data(df_drs_form,'ImpactAnth_A')
    
    #------------------------------BP TABLE-----------------------------------
    output=extract_table('ImpactBP_A')
    
    df_sql=output[0]
    column_list=output[1]
    
    df_drs_form=df_drs[df_drs.columns.intersection(column_list)]
    
    df_drs_form.drop(columns =['time'], inplace=True)
    df_drs_form['examdate'] = df_drs['bp_examdate']
    
    column_list_sas=list(df_drs_form)
    not_list=list(set(column_list) - set(column_list_sas))
    
    #loads data into the sql database
    load_data(df_drs_form,'ImpactBP_A')
    
    #------------------------------Smokedrink TABLE-----------------------------------
    output=extract_table('ImpactSmokedrink_A')
    
    df_sql=output[0]
    column_list=output[1]
    
    df_drs_form=df_drs[df_drs.columns.intersection(column_list)]
    
    df_drs_form['examdate'] = df_drs['smoke_examdate']
    
    column_list_sas=list(df_drs_form)
    not_list=list(set(column_list) - set(column_list_sas))

    #loads data into the sql database
    load_data(df_drs_form,'ImpactSmokedrink_A')
    #------------------------------Medical History TABLE-----------------------------------
    output=extract_table('ImpactMhxmed_A')
    
    df_sql=output[0]
    column_list=output[1]
    
    df_drs_form=df_drs[df_drs.columns.intersection(column_list)]
    
    df_drs_form['examdate'] = df_drs['mhx_examdate']
    
    column_list_sas=list(df_drs_form)
    not_list=list(set(column_list) - set(column_list_sas))

    #loads data into the sql database
    load_data(df_drs_form,'ImpactMhxmed_A')
    #------------------------------Diabetes History TABLE-----------------------------------
    output=extract_table('ImpactDmhxmed_A')
    
    df_sql=output[0]
    column_list=output[1]
    
    df_drs_form=df_drs[df_drs.columns.intersection(column_list)]
    
    df_drs_form['examdate'] = df_drs['dhx_examdate']
    
    column_list_sas=list(df_drs_form)
    not_list=list(set(column_list) - set(column_list_sas))
    
    #loads data into the sql database
    load_data(df_drs_form,'ImpactDmhxmed_A')
    #------------------------------EKG TABLE-----------------------------------
    output=extract_table('ImpactEkg_A')
    
    df_sql=output[0]
    column_list=output[1]
    
    df_drs_form=df_drs[df_drs.columns.intersection(column_list)]
    
    df_drs_form['timesupi'] = pd.to_datetime(df_drs_form["timesupi"], unit='s')
    df_drs_form['timebio'] = pd.to_datetime(df_drs_form["timebio"], unit='s')

    column_list_sas=list(df_drs_form)
    not_list=list(set(column_list) - set(column_list_sas))
    
    df_drs_form1 = df_drs_form.replace('.', np.nan)
    df_drs_form2 = df_drs_form1.replace('    .', np.nan)
    
    #loads data into the sql database
    load_data(df_drs_form2,'ImpactEkg_A')
    #------------------------------Family History TABLE-----------------------------------
    output=extract_table('ImpactFamilyhx_A')
    
    df_sql=output[0]
    column_list=output[1]
    
    df_drs_form=df_drs[df_drs.columns.intersection(column_list)]
    
    column_list_sas=list(df_drs_form)
    not_list=list(set(column_list) - set(column_list_sas))
    
    #loads data into the sql database
    load_data(df_drs_form,'ImpactFamilyhx_A')
    #------------------------------Laboratory TABLE-----------------------------------
    output=extract_table('ImpactLab_A')
    
    df_sql=output[0]
    column_list=output[1]
    
    df_drs_form=df_drs[df_drs.columns.intersection(column_list)]
    
    df_drs_form['todaytim'] = pd.to_datetime(df_drs_form["todaytim"], unit='s')
    df_drs_form['eat_time'] = pd.to_datetime(df_drs_form["eat_time"], unit='s')
    df_drs_form['schtime'] = pd.to_datetime(df_drs_form["schtime"], unit='s')
    
    column_list_sas=list(df_drs_form)
    not_list=list(set(column_list) - set(column_list_sas))
    
    #loads data into the sql database
    load_data(df_drs_form,'ImpactLab_A')
    #------------------------------CRL TABLE-----------------------------------
    output=extract_table('CRLDATA_A')
    
    df_sql=output[0]
    column_list=output[1]
    
    df_drs_form=df_drs[df_drs.columns.intersection(column_list)]
    
    df_drs_form['h_time'] = pd.to_datetime(df_drs_form["h_time"], unit='s')
    df_drs_form['studycrl'] = 'RISK'

    
    column_list_sas=list(df_drs_form)
    not_list=list(set(column_list) - set(column_list_sas))
    
    #loads data into the sql database
    load_data(df_drs_form,'CRLDATA_A')
    #------------------------------CRL TABLE-----------------------------------
    output=extract_table('ImpactCover_A')
    
    df_sql=output[0]
    column_list=output[1]
    
    df_drs_form=df_drs[df_drs.columns.intersection(column_list)]
    
    df_drs_form['schtime'] = pd.to_datetime(df_drs_form["schtime"], unit='s')
    df_drs_form['fasttime'] = pd.to_datetime(df_drs_form["fasttime"], unit='s')

    column_list_sas=list(df_drs_form)
    not_list=list(set(column_list) - set(column_list_sas))
    
    #loads data into the sql database
    load_data(df_drs_form,'ImpactCover_A')
    
    writer = pd.ExcelWriter(r'C:\Users\hsoriano\Documents\Python\Data Analyst Portfolio\Python\COVID_Restructure\missingKeys.xlsx', engine='xlsxwriter')

    df_missing.to_excel(writer, sheet_name='Sheet1')


    writer.save()
    writer.close()