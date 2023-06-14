import pandas as pd
import datacompy
#import pyodbc
import config as cfg
import sqlalchemy
from sqlalchemy import text
import logging
import numpy as np


# Logger initialization
logging.basicConfig(format='[%(levelname)s]: %(message)s', level=logging.DEBUG)

#gets sql table and returns it as a dataframe
def extract_table(table):
    """
    Function to extract table from sql database
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
    #Bring in the lab information
    query = 'SELECT * from [dbo].[' + table + ']'
    df = pd.read_sql_query(sql=text(query), con=engine.connect())
    
    df.columns= df.columns.str.lower()
    columnames=list(df)

    return df,columnames


def load_data(df,table):
    """
    Function that allows loading data into sql database
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
        logging.info('Something went wrong')
    except:
        logging.info('Data already exists in the database')

    logging.info('Close database successfully')
    return

if __name__ == '__main__':
    
    df_drs=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\FINAL DATA\DRS\patient_alldrs_iz.sas7bdat',encoding='latin1')

    output=extract_table('ImpactContactScheduling_A')
    
    df_sql_CS=output[0]
    column_list=output[1]
    
    df_drs.columns= df_drs.columns.str.lower()
    
    #lets restructure bdvisit and visit for all drs dataset
    df_sql_CS1=df_sql_CS.loc[(df_sql_CS['rrid'].isin(df_drs['rrid'])) & (~df_sql_CS['study'].isin(['RISK']))]
    df_sql_CS1=df_sql_CS1.sort_values(['bdvisit'])
    
    df_sql_CS1=df_sql_CS1.groupby("rrid").last()
    df_sql_CS1=df_sql_CS1.reset_index()
    
    df_drs=df_drs.sort_values(['bdvisit'])
    
    df_drs['visit_count'] = df_drs.assign().groupby('rrid').cumcount() + 1
    df_drs['old_visit'] = df_drs['rrid'].map(df_sql_CS1.set_index('rrid')['visit'])
    df_drs['new_visit'] =  df_drs['old_visit'] + df_drs['visit_count']
    df_drs['new_bdvisit'] = df_drs['rrid'] + df_drs['new_visit'].astype(str).str.zfill(2)
    
    df_drs.drop(columns =['old_visit','visit_count','visit','bdvisit','key7'], inplace=True)
    df_drs.rename(columns ={'new_visit':'visit','new_bdvisit':'bdvisit'}, inplace=True)

    df_keys=df_sql_CS1[['rrid','key7','key8']]
    
    df_drs=df_drs.merge(df_keys,how="left",on=['rrid'])
    
    #grab only the columns that belong to ImpactCS_A table
    df_drs_CS=df_drs[df_drs.columns.intersection(column_list)]
    
    column_list_sas=list(df_drs_CS)
    not_list=list(set(column_list) - set(column_list_sas))
    
    #loads data into the sql database
    load_data(df_drs_CS,'ImpactContactScheduling_A')
    
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
    