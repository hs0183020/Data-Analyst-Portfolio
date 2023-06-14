# -*- coding: utf-8 -*-
"""
Created on Fri Apr 14 19:08:45 2023

@author: hsoriano
"""
import config as cfg
import sqlalchemy
import pandas as pd
import logging

# Logger initialization
logging.basicConfig(format='[%(levelname)s]: %(message)s', level=logging.DEBUG)


def load_data(df: pd.DataFrame):
    """
    Function that allows loading data into database
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
    # Create the database
    #engine = sqlalchemy.create_engine(cfg.sql["server"])
    # Create the connection
    #conn = sqlite3.connect('myplayedtracks.sqlite')
    # Create the pointer to direct to specific rows into the database
    #cursor = conn.cursor()
    # Metadata object that will hold the table
    #meta = MetaData(engine)
    # If the table does not exist
    insp = sqlalchemy.inspect(engine)
    if not insp.has_table('LabInfo_Bile'):
        logging.info('Please create table')
        return
    
    try:
        df.to_sql('LabInfo_Bile', engine, index=False, if_exists='append')
    except:
        logging.info('Data already exists in the database')

    #conn.close()

    logging.info('Close database successfully')
