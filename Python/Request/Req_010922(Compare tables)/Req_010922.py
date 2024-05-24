# -*- coding: utf-8 -*-
"""
Created on Tue Jan 09 09:01:14 2023

@author: hsoriano
"""

import pandas as pd
import datacompy
import pyodbc as pyodbc
from functools import reduce

def extract_access(database,table):
    """
    Createds a dataframe from access table.

    Parameters
    ----------
    database : Dataframe
        Access dataframe.

    Returns
    -------
    df : Dataframe
        Returns extracted dataframe from table IMPACTLAB_A with rrid and labid.

    """
    query = "SELECT * FROM " + table
    cnxn = pyodbc.connect(database)
    df = pd.read_sql(query, cnxn)
    cnxn.close()
    return df


def extract_azure(conn,table):
    """
    Createds a dataframe from azure table.

    Parameters
    ----------
    database : Dataframe
        Access dataframe.

    Returns
    -------
    df : Dataframe
        Returns extracted dataframe from table IMPACTLAB_A with rrid and labid.

    """
    query = 'SELECT * from [CCHC_Test].[dbo].' + table
    df = pd.read_sql(query, conn)
    return df

def compare_tables(table,column,column2=None):
    
    df_azure_cchc = extract_azure(conn,table)
    
    #extract only RRID and LABID from these datasets and combine
    df_access_cchc=pd.concat([extract_access(constr_bro,table),
                       extract_access(constr_har,table),
                       extract_access(constr_lar,table)],sort=True,ignore_index=True)
    if column2 is None:
        compare = datacompy.Compare(df_access_cchc,df_azure_cchc,join_columns=[column], 
    
              
            # Optional, defaults to 'df1'
            df1_name = 'Original',
              
            # Optional, defaults to 'df2'
            df2_name = 'New' 
            )
    else:
        compare = datacompy.Compare(df_access_cchc,df_azure_cchc,join_columns=[column,column2], 
    
              
            # Optional, defaults to 'df1'
            df1_name = 'Original',
              
            # Optional, defaults to 'df2'
            df2_name = 'New' 
            )
        
    
    f = open(r'H:\Python\Req_010922(Compare tables)\compare_' + table + '.txt',"w+")
    f.write(compare.report())
    f.close()

if __name__ == "__main__":
    
    
    server = 'spcrudpwm001.database.windows.net'
    database = 'CCHC_Test'
    username ='hugo.soriano@uth.tmc.edu'
    Authentication='ActiveDirectoryInteractive'
    driver= '{ODBC Driver 17 for SQL Server}'
    conn = pyodbc.connect('DRIVER='+driver+
                          ';SERVER='+server+
                          ';PORT=1433;DATABASE='+database+
                          ';UID='+username+
                          ';AUTHENTICATION='+Authentication
                          )
    
    
    #bring in all of the datasets into python
    constr_bro = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\Uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\New_CCHC\Server\BD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    constr_har = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\Uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\New_CCHC\Server\HD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    constr_lar = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\Uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\New_CCHC\Server\LD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    
    compare_tables("A_MAJDEPEP_A","BDVISIT")
    compare_tables("Addressbook_A","RRID")
    compare_tables("Addresshist_A","ADDHISTID")
    compare_tables("B_DYSTHYMIA_A","BDVISIT")
    compare_tables("C_SUICIDALITY_A","BDVISIT")
    compare_tables("D_HYPOMANICEP_A","BDVISIT")
    compare_tables("E_PANICDISORDER_A","BDVISIT")
    compare_tables("F_AGORAPHOBIA_A","BDVISIT")
    compare_tables("G_SEPANXDIS_A","BDVISIT")
    compare_tables("H_SOCIALPHOBIA_A","BDVISIT")
    compare_tables("I_SPECIFICPHOBIA_A","BDVISIT")
    compare_tables("J_OCD_A","BDVISIT")
    compare_tables("K_PTSD_A","BDVISIT")
    compare_tables("L_ALCOHOLABUSE_A","BDVISIT")
    compare_tables("M_NAPASUD_A","BDVISIT")
    compare_tables("N_TICDISORDERS_A","BDVISIT")
    compare_tables("O_ADHD_A","BDVISIT")
    compare_tables("P_CONDUCTDIS_A","BDVISIT")
    compare_tables("Q_OPPOSITIONAL_A","BDVISIT")
    compare_tables("R_PSYCHOTICMOOD_A","BDVISIT")
    compare_tables("S_ANOREXIA_A","BDVISIT")
    compare_tables("T_BULIMIA_A","BDVISIT")
    compare_tables("U_GENANX_A","BDVISIT")
    compare_tables("V_ADJUSTMENT_A","BDVISIT")
    compare_tables("W_RULEOUT_A","BDVISIT")
    compare_tables("X_PERVASIVE_A","BDVISIT")
    
    compare_tables("Cluster_A","KEY6")
    compare_tables("Hide_A","KEY7")
    compare_tables("Household_A","KEY7")
    compare_tables("HouseholdIncome_A","KEY7")
    compare_tables("ImpactAnth_A","BDVISIT")
    compare_tables("ImpactCABStudies_A","BDVISIT")
    compare_tables("ImpactCESD_A","BDVISIT")
    compare_tables("ImpactContactScheduling_A","BDVISIT")
    compare_tables("ImpactCover_A","BDVISIT")
    compare_tables("ImpactDmhxmed_A","BDVISIT")
    compare_tables("ImpactEAN_A","BDVISIT")
    compare_tables("ImpactEkg_A","BDVISIT")
    compare_tables("ImpactFamilyhx_A","BDVISIT")
    compare_tables("ImpactIdd_A","BDVISIT")
    compare_tables("ImpactLab_A","BDVISIT")
    compare_tables("ImpactMhxmed_A","BDVISIT")
    compare_tables("ImpactSAS_A","BDVISIT")
    compare_tables("ImpactSmokedrink_A","BDVISIT")
    compare_tables("ImpactSP_A","BDVISIT")
    compare_tables("ImpactWelch_A","BDVISIT")
    compare_tables("Member_A","KEY8")
    
    compare_tables("PEDIATRIC_A","BDVISIT")
    compare_tables("PEDICTQ_A","BDVISIT")
    compare_tables("PEDISLESC_A","BDVISIT")
    compare_tables("PEDISLESP_A","BDVISIT")
    compare_tables("PEDITANRF_A","BDVISIT")
    compare_tables("PEDITANRM_A","BDVISIT")
    
    compare_tables("PHONEBOOK_A","PHONEID")
    compare_tables("PHONEHIST_A","PHONEID")
    compare_tables("CRLDATA_A","BDVISIT")
    compare_tables("ContactSummary_A","KEY8","VISIT")
    compare_tables("ImpactBP_A","BDVISIT")
    
    
    
    
    
    
    