# -*- coding: utf-8 -*-
"""
Created on Mon Apr  8 16:31:42 2024
Doing quality control from APU Tables, Proc Content and Control file. We are trying to find any anomolies between all
three files. The main file should be the Control file labeled Source_File.

The code takes in three files for each setting
-APU Tables
-Source File
-Proc Contents

@author: Hsoriano
"""

import pandas as pd
import numpy as np

def df_clean(proc_df,sf_df,apu_df):
    
    #saves first word after spliting column variable by _
    proc_df['element_number'] = proc_df['Variable'].str.split('_', n=1, expand=True)[0]
    #saves all of the words that contain a digit in the column element_number
    proc_df['element_number'] = proc_df['element_number'].astype(str).map(lambda x: x if any(c.isnumeric() for c in x) else np.nan)

    #creates a subset an removes any trailing letters from element_number column
    proc_df_1=proc_df[["Variable","Type","element_number"]].reset_index(drop=True)
    proc_df_1['Element_Number_grp'] = proc_df_1['element_number'].str.replace(r'[a-z]$', '', regex=True)
    #adds a column named in_proc
    proc_df_1['in_proc']=1

    #renaming column to variable
    sf_df.rename(columns={"Source_File_Name": "Variable"}, inplace=True)

    #strips any character containing *+^  from element_number
    apu_df['Element_Number_stripped'] = apu_df['Element_Number'].astype(str).map(lambda x: x.strip('*+^').lower())
    #creates a subset an removes any trailing letters from element_number column
    apu_df['Element_Number_grp'] = apu_df['Element_Number_stripped'].str.replace(r'[a-z]$', '', regex=True)
    #adds a column named in_apu
    apu_df['in_apu']=1

    
    return proc_df_1,sf_df,apu_df

if __name__ == '__main__':
    
    #Reading in information for LTCH
    ltch_apu_df = pd.read_excel('C:/Users/hsoriano/Downloads/APU_Tables.xlsx', sheet_name='LTCH')
    ltch_sf_df = pd.read_excel('C:/Users/hsoriano/Downloads/Source_File_Name_23Q4_2400405_draft.xlsx', sheet_name='LTCH_Assessment')
    ltch_proc_df = pd.read_excel('C:/Users/hsoriano/Downloads/23Q4_Proc_Content.xlsx', sheet_name='LTCH')
    
    #Cleans all three files
    ltch_clean_proc,ltch_clean_sf,ltch_clean_apu = df_clean(ltch_proc_df,ltch_sf_df,ltch_apu_df)
    
    #merging Source file with pro contents and saves it in new dataframe
    ltch_merged_sf_proc_df = ltch_clean_sf.merge(ltch_clean_proc, on='Variable', how='left')
    #Merges previous dataframe with apu information
    ltch_merged_sf_proc_apu_df = ltch_merged_sf_proc_df.merge(ltch_clean_apu, on='Element_Number_grp', how='left')

    #exports it to excel
    ltch_merged_sf_proc_apu_df.to_excel( 'C:/Users/hsoriano/Downloads/Merged_LTCH.xlsx', sheet_name='Sheet1')
    
    #Reading in information for SNF
    snf_apu_df = pd.read_excel('C:/Users/hsoriano/Downloads/APU_Tables.xlsx', sheet_name='SNF')
    snf_sf_df = pd.read_excel('C:/Users/hsoriano/Downloads/Source_File_Name_23Q4_2400405_draft.xlsx', sheet_name='SNF_Assessment')
    snf_proc_df = pd.read_excel('C:/Users/hsoriano/Downloads/23Q4_Proc_Content.xlsx', sheet_name='SNF')
    
    #Cleans all three files
    snf_clean_proc,snf_clean_sf,snf_clean_apu = df_clean(snf_proc_df,snf_sf_df,snf_apu_df)
    
    #merging Source file with pro contents and saves it in new dataframe
    snf_merged_sf_proc_df = snf_clean_sf.merge(snf_clean_proc, on='Variable', how='left')
    #Merges previous dataframe with apu information
    snf_merged_sf_proc_apu_df = snf_merged_sf_proc_df.merge(snf_clean_apu, on='Element_Number_grp', how='left')
  
    
    snf_merged_sf_proc_apu_df.to_excel( 'C:/Users/hsoriano/Downloads/Merged_SNF.xlsx', sheet_name='Sheet1')
