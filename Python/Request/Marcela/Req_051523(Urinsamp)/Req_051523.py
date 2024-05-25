import pandas as pd
import datacompy
import pyodbc as pyodbc
import numpy as np

def extract(database):
    """
    Createds a dataframe from access table IMPACTLAB_A with RRID and LABID.

    Parameters
    ----------
    database : Dataframe
        Access dataframe.

    Returns
    -------
    df : Dataframe
        Returns extracted dataframe from table IMPACTLAB_A with rrid and labid.

    """
    query = "SELECT rrid,labid,urinsamp FROM IMPACTLAB_A"
    cnxn = pyodbc.connect(database)
    df = pd.read_sql(query, cnxn)
    cnxn.close()
    return df
    

def create_main(df_cchc,df_covid,df_drs,df_syv):
    """
    Function that creates the main CCHC dataset

    Parameters
    ----------
    df_cchc : Dataframe
        Main access dataset for Brownsvill, Harlingen and Laredo.
    df_covid : Dataframe
        Covid dataset.
    df_syv : Dataframe
        Salud y Vida dataset.
    df_drs : Dataframe
        DRS dataset.

    Returns
    -------
    df_cchc : Dataframe
        Returns the combined dataset called main CCHC dataset.

    """
    
    df_cchc['labid'] = df_cchc['labid'].str.strip()
    df_cchc['labid'] = df_cchc['labid'].str.upper()
    
    df_cchc['rrid'] = df_cchc['rrid'].str.strip()
    df_cchc['rrid'] = df_cchc['rrid'].str.upper()
    
    #df_covid=df_covid[['rrid','labid']]

    #change column headers to lowercase
    df_drs.columns= df_drs.columns.str.lower()
    #keep rrid and labid from DRS dataset
    df_drs=df_drs[['rrid','labid','urinsamp']]
    df_cchc=pd.concat([df_cchc,df_covid,df_drs,df_syv],sort=True,ignore_index=True)
    
    
    return df_cchc

def cleanCovid(df_covid):
    
    df_covid=df_covid.sort_values(by=['study_id', 'cov_rrid'])
    df_covid['cov_rrid'] = df_covid.groupby('study_id')['cov_rrid'].transform('first')
    
    column_list = list(df_covid.columns)
    A_index = column_list.index('cov_visit_lab')
    B_index = column_list.index('phase_2_lab_corp_results_covid19_complete')
    df_covid_lab = df_covid.loc[:, list(df_covid.columns[0:5]) + list(df_covid.columns[A_index:B_index+1])]
    df_covid_lab1 = df_covid_lab.loc[(df_covid_lab['redcap_event_name'].str.contains(r'^visit[0-9]_arm_[0-9]')) ]
    
    df_covid_lab2 =df_covid_lab1[['cov_rrid','cov_ullabid','cov_labid_crl','cov_urinsamp']]
    df_covid_lab3 = df_covid_lab2.loc[(~df_covid_lab2.cov_rrid.isna())] 
    df_covid_lab4 = df_covid_lab3.loc[(~df_covid_lab3.cov_labid_crl.isna()) | (~df_covid_lab3.cov_ullabid.isna())]
    
    df_covid_lab4.loc[(df_covid_lab4['cov_labid_crl'] == 'CV 0261'),'cov_labid_crl'] = 'CV0261'
    df_covid_lab5 = df_covid_lab4.loc[~(df_covid_lab4.cov_labid_crl == df_covid_lab4.cov_ullabid)]
    
    df_covid_lab4.rename(columns={'cov_rrid': 'rrid', 'cov_labid_crl': 'labid',
                                  'cov_urinsamp': 'urinsamp'}, inplace=True)
    df_covid_lab5 =df_covid_lab4[['rrid','labid','urinsamp']]


    return df_covid_lab5

def cleanSYV(df_syv):
    
    df_syv=df_syv.sort_values(by=['study_id', 'cov_rrid'])
    df_syv['cov_rrid'] = df_syv.groupby('study_id')['cov_rrid'].transform('first')
    
    column_list = list(df_syv.columns)
    A_index = column_list.index('cov_visit_lab')
    B_index = column_list.index('phase2_laboratory_covid19_complete')
    df_syv_lab = df_syv.loc[:, list(df_syv.columns[0:5]) + list(df_syv.columns[A_index:B_index+1])]
    df_syv_lab.dropna(subset=list(df_syv.columns[A_index:B_index+1]), how='all', inplace=True)
    
    A_index = column_list.index('cov_visit_crl')
    B_index = column_list.index('phase_2_crl_results_covid19_complete')
    df_syv_crl = df_syv.loc[:, list(df_syv.columns[0:5]) + list(df_syv.columns[A_index:B_index+1])]
    df_syv_crl.dropna(subset=list(df_syv.columns[A_index:B_index+1]), how='all', inplace=True)
    
    df_syv_lab1 = df_syv_lab[['cov_rrid','cov_ullabid','cov_urinsamp']]
    df_syv_crl1 = df_syv_crl[['cov_rrid','cov_labid_crl']]
    
    df_syv_crl1['cov_labid_crl'] = df_syv_crl1['cov_labid_crl'].str.strip()
    df_syv_crl1['cov_labid_crl'] = df_syv_crl1['cov_labid_crl'].str.upper()
    df_syv_lab1['cov_ullabid'] = df_syv_lab1['cov_ullabid'].str.strip()
    df_syv_lab1['cov_ullabid'] = df_syv_lab1['cov_ullabid'].str.upper()
    
    df_syv_crl2 = df_syv_crl1.loc[(~df_syv_crl1.cov_labid_crl.isna())].reset_index(drop=True)
    df_syv_lab2 = df_syv_lab1.loc[(~df_syv_lab1.cov_ullabid.isna())].reset_index(drop=True) 
   
    df_syv_lab2.rename(columns={'cov_rrid': 'rrid', 'cov_ullabid': 'labid',
                                  'cov_urinsamp': 'urinsamp'}, inplace=True)
    #df_syv_full=df_syv_lab2.merge(df_syv_crl2, how='outer',on='labid')
    
    return df_syv_lab2


if __name__ == "__main__":
    #bring in all of the datasets into python
    #constr_bro = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\Uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\New_CCHC\Server\BD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    #constr_har = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\New_CCHC\HRL\CCHC_HD_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    #constr_lar = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\New_CCHC\LD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;'     
    #df_covid=pd.read_csv(r'\\uctnascifs.uthouston.edu\uthsc\SPH\Users\hsoriano\Python\COVID_Restructure\COHORTCOVID19_DATA_2023-04-21_0933.csv')
    #df_drs=pd.read_sas(r'\\uctnascifs.uthouston.edu\uthsc\sph\research\brownsvillesd\public\Diabetes_Core\MSAccess\FINAL DATA\DRS\patient_alldrs_iz.sas7bdat',encoding='latin1')
    #df_syv=pd.read_csv(r'H:\Data Request\DR. McC\Req_042423(Spec Numbers)\NONCOHORTCOVID19SyVA_DATA_2023-04-25_1324.csv')
    

    #extract rrid,labid,edta1-edta4,and rna from these datasets and combine
    #df_cchc=pd.concat([extract(constr_bro),extract(constr_har),extract(constr_lar)],sort=True,ignore_index=True)
     
    #df_covid=cleanCovid(df_covid)
    #df_syv=cleanSYV(df_syv)
    #df_cchc=create_main(df_cchc,df_covid,df_drs,df_syv)

    df_excel_hea= pd.read_excel(r'U:\Users\hsoriano\Data Request\Marcela\Req_051523(Urinsamp)\LABIDsDKD.xlsx', sheet_name='LABIDsDKD')
    df_excel_marc= pd.read_excel(r'U:\Users\hsoriano\Data Request\Marcela\Req_051523(Urinsamp)\URINE SENT TO CRL 11-16-15 MM.xlsx', sheet_name='Sheet1')
    df_excel_inv= pd.read_excel(r'U:\Research\CRU\LABORATORY CRU\LAB CRU Inventory\ALIQUOTS INVENTORY ALL studies\URINE\URINE Samples.xlsx', sheet_name='Urine collected')
    df_freezerpro= pd.read_csv(r'U:\Users\hsoriano\Data Request\Marcela\Req_051523(Urinsamp)\SamplesReport_1684177100_258.csv')
    
    
    df_cchc_base = df_excel_inv.iloc[:,:4]
    df_cchc_base.columns=df_cchc_base.iloc[0]
    df_cchc_base=df_cchc_base.tail(-1)
    df_cchc_base1=df_cchc_base.loc[~df_cchc_base['Amount'].isin([0,np.nan,'PENDING','NO URINE'])]
    Amounts = df_cchc_base1.Amount.unique()
    df_cchc_base1['labid'] = df_cchc_base1['LAB ID'].str.split(" ", n = 1, expand = True)[0]
    df_cchc_base1.rename(columns={"BD Number":"rrid"},inplace=True)
    df_cchc_base2=df_cchc_base1[['rrid','labid','Amount']]
    
    df_cchc_base2['rrid'] = df_cchc_base2['rrid'].str.strip()
    df_cchc_base2['rrid'] = df_cchc_base2['rrid'].str.upper()
    
    df_cchc_base2['labid'] = df_cchc_base2['labid'].str.strip()
    df_cchc_base2['labid'] = df_cchc_base2['labid'].str.upper()
    
    df_cchc_5y = df_excel_inv.iloc[:,5:8]
    df_cchc_5y.columns=df_cchc_5y.iloc[0]
    df_cchc_5y=df_cchc_5y.tail(-1)
    Amounts = df_cchc_5y.Amount.unique()
    df_cchc_5y1=df_cchc_5y.loc[~df_cchc_5y['Amount'].isin([0,np.nan,'NOT OBTAINED','NO URINE COLLECTED'])]

    df_cchc_5y1.rename(columns={"BD Number":"rrid",
                                  "LAB ID":"labid"},inplace=True)
    
    df_cchc_5y1['rrid'] = df_cchc_5y1['rrid'].str.strip()
    df_cchc_5y1['rrid'] = df_cchc_5y1['rrid'].str.upper()
    
    df_cchc_5y1['labid'] = df_cchc_5y1['labid'].str.strip()
    df_cchc_5y1['labid'] = df_cchc_5y1['labid'].str.upper()
    
    
    df_cchc_ped= df_excel_inv.iloc[:,10:13]
    df_cchc_ped.columns =['rrid', 'labid', 'Amount']
    Amounts = df_cchc_ped.Amount.unique()
    df_cchc_ped1=df_cchc_ped.loc[~df_cchc_ped['Amount'].isin([0,np.nan,'REFUSED','NONE COLLECTED'])]

    df_cchc_ped1['rrid'] = df_cchc_ped1['rrid'].str.strip()
    df_cchc_ped1['rrid'] = df_cchc_ped1['rrid'].str.upper()
    
    df_cchc_ped1['labid'] = df_cchc_ped1['labid'].str.strip()
    df_cchc_ped1['labid'] = df_cchc_ped1['labid'].str.upper()
    
    df_cchc_10Y= df_excel_inv.iloc[:,15:18]
    df_cchc_10Y.columns =['rrid', 'labid', 'Amount']
    Amounts = df_cchc_10Y.Amount.unique()
    df_cchc_10Y1=df_cchc_10Y.loc[~df_cchc_10Y['Amount'].isin([0,np.nan,'REFUSED','PENDING'])]

    df_cchc_10Y1['rrid'] = df_cchc_10Y1['rrid'].str.strip()
    df_cchc_10Y1['rrid'] = df_cchc_10Y1['rrid'].str.upper()
    
    df_cchc_10Y1['labid'] = df_cchc_10Y1['labid'].str.strip()
    df_cchc_10Y1['labid'] = df_cchc_10Y1['labid'].str.upper()
    
    df_cchc_15Y= df_excel_inv.iloc[:,25:28]
    df_cchc_15Y.columns =['rrid', 'labid', 'Amount']
    Amounts = df_cchc_15Y.Amount.unique()
    df_cchc_15Y1=df_cchc_15Y.loc[~df_cchc_15Y['Amount'].isin([0,np.nan,'NO URINE; REFUSED','NO URINE'])]

    df_cchc_15Y1['rrid'] = df_cchc_15Y1['rrid'].str.strip()
    df_cchc_15Y1['rrid'] = df_cchc_15Y1['rrid'].str.upper()
    
    df_cchc_15Y1['labid'] = df_cchc_15Y1['labid'].str.strip()
    df_cchc_15Y1['labid'] = df_cchc_15Y1['labid'].str.upper()   
    
    df_cchc_20Y = df_excel_inv.iloc[:,30:33]
    df_cchc_20Y.columns=df_cchc_20Y.iloc[0]
    df_cchc_20Y=df_cchc_20Y.tail(-1)
    Amounts = df_cchc_20Y.Amount.unique()
    df_cchc_20Y1=df_cchc_20Y.loc[~df_cchc_20Y['Amount'].isin([0,np.nan])]

    df_cchc_20Y1['labid'] = df_cchc_20Y1['LAB ID'].str.split(" ", n = 1, expand = True)[0]
    df_cchc_20Y1.rename(columns={"BD #":"rrid"},inplace=True)
    df_cchc_20Y2=df_cchc_20Y1[['rrid','labid','Amount']]
    
    df_cchc_20Y2['rrid'] = df_cchc_20Y2['rrid'].str.strip()
    df_cchc_20Y2['rrid'] = df_cchc_20Y2['rrid'].str.upper()
    
    df_cchc_20Y2['labid'] = df_cchc_20Y2['labid'].str.strip()
    df_cchc_20Y2['labid'] = df_cchc_20Y2['labid'].str.upper()
    
    df_cchc_DRS = df_excel_inv.iloc[:,35:38]
    df_cchc_DRS.columns=df_cchc_DRS.iloc[0]
    df_cchc_DRS=df_cchc_DRS.tail(-1)
    Amounts = df_cchc_DRS.Amount.unique()
    df_cchc_DRS1=df_cchc_DRS.loc[~df_cchc_DRS['Amount'].isin([0,np.nan])]
    
    df_cchc_DRS1['labid'] = df_cchc_DRS1['LAB ID'].str.split(" ", n = 1, expand = True)[0]
    df_cchc_DRS1.rename(columns={"BD #":"rrid"},inplace=True)
    df_cchc_DRS2=df_cchc_DRS1[['rrid','labid','Amount']]
    
    df_cchc_DRS2['rrid'] = df_cchc_DRS2['rrid'].str.strip()
    df_cchc_DRS2['rrid'] = df_cchc_DRS2['rrid'].str.upper()
    
    df_cchc_DRS2['labid'] = df_cchc_DRS2['labid'].str.strip()
    df_cchc_DRS2['labid'] = df_cchc_DRS2['labid'].str.upper()
    
    df_cchc_Cov = df_excel_inv.iloc[:,40:43]
    df_cchc_Cov.columns=df_cchc_Cov.iloc[0]
    df_cchc_Cov=df_cchc_Cov.tail(-1)
    Amounts = df_cchc_Cov.Amount.unique()
    df_cchc_Cov1=df_cchc_Cov.loc[~df_cchc_Cov['Amount'].isin([0,np.nan,'NO URINE'])]

    df_cchc_Cov1['labid'] = df_cchc_Cov1['LAB ID'].str.split(" ", n = 1, expand = True)[0]
    df_cchc_Cov1.rename(columns={"BD Number":"rrid"},inplace=True)
    df_cchc_Cov2=df_cchc_Cov1[['rrid','labid','Amount']]
    
    df_cchc_Cov2['rrid'] = df_cchc_Cov2['rrid'].str.strip()
    df_cchc_Cov2['rrid'] = df_cchc_Cov2['rrid'].str.upper()
    
    df_cchc_Cov2['labid'] = df_cchc_Cov2['labid'].str.strip()
    df_cchc_Cov2['labid'] = df_cchc_Cov2['labid'].str.upper()
    
    
    df_final_inv_cru=pd.concat([df_cchc_base2,df_cchc_5y1,
                                df_cchc_ped1,df_cchc_10Y1,
                                df_cchc_15Y1,df_cchc_20Y2,
                                df_cchc_DRS2,df_cchc_Cov2
                                ],sort=True,ignore_index=True)
    df_final_inv_cru["from_inv"]=1



    
    
    df_freezerpro["Name"] = df_freezerpro["Name"].str.split().str[0]
    df_freezerpro.loc[(df_freezerpro.Name == "5YH0005") & (df_freezerpro['Patient ID'] == "HD0024"),
                       "Name"]="5YH0004"
    Amounts = df_freezerpro.Volume.unique()
    df_freezerpro1=df_freezerpro.groupby(['Name','Patient ID']).size().reset_index(name='count')

    df_dups= df_freezerpro1[df_freezerpro1.duplicated(['Name'], keep=False)]

    df_freezerpro1.rename(columns={"Name":"labid","Patient ID":"rrid","count":"vials"},inplace=True)
    df_freezerpro1["from_fp"]=1
    
    df_final_inv=df_final_inv_cru.merge(df_freezerpro1,how="outer",on=['labid','rrid'])

    df_dups= df_final_inv[df_final_inv.duplicated(['labid'], keep=False)]

    df_final_inv["has_urin"]=1
    
    df_excel_marc=df_excel_marc.loc[~df_excel_marc['RRID'].isna()]
    mask=df_excel_marc.RRID.str[:3].isin(['BD2'])
    df_excel_marc.loc[mask,"RRID"] = 'BD6' + df_excel_marc.loc[mask, 'RRID'].str[3:]
    df_excel_marc['used_urine']=1
    df_excel_marc.rename(columns={"RRID":"labid"},inplace=True)
    df_excel_marc=df_excel_marc[['labid','used_urine']]

    df_excel_hea.columns=df_excel_hea.columns.str.lower()
    df_final_hea=df_excel_hea.merge(df_final_inv,how="left",on=['labid','rrid'])
    
    df_final_hea2=df_final_hea.merge(df_excel_marc,how="left",on=['labid'])
    
    df_final_hea2.loc[(df_final_hea2.has_urin==1),"updated_urine"]=1

    df_final_hea2.loc[(df_final_hea2.has_urin==1) & (df_final_hea2.used_urine==1),"updated_urine"]=0
    
    #---------------------------------------------------Serum------------------------------------------------------
    
    df_serum_inv1= pd.read_excel(r'U:\Research\CRU\LABORATORY CRU\LAB CRU Inventory\ALIQUOTS INVENTORY ALL studies\COHORT-ADULT\CCHC INVENTORY.xlsx', sheet_name='Sheet1')
    
    df_serum_inv1=df_serum_inv1[['BD#','LAB ID #','SERUM 1','SERUM 2','SERUM 3','SERUM 4'
                                 ,'SERUM 5','SERUM 6','SERUM 7','SERUM 8']]
    Amounts = df_serum_inv1['SERUM 8'].unique()
    df_serum_inv1.loc[df_serum_inv1['SERUM 1'].isin([0,np.nan,'TO CRL','NEED  ','* NO PLAIN RED  DRAWN']),"SERUM 1"]=np.nan
    df_serum_inv1.loc[df_serum_inv1['SERUM 2'].isin([0,np.nan,'TO']),"SERUM 2"]=np.nan
    df_serum_inv1.loc[df_serum_inv1['SERUM 3'].isin([0,np.nan,'EDTA 2 NOT DRAWN','PARTIAL EDTA2',
                                                     'PLASMA 2.7 2/29/2016','HARD SPIN PLASMA 1.3 & 1.4; NO EDTA2',
                                                     'NO EDTA 2','DRAW',]),"SERUM 3"]=np.nan
    df_serum_inv1.loc[df_serum_inv1['SERUM 4'].isin([0,np.nan,'until here we ','PARTIAL FILL EDTA2; NOT ENOUGH PLASMA',
                                                     'send to CRL','not enough plasma ','EDTA # 2 ',
                                                     'not send to CRL','CRL','until here to send to CRL on January 2011',
                                                     'NO EDTA 3','TO CRL','CRL 1 ML FROM EDTA 1','PARTIAL FILL EDTA1; NO EDTA2; NO EDTA3',
                                                     'NO EDTA 2','reassigned on 10/04/11','NO EDTA 2, NO EDTA 3',
                                                     'SENT TO CRL','Plasma 2.4-2.6 taken from EDTA 1','Buffy2 taken from EDTA1',
                                                     'lab id\'s renumbered beginning here','Marcela took to campus 2/11/15 for DNA Extraction',
                                                     'No EDTA #2','BLOOD']),"SERUM 4"]=np.nan
    df_serum_inv1.loc[df_serum_inv1['SERUM 8'].isin([0,np.nan]),"SERUM 8"]=np.nan
    
    df_serum_inv1.loc[(df_serum_inv1[['SERUM 1','SERUM 2','SERUM 3','SERUM 4'
                                     ,'SERUM 5','SERUM 6','SERUM 7','SERUM 8']].isna().all(1) ),"has_serum"]=0
    
    df_serum_inv1.loc[(df_serum_inv1['has_serum'].isna()),"has_serum"]=1
    
    df_serum_inv1['BD#'] = df_serum_inv1['BD#'].str.strip()
    df_serum_inv1['BD#'] = df_serum_inv1['BD#'].str.upper()
    
    df_serum_inv1['LAB ID #'] = df_serum_inv1['LAB ID #'].str.strip()
    df_serum_inv1['LAB ID #'] = df_serum_inv1['LAB ID #'].str.upper()
    
    df_serum_inv1.rename(columns={"BD#":"rrid","LAB ID #":"labid"},inplace=True)

    
    df_serum_final1 = df_serum_inv1.loc[(df_serum_inv1['has_serum']==1)]

    df_serum_inv2= pd.read_excel(r'U:\Research\CRU\LABORATORY CRU\LAB CRU Inventory\ALIQUOTS INVENTORY ALL studies\5YFU\5 YRS Follow up Inventory.xlsx', sheet_name='Sheet1')
    column_list=list(df_serum_inv2.columns)
    df_serum_inv2=df_serum_inv2[['5Y#','BD # ','SERUM 1','SERUM 2','SERUM 3','SERUM 4'
                                 ,'SERUM 5','SERUM 6','SERUM 7','SERUM 8']]
    
    Amounts = df_serum_inv2['SERUM 7'].unique()
    df_serum_inv2.loc[df_serum_inv2['SERUM 1'].isin([0,np.nan]),"SERUM 1"]=np.nan
    df_serum_inv2.loc[df_serum_inv2['SERUM 2'].isin([0,np.nan]),"SERUM 2"]=np.nan
    df_serum_inv2.loc[df_serum_inv2['SERUM 3'].isin([0,np.nan]),"SERUM 3"]=np.nan
    df_serum_inv2.loc[df_serum_inv2['SERUM 4'].isin([0,np.nan]),"SERUM 4"]=np.nan
    df_serum_inv2.loc[df_serum_inv2['SERUM 5'].isin([0,np.nan]),"SERUM 5"]=np.nan
    df_serum_inv2.loc[df_serum_inv2['SERUM 6'].isin([0,np.nan]),"SERUM 6"]=np.nan
    df_serum_inv2.loc[df_serum_inv2['SERUM 7'].isin([0,np.nan]),"SERUM 7"]=np.nan
    df_serum_inv2.loc[df_serum_inv2['SERUM 8'].isin([0,np.nan]),"SERUM 8"]=np.nan

    df_serum_inv2.loc[(df_serum_inv2[['SERUM 1','SERUM 2','SERUM 3','SERUM 4'
                                     ,'SERUM 5','SERUM 6','SERUM 7','SERUM 8']].isna().all(1) ),"has_serum"]=0
    
    df_serum_inv2.loc[(df_serum_inv2['has_serum'].isna()),"has_serum"]=1
    
    df_serum_inv2['BD # '] = df_serum_inv2['BD # '].str.strip()
    df_serum_inv2['BD # '] = df_serum_inv2['BD # '].str.upper()
    
    df_serum_inv2['5Y#'] = df_serum_inv2['5Y#'].str.strip()
    df_serum_inv2['5Y#'] = df_serum_inv2['5Y#'].str.upper()
    
    df_serum_inv2.rename(columns={"BD # ":"rrid","5Y#":"labid"},inplace=True)

    df_serum_final2 = df_serum_inv2.loc[(df_serum_inv2['has_serum']==1)]
    
    df_serum_inv3= pd.read_excel(r'U:\Research\CRU\LABORATORY CRU\LAB CRU Inventory\ALIQUOTS INVENTORY ALL studies\10YFU\10 YRS Follow up Inventory.xlsx', sheet_name='Sheet1')
    column_list=list(df_serum_inv3.columns)
    df_serum_inv3=df_serum_inv3[['10Y#','BD#','SERUM  1','SERUM 2','SERUM 3','SERUM 4'
                                 ,'SERUM 5','SERUM 6','SERUM 7','SERUM 8']]
    
    Amounts = df_serum_inv3['SERUM 8'].unique()
    df_serum_inv3.loc[df_serum_inv3['SERUM 7'].isin([0,np.nan]),"SERUM 7"]=np.nan
    df_serum_inv3.loc[df_serum_inv3['SERUM 8'].isin([0,np.nan]),"SERUM 8"]=np.nan
    
    df_serum_inv3.loc[(df_serum_inv3[['SERUM  1','SERUM 2','SERUM 3','SERUM 4'
                                     ,'SERUM 5','SERUM 6','SERUM 7','SERUM 8']].isna().all(1) ),"has_serum"]=0
    
    df_serum_inv3.loc[(df_serum_inv3['has_serum'].isna()),"has_serum"]=1
    
    df_serum_inv3['BD#'] = df_serum_inv3['BD#'].str.strip()
    df_serum_inv3['BD#'] = df_serum_inv3['BD#'].str.upper()
    
    df_serum_inv3['10Y#'] = df_serum_inv3['10Y#'].str.strip()
    df_serum_inv3['10Y#'] = df_serum_inv3['10Y#'].str.upper()
    
    df_serum_inv3.rename(columns={"BD#":"rrid","10Y#":"labid","SERUM  1":"SERUM 1"},inplace=True)

    df_serum_final3 = df_serum_inv3.loc[(df_serum_inv3['has_serum']==1)]
    
    df_serum_inv4= pd.read_excel(r'U:\Research\CRU\LABORATORY CRU\LAB CRU Inventory\ALIQUOTS INVENTORY ALL studies\15YFU\15 YEAR FOLLOW UP.xlsx', sheet_name='Sheet1')
    df_serum_inv4 = df_serum_inv4.iloc[:,[1,2,24,25,26,27,28,29,30,31]]

    column_list=list(df_serum_inv4.columns)
    df_serum_inv4.rename(columns={"Unnamed: 1":"rrid","Unnamed: 2":"labid","Unnamed: 24":"SERUM 1"
                                  ,"Unnamed: 25":"SERUM 2","Unnamed: 26":"SERUM 3","Unnamed: 27":"SERUM 4"
                                  ,"Unnamed: 28":"SERUM 5","Unnamed: 29":"SERUM 6","Unnamed: 30":"SERUM 7"
                                  ,"Unnamed: 31":"SERUM 8"},inplace=True)

    df_serum_inv4 = df_serum_inv4.iloc[4:]
    df_serum_inv4.loc[df_serum_inv4['SERUM 8'].isin([0,np.nan]),"SERUM 8"]=np.nan
    
    df_serum_inv4.loc[(df_serum_inv4[['SERUM 1','SERUM 2','SERUM 3','SERUM 4'
                                     ,'SERUM 5','SERUM 6','SERUM 7','SERUM 8']].isna().all(1) ),"has_serum"]=0
    
    df_serum_inv4.loc[(df_serum_inv4['has_serum'].isna()),"has_serum"]=1
    
    df_serum_inv4['rrid'] = df_serum_inv4['rrid'].str.strip()
    df_serum_inv4['rrid'] = df_serum_inv4['rrid'].str.upper()
    
    df_serum_inv4['labid'] = df_serum_inv4['labid'].str.strip()
    df_serum_inv4['labid'] = df_serum_inv4['labid'].str.upper()
    
    df_serum_final4 = df_serum_inv4.loc[(df_serum_inv4['has_serum']==1)]

    df_serum_inv5= pd.read_excel(r'U:\Research\CRU\LABORATORY CRU\LAB CRU Inventory\ALIQUOTS INVENTORY ALL studies\DRS\DRS INVENTORY.xlsx', sheet_name='Sheet1')
    column_list=list(df_serum_inv5.columns)
    df_serum_inv5=df_serum_inv5[['DR #','BD#','SERUM      1 ',
                                 'SERUM     2','SERUM     3','SERUM     4'
                                 ]]
    
    df_serum_inv5.rename(columns={"BD#":"rrid","DR #":"labid","SERUM      1 ":"SERUM 1"
                                  ,"SERUM     2":"SERUM 2","SERUM     3":"SERUM 3","SERUM     4":"SERUM 4"
                                 },inplace=True)
    
    df_serum_inv5.loc[df_serum_inv5['SERUM 1'].isin([0,np.nan]),"SERUM 1"]=np.nan
    df_serum_inv5.loc[df_serum_inv5['SERUM 2'].isin([0,np.nan]),"SERUM 2"]=np.nan
    df_serum_inv5.loc[df_serum_inv5['SERUM 3'].isin([0,np.nan]),"SERUM 3"]=np.nan
    df_serum_inv5.loc[df_serum_inv5['SERUM 4'].isin([0,np.nan]),"SERUM 4"]=np.nan

    
    df_serum_inv5.loc[(df_serum_inv5[['SERUM 1','SERUM 2','SERUM 3','SERUM 4'
                                    ]].isna().all(1) ),"has_serum"]=0
    
    df_serum_inv5.loc[(df_serum_inv5['has_serum'].isna()),"has_serum"]=1
    
    df_serum_inv5['rrid'] = df_serum_inv5['rrid'].str.strip()
    df_serum_inv5['rrid'] = df_serum_inv5['rrid'].str.upper()
    
    df_serum_inv5['labid'] = df_serum_inv5['labid'].str.strip()
    df_serum_inv5['labid'] = df_serum_inv5['labid'].str.upper()
    
    df_serum_final5 = df_serum_inv5.loc[(df_serum_inv5['has_serum']==1)]
    
    df_serum_final6=pd.concat([df_serum_final1,df_serum_final2,
                                df_serum_final3,df_serum_final4,
                                df_serum_final5
                                ],sort=True,ignore_index=True)
    
    df_freezerpro_ser= pd.read_csv(r'U:\Users\hsoriano\Data Request\Marcela\Req_051523(Urinsamp)\SamplesReport_1684256158_118.csv')

    df_freezerpro_ser["Name"] = df_freezerpro_ser["Name"].str.split().str[0]
    df_freezerpro_ser.loc[(df_freezerpro_ser.Name == "CV0012") & (df_freezerpro_ser['Patient ID'] == "BD1662"),
                       "Name"]="CV0112"
    df_freezerpro_ser.loc[(df_freezerpro_ser.Name == "BA0577") & (df_freezerpro_ser['Patient ID'] == "BA3092"),
                       "Patient ID"]="BD3092"
    df_freezerpro_ser.loc[(df_freezerpro_ser.Name == "5Y1501") & (df_freezerpro_ser['Patient ID'] == "BD2991"),
                       "Name"]="5Y1502"
    df_freezerpro_ser.loc[(df_freezerpro_ser.Name == "5Y1224") & (df_freezerpro_ser['Patient ID'] == "BD2774"),
                       "Name"]="5Y1225"
    df_freezerpro_ser.loc[(df_freezerpro_ser.Name == "5Y1502") & (df_freezerpro_ser['Patient ID'] == "BD2291"),
                       "Patient ID"]="BD2991"

    df_freezerpro_ser1=df_freezerpro_ser.groupby(['Name','Patient ID']).size().reset_index(name='count')

    df_dups= df_freezerpro_ser1[df_freezerpro_ser1.duplicated(['Name'], keep=False)]

    df_freezerpro_ser1.rename(columns={"Name":"labid","Patient ID":"rrid","count":"vials"},inplace=True)
    df_freezerpro_ser1["from_fp"]=1
    
    df_final_ser=df_serum_final6.merge(df_freezerpro_ser1,how="outer",on=['labid','rrid'])

    df_final_ser.loc[(df_final_ser.labid == "BD6329") & (df_final_ser.rrid == "BD2330"),
                       "labid"]="BD6330"
    df_final_ser.loc[(df_final_ser.labid == "BD6474") & (df_final_ser.rrid == "BD2475"),
                       "labid"]="BD6475"
    df_final_ser.loc[(df_final_ser.labid == "BA0479") & (df_final_ser.rrid == "BD3575"),
                       "labid"]="BA0749"
    df_dups= df_final_ser[df_final_ser.duplicated(['labid'], keep=False)]

    df_final_ser["has_serum"]=1
    
    df_final_hea3=df_final_hea2.merge(df_final_ser,how="left",on=['labid'])

    
    writer = pd.ExcelWriter(r'U:\Users\hsoriano\Data Request\Marcela\Req_051523(Urinsamp)\LABIDsDKD_V3.xlsx', engine='xlsxwriter')


    df_final_hea3.to_excel(writer, sheet_name='Sheet1',index=False)

    writer.save()
    
    

