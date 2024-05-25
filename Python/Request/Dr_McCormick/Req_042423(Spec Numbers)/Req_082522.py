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
    query = "SELECT rrid,labid,edta1,edta2,edta3,edta4,rna FROM IMPACTLAB_A"
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
    df_drs=df_drs[['rrid','labid','edta1','edta2','edta3','edta4','rna']]
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
    
    df_covid_lab2 =df_covid_lab1[['cov_rrid','cov_ullabid','cov_labid_crl','cov_edta1','cov_edta3','cov_edta4','cov_rna']]
    df_covid_lab3 = df_covid_lab2.loc[(~df_covid_lab2.cov_rrid.isna())] 
    df_covid_lab4 = df_covid_lab3.loc[(~df_covid_lab3.cov_labid_crl.isna()) | (~df_covid_lab3.cov_ullabid.isna())]
    
    df_covid_lab4.loc[(df_covid_lab4['cov_labid_crl'] == 'CV 0261'),'cov_labid_crl'] = 'CV0261'
    df_covid_lab5 = df_covid_lab4.loc[~(df_covid_lab4.cov_labid_crl == df_covid_lab4.cov_ullabid)]
    
    df_covid_lab4.rename(columns={'cov_rrid': 'rrid', 'cov_labid_crl': 'labid',
                                  'cov_edta1': 'edta1', 'cov_edta3': 'edta3',
                                  'cov_edta4': 'edta4', 'cov_rna': 'rna'}, inplace=True)
    df_covid_lab5 =df_covid_lab4[['rrid','labid','edta1','edta3','edta4','rna']]


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
    
    df_syv_lab1 = df_syv_lab[['cov_rrid','cov_ullabid','cov_edta1','cov_edta3','cov_edta4','cov_rna']]
    df_syv_crl1 = df_syv_crl[['cov_rrid','cov_labid_crl']]
    
    df_syv_crl1['cov_labid_crl'] = df_syv_crl1['cov_labid_crl'].str.strip()
    df_syv_crl1['cov_labid_crl'] = df_syv_crl1['cov_labid_crl'].str.upper()
    df_syv_lab1['cov_ullabid'] = df_syv_lab1['cov_ullabid'].str.strip()
    df_syv_lab1['cov_ullabid'] = df_syv_lab1['cov_ullabid'].str.upper()
    
    df_syv_crl2 = df_syv_crl1.loc[(~df_syv_crl1.cov_labid_crl.isna())].reset_index(drop=True)
    df_syv_lab2 = df_syv_lab1.loc[(~df_syv_lab1.cov_ullabid.isna())].reset_index(drop=True) 
   
    df_syv_lab2.rename(columns={'cov_rrid': 'rrid', 'cov_ullabid': 'labid',
                                  'cov_edta1': 'edta1', 'cov_edta3': 'edta3',
                                  'cov_edta4': 'edta4', 'cov_rna': 'rna'}, inplace=True)
    df_syv_crl2.rename(columns={'cov_rrid': 'rrid', 'cov_labid_crl': 'labid'}, inplace=True)
    #df_syv_full=df_syv_lab2.merge(df_syv_crl2, how='outer',on='labid')
    
    return df_syv_lab2


if __name__ == "__main__":
    #bring in all of the datasets into python
    constr_bro = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\Uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\New_CCHC\Server\BD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    constr_har = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\New_CCHC\HRL\CCHC_HD_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    constr_lar = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\New_CCHC\LD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;'     
    df_covid=pd.read_csv(r'\\uctnascifs.uthouston.edu\uthsc\SPH\Users\hsoriano\Python\COVID_Restructure\COHORTCOVID19_DATA_2023-04-21_0933.csv')
    df_drs=pd.read_sas(r'\\uctnascifs.uthouston.edu\uthsc\sph\research\brownsvillesd\public\Diabetes_Core\MSAccess\FINAL DATA\DRS\patient_alldrs_iz.sas7bdat',encoding='latin1')
    df_syv=pd.read_csv(r'H:\Data Request\DR. McC\Req_042423(Spec Numbers)\NONCOHORTCOVID19SyVA_DATA_2023-04-25_1324.csv')
    

    #extract rrid,labid,edta1-edta4,and rna from these datasets and combine
    df_cchc=pd.concat([extract(constr_bro),extract(constr_har),extract(constr_lar)],sort=True,ignore_index=True)
     
    df_covid=cleanCovid(df_covid)
    df_syv=cleanSYV(df_syv)
    df_cchc=create_main(df_cchc,df_covid,df_drs,df_syv)
    df_cchc=df_cchc.drop_duplicates(subset=['labid'])
    
    df_cchc_sas=pd.read_sas(r'H:\Data Request\DR. McC\Req_042423(Spec Numbers)\cchc_subset.sas7bdat',encoding='latin1')
    df_cchc_sas.columns= df_cchc_sas.columns.str.lower()
    
    df_cchc_baseline_part=df_cchc.drop_duplicates(subset=['rrid'])
    df_spec = pd.read_excel(r'H:\Data Request\DR. McC\Req_042423(Spec Numbers)\RNAseq_lipidomics_proteomics_genotype_id_mapping_updated_20230223.xlsx',sheet_name='Sheet1')
    df_rna_excel1 = pd.read_excel(r'H:\Python\Req_082522(Overlapping_Geno_Pro_Lip_RNA)\Input\RNA manifest overall.xlsx',sheet_name='VU shipment till 2022')
    df_rna_excel2 = pd.read_excel(r'H:\Python\Req_082522(Overlapping_Geno_Pro_Lip_RNA)\Input\RNA manifest overall.xlsx',sheet_name='Othershipment')
    df_prot = pd.read_csv(r'H:\Python\Req_082522(Overlapping_Geno_Pro_Lip_RNA)\Input\Q-02751_McCormick_NPX_2022-07-11.csv',sep = ';')
    df_metabolome = pd.read_excel(r'H:\Data Request\DR. McC\Req_042423(Spec Numbers)\Copy of TOPMed_SampleManifest_BOX_Templatev2 shipped 2-22-23.xlsx',sheet_name='SampleManifest')
    df_rna_marcela1 = pd.read_excel(r'H:\Data Request\DR. McC\Req_042423(Spec Numbers)\All Paxgene and RNA Used 4-3-23.xlsx',sheet_name='Sent to Vanderbuilt')
    df_rna_marcela2 = pd.read_excel(r'H:\Data Request\DR. McC\Req_042423(Spec Numbers)\All Paxgene and RNA Used 4-3-23.xlsx',sheet_name='Used by Other Studies')
    
    
    #get total geno
    df_geno=df_spec[['RRID','genotype_ID']]
    df_geno=df_geno.loc[~df_geno.genotype_ID.isna()]
    df_geno=df_geno.drop_duplicates(subset=['RRID'])
    
    #------------------------Genotype--------------------------------------------------
    df_geno_yes=df_cchc.loc[df_cchc.rrid.isin(df_geno.RRID)]
    df_geno_no=df_cchc.loc[~df_cchc.rrid.isin(df_geno.RRID)]
    #pediatric
    df_geno_pedi=df_geno_yes.loc[df_geno_yes.labid.str[:2].isin(['BL','LP','HP'])]
    df_geno_pedi1=df_geno_pedi.drop_duplicates(subset=['rrid'])
    #adults
    df_geno_adult=df_geno_yes.loc[(~df_geno_yes.labid.str[:2].isin(['BL','LP','HP'])) &
                                  (~df_geno_yes.rrid.isin(df_geno_pedi1.rrid))]
    df_geno_adult1=df_geno_adult.drop_duplicates(subset=['rrid'])
    
    
    #------------------------RNA Samples-----------------------------------------------
    df_rna_spec=df_cchc[['rrid','labid','rna']]
    df_rna_spec1=df_rna_spec.loc[(~df_rna_spec.rna.isna())]
    df_rna_spec2=df_rna_spec1.loc[(~df_rna_spec.rna.isin([2,-1]))]
    
    df_rna_marcela1=df_rna_marcela1[['LABID']]
    df_rna_marcela2=df_rna_marcela2[['LABID']]
    
    df_rna_marcela=pd.concat([df_rna_marcela1[['LABID']],df_rna_marcela2[['LABID']]],sort=True,ignore_index=True)
    df_rna_marcela['LABID'] = df_rna_marcela['LABID'].str.strip()
    df_rna_marcela['LABID'] = df_rna_marcela['LABID'].str.upper()
    df_rna_marcela=df_rna_marcela.loc[~df_rna_marcela.LABID.isna()]
    df_rna_marcela=df_rna_marcela.loc[~df_rna_marcela.LABID.str[:2].isin(['UT','BP'])]
    df_rna_marcela=df_rna_marcela.drop_duplicates(subset=['LABID'])
    df_rna_marcela['shipped_out']=1
    
    df_rna_marcela.columns= df_rna_marcela.columns.str.lower()
    df_rna_spec3=df_rna_spec2.merge(df_rna_marcela, how='left',on='labid')
    df_rna_spec4=df_rna_spec3.loc[df_rna_spec3.shipped_out.isna()]
    
    df_rna_spec5=df_rna_spec4.merge(df_cchc_sas, how='left',on='labid')
    
    df_rna_spec_novisit=df_rna_spec5.loc[df_rna_spec5.visit.isna()]
    df_rna_spec_visit=df_rna_spec5.loc[~df_rna_spec5.visit.isna()]
    
    df_rna_spec_base=df_rna_spec_visit.loc[df_rna_spec_visit.visit==1]
    df_rna_spec_follow=df_rna_spec_visit.loc[df_rna_spec_visit.visit!=1]
    
    df_rna_spec_novisit_base=df_rna_spec_novisit.loc[df_rna_spec_novisit.labid.str[:2].isin(['BL','BA','BD'])]
    df_rna_spec_novisit_follow=df_rna_spec_novisit.loc[~df_rna_spec_novisit.labid.isin(df_rna_spec_novisit_base.labid)]
    

    #------------------------RNA Seq-----------------------------------------------
    df_rna_excel1.columns= df_rna_excel1.columns.str.lower()
    df_rna_excel2.columns= df_rna_excel2.columns.str.lower()
    df_rna_excel1['labid'] = df_rna_excel1['labid'].str.strip()
    df_rna_excel1['labid'] = df_rna_excel1['labid'].str.upper()
    df_rna_excel2['labid'] = df_rna_excel2['labid'].str.strip()
    df_rna_excel2['labid'] = df_rna_excel2['labid'].str.upper()
    
    df_rna_seq=pd.concat([df_rna_excel1[['labid']],df_rna_excel2[['labid']]],sort=True,ignore_index=True)
    df_dups=df_rna_seq[df_rna_seq.duplicated(['labid'], keep=False)]
    df_rna_seq1=df_rna_seq.drop_duplicates(subset=['labid'])
    df_rna_seq3=df_rna_seq1.merge(df_cchc, how='left',on='labid')
    df_rna_seq3.loc[(df_rna_seq3['labid'] == "BD2767"), "rrid"] = "BD2767"
    df_rna_seq3.loc[(df_rna_seq3['labid'] == "BD2768"), "rrid"] = "BD2768"
    df_rna_seq3.loc[(df_rna_seq3['labid'] == "BD2790"), "rrid"] = "BD2790"
    df_rna_seq3.loc[(df_rna_seq3['labid'] == "BP0011"), "rrid"] = "BD2891"
    df_rna_seq3.loc[(df_rna_seq3['labid'] == "BP0013"), "rrid"] = "BD2893"
    df_rna_seq3.loc[(df_rna_seq3['labid'] == "BP0017"), "rrid"] = "BD2897"

    df_rna_seq3_rrid=df_rna_seq3.loc[~df_rna_seq3.rrid.isna()]
    df_rna_seq3_norrid=df_rna_seq3.loc[df_rna_seq3.rrid.isna()]
    
    #pedi
    df_rna_seq_ped=df_rna_seq3_rrid.loc[df_rna_seq3_rrid.labid.str[:2].isin(['BL','LP','HP'])]
    df_rna_seq_ped1=df_rna_seq_ped.drop_duplicates(subset=['rrid'])
    #adult
    df_rna_seq_adult=df_rna_seq3_rrid.loc[(~df_rna_seq3_rrid.labid.str[:2].isin(['BL','LP','HP'])) &
                                          (~df_rna_seq3_rrid.rrid.isin(df_rna_seq_ped1.rrid))]
    df_rna_seq_adult1=df_rna_seq_adult.drop_duplicates(subset=['rrid'])
    
    
    #------------------------RNA Seq using Marcelas file-----------------------------------------------
    
    df_rna_marcela_ped=df_rna_marcela.loc[df_rna_marcela.labid.str[:2].isin(['BL','LP','HP'])]
    df_rna_marcela_noped=df_rna_marcela.loc[~df_rna_marcela.labid.isin(df_rna_marcela_ped.labid)]
    
    df_rna_marcela_noped.loc[(df_rna_marcela_noped['labid'] == "DR767"), "labid"] = "DR0767"
    df_rna_marcela_noped.loc[(df_rna_marcela_noped['labid'] == "DRO471"), "labid"] = "DR0471"
    df_rna_marcela_noped.loc[(df_rna_marcela_noped['labid'] == "DR7598"), "labid"] = "DR0598"
    
    df_rna_marcela_noped=df_rna_marcela_noped.merge(df_cchc_sas, how='left',on='labid')
    
    df_rna_marcela_adult_base=df_rna_marcela_noped.loc[df_rna_marcela_noped.visit==1]
    df_rna_marcela_adult_follo=df_rna_marcela_noped.loc[df_rna_marcela_noped.visit!=1]

    df_rna_marcela_adult_base_part=df_rna_marcela_adult_base.drop_duplicates(subset=['rrid'])
    df_rna_marcela_adult_follo_part=df_rna_marcela_adult_follo.drop_duplicates(subset=['rrid'])

    
    
    #------------------------Proteome O-Link-----------------------------------------------
    #clean up proteomics file dataset
    df_prot.columns= df_prot.columns.str.lower()
    #drop duplicates
    df_prot=df_prot.drop_duplicates(subset=['sampleid'])
    df_prot['rrid_len']=df_prot['sampleid'].apply(len)
    df_prot=df_prot[df_prot['rrid_len'] < 10]
    
    df_prot.rename(columns={'sampleid': 'labid'}, inplace=True)
    df_prot=df_prot[['labid','olinkid']]
    df_prot1=df_prot.merge(df_cchc_sas, how='left',on='labid')
    
    df_prot_baseline=df_prot1.loc[df_prot1.visit==1]
    df_prot_followup=df_prot1.loc[df_prot1.visit!=1]
    
    #------------------------Metabolome -----------------------------------------------
    df_metabolome1=df_metabolome.loc[~df_metabolome.PID.isna()]
    df_metabolome1.rename(columns={'SID': 'labid', 'PID': 'rrid'}, inplace=True)
    df_metabolome2=df_metabolome1.merge(df_cchc_sas, how='left',on=['labid','rrid'])

    df_metabolome_baseline=df_metabolome2.loc[df_metabolome2.visit==1]
    df_metabolome_followup=df_metabolome2.loc[df_metabolome2.visit!=1]
    #------------------------DNA Available -----------------------------------------------
    df_dna_spec=df_cchc[['rrid','labid','edta1','edta2','edta3','edta4']]
    df_dna_spec1=df_dna_spec.loc[df_dna_spec.edta1.isin([1,3]) | 
                                 df_dna_spec.edta2.isin([1,3]) |
                                 df_dna_spec.edta3.isin([1,3]) |
                                 df_dna_spec.edta4.isin([1,3])]
    
    df_dna_spec2=df_dna_spec1.merge(df_cchc_sas, how='left',on='labid')
    
    df_dna_spec_novisit=df_dna_spec2.loc[df_dna_spec2.visit.isna()]
    df_dna_spec_visit=df_dna_spec2.loc[~df_dna_spec2.visit.isna()]
    
    df_dna_spec_base=df_dna_spec_visit.loc[df_dna_spec_visit.visit==1]
    df_dna_spec_follow=df_dna_spec_visit.loc[df_dna_spec_visit.visit!=1]
    
    df_dna_spec_novisit_base=df_dna_spec_novisit.loc[df_dna_spec_novisit.labid.str[:2].isin(['BL','BA','BD'])]
    df_dna_spec_novisit_follow=df_dna_spec_novisit.loc[~df_dna_spec_novisit.labid.isin(df_dna_spec_novisit_base.labid)]
    
    
    #creates dataset to display diabetic numbers on excel sheet
    df_sheet = pd.DataFrame(columns=['Type','Description','Baseline','Followup'])
    df_sheet.loc[len(df_sheet.index)] = ['Cohort','Total Participants recruited','5100','2485'] 
    df_sheet.loc[len(df_sheet.index)] = ['','Pediatrics  (2 year follow up)' ,'449','136'] 
    df_sheet.loc[len(df_sheet.index)] = ['Genotyping','Total CCHC',df_geno.shape[0],''] 
    df_sheet.loc[len(df_sheet.index)] = ['','CCHC Adults',df_geno_adult1.shape[0]-1,'']
    df_sheet.loc[len(df_sheet.index)] = ['','Pediatrics',df_geno_pedi1.shape[0],''] 
    df_sheet.loc[len(df_sheet.index)] = ['','Liver Cancer patients and 1st degree relatives',1,'']
    df_sheet.loc[len(df_sheet.index)] = ['RNA samples','Total samples available',df_rna_spec_base.shape[0]+df_rna_spec_novisit_base.shape[0],df_rna_spec_follow.shape[0]+df_rna_spec_novisit_follow.shape[0]]
    df_sheet.loc[len(df_sheet.index)] = ['Transcriptome RNAseq','Total samples sequenced',df_rna_marcela_ped.shape[0]+df_rna_marcela_adult_base.shape[0]+df_rna_marcela_adult_follo.shape[0],'']
    df_sheet.loc[len(df_sheet.index)] = ['','CCHC Adults',df_rna_marcela_adult_base.shape[0],df_rna_marcela_adult_follo.shape[0]]
    df_sheet.loc[len(df_sheet.index)] = ['','Pediatrics',df_rna_marcela_ped.shape[0],'']
    df_sheet.loc[len(df_sheet.index)] = ['','Liver Cancer patients and relatives','','']
    df_sheet.loc[len(df_sheet.index)] = ['Proteome O-Link','CCHC Adults',df_prot_baseline.shape[0],df_prot_followup.shape[0]]
    df_sheet.loc[len(df_sheet.index)] = ['Metabolome Metabolon (Shipped out)','CCHC Adults',df_metabolome_baseline.shape[0],df_metabolome_followup.shape[0]]
    df_sheet.loc[len(df_sheet.index)] = ['Epi-genetic methylation','DNA Samples available',df_dna_spec_base.shape[0]+df_dna_spec_novisit_base.shape[0],df_dna_spec_follow.shape[0]+df_dna_spec_novisit_follow.shape[0]]
    df_sheet.loc[len(df_sheet.index)] = ['Methylation','Total samples assayed',df_metabolome_baseline.shape[0],df_metabolome_followup.shape[0]]
    
    #output to excel file
    writer = pd.ExcelWriter(r'H:\Data Request\DR. McC\Req_042423(Spec Numbers)\Req_042423(Numbers for Geno,RNAseq,Prot,Meta,Methy)_v4.xlsx', engine='xlsxwriter')

    # Write each dataframe to a different worksheet.

    df_sheet.to_excel(writer, sheet_name='Sheet1')

    # Close the Pandas Excel writer and output the Excel file.
    writer.save()
    
