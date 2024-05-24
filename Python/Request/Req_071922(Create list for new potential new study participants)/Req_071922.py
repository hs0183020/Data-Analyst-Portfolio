import pandas as pd
import datacompy
import numpy as np
from functools import reduce
from datetime import date

def cleanPhonebooks(phone1,phone2,phone3,addB,addH,addL,IDD):
    
    phone1=phone1[['RRID','PHONENUMBER','CONTACTNAME']]
    phone2=phone2[['RRID','PHONENUMBER','CONTACTNAME']]
    phone3=phone3[['RRID','PHONENUMBER','CONTACTNAME']]
    
    phone1['RRID']=phone1['RRID'].str.upper()
    phone2['RRID']=phone2['RRID'].str.upper()
    phone3['RRID']=phone3['RRID'].str.upper()
    
    phonebook=pd.concat([phone1,phone2,phone3],sort=True,ignore_index=True)
    phonebook = phonebook[phonebook['RRID'].notna()]
    phonebook["Phonum"] = phonebook.groupby("RRID").cumcount() + 1
    phonebook1 = phonebook.pivot(index='RRID', columns='Phonum', values=['PHONENUMBER', 'CONTACTNAME'])
    
    addB=addB[['RRID','GIVEN','PATSUR','MATSUR','SPOUSUR','ADDRESS','CITY','ZIP','PPHONE','PHWHOSE','SPHONE','SPHWHOSE']]
    addH=addH[['RRID','GIVEN','PATSUR','MATSUR','SPOUSUR','ADDRESS','CITY','ZIP','PPHONE','PHWHOSE','SPHONE','SPHWHOSE']]
    addL=addL[['RRID','GIVEN','PATSUR','MATSUR','SPOUSUR','ADDRESS','CITY','ZIP','PPHONE','PHWHOSE','SPHONE','SPHWHOSE']]
    
    addB['RRID']=addB['RRID'].str.upper()
    addH['RRID']=addH['RRID'].str.upper()
    addL['RRID']=addL['RRID'].str.upper()
    
    addressbook=pd.concat([addB,addH,addL],sort=True,ignore_index=True)
    addressbook = addressbook[(addressbook['RRID'].notna()) & ~(addressbook['RRID'] == '-1')]
    
    IDD=IDD[['RRID','DOB']]
    now = pd.Timestamp('now')
    IDD['DOB'] = pd.to_datetime(IDD['DOB'], format='%m%d%y') 
    IDD['DOB'] = IDD['DOB'].where(IDD['DOB'] < now, IDD['DOB'] -  np.timedelta64(100, 'Y'))
    IDD['AGE'] = (now - IDD['DOB']).astype('<m8[Y]')
    IDD=IDD[['RRID','AGE']]
    
    data_frames=[addressbook,phonebook1,IDD]
    df_merged = reduce(lambda  left,right: pd.merge(left,right,on=['RRID'],how='left'), data_frames)

    return df_merged

def main(cchc,meds,Deceased):
    #Grab the following variables from the cchc
    cchc_v1=cchc[['RRID','LABID','INTERVIEW_DATE','BMI1','PD_BMI','WEIGHT1','CANCER','CARDIAC_HISTORY','ECHO_CARDIAC_HISTORY']]
    #cchc_v1.columns= cchc_v1.columns.str.lower()
    cchc_v1.loc[cchc_v1['BMI1'].isna() , 'BMI1'] = cchc_v1['PD_BMI']
    del cchc_v1['PD_BMI']
    
    #remove participants that have bmi less than 20
    cchc_v1 = cchc_v1[cchc_v1['BMI1'] >= 20]
    #remove participants that have cancer
    has_cancer = cchc_v1[cchc_v1['CANCER'] == 1]
    cchc_v2 = cchc_v1[~cchc_v1.RRID.isin(has_cancer.RRID)]
    #remove participants that use STATIN medication
    statin_df = meds[meds['CODE'] == 'STATIN']
    cchc_v3 = cchc_v2[~cchc_v2.RRID.isin(statin_df.RRID)]
    
    #not sure about CARDIAC HISTORY
    has_heart = cchc_v2[cchc_v2['CARDIAC_HISTORY'].notna() | cchc_v2['ECHO_CARDIAC_HISTORY'].notna() ]
    
    #calculate percentage change of weight1 by rrid
    cchc_v3['pct_ch'] = 100 * (cchc_v3.groupby('RRID')['WEIGHT1'].apply(pd.Series.pct_change))
    
    #get dataframes of weight loss +/- 3%
    df_percg3 = cchc_v3[cchc_v3['pct_ch'] > 3.00 ]
    df_percl3 = cchc_v3[cchc_v3['pct_ch'] < -3.00]
    
    #remove participants with weight loss +/- 3%
    cchc_v4 = cchc_v3[~cchc_v3.RRID.isin(df_percg3.RRID)]
    cchc_v4 = cchc_v4[~cchc_v4.RRID.isin(df_percl3.RRID)]
    #remove Deceased
    cchc_v4 = cchc_v4[~cchc_v4.RRID.isin(Deceased.RRID)]
    
    df_bmige35 = cchc_v4[cchc_v4['BMI1'] >= 35]
    df_bmig30 = cchc_v4[(cchc_v4['BMI1'] > 30) & (cchc_v4['BMI1'] < 35)]
    #left join to add diabates variable to excel sheet
    #output = pd.merge(Excel_Sheet, cchc_v1, how="left",on=["labid", "rrid"])
    
    return cchc_v3,cchc_v4,df_bmige35,df_bmig30

if __name__ == "__main__":
    #Bring in information from SAS covid and DRS datasets
    cchc=pd.read_sas(r'\\Uthouston.edu\uthsc\SPH\Research\Studies Data\All_CCHC\cchc.sas7bdat',encoding='latin1')
    meds=pd.read_sas(r'\\Uthouston.edu\uthsc\SPH\Research\Studies Data\Medications\participants_medications.sas7bdat',encoding='latin1')
    addB=pd.read_sas(r'\\Uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\TEMPDATA\NEWCCHC\addressbook_a.sas7bdat',encoding='latin1')
    addH=pd.read_sas(r'\\Uthouston.edu\uthsc\sph\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\TEMPDATA\HARLINGEN\ADDRESS-PHONE\cchc_addressbook_a.sas7bdat',encoding='latin1')
    addL=pd.read_sas(r'\\Uthouston.edu\uthsc\sph\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\TEMPDATA\LAREDO\ADDRESS-PHONE\cchc_addressbook_a.sas7bdat',encoding='latin1')
    phoneB=pd.read_sas(r'\\Uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\TEMPDATA\NEWCCHC\phonebook_a.sas7bdat',encoding='latin1')
    phoneH=pd.read_sas(r'\\Uthouston.edu\uthsc\sph\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\TEMPDATA\HARLINGEN\ADDRESS-PHONE\cchc_phonebook_a.sas7bdat',encoding='latin1')
    phoneL=pd.read_sas(r'\\Uthouston.edu\uthsc\sph\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\TEMPDATA\LAREDO\ADDRESS-PHONE\cchc_phonebook_a.sas7bdat',encoding='latin1')
    IDD=pd.read_sas(r'\\Uthouston.edu\uthsc\sph\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\FINAL DATA\BASELINE\DATABASE TABLES SAS\PACKETS\identifiers.sas7bdat',encoding='latin1')
    Deceased = pd.read_excel(r'U:\Research\CRU\Deceased Participants\deceased participants.xlsx',sheet_name='CCHC')
    
    FullAddressBook=cleanPhonebooks(phoneB,phoneH,phoneL,addB,addH,addL,IDD)

    output=main(cchc,meds,Deceased)
    
    output_ge35 = pd.merge(output[2], FullAddressBook, how="left",on=["RRID"])
    output_g30 = pd.merge(output[3], FullAddressBook, how="left",on=["RRID"])
    
    output_ge35_6months  = output_ge35 [(output_ge35['INTERVIEW_DATE'].dt.date > date(2022,1,1)) & (output_ge35['INTERVIEW_DATE'].dt.date <= date(2022,7,27))]
    output_g30_6months = output_g30[(output_g30['INTERVIEW_DATE'].dt.date > date(2022,1,1)) & (output_g30['INTERVIEW_DATE'].dt.date <= date(2022,7,27))]
    
    #output to excel file
    output_ge35.to_excel(r'\\Uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\Data Request\Python\Req_071922\output_ge35v2.xlsx', index = True)
    output_g30.to_excel(r'\\Uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\Data Request\Python\Req_071922\output_g30v2.xlsx', index = True)
    
    output_ge35_6months.to_excel(r'\\Uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\Data Request\Python\Req_071922\output_ge35_6months.xlsx', index = True)
    output_g30_6months.to_excel(r'\\Uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\Data Request\Python\Req_071922\output_g30_6months.xlsx', index = True)
    
