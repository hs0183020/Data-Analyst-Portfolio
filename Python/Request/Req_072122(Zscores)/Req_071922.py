import pandas as pd
import datacompy
import numpy as np
from functools import reduce
import scipy.stats as stats
from scipy.stats import zscore


def main(cchc):
    #Grab the following variables from the cchc
    cchc_v1=cchc[['RRID','LABID','BMI1','PD_BMI','WEIGHT1','HEIGHT','CANCER','CARDIAC_HISTORY','ECHO_CARDIAC_HISTORY']]
    #cchc_v1.columns= cchc_v1.columns.str.lower()
    cchc_v1.loc[cchc_v1['BMI1'].isna() , 'BMI1'] = cchc_v1['PD_BMI']
    del cchc_v1['PD_BMI']
    
    numeric_cols = cchc_v1.select_dtypes(include=[np.number]).columns
    cchc_v1[['BMI1_Z', 'WEIGHT1_Z']] = cchc_v1[['BMI1', 'WEIGHT1']].apply(lambda x: zscore(x,nan_policy='omit'))
    # cchc_v1=cchc_v1[numeric_cols].apply(zscore,nan_policy='omit')
    cchc_v1= cchc_v1.sort_values(by=['BMI1'], ascending=True)
    
    #calculate zscore of BMI1
    # cchc_v1['BMI1_Zscore'] = stats.zscore(cchc_v1['BMI1'],nan_policy='omit')
    # outliers_height=pd.concat([cchc_v1[(cchc_v1['BMI1_Zscore'] < -3.00)],cchc_v1[(cchc_v1['BMI1_Zscore'] >= 3.00)]],sort=True,ignore_index=True)
    # outliers_height= outliers_height.sort_values(by=['BMI1_Zscore'], ascending=True)
    return cchc_v1#,outliers_height

if __name__ == "__main__":
    #Bring in information from SAS covid and DRS datasets
    cchc=pd.read_sas(r'\\Uthouston.edu\uthsc\SPH\Research\Studies Data\All_CCHC\cchc.sas7bdat',encoding='latin1')

    output=main(cchc)
    
    
    

    
    #output to excel file
    #output.to_excel(r'\\Uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\Data Request\Python\Req_071922\output_ge35.xlsx', index = True)


