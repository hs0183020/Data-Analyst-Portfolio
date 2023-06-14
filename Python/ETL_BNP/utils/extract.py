import pandas as pd
import logging


# Logger initialization
logging.basicConfig(format='[%(levelname)s]: %(message)s', level=logging.DEBUG)



def extract_data():
    """
    Function that brings in the sas file for BNP 
    """

    ### Perform the request ###
    try:
        df=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\LABORATORY_RAHC\BNP\FINAL DATA\COMBINED\bnp_all.sas7bdat',encoding='latin1')
    except:
        raise Exception('Please check the excel file for BNP information')
    

    #logging.info(df_bile)
    
    return df
    
