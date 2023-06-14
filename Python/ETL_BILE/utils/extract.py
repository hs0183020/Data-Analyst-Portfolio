import pandas as pd
import logging


# Logger initialization
logging.basicConfig(format='[%(levelname)s]: %(message)s', level=logging.DEBUG)



def extract_data():
    """
    Function that brings in the csv file for BILE 

    """

    ### Perform the request ###
    try:
        df_bile = pd.read_excel(r'\\uthouston.edu\uthsc\SPH\Research\BrownsvilleSD\public\Diabetes_Core\LABORATORY_RAHC\BILE\RAW\2020\Copy of list of bile acid concentrations - 390 subjects 2020-02-14.xlsx',sheet_name='Sheet1')

    except:
        raise Exception('Please check the excel file for BILE information')
    
    df_bile = df_bile.rename(columns={'BILE_TEST_DATE': 'Test_Date'})

    #logging.info(df_bile)
    
    return df_bile
    
