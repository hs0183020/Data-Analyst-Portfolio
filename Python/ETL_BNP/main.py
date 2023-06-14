from utils.extract import extract_data
from utils.transform import transform_data
from utils.load import load_data
import logging

# Logger initialization
logging.basicConfig(format='[%(levelname)s]: %(message)s', level=logging.DEBUG)

if __name__ == '__main__':

    # The extract process
    logging.info('Starting data extract...')
    df_bnp = extract_data()

    # Validation process
    logging.info('Starting transformation process...')
    df_transformed=transform_data(df_bnp)


    # Loading data process
    logging.info('Starting loading process...')
    load_data(df_transformed[0])