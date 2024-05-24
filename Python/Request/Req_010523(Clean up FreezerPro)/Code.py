import pandas as pd
import datacompy
#import pyodbc
from functools import reduce




if __name__ == "__main__":
    
    df_FP = pd.read_csv(r'U:\Users\hsoriano\Python\Req_010523(Clean up FreezerPro)\SamplesReport_1674659476_398.csv',
                         keep_default_na=False)
    
    df_FP["Name"] = df_FP["Name"].str.split().str[0]
    
    df_FP_participants=df_FP.drop_duplicates(subset=['Patient ID'])
    df_FP_visits=df_FP.drop_duplicates(subset=['Name'])
    
    df_FP_SampleType=df_FP.drop_duplicates(subset=['Sample Type'])
    
    df_FP_Freezer=df_FP.drop_duplicates(subset=['Freezer'])
    df_FP_Leve1=df_FP.drop_duplicates(subset=['Level1'])
    df_FP_Leve2=df_FP.drop_duplicates(subset=['Level2'])
    df_FP_Leve3=df_FP.drop_duplicates(subset=['Level3'])


    
    writer = pd.ExcelWriter(r'H:\Python\Req_102122(CRL Report)\Labcorp_report.xlsx', engine='xlsxwriter')
    # Write each dataframe to a different worksheet.
    df_crl_trans.to_excel(writer, sheet_name='Sheet1')
    df_crl.to_excel(writer, sheet_name='Sheet2')
    #df_final_labids.to_excel(writer, sheet_name='SYV_Comments')
    # Close the Pandas Excel writer and output the Excel file.
    writer.save()
    writer.close()
