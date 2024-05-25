import pandas as pd
import datacompy
import pyodbc
from functools import reduce




if __name__ == "__main__":
    
    df_crl = pd.read_csv(r'H:\hsoriano\Python\Req_102122(CRL Report)\CRL_Report_221017021631723.csv',
                         keep_default_na=False)
    df_crl1 = df_crl[df_crl['Result Numeric'] != '']
    df_comments = df_crl[df_crl['Result Numeric'] == '']
    
    df_crl1.loc[df_crl1['Result Comments'].str.contains('No patient age', case=False), 'Result Comments'] = 'No patient age and/or gender provided or N placed in gender box'

    df_crl=df_crl[['Analyte #','Analyte Name','First Name','Reported Date','Result Numeric']]
    
    df_crl.loc[(df_crl['Analyte #'] == 1032), "cchc_var"] = "FBG3"
    df_crl.loc[(df_crl['Analyte #'] == 1040), "cchc_var"] = "BUN"
    df_crl.loc[(df_crl['Analyte #'] == 1370), "cchc_var"] = "CREA"
    df_crl.loc[(df_crl['Analyte #'] == 1198), "cchc_var"] = "SOD"
    df_crl.loc[(df_crl['Analyte #'] == 1180), "cchc_var"] = "POT"
    df_crl.loc[(df_crl['Analyte #'] == 1206), "cchc_var"] = "CHL"
    df_crl.loc[(df_crl['Analyte #'] == 1578), "cchc_var"] = "CO2"
    df_crl.loc[(df_crl['Analyte #'] == 1123), "cchc_var"] = "GOT"
    df_crl.loc[(df_crl['Analyte #'] == 1545), "cchc_var"] = "GPT"
    df_crl.loc[(df_crl['Analyte #'] == 1107), "cchc_var"] = "ALK"
    df_crl.loc[(df_crl['Analyte #'] == 1099), "cchc_var"] = "TBIL"
    df_crl.loc[(df_crl['Analyte #'] == 1073), "cchc_var"] = "TP"
    df_crl.loc[(df_crl['Analyte #'] == 1081), "cchc_var"] = "LALB"
    df_crl.loc[(df_crl['Analyte #'] == 1016), "cchc_var"] = "CALC"
    df_crl.loc[(df_crl['Analyte #'] == 100779), "cchc_var"] = "GFR"
    df_crl.loc[(df_crl['Analyte #'] == 1172), "cchc_var"] = "TRIG"
    df_crl.loc[(df_crl['Analyte #'] == 1065), "cchc_var"] = "CHOL1"
    df_crl.loc[(df_crl['Analyte #'] == 11817), "cchc_var"] = "HDLC"
    df_crl.loc[(df_crl['Analyte #'] == 11919), "cchc_var"] = "LDLCALC"
    df_crl.loc[(df_crl['Analyte #'] == 100065), "cchc_var"] = "CHLR"
    
    
    df_crl.loc[(df_crl['Analyte #'] == 1481), "cchc_var"] = "GHB"
    df_crl.loc[(df_crl['Analyte #'] == 1472), "cchc_var"] = "MCGLUC"
    df_crl.loc[(df_crl['Analyte #'] == 5025), "cchc_var"] = "WBC"
    df_crl.loc[(df_crl['Analyte #'] == 5033), "cchc_var"] = "RBC"
    df_crl.loc[(df_crl['Analyte #'] == 5041), "cchc_var"] = "HGB"
    df_crl.loc[(df_crl['Analyte #'] == 5058), "cchc_var"] = "HCT"
    df_crl.loc[(df_crl['Analyte #'] == 15065), "cchc_var"] = "MCV"
    df_crl.loc[(df_crl['Analyte #'] == 15073), "cchc_var"] = "MCH"
    df_crl.loc[(df_crl['Analyte #'] == 15081), "cchc_var"] = "MCHC"
    df_crl.loc[(df_crl['Analyte #'] == 15172), "cchc_var"] = "PLT"
    df_crl.loc[(df_crl['Analyte #'] == 105007), "cchc_var"] = "RDW"
    #missing mpv
    df_crl.loc[(df_crl['Analyte #'] == 15107), "cchc_var"] = "NEUT_NO"
    df_crl.loc[(df_crl['Analyte #'] == 15123), "cchc_var"] = "LY_NO"
    df_crl.loc[(df_crl['Analyte #'] == 15131), "cchc_var"] = "MO_NO"
    df_crl.loc[(df_crl['Analyte #'] == 15149), "cchc_var"] = "EOS_NO"
    df_crl.loc[(df_crl['Analyte #'] == 15156), "cchc_var"] = "BASO_NO"
    df_crl.loc[(df_crl['Analyte #'] == 15108), "cchc_var"] = "GR_NO"
    
    
    df_crl.loc[(df_crl['Analyte #'] == 15909), "cchc_var"] = "NEUT_ABS_NO"
    df_crl.loc[(df_crl['Analyte #'] == 15917), "cchc_var"] = "LY_ABS_NO"
    df_crl.loc[(df_crl['Analyte #'] == 15933), "cchc_var"] = "EOS_ABS_NO"
    df_crl.loc[(df_crl['Analyte #'] == 15925), "cchc_var"] = "MO_ABS_NO"
    df_crl.loc[(df_crl['Analyte #'] == 15941), "cchc_var"] = "BASO_ABS_NO"
    df_crl.loc[(df_crl['Analyte #'] == 15911), "cchc_var"] = "GR_ABS_NO"
    df_crl.loc[(df_crl['Analyte #'] == 11577), "cchc_var"] = "BUN_CREA_RA"
    df_crl.loc[(df_crl['Analyte #'] == 12039), "cchc_var"] = "GLOB"
    df_crl.loc[(df_crl['Analyte #'] == 12047), "cchc_var"] = "AG_RA"
    df_crl.loc[(df_crl['Analyte #'] == 12059), "cchc_var"] = "NIH"
    
    df_crl=df_crl[df_crl['cchc_var'].notnull()]
    
    df_crl.sort_values(by=['First Name'])
    
    df_crl_trans=df_crl.pivot_table(index='First Name', columns='cchc_var', values='Result Numeric')
    
    

    
    writer = pd.ExcelWriter(r'H:\Python\Req_102122(CRL Report)\Labcorp_report.xlsx', engine='xlsxwriter')
    # Write each dataframe to a different worksheet.
    df_crl_trans.to_excel(writer, sheet_name='Sheet1')
    df_crl.to_excel(writer, sheet_name='Sheet2')
    #df_final_labids.to_excel(writer, sheet_name='SYV_Comments')
    # Close the Pandas Excel writer and output the Excel file.
    writer.save()
    writer.close()
