#----------------------------------------------------------------------------
# Created By  : H. Soriano
# Created Date: 08/15/2022
# version ='1.3'
# ---------------------------------------------------------------------------
""" Simple script created to clean up all of the fibroscan excel files."""  
# ---------------------------------------------------------------------------
import pandas as pd
import os


#cleans up data sheet from regular fibroscan file 
def cleanUpFibro(df_excel):
    
    
    df_excel=df_excel.rename(columns={'Root File Name': 'TE Root File Name',
                                      'Lastname': 'rrid',
                                      'Firstname': 'labid',
                                      'Gender': 'TE Gender',
                                      'Birthdate':'TE Birthdate',
                                      'Operator': 'TE Operator',
                                      'Referring Physician':'TE Referring Physician',
                                      'Indication':'TE Indication',
                                      'Exam Date':'TE Exam Date',
                                      'Exam duration (s)':'TE Exam Duration',
                                      'Exam type': 'TE Exam Type',
                                      'Calibration Status':'TE Calibration Status',
                                      'Total measures number': 'TE Total Measures',
                                      'Valid measures number': 'TE Valid Measures',
                                      'E_med. (kPa)': 'TE kPa',
                                      'E_IQR (kPa)': 'TE IQR',
                                      'E_IQR / E_med.': 'TE IQR RATIO',
                                      'CAP_med. (dB/m)': 'TE_CAP_Med',
                                      'CAP_IQR (dB/m)': 'TE_CAP_IQR',
                                      'Height': 'TE Height',
                                      'Unit (Height)': 'TE_Height_Units',
                                      'Weight': 'TE Weight',
                                      'Unit (Weight)': 'TE_Weight_Units',
                                      'O.M.P.L.F':'TE OMPLF',
                                      'Patient Positioning': 'TE Patient Positioning',
                                      'Narrow Intercostal Space': 'TE Narrow Intercostal Space',
                                      'Thick Adipose Paniculus': 'TE Thick Adipose Paniculus',
                                      'Suboptimal Us Signal':'TE Suboptimal Us Signal',
                                      'Major Vascular Structures':'TE Major Vascular Structures',
                                      'Appropriate Fasting Conditions': 'TE Appropriate Fasting Conditions'
                                      })
    df_excel['rrid'] = df_excel['rrid'].str.strip()
    df_excel['rrid'] = df_excel['rrid'].str.upper()
    
    df_excel['labid'] = df_excel['labid'].str.strip()
    df_excel['labid'] = df_excel['labid'].str.upper()
    
    

    return df_excel

#cleans up SWSdata sheet from regular fibroscan file 
def cleanUpFibro_SWS(df_excel):
    
    
    df_excel=df_excel.rename(columns={'Root File Name': 'TE Root File Name',
                                      'Lastname': 'rrid',
                                      'Firstname': 'labid',
                                      'Gender': 'TE Gender',
                                      'Birthdate':'TE Birthdate',
                                      'Operator': 'TE Operator',
                                      'Referring Physician':'TE Referring Physician',
                                      'Indication':'TE Indication',
                                      'Exam Date':'TE Exam Date',
                                      'Exam duration (s)':'TE Exam Duration',
                                      'Exam type': 'TE Exam Type',
                                      'Calibration Status':'TE Calibration Status',
                                      'Total measures number': 'TE Total Measures',
                                      'Valid measures number': 'TE Valid Measures',
                                      'V_med. (m/s)': 'TE_SWS_Med',
                                      'V_IQR (m/s)': 'TE_SWS_IQR',
                                      'CAP_med. (dB/m)': 'TE CAP_Med',
                                      'CAP_IQR (dB/m)': 'TE CAP_IQR',
                                      'Height': 'TE Height',
                                      'Unit (Height)': 'TE_Height_Units',
                                      'Weight': 'TE Weight',
                                      'Unit (Weight)': 'TE_Weight_Units',
                                      'O.M.P.L.F':'TE OMPLF',
                                      'Patient Positioning': 'TE Patient Positioning',
                                      'Narrow Intercostal Space': 'TE Narrow Intercostal Space',
                                      'Thick Adipose Paniculus': 'TE Thick Adipose Paniculus',
                                      'Suboptimal Us Signal':'TE Suboptimal Us Signal',
                                      'Major Vascular Structures':'TE Major Vascular Structures',
                                      'Appropriate Fasting Conditions': 'TE Appropriate Fasting Conditions'
                                      })
    df_excel['rrid'] = df_excel['rrid'].str.strip()
    df_excel['rrid'] = df_excel['rrid'].str.upper()
    
    df_excel['labid'] = df_excel['labid'].str.strip()
    df_excel['labid'] = df_excel['labid'].str.upper()
    
    

    return df_excel

#cleans up data sheet from portable fibroscan file
def cleanUpFibro_Port(df_excel):
    
    
    df_excel['TE Exam Duration'] = [int(a) * 60 + int(b) for a,b in df_excel['Exam Duration (min)'].str.split(',')]
    df_excel['E IQR/Median (%)'] = df_excel['E IQR/Median (%)'] /100
    df_excel['Fasting'] = [True if x == 'Yes' else False for x in df_excel['Fasting']]
    df_excel['Thick Panniculus'] = [True if x == 'Yes' else False for x in df_excel['Thick Panniculus']]
    
    df_excel=df_excel.rename(columns={'Exam file name': 'TE Root File Name',
                                      'Last name': 'rrid',
                                      'First name': 'labid',
                                      'Gender': 'TE Gender',
                                      'Operator': 'TE Operator',
                                      'Exam Type': 'TE Exam Type',
                                      'Total measurement number': 'TE Total Measures',
                                      'Valid measurement number': 'TE Valid Measures',
                                      'E Median (kPa)': 'TE kPa',
                                      'E IQR (kPa)': 'TE IQR',
                                      'E IQR/Median (%)': 'TE IQR RATIO',
                                      'CAP Median (dB/m)': 'TE_CAP_Med',
                                      'CAP IQR (dB/m)': 'TE_CAP_IQR',
                                      'Height': 'TE Height',
                                      'Height Unit': 'TE_Height_Units',
                                      'Weight': 'TE Weight',
                                      'Weight Unit': 'TE_Weight_Units',
                                      'Fasting': 'TE Appropriate Fasting Conditions',
                                      'Thick Panniculus': 'TE Thick Adipose Paniculus',
                                      'Position Finding': 'TE Patient Positioning',
                                      'Intercostal space': 'TE Narrow Intercostal Space',

                                      
                                      })
    df_excel['rrid'] = df_excel['rrid'].str.strip()
    df_excel['rrid'] = df_excel['rrid'].str.upper()
    
    df_excel['labid'] = df_excel['labid'].str.strip()
    df_excel['labid'] = df_excel['labid'].str.upper()
    
    df_excel['TE Birthdate'] = pd.to_datetime(dict(year=df_excel['Birth date (year)'], month=df_excel['Birth date (month)'], day=df_excel['Birth date (day)']))
    df_excel['TE Exam Date'] = pd.to_datetime(dict(year=df_excel['Exam date (year)'], month=df_excel['Exam date (month)'], day=df_excel['Exam date (day)']))
    
    df_excel = df_excel[['TE Root File Name',
                        'rrid',
                        'labid',
                        'Code',
                        'TE Gender',
                        'TE Operator',
                        'TE Exam Type',
                        'TE Total Measures',
                        'TE Valid Measures',
                        'TE kPa',
                        'TE IQR',
                        'TE IQR RATIO',
                        'TE_CAP_Med',
                        'TE_CAP_IQR',
                        'TE Height',
                        'TE_Height_Units',
                        'TE Weight',
                        'TE_Weight_Units',
                        'TE Appropriate Fasting Conditions',
                        'TE Thick Adipose Paniculus',
                        'TE Patient Positioning',
                        'TE Narrow Intercostal Space',
                        'TE Birthdate',
                        'TE Exam Date',
                        'TE Exam Duration']]

    return df_excel

    

if __name__ == "__main__":
    
    #start to Feb 2022 files
    df_excel_FEB = pd.read_excel(r'U:\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\TEMPDATA\ULTRASOUND DATA\FIBROSCAN\ARCHIVE\Fibroscan_upload_032322.xlsx',sheet_name='Data')
    df_excel_FEB_SWS = pd.read_excel(r'U:\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\TEMPDATA\ULTRASOUND DATA\FIBROSCAN\ARCHIVE\Fibroscan_upload_032322.xlsx',sheet_name='SWS Data')

    path = r'U:\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\TEMPDATA\ULTRASOUND DATA\FIBROSCAN\New folder'
    
    folder='March_2022'
    #March 2022 files
    df_excel_port_MAR = pd.read_excel( os.path.join(path,folder,'FIBROSCAN MARCH 2022 PORTABLE.xlsx'),sheet_name='Data')
    
    #April 2022 files
    folder='April_2022'
    df_excel_APR = pd.read_excel( os.path.join(path,folder,'FIBROSCAN APRIL 2022.xlsx'),sheet_name='Data')
    df_excel_APR_SWS = pd.read_excel( os.path.join(path,folder,'FIBROSCAN APRIL 2022.xlsx'),sheet_name='SWS Data')
    df_excel_port_APR = pd.read_excel( os.path.join(path,folder,'FIBROSCAN PORTABLE APRIL 2022.xlsx'),sheet_name='Data')
    
    #May 2022 files
    folder='May_2022'
    df_excel_MAY = pd.read_excel( os.path.join(path,folder,'Fibroscan Excel May 2022.xlsx'),sheet_name='Data')
    df_excel_MAY_SWS = pd.read_excel( os.path.join(path,folder,'Fibroscan Excel May 2022.xlsx'),sheet_name='SWS Data')
    df_excel_port_MAY = pd.read_excel( os.path.join(path,folder,'Fibroscan in Excel MAY 2022.xlsx'),sheet_name='Data')
    
    #June 2022 files
    folder='June_2022'
    df_excel_June1 = pd.read_excel( os.path.join(path,folder,'Excel Fibroscan 06_2022.xlsx'),sheet_name='Data')
    df_excel_June2 = pd.read_excel( os.path.join(path,folder,'Excel Fibroscan June 2022 non portable.xlsx'),sheet_name='Data')
    df_excel_June3 = pd.read_excel( os.path.join(path,folder,'EXCEL FIBROSCANE JUNE 2022 MINI.xlsx'),sheet_name='Data')
    df_excel_June1_SWS = pd.read_excel( os.path.join(path,folder,'Excel Fibroscan 06_2022.xlsx'),sheet_name='SWS Data')
    df_excel_June2_SWS = pd.read_excel( os.path.join(path,folder,'Excel Fibroscan June 2022 non portable.xlsx'),sheet_name='SWS Data')
    df_excel_June3_SWS = pd.read_excel( os.path.join(path,folder,'EXCEL FIBROSCANE JUNE 2022 MINI.xlsx'),sheet_name='SWS Data')
    df_excel_port_June = pd.read_excel( os.path.join(path,folder,'Excel Fibroscan June 2022 Portable.xlsx'),sheet_name='Data')
    
    #July 2022 files
    folder='July_2022'
    df_excel_July1 = pd.read_excel( os.path.join(path,folder,'FIBRO FS430Mini JULY 2022.xlsx'),sheet_name='Data')
    df_excel_July2 = pd.read_excel( os.path.join(path,folder,'FIBROFS502 JULY 2022 PORTABLE.xlsx'),sheet_name='Data')
    df_excel_July1_SWS = pd.read_excel( os.path.join(path,folder,'FIBRO FS430Mini JULY 2022.xlsx'),sheet_name='SWS Data')
    df_excel_July2_SWS = pd.read_excel( os.path.join(path,folder,'FIBROFS502 JULY 2022 PORTABLE.xlsx'),sheet_name='SWS Data')
    df_excel_port_July = pd.read_excel( os.path.join(path,folder,'FIBRO JULY 2022 #530.xlsx'),sheet_name='Data')

    folder='August_2022'
    df_excel_August1 = pd.read_excel( os.path.join(path,folder,'Fibro August 2022 Portable.xlsx'),sheet_name='Data')
    df_excel_August2 = pd.read_excel( os.path.join(path,folder,'Fibroscans in Excel August 2022 Mini.xlsx'),sheet_name='Data')
    df_excel_August1_SWS = pd.read_excel( os.path.join(path,folder,'Fibro August 2022 Portable.xlsx'),sheet_name='SWS Data')
    df_excel_August2_SWS = pd.read_excel( os.path.join(path,folder,'Fibroscans in Excel August 2022 Mini.xlsx'),sheet_name='SWS Data')
    df_excel_port_August = pd.read_excel( os.path.join(path,folder,'Fibro August 2022.xlsx'),sheet_name='Data')

    folder='September_2022'
    df_excel_September1 = pd.read_excel( os.path.join(path,folder,'FS430MiniPlusF92047_20220930_103316.xlsx'),sheet_name='Data')
    df_excel_September2 = pd.read_excel( os.path.join(path,folder,'FS502TouchF62586_20220930_131224.xlsx'),sheet_name='Data')
    df_excel_September1_SWS = pd.read_excel( os.path.join(path,folder,'FS430MiniPlusF92047_20220930_103316.xlsx'),sheet_name='SWS Data')
    df_excel_September2_SWS = pd.read_excel( os.path.join(path,folder,'FS502TouchF62586_20220930_131224.xlsx'),sheet_name='SWS Data')
    df_excel_port_September= pd.read_excel( os.path.join(path,folder,'GF80215_20220930_103409  #530.xlsx'),sheet_name='Data')

    folder='October_2022'
    df_excel_Oct1 = pd.read_excel( os.path.join(path,folder,'FS502TouchF62586_20221103_142254.xlsx'),sheet_name='Data')
    df_excel_Oct1_SWS = pd.read_excel( os.path.join(path,folder,'FS502TouchF62586_20221103_142254.xlsx'),sheet_name='SWS Data')
    df_excel_port_Oct1 = pd.read_excel( os.path.join(path,folder,'GF80215_20221103_155704.xlsx'),sheet_name='Data')
    df_excel_port_Oct2 = pd.read_excel( os.path.join(path,folder,'LF92047_20221104_095625.xlsx'),sheet_name='Data')

    folder='November 2022'
    df_excel_Nov1 = pd.read_excel( os.path.join(path,folder,'Fibroscan Excel November 2022 502.xlsx'),sheet_name='Data')
    df_excel_Nov1_SWS = pd.read_excel( os.path.join(path,folder,'Fibroscan Excel November 2022 502.xlsx'),sheet_name='SWS Data')
    df_excel_port_Nov1 = pd.read_excel( os.path.join(path,folder,'Fibroscan Excel November 2022.xlsx'),sheet_name='Data')
    df_excel_port_Nov2 = pd.read_excel( os.path.join(path,folder,'Fibroscan in Excel November 2022 #530.xlsx'),sheet_name='Data')

    folder='December_2022'
    df_excel_Dec1 = pd.read_excel( os.path.join(path,folder,'Fibroscans in Excel December 2022 #502.xlsx'),sheet_name='Data')
    df_excel_Dec1_SWS = pd.read_excel( os.path.join(path,folder,'Fibroscans in Excel December 2022 #502.xlsx'),sheet_name='SWS Data')
    df_excel_port_Dec1 = pd.read_excel( os.path.join(path,folder,'Fibroscan in Excel December 2022 #530.xlsx'),sheet_name='Data')

    folder='January_2023'
    df_excel_Jan1 = pd.read_excel( os.path.join(path,folder,'Fibroscans in Excel Januaary 2023 #502.xlsx'),sheet_name='Data')
    df_excel_Jan1_SWS = pd.read_excel( os.path.join(path,folder,'Fibroscans in Excel Januaary 2023 #502.xlsx'),sheet_name='SWS Data')
    df_excel_port_Jan1 = pd.read_excel( os.path.join(path,folder,'Fibroscans in Excel January 2023 #530.xlsx'),sheet_name='Data')
    df_excel_port_Jan2 = pd.read_excel( os.path.join(path,folder,'Fibroscans in Excel January 2023 #430 Mini.xlsx'),sheet_name='Data')

    folder='Feburary_2023'
    df_excel_Feb1 = pd.read_excel( os.path.join(path,folder,'Fibroscan in Excel February 2023 # 502.xlsx'),sheet_name='Data')
    df_excel_Feb1_SWS = pd.read_excel( os.path.join(path,folder,'Fibroscan in Excel February 2023 # 502.xlsx'),sheet_name='SWS Data')
    df_excel_port_Feb1 = pd.read_excel( os.path.join(path,folder,'Fibroscans in Excel Feb.2023 #530.xlsx'),sheet_name='Data')
    df_excel_port_Feb2 = pd.read_excel( os.path.join(path,folder,'Fibroscan in Excel February 2023 #430.xlsx'),sheet_name='Data')

    folder='March_2023'
    df_excel_Mar1 = pd.read_excel( os.path.join(path,folder,'Fibrocans March 2023 502.xlsx'),sheet_name='Data')
    df_excel_Mar1_SWS = pd.read_excel( os.path.join(path,folder,'Fibrocans March 2023 502.xlsx'),sheet_name='SWS Data')
    df_excel_port_Mar1 = pd.read_excel( os.path.join(path,folder,'FIBROSCANS MARCH 2023 # 430.xlsx'),sheet_name='Data')
    df_excel_port_Mar2 = pd.read_excel( os.path.join(path,folder,'Fibroscans March 2023 #530.xlsx'),sheet_name='Data')

    folder='April_2023'
    df_excel_Apr1 = pd.read_excel( os.path.join(path,folder,'Fibroscan in Excell April 2023-#502.xlsx'),sheet_name='Data')
    df_excel_Apr1_SWS = pd.read_excel( os.path.join(path,folder,'Fibroscan in Excell April 2023-#502.xlsx'),sheet_name='SWS Data')
    df_excel_port_Apr1 = pd.read_excel( os.path.join(path,folder,'Fibroscans April 2023 # 530.xlsx'),sheet_name='Data')
    df_excel_port_Apr2 = pd.read_excel( os.path.join(path,folder,'Fibroscans April 2023 #430.xlsx'),sheet_name='Data')

    folder='May_2023'
    df_excel_May1 = pd.read_excel( os.path.join(path,folder,'FIBROSCAN MAY2023 #502.xlsx'),sheet_name='Data')
    df_excel_May1_SWS = pd.read_excel( os.path.join(path,folder,'FIBROSCAN MAY2023 #502.xlsx'),sheet_name='SWS Data')
    df_excel_port_May1 = pd.read_excel( os.path.join(path,folder,'Fibroscans in Excel May 2023 #530.xlsx'),sheet_name='Data')
    df_excel_port_May2 = pd.read_excel( os.path.join(path,folder,'Fibroscans in Excel May 2023   #430.xlsx'),sheet_name='Data')

    df_excel=pd.concat([df_excel_APR,df_excel_MAY,df_excel_June1,df_excel_June2,df_excel_June3,df_excel_July1,df_excel_July2,
                        df_excel_August1,df_excel_August2,df_excel_September1,df_excel_September2,df_excel_Oct1,
                        df_excel_Nov1,df_excel_Jan1,df_excel_Jan1,df_excel_Feb1,df_excel_Mar1,df_excel_Apr1,df_excel_May1],sort=True,ignore_index=True)
    df_excel_SWS=pd.concat([df_excel_APR_SWS,df_excel_MAY_SWS,df_excel_June1_SWS,df_excel_June2_SWS,df_excel_June3_SWS,df_excel_July1_SWS,df_excel_July2_SWS,
                            df_excel_August1_SWS,df_excel_August2_SWS,df_excel_September1_SWS,df_excel_September2_SWS,df_excel_Oct1_SWS,
                            df_excel_Nov1_SWS,df_excel_Dec1_SWS,df_excel_Jan1_SWS,df_excel_Feb1_SWS,df_excel_Mar1_SWS,
                            df_excel_Apr1_SWS,df_excel_May1_SWS],sort=True,ignore_index=True)
    df_excel_port=pd.concat([df_excel_port_MAR,df_excel_port_APR,df_excel_port_MAY,df_excel_port_June,df_excel_port_July,
                             df_excel_port_August,df_excel_port_September,df_excel_port_Oct1,df_excel_port_Oct2,
                             df_excel_port_Nov1,df_excel_port_Nov2,df_excel_port_Dec1,df_excel_port_Jan1,df_excel_port_Jan2,
                             df_excel_port_Feb1,df_excel_port_Feb2,df_excel_port_Mar1,df_excel_port_Mar2,df_excel_port_Apr1,
                             df_excel_port_Apr2,df_excel_port_May1,df_excel_port_May2],sort=True,ignore_index=True)
    
        
    df_excel = cleanUpFibro(df_excel)
    df_excel_SWS = cleanUpFibro_SWS(df_excel_SWS)
    df_excel_port= cleanUpFibro_Port(df_excel_port)
    

    df_excel_final=pd.concat([df_excel_FEB,df_excel,df_excel_port],sort=True,ignore_index=True)
    df_excel_SWS_final=pd.concat([df_excel_FEB_SWS,df_excel_SWS],sort=True,ignore_index=True)
    df_dups = df_excel_final[df_excel_final.duplicated(['TE Root File Name'], keep=False)]
    df_dups_SWS = df_excel_SWS_final[df_excel_SWS_final.duplicated(['TE Root File Name'], keep=False)]
    
    df_excel_final=df_excel_final.drop_duplicates(subset=['TE Root File Name'])
    df_excel_SWS_final=df_excel_SWS_final.drop_duplicates(subset=['TE Root File Name'])
    
    # Create a Pandas Excel writer using XlsxWriter as the engine.
    writer = pd.ExcelWriter(r'U:\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\TEMPDATA\ULTRASOUND DATA\FIBROSCAN\New folder\FIBROSCAN_Clean.xlsx', engine='xlsxwriter')
    writer_sws = pd.ExcelWriter(r'U:\Research\BrownsvilleSD\public\Diabetes_Core\MSAccess\TEMPDATA\ULTRASOUND DATA\FIBROSCAN\New folder\Dups.xlsx', engine='xlsxwriter')
    
    df_excel_final.to_excel(writer, sheet_name='data')
    df_excel_SWS_final.to_excel(writer, sheet_name='SWS data')
    
    df_dups.to_excel(writer_sws, sheet_name='data')
    df_dups_SWS.to_excel(writer_sws, sheet_name='SWS data')
    
    writer.save()
    writer_sws.save()