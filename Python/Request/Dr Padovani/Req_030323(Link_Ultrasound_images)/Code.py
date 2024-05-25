import pandas as pd
import datacompy
import pyodbc as pyodbc
import os
import re


def extract(database):
    query = "SELECT rrid,labid FROM IMPACTLAB_A"
    cnxn = pyodbc.connect(database)
    df = pd.read_sql(query, cnxn)
    cnxn.close()
    return df
    

def createMain(df_cchc,df_covid,df_syv,df_drs):
    
    df_covid=df_covid[['rrid','labid']]
    df_syv=df_syv[['rrid','labid']]
    #change column headers to lowercase
    df_drs.columns= df_drs.columns.str.lower()
    #keep rrid and labid from DRS dataset
    df_drs=df_drs[['rrid','labid']]
    df_cchc=pd.concat([df_cchc,df_covid,df_syv,df_drs],sort=True,ignore_index=True)
    
    

    return df_cchc    

def compareMainRetna(df_cchc,df_excel):
    
    df_excel['rrid'] = df_excel['rrid'].str.strip()
    df_excel['rrid'] = df_excel['rrid'].str.upper()
    
    df_excel['labid'] = df_excel['labid'].str.strip()
    df_excel['labid'] = df_excel['labid'].str.upper()
    
    #df_excel=df_excel.loc[df_excel.labid.notnull()]
    
    compare = datacompy.Compare(df_cchc,df_excel,join_columns=['LABID', 'RRID'], 

          
        # Optional, defaults to 'df1'
        df1_name = 'Original',
          
        # Optional, defaults to 'df2'
        df2_name = 'New' 
        )
    #Not in CCHC but in EXCEL
    notInCCHC=compare.df2_unq_rows
    
    return df_cchc,notInCCHC,compare


if __name__ == "__main__":
    #bring in all of the datasets into python
    constr_bro = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\BrownsvilleSD\public\Diabetes_Core\New_CCHC\Server\BD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    constr_har = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\New_CCHC\HRL\CCHC_HD_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;' 
    constr_lar = r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=\\UTHOUSTON.EDU\UTHSC\SPH\Research\CRU\Diabetes_Core\New_CCHC\LD\CCHC_SERVER_DB.ACCDB;Exclusive=1;Pwd=5167;ExtendedAnsiSQL=1;'     
    df_covid=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\Covid\covid.sas7bdat',encoding='latin1')
    df_syv=pd.read_sas(r'\\uthouston.edu\uthsc\SPH\Research\Studies Data\SYV_Covid\syv_covid.sas7bdat',encoding='latin1')
    df_drs=pd.read_sas(r'\\uctnascifs.uthouston.edu\uthsc\sph\research\brownsvillesd\public\Diabetes_Core\MSAccess\TEMPDATA\DRS\crldata.sas7bdat',encoding='latin1')
    
    df_cchc=pd.concat([extract(constr_bro),extract(constr_har),extract(constr_lar)],sort=True,ignore_index=True)
    df_cchc=createMain(df_cchc,df_covid,df_syv,df_drs)


    df_retina_wksheet = pd.read_excel(r'\\uthouston.edu\uthsc\SPH\Users\hsoriano\Data Request\Dr Padovani\Req_030323\2023MASTER_CCHCRetinal OCT worksheet.xlsx',sheet_name='Retina worksheet ORIGINAL (2)')

    df_retina_wksheet.rename(columns={'BD': 'rrid', 'LAB ID': 'labid'}, inplace=True)
    df_retina_wksheet.loc[(df_retina_wksheet['rrid'] == 'BD1470') & (df_retina_wksheet['labid'] == 'UL0072')  ,'labid'] = '10Y0216'
    df_retina_wksheet.loc[(df_retina_wksheet['rrid'] == 'BD2065') & (df_retina_wksheet['labid'] == '5Y1121 / UL0072')  ,'labid'] = '5Y1121'
    df_retina_wksheet.loc[(df_retina_wksheet['rrid'] == 'BD2808') & (df_retina_wksheet['labid'] == 'UL0061')  ,'labid'] = 'BD6808'
    df_retina_wksheet.loc[(df_retina_wksheet['rrid'] == 'BD2811') & (df_retina_wksheet['labid'] == 'UL0062')  ,'labid'] = 'BD6811'
    df_retina_wksheet.loc[(df_retina_wksheet['rrid'] == 'BD2868') & (df_retina_wksheet['labid'] == 'UL0065')  ,'labid'] = 'BA0023'
    df_retina_wksheet.loc[(df_retina_wksheet['rrid'] == 'BD2998') & (df_retina_wksheet['labid'] == 'UL0069')  ,'labid'] = 'BA0098'
    df_retina_wksheet.loc[(df_retina_wksheet['rrid'] == 'BD3088') & (df_retina_wksheet['labid'] == 'UL0056')  ,'labid'] = 'BA0154'
    df_retina_wksheet.loc[(df_retina_wksheet['rrid'] == 'BD3522') & (df_retina_wksheet['labid'] == 'BA0507 / UL0070')  ,'labid'] = 'BA0507'

    
    df_oct = pd.DataFrame(columns=['name', 'path'])
    for root, dirs, files in os.walk(r'\\uthouston.edu\uthsc\SPH\Research\CollaboratorsCRU\Retina Heidelberg Spectralis OCT images', topdown=False):
        for name in dirs:
            df_oct.loc[len(df_oct)] = [name,os.path.join(root, name)]
            
    df_oct1=df_oct.loc[df_oct['name'].str.contains(r'(^BD|HD)',regex=True, case=False)].reset_index(drop=True)
    df_oct2=df_oct.loc[df_oct['name'].str.contains(r'^(?!BD|HD)',regex=True, case=False)]
    
    df_oct1['rrid'] = df_oct1.name.str.split('\s?-|=\s?', expand = True)[0].str.strip()
    df_oct1['labid'] = df_oct1.name.str.split('\s?-|=\s?', expand = True)[1].str.strip()
    
    df_oct1.loc[(df_oct1['name'] == 'BD1455  15Y0131'),'labid'] = '15Y0131'
    df_oct1.loc[(df_oct1['name'] == 'BD1455  15Y0131'),'rrid'] = 'BD1455'
    df_oct1.loc[(df_oct1['name'] == 'BD1745 - 10Y0443'),'rrid'] = 'BD1746'
    df_oct1.loc[(df_oct1['name'] == 'BD2857 - 15Y1348'),'labid'] = '5Y1348'
    df_oct1.loc[(df_oct1['name'] == 'BD1112 - 10Y0243'),'labid'] = '10Y0423'
    df_oct1.loc[(df_oct1['name'] == 'BD2500 - 10Y0520'),'rrid'] = 'BD1500'
    df_oct1.loc[(df_oct1['name'] == 'BD2792 - 15Y1273'),'labid'] = '5Y1273'
    df_oct1.loc[(df_oct1['name'] == 'BD3536 - BA0592'),'rrid'] = 'BD3635'
    df_oct1.loc[(df_oct1['name'] == 'BD3337 - BA0666'),'rrid'] = 'BD3733'
    df_oct1.loc[(df_oct1['name'] == 'BD2760 - 5Y1233'),'rrid'] = 'BD2766'
    df_oct1.loc[(df_oct1['name'] == 'BD1089 - 15Y0071'),'rrid'] = 'BD1098'
    df_oct1.loc[(df_oct1['name'] == 'BD1470 - 10Y0127'),'labid'] = '15Y0127'
    df_oct1.loc[(df_oct1['name'] == 'BD1869 - 15Y1389'),'labid'] = '5Y1389'
    df_oct1.loc[(df_oct1['name'] == 'BD1331 - 15Y0130'),'rrid'] = 'BD1333'
    df_oct1.loc[(df_oct1['name'] == 'BD3517 - UL0076'),'labid'] = 'BA0501'
    df_oct1.loc[(df_oct1['name'] == 'BD0786 - 5Y0120'),'labid'] = '15Y0120'
    df_oct1.loc[(df_oct1['name'] == 'BD1740 - 10Y454'),'labid'] = '10Y0454'

    df_dups= df_oct1[df_oct1.duplicated(['rrid','labid'], keep=False)]
    df_oct2=df_oct1.drop_duplicates(subset=['rrid','labid']).reset_index(drop=True)
    
    df_retina_images = pd.DataFrame(columns=['name', 'path'])
    for root, dirs, files in os.walk(r'\\uthouston.edu\uthsc\SPH\Research\CollaboratorsCRU\Retina Canon images', topdown=False):
        for name in dirs:
            df_retina_images.loc[len(df_retina_images)] = [name,os.path.join(root, name)]
            
    
    
    df_retina_images.drop(df_retina_images[(df_retina_images['name'] == 'Retina Canon Lombart')].index , inplace=True)
    df_retina_images.drop(df_retina_images[(df_retina_images['name'] == 'Zeiss Images')].index , inplace=True)

    df_retina_images1=df_retina_images.drop_duplicates(subset=['name']).reset_index(drop=True)
    df_retina_images1_p1 = df_retina_images1.iloc[:282].reset_index(drop=True)
    df_retina_images1_p2 = df_retina_images1.iloc[282:].reset_index(drop=True)
    
    df_retina_images1_p1['rrid'] = df_retina_images1_p1.name.str.split(' ', expand = True)[0]
    df_retina_images1_p2['rrid'] = df_retina_images1_p2.name.str.split('-', expand = True)[0]
    
    df_retina_images1_p1['labid'] = df_retina_images1_p1.name.str.split(' ', expand = True)[2]
    df_retina_images1_p2['labid'] = df_retina_images1_p2.name.str.split('-', expand = True)[1]
    
    df_retina_images1=pd.concat([df_retina_images1_p1,df_retina_images1_p2],ignore_index=True)
    
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD1244 BD1244  5Y1042'),'labid'] = '5Y1042'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD1153 BD1153-10Y0221'),'labid'] = '10Y0221'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD1161 BD1161-10Y0217'),'labid'] = '10Y0217'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD1161 BD1161-10Y0217'),'labid'] = '10Y0217'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD1444 BD1444-10Y0223'),'labid'] = '10Y0223'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD2016 BD2016-5Y0931'),'labid'] = '5Y0931'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD2981 BD2981-000292'),'labid'] = 'BA0499'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD3088 BD3088-UL0056'),'labid'] = 'UL0056'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD3398 BD3398-BA0400'),'labid'] = 'BA0400'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD3404 BD3404-BA0406'),'labid'] = 'BA0406'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD3410 BD3410-BA0410'),'labid'] = 'BA0410'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD3411 BD3411-BA0411'),'labid'] = 'BA0411'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD3426 BD3426-BA0423'),'labid'] = 'BA0423'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD3434 BD3434-BA0428'),'labid'] = 'BA0428'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD3482 BD3482-000085'),'labid'] = 'BA0469'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD3484 BD3484-000084'),'labid'] = 'BA0472'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD1473 5Y1145'),'labid'] = '5Y1145'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD1318 BD1318 10Y0230'),'labid'] = '10Y0260'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD1873 BD1873 10Y029'),'labid'] = '10Y0329'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD1942 BD1942 10Y0343'),'labid'] = '10Y0345'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD0762-10Y0902'),'labid'] = '10Y0402'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD0359 BD0359 10Y1018'),'labid'] = '5Y1018'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD1470 BD1470 UL0072'),'labid'] = '10Y0216'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD2808 BD2808 UL0061'),'labid'] = 'BD6808'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD2811 BD2811 UL0062'),'labid'] = 'BD6811'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD2868 BD2868 UL0065'),'labid'] = 'BA0023'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD2998 BD2998 UL0069'),'labid'] = 'BA0098'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD3088 BD3088-UL0056'),'labid'] = 'BA0154'
    df_retina_images1.loc[(df_retina_images1['name'] == 'BD0762-10Y0902'),'labid'] = '10Y0402'

    df_retina_images1=df_retina_images1.drop_duplicates(subset=['rrid','labid']).reset_index(drop=True)
    output=compareMainRetna(df_cchc,df_retina_images1)
    
    df_retina_images2 = pd.DataFrame(columns=['name', 'path'])
    for root, dirs, files in os.walk(r'\\uthouston.edu\uthsc\SPH\Research\CollaboratorsCRU\Retina Zeiss images', topdown=False):
        for name in dirs:
            df_retina_images2.loc[len(df_retina_images2)] = [name,os.path.join(root, name)]
            
    df_retina_images2 = pd.DataFrame(columns=['name','path'])
    path=r'\\uthouston.edu\uthsc\SPH\Research\CollaboratorsCRU\Retina Zeiss images'
    for name in os.listdir(path):
        dir1=os.path.join(path, name)
        if os.path.isdir(dir1):
            df_retina_images2.loc[len(df_retina_images2)] =[name,dir1]
            
    df_dups = df_retina_images2[df_retina_images2.duplicated(['name'], keep=False)]
    df_retina_images2['name'] = df_retina_images2['name'].str.upper()
    df_retina_images3=df_retina_images2.drop_duplicates(subset=['name']).reset_index(drop=True)
    df_retina_images3['rrid'] = df_retina_images3.name.str.split(' |_|-', expand = True)[0]
    df_retina_images3['labid'] = df_retina_images3.name.str.split(' |_|-', expand = True)[1]
    
    df_retina_images3.loc[(df_retina_images3['name'] == 'BD28725Y1271'),'labid'] = '5Y1271'
    df_retina_images3.loc[(df_retina_images3['name'] == 'BD27835Y1342'),'labid'] = '5Y1342'
    df_retina_images3.loc[(df_retina_images3['name'] == '10Y0573_1BD276119630403'),'labid'] = '10Y0573'
    df_retina_images3.loc[(df_retina_images3['name'] == 'BD34895Y1521-119871006'),'labid'] = '5Y1521'
    df_retina_images3.loc[(df_retina_images3['name'] == 'BD28725Y1271'),'rrid'] = 'BD2872'
    df_retina_images3.loc[(df_retina_images3['name'] == 'BD27835Y1342'),'rrid'] = 'BD2783'
    df_retina_images3.loc[(df_retina_images3['name'] == '10Y0573_1BD276119630403'),'rrid'] = 'BD2761'
    df_retina_images3.loc[(df_retina_images3['name'] == 'BD34895Y1521-119871006'),'rrid'] = 'BD3489'
    
    df_nolabid=df_retina_images3[df_retina_images3['labid'].isna()]
    df_labid=df_retina_images3[~df_retina_images3['labid'].isna()]
    
    df_nolabid['rrid']=df_nolabid['name'].str.extract(r'(^\w{2}\d{4})', expand=True)
    df_nolabid['name'] = df_nolabid.apply(lambda row : row['name'].replace(str(row['rrid']), ''), axis=1)
    
    df_nolabid['labid']=df_nolabid['name'].str.extract(r'(^(5Y|BA|BD|CV)\d{4})', expand=True)[0]
    df_nolabid1=df_nolabid[df_nolabid['labid'].isna()]
    df_labid1=df_nolabid[~df_nolabid['labid'].isna()]
    
    df_nolabid1['labid']=df_nolabid1['name'].str.extract('^((10Y|15Y|20Y|5YH)\d{4})', expand=False)[0]
    df_nolabid2=df_nolabid1[df_nolabid1['labid'].isna()]
    df_labid2=df_nolabid1[~df_nolabid1['labid'].isna()]
    
    df_retina_images4=pd.concat([df_labid,df_labid1,df_labid2,df_nolabid2],ignore_index=True)
    
    
    df_retina_images4=df_retina_images4.drop_duplicates(subset=['rrid','labid']).reset_index(drop=True)


    df_retina_images4.loc[(df_retina_images4['name'] == 'BD3035 5Y1435'),'labid'] = '5Y1487'
    df_retina_images4.loc[(df_retina_images4['name'] == 'BD2149 10Y0573'),'labid'] = '10Y0537'
    df_retina_images4.loc[(df_retina_images4['name'] == 'BD2500_10Y0520'),'rrid'] = 'BD1500'
    df_retina_images4.loc[(df_retina_images4['name'] == 'BD3603 BA573'),'labid'] = 'BA0573'
    df_retina_images4.loc[(df_retina_images4['name'] == 'BD3337 BA0666'),'rrid'] = 'BD3733'
    df_retina_images4.loc[(df_retina_images4['name'] == 'BD1479 10Y0540'),'labid'] = '10Y0502'
    df_retina_images4.loc[(df_retina_images4['name'] == 'BD3216 5Y1521'),'rrid'] = 'BD3489'
    df_retina_images4.loc[(df_retina_images4['name'] == 'HA0020 5YH0025'),'rrid'] = 'HD0020'
    df_retina_images4.loc[(df_retina_images4['name'] == 'HA0177 5YH0027'),'rrid'] = 'HD0177'
    df_retina_images4.loc[(df_retina_images4['name'] == 'BD0762_10Y0902'),'labid'] = '10Y0402'
    df_retina_images4.loc[(df_retina_images4['name'] == 'BD3745 BA0580'),'labid'] = 'BA0680'
    df_retina_images4.loc[(df_retina_images4['name'] == '5Y1480'),'rrid'] = 'BD3225'
    df_retina_images4.loc[(df_retina_images4['name'] == 'BD3738 BA0721'),'labid'] = 'BA0672'
    df_retina_images4.loc[(df_retina_images4['name'] == 'BD2590  5Y1073'),'labid'] = '5Y1073'
    df_retina_images4.loc[(df_retina_images4['name'] == 'BD2554  5Y1120'),'labid'] = '5Y1120'

    df_dups = df_retina_images4[df_retina_images4.duplicated(['labid'], keep=False)]

    df_retina_images4=df_retina_images4.drop_duplicates(subset=['rrid','labid']).reset_index(drop=True)

    output=compareMainRetna(df_cchc,df_retina_images4)


    df_retina_final=pd.concat([df_retina_images4,df_retina_images1],ignore_index=True)
    df_retina_final1=df_retina_final.drop_duplicates(subset=['rrid','labid']).reset_index(drop=True)

    
    
    output_oct=compareMainRetna(df_cchc,df_oct2)
    
    output_retina=compareMainRetna(df_cchc,df_retina_final1)
    
    output=compareMainRetna(df_cchc,df_retina_wksheet)
    

    
    df_retina_wksheet1=pd.merge(df_retina_wksheet, df_oct2, how="left", on=["rrid","labid"])
    df_retina_wksheet2=pd.merge(df_retina_wksheet1, df_retina_final1, how="left", on=["rrid","labid"])
    #output to excel file
    writer = pd.ExcelWriter(r'U:\Users\hsoriano\Data Request\Dr Padovani\Req_030323\RETINAL and OCT WORKSHEET_withPaths.xlsx', engine='xlsxwriter')


    #df_retina_images1.to_excel(writer, sheet_name='Retina Canon images',index=False)
    df_retina_wksheet2.to_excel(writer, sheet_name='Retina worksheet',index=False)
    #output_oct[1].to_excel(writer, sheet_name='OCT errors',index=False)
    #output_retina[1].to_excel(writer, sheet_name='Retina errors',index=False)

    #worksheet = writer.sheets['Baseline 75<Trig<=150']

    #worksheet.write(0, 0, 'Baseline 75<Trig<=150')
    
   
    # Close the Pandas Excel writer and output the Excel file.
    writer.save()
    