import pytesseract
import os
from tempfile import TemporaryDirectory
from pathlib import Path
from pdf2image import convert_from_path
from PIL import Image
import pandas as pd
import numpy as np


# We may need to do some additional downloading and setup...
# Windows needs a PyTesseract Download
# https://github.com/UB-Mannheim/tesseract/wiki/Downloading-Tesseract-OCR-Engine
pytesseract.pytesseract.tesseract_cmd = (r"C:\Users\hsoriano\AppData\Local\Tesseract-OCR\tesseract.exe")

# Windows also needs poppler_exe
path_to_poppler_exe = Path(r"H:\Python\Req_090122(PDF_to_Pandas)\Lib\Release-22.04.0-0\poppler-22.04.0\Library\bin")
	
# Put our output files in a sane place...
out_directory = r"H:\Python\Req_090122(PDF_to_Pandas)\Output"


def extract_pdf(PDF_file):
    image_file_list=[]
    
    with TemporaryDirectory() as tempdir:
        
        try:
            pdf_pages = convert_from_path(PDF_file, 500, poppler_path=path_to_poppler_exe)
        except: 
            print("Error on " + PDF_file + "\n")
            
        for page_enumeration, page in enumerate(pdf_pages, start=1):
            
            filename = f"{tempdir}\page_{page_enumeration:03}.jpg"

            page.save(filename, "JPEG")
            image_file_list.append(filename)

        for image_file in image_file_list:
            text = pytesseract.image_to_data(Image.open(image_file),lang='eng', output_type='data.frame')
        
            return text

def extract_values(input_df,file):
    
    
    
    input_df = input_df[input_df['text'].notnull()]
    input_df = input_df[input_df['text']!=" "]
    input_df = input_df.reset_index(drop=True) 
    
    if len(input_df) == 0:
        data = {'filename':file}
        output_df = pd.DataFrame.from_records([data])
        return output_df
        
                
    try:
        vent_row = input_df.index[input_df['text'] == "Vent."].to_list()[0]
        vent_row = vent_row + 2
    except:
        vent_row = 0

    try:
        PR_row = input_df.index[input_df['text'] == "PR"].to_list()[0]
        PR_row = PR_row + 2
    except:
        PR_row = 0
    
    try:
        QRS_row = input_df.index[input_df['text'] == "QRS"].to_list()[0]
        QRS_row = QRS_row + 2
    except:
        QRS_row = 0
    
    try:
        QT_QTC_row = input_df.index[input_df['text'].str.upper() == "QT/QTC"].to_list()[0]
        QT_QTC_row = QT_QTC_row + 1
    except:
        QT_QTC_row = 0
    
    if QT_QTC_row == 0:
        try:
            QT_QTC_row = input_df.index[input_df['text'].str.upper() == "QOT/QTC"].to_list()[0]
            QT_QTC_row = QT_QTC_row + 1
        except:
            QT_QTC_row = 0
    
    try:
        PTR_row1 = input_df.index[input_df['text'] == "P-R-T"].to_list()[0]
        PTR_row1 = PTR_row1 + 2
        PTR_row2 = PTR_row1 + 1
        PTR_row3 = PTR_row2 + 1
    except:
        PTR_row1 = 0
        PTR_row2 = 0
        PTR_row3 = 0
        
    ventricular_rate = input_df.iloc[vent_row,11]
    printerval = input_df.iloc[PR_row,11]
    QRSDuration = input_df.iloc[QRS_row,11]
    QT_QTC = input_df.iloc[QT_QTC_row,11]
    PRTAxis1 = input_df.iloc[PTR_row1,11]
    PRTAxis2 = input_df.iloc[PTR_row2,11]
    PRTAxis3 = input_df.iloc[PTR_row3,11]
    
    data = {'ventricular_rate':ventricular_rate,'printerval':printerval,
            'QRSDuration':QRSDuration,'QT_QTC':QT_QTC,'PRTAxis1':PRTAxis1,
            'PRTAxis2':PRTAxis2,'PRTAxis3':PRTAxis3,'filename':file}

    output_df = pd.DataFrame.from_records([data])
    return output_df
	
if __name__ == "__main__":
	# We only want to run this if it's directly executed!
    directory1=r'U:\Research\CRU\EKG\EKGs CCHC 5YRS 10YRS 15YRS  DRS  PEDI\10 YRS'
    df_already_read = pd.read_excel(r'H:\Python\Req_090122(PDF_to_Pandas)\Output\10YR_EKGs.xlsx',sheet_name='Sheet1')
    dont_extract = df_already_read['filename'].tolist()
    dont_extract2=["HD0513.pdf","HD0329 PART1.pdf","HD0334_PART1.pdf","HD0334_PART3.pdf",
                  "HD0334_PART2.pdf","HD0328 PART 2.pdf"]
    dont_extract.extend(dont_extract2)
    files = [f for f in os.listdir(directory1) if f.lower().endswith('.pdf') ]
    extract = ["15Y0015 - BD0246.pdf"]
    
    output = pd.DataFrame()
    
    for f in files:

        pdf_file=os.path.join(directory1, f)
        
        if f not in dont_extract:
            unstructured_df = extract_pdf(pdf_file)
            structured_df = extract_values(unstructured_df,f)
            output=pd.concat([output,structured_df],sort=True,ignore_index=True)
            
    output.to_excel(r'H:\Python\Req_090122(PDF_to_Pandas)\Output\10YR_EKGs_v3.xlsx', index = True)