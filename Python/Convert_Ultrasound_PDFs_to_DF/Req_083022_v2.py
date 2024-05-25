import pytesseract
import os
from tempfile import TemporaryDirectory
from pathlib import Path
from pdf2image import convert_from_path
from PIL import Image
import pandas as pd

# We may need to do some additional downloading and setup...
# Windows needs a PyTesseract Download
# https://github.com/UB-Mannheim/tesseract/wiki/Downloading-Tesseract-OCR-Engine
pytesseract.pytesseract.tesseract_cmd = ("You need to replace with you installation path for pytesseract")

# Windows also needs poppler_exe
path_to_poppler_exe = Path("You need to replace with you installation path for poppler_exe")
	

def extract_pdf(PDF_file):
    """
    Reads in a EKG PDF file and extracts the text information using OCR technology.

    Parameters
    ----------
    PDF_file : file
        EKG PDF file.

    Returns
    -------
    text : string
        Text information from the image file created by OCR data.

    """
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

def extract_values(input_df):
    """
    Reads in semi structured data and grabs key variables from the dataframe extracted by extract_pdf fuction.

    Parameters
    ----------
    input_df : dataframe
        Semi structured dataframe extracted from PDF file.

    Returns
    -------
    output_df: dataframe
        Returns structured dataframe with all of the key variables.

    """
    
    input_df = input_df[input_df['text'].notnull()]
    input_df = input_df[input_df['text']!=" "]
    input_df = input_df.reset_index(drop=True) 
    
    if input_df.shape[0] < 43:
        return pd.DataFrame()


    rrid = input_df.iloc[7,11]
    rrid1 = input_df.iloc[8,11]
    labid = input_df.iloc[11,11]
    ventricular_rate = input_df.iloc[22,11]
    printerval = input_df.iloc[27,11]
    QRSDuration = input_df.iloc[32,11]
    QT_QTC = input_df.iloc[37,11]
    PRTAxis = input_df.iloc[43,11]
      
    data = {'rrid':rrid, 'rrid1':rrid1,'labid':labid,
            'ventricular_rate':ventricular_rate,'printerval':printerval,
            'QRSDuration':QRSDuration,'QT_QTC':QT_QTC,'PRTAxis':PRTAxis}

    output_df = pd.DataFrame.from_records([data])
    return output_df
	
if __name__ == "__main__":

    """ Executes Main. The following code runs the code for the directory 10 YRS.
        Goes into the directory, searches for all PDF files, extracts the information, and
        places it into a dataframe. I need to mess with the indexes in the fuction extract_values
        to maximize quality data output.The final dataframe is outputed into an excel file.
    """
    
    directory1=r'U:\Research\CRU\EKG\EKGs CCHC 5YRS 10YRS 15YRS  DRS  PEDI\10 YRS'
    files = [f for f in os.listdir(directory1) if f.lower().endswith('.pdf') ]
    extract=["10Y0010.pdf"]
    dont_extract=["HD0513.pdf","HD0329 PART1.pdf","HD0334_PART1.pdf","HD0334_PART3.pdf",
                  "HD0334_PART2.pdf","HD0328 PART 2.pdf"]
    output = pd.DataFrame()
    
    for f in files:

        pdf_file=os.path.join(directory1, f)
        
        if f in extract:
            unstructured_df = extract_pdf(pdf_file)
            structured_df = extract_values(unstructured_df)
            output=pd.concat([output,structured_df],sort=True,ignore_index=True)
            
    #output.to_excel("need to change to your excel output path", index = True)