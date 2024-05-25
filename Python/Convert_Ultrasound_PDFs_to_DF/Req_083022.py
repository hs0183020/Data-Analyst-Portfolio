import pytesseract
import os
from tempfile import TemporaryDirectory
from pathlib import Path
from pdf2image import convert_from_path
from PIL import Image


# We may need to do some additional downloading and setup...
# Windows needs a PyTesseract Download
# https://github.com/UB-Mannheim/tesseract/wiki/Downloading-Tesseract-OCR-Engine
pytesseract.pytesseract.tesseract_cmd = (r"C:\Users\hsoriano\AppData\Local\Tesseract-OCR\tesseract.exe")

# Windows also needs poppler_exe
path_to_poppler_exe = Path(r"H:\Python\Req_090122(PDF_to_Pandas)\Lib\Release-22.04.0-0\poppler-22.04.0\Library\bin")
	
# Put our output files in a sane place...
out_directory = r"H:\Python\Req_090122(PDF_to_Pandas)\Output"


def extract_pdf(PDF_file,text_file):
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

        with open(text_file, "a") as output_file:
            
            for image_file in image_file_list:
                text = str(((pytesseract.image_to_string(Image.open(image_file)))))
                text = text.replace("-\n", "")
                output_file.write(text)

	
if __name__ == "__main__":
	# We only want to run this if it's directly executed!
    directory1=r'U:\Research\CRU\EKG\EKGs CCHC 5YRS 10YRS 15YRS  DRS  PEDI\10 YRS'
    files = [f for f in os.listdir(directory1) if f.lower().endswith('.pdf') ]
    dont_extract=["HD0513.pdf","HD0329 PART1.pdf","HD0334_PART1.pdf","HD0334_PART3.pdf",
                  "HD0334_PART2.pdf","HD0328 PART 2.pdf"]
    
    for f in files:
        filename_noext=os.path.splitext(f)[0]
        filename_txt=filename_noext +".txt"
        output_file = os.path.join(out_directory, filename_txt)
        pdf_file=os.path.join(directory1, f)
        
        if f not in dont_extract:
            extract_pdf(pdf_file,output_file)
        
