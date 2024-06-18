#   version 
#   
# update 08/10/2023 for HHA cy 2024 letters  
#   
# updated 06/16/2023 for fy 2024 reporting Hospcie, IRF, LTCH, and SNF - Elizabeth Oliver   
#   
# updated 08/11/2022 for hha cy 2023 reporting - Elizabeth Oliver
#    
#Updated 06/09/2022 -- Elizabeth Oliver
#    
#updates from 03/23/2022 version for use in FY2023 reporting 
#for ILS and Hospice    
#    
#updates from 09/13/2021 for use in hospice recon reopening
#changes to hospice related section and control flags
#    
#updated from 0622 version --- filenaming between HHA and others changes and is the most common
# mistake to make when updating code between them. I updated the write.letter function to use a variable
# I then updated the data statement for each QRP to assign the filenaming to that variable    
#
#variable name changes for hospice and snf - to File_Name_ID instead of FACID etc.
#
#added control flags for selectively running QRP settings
#
#fixed - the ATTN: ADMINISTATOR is not working correctly for how I created the inputs
#    
#renamed inputs for each QRP to NOT be same keywords
#also, wrote code within write.letter to change output location to new wd
#also, also, commented out all HHA calls
#the code can now be run in its entirety
#
#updated file names of non-compliant lists and started testing code
#
#updated 05/26/2021 for use in 2022FY non-comply letter for IRF, LTCH, SNF, and Hospice 
#the reasons for failure are updated 
#flu variable added to reasons for failure in IRF and LTCH 
#the FY in the file naming function is updated
#the input references are updated.  
#    
#HHA added - when HHA is run the file naming needs to be changed to reflect CY not FY.
#Change file naming back when running for Hospice, IRF, LTCH, and SNF
#
#there is a statement after wrtie.letter function that I don't think we need. It ran just fine when I commented out
#Hospice, IRF,  SNF, and LTCH are updated.
#
#
#update from 20200630 - fix spelling and typographical errors
#
#update from 20200622A
#problem with HIS in hopsice message was not showing when HIS was NA, fixed on input spreadsheet
#
#
#update from 20200622 nonlettered version
#It was preferered that the contact name and all addresses be uppercase. The toupper() function was used.
#Comments added to clarify what files are needed
#
#update from 20200619 version
#if the point of contact is missing it will need to say "Administrator" (ALL CAPS)
#This was added to all QRPs
#
#UPDATE FROM nondated version
#naming convention for filenames in iqies was incorrect
#
#
#update from 20231004 version
#Applied QA corrections to HHA variable qao_fail and hhcahps_fail messages 
#
#update from 20240614 version
#Applied corrections to SNF,IRF,HOSPICE, and LTCH (CAUTI - ASMT) messages to align with FY2025 
#
#
#SET UP CODE
#
# packages
library(tools)
library(openxlsx)
library(dplyr)
library(stringr)
library(readr)
library(fs)

#    PRIOR TO RUNNING:
#    Make sure that you place a copy of the CMS logo file 
#    -- which the program expects to be CMS.PNG 
#    -- in each of the folders below

basepath <- "C:/Users/hsoriano/Documents/Letters/"

input_path <- paste0(basepath,"Input/")

wd_path <-  paste0(basepath,"Output")
hha_path <- paste0(wd_path,"/HHA")
hosp_path <- paste0(wd_path,"/HOSP")
ltch_path <- paste0(wd_path,"/LTCH")
irf_path <- paste0(wd_path,"/IRF")
snf_path <- paste0(wd_path,"/SNF")

setwd(wd_path)

# set control flags for selectively running QRP settings
# use 1 for running the QRP setting letter generation code
# any other value for NOT running
run_HHA <- 0
run_HOSP <- 0
run_LTCH <- 1
run_IRF <- 1
run_SNF <- 0

# Update with each QRP per FY:
# each QRP needs:
# 1. fname reference where your xlsx of providers who failed should go, this file should contain all needed information
# 2. template reference where the QRP specific latex template should go, template needs to be updated with each FY
# 3. Confirm that all variable names in rename and mutate statements. The variables listed before each = statement should 
#    match the LaTex template and the variables listed after each = should match the Letter Generation List field names.
# 4. Confirm the "reason" statements match the approved wording
# 5. Confirm the "format" needed for filename convention, iqies or casper, for each QRP
# 6. Confirm the file naming convention in f_Name for each QRP call are correct and the FY or CY referenced is correct.
# 7. Update the path statements (above) to your desired folders for output letters. 
#    The CMS logo should be stored here, as well. You will need one logo per folder, unfortunately.
# 8. Update the control flags (also, above) to run the QRP settings desired  
# 
# 
# Each run:
# 1. Reset control flags to selectively run code per QRP setting, if needed
# 2. Verify you folder stricture in the path statements
# 3. Highlight everything and hit run 
# 
#
#
#fname reference should include the internal provider number, ccn, facility name, 
#facility street address (one or two lines), city,
#state, zip code, point of contact first name, and point of contact last name.
#For internal provider number,
#Hopsice, IRF, and LTCH this is intrnl_prvdr_num in the providers data
#SNF this is the fac_id
#For CCN
#Hospice, IRF, and LTCH this in prvdr_num
#SNF this is mcare_id


# FUNCTIONS FOR LETTERS ETC

# dictionaries for naming
first_num <- 290
cat <- list("IRF"="RQset2",
            "LTC"="SQset2",
            "SB"="SQset2",
            "HOSPC"="HQset2",
		        "HHA"="AQset2")
nums <- list("IRF"=paste0("RQ", str_pad(first_num+0, 6, pad="0")),
             "LTC"=paste0("SQ", str_pad(first_num+1, 6, pad="0")),
             "SB"=paste0("SQ", str_pad(first_num+1, 6, pad="0")),
             "HOSPC"=paste0("HQ", str_pad(first_num+2, 6, pad="0")),
		          "HHA"=paste0("AQ", str_pad(first_num+2, 6, pad="0")))

write.letter <- function(template, data, format, location) {
    #set new wd for the output
    setwd(location)
    
    # loop over rows of data
    for (i in seq_len(nrow(data))) {
      # progress bar
      cat(i, "of", nrow(data), "\r")
        
      # get the i-th facility
      facility <- as.vector(data[i, ])
      
      # replace NAs with blanks
      facility[is.na(facility)] <- ""
      
      # copy the template
      new.file <- template
      
      # now loop over each characteristic of the facility
      for (char in names(facility)) {
        # check if characteristic is templated in
        if (any(grepl(paste0("\\$", char, "\\$"), new.file))) {
          old <- facility[char]
          clean.1 <- sub("&", "\\\\\\\\&", old)
          #clean.2 <- sub("#", "\\\\\\\\#", clean.1)
          clean.2 <- gsub("(?<!\\\\)#", "\\\\\\\\#", clean.1, perl=TRUE)
          
          # replace
          new.file <- gsub(paste0("\\$", char, "\\$"), clean.2, 
                           new.file)
        }
      }
      
      # replace bad &s with escaped ones
      new.file <- sub(" &", " \\\\&", new.file)
      
      # save the edited file
      cat(new.file, file="output.tex", sep="\n")
      
      # now convert to PDF
      log <- system("pdflatex output.tex", intern=TRUE)
      
      # run twice
      log <- system("pdflatex output.tex", intern=TRUE)
      
      if (tolower(format) == "iqies") {
        name <- c("1"=facility$setting,
                  "2"=facility$CCN,
                  "3"=facility$f_Name)
        name <- paste(name, collapse="_")
      } else if (tolower(format) == "casper") {
        name <- c("1"=cat[[facility$setting]],
                  "2"=nums[[facility$setting]],
                  "3"=paste(facility$State,
                            facility$setting,
                            facility$Internal.Provider.ID,
                            sep="_"),
                  "4"=facility$f_Name)
        name <- paste(name, collapse="##")
      }
      
      # rename pdf
      file.rename("output.pdf", name)
      
      
    }
}

#I don't think we need this file.remove. Everything works correctly when commented out
#file.remove(list.files("output", full.names=TRUE))

# function for pasting
paste2 <- function(...,sep=", ") {
    L <- list(...)
    L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
    gsub(paste0("(^",sep,"|",sep,"$)"),"",
         gsub(paste0(sep,sep),sep,
              do.call(paste,c(L,list(sep=sep)))))
}




# QRP START 



# HHA
# 
if (run_HHA==1){
cat("Running HHA\n")
#    This is the LaTeX template for the letters
templateHHA <- readLines(paste0(input_path,"hha_CY2024_EO.tex"), warn=FALSE)
#    This is the Excel workbook containing the noncompliant HHAs
fnameHHA <- paste0(input_path,"Failed_APU_CY2024_20230830.xlsx")
dataHHA <- read.xlsx(fnameHHA, sheet = 3, check.names=TRUE) %>% 
    rename(Internal.Provider.ID = macid,
           State = state_cd) %>%
    mutate(setting="HHA",
         f_Name = "CY2024_Non_Compliance_Notification.pdf",
         Zip.Code=str_pad(prmry_zip, 5, pad="0"),
         CCN=str_pad(ccn, 6, pad="0"),
         Name = toupper(fac_name),
         Address1 = toupper(prmry_adr_line_1),
         Address2 = toupper(prmry_adr_line_2),
         City = toupper(city_name),
         QAO=ifelse(qao_fail == 0, NA, 
                    "\\\\{Did not achieve a 90\\\\% threshold on the Quality Assessment Only (QAO) pay-for-reporting 
                         requirement from July 1, 2022-June 30, 2023.\\\\}"),
         HHCAHPS=ifelse(hhcahps_fail == 0, NA,
                      "\\\\{Did not collect monthly HHCAHPS data and submit 
                            data to the HHCAHPS Data Center from April 1, 2022-March 31, 2023.\\\\}")) %>% 
  rowwise() %>% 
  select(setting, f_Name, Zip.Code, CCN, Internal.Provider.ID, Name, Address1, Address2, City, State, QAO, HHCAHPS) %>%
  mutate(reasons=paste(na.omit(c(QAO, HHCAHPS)),
                       collapse="\\\\\\\\"))


write.letter(templateHHA, dataHHA, "iqies", location=hha_path)

# remove auxiliary files
file.remove(list.files(pattern="output\\..*"))
}



# Hospice
# 
if (run_HOSP==1){
cat("Running HOSPC\n")
#    This is the LaTeX template for the letters for Hospice
templateHOSP <- readLines(paste0(input_path,"hospice_FY2025_EO.tex"), warn=FALSE)
#    This is the Excel workbook containing the noncompliant Hospices and all needed variables
fnameHOSP <- paste0(input_path,"PAC-HOSPICE-APU-FY2025-Non-Compliant-List-For-Letters_20240605.xlsx")
dataHOSP <- read.xlsx(fnameHOSP, check.names=TRUE) %>% 
  rename(Internal.Provider.ID=File_Name_ID,
         CCN=ccn,
         Facility.Name..DBA...Doing.Business.As...Name.=fac_name,
         City=city_name,
         State=state_cd,
         Zip.Code=zip_cd,
         Contact.First.Name=contact_first_name,
         Contact.Last.Name=contact_last_name) %>% 
  mutate(setting="HOSPC",
         f_Name = "FY2025_Non_Compliance_Notification.pdf",
         Contact.First.Name=ifelse(Contact.First.Name=="ADMINISTRATOR",
                                   "ATTN:", toupper(Contact.First.Name)),
         Contact.Last.Name=ifelse(Contact.First.Name=="ATTN:",
                                  "ADMINISTRATOR", toupper(Contact.Last.Name)),
         Street.Address=st_adr,
         Zip.Code=str_pad(Zip.Code, 5, pad="0"),
         CCN=str_pad(CCN, 6, pad="0"),
         HIS=ifelse(meet_HIS == "Yes", NA, 
                    "\\\\{Did not achieve a 90\\\\% threshold on the Hospice Item Set (HIS) pay-for-reporting requirement for 
                    CY 2023 (January 1, 2023-December 31, 2023).\\\\}"),
         CAHPS=ifelse(meet_CAHPS == "Yes", 
                      NA,
                      "\\\\{Did not collect and successfully submit sufficient monthly CAHPS \\\\textregistered ~Hospice Survey 
                      data for CY 2023 (January 1, 2023-December 31, 2023).\\\\}")) %>% 
  rowwise() %>% 
  mutate(reasons=paste(na.omit(c(HIS, CAHPS)),
                       collapse="\\\\\\\\"))

write.letter(templateHOSP, dataHOSP, "CASPER", location=hosp_path)

# remove auxiliary files
file.remove(list.files(pattern="output\\..*"))

#the code to write these to zip files is not working. At this time we prefer not to use.
#to_zip <- list.files("output", "*.pdf", full.names=TRUE)
#total <- length(to_zip)
#num <- 70
#for (i in seq(1, total, num)) {
#  end <- min(total, i + num - 1)
#  zip(file.path("output", 
#                paste0("HQset2_", i, ".zip")), 
#      to_zip[i:end], 
#      flags="-1mj")
#}
}


# LTCH
# 
if (run_LTCH == 1){
cat("Running LTCH\n")
#    This is the LaTeX template for the letters for LTCH
templateLTCH <- readLines(paste0(input_path,"ltch_FY2025_EO.tex"), warn=FALSE)
#    This is the Excel workbook containing the noncompliant ltchs and all needed variables
fnameLTCH <- paste0(input_path,"PAC-LTCH-APU-FY2025-Non-Compliant-List-For-Letters_20240613.xlsx")
dataLTCH <- read.xlsx(fnameLTCH, check.names=TRUE) %>%
  rename(Facility.Name..DBA...Doing.Business.As...Name.=fac_name,
         CCN=ccn,
         Internal.Provider.ID=File_Name_ID,
         Street.Address=st_adr,
         City=city_name,
         State=state_cd,
         Zip.Code=zip_cd,
         Contact.First.Name=contact_first_name,
         Contact.Last.Name=contact_last_name,
         Meet.CAUTI.=meet_cauti,
         Meet.CDIFF.=meet_cdiff,
         Meet.CLABSI.=meet_clabsi,
         Meet.HCP.Covid.=meet_hcpcovid,
         Meet.HCP.Flu.=meet_hcpflu,
         Meet.Asmt=meet_asmt) %>%
  mutate(setting="LTCH",
         f_Name = "FY2025_Non_Compliance_Notification.pdf",
         Contact.First.Name=ifelse(Contact.First.Name=="ADMINISTRATOR",
                                   "ATTN:", toupper(Contact.First.Name)),
         Contact.Last.Name=ifelse(Contact.First.Name=="ATTN:",
                                  "ADMINISTRATOR", toupper(Contact.Last.Name)),
         Zip.Code=str_pad(Zip.Code, 5, pad="0"),
         CAUTI=ifelse(Meet.CAUTI. == "Yes", NA, 
                      "\\\\{Did not submit all required months of complete CMIT Measure ID \\\\#00459 National Healthcare Safety Network (NHSN)
                      Catheter-Associated Urinary Tract Infection (CAUTI) Outcome Measure data\\\\}"),
         CDI=ifelse(Meet.CDIFF. == "Yes", NA,
                    "\\\\{Did not submit all required months of complete CMIT Measure ID \\\\#00462 National Healthcare Safety Network (NHSN) 
                    Facility-wide Inpatient Hospital-onset \\\\textit{Clostridium difficile} Infection (CDI) Outcome Measure data\\\\}"),
         CLABSI=ifelse(Meet.CLABSI. == "Yes", NA,
                       "\\\\{Did not submit all required months of complete CMIT Measure ID \\\\#00460 National Healthcare Safety Network (NHSN) 
                       Central Line-Associated Bloodstream Infection (CLABSI) Outcome Measure data\\\\}"),
         HCP=ifelse(Meet.HCP.Flu. == "Yes", NA,
                   "\\\\{Did not submit CMIT Measure ID \\\\#00390 Influenza Vaccination Coverage among Healthcare Personnel data\\\\}"),
         COVID=ifelse(Meet.HCP.Covid. == "Yes", NA,
                    "\\\\{Did not submit all required months of complete COVID-19 Vaccination Coverage among Healthcare Personnel data\\\\}"),
         MEET.Asmt=ifelse(Meet.Asmt == "Yes", NA, 
                          "\\\\{Did not achieve an  80\\\\% threshold on the Long-Term Care Data Set (LCDS) reporting requirement for 
                          CY 2023 (Janurary 1, 2023-December 31, 2023).\\\\}")

  ) %>% 
  rowwise() %>% 
  mutate(reasons=paste(na.omit(c(CAUTI, CDI, CLABSI, HCP, COVID, MEET.Asmt)),
                       collapse="\\\\\\\\"))
  

write.letter(templateLTCH, dataLTCH, "iqIES", location=ltch_path)

# remove auxiliary files
file.remove(list.files(pattern="output\\..*"))
}


# IRF
# 
if (run_IRF==1){
cat("Running IRF\n")
#    This is the LaTeX template for the letters for IRF
templateIRF <- readLines(paste0(input_path,"irf_FY2025_EO.tex"), warn=FALSE)
#    This is the Excel workbook containing the noncompliant irfs and all needed variables
fnameIRF <- paste0(input_path,"PAC-IRF-APU-FY2025-Non-Compliant-List-For-Letters_20240613.xlsx")

dataIRF <- read.xlsx(fnameIRF, check.names=TRUE) %>%
  rename(CCN=ccn,
         Internal.Provider.ID=File_Name_ID,
         Street.Address=st_adr,
         Second.Line.Address=addtnl_st_adr,
         City=city_name,
         State=state_cd,
         Zip.Code=zip_cd,
         Facility.Name..DBA...Doing.Business.As...Name.=fac_name,
         Contact.First.Name=contact_first_name,
         Contact.Last.Name=contact_last_name,
         Meet.CAUTI.=meet_cauti,
         Meet.CDIFF.=meet_cdiff,
         Meet.HCP.Covid.=meet_hcpcovid,
         Meet.HCP.Flu.=meet_hcpflu,
         Meet.Asmt=meet_asmt) %>%
  mutate(setting="IRF",
         f_Name = "FY2025_Non_Compliance_Notification.pdf",
         Contact.First.Name=ifelse(Contact.First.Name=="ADMINISTRATOR",
                                   "ATTN:", toupper(Contact.First.Name)),
         Contact.Last.Name=ifelse(Contact.First.Name=="ATTN:",
                                  "ADMINISTRATOR", toupper(Contact.Last.Name)),
         Zip.Code=str_pad(Zip.Code, 5, pad="0"),
         CCN=str_pad(CCN, 6, pad="0"),
         CAUTI=ifelse(Meet.CAUTI. == "Yes", NA, 
                      "\\\\{Did not submit all required months of complete CMIT Measure ID \\\\#00459 National Healthcare Safety Network (NHSN) 
                      Catheter-Associated Urinary Tract Infection (CAUTI) Outcome Measure data\\\\}"),
         CDI=ifelse(Meet.CDIFF. == "Yes", NA, 
                    "\\\\{Did not submit all required months of complete CMIT Measure ID \\\\#00462 National Healthcare Safety Network (NHSN) 
                    Facility-wide Inpatient Hospital-onset \\\\textit{Clostridium difficile} Infection (CDI) Outcome Measure data\\\\}"),
         HCP=ifelse(Meet.HCP.Flu. == "Yes", NA, 
                        "\\\\{Did not submit CMIT Measure ID \\\\#00390 Influenza Vaccination Coverage among Healthcare Personnel data\\\\}"),
         COVID=ifelse(Meet.HCP.Covid. == "Yes", NA,
                      "\\\\{Did not submit all required months of complete COVID-19 Vaccination Coverage among Healthcare Personnel data\\\\}"),
         MEET.Asmt=ifelse(Meet.Asmt == "Yes", NA, 
                    "\\\\{Did not achieve a 95\\\\% threshold on the IRF Patient Assessment Instrument (IRF-PAI) reporting requirement for 
                    CY 2023 (January 1, 2023-December 31, 2023).\\\\}")
         ) %>%
  rowwise() %>% 
  mutate(reasons=paste(na.omit(c(CAUTI, CDI, HCP,COVID, MEET.Asmt)),
                       collapse="\\\\\\\\"))

write.letter(templateIRF, dataIRF, "iQIES", location=irf_path)

# remove auxiliary files
file.remove(list.files(pattern="output\\..*"))
}


# SNF
# 
if (run_SNF == 1){
cat("Running SNF\n")
#    This is the LaTeX template for the letters for SNF
templateSNF <- readLines(paste0(input_path,"snf_FY2025_EO.tex"), warn=FALSE)
#    This is the Excel workbook containing the noncompliant snfs and all needed variables
fnameSNF <- paste0(input_path,"PAC-SNF-APU-FY2025-Non-Compliant-List-For-Letters_20240614.xlsx")
dataSNF <- read.xlsx(fnameSNF, check.names=TRUE) %>%
  rename(CCN=ccn,
         Internal.Provider.ID=File_Name_ID,
         Street.Address=st_adr,
         City=city_name,
         State=state_cd,
         Zip.Code=zip_cd,
         Facility.Name..DBA...Doing.Business.As...Name.=fac_name,
         Contact.First.Name=contact_first_name,
         Contact.Last.Name=contact_last_name,
         Meet.HCP.Covid.=meet_hcpcovid,
         Meet.HCP.Flu. = meet_hcpflu,
         Meet.Asmt=meet_asmt ) %>%
  mutate(
         setting="SNF",
         f_Name = "FY2025_Non_Compliance_Notification.pdf",
         Contact.First.Name=ifelse(Contact.First.Name=="ADMINISTRATOR",
                                   "ATTN:", toupper(Contact.First.Name)),
         Contact.Last.Name=ifelse(Contact.First.Name=="ATTN:",
                                  "ADMINISTRATOR", toupper(Contact.Last.Name)),
         Zip.Code=str_pad(Zip.Code, 5, pad="0"),
         CCN=str_pad(CCN, 6, pad="0"),
         COVID=ifelse((Meet.HCP.Covid. == "Yes" | Meet.HCP.Covid. == "") , NA,
                      "\\\\{Did not submit all required months of complete COVID-19 Vaccination Coverage among Healthcare Personnel data \\\\}"),
         HCP=ifelse(Meet.HCP.Flu. == "Yes" | Meet.HCP.Flu. == "", NA, 
                    "\\\\{Did not submit CMIT Measure ID \\\\#00390 Influenza Vaccination Coverage among Healthcare Personnel data\\\\}"),
         MEET.Asmt=ifelse(Meet.Asmt == "Yes", NA, 
                          "\\\\{Did not achieve an 80\\\\% threshold on the Minimum Data Set (MDS) 
                          reporting requirement for CY 2023 (January 1, 2023-December 31, 2023).\\\\}")
        ) %>%
  rowwise() %>% 
  mutate(reasons=paste(na.omit(c(COVID, HCP, MEET.Asmt)),
                       collapse="\\\\\\\\"))
  

write.letter(templateSNF, dataSNF, "iQIES", location=snf_path)

# remove auxiliary files
file.remove(list.files(pattern="output\\..*"))
}


