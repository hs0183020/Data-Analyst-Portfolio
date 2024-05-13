# 
#updated for HHA CY2024 letters
#   modified Failed_QAO grepl string value from "only" to "Only" 20231004
# 
#updated 20230619 for Hospice, IRF, LTCH, and SNF FY2024 reporting
#   for SNF added HCP FLU to reasons for failures checks
#   also for SNF changed filenaming convention checks to iqies setting style
#   
#
#updated 220812 for HHA CY2023 reporting 
#   
#updated 220609 for use in FY2023 reporting   
#   
#   
#updated 20210630
#updated 20210907
#   file name change 20220125
#   updated program to include references for ALL PAC QRP settings
#   
#   
# The purpose of this program is to read information from the non-compliance 
# letters in pdf version and create a data.frame
# This data.frame will be used to quality check the letters generated.
# 
# To update this program:
#   1. set year_ref used in file naming --- match the CY or FY of letters
#   2. set control flags to 1 for all settings you need to run
#   3. set WD for output --- should be same for all settings
#   4. set input path for each QRP you need to run 
#      -- where your input PDFs are stored - they should be different for each setting
#   5. if new measures have been added to any one setting, the data step for finding reasons for failure
#      will need to be updated, as well as any select statements downstream for that setting
#   6. other updates to cleaning steps may be necessary if structure of the letter changes
#      they may throw an error, or may not throw an error just give junk output, so please be watchful
# 
# 
# To run this program:
#   1. verify that you set control flags to 1 for all settings you need to run
#   2. verify all updates needed (listed above) have been made
#   3. select all and hit run
# 
# 
# 
# 

# start active code

    # packages needed
    library(pdftools)
    library(tools)
    library(openxlsx)
    library(dplyr)
    library(stringr)
    library(readr)
    library(tidytext)
    library(rlist)
    
 
    today <- Sys.Date()
    format(today, format="%Y%m%d")
    
    #year reference for output file naming --- should match CY or FY of letters
    year_ref <- "2024"
    
   
    # control flags
    # each PAC QRP has different calls and setup work
    # when run_[QRP] == 1 the program will run for that setting
    # set to 0 when you do not wish to run for that setting
    run_HHA <- 1
    run_Hospice <- 0
    run_IRF <- 0
    run_LTCH <- 0
    run_SNF <- 0
    
    # working directory will be location of output - can be same for all setting
    setwd("C:/Users/hsoriano/Documents/Letters/output")
    

    # path of pdf letters - each needs the / at end
    # each PAC QRP needs a different path
    hha_path <- "C:/Users/hsoriano/Documents/Letters/output/HHA/"
    hospice_path <- "C:/Users/hsoriano/Documents/Letters/output/HOSP/"
    irf_path <- "C:/Users/hsoriano/Documents/Letters/output/IRF/"
    ltch_path <- "C:/Users/hsoriano/Documents/Letters/output/LTCH/"
    snf_path <- "C:/Users/hsoriano/Documents/Letters/output/SNF/"

    

##### shared functions --- no updates needed

    # function to read letters into single variable 
    # also reads file name into a variable
    read_letters <- function(filenames){
        
        
        document_text <- pdf_text(paste(in_path, 
                                        filenames, sep = "")) 
        file_name_raw <- gsub(x=filenames, pattern=".pdf", replacement="")
        letters_tmp <- cbind(document_text, file_name_raw)
    }
    
    # function to read letters into strings
    read_strings <- function(files){
        
        
        letters_2_raw <- pdf_text(paste(in_path, 
                                        files, sep = "")) 
        page_1 <- letters_2_raw[1]
        
        strings_raw <- page_1 %>%
            strsplit("[\r\n]+")
        strings_transpose <- as.data.frame(t(as.data.frame(strings_raw, stringsAsFactors = FALSE)), stringsAsFactors = FALSE)
        strings_tmp <- strings_transpose %>%
            select(V7, V8, V9, V10, V12, V14, V15)
        
    }
   
    
    # function for all shared code actions 
    # this utilizes the functions above to read the pdf letters multiple times 
    # and create the core data sets
    # it also renames those core data sets with a setting tag
    make_data <- function(setting, in_path){
      
        assign("setting", setting, envir = .GlobalEnv)
        assign("in_path", in_path, envir = .GlobalEnv)
      
        # create a vector with the file names
        file_vector <- list.files(path=in_path, pattern=".pdf")
    
        # read letters into variables
        # keep page 1 only
        # read file name into variable
        letters_raw <- mapply(file_vector, FUN=read_letters)
        letter_frame <- as.data.frame(letters_raw, stringsAsFactors = FALSE)
        letter_transpose <- as.data.frame(t(letter_frame), stringsAsFactors = FALSE)
        letters_reduced <- letter_transpose %>%
            select (V1, V3) %>%
            rename(file_name = V3)

        # separate letters into strings
        # keep only the lines of interest
        # V8-V10 are the address lines
        # V12 is the Re line that contains the ccn
        # V14 and V15 are the line in body that contains fac name and ccn
        strings_raw <- mapply(file_vector, FUN=read_strings)
        strings_frame <- as.data.frame(strings_raw, stringsAsFactors = FALSE)
        strings_reduced <- as.data.frame(t(strings_frame), stringsAsFactors = FALSE)
    
    
       
        # rename out data sets with tag for setting
        # and assign them for use outside the function
        assign(paste(setting,"letters_reduced", sep = "", collapse =NULL), letters_reduced, envir = .GlobalEnv)
        assign(paste(setting,"strings_reduced", sep = "", collapse =NULL), strings_reduced, envir = .GlobalEnv)

        
    }
    
    # functions to split filename into useable variables
    # two needed - one for casper and one for iqies structures
    # Hospice and SNF use casper
    # HHA IRF and LTCH use iqies
    # 
    # for iqies
    # clean up the filename and transform into usable variables
    file_name_iqies <- function(clean_setting, clean_data){
    # extract setting, ccn, and FY referenced
    # split by underscore
    iqies_file_name_split <- strsplit(clean_data$file_name, "_", fixed = TRUE)
    iqies_file_name_data <- data.frame(iqies_file_name_split)
    iqies_file_name_transpose <- as.data.frame(t(iqies_file_name_data))
    
    iqies_file_name_vars <- iqies_file_name_transpose %>%
      rename(file_name_set = V1,
             file_name_ID = V2) %>%
      mutate(file_name_Year = substr(V3, 3, 6)) %>%
      mutate(file_name_ref = substr(V3, 1, 2)) %>%
      select(file_name_set, file_name_ID, file_name_ref, file_name_Year)
    
    # rename out data sets with tag for setting
    # and assign them for use outside the function
    assign(paste(clean_setting,"file_name_vars", sep = "_", collapse =NULL), iqies_file_name_vars, envir = .GlobalEnv)
    
    }
    
    # for casper
    # clean up the filename and transform into usable variables
    file_name_casper <- function(casper_setting, casper_data){
      # extract setting, ccn, and FY referenced
      # split by underscore
      casper_file_name_split <- strsplit(casper_data$file_name, "[##]|[_]", perl=TRUE, fixed = FALSE)
      casper_file_name_data <- data.frame(casper_file_name_split)
      casper_file_name_transpose <- as.data.frame(t(casper_file_name_data))
      
      casper_file_name_vars <- casper_file_name_transpose %>%
        rename(file_name_set = V1,
               file_name_count = V3,
               file_name_state = V5,
               file_name_setting = V6,
               file_name_ID = V7) %>%
        mutate(file_name_Year = substr(V9, 3, 6),
               file_name_ref = substr(V9, 1, 2)) %>%
        select(file_name_set, file_name_count, file_name_state, file_name_setting, file_name_ID, file_name_ref, file_name_Year)
      
      # rename out data sets with tag for setting
      # and assign them for use outside the function
      assign(paste(casper_setting,"file_name_vars", sep = "_", collapse =NULL), casper_file_name_vars, envir = .GlobalEnv)
      
    }
   
    # function to selectively clear environment
    # the goal of this function is to allow more than one setting to run at the same time
    # while minimizing the risk in using similar data set names 
    # clearing a few key parameters should serve this aim well
    clear_parameters <- function(want_clear){
        if(want_clear == 1){
          if(exists("settings", mode= "any", envir=.GlobalEnv)){
            rm(setting, envir = .GlobalEnv)
          } 
          if(exists("in_path", mode= "any", envir=.GlobalEnv)){
            rm(in_path, envir = .GlobalEnv)
          }
        }
       
    }
     
    
##### START QRP SPECIFIC SET UP  
##### each QRP specific set up needs the following
##### a call to function clear parameters
##### a call to function make data
##### NEEDED cleaning steps for the following
#####       1. finding reasons for failure from page 1 variable
#####       2. cleaning up demographic info from strings variables
##### combining all data and ordering variables
##### final output naming structure
##### final output write to XLSX
    
    
    
# for HHA only
# check for control flag settings for [QRP]
if(run_HHA == 1){
    # call function to clear any existing parameters
    clear_parameters(want_clear=1)
    
    # call function to make data
    # key output of this function are
    # setting should be the QRP setting short name
    # [QRP]letters_reduced
    # [QRP]strings_reduced
    # [QRP]file_name_vars
    make_data(setting="HHA", in_path= hha_path)

    #find reasons for failure from V1
    hha_letters_reasons <- HHAletters_reduced %>%
        mutate(Failed_QAO = grepl("threshold on the Quality Assessment Only (QAO) pay-for-reporting", V1, 
                                ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        mutate(Failed_HHCAHPS = grepl("Did not collect monthly HHCAHPS data and submit", V1, 
                                ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        select(file_name, V1, Failed_QAO, Failed_HHCAHPS)
   
    
    #combine the data sets and reduce
    hha_letters_full <- cbind(hha_letters_reasons, HHAstrings_reduced)
    
    
    # clean up the data set and transform into usable variables
    # pull ccn from V12 line (reference line)
        # note ccn for HHA is assumed to have no alpha-numeric values
        # code will need to change if this suddenly changes
    # pull separate name and ccn from V14 line (body of letter)
    #   # I found that sometimes ccn is in V15... 
    # split V10 into separate city, state, and zip (address line)
    # rename V7 is the contact name found in the address line
    # rename V8 is the facility name found in address line
    # rename v9 is the street address in address line
    # keep only variables of interest 
    hha_letters_clean1 <- hha_letters_full %>%
        mutate(CCN_re_Line = regmatches(V12, regexpr("\\d{6}", V12, ignore.case = TRUE), invert = FALSE)) %>%
        mutate(V14_V15_Smush = paste(V14, V15, sep=" "))%>%
        mutate(Smush_Name_CCN = gsub("This Letter is to officially notify you that ", "", V14_V15_Smush, ignore.case = TRUE, perl = FALSE)) %>%
        mutate(Name_Body = sub("\\;.*", "", Smush_Name_CCN)) %>%
        mutate(CCN_Body = regmatches(Smush_Name_CCN, regexpr("\\d{6}", Smush_Name_CCN, ignore.case = TRUE), invert = FALSE)) %>%
        mutate(City = gsub(", \\D{2} \\d{5}", "", V10 , ignore.case = TRUE, perl = TRUE)) %>%
        mutate(State_Zip =  sub(".*,", "", V10)) %>%
        mutate(State = regmatches(State_Zip, regexpr("\\b[A-Z]{2}\\b", State_Zip, ignore.case = TRUE), invert = FALSE)) %>%
        mutate(Zip = regmatches(State_Zip, regexpr("\\d{5}", State_Zip, ignore.case = TRUE), invert = FALSE)) %>%
        rename(Contact_Full_Name = V7,
               Name_Adr_Line = V8,
               Str_Adr = V9,
               )%>%
        select(file_name, V1, Contact_Full_Name, Name_Adr_Line, Str_Adr, City, State, Zip, CCN_re_Line, Name_Body, CCN_Body, Failed_QAO, Failed_HHCAHPS)
    
    # call function to clean file name into useable variables
    # HHA uses iqies function
    file_name_iqies(clean_setting="hha", clean_data=hha_letters_clean1)
    
    #combine all variables
    hha_final <- cbind(hha_letters_clean1, hha_file_name_vars) %>%
        select(file_name, file_name_set, file_name_ID, file_name_ref, file_name_Year,
               Contact_Full_Name,Name_Adr_Line, Str_Adr, City, State, Zip, CCN_re_Line, 
               Name_Body, CCN_Body, Failed_QAO, Failed_HHCAHPS)
    
    
    # XLSX output naming 
    HHA_out_name <- paste("QA_HHA_CY", year_ref, "_", today, ".xlsx", sep= "", collapse = NULL)
    
    #output into an excel file
    write.xlsx(hha_final, HHA_out_name, append = FALSE)

#close HHA section
}   
   
    
    
    # for Hospice only
    # check for control flag settings for [QRP]
    if(run_Hospice == 1){
      # call function to clear any existing parameters
      clear_parameters(want_clear=1)
      
      # call function to make data
      # key output of this function are
      # setting should be the QRP setting short name
      # [QRP]letters_reduced
      # [QRP]strings_reduced
      # [QRP]file_name_vars
      make_data(setting="HOSP", in_path= hospice_path)
      
      #find reasons for failure from V1
      hosp_letters_reasons <- HOSPletters_reduced %>%
        mutate(Failed_HIS = grepl("threshold on the Hospice Item Set (HIS)", V1, 
                                  ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        mutate(Failed_CAHPS = grepl("Did not collect and successfully submit sufficient monthly CAHPS", V1, 
                                      ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        select(file_name, V1, Failed_HIS, Failed_CAHPS)
      
      
      #combine the data sets and reduce
      hosp_letters_full <- cbind(hosp_letters_reasons, HOSPstrings_reduced)
      
      
      # clean up the data set and transform into usable variables
      # pull ccn from V12 line (reference line)
      # note ccn for HHA is assumed to have no alpha-numeric values
      # code will need to change if this suddenly changes
      # pull separate name and ccn from V14 line (body of letter)
      #   # I found that sometimes ccn is in V15... 
      # split V10 into separate city, state, and zip (address line)
      # rename V7 is the contact name found in the address line
      # rename V8 is the facility name found in address line
      # rename v9 is the street address in address line
      # keep only variables of interest
      hosp_letters_clean1 <- hosp_letters_full %>%
        mutate(CCN_re_Line = regmatches(V12, regexpr("[A-F|0-9]{1}\\d{5}", V12, ignore.case = TRUE), invert = FALSE)) %>%
        mutate(V14_V15_Smush = paste(V14, V15, sep=" "))%>%
        mutate(Smush_Name_CCN = gsub("This Letter is to officially notify you that ", "", V14_V15_Smush, ignore.case = TRUE, perl = FALSE)) %>%
        mutate(Name_Body = sub("\\;.*", "", Smush_Name_CCN)) %>%
        mutate(CCN_Body = regmatches(Smush_Name_CCN, regexpr("[A-F|0-9]{1}\\d{5}", Smush_Name_CCN, ignore.case = TRUE), invert = FALSE)) %>%
        mutate(City = gsub(", \\D{2} \\d{5}", "", V10 , ignore.case = TRUE, perl = TRUE)) %>%
        mutate(State_Zip =  sub(".*,", "", V10)) %>%
        mutate(State = regmatches(State_Zip, regexpr("\\b[A-Z]{2}\\b", State_Zip, ignore.case = TRUE), invert = FALSE)) %>%
        mutate(Zip = regmatches(State_Zip, regexpr("\\d{5}", State_Zip, ignore.case = TRUE), invert = FALSE)) %>%
        rename(Contact_Full_Name = V7,
               Name_Adr_Line = V8,
               Str_Adr = V9,
        )%>%
        select(file_name, V1, Contact_Full_Name, Name_Adr_Line, Str_Adr, City, State, Zip, CCN_re_Line, Name_Body, CCN_Body, Failed_HIS, Failed_CAHPS)
      
      
      # clean up the filename and transform into usable variables
      file_name_casper(casper_setting="hosp", casper_data=hosp_letters_clean1)
      
      #combine all variables
      hosp_final <- cbind(hosp_letters_clean1, hosp_file_name_vars) %>%
        select(file_name, file_name_set, file_name_count, file_name_state, 
               file_name_setting, file_name_ID, file_name_ref, file_name_Year,
               Contact_Full_Name, Name_Adr_Line, Str_Adr, City, State, Zip, CCN_re_Line, 
               Name_Body, CCN_Body, Failed_HIS, Failed_CAHPS)
      
      
      # XLSX output naming 
      HOSP_out_name <- paste("QA_Hospice_FY", year_ref, "_", today, ".xlsx", sep= "", collapse = NULL)
      
      #output into an excel file
      write.xlsx(hosp_final, HOSP_out_name, append = FALSE)
      
      #close Hospice section
    }   
    
    # for IRF only
    # check for control flag settings for [QRP]
    if(run_IRF == 1){
      # call function to clear any existing parameters
      clear_parameters(want_clear=1)
      
      # call function to make data
      # key output of this function are
      # setting should be the QRP setting short name
      # [QRP]letters_reduced
      # [QRP]strings_reduced
      # [QRP]file_name_vars
      make_data(setting="IRF", in_path= irf_path)
      
      #find reasons for failure from V1
      irf_letters_reasons <- IRFletters_reduced %>%
        mutate(Failed_CAUTI = grepl("Catheter-Associated Urinary Tract Infection (CAUTI)", V1, 
                                  ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        mutate(Failed_CDIFF = grepl("Facility-wide Inpatient Hospital-onset Clostridium difficile Infection (CDI)", V1, 
                                    ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        mutate(Failed_HCPFLU = grepl("Influenza Vaccination Coverage among Healthcare Personnel data", V1, 
                                    ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        mutate(Failed_HCPCOVID = grepl("Did not submit all required months of complete COVID-19 Vaccination", V1, 
                                     ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        mutate(Failed_ASMT = grepl("threshold on the IRF Patient Assessment Instrument (IRF-PAI) reporting", V1, 
                                      ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        select(file_name, V1, Failed_CAUTI, Failed_CDIFF, Failed_HCPFLU, Failed_HCPCOVID, Failed_ASMT)
      
      
      #combine the data sets and reduce
      irf_letters_full <- cbind(irf_letters_reasons, IRFstrings_reduced)
      
      
      # clean up the data set and transform into usable variables
      # pull ccn from V12 line (reference line)
      # note ccn for HHA is assumed to have no alpha-numeric values
      # code will need to change if this suddenly changes
      # pull separate name and ccn from V14 line (body of letter)
      #   # I found that sometimes ccn is in V15... 
      # split V10 into separate city, state, and zip (address line)
      # rename V7 is the contact name found in the address line
      # rename V8 is the facility name found in address line
      # rename v9 is the street address in address line
      # keep only variables of interest
      irf_letters_clean1 <- irf_letters_full %>%
        mutate(CCN_re_Line = regmatches(V12, regexpr("\\d{2}[3TR]{1}[ABC0-9]{1}\\d{2}", V12, ignore.case = TRUE), invert = FALSE)) %>%
        mutate(V14_V15_Smush = paste(V14, V15, sep=" "))%>%
        mutate(Smush_Name_CCN = gsub("This Letter is to officially notify you that ", "", V14_V15_Smush, ignore.case = TRUE, perl = FALSE)) %>%
        mutate(Name_Body = sub("\\;.*", "", Smush_Name_CCN)) %>%
        mutate(CCN_Body = regmatches(Smush_Name_CCN, regexpr("\\d{2}[3TR]{1}[ABC0-9]{1}\\d{2}", Smush_Name_CCN, ignore.case = TRUE), invert = FALSE)) %>%
        mutate(City = gsub(", \\D{2} \\d{5}", "", V10 , ignore.case = TRUE, perl = TRUE)) %>%
        mutate(State_Zip =  sub(".*,", "", V10)) %>%
        mutate(State = regmatches(State_Zip, regexpr("\\b[A-Z]{2}\\b", State_Zip, ignore.case = TRUE), invert = FALSE)) %>%
        mutate(Zip = regmatches(State_Zip, regexpr("\\d{5}", State_Zip, ignore.case = TRUE), invert = FALSE)) %>%
        rename(Contact_Full_Name = V7,
               Name_Adr_Line = V8,
               Str_Adr = V9,
        )%>%
        select(file_name, V1, Contact_Full_Name, Name_Adr_Line, Str_Adr, City, State, Zip, 
               CCN_re_Line, Name_Body, CCN_Body, Failed_CAUTI, Failed_CDIFF, Failed_HCPFLU, Failed_HCPCOVID, Failed_ASMT)
      
      # call function to clean file name into useable variables
      # IRF uses iqies function
      file_name_iqies(clean_setting="irf", clean_data=irf_letters_clean1)
      
      #combine all variables
      irf_final <- cbind(irf_letters_clean1, irf_file_name_vars) %>%
        select(file_name, file_name_set, file_name_ID, file_name_ref, file_name_Year,
               Contact_Full_Name, Name_Adr_Line, Str_Adr, City, State, Zip, CCN_re_Line, 
               Name_Body, CCN_Body, Failed_CAUTI, Failed_CDIFF, Failed_HCPFLU, Failed_HCPCOVID, Failed_ASMT)
      
      
      # XLSX output naming 
      irf_out_name <- paste("QA_IRF_FY", year_ref, "_", today, ".xlsx", sep= "", collapse = NULL)
      
      #output into an excel file
      write.xlsx(irf_final, irf_out_name, append = FALSE)
      
      #close IRF section
    }    
    
    # for LTCH only
    # check for control flag settings for [QRP]
    if(run_LTCH == 1){
      # call function to clear any existing parameters
      clear_parameters(want_clear=1)
      
      # call function to make data
      # key output of this function are
      # setting should be the QRP setting short name
      # [QRP]letters_reduced
      # [QRP]strings_reduced
      # [QRP]file_name_vars
      make_data(setting="LTCH", in_path= ltch_path)
      
      #find reasons for failure from V1
      ltch_letters_reasons <- LTCHletters_reduced %>%
        mutate(Failed_CAUTI = grepl("Catheter-Associated Urinary Tract Infection (CAUTI)", V1, 
                                    ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        mutate(Failed_CLABSI = grepl("Central Line-Associated Bloodstream Infection (CLABSI)", V1, 
                                    ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        mutate(Failed_CDIFF = grepl("Facility-wide Inpatient Hospital-onset Clostridium difficile Infection (CDI)", V1, 
                                    ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        mutate(Failed_HCPFLU = grepl("Influenza Vaccination Coverage among Healthcare Personnel data", V1, 
                                     ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        mutate(Failed_HCPCOVID = grepl("Did not submit all required months of complete COVID-19 Vaccination", V1, 
                                       ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        mutate(Failed_ASMT = grepl("threshold on the Long-Term Care Data Set (LCDS)", V1, 
                                   ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        select(file_name, V1, Failed_CAUTI,  Failed_CDIFF, Failed_CLABSI, Failed_HCPFLU, Failed_HCPCOVID, Failed_ASMT)
      
      
      #combine the data sets and reduce
      ltch_letters_full <- cbind(ltch_letters_reasons, LTCHstrings_reduced)
      
      
      # clean up the data set and transform into usable variables
      # pull ccn from V12 line (reference line)
      # note ccn for HHA is assumed to have no alpha-numeric values
      # code will need to change if this suddenly changes
      # pull separate name and ccn from V14 line (body of letter)
      #   # I found that sometimes ccn is in V15... 
      # split V10 into separate city, state, and zip (address line)
      # rename V7 is the contact name found in the address line
      # rename V8 is the facility name found in address line
      # rename v9 is the street address in address line
      # keep only variables of interest
      ltch_letters_clean1 <- ltch_letters_full %>%
        mutate(CCN_re_Line = regmatches(V12, regexpr("\\d{2}[2]{1}\\d{3}", V12, ignore.case = TRUE), invert = FALSE)) %>%
        mutate(V14_V15_Smush = paste(V14, V15, sep=" "))%>%
        mutate(Smush_Name_CCN = gsub("This Letter is to officially notify you that ", "", V14_V15_Smush, ignore.case = TRUE, perl = FALSE)) %>%
        mutate(Name_Body = sub("\\;.*", "", Smush_Name_CCN)) %>%
        mutate(CCN_Body = regmatches(Smush_Name_CCN, regexpr("\\d{2}[2]{1}\\d{3}", Smush_Name_CCN, ignore.case = TRUE), invert = FALSE)) %>%
        mutate(City = gsub(", \\D{2} \\d{5}", "", V10 , ignore.case = TRUE, perl = TRUE)) %>%
        mutate(State_Zip =  sub(".*,", "", V10)) %>%
        mutate(State = regmatches(State_Zip, regexpr("\\b[A-Z]{2}\\b", State_Zip, ignore.case = TRUE), invert = FALSE)) %>%
        mutate(Zip = regmatches(State_Zip, regexpr("\\d{5}", State_Zip, ignore.case = TRUE), invert = FALSE)) %>%
        rename(Contact_Full_Name = V7,
               Name_Adr_Line = V8,
               Str_Adr = V9,
        )%>%
        select(file_name, V1, Contact_Full_Name, Name_Adr_Line, Str_Adr, City, State, Zip, 
               CCN_re_Line, Name_Body, CCN_Body, 
               Failed_CAUTI,  Failed_CDIFF, Failed_CLABSI, Failed_HCPFLU, Failed_HCPCOVID, Failed_ASMT)
      
      # call function to clean file name into useable variables
      # ltch uses iqies function
      file_name_iqies(clean_setting="ltch", clean_data=ltch_letters_clean1)
      
      #combine all variables
      ltch_final <- cbind(ltch_letters_clean1, ltch_file_name_vars) %>%
        select(file_name, file_name_set, file_name_ID, file_name_ref, file_name_Year,
               Contact_Full_Name, Name_Adr_Line, Str_Adr, City, State, Zip, CCN_re_Line, 
               Name_Body, CCN_Body, 
               Failed_CAUTI,  Failed_CDIFF, Failed_CLABSI, Failed_HCPFLU, Failed_HCPCOVID, Failed_ASMT)
      
      
      # XLSX output naming 
      ltch_out_name <- paste("QA_LTCH_FY", year_ref, "_", today, ".xlsx", sep= "", collapse = NULL)
      
      #output into an excel file
      write.xlsx(ltch_final, ltch_out_name, append = FALSE)
      
      #close LTCH section
    }   
    

    # for SNF only
    # check for control flag settings for [QRP]
    if(run_SNF == 1){
      # call function to clear any existing parameters
      clear_parameters(want_clear=1)
      
      # call function to make data
      # key output of this function are
      # setting should be the QRP setting short name
      # [QRP]letters_reduced
      # [QRP]strings_reduced
      # [QRP]file_name_vars
      make_data(setting="SNF", in_path= snf_path)
      
      #find reasons for failure from V1
      snf_letters_reasons <- SNFletters_reduced %>%
        mutate(Failed_HCPCOVID = grepl("Did not submit all required months of complete COVID-19 Vaccination", V1, 
                                       ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        mutate(Failed_HCPFLU = grepl("Influenza Vaccination Coverage among Healthcare Personnel data", V1, 
                                     ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        mutate(Failed_ASMT = grepl("threshold on the Minimum Data Set (MDS)", V1, 
                                  ignore.case = FALSE, perl = FALSE, fixed = TRUE)) %>%
        select(file_name, V1, Failed_HCPCOVID, Failed_HCPFLU, Failed_ASMT)
      
      
      #combine the data sets and reduce
      snf_letters_full <- cbind(snf_letters_reasons, SNFstrings_reduced)
      
      
      # clean up the data set and transform into usable variables
      # pull ccn from V12 line (reference line)
      # note ccn for HHA is assumed to have no alpha-numeric values
      # code will need to change if this suddenly changes
      # pull separate name and ccn from V14 line (body of letter)
      #   # I found that sometimes ccn is in V15... 
      # split V10 into separate city, state, and zip (address line)
      # rename V7 is the contact name found in the address line
      # rename V8 is the facility name found in address line
      # rename v9 is the street address in address line
      # keep only variables of interest
      snf_letters_clean1 <- snf_letters_full %>%
        mutate(CCN_re_Line = regmatches(V12, regexpr("\\d{6}", V12, ignore.case = TRUE), invert = FALSE)) %>%
        mutate(V14_V15_Smush = paste(V14, V15, sep=" "))%>%
        mutate(Smush_Name_CCN = gsub("This Letter is to officially notify you that ", "", V14_V15_Smush, ignore.case = TRUE, perl = FALSE)) %>%
        mutate(Name_Body = sub("\\;.*", "", Smush_Name_CCN)) %>%
        mutate(CCN_Body = regmatches(Smush_Name_CCN, regexpr("\\d{6}", Smush_Name_CCN, ignore.case = TRUE), invert = FALSE)) %>%
        mutate(City = gsub(", \\D{2} \\d{5}", "", V10 , ignore.case = TRUE, perl = TRUE)) %>%
        mutate(State_Zip =  sub(".*,", "", V10)) %>%
        mutate(State = regmatches(State_Zip, regexpr("\\b[A-Z]{2}\\b", State_Zip, ignore.case = TRUE), invert = FALSE)) %>%
        mutate(Zip = regmatches(State_Zip, regexpr("\\d{5}", State_Zip, ignore.case = TRUE), invert = FALSE)) %>%
        rename(Contact_Full_Name = V7,
               Name_Adr_Line = V8,
               Str_Adr = V9,
        )%>%
        select(file_name, V1, Contact_Full_Name, Name_Adr_Line, Str_Adr, City, State, Zip, 
               CCN_re_Line, Name_Body, CCN_Body, Failed_HCPCOVID, Failed_HCPFLU, Failed_ASMT)
      
      # call function to clean file name into useable variables
      # SNF  uses iqies function FY2024 onward
      file_name_iqies(clean_setting="snf", clean_data=snf_letters_clean1)
      
      
      #combine all variables
      snf_final <- cbind(snf_letters_clean1, snf_file_name_vars) %>%
        select(file_name, file_name_set, file_name_ID, file_name_ref, file_name_Year,
               Contact_Full_Name, Name_Adr_Line, Str_Adr, City, State, Zip, CCN_re_Line, 
               Name_Body, CCN_Body, Failed_HCPCOVID, Failed_HCPFLU, Failed_ASMT)
      
      
      # XLSX output naming 
      SNF_out_name <- paste("QA_SNF_FY", year_ref, "_", today, ".xlsx", sep= "", collapse = NULL)
      
      #output into an excel file
      write.xlsx(snf_final, SNF_out_name, append = FALSE)
      
      #close SNF section
    }   