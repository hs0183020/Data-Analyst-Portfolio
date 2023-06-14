#install.packages("haven")
#install.packages("writexl")
#install.packages("dplyr")

library("writexl")
library("haven")
library("dplyr")
mycchc <- read_sas("//uctnascifs.uthouston.edu/sph/Research/Studies Data/All_CCHC/cchc.sas7bdat")
names(mycchc ) <- toupper(names(mycchc))

df_mycchc <- subset(mycchc , ,select=c('RRID','INTERVIEW_DATE','LABID','STUDY','VISIT','CKD_EPI_GFR_CALC','CREA','GFR'))
df_mycchc1 <- subset(df_mycchc,!(is.na(df_mycchc$INTERVIEW_DATE)) ,)

df_clac_gfr_or_gfr <- subset(df_mycchc1,!(is.na(df_mycchc1$CKD_EPI_GFR_CALC)) | !(is.na(df_mycchc1$GFR)),)
df_clac_gfr_or_gfr$YEAR <-format(as.Date(df_clac_gfr_or_gfr$INTERVIEW_DATE, format="%d/%m/%Y"),"%Y")
df_clac_gfr_or_gfr <- arrange(df_clac_gfr_or_gfr,YEAR,CKD_EPI_GFR_CALC)

df_clac_gfr_or_gfr1 <- df_clac_gfr_or_gfr %>% 
    group_by(YEAR) %>%
    mutate(
        FIRST = dplyr::first(CKD_EPI_GFR_CALC),
        LAST = dplyr::last(CKD_EPI_GFR_CALC)
    )

df_clac_gfr_or_gfr2 <- df_clac_gfr_or_gfr1 %>% 
   group_by(AGE_AT_VISIT) %>% 
   filter( row_number()==n())

df_clac_gfr_or_gfr2$DECREASE <- (df_clac_gfr_or_gfr2$LAST - df_clac_gfr_or_gfr2$FIRST)

df_clac_gfr_or_gfr3 <- subset(df_clac_gfr_or_gfr2, ,select=c('RRID','AGE_AT_VISIT','LABID','FIRST','LAST','DECREASE'))

new_row = c(DECREASE= mean(df_clac_gfr_or_gfr3$DECREASE))
df_clac_gfr_or_gfr4 = rbind(df_clac_gfr_or_gfr3,new_row)

View(df_clac_gfr_or_gfr)

write_xlsx(df_clac_gfr_or_gfr4,"//uctnascifs.uthouston.edu/uthsc/SPH/Research/BrownsvilleSD/public/Diabetes_Core/Data Request/Farmani/Req_050323(decrease)/Req_050323.xlsx")