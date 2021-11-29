# retrieve raw data from database
setwd("~/New York State Office of Information Technology Services/BWAM - Lakes Database/Current")
source("new_database/Reading.LMAS.Data.R")
setwd("~/OneDrive - New York State Office of Information Technology Services/Rscripts/Wallkill/")

rm(list=setdiff(ls(), c('newdata')))

################water column#################
df<-newdata %>%
  filter(LOCATION_PWL_ID == "1306-0037",
         SAMPLE_DATE>'2018-01-01'
         ) %>%
  mutate(combined=paste(CHARACTERISTIC_NAME,
                        INFORMATION_TYPE,
                        RSLT_RESULT_SAMPLE_FRACTION,
                        sep = "_"))  %>%
  select(LAKE_HISTORY_ID,
         SAMPLE_DATE,
         combined,
         RSLT_RESULT_VALUE,
         RSLT_LABORATORY_QUALIFIER,
         RSLT_VALIDATOR_QUALIFIER) %>%
  mutate(RSLT_RESULT_VALUE=ifelse(!is.na(RSLT_LABORATORY_QUALIFIER)&(RSLT_LABORATORY_QUALIFIER=="U"|RSLT_LABORATORY_QUALIFIER=="UE"),"0",RSLT_RESULT_VALUE),
         RSLT_RESULT_VALUE=as.numeric(RSLT_RESULT_VALUE)) %>%
  filter(!is.na(RSLT_RESULT_VALUE),
         is.na(RSLT_VALIDATOR_QUALIFIER)|(RSLT_VALIDATOR_QUALIFIER!="R"),
         combined %in% c('CHLOROPHYLL A_OW_TOTAL',
                         "CHLOROPHYLL A_BS_TOTAL",
                         'PHOSPHORUS, TOTAL_OW_TOTAL',
                         'PHOSPHORUS, TOTAL_BS_TOTAL',
                         "NITROGEN, NITRATE-NITRITE_OW_TOTAL",
                         "NITROGEN, NITRATE-NITRITE_BS_TOTAL",
                         "NITROGEN, KJELDAHL, TOTAL_OW_TOTAL",
                         "NITROGEN, KJELDAHL, TOTAL_BS_TOTAL",
                         "NITROGEN, AMMONIA (AS N)_OW_TOTAL",
                         "NITROGEN, AMMONIA (AS N)_BS_TOTAL",
                         "CHLORIDE_OW_TOTAL",
                         "CHLORIDE_BS_TOTAL",
                         "IRON_OW_TOTAL",
                         "IRON_BS_TOTAL",
                         "DEPTH, SECCHI DISK DEPTH_SD_NA",
                         "TRUE COLOR_OW_TOTAL")) %>%
  select(LAKE_HISTORY_ID,SAMPLE_DATE,combined,RSLT_RESULT_VALUE) %>%
  distinct(LAKE_HISTORY_ID,SAMPLE_DATE,combined,RSLT_RESULT_VALUE,.keep_all = TRUE) %>%
  rename(LAKE_ID=LAKE_HISTORY_ID,
         chemical_name=combined,
         result_value=RSLT_RESULT_VALUE)
  
df<-df %>% 
  # pivot wide
  spread(chemical_name,result_value,fill = NA)

df <- df %>% 
  # create total nitrogen
  mutate(`NITROGEN, OW_TOTAL`=`NITROGEN, NITRATE-NITRITE_OW_TOTAL`+`NITROGEN, KJELDAHL, TOTAL_OW_TOTAL`,
         `NITROGEN, BS_TOTAL`=`NITROGEN, NITRATE-NITRITE_BS_TOTAL`+`NITROGEN, KJELDAHL, TOTAL_BS_TOTAL`)

write.csv(df, file="sturgeon_WaterColumn_data.csv", na = "", quote = TRUE, row.names = FALSE)



################depth profile#################

rm(list=setdiff(ls(), c('newdata')))

df<-newdata %>%
  filter(LOCATION_PWL_ID == "1306-0037",
         SAMPLE_DATE>'2018-01-01',
         SAMPLE_TYPE=="WATER COLUMN",
         INFORMATION_TYPE=="DP") %>%
  mutate(combined=paste(CHARACTERISTIC_NAME,
                        INFORMATION_TYPE,
                        RSLT_RESULT_SAMPLE_FRACTION,
                        sep = "_"))  %>%
  select(LAKE_HISTORY_ID,
         SAMPLE_DATE,
         combined,
         RSLT_RESULT_VALUE,
         RSLT_LABORATORY_QUALIFIER,
         RSLT_VALIDATOR_QUALIFIER,
         RSLT_PROFILE_DEPTH) %>%
  mutate(RSLT_RESULT_VALUE=ifelse(!is.na(RSLT_LABORATORY_QUALIFIER)&(RSLT_LABORATORY_QUALIFIER=="U"|RSLT_LABORATORY_QUALIFIER=="UE"),"0",RSLT_RESULT_VALUE),
         RSLT_RESULT_VALUE=as.numeric(RSLT_RESULT_VALUE)) %>%
  filter(!is.na(RSLT_RESULT_VALUE),
         is.na(RSLT_VALIDATOR_QUALIFIER)|(RSLT_VALIDATOR_QUALIFIER!="R")) %>%
  select(LAKE_HISTORY_ID,SAMPLE_DATE,combined,RSLT_RESULT_VALUE,RSLT_PROFILE_DEPTH) %>%
  distinct(LAKE_HISTORY_ID,SAMPLE_DATE,combined,RSLT_PROFILE_DEPTH,.keep_all = TRUE) %>%
  rename(LAKE_ID=LAKE_HISTORY_ID,
         chemical_name=combined,
         result_value=RSLT_RESULT_VALUE,
         profile_depth=RSLT_PROFILE_DEPTH)

df<-df %>% 
  # pivot wide
  spread(chemical_name,result_value,fill = NA)

write.csv(df, file="sturgeon_DepthProfile_data.csv", na = "", quote = TRUE, row.names = FALSE)
