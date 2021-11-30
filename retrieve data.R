# retrieve raw data from database
setwd("~/New York State Office of Information Technology Services/BWAM - Lakes Database/Current")
source("new_database/Reading.LMAS.Data.R")
setwd("~/OneDrive - New York State Office of Information Technology Services/Rscripts/Wallkill/")

rm(list=setdiff(ls(), c('newdata')))

################water column#################
df<-newdata %>%
  filter(LOCATION_PWL_ID == "1306-0037",
         SAMPLE_DATE>'2018-01-01',
         SAMPLE_TYPE == "WATER COLUMN",
         INFORMATION_TYPE %in% c("OW","SD","BS")
         ) %>%
  mutate(combined=paste(CHARACTERISTIC_NAME,
                        RSLT_RESULT_SAMPLE_FRACTION,
                        sep = "_"))  %>%
  select(LAKE_HISTORY_ID,
         SAMPLE_DATE,
         combined,
         INFORMATION_TYPE,
         RSLT_RESULT_VALUE,
         RSLT_RESULT_UNIT,
         RSLT_LABORATORY_QUALIFIER,
         RSLT_VALIDATOR_QUALIFIER) %>%
  mutate(RSLT_RESULT_VALUE=ifelse(!is.na(RSLT_LABORATORY_QUALIFIER)&(RSLT_LABORATORY_QUALIFIER=="U"|RSLT_LABORATORY_QUALIFIER=="UE"),"0",RSLT_RESULT_VALUE),
         RSLT_RESULT_VALUE=as.numeric(RSLT_RESULT_VALUE)) %>%
  filter(!is.na(RSLT_RESULT_VALUE),
         is.na(RSLT_VALIDATOR_QUALIFIER)|(RSLT_VALIDATOR_QUALIFIER!="R")) %>%
  select(LAKE_HISTORY_ID,SAMPLE_DATE,combined,INFORMATION_TYPE,RSLT_RESULT_VALUE,RSLT_RESULT_UNIT) %>%
  distinct(LAKE_HISTORY_ID,SAMPLE_DATE,combined,INFORMATION_TYPE,RSLT_RESULT_VALUE,.keep_all = TRUE) %>%
  rename(LAKE_ID=LAKE_HISTORY_ID,
         chemical_name=combined,
         strata=INFORMATION_TYPE,
         result_value=RSLT_RESULT_VALUE,
         unit=RSLT_RESULT_UNIT)

write.csv(df, file="sturgeon_WaterColumn_data_LONG.csv", na = "", quote = TRUE, row.names = FALSE)
  
df<-df %>% 
  select(LAKE_ID,SAMPLE_DATE,chemical_name,strata,result_value) %>% 
  # pivot wide
  pivot_wider(names_from=c(chemical_name,strata),
              values_from=result_value)

df <- df %>% 
  # create total nitrogen
  mutate(`NITROGEN, TOTAL_OW`=`NITROGEN, NITRATE-NITRITE_TOTAL_OW`+`NITROGEN, KJELDAHL, TOTAL_TOTAL_OW`,
         `NITROGEN, TOTAL_BS`=`NITROGEN, NITRATE-NITRITE_TOTAL_BS`+`NITROGEN, KJELDAHL, TOTAL_TOTAL_BS`)

write.csv(df, file="sturgeon_WaterColumn_data_WIDE.csv", na = "", quote = TRUE, row.names = FALSE)



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
