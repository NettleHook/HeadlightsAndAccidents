library(dplyr)
library(readr)
library(stringr)
crashes <- read_csv("./data/2025_crashes.csv", col_select = c("Collision Id", "Crash Date Time", "Day Of Week", "LightingCode", "Primary Collision Factor Violation"))
names(crashes) <- c("CollisionId", "CrashDate", "WeekDay", "LightingCode", "Prim_CollisionFactor")

parties <- read_csv("./data/2025_parties.csv", col_select = c("CollisionId", "Other Associate Factor", "StatedAge"))
names(parties) <- c("CollisionId", "OtherCollisionFactor", "Age")
#replace "None Apparent"s/NAs before proceeding?
parties <- parties %>%
  mutate(
    OtherCollisionFactor = str_replace(OtherCollisionFactor, "NONE APPARENT", "0"),
    OtherCollisionFactor = str_replace(OtherCollisionFactor, "VISION OBSCUREMENT - SUNLIGHT", "0"),
    #OtherCollisionFactor = str_replace(OtherCollisionFactor, "VC SECTION VIOLATED - ", ""),
    Age = ifelse(is.na(Age), 0, Age)
  )
other_factors <- unique(parties$OtherCollisionFactor)

#will need to merge datasets on collision id first before sorting
crash_data <- merge(crashes, parties, by = "CollisionId")
grouped_crash_data <- crash_data %>%
  group_by(CollisionId) %>%
  summarise_all(funs(toString(na.omit(.)))) #FIXME: funs is deprecated

crash_data_mergedFactors <- grouped_crash_data %>%
  mutate(
    CrashDate = parse_datetime(str_split_i(CrashDate, " ", 1), format = "%m/%d/%Y"),
    factor = paste(Prim_CollisionFactor, OtherCollisionFactor, sep =","),
    WeekDay = str_split_i(WeekDay, ",", 1),
    Age = str_split(Age, ","),
    Dark = !(is.na(LightingCode) | (str_split_i(LightingCode, ",", 1) == "A"))
)

#may want to encode any factor involving "HEAD LIGHTS"-- but will need to justify in the paper
head_lights_involvment <- (grepl("HEAD ?LIGHT", crash_data_mergedFactors$factor, ignore.case = TRUE) | grepl("HIGH ?BEAM", crash_data_mergedFactors$factor, ignore.case = TRUE) | grepl("HDLIGHTS", crash_data_mergedFactors$factor, ignore.case = TRUE)) & !(grepl("DIM HEAD ?LIGHTS", crash_data_mergedFactors$factor, ignore.case = TRUE))
vision_obsc_light <- grepl("VISION OBSCUREMENT - ", crash_data_mergedFactors$factor, ignore.case = TRUE) & (grepl("LIGHT", crash_data_mergedFactors$factor, ignore.case = TRUE) | grepl("GLARE", crash_data_mergedFactors$factor, ignore.case = TRUE) | grepl("BLINDED", crash_data_mergedFactors$factor, ignore.case = TRUE)) & !(grepl("SUN", crash_data_mergedFactors$factor, ignore.case = TRUE))
sum(vision_obsc_light)
unique(crash_data_mergedFactors$factor[head_lights_involvment])
#we'll need to exclude dim headlights, but include headlights being off--justify in paper
unique(crash_data_mergedFactors$factor[vision_obsc_light]) #should select specifically for vision obscurement glare, and vision obscurement light. Then only set true if these also happen after sunset (can presume due to headlights)
#will also want to mention that some fields just say vision obscurement without elaboration--these were not included

#now we can clean up factor to just include numbers and commas
crash_data_cleanedFactors <- crash_data_mergedFactors %>%
  mutate(
    cleaned_factors = gsub("[^0-9,.-]", "", factor),
    cleaned_factors = str_split(cleaned_factors, ",")
  )

light_codes <- c(24250, 24604, 24400:24410, 24800, 25250, 25650, 25651, 26100)
check_codes <- function(codes){
 for(code in codes){
   if(is.na(as.numeric(code))){
     next
   }
   if(as.numeric(code) %in% light_codes){
     return(TRUE)
   }
 }
  return(FALSE)
}
light_codes_present <- sapply(crash_data_cleanedFactors$cleaned_factors, check_codes)
sum(light_codes_present)

elderly_check<- function(ages){
  for(age in ages){
    if (as.integer(age) >= "70"){
      return(TRUE)
    }
  }
  return(FALSE)
}

#get proportion of accidents involving light: 
almost_final_crash_data <- crash_data_cleanedFactors %>%
  mutate(
  headlightsInvolved = light_codes_present | (vision_obsc_light & Dark) | head_lights_involvment, 
  elderlyInvolved = lapply(Age, FUN = elderly_check),
  Month = month(CrashDate),
  Year = format(CrashDate,"%Y"),
)

prop_data <- almost_final_crash_data %>%
  group_by(CrashDate) %>%
  summarize(
    headlight_sum = sum(headlightsInvolved),
    num_crashes = n(),
    prop = headlight_sum/num_crashes
  )

#want to rebuild data frame to only include the columns we want before saving it to clean_data
