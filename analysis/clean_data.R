library(dplyr)
library(readr)
library(stringr)
crashes <- read_csv("./data/2025_crashes.csv", col_select = c("Collision Id", "Crash Date Time", "Day Of Week", "LightingCode", "Primary Collision Factor Violation"))
crashes <- rbind(crashes, read_csv("./data/2024_crashes.csv", col_select = c("Collision Id", "Crash Date Time", "Day Of Week", "LightingCode", "Primary Collision Factor Violation")))
crashes <- rbind(crashes, read_csv("./data/2023_crashes.csv", col_select = c("Collision Id", "Crash Date Time", "Day Of Week", "LightingCode", "Primary Collision Factor Violation")))
crashes <- rbind(crashes, read_csv("./data/2022_crashes.csv", col_select = c("Collision Id", "Crash Date Time", "Day Of Week", "LightingCode", "Primary Collision Factor Violation")))
crashes <- rbind(crashes, read_csv("./data/2021_crashes.csv", col_select = c("Collision Id", "Crash Date Time", "Day Of Week", "LightingCode", "Primary Collision Factor Violation")))
crashes <- rbind(crashes, read_csv("./data/2020_crashes.csv", col_select = c("Collision Id", "Crash Date Time", "Day Of Week", "LightingCode", "Primary Collision Factor Violation")))
crashes <- rbind(crashes, read_csv("./data/2019_crashes.csv", col_select = c("Collision Id", "Crash Date Time", "Day Of Week", "LightingCode", "Primary Collision Factor Violation")))
crashes <- rbind(crashes, read_csv("./data/2018_crashes.csv", col_select = c("Collision Id", "Crash Date Time", "Day Of Week", "LightingCode", "Primary Collision Factor Violation")))
crashes <- rbind(crashes, read_csv("./data/2017_crashes.csv", col_select = c("Collision Id", "Crash Date Time", "Day Of Week", "LightingCode", "Primary Collision Factor Violation")))
crashes <- rbind(crashes, read_csv("./data/2016_crashes.csv", col_select = c("Collision Id", "Crash Date Time", "Day Of Week", "LightingCode", "Primary Collision Factor Violation")))
names(crashes) <- c("CollisionId", "CrashDate", "WeekDay", "LightingCode", "Prim_CollisionFactor")
crashes_reformat <- crashes %>%
  mutate(
    CrashDate = str_split_i(CrashDate, " ", 1)
  )

parties <- read_csv("./data/2025_parties.csv", col_select = c("CollisionId", "Other Associate Factor", "StatedAge"))
parties <- rbind(parties, read_csv("./data/2024_parties.csv", col_select = c("CollisionId", "Other Associate Factor", "StatedAge")))
parties <- rbind(parties, read_csv("./data/2023_parties.csv", col_select = c("CollisionId", "Other Associate Factor", "StatedAge")))
parties <- rbind(parties, read_csv("./data/2022_parties.csv", col_select = c("CollisionId", "Other Associate Factor", "StatedAge")))
parties <- rbind(parties, read_csv("./data/2021_parties.csv", col_select = c("CollisionId", "Other Associate Factor", "StatedAge")))
parties <- rbind(parties, read_csv("./data/2020_parties.csv", col_select = c("CollisionId", "Other Associate Factor", "StatedAge")))
parties <- rbind(parties, read_csv("./data/2019_parties.csv", col_select = c("CollisionId", "Other Associate Factor", "StatedAge")))
parties <- rbind(parties, read_csv("./data/2018_parties.csv", col_select = c("CollisionId", "Other Associate Factor", "StatedAge")))
parties <- rbind(parties, read_csv("./data/2017_parties.csv", col_select = c("CollisionId", "Other Associate Factor", "StatedAge")))
parties <- rbind(parties, read_csv("./data/2016_parties.csv", col_select = c("CollisionId", "Other Associate Factor", "StatedAge")))
names(parties) <- c("CollisionId", "OtherCollisionFactor", "Age")

#replace "None Apparent"s/NAs before proceeding
parties_reformat <- parties %>%
  mutate(
    OtherCollisionFactor = str_replace(OtherCollisionFactor, "NONE APPARENT", "0"),
    OtherCollisionFactor = str_replace(OtherCollisionFactor, "VISION OBSCUREMENT - SUNLIGHT", "0"),
    Age = ifelse(is.na(Age), 0, Age)
  )

#will need to merge datasets on collision id first before sorting
crash_data <- merge(crashes_reformat, parties_reformat, by = "CollisionId")

grouped_crash_data <- crash_data %>%
  group_by(CollisionId) %>%
  summarise(across(everything(), list))

get_first<- function(vec){
  vec[1]
}
crash_data_mergedFactors <- grouped_crash_data %>%
  mutate(
    CrashDate = sapply(CrashDate, FUN = get_first),
    Age = sapply(Age, FUN = max),
    factor = paste(Prim_CollisionFactor, OtherCollisionFactor, sep =","),
    WeekDay = sapply(WeekDay, FUN = get_first),
    Dark = !(is.na(LightingCode) | (lapply(LightingCode, FUN = get_first) == "A"))
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

#get proportion of accidents involving light: 
almost_final_crash_data <- crash_data_cleanedFactors %>%
  mutate(
  headlightsInvolved = light_codes_present | (vision_obsc_light & Dark) | head_lights_involvment, 
  elderlyInvolved = Age>= 70,
  CrashDate_parse = parse_datetime(CrashDate, format = "%m/%d/%Y"),
  Month = sapply(CrashDate_parse, FUN = month),
  Year = sapply(CrashDate_parse, FUN = year)
)

prop_data <- almost_final_crash_data %>%
  group_by(CrashDate) %>%
  summarize(
    headlight_sum = sum(headlightsInvolved),
    num_crashes = n(),
    prop = headlight_sum/num_crashes
  )

#want to rebuild data frame to only include the columns we want before saving it to clean_data

final_crash_data <- merge(almost_final_crash_data, prop_data, by = "CrashDate")

final_reduced <- subset(final_crash_data, select = c(CrashDate, WeekDay, Month, Year, elderlyInvolved, prop))

write.csv(final_reduced, "./data/clean_crash_data.csv")
