library(dplyr)
library(readr)
library(stringr)
library(lubridate)
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
crashes_reformat_reduced <- crashes_reformat[!(crashes_reformat$LightingCode == "A" | is.na(crashes_reformat$LightingCode)), ]

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
crash_data <- merge(crashes_reformat_reduced, parties_reformat, by = "CollisionId")

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
    WeekDay = sapply(WeekDay, FUN = get_first)
  )

#may want to encode any factor involving "HEAD LIGHTS"-- but will need to justify in the paper
head_lights_involvment <- (grepl("HEAD ?LIGHT", crash_data_mergedFactors$factor, ignore.case = TRUE) | grepl("HIGH ?BEAM", crash_data_mergedFactors$factor, ignore.case = TRUE) | grepl("HDLIGHTS", crash_data_mergedFactors$factor, ignore.case = TRUE)) & !(grepl("DIM HEAD ?LIGHTS", crash_data_mergedFactors$factor, ignore.case = TRUE))
vision_obsc_light <- grepl("VISION OBSCUREMENT - ", crash_data_mergedFactors$factor, ignore.case = TRUE) & (grepl("LIGHT", crash_data_mergedFactors$factor, ignore.case = TRUE) | grepl("GLARE", crash_data_mergedFactors$factor, ignore.case = TRUE) | grepl("BLINDED", crash_data_mergedFactors$factor, ignore.case = TRUE)) & !(grepl("SUN", crash_data_mergedFactors$factor, ignore.case = TRUE))
#we'll need to exclude dim headlights, but include headlights being off--justify in paper
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

#get proportion of accidents involving light: 
almost_final_crash_data <- crash_data_cleanedFactors %>%
  mutate(
    headlightsInvolved = light_codes_present | vision_obsc_light | head_lights_involvment, 
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

write.csv(final_reduced, "./data/clean_crash_data_night.csv")

#removing age and reducing row obs to one per date for easier exploratory analysis
final_reduced_ageless <-  subset(final_reduced, select = c(CrashDate, WeekDay, Month, Year, prop)) %>%
  distinct()
write.csv(final_reduced_ageless, "./data/clean_crash_data_reduced_night.csv")

final_no_zero <- final_reduced[final_reduced$prop > 0,]
write.csv(final_no_zero, "./data/clean_crash_data_no_zero_night.csv")

zeroes_night = data.frame(Year = 2016:2025,
                          DaysWithHeadlightAccidents = c(sum(final_reduced_ageless[final_reduced_ageless$Year == 2016,]$prop != 0),
                                                         sum(final_reduced_ageless[final_reduced_ageless$Year == 2017,]$prop != 0),
                                                         sum(final_reduced_ageless[final_reduced_ageless$Year == 2018,]$prop != 0),
                                                         sum(final_reduced_ageless[final_reduced_ageless$Year == 2019,]$prop != 0),
                                                         sum(final_reduced_ageless[final_reduced_ageless$Year == 2020,]$prop != 0),
                                                         sum(final_reduced_ageless[final_reduced_ageless$Year == 2021,]$prop != 0),
                                                         sum(final_reduced_ageless[final_reduced_ageless$Year == 2022,]$prop != 0),
                                                         sum(final_reduced_ageless[final_reduced_ageless$Year == 2023,]$prop != 0),
                                                         sum(final_reduced_ageless[final_reduced_ageless$Year == 2024,]$prop != 0),
                                                         sum(final_reduced_ageless[final_reduced_ageless$Year == 2025,]$prop != 0)),
                          DaysWithoutHeadlightAccidents = c(sum(final_reduced_ageless[final_reduced_ageless$Year == 2016,]$prop == 0),
                                                            sum(final_reduced_ageless[final_reduced_ageless$Year == 2017,]$prop == 0),
                                                            sum(final_reduced_ageless[final_reduced_ageless$Year == 2018,]$prop == 0),
                                                            sum(final_reduced_ageless[final_reduced_ageless$Year == 2019,]$prop == 0),
                                                            sum(final_reduced_ageless[final_reduced_ageless$Year == 2020,]$prop == 0),
                                                            sum(final_reduced_ageless[final_reduced_ageless$Year == 2021,]$prop == 0),
                                                            sum(final_reduced_ageless[final_reduced_ageless$Year == 2022,]$prop == 0),
                                                            sum(final_reduced_ageless[final_reduced_ageless$Year == 2023,]$prop == 0),
                                                            sum(final_reduced_ageless[final_reduced_ageless$Year == 2024,]$prop == 0),
                                                            sum(final_reduced_ageless[final_reduced_ageless$Year == 2025,]$prop == 0)))
zeroes_night <- zeroes_night %>% mutate(total = DaysWithHeadlightAccidents + DaysWithoutHeadlightAccidents) 
write.csv(zeroes_night, "./data/night_zeroes.csv")