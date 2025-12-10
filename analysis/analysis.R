library(dplyr)
library(ggplot2)

#Explore
crash_data_reduced <- read_csv("./data/clean_crash_data_reduced_night.csv")
ggsave("./paper/ref/headlight_prop_against_year.jpeg", plot = ggplot(crash_data_reduced, aes(y = prop, x = Year)) + 
         geom_point() + 
         geom_smooth(method = "lm"))


ggsave("./paper/ref/log_headlight_prop_against_year.jpeg", plot = ggplot(crash_data_reduced, aes(y = log(prop), x = Year)) + 
         geom_point() + 
         geom_smooth(method = "lm", se = FALSE))

#Would ideally like to drop at least some zeroes. Checking for any pattern:
night_zeroes <- read_csv("./data/night_zeroes.csv")
ggsave("./paper/ref/night_zeroes.jpeg", plot = ggplot(night_zeroes, aes(x = Year, y = DaysWithoutHeadlightAccidents)) + 
         geom_point() + 
         geom_smooth(method= "loess", se = FALSE))
#None, will have to leave them

#Looking at just night data. We already plan to use a log transform on the response var, but have zeroes which pose a problem.
#Since this is a proportion, we'll run the regression against the converse and just have to keep that in mind when we're doing inference
crash_data_night <- read_csv("./data/clean_crash_data_night.csv") %>%
  mutate(
    prop_converse = 1-prop
  )

ggsave("./paper/ref/headlight_prop_against_year_night.jpeg", plot = ggplot(crash_data_night, aes(y = prop_converse, x = Year)) + 
         geom_point() + 
         geom_smooth(method = "lm"))

ggsave("./paper/ref/log_headlight_prop_against_year_night.jpeg", plot = ggplot(crash_data_night, aes(y = log(prop_converse), x = Year)) + 
         geom_point() + 
         geom_smooth(method = "lm"))


#Models
full_model_night <- lm(log(prop_converse) ~ WeekDay + as.factor(Month) + Year, data = crash_data_night)

full_model_night_age <- lm(log(prop_converse) ~ WeekDay + as.factor(Month) +  elderlyInvolved*Year, data = crash_data_night)

#Saving model information to text file so it can be pulled later if needed
sink("./paper/ref/full_model_night.txt")
print(summary(full_model_night))
sink()
sink("./paper/ref/full_model_night_age.txt")
print(summary(full_model_night_age))
sink()

#Will need to compare the two models. Already thinking that 
#full_model_night is the better one because of the uncertainty associated with
#the elderly classifier. Standard error > coefficient for both the standalone and interaction effect

model_compare_info <- data.frame(night_residuals = resid(full_model_night), night_fitted = fitted(full_model_night), night_age_residuals = resid(full_model_night_age), night_age_fitted = fitted(full_model_night_age))

ggsave("./paper/ref/night_model_resid_against_fitted.jpeg", plot = ggplot(model_compare_info, aes(y= night_residuals, x = night_fitted)) + geom_point())
ggsave("./paper/ref/night_age_model_resid_against_fitted.jpeg", plot = ggplot(model_compare_info, aes(y= night_age_residuals, x = night_age_fitted)) + geom_point())

#Also clear from residuals that the age variable is not accounting for much more variability.

