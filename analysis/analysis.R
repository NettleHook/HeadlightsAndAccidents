library(dplyr)
library(ggplot2)

crash_data_reduced <- read_csv("./data/clean_crash_data_reduced.csv")

ggplot(crash_data_reduced, aes(y = prop, x = Year)) + 
  geom_point() + 
  geom_smooth(method = "lm")
ggsave("./paper/ref/headlight_prop_against_year.jpeg", plot = get_last_plot())

ggplot(crash_data_reduced, aes(y = log(prop), x = Year)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
ggsave("./paper/ref/log_headlight_prop_against_year.jpeg", plot = get_last_plot())

reduced_model <- lm(prop ~ WeekDay + as.factor(Month) + Year, data = crash_data_reduced)

crash_data_no_zeros <- read_csv("./data/clean_crash_data_no_zero_night.csv")
ggplot(crash_data_no_zeros, aes(y = prop, x = Year)) + 
  geom_point() + 
  geom_smooth(method = "lm")
ggsave("./paper/ref/headlight_prop_against_year_no_zero.jpeg", plot = get_last_plot())

ggplot(crash_data_no_zeros, aes(y = log(prop), x = Year)) + 
  geom_point() + 
  geom_smooth(method = "lm")
ggsave("./paper/ref/log_headlight_prop_against_year_no_zero.jpeg", plot = get_last_plot())

full_model_no_zero <- lm(prop ~ WeekDay + as.factor(Month) + Year, data = crash_data_no_zeros)

full_model_no_zero_age <- lm(prop ~ WeekDay + as.factor(Month) +  elderlyInvolved*Year, data = crash_data_no_zeros)

crash_data_night <- read_csv("./data/clean_crash_data_night.csv")
ggplot(crash_data_night, aes(y = prop, x = Year)) + 
  geom_point() + 
  geom_smooth(method = "lm")
ggsave("./paper/ref/headlight_prop_against_year_night.jpeg", plot = get_last_plot())

ggplot(crash_data_night, aes(y = log(prop), x = Year)) + 
  geom_point() + 
  geom_smooth(method = "lm")
ggsave("./paper/ref/log_headlight_prop_against_year_night.jpeg", plot = get_last_plot())

full_model_night <- lm(prop ~ WeekDay + as.factor(Month) + Year, data = crash_data_night)

full_model_night_age <- lm(prop ~ WeekDay + as.factor(Month) +  elderlyInvolved*Year, data = crash_data_night)

sink()
sink("./paper/ref/reduced_model.txt")
print(summary(reduced_model))
sink()
sink("./paper/ref/full_model_no_zero.txt")
print(summary(full_model_no_zero))
sink()
sink("./paper/ref/full_model_no_zero_age.txt")
print(summary(full_model_no_zero_age))
sink()
sink("./paper/ref/full_model_night.txt")
print(summary(full_model_night))
sink()
sink("./paper/ref/full_model_night_age.txt")
print(summary(full_model_night_age))
sink()
