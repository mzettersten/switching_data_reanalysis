### As of June 2017, eyetrackingR was only compatible with the older 0.5.0 version of dplyr and not newer versions.  See https://github.com/jwdink/eyetrackingR for updates and instructions for reverting to the older version, in order to run this code

if(!"eyetrackingR" %in% installed.packages()) install.packages("eyetrackingR")
if(!"tidyverse" %in% installed.packages()) install.packages("tidyverse")

## Clean the workspace
rm(list=ls(all=T))
options(tibble.print_max = 20, tibble.width = Inf) #Will print whole tibbles

## load libraries
library(eyetrackingR)
library(tidyverse)

## load data
mydata<- read.csv("./data/switching_data.csv")


## Creates initial dataframe
data <- make_eyetrackingr_data(mydata,
                               participant_column = "id",
                               trial_column = "trial.number.unique",
                               time_column = "TrialTimestamp",
                               trackloss_column = "trackloss",
                               aoi_columns = c('look.target', 'look.distractor'),
                               treat_non_aoi_looks_as_missing = TRUE)


#---------------------------------------------------------------------------------------
### Accuracy

## Creates dataframe that is only data within window of analysis, 360ms after noun onset (which occurs at 5400ms)
response_window <- subset_by_window(data, 
                                    window_start_time = 5760, #360ms after noun onset
                                    window_end_time = 7400, #2000ms after noun onset
                                    rezero = FALSE)


## Gets rid of rows with trackloss within window of analysis
response_window_clean <- clean_by_trackloss(data = response_window,
                                            trial_prop_thresh = (1 - 750/(2000-360))) # Need at least 750ms looking = 750/(2000-360)

## Aggregate by subject across response window
response_window_agg_by_sub <- make_time_window_data(response_window_clean, 
                                                    aois = "look.target",
                                                    predictor_columns=c("trial.type", "age.group", "switch.type", "per.dom", "per.nondom", "lang.mix"),
                                                    summarize_by = "id",
                                                    other_dv_columns = c("PupilLeft", "PupilRight"))

## Means and SDs by trial type
response_window_agg_by_sub %>% # Get all the means
  group_by(switch.type, age.group, trial.type) %>%
  summarise(target.mean = mean(Prop), target.sd = sd(Prop), n = length(unique(id)))

## t-tests: Accuracy on same-language vs. switched-language trials
response_window_agg_by_sub %>%
  group_by(switch.type, age.group) %>%
  do(broom::tidy(t.test(.$Prop ~ .$trial.type, data = ., paired = TRUE))) %>%
  mutate(cohen_d = statistic/sqrt(parameter +1))

## Aggregate by subject across response window, on each Carrier type
response_window_agg_by_sub_carrier <- make_time_window_data(response_window_clean, 
                                                    aois = "look.target",
                                                    predictor_columns=c("trial.type", "age.group", "switch.type", "Carrier", "per.dom", "per.nondom", "lang.mix"),
                                                    summarize_by = "id",
                                                    other_dv_columns = c("PupilLeft", "PupilRight"))


## Means and SDs by trial type and Carrier
response_window_agg_by_sub_carrier %>% # Get all the means
  group_by(switch.type, age.group, Carrier, trial.type) %>%
  summarise(target.mean = mean(Prop), target.sd = sd(Prop), n = length(unique(id)))

## t-tests: Accuracy by carrier type on same-language vs. switched-language trials
response_window_agg_by_sub_carrier %>%
  group_by(switch.type, age.group, Carrier) %>%
  do(broom::tidy(t.test(.$Prop ~ .$trial.type, data = ., paired = TRUE))) %>%
  mutate(cohen_d = statistic/sqrt(parameter +1))


#---------------------------------------------------------------------------------------
### Pupillometry

pupil_baseline <- subset_by_window(data, 
                                   window_start_time = 5200, #200 ms prior to noun onset 
                                   window_end_time = 5400, # noun onset
                                   rezero = FALSE)

### Calculate average baseline for each trial
pupil_baseline_by_trial <- pupil_baseline %>%
  group_by(id, trial.number.unique) %>%
  summarize(BaselineRight = mean(PupilRight), BaselineLeft = mean(PupilLeft)) %>%
  select(id, trial.number.unique, BaselineLeft, BaselineRight)

left.na <- is.na(pupil_baseline_by_trial$BaselineLeft) # Finds NAs in left baseline
right.na <- is.na(pupil_baseline_by_trial$BaselineRight) # Finds NAs in right baseline

pupil_baseline_by_trial$BaselineLeft[left.na] <- pupil_baseline_by_trial$BaselineRight[left.na] # Replace left NAs with right
pupil_baseline_by_trial$BaselineRight[right.na] <- pupil_baseline_by_trial$BaselineLeft[right.na] # Replace right NAs with left

## Select the pupillometry time window from the onset of target noun at 5400ms
excl_data <- data %>%
 filter(id != "Subject_35") %>%
 filter(id != "Subject_34") %>%
 filter(id != "Subject_95") 

pupil_window <- subset_by_window(excl_data, 
                                 window_start_time = 5400,
                                 window_end_time = 7400, #2000ms after noun onset
                                 rezero = TRUE)



## Clean using same trackloss criteria as accuracy analysis
pupil_window_clean <- clean_by_trackloss(data = pupil_window,
                                         window_start_time = 360, 
                                         window_end_time = 2000,
                                         trial_prop_thresh = (1 - 750/(2000-360))) # Need at least 750ms looking = 750/(2000-360)

## Merge pupil data over time with baseline data.  Drops trials with no baseline
pupil_response_window <- merge(pupil_window_clean, pupil_baseline_by_trial)

## Calculate corrected pupil sizes
pupil_response_window <- pupil_response_window %>%
  mutate(CorrectedLeft = PupilLeft - BaselineLeft) %>%
  mutate(CorrectedRight = PupilRight - BaselineRight) 


# Average the two eyes (PupilLeft and PupilRight columns), if there is only one use that one

pupil_response_window$PupilMean <- rowMeans(pupil_response_window[which(colnames(pupil_response_window)=="CorrectedLeft"):which(colnames(pupil_response_window)=="CorrectedRight")], na.rm = TRUE)
pupil_response_window$Pupil <- pupil_response_window$PupilMean

# Get rid of unneeded columns
pupil_response_window <- pupil_response_window %>%
  filter(!is.na(Pupil)) %>%
  select(-PupilMean) #Don't need this column

# Recreate dataset that can be read by EyetrackingR functions
pupil_response_window2 <- make_eyetrackingr_data(pupil_response_window,
                                                participant_column = "id",
                                                trial_column = "trial.number.unique",
                                                time_column = "TrialTimestamp",
                                                trackloss_column = "trackloss",
                                                aoi_columns = c('look.target', 'look.distractor'),
                                                treat_non_aoi_looks_as_missing = TRUE)

## Timecourse analysis

## Create pupil timesequence data
response_time_pupil <- make_time_sequence_data(pupil_response_window2, time_bin_size = 200, 
                                               predictor_columns = c("trial.type", "age.group", "Carrier", "switch.type"),
                                               aois = "look.target",
                                               summarize_by = "id",
                                               other_dv_columns = c("Pupil"))



## Trick eyetrackingR code into working by replacing Prop data with Pupil data.  Prop column now contains pupil data
response_time_pupil$Prop <- response_time_pupil$Pupil



#---- Within-sentence

## Within-sentence 20-month-olds dominant language.

pupil_tb_analysis_20_dom_within <- response_time_pupil %>%
  filter(age.group == "20-month-olds") %>%
  filter(Carrier == "Dominant") %>%
  filter(switch.type == "Within-sentence") %>%
  analyze_time_bins(data = ., 
                    predictor_column = "trial.type", 
                    test = "t.test", 
                    alpha = .05)

summary(pupil_tb_analysis_20_dom_within)


## Within-sentence 20-month-olds non-dominant language

pupil_tb_analysis_20_non_within <- response_time_pupil %>%
  filter(age.group == "20-month-olds") %>%
  filter(Carrier == "Non-Dominant") %>%
  filter(switch.type == "Within-sentence") %>%
  analyze_time_bins(data = ., 
                    predictor_column = "trial.type", 
                    test = "t.test", 
                    alpha = .05)

summary(pupil_tb_analysis_20_non_within)


## Within-sentence Adult dominant language

pupil_tb_analysis_adult_dom_within <- response_time_pupil %>%
  filter(age.group == "Adults") %>%
  filter(Carrier == "Dominant") %>%
  filter(switch.type == "Within-sentence") %>%
  analyze_time_bins(data = ., 
                    predictor_column = "trial.type", 
                    test = "t.test", 
                    alpha = .05)

summary(pupil_tb_analysis_adult_dom_within)

## Within-sentence Adult non-dominant language

pupil_tb_analysis_adult_non_within <- response_time_pupil %>%
  filter(age.group == "Adults") %>%
  filter(Carrier == "Non-Dominant") %>%
  filter(switch.type == "Within-sentence") %>%
  analyze_time_bins(data = ., 
                    predictor_column = "trial.type", 
                    test = "t.test", 
                    alpha = .05)

summary(pupil_tb_analysis_adult_non_within)


#---- Across-sentence

## Across-sentence 20-month-olds dominant language.

pupil_tb_analysis_20_dom_across <- response_time_pupil %>%
  filter(age.group == "20-month-olds") %>%
  filter(Carrier == "Dominant") %>%
  filter(switch.type == "Across-sentence") %>%
  analyze_time_bins(data = ., 
                    predictor_column = "trial.type", 
                    test = "t.test", 
                    alpha = .05)

summary(pupil_tb_analysis_20_dom_across)


## Across-sentence 20-month-olds non-dominant language

pupil_tb_analysis_20_non_across <- response_time_pupil %>%
  filter(age.group == "20-month-olds") %>%
  filter(Carrier == "Non-Dominant") %>%
  filter(switch.type == "Across-sentence") %>%
  analyze_time_bins(data = ., 
                    predictor_column = "trial.type", 
                    test = "t.test", 
                    alpha = .05)

summary(pupil_tb_analysis_20_non_across)


## Across-sentence Adult dominant language

pupil_tb_analysis_adult_dom_across <- response_time_pupil %>%
  filter(age.group == "Adults") %>%
  filter(Carrier == "Dominant") %>%
  filter(switch.type == "Across-sentence") %>%
  analyze_time_bins(data = ., 
                    predictor_column = "trial.type", 
                    test = "t.test", 
                    alpha = .05)

summary(pupil_tb_analysis_adult_dom_across)

## Across-sentence Adult non-dominant language

pupil_tb_analysis_adult_non_across <- response_time_pupil %>%
  filter(age.group == "Adults") %>%
  filter(Carrier == "Non-Dominant") %>%
  filter(switch.type == "Across-sentence") %>%
  analyze_time_bins(data = ., 
                    predictor_column = "trial.type", 
                    test = "t.test", 
                    alpha = .05)

summary(pupil_tb_analysis_adult_non_across)

#---------------------------------------------------------------------------------------
### SI analyses: Infant correlations with accuracy


## Look at correlations with switch cost irrespective of Carrier (switching direction)

# Create dataset for correlations
switch.cost <- response_window_agg_by_sub %>%
  filter(age.group == "20-month-olds") %>%
  select(id, age.group, switch.type, trial.type, per.dom, per.nondom, lang.mix, Prop) %>%
  spread(trial.type, Prop) %>%
  mutate(sw.cost = same - switch) %>%
  group_by(switch.type)

# Correlate switch cost and percent exposure to non-dominant language (balance) by switch type
switch.cost%>%  
  do(broom::tidy(cor.test(.$sw.cost, .$per.nondom)))

# Correlate switch cost and language mixing by mix type
switch.cost%>%
  do(broom::tidy(cor.test(.$sw.cost, .$lang.mix)))

## Look at correlations with switch cost as a function of Carrier (switching direction)

# Create dataset for correlations
switch.cost.carrier <- response_window_agg_by_sub_carrier %>%
  filter(age.group == "20-month-olds") %>%
  select(id, age.group, switch.type, trial.type, Carrier, per.dom, per.nondom, lang.mix, Prop) %>%
  spread(trial.type, Prop) %>%
  mutate(sw.cost = same - switch) %>%
  group_by(switch.type, Carrier)

# Correlate switch cost and percent exposure to non-dominant language (balance) by switch type and Carrier
switch.cost.carrier %>%
  do(broom::tidy(cor.test(.$sw.cost, .$per.nondom)))

# Correlate switch cost and language mixing by mix type and carrier
switch.cost.carrier %>%
  do(broom::tidy(cor.test(.$sw.cost, .$lang.mix)))
