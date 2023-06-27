library(tidyverse)

# Iterate through data and add exposure time and previous choice
# Initialize exposure time as 0
theta <- theta_fun()
e_time <- 0
data[, c('previous_choice', 'previous_trial_outcome', 'exposure_time')] <- NA
for (i in 1:nrow(data)) {
  
  # If exposure time is 0, assign zero
  # Else assign craving variable
  if (e_time == 0) {
    data$exposure_time[i] <- 0
  } else {
    # Previous outcomes
    out_hist <- if_else(data$outcome[(i-e_time):(i-1)] > 0, 1, 0)
    
    # data$exposure_time[i] <- sum((1:(e_time))*out_hist*0.9^((e_time-1):0))
    data$exposure_time[i] <- sum(out_hist*theta^((e_time-1):0))
  }
  
  # Reset if new sequence
  if(i != nrow(data)) {
    if (data$sequence_number[i+1] != data$sequence_number[i]) {
      e_time <- 0
    } else {
      e_time <- e_time + 1
    }
  }
  
  # Add previous choice
  if(i != 1) {
    if(data$id[i] == data$id[i-1]) {
      data$previous_choice[i] <- data$choice[i - 1]
      data$previous_trial_outcome[i] <- if_else(data$outcome[i-1] > 0, 1, 0)
    }
  }
}

#### Data processing ####

# Make block_type and treatment factors
data <- data %>%
  filter(!(block_type == "S" &
             block_number == 1)) %>%
  mutate(block_type = as.factor(block_type),
         treatment = as.factor(treatment),
         block_number = block_number - 1)


# Remove bets where trials are missed
# Recode reward and uncertainty variable, make gender and major factors
data <- data %>%
  filter(choice != 2 & previous_choice != 2) %>%
  mutate(aaron_mood = factor(aaron_mood, levels = 1:0,
                             labels = c('Low', 'High')),
         reward_value = factor(reward_value, levels = 1:2,
                               labels = c('Low', 'High')),
         gender = as.factor(gender),
         major = as.factor(major)) %>%
  group_by(id) %>%
  mutate(craver = if_else(sum(choice[block_type == 'C']) > 0, 1, 0),
         craver_2 = if_else(sum(choice[block_type == 'C']) > 1, 1, 0)) %>%
  ungroup

# Make indicator of where blue interspersed block is
data <- data %>%
  group_by(sequence_number, id) %>%
  mutate(blue_interspersed =
           if_else(block_type == 'S' & block_number == 2, 2,
                   if_else(block_type == 'S' & block_number == 3, 3, 
                           if_else(block_type == 'S' & block_number == 4, 4, 1)))) %>%
  ungroup %>%
  mutate(blue_interspersed = replace(blue_interspersed, blue_interspersed == 1, NA)) %>%
  group_by(sequence_number, id) %>%
  mutate(blue_interspersed = mean(blue_interspersed, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(blue_interspersed = if_else(treatment == 'test', 1, blue_interspersed))



