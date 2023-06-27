suppressMessages({
  library(tidyverse)
  library(moments)
  library(EnvStats)
  library(blme)
  library(BayesianFirstAid)
  library(knitr)
  library(MuMIn)
  library(performance)
  library(ggsignif)
})

setwd(choose.dir())

# SE function
se <- function (x) {
  se = sd(x, na.rm = TRUE)/sqrt(length(x))
  return(se)
}

#### LOAD DATA ####

# Load data and run feature engineering with theta set to 1

filepath <- './data/data_full.csv'
data <- read.csv(filepath)

theta_fun <- function() {
  return(1)
}

source("./feature_engineering.R", local = knit_global())

# Save engineered data
write.csv(data, file = './data/data_reduced.csv', row.names = FALSE)

set.seed(41)

filepath <- './data/data_reduced.csv'
data <- read.csv(filepath)


#### DESCRIPTIVE STATS ####

# Basic descriptive statistics and some tables

# Number of participants
n <- length(unique(data$id))

# Age distribution
age_by_n <- data %>%
  group_by(id) %>%
  summarize(age = mean(age)) %>%
  ungroup %>%
  filter(age != 0)

# Gender distribution
gender_by_n <- data %>%
  group_by(id) %>%
  summarize(gender = mean(gender)) %>%
  ungroup

# Major distribution
major_by_n <- data %>%
  group_by(id) %>%
  summarize(major = mean(major)) %>%
  ungroup

mean_age <- round(mean(age_by_n$age), 2)
min_age <- min(age_by_n$age)
max_age <- max(age_by_n$age)

n_gender_1 <- sum(gender_by_n$gender == 1)
n_gender_2 <- sum(gender_by_n$gender == 2)
n_gender_3 <- sum(gender_by_n$gender == 3)

n_major_1 <- sum(major_by_n$major == 1)
n_major_2 <- sum(major_by_n$major == 2)
n_major_3 <- sum(major_by_n$major == 3)
n_major_4 <- sum(major_by_n$major == 4)


## Descriptive stats tables

# Accuracy in questions - table S1
summarize_mcq <- data %>%
  filter(MCQ_Q4 != 3) %>%
  group_by(id, craver_2) %>%
  summarize(MCQ1 = mean(MCQ_Q1),
            MCQ2 = mean(MCQ_Q2),
            MCQ3 = mean(MCQ_Q3),
            MCQ4 = mean(MCQ_Q4),
            MCQ5 = mean(MCQ_Q5),
            MCQ6 = mean(MCQ_Q6)) %>%
  ungroup()

mcq_optimal <- summarize_mcq %>%
  filter(craver_2 == 0) %>%
  select(starts_with('MCQ')) %>%
  apply(MARGIN = 2, FUN = mean) %>%
  mean() %>%
  round(2)
mcq_opt_out <- paste0(mcq_optimal*100, '%')

mcq_craver <- summarize_mcq %>%
  filter(craver_2 == 1) %>%
  select(starts_with('MCQ')) %>%
  apply(MARGIN = 2, FUN = mean) %>%
  mean() %>%
  round(2)
mcq_cra_out <- paste0(mcq_craver*100, '%')

mcq_total <- summarize_mcq %>%
  select(starts_with('MCQ')) %>%
  apply(MARGIN = 2, FUN = mean) %>%
  mean() %>%
  round(2)
mcq_tot_out <- paste0(mcq_total*100, '%')

post_game_quiz <- data %>%
  filter(craver_2 == 1) %>%
  group_by(id, treatment) %>%
  summarize(post_game = mean(post_game_quiz_correct)) %>%
  ungroup %>%
  filter(!is.na(post_game)) %>%
  select(post_game) %>%
  as.matrix() %>%
  mean() %>%
  round(2)
post_game_quiz_out <- paste0(post_game_quiz*100, '%')

total_opt <- paste0(mean(mcq_optimal, post_game_quiz)*100, '%')
total_cra <- paste0(mean(mcq_craver, post_game_quiz)*100, '%')
total_tot <- paste0(mean(mcq_total, post_game_quiz)*100, '%')

table_s1 <- data.frame(participant_type = c('Optimal', 'Cravers', 'Total'),
                       MCQ = c(mcq_opt_out, mcq_cra_out, mcq_tot_out),
                       post_game_quiz = c('-', post_game_quiz_out, post_game_quiz_out),
                       Total = c(total_opt, total_cra, total_tot))
colnames(table_s1) <- c('Participant type', 'MCQ', 'Post-game quiz', 'Total')

# pre game strategy - table S2
table_s2 <- data %>%
  group_by(id) %>%
  summarize(pre_game = mean(pre_game_strategy)) %>%
  ungroup() %>%
  count(pre_game) %>%
  select(n) %>%
  t %>%
  as.data.frame()
colnames(table_s2) <- c('No strategy', 'Not quite sure', 'Quite confident', 'Think it is right')

# Fraction of cravers in experiment - table S3
cravers_test <- data %>%
  filter(treatment == "test") %>%
  group_by(id) %>%
  slice_head(n = 1) %>%
  ungroup %>%
  summarize(bet_once_in_yellow = mean(craver),
            bet_twice_in_yellow = mean(craver_2)) %>%
  t
n_test <- data %>%
  filter(treatment == "test") %>%
  group_by(id) %>%
  select(id) %>%
  unique %>%
  ungroup %>%
  nrow
cravers_test_frac <- paste0(cravers_test*n_test, rep('/', 2), n_test, ' = ',
                            paste0(round(cravers_test, 2)*100, '%'))

cravers_control <- data %>%
  filter(treatment == "control") %>%
  group_by(id) %>%
  slice_head(n = 1) %>%
  ungroup %>%
  summarize(bet_once_in_yellow = mean(craver),
            bet_twice_in_yellow = mean(craver_2)) %>%
  t
n_control <- data %>%
  filter(treatment == "control") %>%
  group_by(id) %>%
  select(id) %>%
  unique %>%
  ungroup %>%
  nrow
cravers_ctrl_frac <- paste0(cravers_control*n_control, rep('/', 2), n_control, ' = ',
                            paste0(round(cravers_control, 2)*100, '%'))

cravers_total <- data %>%
  group_by(id) %>%
  slice_head(n = 1) %>%
  ungroup %>%
  summarize(bet_once_in_yellow = mean(craver),
            bet_twice_in_yellow = mean(craver_2)) %>%
  t
cravers_total_frac <- paste0(cravers_total*n, rep('/', 2), n, ' = ',
                             paste0(round(cravers_total, 2)*100, '%'))

table_s3 <- data.frame(craving_definition = c('Bet in yellow', 'At least twice in yellow'),
                       Test = cravers_test_frac,
                       Control = cravers_ctrl_frac,
                       Total = cravers_total_frac)
colnames(table_s3) <- c('Craving definition', 'Test', 'Control', 'Total')

# Betting rate in blue
betting_stats_blue_c <- data %>%
  filter(block_type == "S" & treatment == 'control') %>%
  group_by(id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  psych::describe()

betting_stats_blue_c_sub <- betting_stats_blue_c['betting_rate',
                                                 c('n', 'mean', 'median', 'sd',
                                                   'min', 'max', 'skew')] %>%
  round(3) %>%
  t
colnames(betting_stats_blue_c_sub) <- 'Control'

betting_stats_blue_t <- data %>%
  filter(block_type == "S" & treatment == 'test') %>%
  group_by(id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  psych::describe()

betting_stats_blue_t_sub <- betting_stats_blue_t['betting_rate',
                                                 c('n', 'mean', 'median', 'sd',
                                                   'min', 'max', 'skew')] %>%
  round(3) %>%
  t
colnames(betting_stats_blue_t_sub) <- 'Test'

betting_stats_blue <- data %>%
  filter(block_type == "S") %>%
  group_by(id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  psych::describe()

betting_stats_blue_sub <- betting_stats_blue['betting_rate',
                                             c('n', 'mean', 'median', 'sd',
                                               'min', 'max', 'skew')] %>%
  round(3) %>%
  t
colnames(betting_stats_blue_sub) <- 'Total'

# Betting rate in yellow
betting_stats_yellow_c <- data %>%
  filter(block_type == "C" & treatment == 'control') %>%
  group_by(id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  psych::describe()

betting_stats_yellow_c_sub <- betting_stats_yellow_c['betting_rate',
                                                     c('n', 'mean', 'median', 'sd',
                                                       'min', 'max', 'skew')] %>%
  round(3) %>%
  t
colnames(betting_stats_yellow_c_sub) <- 'Control'

betting_stats_yellow_t <- data %>%
  filter(block_type == "C" & treatment == 'test') %>%
  group_by(id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  psych::describe()

betting_stats_yellow_t_sub <- betting_stats_yellow_t['betting_rate',
                                                     c('n', 'mean', 'median', 'sd',
                                                       'min', 'max', 'skew')] %>%
  round(3) %>%
  t
colnames(betting_stats_yellow_t_sub) <- 'Test'

betting_stats_yellow <- data %>%
  filter(block_type == "C") %>%
  group_by(id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  psych::describe()

betting_stats_yellow_sub <- betting_stats_yellow['betting_rate',
                                                 c('n', 'mean', 'median', 'sd',
                                                   'min', 'max', 'skew')] %>%
  round(3) %>%
  t
colnames(betting_stats_yellow_sub) <- 'Total'

top_row <- c('', 'Blue', '', '', 'Yellow', '')

table_s4 <- betting_stats_blue_c_sub %>%
  cbind(betting_stats_blue_t_sub, betting_stats_blue_sub, betting_stats_yellow_c_sub,
        betting_stats_yellow_t_sub, betting_stats_yellow_sub) %>%
  as.data.frame()

cols_treat <- colnames(table_s4)

table_s4 <- cols_treat %>%
  rbind(table_s4)

colnames(table_s4) <- c('', 'Blue', '', '', 'Yellow', '')
rownames(table_s4) <- c('', 'N', 'Mean', 'Median', 'SD', 'Min', 'Max', 'Skew')
