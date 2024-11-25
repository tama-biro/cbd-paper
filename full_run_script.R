suppressMessages({
  library(tidyverse)
  library(moments)
  library(EnvStats)
  library(blme)
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
                                                 c('mean', 'median', 'sd',
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
                                                 c('mean', 'median', 'sd',
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
                                             c('mean', 'median', 'sd',
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
                                                     c('mean', 'median', 'sd',
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
                                                     c('mean', 'median', 'sd',
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
                                                 c('mean', 'median', 'sd',
                                                   'min', 'max', 'skew')] %>%
  round(3) %>%
  t
colnames(betting_stats_yellow_sub) <- 'Total'

table_s4 <- betting_stats_blue_c_sub %>%
  cbind(betting_stats_blue_t_sub, betting_stats_blue_sub, betting_stats_yellow_c_sub,
        betting_stats_yellow_t_sub, betting_stats_yellow_sub) %>%
  as.data.frame()

cols_treat <- colnames(table_s4)

table_s4 <- cols_treat %>%
  rbind(table_s4)

colnames(table_s4) <- c('', 'Blue', '', '', 'Yellow', '')
rownames(table_s4) <- c('', 'Mean', 'Median', 'SD', 'Min', 'Max', 'Skew')


#### Results ####

#### Test 1 ####

# Paired one-tailed t-test, participant level
# Betting rate in low/high reward
data_1 <- data %>%
  mutate(reward_value = factor(reward_value, levels = c("Low", "High"),
                               labels = c("Low", "High"))) %>%
  filter(block_type == "C") %>%
  group_by(id, reward_value) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup


# Check skew and normality assumption
skew_1_bf <- round(skewness(sqrt(data_1$betting_rate)), 3)
shap_1_bf <- shapiro.test(data_1$betting_rate)


# Box-Cox transform
out <- boxcox(data_1$betting_rate + 1, lambda = seq(-25, 25, by = 0.25))
bc_1_lambda <- out$lambda[which.max(out$objective)]

data_1$betting_rate_bc <- boxcoxTransform(data_1$betting_rate + 1,
                                          lambda = bc_1_lambda)

skew_1_af <- round(skewness(sqrt(data_1$betting_rate_bc)), 3)
shap_1_af <- shapiro.test(data_1$betting_rate_bc)


# Run test
t_test_1 <- t.test(betting_rate_bc ~ reward_value, data = data_1,
                   alternative = 'less', paired = TRUE)

effect_size_t_1 <- (
  (mean(data_1$betting_rate_bc[data_1$reward_value == 'High']) - mean(data_1$betting_rate_bc[data_1$reward_value == 'Low']))/
    sqrt(
      (sd(data_1$betting_rate_bc[data_1$reward_value == 'High'])^2 + sd(data_1$betting_rate_bc[data_1$reward_value == 'Low'])^2)/2
    )
)
effect_size_t_1 <- round(effect_size_t_1, 3)

# Non-parametric
wilcox_1 <- wilcox.test(betting_rate ~ reward_value, data = data_1,
                        alternative = 'less', paired = TRUE)

#### Test 3 ####

# Logistic model to predict betting in any trial

data_3 <- data %>%
  mutate(reward_value = factor(reward_value, levels = c("Low", "High"),
                               labels = c("Low", "High")),
         uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                              labels = c("Low", "High")),
         treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test")),
         color = factor(block_type, levels = c("S", "C"),
                        labels = c("Blue", "Yellow")),
         age = scale(age),
         gender = factor(gender),
         major = factor(major),
         reaction_time = scale(reaction_time),
         sequence_number = scale(sequence_number),
         reward_exposure = scale(exposure_time))

log_mod_3 <- bglmer(choice ~ reward_value + uncertainty +
                      treatment * color + previous_choice + reward_exposure +
                      age + gender + major + sequence_number + reaction_time +
                      (1 | id), fixef.prior = t, data = data_3,
                    family = binomial(link = "logit"))

summary(log_mod_3)

# Nakagawa R^2
r2_nakagawa(log_mod_3)$R2_conditional

# Number of obs
summary(log_mod_3)$devcomp$dims[1]

# Multicollinearity
data_3_vif <- data_3[,c('reward_value', 'uncertainty', 'treatment',
                        'color', 'age', 'gender', 'major', 'reaction_time',
                        'sequence_number', 'previous_choice',
                        'reward_exposure')] %>%
  mutate(reward_value = as.numeric(reward_value),
         uncertainty = as.numeric(uncertainty),
         treatment = as.numeric(treatment),
         gender = as.numeric(gender),
         major = as.numeric(major),
         color = as.numeric(color))
vif_mod_3_out <- usdm::vif(data_3_vif)

# Stepwise model building
log_mod_3_1 <- bglmer(choice ~ reward_value + uncertainty +
                        treatment * color + age + gender + major + (1 | id),
                      fixef.prior = t, data = data_3,
                      family = binomial(link = "logit"))

log_mod_3_2 <- bglmer(choice ~ reward_value + uncertainty +
                        treatment * color + age + gender + major +
                        sequence_number + (1 | id),
                      fixef.prior = t, data = data_3,
                      family = binomial(link = "logit"))

log_mod_3_3 <- bglmer(choice ~ reward_value + uncertainty +
                        treatment * color + age + gender + major +
                        sequence_number + reaction_time + (1 | id),
                      fixef.prior = t, data = data_3,
                      family = binomial(link = "logit"))

log_mod_3_4 <- bglmer(choice ~ reward_value + uncertainty +
                        treatment * color + age + gender + major +
                        sequence_number + reaction_time + previous_choice +
                        (1 | id), fixef.prior = t, data = data_3,
                      family = binomial(link = "logit"))

log_mod_3_5 <- bglmer(choice ~ reward_value + uncertainty +
                        treatment * color + age + gender + major +
                        sequence_number + reaction_time + previous_choice +
                        reward_exposure + (1 | id),
                      fixef.prior = t, data = data_3,
                      family = binomial(link = "logit"))

s_3_pcr <- c('R^2',
             round(r2_nakagawa(log_mod_3_1)$R2_conditional, 3),
             round(r2_nakagawa(log_mod_3_2)$R2_conditional, 3),
             round(r2_nakagawa(log_mod_3_3)$R2_conditional, 3),
             round(r2_nakagawa(log_mod_3_4)$R2_conditional, 3),
             round(r2_nakagawa(log_mod_3_5)$R2_conditional, 3))
s_3_pcn <- c('N',
             summary(log_mod_3_1)$devcomp$dims[1],
             summary(log_mod_3_2)$devcomp$dims[1],
             summary(log_mod_3_3)$devcomp$dims[1],
             summary(log_mod_3_4)$devcomp$dims[1],
             summary(log_mod_3_5)$devcomp$dims[1])

s_3_pcr

s_3_pcn


#### Test 5 ####

data_5 <- data %>%
  filter(block_type == "C" & craver_2 == 1) %>%
  mutate(reward_value = factor(reward_value, levels = c("Low", "High"),
                               labels = c("Low", "High")),
         uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                              labels = c("Low", "High")),
         treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test")),
         age = scale(age),
         gender = factor(gender),
         major = factor(major),
         reward_exposure = scale(exposure_time),
         reaction_time = scale(reaction_time),
         sequence_number = scale(sequence_number))

log_mod_5 <- bglmer(choice ~ reward_value + uncertainty * treatment +
                      age + gender + major + sequence_number +
                      previous_choice + reward_exposure + reaction_time +
                      (1 | id), data = data_5, fixef.prior = t,
                    family = binomial(link = "logit"))

summary(log_mod_5)

# Nakagawa R^2
r2_nakagawa(log_mod_5)$R2_conditional

# Number of obs
summary(log_mod_5)$devcomp$dims[1]

# Multicollinearity
data_5_vif <- data_5[,c('reward_value', 'uncertainty', 'treatment',
                        'age', 'gender', 'major',
                        'sequence_number', 'previous_choice',
                        'reward_exposure')] %>%
  mutate(reward_value = as.numeric(reward_value),
         uncertainty = as.numeric(uncertainty),
         treatment = as.numeric(treatment),
         gender = as.numeric(gender),
         major = as.numeric(major))
vif_mod_5_out <- usdm::vif(data_5_vif)

# Stepwise model building
log_mod_5_1 <- bglmer(choice ~ reward_value + uncertainty * treatment +
                        age + gender + major + sequence_number + (1 | id),
                      data = data_5, fixef.prior = t,
                      family = binomial(link = "logit"))

log_mod_5_2 <- bglmer(choice ~ reward_value + uncertainty * treatment +
                        age + gender + major + sequence_number + reaction_time +
                        (1 | id), data = data_5, fixef.prior = t,
                      family = binomial(link = "logit"))

log_mod_5_3 <- bglmer(choice ~ reward_value + uncertainty * treatment +
                        age + gender + major + sequence_number + reaction_time +
                        previous_choice + (1 | id),
                      data = data_5, fixef.prior = t,
                      family = binomial(link = "logit"))

log_mod_5_4 <- bglmer(choice ~ reward_value + uncertainty * treatment +
                        age + gender + major + sequence_number + reaction_time +
                        previous_choice + reward_exposure + (1 | id),
                      data = data_5, fixef.prior = t,
                      family = binomial(link = "logit"))

s_5_pcr <- c('R^2',
             round(r2_nakagawa(log_mod_5_1)$R2_conditional, 3),
             round(r2_nakagawa(log_mod_5_2)$R2_conditional, 3),
             round(r2_nakagawa(log_mod_5_3)$R2_conditional, 3),
             round(r2_nakagawa(log_mod_5_4)$R2_conditional, 3))
s_5_pcn <- c('N',
             summary(log_mod_5_1)$devcomp$dims[1],
             summary(log_mod_5_2)$devcomp$dims[1],
             summary(log_mod_5_3)$devcomp$dims[1],
             summary(log_mod_5_4)$devcomp$dims[1])

s_5_pcr
s_5_pcn


#### Test 6 ####

# Paired one-tailed t-test, participant level
# Betting rate in low/high uncertainty
data_6 <- data %>%
  mutate(uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                              labels = c("Low", "High"))) %>%
  filter(block_type == "C") %>%
  group_by(id, uncertainty) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup

# Check skew and normality assumption
skew_6_bf <- round(skewness(sqrt(data_6$betting_rate)), 3)
shap_6_bf <- shapiro.test(data_6$betting_rate)


# Box-Cox transform
out <- boxcox(data_6$betting_rate + 1, lambda = seq(-25, 25, by = 0.25))
bc_6_lambda <- out$lambda[which.max(out$objective)]

data_6$betting_rate_bc <- boxcoxTransform(data_6$betting_rate + 1,
                                          lambda = bc_6_lambda)

skew_6_af <- round(skewness(sqrt(data_6$betting_rate_bc)), 3)
shap_6_af <- shapiro.test(data_6$betting_rate_bc)


# Run test
t_test_6 <- t.test(x = data_6$betting_rate_bc[data_6$uncertainty == "Low"],
                   y = data_6$betting_rate_bc[data_6$uncertainty == "High"],
                   alternative = 'less',
                   paired = TRUE)

cohens_d(x = data_6$betting_rate_bc[data_6$uncertainty == "Low"],
         y = data_6$betting_rate_bc[data_6$uncertainty == "High"],
         alternative = 'less',
         paired = TRUE)

# Non-parametric
wilcox_6 <- wilcox.test(x = data_6$betting_rate[data_6$uncertainty == "Low"],
                        y = data_6$betting_rate[data_6$uncertainty == "High"],
                        alternative = 'less',
                        paired = TRUE)

effectsize(wilcox_6)

data_plot_u <- data %>%
  filter(block_type == "C") %>%
  group_by(aaron_mood, id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup

out <- boxcox(data_plot_u$betting_rate + 1, lambda = seq(-25, 25, by = 0.25))
bc_plot6_lambda <- out$lambda[which.max(out$objective)]

data_plot_u$betting_rate_bc <- boxcoxTransform(data_plot_u$betting_rate + 1,
                                               lambda = bc_plot6_lambda)

# Plot test 6
data_plot_u <- data_plot_u %>%
  group_by(aaron_mood) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate)) %>%
  ungroup %>%
  mutate(aaron_mood = factor(aaron_mood, levels = c('Low', 'High'),
                             labels = c('Low', 'High')))

# Test for uncertainty
p_unc <- t_test_6$p.value
p_unc <- if_else(p_unc < .001, '***',
                 if_else(p_unc < .01, '**',
                         if_else(p_unc < .05, '*', 'n.s.')))

ggplot(data_plot_u, aes(x = aaron_mood, y = betting_rate)) +
  geom_bar(stat='identity', position = position_dodge(.9), fill = '#ffd700',
           width = 0.6) +
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se),
                position = position_dodge(.9), width = .1) +
  scale_x_discrete(name = '', breaks = c('Low', 'High'),
                   labels = c('Low uncertainty', 'High uncertainty')) +
  scale_y_continuous(name = 'Betting rate') +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_signif(comparisons = list(c('Low', 'High')),
              map_signif_level = TRUE,
              annotations = c(p_unc),
              margin_top = 0.1,
              size = .8,
              textsize = 6)

ggsave('./plots/betting_rate_by_unc.png', width = 4, height = 7)


#### Test 7 ####

# T-test in yellow and test
data_7_1 <- data %>%
  filter(treatment == "test" & block_type == "C") %>%
  group_by(id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup

n_7_1 <- nrow(data_7_1)

t_test_7_1 <- t.test(data_7_1$betting_rate, mu = 0, alternative = 'greater')

effect_size_t_7_1 <- mean(data_7_1$betting_rate)/sd(data_7_1$betting_rate)
effect_size_t_7_1 <- round(effect_size_t_7_1, 3)

# T-test C vs T in yellow
data_7_2 <- data %>%
  filter(block_type == "C") %>%
  group_by(id, treatment) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup

out <- boxcox(data_7_2$betting_rate + 1, lambda = seq(-25, 25, by = 0.25))
bc_72_lambda <- out$lambda[which.max(out$objective)]

data_7_2$betting_rate_bc <- boxcoxTransform(data_7_2$betting_rate + 1,
                                            lambda = bc_72_lambda)

t_test_7_2 <- t.test(betting_rate_bc ~ treatment,
                     data = data_7_2,
                     alternative='less')

# Set up params for Cohen's D
len_test <- length(data_7_2$betting_rate_bc[data_7_2$treatment == 'test'])
len_control <- length(data_7_2$betting_rate_bc[data_7_2$treatment == 'control'])

mean_test <- mean(data_7_2$betting_rate_bc[data_7_2$treatment == 'test'])
mean_control <- mean(data_7_2$betting_rate_bc[data_7_2$treatment == 'control'])

sd_test <- sd(data_7_2$betting_rate_bc[data_7_2$treatment == 'test'])
sd_control <- sd(data_7_2$betting_rate_bc[data_7_2$treatment == 'control'])


effect_size_t_7_2 <- (
  (mean_test - mean_control)/
    sqrt(
      ((len_test-1)*(sd_test^2)+(len_control-1)*(sd_control^2))/(len_test + len_control - 2)
    )
)
effect_size_t_7_2 <- round(effect_size_t_7_2, 3)

# Non-parametric
wilcox_7_2 <- wilcox.test(x = data_7_2$betting_rate[data_7_2$treatment == "test"],
                          y = data_7_2$betting_rate[data_7_2$treatment == "control"],
                          paired = FALSE,
                          alternative='less',
                          data = data_7_2)

effectsize(wilcox_7_2)

# Plot test 7 (2)
data_plot_t <- data_7_2 %>%
  group_by(treatment) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate)) %>%
  ungroup %>%
  mutate(treatment = factor(treatment, levels = c('control', 'test'),
                            labels = c('Control', 'Test')))

p_treat <- t_test_7_2$p.value
p_treat <- if_else(p_treat < .001, '***',
                   if_else(p_treat < .01, '**',
                           if_else(p_treat < .05, '*', 'n.s.')))

ggplot(data_plot_t, aes(x = treatment, y = betting_rate)) +
  geom_bar(stat='identity', position = position_dodge(.9), fill = '#ffd700',
           width = 0.6) +
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se),
                position = position_dodge(.9), width = .1) +
  scale_x_discrete(name = '', breaks = c('Control', 'Test')) +
  scale_y_continuous(name = 'Betting rate') +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_signif(comparisons = list(c('Control', 'Test')),
              map_signif_level = TRUE,
              annotations = c(p_unc),
              margin_top = 0.1,
              size = .8,
              textsize = 6)

ggsave('./plots/betting_rate_by_treat.png', width = 4, height = 7)


#### Distribution plot ####

data_dists <- data %>%
  group_by(id, treatment, block_type) %>%
  summarize(betting_rate = mean(choice, na.rm=TRUE)) %>%
  ungroup %>%
  mutate(block_type = factor(block_type, levels = c('S', 'C'),
                             labels = c('S', 'C')))

ggplot(data_dists, aes(x = treatment, y = betting_rate, fill = block_type)) +
  geom_boxplot() +
  scale_x_discrete(name = '',
                   breaks = c('control', 'test'),
                   labels = c('Control', 'Test')) +
  scale_y_continuous(name = 'Betting rate') +
  scale_fill_manual(name = 'Session color',
                    breaks = c('C', 'S'),
                    labels = c('Yellow', 'Blue'),
                    values = c('#ffd700', '#0057b7')) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0))) +
  facet_wrap('block_type', nrow = 1, scales = 'free_y') +
  theme(strip.text.x = element_blank(),
        panel.spacing = unit(15, "pt"))

ggsave('./plots/betting_rate_distribution.png', width = 7, height = 5)


#### Plot Reward Exposure ####

data_plot_y <- data %>%
  filter(block_type == 'C' & craver_2 == 1) %>%
  mutate(exp_bins = as.numeric(cut_interval(exposure_time, 3)),
         exp_num = cut_interval(exposure_time, 3)) %>%
  group_by(exp_bins, id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(exp_bins) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate, na.rm = TRUE)) %>%
  ungroup

data_plot_b <- data %>%
  filter(block_type == 'S' & craver_2 == 1) %>%
  mutate(exp_bins = as.numeric(cut_interval(exposure_time, 3)),
         exp_num = cut_interval(exposure_time, 3)) %>%
  group_by(exp_bins, id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  filter(betting_rate > .6) %>%
  group_by(exp_bins) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate, na.rm = TRUE)) %>%
  ungroup

data_plot <- data_plot_b %>%
  rbind(data_plot_y) %>%
  mutate(session_color = rep(c('Blue', 'Yellow'),
                             times = c(nrow(data_plot_b),
                                       nrow(data_plot_y))),
         session_color = factor(session_color,
                                levels = c('Blue', 'Yellow'),
                                labels = c("(A) Blue", "(B) Yellow")))

ggplot(data_plot, aes(x = exp_bins,
                      y = betting_rate,
                      col = session_color)) +
  geom_line() +
  geom_errorbar(aes(ymin = betting_rate - se,
                    ymax = betting_rate + se),
                width = 0.2) +
  labs(x = 'Previous reward exposure', y = 'Betting rate') +
  scale_x_continuous(breaks = 1:3) +
  scale_color_manual(name = 'Session color',
                     breaks = c('(A) Blue', '(B) Yellow'),
                     values = c('#0057b7', '#ffd700')) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.position = 'none') +
  facet_wrap('session_color', scales = 'free_y')

ggsave('./plots/betting_by_reward_exposure.png', width = 8, height = 5)

