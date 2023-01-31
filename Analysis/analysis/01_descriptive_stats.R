# This script contains all the descriptive analysis. 
# The order fmatches the structure of the article.
#

source("00_format_data.R")

# Demographics -------------------------------------------------------------

# number of participants in the conditions
table(df_valid$conditionType)

# gender
sum(grepl('fe', df_valid[ df_valid$gender != '-',]$gender, ignore.case = TRUE)) # female?

#age
summary(strtoi(df_valid$age))
sd(strtoi(df_valid$age))

#time
aggregate(totalTime ~ conditionType, df_valid, mean)
summary(df_valid$totalTime)

# NFC correlations -------------------------------------------------------------

# only collect one datapoint per participant
df <- subset(trial_frame, index == 1)

# NFC and baseline
cor.test(df$ncs, df$clicks_baseline)
cor.test(df$ncs, df$strategyscore_baseline)

# NFC and engagement 
df <- subset(df_valid, reflection)
df$effort_num <- as.integer(df$effort == "A lot of effort (e.g. you have deeply considered the questions and answered in detail)")
cor.test(df$ncs, df$effort_num)


# Trial data: Comparison of control conditions  -------------------------------------------------------------

# descriptive
describeBy(trial_frame$score, trial_frame$condition)
describeBy(trial_frame$strategyscore, trial_frame$condition)
describeBy(trial_frame$adaptive, trial_frame$condition)
describeBy(trial_frame$moderate, trial_frame$condition)
describeBy(trial_frame$maladaptive, trial_frame$condition)

# Trial data: Overall performance per reflection -------------------------------------------------------------

# performance
describeBy(trial_frame$score, trial_frame$reflection)
describeBy(trial_frame$strategyscore, trial_frame$reflection)

# adaptivity
clusters

# strategy types
types

# strategies
strategies

# amount of planning
describeBy(trial_frame$number_clicks, trial_frame$reflection)


# baseline difference
df <- subset(trial_frame, index < 2)
t.test(strategyscore_baseline ~ reflection, df, var.equal = T)


# Trial data: Learning Rates ---------------------------------------------------------

# data of learning phase
df <- subset(trial_frame, index %in% c(3,7))

# Learning rates:
t <- aggregate(. ~ reflection + pid, df, diff)
describeBy(t$strategyscore/4, t$reflection)
describeBy(t$score/4, t$reflection)
describeBy(t$number_clicks/4, t$reflection)

# Learning rates:
t <- aggregate(. ~ index + reflection, df, mean)
aggregate(adaptive/4 ~ reflection, t, diff)
aggregate(maladaptive/4 ~ reflection, t, diff)
 

# Transition data: Frequency of planning change ---------------------------------------------------------

aggregate(cluster_change ~ reflection, aggregate(cluster_change ~ pid + reflection, transition_frame, sum), mean)
aggregate(type_change ~ reflection, aggregate(type_change ~ pid + reflection, transition_frame, sum), mean)
aggregate(strategy_change ~ reflection, aggregate(strategy_change ~ pid + reflection, transition_frame, sum), mean)
aggregate(clicks_change ~ reflection, aggregate(clicks_change ~ pid + reflection, transition_frame, sum), mean)


# Transition data: Average improvement per trial ---------------------------------------------------------

aggregate(cluster_change_delta ~ reflection, transition_frame, mean)
aggregate(strategyscore_delta ~ reflection, transition_frame, mean)
aggregate(clicks_change_delta ~ reflection, transition_frame, mean)

# Transition data: Reflection prompts vs. Normal transition ---------------------------------------------------------

# percentages of changes
df <- subset(transition_frame, reflection)
describeBy(df$strategy_change, df$rprompt, digits=3, mat = TRUE)
describeBy(df$cluster_change, df$rprompt, digits=3, mat = TRUE)
describeBy(df$type_change, df$rprompt, digits=3, mat = TRUE)
describeBy(df$clicks_change, df$rprompt, digits=3, mat = TRUE)
describeBy(df$clicks_change_delta, df$rprompt, digits=3, mat = TRUE)

# Engagement Scores: ------------------------------------------------------

# Self-reported engagement
prop.table(table(subset(df_valid, reflection)$effort))

# only reflection prompt trials
df <- subset(transition_frame, rprompt)
  
# engagement
round(prop.table(table(df$engagement)), 3)

# RT
summary(df$RT)
sd(df$RT)

# Length
summary(df$length)
sd(df$length)

# engagement over time
#cor.test(df_prompts$engagement, df_prompts$transition)
rmcorr(pid, transition, engagement, df_prompts)
aggregate(engagement ~ transition, df_prompts, FUN = mean)
round(prop.table(table(subset(df_prompts, transition < 10)$engagement)), 3)
round(prop.table(table(subset(df_prompts, transition > 10)$engagement)), 3)

# - - only control prompt trials
df <- subset(df_valid, condition == 1)

# RT
mean(unlist(readArrayColumn(df$promptsRt, 6)))
sd(unlist(readArrayColumn(df$promptsRt, 6)))

# Length
mean(unlist(readArrayColumn(df$promptsLength, 6)))
sd(unlist(readArrayColumn(df$promptsLength, 6)))


# Engagement: Frequency of planning change ---------------------------------------------------------

# High engagement vs. low/no engagemnt prompts
# only reflection prompt trials
df <- subset(transition_frame, rprompt)
df$HE <- as.numeric(df$engagement == 2)

# change
describeBy(df$strategy_change, df$HE, digits=3, mat = TRUE)
describeBy(df$cluster_change, df$HE, digits=3, mat = TRUE)
describeBy(df$strategy_change, df$HE, digits=3, mat = TRUE)

# magnitude of change
describeBy(df$cluster_change_delta, df$HE, digits=3, mat = TRUE)
describeBy(df$strategyscore_delta, df$HE, digits=3, mat = TRUE)
describeBy(df$clicks_change_delta, df$HE, digits=3, mat = TRUE)


# Temporal Dynamics: Comparison of reflection periods ---------------------------------------------------------

# Cumulate the change of the first three reflection periods
df <- subset(transition_frame, transition %in% 3:11)
df$transition <- floor(df$transition/3)

# cumulative magnitude of change
aggregate(strategyscore_delta ~ reflection + transition, df, mean)
aggregate(cluster_change_delta ~ reflection + transition, df, mean)

# cumulative frequency of change
aggregate(strategy_change~ reflection + transition, df, mean)
aggregate(cluster_change ~ reflection + transition, df, mean)



# Trial data: Difference between the learning and the performance phase -------------------------------------------------------------

# laod the models
load(sprintf('../results/models/03_trialdata_baselinevalue/performance_phase/21_strategyscore.RData'))
m1 <- model
load(sprintf('../results/models/03_trialdata_baselinevalue/learning_phase/7_strategyscore.RData'))
m2 <- model

# extract stats
# 7 = reflection:index
b1 <- summary(m1)$coefficients[7,1]
b2 <- summary(m2)$coefficients[7,1]
se1 <- summary(m1)$coefficients[7,2]
se2 <- summary(m2)$coefficients[7,2]

#  calculate test statistics
z = b2-b1/sqrt(se2**2 + se1**2)
p = min(pnorm(z, 0, 1, lower.tail = FALSE), pnorm(z, 0, 1, lower.tail = TRUE))*2 # two-tailed
print(z)
print(p)


# Compare adaptiveness and type -------------------------------------------
prop.table(table(subset(trial, cluster == 0)$type))
# --> the maladaptive cluster consists to 66% of near-sighted strategies and of 33% of others

prop.table(table(subset(strategies, cluster == 1)$type))
# --> the moderate cluster consists to 32% of other strategies and to 67% of far-sighted strategies

prop.table(table(subset(strategies, cluster == 2)$type))
# --> the adaptive cluster consists to 4% of other strategies and to 96% of far-sighted strategies



prop.table(table(subset(trial_frame, cluster == 0)$type))
# --> the moderate strategies that are used consist to 7% of other strategies and to 93% of near-sighted strategies

prop.table(table(subset(trial_frame, cluster == 1)$type))
# --> the moderate strategies that are used consist to 27% of other strategies and to 73% of far-sighted strategies

prop.table(table(subset(trial_frame, cluster == 2)$type))
# --> the adaptive strategies that are used consist to 1% of other strategies and to 99% of far-sighted strategies


View(subset(strategies, cluster == 0))
View(subset(strategies, type == 2))

View(subset(strategies, cluster == 1))
View(subset(strategies, type == 0)) 

View(subset(strategies, cluster == 2))
View(subset(strategies, type == 1)) 
View(subset(strategies, type == 3)) 

# Self-evaluation -------------------------------------------

# only reflection group
df <- subset(trial_frame, reflection & index == 10)

# stats
mean(df$sea_lm)
sd(df$sea_lm)
hist(df$sea_lm)

hist(df$sea_cor)
mean(df$sea_cor, na.rm=T)
sd(df$sea_cor, na.rm=T)


hist(df$skill)
plot(df$sea_cor[!is.na(df$sea_cor)], df$skill[!is.na(df$sea_cor)])
cor.test(df$sea_cor[!is.na(df$sea_cor)], df$skill[!is.na(df$sea_cor)])





