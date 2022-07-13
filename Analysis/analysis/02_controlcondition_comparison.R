# This script compares the active (condition 1) and the passive control condition (condition 0). 
# 
#

source("00_format_data.R")
model_path <- '02_controlcomparison/'

# 1 Data preparation ------------------------------------------------------

# subset of only control conditions
df <- subset(trial_frame, condition != 2)

# center/scale
df$index <- scale(df$index, scale = scale_variables)
df$ncs <- scale(df$ncs, scale = scale_variables)

# 2 Score -----------------------------------------------------------------

# ANCOVA
df$baseline <- scale(df$strategyscore_baseline, scale = scale_variables)
model <- aov(score ~ baseline + ncs + condition,
             aggregate(score ~ pid + baseline + ncs + condition, df, FUN = mean))
summary(model)
save(model, file = paste('../results/models/', model_path, 'score',".RData", sep=''))


# 3 Strategy Score -----------------------------------------------------------------

# descriptive
aggregate(strategyscore ~ condition, df, mean)
aggregate(strategyscore ~ condition, df, sd)

# ANCOVA
df$baseline <- scale(df$strategyscore_baseline, scale = scale_variables)
model <- aov(strategyscore ~ baseline + ncs + condition,
             aggregate(strategyscore ~ pid + baseline + ncs + condition, df, FUN = mean))
summary(model)
save(model, file = paste('../results/models/', model_path, 'strategyscore',".RData", sep=''))

# 4 Adaptive Cluster -----------------------------------------------------------------

# descriptive
prop.table(table(df$adaptive, df$condition), margin=2)

# ANCOVA
df$baseline <- scale(df$strategyscore_baseline, scale = scale_variables)
model <- aov(adaptive ~ baseline + ncs + condition,
             aggregate(adaptive ~ pid + baseline + ncs + condition, df, FUN = mean))
summary(model)
save(model, file = paste('../results/models/', model_path, 'adaptive',".RData", sep=''))

# 5 Moderate Cluster -----------------------------------------------------------------

# descriptive
prop.table(table(df$moderate, df$condition), margin=2)

# ANCOVA
df$baseline <- scale(df$moderate_baseline, scale = scale_variables)
model <- aov(moderate ~ baseline + ncs + condition,
             aggregate(moderate ~ pid + baseline + ncs + condition, df, FUN = mean))
summary(model)
save(model, file = paste('../results/models/', model_path, 'moderate',".RData", sep=''))

# 6 Maldaptive Cluster -----------------------------------------------------------------

# descriptive
prop.table(table(df$maladaptive, df$condition), margin=2)

# ANCOVA
df$baseline <- scale(df$strategyscore_baseline, scale = scale_variables)
model <- aov(maladaptive ~ baseline + ncs + condition,
             aggregate(maladaptive ~ pid + baseline + ncs + condition, df, FUN = mean))
summary(model)
save(model, file = paste('../results/models/', model_path, 'maladaptive',".RData", sep=''))

# 7 Number of clicks -----------------------------------------------------------------

# descriptive
aggregate(number_clicks ~ condition, df, mean)
aggregate(number_clicks ~ condition, df, sd)

# ANCOVA
df$baseline <- scale(df$clicks_baseline, scale = scale_variables)
model <- aov(number_clicks ~ baseline + ncs + condition,
             aggregate(number_clicks ~ pid + baseline + ncs + condition, df, FUN = mean))
summary(model)
save(model, file = paste('../results/models/', model_path, 'number_clicks',".RData", sep=''))

