# This script analysis how the effect of reflection evolves over time.
# It analyses the first three reflection periods (3-6, 6-9, 9-12) separately. 

source("00_format_data.R")

# 1 Data Preparation -----------------------------------------------------------------
model_path <- '05_transitiondata_temporal/'
N <- 3:11

# subset
df <- subset(transition_frame, transition %in% N)

# center/scale/cast
df$transition_scaled <- scale(df$transition, scale=scale_variables)
df$ncs <- scale(df$ncs, scale = scale_variables)

# Cumulate the change per reflection period
df$transition_period <- floor(df$transition/3)

# 2 Amount of improvement over time ----------------------------------------------------------------

# Check for interaction
model <- fitModel_lm('strategyscore_delta', 'ncs * reflection * transition_scaled + (1|pid)', df)
storeModel(model, model_path, 'strategyscore_delta',  sprintf('09-temporal-dynamics  strategyscore-delta'), 'lm', 
           compile = T)

model <- fitModel_lm('cluster_change_delta', 'ncs * reflection * transition_scaled + (1|pid)', df)
storeModel(model, model_path, 'cluster_change_delta',  sprintf('09-temporal-dynamics  cluster-change-delta'), 'lm',
           compile = T)

# compare for each reflection period seperately
df_s <- aggregate(strategyscore_delta ~ pid + reflection + transition_period, df, mean)
df_c <- aggregate(cluster_change_delta ~ pid + reflection + transition_period, df, mean)
for(trans in unique(df$transition_period)){
  print('-----------------------')
  print(trans)
  df_s2 <- subset(df_s, transition_period == trans)
  df_c2 <- subset(df_c, transition_period == trans)
  
  t_c <- wilcox.test(cluster_change_delta ~ reflection, df_c2)
  print(t_c)
  
  t_s <- wilcox.test(strategyscore_delta ~ reflection, df_s2)
  print(t_s)
  
  print('Corrected:')
  print(p.adjust(c(t_c$p.value, t_s$p.value), "BH"))
}


# 3 Amount of change over time ----------------------------------------------------------------

model <- fitModel_glm('strategy_change', 'ncs + reflection * transition_scaled + (1|pid)', df)
summary(model)
storeModel(model, model_path, 'strategy_change',  sprintf('09-temporal-dynamics  strategy-change'), 'glm',
            compile = T)

# Compare for transitions separately
for(trans in unique(df$transition_period)){
  print('-----------------------')
  print(trans)
  df_2 <- subset(df, transition_period == trans)
  
  t_c <- chisq.test(table(df_2$cluster_change, df_2$reflection))
  #print(t_c)
  
  t_s <- chisq.test(table(df_2$strategy_change, df_2$reflection))
  print(t_s)
  
  print('Corrected:')
  print(p.adjust(c(t_c$p.value, t_s$p.value), "BH"))
}

