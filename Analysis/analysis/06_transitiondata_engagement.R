# This script tests if the effect of reflection is moderated by engagement.
# 
#

source("00_format_data.R")

# 0 Models -----------------------------------------------------------------
model_formula <- "prevscore + HE + ncs +
                  prevscore:HE + prevscore:ncs +
                  prevscore:HE:ncs + 
                  (1|pid)"

model_path <- '06_transitiondata_engagement/'

# 1 Data Preparation -----------------------------------------------------------------

# subset
N <- 1:21
df <- subset(transition_frame, rprompt)

# scale
df$ncs <- scale(df$ncs, scale=scale_variables)

# engagement coding
df$HE <- as.numeric(df$engagement == 2)
table(df$HE)

getDescriptive <- function(col, name){
  
  t <- table(df[,col], df$HE)
  print(round(prop.table(t, margin=2), 3))
}


# 2 Change of planning (binary) -----------------------------------------------------------------
# -- -- -- Adaptiveness change

# fit model
df$prevscore <- scale(df$prev_adaptive, scale=scale_variables)
model <- fitModel_glm('cluster_change', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_cluster-change', max(N)),  '10-engagment  cluster-change', 'glm')


# -- -- -- Type change
# fit model
df$prevscore <- scale(df$prev_strategyscore, scale=scale_variables)
model <- fitModel_glm('type_change', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_type-change', max(N)),  '10-engagment  type-change', 'glm')


# -- -- -- Strategy change
# fit model
df$prevscore <- scale(df$prev_strategyscore, scale=scale_variables)
model <- fitModel_glm('strategy_change', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_strategy-change', max(N)),  '10-engagment  strategy-change', 'glm')


# 3 Magnitude of change (continuous)  -----------------------------------------------------------------
# -- -- -- Adaptiveness change
# fit model
df$prevscore <- df$prev_cluster
model <- fitModel_lm('cluster_change_delta', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_cluster-magnitude', max(N)),  '10-engagment  cluster-magnitude', 'lm')


# -- -- -- Strategy change
# fit model
df$prevscore <- scale(df$prev_strategyscore, scale=scale_variables)
model <- fitModel_lm('strategyscore_delta', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_strategy-magnitude', max(N)),  '10-engagment  strategy-magnitude', 'lm')


# 4 Amount of planning -----------------------------------------------------------------
# -- -- -- Clicks change
# fit model
df$prevscore <- scale(df$prev_strategyscore, scale=scale_variables)
model <- fitModel_glm('clicks_change', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_clicks-change', max(N)),  '10-engagment  clicks-change', 'glm')


# -- -- -- Clicks Magnitude
# fit model
df$prevscore <- scale(df$prev_strategyscore, scale=scale_variables)
model <- fitModel_lm('clicks_change_delta', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_clicks-magnitude', max(N)),  '10-engagment  clicks-magnitude', 'lm')


# 5 Benjamini-Hochberg Correction ----------------------------------------

applyBHCorection(c('cluster-change', 'type-change', 'strategy-change'), '10-engagement')
applyBHCorection(c('cluster-magnitude', 'strategy-magnitude'), '10-engagement')
applyBHCorection(c('clicks-change', 'clicks-magnitude'), '10-engagement')


# Johnson Neyman ----------------------------------------------------------

print('The johnson neyman interval for the main effect of engagement translated into percentiles of the previous expected score distribution:')

load(sprintf('../results/models/06_transitiondata_engagement/21_strategy-magnitude.RData'))
j <- johnson_neyman(model = model, pred = HE,  modx = prevscore)
percentile <- ecdf(model@frame$prevscore)
round(percentile(j$bounds),2)
