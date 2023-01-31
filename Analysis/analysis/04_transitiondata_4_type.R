# This script uses mixed-models to analyze transition data.
# The applied model tests the moderation of the previous strategy type on the dv. 
# 

source("00_format_data.R")

# 0 Models -----------------------------------------------------------------
N <- 4:21

model_formula <- "reflection + 
                  prompt + prompt:reflection +
                  ncs + ncs:reflection + 
                  ncs:prompt + ncs:prompt:reflection + 
                  prev_type_other + prev_type_other:reflection + 
                  prev_type_other:prompt + prev_type_other:prompt:reflection + 
                  prev_type_np + prev_type_np:reflection + 
                  prev_type_np:prompt + prev_type_np:prompt:reflection +
                  prev_type_ns + prev_type_ns:reflection + 
                  prev_type_ns:prompt + prev_type_ns:prompt:reflection +
                  (1|pid)"

model_path <- '04_transitiondata_type/'

# 1 Data Preparation -----------------------------------------------------------------

# subset 
df <- subset(transition_frame, transition %in% (N-1))

# center/scale/cast
df$transition <- scale(df$transition, scale=scale_variables)
df$ncs <- scale(df$ncs, scale = scale_variables)
df$prompt <- factor(df$prompt)


# 2 Change of planning (binary) -----------------------------------------------------------------

# -- -- -- Adaptiveness change
# fit model
model <- fitModel_glm('cluster_change', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_cluster-change', max(N)),  sprintf('08-transitiondata  cluster-change (%i trials)',max(N)), 
           'glm')


# -- -- -- Type change
# fit model
model <- fitModel_glm('type_change', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_type-change', max(N)),  sprintf('08-transitiondata  type-change (%i trials)',max(N)),
           'glm')


# -- -- -- Strategy change
# fit model
model <- fitModel_glm('strategy_change', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_strategy-change', max(N)),  sprintf('08-transitiondata  strategy-change (%i trials)',max(N)),
           'glm')


# 3 Magnitude of Change (continuous)  -----------------------------------------------------------------

# -- -- -- Adaptiveness change
# fit model
model <- fitModel_lm('cluster_change_delta', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_cluster-magnitude', max(N)),  sprintf('08-transitiondata  cluster-magnitude (%i trials)',max(N)),
           'lm')


# -- -- -- Strategy change
# fit model
model <- fitModel_lm('strategyscore_delta', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_strategy-magnitude', max(N)),  sprintf('08-transitiondata  strategy-magnitude (%i trials)',max(N)),
           'lm')


# 4 Amount of planning  -----------------------------------------------------------------

# -- -- -- Clicks change
# fit model
model <- fitModel_glm('clicks_change', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_clicks-change', max(N)),  sprintf('08-transitiondata  clicks-change (%i trials)',max(N)),
           'glm')


# -- -- -- Clicks magnitude
# fit model
model <- fitModel_lm('clicks_change_delta', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_clicks-magnitude', max(N)),  sprintf('08-transitiondata  clicks-magnitude (%i trials)',max(N)),
           'lm')


# 9 Benjamini-Hochberg Correction ----------------------------------------

applyBHCorection(c('cluster-change', 'type-change', 'strategy-change'), '08-transitiondata')
applyBHCorection(c('cluster-magnitude', 'strategy-magnitude'), '08-transitiondata')
applyBHCorection(c('clicks-change', 'clicks-magnitude'), '08-transitiondata')
