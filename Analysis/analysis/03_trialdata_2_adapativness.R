# This script uses mixed-models to analyze trial data.
# The applied model tests the moderation of initial strategy adaptiveness. 
# One can either choose to analyse the learning phase (3-7) or the performance phase (7-21).
# 

source("00_format_data.R")

# pick learning or performance phase
performance_phase <- TRUE

# 0 Models -----------------------------------------------------------------
if(performance_phase) N <- 7:21 else N <- 3:7
if(performance_phase) phase <- 'performance_phase' else phase <- 'learning_phase'

model_formula <- "reflection + 
                  index + reflection:index +
                  ncs + ncs:reflection + 
                  ncs:index + ncs:index:reflection + 
                  cluster_adaptive_b + cluster_adaptive_b:reflection + 
                  cluster_adaptive_b:index + cluster_adaptive_b:index:reflection + 
                  cluster_maladaptive_b + cluster_maladaptive_b:reflection + 
                  cluster_maladaptive_b:index + cluster_maladaptive_b:index:reflection +
                  (1|pid)"

model_formula_simplifed <- "reflection + 
                  index + reflection:index +
                  cluster_adaptive_b + cluster_adaptive_b:reflection + 
                  cluster_adaptive_b:index + cluster_adaptive_b:index:reflection + 
                  cluster_maladaptive_b + cluster_maladaptive_b:reflection + 
                  cluster_maladaptive_b:index + cluster_maladaptive_b:index:reflection +
                  (1|pid)"

model_path <- paste('03_trialdata_adaptiveness/', phase, '/', sep = '')


# 1 Data Preparation -----------------------------------------------------------------

# subset
df <- subset(trial_frame, index %in% N)

# center/scale
df$index <- scale(df$index, scale = scale_variables)
df$ncs <- scale(df$ncs, scale = scale_variables)

df$cluster_adaptive_b <- scale(df$adaptive_baseline, scale = scale_variables)
df$cluster_maladaptive_b <- scale(df$maladaptive_baseline, scale = scale_variables)

# 2 Score -----------------------------------------------------------------

# fit model
model <- fitModel_lm('score', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_score', max(N)),  sprintf('07-trialdata  score (%i )',max(N)),
           'lm')

# 3 Strategy Score --------------------------------------------------------

# fit model
model <- fitModel_lm('strategyscore', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_strategyscore', max(N)),  sprintf('07-trialdata  strategyscore (%s)', phase),
           'lm')


# 4 Strategy Cluster -----------------------------------------------------------------

# ------ adaptive cluster
# model
model <- fitModel_glm('adaptive', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_adaptive', max(N)),  sprintf('07-trialdata  adaptive (%i )',max(N)),
           'glm')


# ------  moderately adaptive cluster
# model
model <- fitModel_glm('moderate', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_moderate', max(N)),  sprintf('07-trialdata  moderate (%i )',max(N)),
           'glm')


# ------  maladaptive cluster
if(performance_phase){
  
  # this model was not identifiable when running on different machines
  
  # model <- fitModel_glm('maladaptive', model_formula_simplifed, df)
  
  # store model
  #storeModel(model, model_path, sprintf('%i_maladaptive', max(N)),  sprintf('07-trialdata  maladaptive (%i )',max(N)),
  #           'glm')
  
} else {
  # no convergence even with simplified models
}


# 5 Benjamini-Hochberg Correction ----------------------------------------

model_names <- c('score', 'strategyscore', 'adaptive', 'moderate')
applyBHCorection(model_names, '07-trialdata')


# 6 Number of clicks -----------------------------------------------------------

# Model
model <- fitModel_lm('number_clicks', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_clicks', max(N)),  sprintf('07-trialdata  number clicks (%i )',max(N)),
           'lm', compile = TRUE)

