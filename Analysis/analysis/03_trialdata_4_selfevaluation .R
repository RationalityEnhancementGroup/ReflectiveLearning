# This script tests if the effect of reflection is moderated by accurate self-evaluation.
# 
#

source("00_format_data.R")

# pick learning or performance phase
performance_phase <- TRUE
sea_focus <- FALSE
sea_accuracy <- !sea_focus

# 0 Models -----------------------------------------------------------------
if(performance_phase) N <- 7:21 else N <- 3:7
if(performance_phase) phase <- 'performance_phase' else phase <- 'learning_phase'


model_formula <- "reflection + baseline + reflection:baseline + 
                  index + reflection:index + baseline:index + reflection:baseline:index + 
                  ncs + ncs:reflection + baseline:ncs + 
                  ncs:index + ncs:index:reflection + baseline:ncs:index +
                  reflection:sea +
                  reflection:baseline:sea + 
                  reflection:index:sea + 
                  (1|pid)"
                  
model_formula_simplified <- "reflection + baseline + reflection:baseline + 
                            index + reflection:index + baseline:index + reflection:baseline:index + 
                            ncs + ncs:reflection + baseline:ncs +
                            ncs:index +
                            (1|pid)"

if(sea_focus){
  model_path <- paste('03_trialdata_selfevaluation_focus/', phase, '/', sep = '')
} else {
  model_path <- paste('03_trialdata_selfevaluation_accuracy/', phase, '/', sep = '')
}



# 1 Data Preparation -----------------------------------------------------------------

# description
df <- subset(trial_frame, reflection & index == 10)
hist(df$luck)
hist(df$skill)
hist(df$sea_lm)

# 2. Final sea chosen
if(sea_focus){
  trial_frame$sea <- trial_frame$sea_lm
} else {
  trial_frame$sea <- trial_frame$sea_cor
}
df <- subset(trial_frame, !is.na(trial_frame$sea))

# subset
df <- subset(df, index %in% N)

# center/scale
df$index <- scale(df$index, scale = scale_variables)
df$ncs <- scale(df$ncs, scale = scale_variables)
df$sea <- scale(df$sea, scale = scale_variables)


# 2 Score -----------------------------------------------------------------

# fit model
df$baseline <- scale(df$strategyscore_baseline, scale = scale_variables) + 1
model <- fitModel_lm('score', model_formula, df)
summary(model)

# store model
storeModel(model, model_path, sprintf('%i_score', max(N)),  sprintf('07-trialdata  score (%i )',max(N)),
           'lm')

# 3 Strategy Score --------------------------------------------------------

# fit model
df$baseline <- scale(df$strategyscore_baseline, scale = scale_variables) + 1
model <- fitModel_lm('strategyscore', model_formula, df)
summary(model)

# store model
storeModel(model, model_path, sprintf('%i_strategyscore', max(N)),  sprintf('07-trialdata  strategyscore (%s)', phase),
           'lm')


# 4 Strategy Cluster -----------------------------------------------------------------

# ------ adaptive cluster
# model
df$baseline <- scale(df$strategyscore_baseline, scale = scale_variables) + 1
model <- fitModel_glm('adaptive', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_adaptive', max(N)),  sprintf('07-trialdata  adaptive (%i )',max(N)),
           'glm')


# ------  moderately adaptive cluster
# model
df$baseline <- scale(df$moderate_baseline, scale = scale_variables) + 1
model <- fitModel_glm('moderate', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_moderate', max(N)),  sprintf('07-trialdata  moderate (%i )',max(N)),
           'glm')


# ------  maladaptive cluster
# model
df$baseline <- scale(df$strategyscore_baseline, scale = scale_variables) + 1

if(performance_phase){
  model <- fitModel_glm('maladaptive', model_formula, df)
  
} else {
  # react to model convergence fail
  model <- fitModel_glm('maladaptive', model_formula_simplified, df)
}

# store model
storeModel(model, model_path, sprintf('%i_maladaptive', max(N)),  sprintf('07-trialdata  maladaptive (%i )',max(N)),
           'glm')


# 5 Benjamini-Hochberg Correction ----------------------------------------

model_names <- c('score', 'strategyscore', 'adaptive', 'moderate', 'maladaptive')
applyBHCorection(model_names, '07-trialdata')


# 6 Number of clicks -----------------------------------------------------------

# Model
df$baseline <- scale(df$clicks_baseline, scale = scale_variables) + 1
model <- fitModel_lm('number_clicks', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_clicks', max(N)),  sprintf('07-trialdata  number clicks (%i )',max(N)),
           'lm', compile = T)




