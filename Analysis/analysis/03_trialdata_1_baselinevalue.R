# This script uses mixed-models to analyze trial data.
# The applied model corrects for the initial baseline of the corresponding dv. 
# One can either choose to analyse the learning phase (3-7) or the performance phase (7-21)


source("00_format_data.R")

# pick learning or performance phase
performance_phase <- FALSE

# 0 Models -----------------------------------------------------------------
if(performance_phase) N <- 7:21 else N <- 3:7
if(performance_phase) phase <- 'performance_phase' else phase <- 'learning_phase'

model_formula <- "reflection + baseline + reflection:baseline + 
                  index + reflection:index + baseline:index + reflection:baseline:index + 
                  ncs + ncs:reflection + baseline:ncs + 
                  ncs:index + ncs:index:reflection + baseline:ncs:index +
                  (1|pid)"

model_formula_simplified <- "reflection + baseline + reflection:baseline + 
                            index + reflection:index + baseline:index + reflection:baseline:index + 
                            ncs + ncs:reflection + baseline:ncs +
                            ncs:index +
                            (1|pid)"

model_path <- paste('03_trialdata_baselinevalue/', phase, '/', sep = '')


# 1 Data Preparation -----------------------------------------------------------------

# subset
df <- subset(trial_frame, index %in% N)

# center/scale
df$index <- scale(df$index, scale = scale_variables)
df$ncs <- scale(df$ncs, scale = scale_variables)

# 2 Score -----------------------------------------------------------------

# fit model
df$baseline <- scale(df$strategyscore_baseline, scale = scale_variables) + 1
model <- fitModel_lm('score', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_score', max(N)),  sprintf('07-trialdata  score (%i )',max(N)),
           'lm')

# 3 Strategy Score --------------------------------------------------------

# fit model
df$baseline <- scale(df$strategyscore_baseline, scale = scale_variables) + 1
model <- fitModel_lm('strategyscore', model_formula, df)

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
           'lm')

# 7 Strategy Type -----------------------------------------------------------------

# ------ far sighted type
# model
df$baseline <- scale(df$type_fs_baseline, scale = scale_variables)
model <- fitModel_glm('type_fs', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_typefs', max(N)),  sprintf('07-trialdata far-sighted (%i )',max(N)),
           'glm')

# ------ other type
# model
df$baseline <- scale(df$type_other_baseline, scale = scale_variables)
model <- fitModel_glm('type_other', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_typeother', max(N)),  sprintf('07-trialdata other (%i )',max(N)),
           'glm')


# ------ near sighted type
# model
df$baseline <- scale(df$type_ns_baseline, scale = scale_variables)

if(performance_phase){
  model <- fitModel_glm('type_ns', model_formula, df)
  
} else {
  # react to model convergence fail
  model <- fitModel_glm('type_ns', model_formula_simplified, df)
}
# store model
storeModel(model, model_path, sprintf('%i_typens', max(N)),  sprintf('07-trialdata near-sighted (%i )',max(N)),
           'glm')

# ------ no-planning type
# model
df$baseline <- scale(df$type_np_baseline, scale = scale_variables)
model <- fitModel_glm('type_np', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_typenp', max(N)),  sprintf('07-trialdata np-planning (%i )',max(N)),
           'glm')

# ------ Correction
model_names <- c('typens', 'typefs', 'typeother', 'typenp')
applyBHCorection(model_names, '07-trialdata')


# Johnson Neyman ----------------------------------------------------------
if(performance_phase){
  
  model_names <- c('score', 'strategyscore', 'clicks', 'adaptive', 'moderate', 'maladaptive', 'typenp', 'typefs')
  print('The johnson neyman intervals for main effect of reflection translated into percentiles of the baseline distribution:')
  for(model_name in model_names){
    load(sprintf('../results/models/%s%i_%s.RData', model_path, 21, model_name))
    j <- johnson_neyman(model = model, pred = reflectionTRUE,  modx = baseline)
    percentile <- ecdf(model@frame$baseline)
    j2 <- round(percentile(j$bounds),2)
    
    print('')
    print(model_name)
    print(j$bounds)
    print(j2)
  }
}

# Strategy Type follow-up -----------------------------------------------------------------

model_path <- paste('03_trialdata_baselinevalue/', phase, '/followup/', sep = '')

# evaluate all with the no-planning baseline
df$baseline <- scale(df$type_np_baseline, scale = scale_variables) - 1 


# ------ far sighted type
# model
model <- fitModel_glm('type_fs', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_typefs', max(N)),  sprintf('07-trialdata far-sighted (%i )',max(N)),
           'glm')


# - - - - Johnson Neyman
if(performance_phase){
  model_names <- c('typefs')
  print('The johnson neyman intervals for main effect of reflection translated into percentiles of the baseline distribution:')
  for(model_name in model_names){
    load(sprintf('../results/models/%s%i_%s.RData', model_path, 21, model_name))
    j <- johnson_neyman(model = model, pred = reflectionTRUE,  modx = baseline)
    percentile <- ecdf(model@frame$baseline)
    j <- round(percentile(j$bounds),2)
    
    print('')
    print(model_name)
    print(j)
  }
}

