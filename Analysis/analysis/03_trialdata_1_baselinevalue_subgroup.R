# This script uses mixed-models to analyze trial data.
# The applied model corrects for the initial baseline of the corresponding dv. 
# One can either choose to analyse the learning phase (3-7) or the performance phase (7-21)


source("00_format_data.R")

# pick learning or performance phase
performance_phase <- TRUE

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

model_path <- paste('03_trialdata_baselinevalue_subgroup/', phase, '/', sep = '')


# 1 Data Preparation -----------------------------------------------------------------

# subset
df <- subset(trial_frame, index %in% N)
df <- subset(df, df$type_np_all == 0)

# center/scale
df$index <- scale(df$index, scale = scale_variables)
df$ncs <- scale(df$ncs, scale = scale_variables)

table(df$reflection)/length(N)

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

# 5 Benjamini-Hochberg Correction ----------------------------------------

model_names <- c('score', 'strategyscore')
applyBHCorection(model_names, '07-trialdata')


# 6 Number of clicks -----------------------------------------------------------

# Model
df$baseline <- scale(df$clicks_baseline, scale = scale_variables) + 1
model <- fitModel_lm('number_clicks', model_formula, df)

# store model
storeModel(model, model_path, sprintf('%i_clicks', max(N)),  sprintf('07-trialdata  number clicks (%i )',max(N)),
           'lm', compile = TRUE)


# Johnson Neyman ----------------------------------------------------------
if(performance_phase){
  
  model_names <- c('score', 'strategyscore', 'clicks')
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