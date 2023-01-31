performance_phase <- TRUE
if(performance_phase) N <- 7:21 else N <- 3:7
if(performance_phase) phase <- 'performance_phase' else phase <- 'learning_phase'


# Function ----------------------------------------------------------------

createTrialTable <- function(){
  
  string <- ' '
  for(i in 1:length(coefs)){
    string <- paste(string, '\n\n &')
    
    coef <- coefs[i]
    coef_name <- coefs_names[i]
    string <- paste(string, coef_name)
    
    for(model in models){
      string <- paste(string, '\n')
      
      # full model
      if(coef %in% row.names(summary(model)$coefficients)){
        cof <- summary(model)$coefficients[coef,]
        
        # extract pvalue
        pval <- model@optinfo$pvalcorrected[[coef]]
        if(is.null(pval)){
          pval <- cof[5]
        }
        
        # add estimate
        estimate <- round(cof[1], 2)
        if(pval <= 0.05){
          addon <- sprintf("& \\textbf{%s}", estimate)
        } else {
          addon <- sprintf("& %s", estimate)
        }
        string <- paste(string, addon)
        
        # add pvalue
        if(pval < 0.001){
          pval <- '<.001'
        } else {
          pval <-  substring(round(pval, digits = 3), 2) 
        }
        addon <- sprintf(" \t& %s", pval)
        string <- paste(string, addon)
        
      } else {
        # simplieifed model
        string <- paste(string, '& - & -')
        
      }
      
    }
    string <- paste(string, ' \\\\')
  }
  
  # store
  new_file <- file('../table.txt')
  writeLines(string, new_file)
  close(new_file)
  
  
  
  
  
}


createTransitionTable <- function(){
  
  string <- ' '
  for(i in 1:length(coefs)){
    string <- paste(string, '\n\n')
    
    coef <- coefs[i]
    coef_name <- coefs_names[i]
    string <- paste(string, coef_name)
    
    for(model in models){
      string <- paste(string, '\n')
      
      # full model
      if(coef %in% row.names(summary(model)$coefficients)){
        cof <- summary(model)$coefficients[coef,]
        
        # extract pvalue
        pval <- model@optinfo$pvalcorrected[[coef]]
        if(is.null(pval)){
          pval <- cof[5]
        }
        
        # add estimate
        estimate <- round(cof[1], 2)
        if(pval <= 0.05){
          addon <- sprintf("& \\textbf{%s}", estimate)
        } else {
          addon <- sprintf("& %s", estimate)
        }
        string <- paste(string, addon)
        
        # add pvalue
        if(pval < 0.001){
          pval <- '<.001'
        } else {
          pval <-  substring(round(pval, digits = 3), 2) 
        }
        addon <- sprintf(" \t& %s", pval)
        string <- paste(string, addon)
        
      } else {
        # simplieifed model
        string <- paste(string, '& - & -')
        
      }
      
    }
    string <- paste(string, ' \\\\')
  }
  
  # store
  new_file <- file('../table.txt')
  writeLines(string, new_file)
  close(new_file)

}

# Trialdata -----------------------------------------------------------------
model_path <- paste('03_trialdata_baselinevalue/', phase, '/', sep = '')

# Load Models
model_names <- c('strategyscore_corrected', 'score_corrected', 'clicks')
models <- c()
for(model_name in model_names){
  load(sprintf('../results/models/%s%i_%s.RData', model_path, max(N), model_name))
  models <- c(models, model)
}

# Create Table


# obtain coefficients
coefs <- row.names(summary(models[[1]])$coefficients)

# manually order and name coeeficients
coefs <- c("reflectionTRUE", "baseline", "index", "ncs", 
           "reflectionTRUE:baseline" , "reflectionTRUE:ncs" , "baseline:ncs", 
           "reflectionTRUE:index", "baseline:index",  "index:ncs" ,
           "reflectionTRUE:baseline:index", "reflectionTRUE:index:ncs" , "baseline:index:ncs")

coefs_names <- c("Reflection", "Baseline", "Trial Nr", "NFC", 
           "Reflection $\\times$ Baseline" , "Reflection  $\\times$ NFC" , "Baseline  $\\times$ NFC", 
           "Trial Nr  $\\times$ Reflection", "Trial Nr  $\\times$ Baseline",  "Trial Nr  $\\times$ NFC" ,
           "Trial Nr  $\\times$ Reflection  $\\times$Baseline", "Trial Nr  $\\times$ Reflection  $\\times$ NFC" ,
           "Trial Nr  $\\times$ Baseline  $\\times$ NFC")


createTrialTable()

# Trialdata For Strategy Type usage -----------------------------------------------------------------
model_path <- paste('03_trialdata_baselinevalue/', phase, '/', sep = '')

# Load Models
model_names <- c('typenp_corrected', 'typens_corrected', 'typefs_corrected', 'typeother_corrected')
models <- c()
for(model_name in model_names){
  load(sprintf('../results/models/%s%i_%s.RData', model_path, max(N), model_name))
  models <- c(models, model)
}

# Create Table

# obtain coefficients
row.names(summary(models[[1]])$coefficients)

# manually order and name coeeficients
coefs <- c("reflectionTRUE", "baseline", "index", "ncs", 
           "reflectionTRUE:baseline" , "reflectionTRUE:ncs" , "baseline:ncs", 
           "reflectionTRUE:index", "baseline:index",  "index:ncs" ,
           "reflectionTRUE:baseline:index", "reflectionTRUE:index:ncs" , "baseline:index:ncs")

coefs_names <- c("Reflection", "Baseline", "Trial Nr", "NFC", 
                 "Reflection $\\times$ Baseline" , "Reflection  $\\times$ NFC" , "Baseline  $\\times$ NFC", 
                 "Trial Nr  $\\times$ Reflection", "Trial Nr  $\\times$ Baseline",  "Trial Nr  $\\times$ NFC" ,
                 "Trial Nr  $\\times$ Reflection  $\\times$Baseline", "Trial Nr  $\\times$ Reflection  $\\times$ NFC" ,
                 "Trial Nr  $\\times$ Baseline  $\\times$ NFC")


createTrialTable()

# Trialdata Type -----------------------------------------------------------------

# -- -- config
model_path <- paste('03_trialdata_type/', phase, '/', sep = '')

# Load Models
model_names <- c('strategyscore_corrected', 'score_corrected', 'clicks')
models <- c()
for(model_name in model_names){
  load(sprintf('../results/models/%s%i_%s.RData', model_path, max(N), model_name))
  models <- c(models, model)
}


# -- -- Create Table 
# obtain coefficients
row.names(summary(models[[1]])$coefficients)

# manually order and name coefficients
coefs <- c("reflectionTRUE", "index", "ncs", "type_other_b" , "type_ns_b", "type_np_b",
           "reflectionTRUE:ncs", "reflectionTRUE:type_other_b", "reflectionTRUE:type_ns_b", "reflectionTRUE:type_np_b",
           "reflectionTRUE:index", "index:ncs", "index:type_other_b" , "index:type_ns_b", "index:type_np_b",
           "reflectionTRUE:index:ncs", "reflectionTRUE:index:type_other_b", "reflectionTRUE:index:type_ns_b", "reflectionTRUE:index:type_np_b")


coefs_names <- c("Reflection", "Trial Nr", "NFC", "Other baseline", "Near-sighted baseline", "No-planning baseline",
                 "Reflection  $\\times$ NFC", "Reflection  $\\times$ Other baseline", "Reflection $\\times$ Near-sighted baseline", "Reflection $\\times$ No-planning baseline",
                 "Trial Nr  $\\times$ Reflection", "Trial Nr  $\\times$ NFC", "Trial Nr  $\\times$ Other baseline", "Trial Nr  $\\times$ Near-sighted baseline", "Trial Nr  $\\times$ No-planning baseline",
                 "Trial Nr  $\\times$ Reflection  $\\times$ NFC", "Trial Nr  $\\times$ Reflection  $\\times$ Other baseline", "Trial Nr  $\\times$ Reflection  $\\times$ Near-sighted baseline", "Trial Nr  $\\times$ Reflection  $\\times$ No-planning baseline")


createTrialTable()

# Trialdata Selfevaluation Focus -----------------------------------------------------------------

model_path <- paste('03_trialdata_selfevaluation_focus/', phase, '/', sep = '')

# Load Models
model_names <- c('strategyscore_corrected', 'score_corrected', 'clicks')
models <- c()
for(model_name in model_names){
  load(sprintf('../results/models/%s%i_%s.RData', model_path, max(N), model_name))
  models <- c(models, model)
}

# Create Table

# obtain coefficients
coefs <- row.names(summary(models[[1]])$coefficients)

# manually order and name coeeficients
coefs <- c("reflectionTRUE", "baseline", "index", "ncs", 
           "reflectionTRUE:baseline" , "reflectionTRUE:ncs" ,"reflectionTRUE:sea", 
           "baseline:ncs", 
           "reflectionTRUE:index", "baseline:index",  "index:ncs" ,
           "reflectionTRUE:baseline:index", "reflectionTRUE:index:ncs" , 
           "reflectionTRUE:baseline:sea",   "reflectionTRUE:index:sea",
           "baseline:index:ncs")

coefs_names <- c("Reflection", "Baseline", "Trial Nr", "NFC", 
                 "Reflection $\\times$ Baseline" , "Reflection  $\\times$ NFC" , "Reflection $\\times$ SEF", 
                 "Baseline  $\\times$ NFC", 
                 "Trial Nr  $\\times$ Reflection", "Trial Nr  $\\times$ Baseline",  "Trial Nr  $\\times$ NFC" ,
                 "Trial Nr  $\\times$ Reflection  $\\times$Baseline", "Trial Nr  $\\times$ Reflection  $\\times$ NFC" ,
                 "Baseline  $\\times$ Reflection  $\\times$ SEF", "Trial Nr  $\\times$ Reflection  $\\times$ SEF", 
                 "Trial Nr  $\\times$ Baseline  $\\times$ NFC")


createTrialTable()

# Trialdata Selfevaluation Accuracy -----------------------------------------------------------------

model_path <- paste('03_trialdata_selfevaluation_accuracy/', phase, '/', sep = '')

# Load Models
model_names <- c('strategyscore_corrected', 'score_corrected', 'clicks')
models <- c()
for(model_name in model_names){
  load(sprintf('../results/models/%s%i_%s.RData', model_path, max(N), model_name))
  models <- c(models, model)
}

# Create Table
# obtain coefficients
coefs <- row.names(summary(models[[1]])$coefficients)

# manually order and name coeeficients
coefs <- c("reflectionTRUE", "baseline", "index", "ncs", 
           "reflectionTRUE:baseline" , "reflectionTRUE:ncs" ,"reflectionTRUE:sea", 
           "baseline:ncs", 
           "reflectionTRUE:index", "baseline:index",  "index:ncs" ,
           "reflectionTRUE:baseline:index", "reflectionTRUE:index:ncs" , 
           "reflectionTRUE:baseline:sea",   "reflectionTRUE:index:sea",
           "baseline:index:ncs")

coefs_names <- c("Reflection", "Baseline", "Trial Nr", "NFC", 
                 "Reflection $\\times$ Baseline" , "Reflection  $\\times$ NFC" , "Reflection $\\times$ SEA", 
                 "Baseline  $\\times$ NFC", 
                 "Trial Nr  $\\times$ Reflection", "Trial Nr  $\\times$ Baseline",  "Trial Nr  $\\times$ NFC" ,
                 "Trial Nr  $\\times$ Reflection  $\\times$Baseline", "Trial Nr  $\\times$ Reflection  $\\times$ NFC" ,
                 "Baseline  $\\times$ Reflection  $\\times$ SEA", "Trial Nr  $\\times$ Reflection  $\\times$ SEA", 
                 "Trial Nr  $\\times$ Baseline  $\\times$ NFC")

createTrialTable()

# Transitiondata -----------------------------------------------------------------

# -- -- config
model_path <- '04_transitiondata_previousvalue/'

# Load Models
model_names <- c('strategy-change', 'type-change', 'clicks-change',
                  'strategy-magnitude', 'clicks-magnitude')
models <- c()
for(model_name in model_names){
  load(sprintf('../results/models/%s%i_%s_corrected.RData', model_path, max(N), model_name))
  models <- c(models, model)
}


# -- -- Create Table 
# obtain coefficients
row.names(summary(models[[1]])$coefficients)

# manually order and name coefficients
coefs <- c("reflectionTRUE", "promptTRUE", "ncs", "prevscore" , 
           "promptTRUE:ncs", "promptTRUE:prevscore", 
           "reflectionTRUE:ncs", "reflectionTRUE:promptTRUE", "reflectionTRUE:prevscore",
           "reflectionTRUE:promptTRUE:ncs", "reflectionTRUE:promptTRUE:prevscore")


coefs_names <- c("Reflection", "Prompt", "NFC", "Previous value",
                 "Prompt  $\\times$ NFC", "Prompt  $\\times$ Previous value", 
                 "Reflection $\\times$ NCS", "Reflection $\\times$ Prompt ", "Reflection  $\\times$ Previous value", 
                 "Reflection $\\times$ Prompt $\\times$ NFC", "Reflection $\\times$ Prompt $\\times$ Previous value")
createTransitionTable()




# Transitiondata Score -----------------------------------------------------------------

# -- -- config
model_path <- '04_transitiondata_previousscore/'

# Load Models
model_names <- c('strategy-change', 'type-change', 'clicks-change',
                 'strategy-magnitude', 'clicks-magnitude')
models <- c()
for(model_name in model_names){
  load(sprintf('../results/models/%s%i_%s_corrected.RData', model_path, max(N), model_name))
  models <- c(models, model)
}


# -- -- Create Table 
# obtain coefficients
row.names(summary(models[[1]])$coefficients)

# manually order and name coefficients
coefs <- c("reflectionTRUE", "promptTRUE", "ncs", "prevscore" , 
           "promptTRUE:ncs", "promptTRUE:prevscore", 
           "reflectionTRUE:ncs", "reflectionTRUE:promptTRUE", "reflectionTRUE:prevscore",
           "reflectionTRUE:promptTRUE:ncs", "reflectionTRUE:promptTRUE:prevscore")


coefs_names <- c("Reflection", "Prompt", "NFC", "Previous score",
                 "Prompt  $\\times$ NFC", "Prompt  $\\times$ Previous score", 
                 "Reflection $\\times$ NCS", "Reflection $\\times$ Prompt ", "Reflection  $\\times$ Previous score", 
                 "Reflection $\\times$ Prompt $\\times$ NFC", "Reflection $\\times$ Prompt $\\times$ Previous score")
createTransitionTable()




# Transitiondata Type -----------------------------------------------------------------

# -- -- config
model_path <- '04_transitiondata_type/'

# Load Models
model_names <- c('strategy-change', 'type-change', 'clicks-change',
                 'strategy-magnitude', 'clicks-magnitude')
models <- c()
for(model_name in model_names){
  load(sprintf('../results/models/%s%i_%s_corrected.RData', model_path, max(N), model_name))
  models <- c(models, model)
}


# -- -- Create Table 
# obtain coefficients
row.names(summary(models[[1]])$coefficients)

# manually order and name coefficients
coefs <- c("reflectionTRUE", "promptTRUE", "ncs", "prev_type_otherTRUE", "prev_type_nsTRUE", "prev_type_npTRUE",
           "promptTRUE:ncs", "promptTRUE:prev_type_otherTRUE", "promptTRUE:prev_type_nsTRUE", "promptTRUE:prev_type_npTRUE",
           "reflectionTRUE:ncs", "reflectionTRUE:promptTRUE", 
           "reflectionTRUE:prev_type_otherTRUE", "reflectionTRUE:prev_type_nsTRUE", "reflectionTRUE:prev_type_npTRUE",
           "reflectionTRUE:promptTRUE:ncs", "reflectionTRUE:promptTRUE:prev_type_otherTRUE", "reflectionTRUE:promptTRUE:prev_type_nsTRUE", "reflectionTRUE:promptTRUE:prev_type_npTRUE")


coefs_names <- c("Reflection", "Prompt", "NFC", "Previous other", "Previous near-sighted", "Previous no-planning",
                 "Prompt  $\\times$ NFC", "Prompt  $\\times$ Previous other", "Prompt  $\\times$ Previous near-sighted", "Prompt  $\\times$ Previous no-planning", 
                 "Reflection $\\times$ NCS", "Reflection $\\times$ Prompt ",
                 "Reflection  $\\times$ Previous other", "Reflection  $\\times$ Previous near-sighted", "Reflection  $\\times$ Previous no-planning",
                 "Reflection $\\times$ Prompt $\\times$ NFC", 
                 "Reflection $\\times$Prompt  $\\times$ Previous other", "Reflection $\\times$ Prompt $\\times$ Previous near-sighted", "Reflection $\\times$ Prompt $\\times$ Previous no-planning") 

createTransitionTable()




# Transitiondata Engagement -----------------------------------------------------------------

# -- -- config
model_path <- '06_transitiondata_engagement/'

# Load Models
model_names <- c('strategy-change', 'type-change', 'clicks-change',
                 'strategy-magnitude', 'clicks-magnitude')
models <- c()
for(model_name in model_names){
  load(sprintf('../results/models/%s%i_%s_corrected.RData', model_path, max(N), model_name))
  models <- c(models, model)
}


# -- -- Create Table 
# obtain coefficients
row.names(summary(models[[1]])$coefficients)

# manually order and name coefficients
coefs <- c("prevscore",  "ncs" , "HE", "prevscore:ncs", 
           "prevscore:HE", "prevscore:HE:ncs")

coefs_names <- c("Previous value", "NFC", "High engagement", 
                 "Previous value $\\times$ NFC ", "Previous value $\\times$ High engegament ", 
                 "Previous value $\\times$ High engegament $\\times$ NFC")
createTransitionTable()



