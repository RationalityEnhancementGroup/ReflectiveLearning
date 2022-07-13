# This script creates all relevant dataframes including the trialframe, which contains all 
# relevant data on how participants behaved in a trial (score, expected score ...)
# and the transitionframe which captures how these variables changed between trials.  
#
# The script also contains functions to fit the models, correct the obtained p-values with 
# the benjamini-hochberg correction, store the models and output the model results to pdf's.
#

# 0. Import Packages ---------------------------------------------------------
library(rcompanion)
library(psych)

# regression
library(lme4)
library(lmerTest)
library(interactions)

# output models to latex
library(stringr)
library(tools)
library(filesstrings)

# plots
library(cowplot)
library(dplyr)
library(ggplot2)
library(igraph)

# 1. Import Data and Exclusion --------------------------------------------------------------

# reset 
rm(list = ls())

# import
df_all <- read.csv('../data/experiment/dataframe.csv')

# early quitter
df_finished <- df_all[df_all$status != 6,]

# cast variables
df_finished$totalTime <- as.numeric(df_finished$totalTime)
df_finished$attemptsQuiz <- as.numeric(df_finished$attemptsQuiz)
df_finished$condition <- factor(df_finished$condition)
df_finished$pid <- factor(df_finished$pid)
df_finished$reflection <- df_finished$reflection == 'True'

# exclusion criteria
df_valid <- df_finished[(df_finished$attemptsQuiz <= 3) & (df_finished$naive == 'True'),]
df_valid$pid <- factor(df_valid$pid)

# ---- annotations
df_prompts <- read.csv('../data/experiment/dataframe_prompts.csv')
df_prompts$transition <- df_prompts$index

# ---- read strategy information
strategies <- read.csv('../data/strategies/strategies_clustered.csv')

remove(df_all, df_finished)

# 2. Decode NCS--------------------------------------------------------------

readArrayColumn <- function(col, ncol){
  res<- data.frame(matrix(ncol=ncol,nrow=0 ))
  for (el in col) {
    res[nrow(res) + 1,] = scan(text=substr(el, 2, nchar(el)-1), sep=',', quiet = T)
  }
  res
}


# NCS
# https://www.midss.org/sites/default/files/ncogscale.pdf
ncs_temp <- readArrayColumn(df_valid$ncs, 18)

# inverted scores
ncs_temp$X3 <- 6 - ncs_temp$X3
ncs_temp$X5 <- 6 - ncs_temp$X5
ncs_temp$X7 <- 6 - ncs_temp$X7
ncs_temp$X8 <- 6 - ncs_temp$X8
ncs_temp$X9 <- 6 - ncs_temp$X9
ncs_temp$X12 <- 6 - ncs_temp$X12
ncs_temp$X16 <- 6 - ncs_temp$X16
ncs_temp$X17 <- 6 - ncs_temp$X17

# sum up scores
df_valid$ncs <- rowSums(ncs_temp)
remove(ncs_temp)

# 3. Count Strategies ----------------------------------------

# counts proportion of strategy usage
appendStrategyProportions <- function(conditions, name){
  
  # count occurrences of strategy
  count <- plyr::count
  t <- count(c(as.matrix(strat_matrix[df_valid$condition %in% conditions, ])))
  t$prop <- t$freq/sum(t$freq)
  
  # add to strategy data frame
  strategies <- merge(strategies, t, by.x=c('strategy'), by.y=c('x'), all = TRUE)
  names(strategies)[ncol(strategies)-1] <- paste(name, '_freq', sep = "")
  names(strategies)[ncol(strategies)] <- paste(name, '_prop', sep = "")
  strategies
}

# strategy_matrix
strat_matrix <- readArrayColumn(df_valid$strategies, 21)

# add proportions per condition
strategies <- appendStrategyProportions(c(0,1,2), 'all')
strategies <- appendStrategyProportions(c(0,1), 'control')
strategies <- appendStrategyProportions(c(2), 'exp')

# NaNs and order
strategies[is.na(strategies)] <- 0
strategies <- strategies[order(strategies$score, decreasing=TRUE),]

remove(appendStrategyProportions)

# 4. Count Types ----------------------------------------

# construct type dataframe
t1 <- aggregate(strategies,by=list(strategies$type), FUN=mean)
t2 <- aggregate(strategies,by=list(strategies$type), FUN=sum)

# merge infos of the two aggregations
types <- merge(t1[,c('Group.1', 'score', 'cluster')], t2[,c(1, 7:ncol(t2))], by='Group.1')
names(types)[1] <- 'type'

# NaNs and order
types <- round(types, 3)

# add names of strategy types
t3 <- read.csv('../data/strategies/strategytypes_names.csv', sep=';')
types <- merge(types, t3, by='type')
types <- types[order(types$score, decreasing=TRUE),]

remove(t1, t2, t3)

# 5. Count Clusters -------------------------------------------------------

# construct cluster dataframe
t1 <- aggregate(strategies,by=list(strategies$cluster), FUN=mean)
t2 <- aggregate(strategies,by=list(strategies$cluster), FUN=sum)
clusters <- merge(t1[,c('Group.1', 'score')], t2[,c(1, 7:ncol(t2))], by='Group.1')
names(clusters)[1] <- 'cluster'

# NaNs and order
clusters <- clusters[order(clusters$score, decreasing=TRUE),]
clusters <- round(clusters, 3)

remove(t1, t2)


# 6. Create Trial Dataframe -----------------------------------------

trial_frame <- data.frame('pid' = rep(df_valid$pid, each=21),
                          'index'= rep(1:21, times=nrow(df_valid)),
                           'condition'= rep(df_valid$condition, each=21),
                           'reflection'= rep(df_valid$reflection, each=21),
                           'ncs'= rep(df_valid$ncs, each=21),
                           'score' =  as.vector(t(readArrayColumn(df_valid$ML_scores, 21))),
                           'strategy' = as.vector(t(strat_matrix)),
                           'number_clicks' =  as.vector(t(readArrayColumn(df_valid$ML_clicksNumber, 21))))

# find indices of strategies in strategy dataframe
t <- unlist(lapply(trial_frame$strategy, function(z){which(strategies$strategy == z)}))

# retrieve strategy infos
trial_frame$cluster <- strategies[t,]$cluster
trial_frame$strategyscore <- strategies[t,]$score

# mode infos
trial_frame$type <- strategies[t,]$type
trial_frame$type_other <- trial_frame$type == 0
trial_frame$type_fs <- trial_frame$type == 1
trial_frame$type_ns <- trial_frame$type == 2

# add dv explicitly
trial_frame$adaptive    <- trial_frame$cluster == 2
trial_frame$moderate    <- trial_frame$cluster == 1
trial_frame$maladaptive <- trial_frame$cluster == 0

# add baseline of first 3 trials
t <- aggregate(.~ pid, subset(trial_frame, index < 4), FUN = sum)[c("pid","score","adaptive", "moderate", "maladaptive", "number_clicks", "strategyscore",
                                                                    'type_ns', 'type_fs', 'type_other')]
colnames(t) <- c('pid', 'score_baseline', 'adaptive_baseline', 'moderate_baseline', 'maladaptive_baseline', 'clicks_baseline', 'strategyscore_baseline', 
                 'type_ns_baseline', 'type_fs_baseline', 'type_other_baseline')
trial_frame <- merge(trial_frame, t, by='pid', all.x = TRUE, sort = FALSE)

# transfer score baseline to mean
trial_frame$score_baseline <- trial_frame$score_baseline/3
trial_frame$clicks_baseline <- trial_frame$clicks_baseline/3

# sort trial frame (important!)
trial_frame <- trial_frame[order(trial_frame$condition, trial_frame$pid, trial_frame$index),]

remove(t)


# 7. Create Transition Dataframe ----------------------------------------

# create transition frame from trial frame
# the last trial has no transition
last_trials <- (seq(21, to=nrow(trial_frame),by=21))
transition_frame <- trial_frame[-last_trials, c('pid', 'index','condition', 'reflection', 'ncs',  'score',
                                                'cluster', 'strategyscore', 'adaptive', 'maladaptive', 'moderate',
                                                'strategyscore_baseline', 'adaptive_baseline', 'maladaptive_baseline', 
                                                'type_ns', 'type_fs', 'type_other',
                                                'type_ns_baseline', 'type_fs_baseline', 'type_other_baseline'
                                                )]

# change names
names(transition_frame) <- c('pid', 'transition','condition', 'reflection', 'ncs', 'prev_score', 
                             'prev_cluster', 'prev_strategyscore', 'prev_adaptive', 'prev_maladaptive', 'prev_moderate',
                             'strategyscore_baseline', 'adaptive_baseline', 'maladaptive_baseline',
                             'prev_type_ns', 'prev_type_fs', 'prev_type_other',
                             'type_ns_baseline', 'type_fs_baseline', 'type_other_baseline')

# add engagement 
transition_frame <- merge(transition_frame, df_prompts[,c(-2)], by = c('pid', 'transition'), all.x = TRUE, sort = TRUE)

# sort trial frame (important!)
transition_frame <- transition_frame[order(transition_frame$condition, transition_frame$pid, transition_frame$transition),]

# add prompt info
transition_frame$prompt  <- transition_frame$transition %% 3 == 0
transition_frame$cprompt <- transition_frame$prompt & (!transition_frame$reflection)
transition_frame$rprompt <- transition_frame$prompt & (transition_frame$reflection)

# add behavior deltas
transition_frame$cluster_change_delta  <- diff(trial_frame$cluster)[-last_trials]
transition_frame$clicks_change_delta  <- diff(trial_frame$number_clicks)[-last_trials]

# add behavior changes
transition_frame$strategy_change <- diff(trial_frame$strategy)[-last_trials] != 0
transition_frame$type_change     <- diff(trial_frame$type)[-last_trials] != 0
transition_frame$cluster_change  <- diff(trial_frame$cluster)[-last_trials] != 0
transition_frame$clicks_change  <- diff(trial_frame$number_clicks)[-last_trials] != 0

# add score deltas
transition_frame$score_delta <- diff(trial_frame$score)[-last_trials]
transition_frame$strategyscore_delta <- diff(trial_frame$strategyscore)[-last_trials]

# dv
transition_frame$cluster_change_beneficial    <- transition_frame$cluster_change_delta > 0
transition_frame$cluster_change_unbeneficial  <- transition_frame$cluster_change_delta < 0
transition_frame$strategy_change_beneficial   <- transition_frame$strategyscore_delta > 0
transition_frame$strategy_change_unbeneficial <- transition_frame$strategyscore_delta < 0
transition_frame$clicks_change_increase <- transition_frame$clicks_change_delta > 0
transition_frame$clicks_change_decrease <- transition_frame$clicks_change_delta < 0

remove(last_trials, strat_matrix)


# 8. Functions: Model fitting and storing --------------------------------------------

# scaling of predictors
scale_variables <- TRUE

fitModel_lm <- function(dv, formula, df){
  # settings
  max_number_restarts = 2
  
  # concatenate
  formula <- reformulate(termlabels = formula, response = dv)
  
  # fit
  model <- lmer(formula, df)
  
  # restart if not converged
  for(i in 1:max_number_restarts){
    if(any(grepl("failed to converge", model@optinfo$conv$lme4$messages))){
      print('restart')
      ss <- getME(model, c("theta","fixef"))
      model <- update(model, start=ss,control=lmerControl(optimizer="bobyqa",
                                                          optCtrl=list(maxfun=2e5)))
      model@optinfo$ss <- ss
    }
    
  }
  model
}

fitModel_glm <- function(dv, formula, df){
  # settings
  max_number_restarts = 2
  
  # concatenate
  formula = reformulate(termlabels = formula, response = dv)
  
  # fit
  model <- glmer(formula, df, family = binomial)
  
  # restart if not converged
  for(i in 1:max_number_restarts){
    if(any(grepl("failed to converge", model@optinfo$conv$lme4$messages))){
      print('restart')
      ss <- getME(model, c("theta","fixef"))
      model <- update(model, start=ss,control=glmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=2e5)))
      
      model@optinfo$ss <- ss
      print(model@optinfo$conv$lme4$messages)
    }
  }
  
  model
}

toTex <- function(model, digits, names, type, compile, pvalcorrected, title){
  
  # header
  if(compile) {
    string <- c("\\documentclass{article}", "\\usepackage{booktabs}", "\\usepackage[T1]{fontenc}", "\\begin{document}", '\n')
  } else {
    string <- c('')
  }
  
  
  # content
  if(type %in% c('lm', 'glm')){
    model_string <- toTex_model(model, digits, type, title, names, pvalcorrected)
    string <- c(string, c(model_string))
    
  } else {
    stop('invalid type')
  }
  
  # messages
  for(mes in model@optinfo$conv$lme4$messages){
    string <- c(string, c(paste(mes, ' \\\\')))
  }
  for(mes in summary(model)$fitMsgs){
    string <- c(string, c(paste(mes, ' \\\\')))
  }
  
  # closer
  if(compile){
    string <- c(string, c('\n'), c("\\end{document}"))
  }
  
  string
}

toTex_model <- function(model, digits, type, title ='notitle', names=c(), pvalcorrected=FALSE){
  
  # ------ Table start + config
  string <- paste("\\begin{table}[h]
                    \\caption{", title, "}
                    \\label{label}
                    \\centering ", sep='')
  
  if(type == 'lm'){
    string2 <-   "\\begin{tabular}{lllllll}
                 \\toprule
                 Fixed Effect     & Estimate     & 95\\% CI  &  df & $t$ & $p$ & Significance \\\\
                 \\midrule"
    delimiter <- '\\cmidrule{1-7}\n  '
    coefficent_indices <- c(1, 0, 3, 4)
    offset <- 2
    
  } else if(type == 'glm'){
    string2 <- " \\begin{tabular}{llllll}
                 \\toprule
                 Fixed Effect     & Estimate     & 95\\% CI  & $z$ & $p$ & Significance \\\\
                 \\midrule"
    delimiter <- '\\cmidrule{1-6}\n  '
    coefficent_indices <- c(1, 0, 3)
    offset <- 1
    
  }
  string <- paste(string, string2, sep='')
  
  
  
  
  # ------ Table content
  if(length(names) == 0){
    names <- row.names(summary(model)$coefficients)
  }
  
  # loop trough estimators
  cof <- summary(model)$coefficients
  confint <- model@optinfo$confint
  ndots <- 0
  
  for(i in 1:nrow(cof)){
    row <- '\n  '
    
    # delimiter for interactions
    if(str_count(names[i], ":") > ndots){
      row <- paste(row, delimiter)
      ndots <- str_count(names[i], ":")
    }
    
    # name of effect
    name <- gsub("_", "", names[i])
    row <- paste(row, name)
    
    # loop trough estimate, df, t
    for(j in coefficent_indices){
      if(j == 0){
        
        # confidence interval
        if(is.null(confint)){
          ci <- c('-', '-', '-')
        } else {
          ci <- round(confint[i+offset,], digits)
        }
        addon <- sprintf("&  [%s, %s]", ci[1], ci[2])
      
        
      } else {
        
        # normal number
        addon <- round(cof[i,j], digits)
        addon <- sprintf("& %s", addon)
      }
      row <- paste(row, addon)
    }
    
    # use corrected p value?
    pval <- if(pvalcorrected) model@optinfo$pvalcorrected[i] else cof[i, tail(coefficent_indices, 1) +1]
    if(pvalcorrected) print('use corrected pvalues')
    
    # add p value
    #print(pval)
    addon <- substring(round(pval, digits = 3), 2)
    if(pval < 0.001) addon <- '<.001'
    addon <- sprintf("& %s", addon)
    row <- paste(row, addon)
    
    # significance
    addon <- if(pval < 0.05) '*' else ' '
    addon <- sprintf("& %s", addon)
    row <- paste(row, addon)
    
    # row end
    row <- paste(row, '\\\\')
    string <- paste(string, row)

  }
  
  # closing lines
  end <- '\n   \\bottomrule
          \\end{tabular}
          \\end{table}'
  string <- paste(string, end)
  
  # check naming
  print(data.frame(names, rownames(cof)))
  print('')
  
  # return final string
  string
}

storeModel <- function(model, model_folder, model_name, title, type, digits=2, compile=FALSE, names = c(), confidence=FALSE, pvalcorrected=FALSE){
  
  
  if(confidence){
    
    print('start computing CIs')
    # extract last starting point for confint 
    ss <- model@optinfo$ss
    # compute confidence intervals
    model@optinfo$confint <- confint(model, oldNames=FALSE)
    print('finished computing CIs')
  }
  
  # store model
  path = paste("../results/models/",model_folder, model_name, sep='')
  save(model, file = paste(path,".RData", sep=''))
  
  # save to tex file
  # compile to pdf
  if(compile){
    path = paste("../results/tex/",model_folder, model_name, sep='')
    new_file <- file(paste(path, ".tex", sep=''))
    string <- toTex(model, digits, names, type, compile, pvalcorrected, title)
    
    writeLines(string, new_file)
    close(new_file)
  

    tools::texi2pdf(paste(path,".tex", sep=''), clean = TRUE)
    
    # move output pdf to folder
    path = paste("../results/tex/",model_folder, sep='')
    file.move(paste(model_name, '.pdf', sep=''), path, overwrite = TRUE)
    
    # remove tex file
    path = paste("../results/tex/",model_folder, model_name, '.tex', sep='')
    file.remove(path)
  }
  
}

modelPlots <- function(model){
  par(mfrow=c(2,2))
  qqnorm(resid(model))
  plot(fitted(model), resid(model))
  plot(fitted(model), sqrt(abs(scale(resid(model), scale = T))))
  plot(cooks.distance(model))
  par(mfrow=c(1,1))
}


# 9. Functions: Benjamini Hochberg Correction ----------------------------------------

applyBHCorection <- function(model_names, title){
  
  models <- c()
  for(model_name in model_names){
    
    # load model
    load(sprintf('../results/models/%s%i_%s.RData', model_path, max(N), model_name))
    
    # add pvalues space
    if(class(model) == "glmerMod"){
      model@optinfo$pvalcorrected <- summary(model)$coefficients[,4]
    } else {
      model@optinfo$pvalcorrected <- summary(model)$coefficients[,5]
    }
    
    models <- c(models, model)
  }
  
  # correct pvalues
  coefs <- names(summary(models[[1]])$coefficients[,1])
  for(coef in coefs){
    
    # collect pvalues
    pvals <- c()
    for(model in models){
      if(coef %in% names(summary(model)$coefficients[,1])){
        pvals <- c(pvals, model@optinfo$pvalcorrected[coef])
      }
    }
    
    # apply correction
    pvals <- p.adjust(pvals, "BH")
    
    # add to model
    for(i in 1:length(pvals)){
      if(coef %in% names(summary(model)$coefficients[,1])){
        models[[i]]@optinfo$pvalcorrected[coef] <- pvals[i]
      }
    }
  }
  
  # store models
  for(i in 1:length(models)){
    model <- models[[i]]
    model_name <- model_names[i]
    
    if(class(model) == "glmerMod"){
      type = 'glm'
    } else {
      type = 'lm'
    }
    
    storeModel(model, model_path, sprintf('%i_%s_corrected', max(N), model_name), 
               sprintf('%s %s (%i )', title, model_name, max(N)),
               type, pvalcorrected=TRUE, compile = TRUE)
  }
}




