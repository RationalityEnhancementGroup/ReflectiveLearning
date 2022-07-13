# Pre-processing III: This script clusters the applied planning strategies in three 
# adapativness clusters based on their expected score '

#  0 Packages -------------------------------------------------------------

library(tibble)

# 1 Import and Exclusion --------------------------------------------------------------

df_all <- read.csv('../data/experiment/dataframe.csv')

# early quitter
df_finished <- df_all[df_all$status != 6,]

# exclusion criteria
df_valid <- df_finished[(df_finished$attemptsQuiz <= 3) & (df_finished$naive == 'True'),]
df_valid$pid <- factor(df_valid$pid)

# ---- read strategy information
strategies <- read.csv('../data/strategies/strategies.csv')
names(strategies) <- c('strategy', 'score', 'type')


# 2 Count Strategies ----------------------------------------

# util function
count <- plyr::count
readArrayColumn <- function(col, ncol){
  res<- data.frame(matrix(ncol=ncol,nrow=0 ))
  for (el in col) {
    res[nrow(res) + 1,] = scan(text=substr(el, 2, nchar(el)-1), sep=',', quiet = TRUE)
  }
  res
}

# counts how often a strategy was used
appendStrategyProportions <- function(conditions, name){
  
  # get strategy matrix
  strat_matrix <- readArrayColumn(df_valid$strat, 21)
  
  # count occurrences of strategy for selected conditions
  t <- count(c(as.matrix(strat_matrix[df_valid$condition %in% conditions,])))
  t$prop <- t$freq/sum(t$freq)
  
  # add to strategy data frame
  strategies <- merge(strategies, t, by.x=c('strategy'), by.y=c('x'), all = TRUE)
  names(strategies)[ncol(strategies)-1] <- paste(name, '_freq', sep = "")
  names(strategies)[ncol(strategies)] <- paste(name, '_prop', sep = "")
  strategies
}

# count overall frequency
strategies <- appendStrategyProportions(c(0,1,2), 'all')

# NaNs and order
strategies[is.na(strategies)] <- 0
strategies <- strategies[order(strategies$score, decreasing=TRUE),]

# remove not used strategies
strategies <- strategies[strategies$all_freq != 0, ]


# 3 Determine Cluster --------------------------------------------------------------

# normalize
t <- strategies$score
t <- (t-min(t))/(max(t)-min(t))

# cluster
fit <- kmeans(t, centers = c(0.6, 0.5, 0.4))

# assign cluster to strategies
strategies <- add_column(strategies, cluster = max(fit$cluster) - fit$cluster, .after = 'type')

# plot
plot(strategies$score, ylim=c(-20,45))
abline(v=tail(which(strategies$cluster == 1), n =1) + 0.5 )
abline(v=tail(which(strategies$cluster == 2), n =1) + 0.5 )


# 4 Store --------------------------------------------------------------

# delete proportions
strategies <- strategies[, 1:4]
write.csv(strategies,"../data/strategies/strategies_clustered.csv", row.names = FALSE)

# reset global environment
rm(list = ls())
