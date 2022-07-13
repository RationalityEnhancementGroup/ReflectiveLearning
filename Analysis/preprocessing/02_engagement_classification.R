# Pre-processing II: This script let's you assess all reflection prompts for the degree 
# of engagement and stores the results as dataframe_prompts.csv'

#  0 Packages -------------------------------------------------------------


library(psych)

# 1 Import and Exclusion --------------------------------------------------------------

df_all <- read.csv('../data/experiment/dataframe.csv')

# early quitter
df_finished <- df_all[df_all$status != 6,]

# exclusion criteria
df_valid <- df_finished[df_finished$attemptsQuiz <= 3,]

# subset of reflection condition
df <- subset(df_valid, condition == 2)
nprompts <- 6


# 2 Create Prompts Dataframe ------------------------------------------------

readArrayColumn <- function(col, ncol){
  res<- data.frame(matrix(ncol=ncol,nrow=0 ))
  for (el in col) {
    res[nrow(res) + 1,] = scan(text=substr(el, 2, nchar(el)-1), sep=',', quiet = TRUE)
  }
  res
}

# storage
df_prompts <- data.frame(matrix(nrow=0, ncol=5))
names(df_prompts) <- c('pid', 'index', 'RT', 'length', 'content')

# convert RT and length
RTs <- readArrayColumn(df$promptsRt, nprompts)
lengths <- readArrayColumn(df$promptsLengths, nprompts)

# loop through participants
for(i in 1:nrow(df)){
  
  # loop through prompts
  prompts <- strsplit(df$promptsContent[i], '---------------------------------')
  print(df$pid[i])

  for(p in 1:nprompts){
    
    # store
    df_prompts[nrow(df_prompts) + 1,] = c(df$pid[i], p*3, RTs[i,p], lengths[i, p], prompts[[1]][p])
  }
}
df_prompts$index <- as.numeric(df_prompts$index)
df_prompts$RT <- as.numeric(df_prompts$RT)
df_prompts$length <- as.numeric(df_prompts$length)

# 3 Engagement Assessment ------------------------------------------------

# 0: no engagement
# 1: low engagement
# 2: high engagement

assess_prompts <- function(df){
  
  # shuffle to make judgment more objective
  df <- df[sample(nrow(df)),]
  
  # placeholder
  df$meaningful <- NaN
  
  # loop trough prompts
  for(i in 1:nrow(df)){
    print('')
    print(paste('Content:', df$content[i]))
    
    df$meaningful[i] <- as.integer(readline(prompt="Enter Meaningfullness 1=no/2=yes: ")) == 2
  }
  
  df$meaningful <- as.logical(df$meaningful)
  df<- df[order(df$pid, df$index),]
  df
}

# asses meaningfulness
df_annotated <- assess_prompts(df_prompts)

# decision tree of classification
# low engagement: default
df_annotated$engagement <- 1

# no meaningful response -> no engagement
df_annotated[!df_annotated$meaningful,]$engagement <- 0

# quick response -> low engagement
df_annotated[df_annotated$meaningful & df_annotated$RT < 69,]$engagement <- 1

# high engagement
df_annotated[df_annotated$meaningful & (df_annotated$RT > 144 | df_annotated$length > 292),]$engagement <- 2

# store
write.csv(df_annotated,"../data/experiment/dataframe_prompts.csv", row.names = FALSE)




# reset global environment
rm(list = ls())