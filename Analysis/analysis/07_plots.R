# This script generates all plots.

source("00_format_data.R")
N <- 21

# 1.Functions: Learning Curves plotting -------------------------------------------------------------------

getLC <- function(df, dv, labels = c("Control","Reflective Learning"), return=FALSE, path=''){
  
  if(is.null(df$index)){
    df$index <- df$transition
  }
  
  # rescale
  if(!is.null(attr(df$index, 'scaled:scale'))){
    df$index <- df$index * attr(df$index, 'scaled:scale') + attr(df$index, 'scaled:center')
  }
  
  # aggregate mean
  formula = reformulate(termlabels = 'reflection + index', response = dv)
  data_mean <- aggregate(formula, df, mean)
  
  # aggregate error
  data_N <- aggregate(formula, df, length)
  data_sd <- aggregate(formula, df, sd)
  data_mean$error <- 1.96*data_sd[,c(dv)]/sqrt(data_N[,c(dv)])
  
  #rename
  names(data_mean) <- c('reflection', 'index', 'mean', 'error')
  
  if(return){
    data_mean
  }
  plotLC(data_mean, dv, labels, return, path = path)
}

plotLC <- function(data, ylabel, labels, return=FALSE, path=''){
  
  # plot
  colors <- c("#3977AF", "#EF8536")
  
  pl <- ggplot(data, mapping = aes(x=index, y=mean, group=reflection)) + geom_line(aes(color=reflection))
  pl <- pl  + geom_ribbon(subset(data, !reflection), mapping=aes(ymin=mean-error, ymax=mean+error), alpha=0.2, fill=colors[1])
  pl <- pl  + geom_ribbon(subset(data, reflection), mapping=aes(ymin=mean-error, ymax=mean+error), alpha=0.2, fill=colors[2])
  
  # design
  pl <- pl + xlab("Trial") + ylab(ylabel) + theme_bw() + theme(legend.position = "top", plot.title = element_text(hjust = 0.5))
  pl <- pl  + scale_color_manual(values = colors, labels = labels)   + theme(text = element_text(size = 15))  
  pl <- pl + scale_x_continuous(limits = c(0,max(N)+1), expand = c(0, 0), breaks = seq(0 , max(N), 6), minor_breaks = seq(0 , max(N), 3))
  
  if(return){
    pl
  } else {
    if(path != ''){
      png(file=path, width=900, height=450)
      plot(pl)
      dev.off()
    }
    plot(pl)
  }
}

# Subgroup functions
getLC_subgroup <- function(df, col, dv, return=FALSE, labels = c('C:low', 'R:low', 'C:high', 'R:high')){
  
  # rescale
  if(is.null(df$index)){
    df$index <- df$transition
  }
  
  # rescale
  if(!is.null(attr(df$index, 'scaled:scale'))){
    df$index <- df$index * attr(df$index, 'scaled:scale') + attr(df$index, 'scaled:center')
  }
  
  # prepare 
  formula = reformulate(termlabels = 'reflection + cond2 + index', response = dv)
  df$cond2 <- df[, c(col)] < median(df[, c(col)])
  print(median(df[, c(col)]))
  
  # aggregate mean
  data_mean <- aggregate(formula, df, mean)
  
  # aggregate error
  data_N  <- aggregate(formula, df, length)
  data_sd <- aggregate(formula, df, sd)
  data_mean$error <- 1.96*data_sd[,c(dv)]/sqrt(data_N[,c(dv)])
  
  #rename
  names(data_mean) <- c('reflection', 'cond2', 'index', 'mean', 'error')
  
  plotLC_subgroup(data_mean, dv, return = return, labels = labels)
}


plotLC_subgroup <- function(data, ylabel, return=FALSE, labels=c()){
  
  # plot
  colors <- c("#3977AF", "#EF8536")
  pl <- ggplot(data, mapping = aes(x=index, y=mean, group=interaction(reflection, cond2))) + 
    geom_line(aes(color=reflection, linetype=cond2))
  
  # design
  pl <- pl + xlab("Trial") + ylab(ylabel) + theme_bw() + theme(legend.position = "top", plot.title = element_text(hjust = 0.5))
  pl <- pl  + scale_color_manual(values = colors, labels = c('Control', 'Reflective Learning'))   + 
    scale_linetype_manual(values = c(2,1), labels = c('High Baseline', 'Low Baseline'))  +
    theme(text = element_text(size = 15))  
  pl <- pl + scale_x_continuous(limits = c(0,max(N)+1), expand = c(0, 0), breaks = seq(0 , max(N), 6), minor_breaks = seq(0 , max(N), 3))
  
  if(return){
    pl
  } else {
    plot(pl)
  }
}


# 2 Trial data Plot --------------------------------------------------------


pl <- getLC(trial_frame, 'strategyscore',  path='', return = T)
pl <- pl + ylab("Expected Score") 
pl <- pl + scale_x_continuous(limits = c(1,max(N)),
                              expand = c(0, 0), 
                              breaks = c(3,6,9,12,15,18),
                              minor_breaks = seq(0 , max(N), 3))

pl <- pl + scale_y_continuous(breaks = c(10,20,30), limits = c(9,35))

pl <- pl + theme(legend.title = element_blank(), 
                 panel.grid.minor.y = element_blank(), 
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.x = element_blank())

pl <- pl + geom_vline(xintercept = c(3,6,9,12,15,18), linetype="dotted", 
                      color = "black", size=0.5, alpha=0.5)

pl <- pl +  geom_label(aes(x = 3.3, y = 9, label = "R"), fill = "white", color="grey", size=4)
pl <- pl +  geom_label(aes(x = 6.3, y = 9, label = "R"), fill = "white", color="grey", size=4)
pl <- pl +  geom_label(aes(x = 9.3, y = 9, label = "R"), fill = "white", color="grey", size=4)
pl <- pl +  geom_label(aes(x = 12.3, y = 9, label = "R"), fill = "white", color="grey", size=4)
pl <- pl +  geom_label(aes(x = 15.3, y = 9, label = "R"), fill = "white", color="grey", size=4)
pl <- pl +  geom_label(aes(x = 18.3, y = 9, label = "R"), fill = "white", color="grey", size=4)

pl <- pl + geom_segment(aes(x = 3, y = 34, xend = 6.99, yend = 34), color='darkgrey',
                        arrow = arrow(length = unit(0.3, "cm"), ends='both', angle=90))
pl <- pl +  geom_label(aes(x = 5, y = 34, label = "Learning \n Phase"), fill = "white", color="darkgrey", size=4)

pl <- pl + geom_segment(aes(x = 7.01, y = 34, xend = 20.95, yend = 34), color='darkgrey',
                        arrow = arrow(length = unit(0.3, "cm"), ends='both', angle=90))
pl <- pl +  geom_label(aes(x = 14, y = 34, label = "Performance \n Phase"), fill = "white", color="darkgrey", size=4)

pdf(file='../plots/strategyscore.pdf', width=6, height=4)
pl
dev.off()



# 3 Baseline Interaction Plot --------------------------------------------------------

pl <- getLC_subgroup(trial_frame, 'strategyscore_baseline', 'strategyscore', return = T)

pl <- pl + ylab("Expected Score") 
pl <- pl + scale_x_continuous(limits = c(1,max(N)),
                              expand = c(0, 0), 
                              breaks = c(3,6,9,12,15,18),
                              minor_breaks = seq(0 , max(N), 3))

pl <- pl + scale_y_continuous(breaks = c(0,10,20,30), limits = c(-5,36))

pl <- pl + theme(legend.title = element_blank(), 
                 panel.grid.minor.y = element_blank(), 
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.x = element_blank(),
                 legend.position="top",
                 legend.text=element_text(size=13), 
                 legend.direction="vertical")

pl <- pl + geom_vline(xintercept = c(3,6,9,12,15,18), linetype="dotted", 
                      color = "black", size=0.5, alpha=0.5)

pl <- pl +  geom_label(aes(x = 3.3, y = -5, label = "R"), fill = "white", color="grey", size=4)
pl <- pl +  geom_label(aes(x = 6.3, y = -5, label = "R"), fill = "white", color="grey", size=4)
pl <- pl +  geom_label(aes(x = 9.3, y = -5, label = "R"), fill = "white", color="grey", size=4)
pl <- pl +  geom_label(aes(x = 12.3, y = -5, label = "R"), fill = "white", color="grey", size=4)
pl <- pl +  geom_label(aes(x = 15.3, y = -5, label = "R"), fill = "white", color="grey", size=4)
pl <- pl +  geom_label(aes(x = 18.3, y = -5, label = "R"), fill = "white", color="grey", size=4)

pdf(file='../plots/strategyscore_baseline.pdf', width=6, height=4.5)
pl
dev.off()


# 4 Strategy Change Plot --------------------------------------------------------

pl <- getLC(transition_frame, 'strategy_change',  path='', return = T)
pl <- pl + ylab("Proportion of \nstrategy changes") + xlab("Transition") 
pl <- pl + scale_x_continuous(limits = c(1,max(N)-1),
                              expand = c(0, 0), 
                              breaks = c(3,6,9,12,15,18),
                              minor_breaks = seq(0 , max(N), 3))

pl <- pl + scale_y_continuous(labels = scales::percent)

pl <- pl + theme(legend.title = element_blank(), 
                 panel.grid.minor.y = element_blank(), 
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.x = element_blank())

pl <- pl + geom_vline(xintercept = c(3,6,9,12,15,18), linetype="dotted", 
                      color = "black", size=0.5, alpha=0.5)

pl <- pl +  geom_label(aes(x = 3, y = 0.01, label = "R"), fill = "white", color="grey", size=4)
pl <- pl +  geom_label(aes(x = 6, y = 0.01, label = "R"), fill = "white", color="grey", size=4)
pl <- pl +  geom_label(aes(x = 9, y = 0.01, label = "R"), fill = "white", color="grey", size=4)
pl <- pl +  geom_label(aes(x = 12, y = 0.01, label = "R"), fill = "white", color="grey", size=4)
pl <- pl +  geom_label(aes(x = 15, y = 0.01, label = "R"), fill = "white", color="grey", size=4)
pl <- pl +  geom_label(aes(x = 18, y = 0.01, label = "R"), fill = "white", color="grey", size=4)


pdf(file='../plots/strategy_change.pdf', width=6, height=4)
pl
dev.off()

