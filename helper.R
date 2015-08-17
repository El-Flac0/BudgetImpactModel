## Final Helper function

## Prep data and draft function.

require(dplyr)
require(RCurl)
require(reshape2)
require(RColorBrewer)
require(scales)
require(ggplot2)

marketShare <- function(share=0.05) {
  mkt1 <- share
  mkt2 <- share+0.05
  mkt3 <- share+0.1
  df <- data.frame(treatment=character(), stringsAsFactors = FALSE,
                   Year_0=double(),
                   Year_1=double(),
                   Year_2=double(),
                   Year_3=double()
  )
  
  df[1,] <- c('X', 0, mkt1, mkt2, mkt3)
  df[2,] <- c('A', 0.05, 0.05*(1-mkt1), 0.05*(1-mkt2), 0.05*(1-mkt3))
  df[3,] <- c('B', 0.1, 0.1*(1-mkt1), 0.1*(1-mkt2), 0.1*(1-mkt3))
  df[4,] <- c('C', 0.1,  0.1*(1-mkt1), 0.1*(1-mkt2), 0.1*(1-mkt3))
  df[5,] <- c('D', 0.5, 0.5*(1-mkt1), 0.5*(1-mkt2), 0.5*(1-mkt3))
  df[6,] <- c('E', 0.03, 0.03*(1-mkt1), 0.03*(1-mkt2), 0.03*(1-mkt3))
  df[7,] <- c('F', 0.12, 0.12*(1-mkt1), 0.12*(1-mkt2), 0.12*(1-mkt3))
  df[8,] <- c('G', 0.04, 0.04*(1-mkt1), 0.04*(1-mkt2), 0.04*(1-mkt3))
  df[9,] <- c('H', 0.06, 0.06*(1-mkt1), 0.06*(1-mkt2), 0.06*(1-mkt3))
  
  df
}

# function to create plot

plotMaker <- function(incidence=0.01, popSize=100000, treat=0.8, share=0.05,
                      impact='absolute', plot='stratified', stacked=FALSE) {
  
  cost_per_unit <- c(5, 1,1,1,1,1,1,1,1)
  units_per_patient <- c(100,100,100,100,100,100,100,100,100)
  use_type <- c('Treatment_X', 'Treatment_A', 'Treatment_B', 'Treatment_C', 'Treatment_D',
                'Treatment_E', 'Treatment_F', 'Treatment_G', 'Treatment_H')
  
  therapyCost <- data.frame(cost_per_unit, units_per_patient, use_type)
  
  cost_per_unit <- c(150, 200, 900, 75)
  units_per_patient <- c(12, 2, 2, 10)
  use_type <- c('Clinic Visit', 'Emergency Visit', 'Hospital Day', 'Lab Test')
  
  medUtil <- data.frame(cost_per_unit, units_per_patient, use_type)
  
  resUse <- rbind(therapyCost, medUtil)
  
  
  resUse$cost_per_patient <- resUse$cost_per_unit*resUse$units_per_patient
  firstUse <- resUse[c(1,2,4,3)]
  
  #calculate market shares
  mkt1 <- share
  mkt2 <- share+0.05
  mkt3 <- share+0.1
  df <- data.frame(treatment=character(), stringsAsFactors = FALSE,
                   Year_0=double(),
                   Year_1=double(),
                   Year_2=double(),
                   Year_3=double()
  )
  
  df[1,] <- c('X', 0, mkt1, mkt2, mkt3)
  df[2,] <- c('A', 0.05, 0.05*(1-mkt1), 0.05*(1-mkt2), 0.05*(1-mkt3))
  df[3,] <- c('B', 0.1, 0.1*(1-mkt1), 0.1*(1-mkt2), 0.1*(1-mkt3))
  df[4,] <- c('C', 0.1,  0.1*(1-mkt1), 0.1*(1-mkt2), 0.1*(1-mkt3))
  df[5,] <- c('D', 0.5, 0.5*(1-mkt1), 0.5*(1-mkt2), 0.5*(1-mkt3))
  df[6,] <- c('E', 0.03, 0.03*(1-mkt1), 0.03*(1-mkt2), 0.03*(1-mkt3))
  df[7,] <- c('F', 0.12, 0.12*(1-mkt1), 0.12*(1-mkt2), 0.12*(1-mkt3))
  df[8,] <- c('G', 0.04, 0.04*(1-mkt1), 0.04*(1-mkt2), 0.04*(1-mkt3))
  df[9,] <- c('H', 0.06, 0.06*(1-mkt1), 0.06*(1-mkt2), 0.06*(1-mkt3))
  
  df
  
  #calculate population under treatment
  treatPop <- as.numeric(incidence)*as.numeric(popSize)*as.numeric(treat)
  firstUse$totalCost <- firstUse$cost_per_patient*treatPop
  resUse <- firstUse[c(4:5)]
  
  subset <- filter(resUse, !grepl('Clinic Visit|Emergency Visit|Hospital Day|Lab Test', use_type))
  subset$use_type <- as.character(subset$use_type)
  
  for (x in seq_along(subset$use_type)) {
    subset$Year_0[x] <- switch(subset$use_type[x],
                               Treatment_X=subset$totalCost[x]*as.numeric(df$Year_0[1]),
                               Treatment_A=subset$totalCost[x]*as.numeric(df$Year_0[2]),
                               Treatment_B=subset$totalCost[x]*as.numeric(df$Year_0[3]),
                               Treatment_C=subset$totalCost[x]*as.numeric(df$Year_0[4]),
                               Treatment_D=subset$totalCost[x]*as.numeric(df$Year_0[5]),
                               Treatment_E=subset$totalCost[x]*as.numeric(df$Year_0[6]),
                               Treatment_F=subset$totalCost[x]*as.numeric(df$Year_0[7]),
                               Treatment_G=subset$totalCost[x]*as.numeric(df$Year_0[8]),
                               Treatment_H=subset$totalCost[x]*as.numeric(df$Year_0[9])
    )
    subset$Year_1[x] <- switch(subset$use_type[x],
                               Treatment_X=subset$totalCost[x]*as.numeric(df$Year_1[1]),
                               Treatment_A=subset$totalCost[x]*as.numeric(df$Year_1[2]),
                               Treatment_B=subset$totalCost[x]*as.numeric(df$Year_1[3]),
                               Treatment_C=subset$totalCost[x]*as.numeric(df$Year_1[4]),
                               Treatment_D=subset$totalCost[x]*as.numeric(df$Year_1[5]),
                               Treatment_E=subset$totalCost[x]*as.numeric(df$Year_1[6]),
                               Treatment_F=subset$totalCost[x]*as.numeric(df$Year_1[7]),
                               Treatment_G=subset$totalCost[x]*as.numeric(df$Year_1[8]),
                               Treatment_H=subset$totalCost[x]*as.numeric(df$Year_1[9])
    )
    subset$Year_2[x] <- switch(subset$use_type[x],
                               Treatment_X=subset$totalCost[x]*as.numeric(df$Year_2[1]),
                               Treatment_A=subset$totalCost[x]*as.numeric(df$Year_2[2]),
                               Treatment_B=subset$totalCost[x]*as.numeric(df$Year_2[3]),
                               Treatment_C=subset$totalCost[x]*as.numeric(df$Year_2[4]),
                               Treatment_D=subset$totalCost[x]*as.numeric(df$Year_2[5]),
                               Treatment_E=subset$totalCost[x]*as.numeric(df$Year_2[6]),
                               Treatment_F=subset$totalCost[x]*as.numeric(df$Year_2[7]),
                               Treatment_G=subset$totalCost[x]*as.numeric(df$Year_2[8]),
                               Treatment_H=subset$totalCost[x]*as.numeric(df$Year_2[9])
    )
    subset$Year_3[x] <- switch(subset$use_type[x],
                               Treatment_X=subset$totalCost[x]*as.numeric(df$Year_3[1]),
                               Treatment_A=subset$totalCost[x]*as.numeric(df$Year_3[2]),
                               Treatment_B=subset$totalCost[x]*as.numeric(df$Year_3[3]),
                               Treatment_C=subset$totalCost[x]*as.numeric(df$Year_3[4]),
                               Treatment_D=subset$totalCost[x]*as.numeric(df$Year_3[5]),
                               Treatment_E=subset$totalCost[x]*as.numeric(df$Year_3[6]),
                               Treatment_F=subset$totalCost[x]*as.numeric(df$Year_3[7]),
                               Treatment_G=subset$totalCost[x]*as.numeric(df$Year_3[8]),
                               Treatment_H=subset$totalCost[x]*as.numeric(df$Year_3[9])
    )
  }
  
  resUse <- filter(resUse, grepl('Clinic Visit|Emergency Visit|Hospital Day|Lab Test', use_type))
  resUse$Year_0 <- resUse$totalCost
  resUse$Year_1 <- resUse$totalCost
  resUse$Year_2 <- resUse$totalCost
  resUse$Year_3 <- resUse$totalCost
  
  resUse <- rbind(resUse, subset)
  
  resUse <- resUse[c(1,3:6)]
  
  mRes <- melt(resUse, id=c('use_type'))
  
  without <- filter(mRes, grepl('Year_0', variable))
  without$include_X <- 'without'
  without1 <- without
  without1$variable <- 'Year_1'
  without2 <- without
  without2$variable <- 'Year_2'
  without3 <- without
  without3$variable <- 'Year_3'
  
  without <- rbind(without1, without2, without3)
  
  with <- filter(mRes, !grepl('Year_0', variable))
  with$include_X <- 'with'
  
  resLong <- rbind(without, with)
  resLong$value <- resLong$value/1000
  arrange(resLong, use_type)
  
  ## Begin the Plotting Labyrinth
  
  #create clor scheme for stack plot.
  colors <- brewer.pal(11, "Spectral")
  pal <- colorRampPalette(colors)
  myCols <- pal(13)
  names(myCols) <- levels(resLong$use_type)
  colScale <- scale_fill_manual(name='Resource Use', values=myCols)
  
  #absolute
  if (impact == "absolute") {
    #absolute stack
    if (stacked == TRUE) {
      #absolute aggregated strat
      if (plot == "aggregated") {
        stack <- resLong %>%
          group_by(use_type, include_X) %>%
          summarise(total=sum(value))
        
        g <- ggplot(stack, aes(x=include_X, y=total, fill=use_type)) +
          geom_bar(position='stack', stat='identity', aes(order=desc(use_type))) +
          colScale + xlab('Treatment X Included') +
          scale_y_continuous(name='Total Cost (Thousands $USD)', labels = comma)

      } else {
        #absolute stratified stack
        g <- ggplot(resLong, aes(x=include_X, y=value, fill=use_type)) +
          geom_bar(position='stack', stat='identity') +
          colScale +
          facet_grid(~ variable) + scale_y_continuous(name='Total Cost (Thousands $USD)', labels = comma) +
          xlab('Treatment X Included')
      }
    }
    else if (stacked == FALSE) {
      #absolute flat
      abso <- resLong %>%
        group_by(variable, include_X) %>%
        summarise(total=sum(value))
      
      #absolute aggregate flat    
      if (plot == "aggregated") {
        aggr <- abso %>%
          group_by(include_X) %>%
          summarise(total=sum(total))
        
        g <- ggplot(aggr, aes(x=include_X, y=total, fill=include_X)) +
          geom_bar(position='dodge', stat='identity') + guides(fill=FALSE) +
          scale_y_continuous(name='Total Cost (Thousands $USD)', labels = comma) +
          xlab('Treatment X Included')
      } else if (plot == 'stratified') {
        
        #absolute stratified flat
        g <- ggplot(abso, aes(x=include_X, y=total, fill=include_X)) +
          geom_bar(position='dodge', stat='identity') +
          facet_grid(~ variable) + guides(fill=FALSE) +
          scale_y_continuous(name='Total Cost (Thousands $USD)', labels = comma) +
          xlab('Treatment X Included')
      }
    }
  } 
  else if (impact == 'incremental') {
    #incremental stack
    incWith <- resLong %>%
      filter(grepl('Treatment', use_type)) %>%
      filter(!grepl('without', include_X))
      
    if (stacked == TRUE) {
      # incremental aggregate stack
      if (plot == 'aggregated') {
        aggr1 <- incWith %>%
          group_by(use_type, include_X) %>%
          summarise(total=sum(value))
        
        g <- ggplot(aggr1, aes(x=include_X, y=total, fill=use_type)) +
          geom_bar(position='stack', stat='identity') +
          colScale +
          scale_y_continuous(name='Total Cost (Thousands $USD)', labels = comma) +
          xlab('Treatment X Included')
      } else if (plot == 'stratified') {
        # incremental stratified stack
        g <- ggplot(incWith, aes(x=include_X, y=value, fill=use_type)) +
          geom_bar(position='stack', stat='identity') +
          colScale +
          facet_grid(~ variable) +
          scale_y_continuous(name='Total Cost (Thousands $USD)', labels = comma) +
          xlab('Treatment X Included')
      }
    } 
    else if (stacked == FALSE) {
      #incremental flat
      abso <- resLong %>%
        group_by(variable, include_X) %>%
        summarise(total=sum(value))
      incl <- abso %>%
        filter(grepl('without', include_X))
      abso$total <- abso$total-incl$total[1]
      incr <- abso %>%
        filter(!grepl('without', include_X))
      
      if (plot == 'aggregated') {
        #incremental aggregate flat
        aggr2 <- incr %>%
          group_by(include_X) %>%
          summarise(total=sum(total))
        
        g <- ggplot(aggr2, aes(x=include_X, y=total, fill=include_X)) +
          geom_bar(position='dodge', stat='identity') + guides(fill=FALSE) +
          scale_y_continuous(name='Total Cost (Thousands $USD)', labels = comma) +
          xlab('Treatment X Included')
      } 
      else if (plot == 'stratified') {
        #incremental stratified flat
        g <- ggplot(incr, aes(x=include_X, y=total, fill=include_X)) +
          geom_bar(position='dodge', stat='identity') +
          facet_grid(~ variable) + guides(fill=FALSE) +
          scale_y_continuous(name='Total Cost (Thousands $USD)', labels = comma) +
          xlab('Treatment X Included')
      }
    }  
  }
  print(g)
}