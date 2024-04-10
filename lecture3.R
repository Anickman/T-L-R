install.packages("randomizr")
install.packages("MarketMatching")
install.packages("matchit")
install.packages("readr")
library(readr)
library(randomizr)
library(MarketMatching)

# set test and control 
set.seed(119)
Assignment <- complete_ra(N=1000, m_each=c(750,250), conditions = c("control","test"))
table(Assignment)

# Join this table with the file 

#Calculate means for test and control groups:  
  
  aggregate(Joined[,2:6], list(Joined$Assignment),mean)

#Calculate means, boxplots, and t-tests for test and control groups:
  for (i in 2:6) {
    boxplot(Joined[, i] ~ Joined$Assignment,
            ylab = names(Joined[i]),
            xlab = "Assignment Group"
    )
    print(t.test(Joined[, i] ~ Joined$Assignment))
  }

# MarketMatching:
  
View(DMA_Metrics)

DMA_Metrics$Date<- as.Date(DMA_Metrics$Date)
  
  mm<- MarketMatching::best_matches(DMA_Metrics,
                                    id_variable = "dmaname",
                                    date_variable = "Date",
                                    matching_variable = "TicketSales",
                                    #matches = 5,
                                    suggest_market_splits = TRUE,
                                    parallel = FALSE,
                                    dtw_emphasis = 0,
                                    start_match_period = "2019-01-01",
                                    end_match_period = "2021-03-01")
  
print(mm$SuggestedTestControlSplits,n=20)

mm2<- MarketMatching::best_matches(DMA_Metrics,
                                  id_variable = "dmaname",
                                  date_variable = "Date",
                                  matching_variable = "TicketSales",
                                  matches = 3,
                                  #suggest_market_splits = TRUE,
                                  parallel = FALSE,
                                  dtw_emphasis = 0,
                                  start_match_period = "2019-01-01",
                                  end_match_period = "2021-03-01")
mm2$BestMatches

# Find 10 matches for DFW and Madison
mm3<- MarketMatching::best_matches(DMA_Metrics,
                                         id_variable = "dmaname",
                                         date_variable = "Date",
                                         matching_variable = "TicketSales",
                                         matches = 10,
                                         #suggest_market_splits = TRUE,
                                         parallel = FALSE,
                                         dtw_emphasis = 0.5,
                                         start_match_period = "2019-01-01",
                                         end_match_period = "2021-03-01",
                                   markets_to_be_matched =c("DALLAS-FT. WORTH","MADISON") )

mm3$BestMatches




# MarketMatching - look at model weights and visualize time series matches:
  
  results<-MarketMatching::inference(matched_markets=mm3, 
                                     analyze_betas=TRUE, 
                                     test_market = "MADISON", 
                                     end_post_period = "2021-06-01")

  results$PlotActuals
  results$PlotActualVersusExpected
  

  
  #MatchIt:
  install.packages("MatchIt")
  install.packages("RTools")
  library(MatchIt)
  
  m.out<- MatchIt(Treatment ~ TicketsPurchased 
                 + AvTicketUse 
                 + AvAPUse 
                 + DistancefromUO, 
                 data=DMA_Attributes, 
                 method="nearest",
                 distance="mahalanobis",
                 ratio=1, 
                 replace = TRUE)
