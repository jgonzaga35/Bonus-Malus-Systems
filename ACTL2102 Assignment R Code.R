#########################################################################
##                ACTL2102 ASSIGNMENT 2020 T2                          ##
#########################################################################
##                         TASK 1                                      ##
#########################################################################
#########################################################################
## Part 1: Determine the probability transition matrices for the three ##
## different schemes, for each group of policyholder.                  ##
#########################################################################

# Install and load packages.
install.packages("dplyr"); library(dplyr)
install.packages("ggplot2"); library(ggplot2)
install.packages("markovchain"); library(markovchain)
install.packages("moments"); library(moments)

# A function that returns the probability transition matrix
# for a specified scheme, age group and claim frequency.  
getScheme <- function(scheme, poi_young, poi_old) {
  
    # Declare an empty list to contain schemes for each scheme and age group.
    schemes <- list()
    
    # Define probabilities for each age group.
    ########## YOUNG #####################  ######## OLD ###############
    # Probability of 1 or more claims: 
    pp1 <- 1 - ppois(0, poi_young);        qq1 <- 1 - ppois(0, poi_old);
    # Probability of 0 claims:
    dp0 <- dpois(0, poi_young);            dq0 <- dpois(0, poi_old);
    # Probability of 2 or more claims:
    pp2 <- 1 - ppois(1, poi_young);        qq2 <- 1 - ppois(1, poi_old);
    # Probability of 1 claim:
    dp1 <- dpois(1, poi_young);            dq1 <- dpois(1, poi_old);
    
    # Assign matrices to the list.
    # Scheme 1: schemes[[1]] (young) and schemes [[4]] (old)
    # Scheme 2: schemes[[2]] (young) and schemes [[5]] (old)
    # Scheme 3: schemes[[3]] (young) and schemes [[6]] (old)
    
    schemes[[1]] <- new("markovchain", states = c("State -2","State -1",
                                                  "State 0","State 1",
                                                  "State 2"), 
                                transitionMatrix = 
                                matrix(data = c(pp1, dp0,   0,   0,   0,
                                                pp1,   0, dp0,   0,   0,  
                                                pp2, dp1,   0, dp0,   0,
                                                0  , pp2, dp1,   0, dp0,
                                                0  ,   0, pp2, dp1, dp0), 
                                        byrow = T, nrow = 5), 
                               name = "Scheme 1: Young")
    schemes[[4]] <- new("markovchain", states = c("State -2","State -1",
                                                  "State 0","State 1",
                                                  "State 2"), 
                         transitionMatrix = 
                           matrix(data = c(qq1, dq0,   0,   0,   0,
                                           qq1,   0, dq0,   0,   0,  
                                           qq2, dq1,   0, dq0,   0,
                                           0  , qq2, dq1,   0, dq0,
                                           0  ,   0, qq2, dq1, dq0), 
                                  byrow = T, nrow = 5), 
                         name = "Scheme 1: Old")
    schemes[[2]] <- new("markovchain", states = c("State -1","State 0",
                                                  "State 1"), 
                       transitionMatrix = 
                         matrix(data = c(pp1, dp0,   0, 
                                         pp1,   0, dp0,    
                                         pp2, dp1, dp0), 
                                byrow = T, nrow = 3), 
                       name = "Scheme 2: Young")
    schemes[[5]] <- new("markovchain", states = c("State -1","State 0",
                                                  "State 1"), 
                         transitionMatrix = 
                           matrix(data = c(qq1, dq0,   0, 
                                           qq1,   0, dq0,    
                                           qq2, dq1, dq0), 
                                  byrow = T, nrow = 3), 
                         name = "Scheme 2: Old")
    schemes[[3]] <- new("markovchain", states = c("State -3","State -2",
                                                  "State -1",
                                                  "State 0","State 1",
                                                  "State 2","State 3"), 
                       transitionMatrix = 
                         matrix(data = c(pp1, dp0,   0,   0,    0,   0,   0, 
                                         pp1,   0, dp0,   0,    0,   0,   0,    
                                         pp2, dp1,   0, dp0,    0,   0,   0,  
                                           0, pp2, dp1,   0,  dp0,   0,   0,
                                           0,   0, pp2, dp1,    0, dp0,   0,
                                           0,   0,   0, pp2,  dp1,   0, dp0,
                                           0,   0,   0,   0,  pp2, dp1, dp0), 
                                byrow = T, nrow = 7), 
                       name = "Scheme 3: Young")
    schemes[[6]] <- new("markovchain", states = c("State -2","State -1",
                                                  "State 0","State 1",
                                                  "State 2"), 
                       transitionMatrix = 
                         matrix(data = c(qq1, dq0,   0,   0,   0,
                                         qq1,   0, dq0,   0,   0,  
                                         qq2, dq1,   0, dq0,   0,
                                         0  , qq2, dq1,   0, dq0,
                                         0  ,   0, qq2, dq1, dq0), 
                                byrow = T, nrow = 5), 
                       name = "Scheme 3: Old")
  
    # Return the specified scheme.
    schemes[[scheme]]
}

showMatrices <- function() {
    for (i in 1:6) {
        show(getScheme(i, 0.25, 0.15))
    }
}
#########################################################################
## Task 1 Part 2: Simulation                                           ##
#########################################################################

# Function to simulate 1000 times the number of policyholders 
# in each state and provides summary statistics for the gross premium.
simulation <- function(output) {
    
    # Set seed to be able to reproduce results.
    set.seed(5255845)
  
    # Import first 10,000 rows of claimsdata.csv file.
    df <- read.csv("claimsdata.csv", header=TRUE);
    data <- df[1:10000,]
    
    # Create dataframe to store results for each simulation.
    x1 = x2 = x3 = x4 = x5 = x6 = x7 = rep(0, 1000)
    results = as.data.frame(cbind(x1, x2, x3, x4, x5, x6, x7))
    colnames(results) <- c( "State -2", "State -1", "State 0",
                           "State 1", "State 2", 
                           "Gross Premium", "Num of Claims")
    
    # Arrange data by age so rows 1:5000 are young and rows 5001:10000 are old.
    data <- arrange(data, data$age)
  
    # Simulate for 1000 times.
    for (i in 1:1000) {
      # Add extra columns to dataframe to store simulation results.
      data$ncdlevel21 = data$claim_num = data$premium = rep(0, 10000)
      
      # Generate random claim numbers for each age group.
      # Store claim numbers to corresponding age group.
      young_claims <- rpois(5000,0.25)
      old_claims <- rpois(5000, 0.15)
      data$claim_num[1:5000] <- young_claims
      data$claim_num[5001:10000] <- old_claims
      
      # Increment ncdlevel21 rows based on 2021 claim numbers 
      # and 2020 discount level.
      data$ncdlevel21[which(data$claim_num == 0)] = 
          data$ncdlevel20[which(data$claim_num == 0)] + 1
      
      data$ncdlevel21[which(data$ncdlevel21 >= 2)] = 2
      
      data$ncdlevel21[which(data$claim_num == 1)] = 
          data$ncdlevel20[which(data$claim_num == 1)] - 1
      
      data$ncdlevel21[which(data$claim_num >= 2)] =
          data$ncdlevel20[which(data$claim_num >= 2)] - 2
      
      data$ncdlevel21[which(data$ncdlevel21 <= -2)] = -2
      
      # Set base premiums for each age group.
      data$premium[1:5000] = 400
      data$premium[5001:10000] = 300
      
      # Multiply each policyholder's base premium by corresponding
      # discount factor.
      data$premium[which(data$ncdlevel21 == -2)] = 
          data$premium[which(data$ncdlevel21 == -2)]*1.2
      
      data$premium[which(data$ncdlevel21 == -1)] = 
		data$premium[which(data$ncdlevel21 == -1)]*1.1
      
      data$premium[which(data$ncdlevel21 == 1)] = 
		data$premium[which(data$ncdlevel21 == 1)]*0.9
      
      data$premium[which(data$ncdlevel21 == 2)] = 
		data$premium[which(data$ncdlevel21 == 2)]*0.8
      
      # Sum total number of policyholders in each state.
      neg_2 = sum(data$ncdlevel21 == -2)
      neg_1 = sum(data$ncdlevel21 == -1)
      zero = sum(data$ncdlevel21 == 0)
      pos_1 = sum(data$ncdlevel21 == 1)
      pos_2 = sum(data$ncdlevel21 == 2)
      # Calculate gross premium and record results.
      gross_premium = sum(data$premium)
      total_claims = sum(data$claim_num)
      results[i ,] = c(neg_2, neg_1, zero, pos_1, pos_2, gross_premium, total_claims);
    
    }
    # Provide summary statistics.
    if (output == "summary") {
        summary(results)
    }
    else if (output == "hist") {
        hist(results$`Gross Premium`, xlab = "Total Gross Premium",
             main = "Distribution of Total Gross Premium", 
             col = "blue", breaks = 100)
    } else if (output == "donut") {
        states <- c(mean(results$`State -2`), mean(results$`State -1`),
                    mean(results$`State 0`), mean(results$`State 1`),
                    mean(results$`State 2`))
        shades <- c("turquoise", "lightblue", "skyblue", "blue","darkblue")
        pie(states, labels = colnames(results), col = shades, main = 
                "Mean Distribution of Policyholders in Discount Classes")
        symbols(0, 0, circles = 0.1, add=TRUE, bg="white")
    }
}


    
#########################################################################
##                         TASK 2                                      ##
#########################################################################
#########################################################################
# 1. Plot the Loimaranta efficiency of the three different schemes     ##
# as a function of lambda (the claim frequency) and explain results.   ## 
#########################################################################

# Returns mean premium charged for a policyholder with
# claim frequency "lambda" in scheme number "x"
P <- function(scheme, lambda) {
  
  # Declare and initialise starting conditions.
  poi_young = lambda
  poi_old = lambda
  baseYoung = 400
  baseOld = 300
  
  # Generate transition probability matrices for old
  # age group and young age group using equivalent lambdas.
  matrixYoung <- getScheme(scheme, poi_young, poi_old)
  matrixOld <- getScheme(scheme + 3, poi_young, poi_old)
  
  # Find the number of states in the scheme's transition matrix.
  total_states_young = ncol(matrixYoung@transitionMatrix)
  seq_of_states_young = 1:total_states_young
  total_states_old = ncol(matrixOld@transitionMatrix)
  seq_of_states_old = 1:total_states_old
  
  # Find discount levels.
  discount_factor_young <- 0.1*((total_states_young - 1)/2)
  discount_factor_old <- 0.1*((total_states_old - 1)/2)
  
  maxYoung = 1 + discount_factor_young
  minYoung = 1 - discount_factor_young
  maxOld = 1 + discount_factor_old
  minOld = 1 - discount_factor_old
  
  discountYoung <- seq(maxYoung, minYoung, by = -0.1)
  premiumYoung = discountYoung*baseYoung
  discountOld <- seq(maxOld, minOld, by = -0.1)
  premiumOld = discountOld*baseOld
  
  # Compute mean premium.
  mean_premium_young = sum(steadyStates(matrixYoung)[seq_of_states_young]*premiumYoung)
  mean_premium_old = sum(steadyStates(matrixOld)[seq_of_states_old]*premiumOld)
  mean_premium = (mean_premium_young + mean_premium_old)/2
  mean_premium
}

# Function to approximate first derivative of function P.
derivativeP <- function(scheme, lambda, delta) {
    result = (P(scheme, lambda + delta) - P(scheme, lambda))/delta
    return(result)
}

# Computes Loimaranta efficiency given scheme and lambda with breaks of 0.01.
loimaranta_efficiency <- function(scheme, lambda, delta) {
    result = (lambda/P(scheme, lambda))*derivativeP(scheme, lambda, delta)
    result
}

# Plot the Loimaranta Efficiency for each scheme for lambda from 0 to 1.
plot_efficiency <- function(from, to, breaks) {
    lambda <- seq(from, to, by = breaks)
    scheme1 = scheme2 = scheme3 = c()
    efficiency <- as.data.frame(cbind(scheme1, scheme2, scheme3))
    for (scheme in 1:3) {
      row = 1
      for (i in lambda) {
        efficiency[row, scheme] = loimaranta_efficiency(scheme, i, breaks)
        row = row + 1
      }
    }
    mydata <- data.frame(cbind(lambda, efficiency))
    ggplot(data=mydata)+
      geom_line(mapping=aes(y=efficiency$V1, x = lambda,color="Scheme 1"),size=1 ) +
      geom_line(mapping=aes(y=efficiency$V2, x = lambda,color="Scheme 2"),size=1) +
      geom_line(mapping=aes(y=efficiency$V3, x = lambda, color="Scheme 3"),size=1) +
      scale_color_manual(values = c(
        'Scheme 1' = 'red', 'Scheme 2' = 'green', 'Scheme 3' = 'darkblue')) +
      labs(color = 'Scheme Number') +
      ylab("Efficiency") + xlab("Lambda") + theme_minimal()+
      ggtitle("Loimaranta Efficiency") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = c(0.9, 0.9), panel.grid = element_blank())
}

#####################################################################
# 2. Compare the long-run profitability.                            #
#####################################################################

# A function to compare the long-run profitability for each scheme.
# Profitability is defined as the ratio of total gross premium to 
# total claim costs.
compare_profitability <- function(from, to, by) {

  lambda = seq(from,to, by);
  scheme1 = scheme2 = scheme3 = c()
  profitabilities <- as.data.frame(cbind(scheme1, scheme2, scheme3))
  for (scheme in 1:3) {
    row = 1
    for (i in lambda) {
      # Obtain the specified scheme's transition probability matrices.
      youngScheme <- getScheme(scheme, i, i)
      # Assume that claim frequency for older age group is lower by 0.05.
      oldScheme <- getScheme(scheme + 3, i, i - 0.1)
      
      # Calculate the long run proportions of each state, multiply by number 
      # of policyholders.
      steadyStatesYoung <- steadyStates(youngScheme)*5000
      steadyStatesOld <- steadyStates(oldScheme)*5000
      
      # Find total number of states/levels for young and old policyholders.
      total_states_young = ncol(youngScheme@transitionMatrix)
      sequenceYoung = 1:total_states_young
      total_states_old = ncol(oldScheme@transitionMatrix)
      sequenceOld = 1:total_states_old
      
      # Find number of discount levels in the given scheme and the premium 
      # charged in each level for each age group.
      basePrem_Young = 400
      basePrem_Old = 300
      
      m <- 0.1*((total_states_young - 1)/2)
      n <- 0.1*((total_states_old - 1)/2)
      maxYoung = 1 + m
      minYoung = 1 - m
      maxOld = 1 + n
      minOld = 1 - n
      
      # Store discount factors into vector and multiply by base premium
      # to obtain gross premium charged for each discount level.
      discountYoung <- seq(maxYoung, minYoung, by = -0.1)
      premiumYoung = discountYoung*basePrem_Young
      discountOld <- seq(maxOld, minOld, by = -0.1)
      premiumOld = discountOld*basePrem_Old
      
      # Multiply premium charged with corresponding proportion in each state
      # and add together to compute total profits.
      profitsYoung <- steadyStatesYoung*premiumYoung
      profitsOld <- steadyStatesOld*premiumOld
      profits <- sum(profitsYoung) + sum(profitsOld) 
      
      # Calculate expected claim costs from each age group.
      expenses <- i*5000*1000 + i*5000*1000
      profitabilities[row, scheme] = profits/expenses
      row = row + 1
    }
  }
  # Plot profitability graph for each scheme.
  mydata <- data.frame(cbind(lambda, profitabilities))
  ggplot(data=mydata)+
    geom_line(mapping=aes(y=profitabilities$V1, x = lambda,color="Scheme 1"),size=1 ) +
    geom_line(mapping=aes(y=profitabilities$V2, x = lambda,color="Scheme 2"),size=1) +
    geom_line(mapping=aes(y=profitabilities$V3, x = lambda, color="Scheme 3"),size=1) +
    geom_hline(mapping = aes(yintercept =1), color="red")+
    scale_color_manual(values = c(
      'Scheme 1' = 'red', 'Scheme 2' = 'green', 'Scheme 3' = 'darkblue')) +
    labs(color = 'Scheme Number') +
    ylab("Profitability Ratio")+ xlab("Claim Frequency") +
    theme_minimal() + ggtitle("Long Run Profitability") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = c(0.9, 0.9), panel.grid = element_blank())
}

# Functions to produce required output for each task.

# Print all the transition matrices.
showMatrices()
# Print summary statistics.
simulation("summary")
# Plot histogram of distribution of total gross premium.
simulation("hist")
# Plot donut graph of distribution of policyholders.
simulation("donut")
# Measure system time for simulation algorithm.
system.time(simulation("summary"))
# Plot Loimaranta Efficiency from Lambda = 0.05 to Lambda = 1, breaks of 0.01.
plot_efficiency(0.05,1, 0.01)
# Plot profitability from Lambda = 0.01 to Lambda = 1
# Old policyholders are assumed to have Lambda = Lambda(young) - 0.1
compare_profitability(0.11,1,0.01)


