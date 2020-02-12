# Expected goals functions

calculateScore <- function(home, away) {
  # Set score to nil-nil for start of game
  homeGoals <- 0
  awayGoals <- 0
  
  # Need a function in our function
  # runs runif(1) test for goals in a list
  testShots <- function(shots) {
    # Start goal count at 0
    goal <- 0
    
    # If a shot goes in, add a goal
    for (shot in shots) {
      if(runif(1) <= shot){
        goal <- goal + 1
      }
    }
    #Finally, return the number of goals
    return(goal)
  }
  #Run the above formula for home and away lists
  homeGoals = testShots(home)
  awayGoals = testShots(away) 
  score <- c(homeGoals, awayGoals)
  
  #Return the score
  return(score)
}

calculateWinner <- function(home, away) {
  # Set score to nil-nil for start of game
  homeGoals <- 0
  awayGoals <- 0
  
  # Need a function in our function
  # runs runif(1) test for goals in a list
  testShots <- function(shots) {
    # Start goal count at 0
    goal <- 0
    # If a shot goes in, add a goal
    for (shot in shots) {
      if(runif(1) <= shot){
        goal <- goal + 1
      }
    }
    #Finally, return the number of goals
    return(goal)
  }
  #Run the above function for home and away lists
  homeGoals = testShots(home)
  awayGoals = testShots(away) 
  
  #Return the score
  if (homeGoals > awayGoals) {
    return("home")
  } else if (awayGoals > homeGoals) {
    return("away")
  } else {
    return("draw")
  }
}

calculateChance <- function(team1, team2) {
  home <- 0
  away <- 0
  draw <- 0
  
  # Run 10,000 times
  for (i in 1:10000) {
    matchWinner <- calculateWinner(team1, team2)
    if (matchWinner == "home") {
      home <-  home + 1
    } else if (matchWinner == "away") {
      away <-  away + 1
    } else {
      draw <-  draw + 1
    }
  }
  home <-  home/100
  away <-  away/100
  draw <-  draw/100
  print(paste0("Over 10,000 games home wins ", home, "% of games. Away wins ", away, "% of games. ", draw, "% of games end in a tie"))
}

checkScoreOdds <- function(homeXG, awayXG, scoreCheck) {
  correct <- 0
  
  # Run 10,000 times
  for (i in 1:10000) {
    score <- calculateScore(homeXG, awayXG)
    
    if (score[1] == scoreCheck[1] && score[2] == scoreCheck[2]) {
      correct <-  correct + 1
    } 
  }
  return(correct/10000)
}

everyScore <- function(homeTeam, awayTeam) {
  df <- data.frame("HomeScore" = as.integer(character()), 
                   "AwayScore" = as.integer(character()))
  for (i in 1:10000) {
    score <- calculateScore(homeTeam, awayTeam)
    df <- rbind(df, score) 
  }
  return(df)
}
