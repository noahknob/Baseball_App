yahoo_scheduale_func <- function(){
  value <- c(-1:7)
  team.scheduale <- matrix(0 , 10 , 9)
  for (z in 1:9) {
    for (i in 1:9) {
      for (j in 1:9) {
        if (i + value[j] != 0) {
          if (i == j) {
            team.scheduale[i,(i + value[j]) %% 9 ] <- 10
          }
          else team.scheduale[i,(i + value[j]) %% 9 ] <- j
        }
        else next


      }
    }
    team.scheduale[10,z] <- (sum(1:10) - sum(team.scheduale[,z]))
  }
  team.scheduale[,9] <- 10:1
  return(team.scheduale)

}


full_scheduale <- function() {
  rep_sched <- matrix(0,10,21)
  team.scheduale <- yahoo_scheduale_func()
  for (i in 1:10) {
    rep_sched[i,] <- rep(team.scheduale[i,], length.out = 21)
  }
  return(rep_sched)
}

yahoo <- full_scheduale()


##################################################### Randomized scheduale develop ##########################################
############################################################# Kinda Crazy ###################################################


li

recursive.factorial <- function(x) {
  if (x == 0)    return(1)
  else           return(x * recursive.factorial(x - 1))

}


factorial.5 <- recursive.factorial(5)

row_col <- list(1:10,1:9)

matchup_test = matrix(0,length(team_id),length(team_id),dimnames = row_col)


recurssive.matchup <- function(team_id=1:10, matchup = matrix(0,length(team_id),length(team_id),dimnames = row_col)) {

  if (length(team_id) == 2) {
    matchup[team_id[1],1] <- team_id[2]
    matchup[team_id[2],1] <- team_id[1]
    return(matchup)

  }
  else {
    match <- sort(sample(team_id,2,replace = FALSE))

    matchup[match[1],1] <- match[2]
    matchup[match[2],1] <- match[1]
    team_id <- team_id[!team_id %in% match]
