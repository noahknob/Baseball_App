li

recursive.factorial <- function(x) {
  if (x == 0)    return (1)
  else           return (x * recursive.factorial(x-1))
}


factorial.5 <- recursive.factorial(5)

row_col <- list(1:10,1:9)

matchup_test= matrix(0,length(team_id),length(team_id),dimnames =row_col)


recurssive.matchup <- function(team_id=1:10, matchup = matrix(0,length(team_id),length(team_id),dimnames =row_col)) {

  if (length(team_id)==2) {
    matchup[team_id[1],1] <- team_id[2]
    matchup[team_id[2],1] <- team_id[1]
       return(matchup)
  }
  else {
    match <- sort(sample(team_id,2,replace = FALSE))

    matchup[match[1],1] <- match[2]
    matchup[match[2],1] <- match[1]
    team_id <- team_id[!team_id %in% match]
        return (recurssive.matchup(team_id,matchup))
  }
}
row_col <- list(1:10,1:9)
matchup_test = matrix(0,10,ncol=9,dimnames=row_col)


week_vec=1

recurssive.matchup <- function(team_id=1:10, matchup = matrix(0,length(team_id),9,dimnames =row_col),week_vec=1) {

  if (length(week_vec)==1) {
    if (length(team_id)==2) {
    matchup[team_id[1],length(week_vec)] <- team_id[2]
    matchup[team_id[2],length(week_vec)] <- team_id[1]
    team_id <- 1:10
    return (recurssive.matchup(team_id,matchup,week_vec=1:(length(week_vec)+1)))
    }
    else {
      match <- sort(sample(team_id,2,replace = FALSE))

      matchup[match[1],length(week_vec)] <- match[2]
      matchup[match[2],length(week_vec)] <- match[1]
      team_id <- team_id[!team_id %in% match]
      return (recurssive.matchup(team_id,matchup,week_vec))
    }

  }

  else {
    team_id <- 1:10
    flag <- TRUE
    value <- TRUE
     while (flag==TRUE){
      if (all(matchup[,length(week_vec)]!=0)){
        flag <- FALSE
        value <- FALSE
        }
      while (value==TRUE){
        if (length(team_id)==0){
          value <- FALSE
          break
        }

        match <- sort(sample(team_id,2,replace = FALSE))
        if (all(matchup[match[1],1:(length(week_vec)-1)]!= match[2])){
          matchup[match[1],length(week_vec)] <- match[2]
          matchup[match[2],length(week_vec)] <- match[1]
          team_id <- team_id[!team_id %in% match]
          if (length(team_id)==2){
            if (any(matchup[team_id[1],]==team_id[2])){
              team_id <- sort(c(team_id,match))
            }
            else {
              matchup[match[1],length(week_vec)] <- match[2]
              matchup[match[2],length(week_vec)] <- match[1]
              team_id <- team_id[!team_id %in% match]
            }
          }
          }
      }



    }

    return (recurssive.matchup(team_id,matchup,week_vec=1:(length(week_vec)+1)))
  }

   if (week_vec==1:9){
    return (matchup)
  }
}

scheduale <- recurssive.matchup()



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


x <- 1:10
for (i in 1:100000) {
  newi <- (i %% length(x)) + 1
  x[newi] <- i
}

t <- 11 %% 10 + 1
