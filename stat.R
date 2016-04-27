library(plyr)

hasPlayed <- function(player1, player2, match) {
  matchPlayerSet = apply(match, 1, function(m) {
    # return TRUE if there is a recorded match of this set of players
    setequal( c(player1, player2), c(m['matchWinner'],m['matchLoser']) )
  })
  return(max(matchPlayerSet))
}

assignGroup <- function(player, playerPerGroup = 4) {

  # calculate the number of groups
  groups = floor(length(player) / playerPerGroup)

  # shuffle players
  # might want to be careful with the seed
  set.seed(2015)
  resampled = sample(player, length(player), replace=FALSE)
  
  # assign players to groups
  assignment = list()
  for (i in 1:length(player)) {
    assignment[[i]] = data.frame(
      player = resampled[i],
      groupNum = (i-1)%%groups+1)
  }
  assignment = do.call(rbind, assignment)
  assignment = assignment[ order(assignment[,'groupNum'], assignment[,'player']), ]
  
  # print the groups
  for (g in 1:groups) {
    grp = assignment[assignment$groupNum==g,'player']
    print(paste("GROUP",g,":  ",paste(grp,collapse=', ')))
  }
  
  return(assignment)
}

matchupBuilder <- function(group, assignment, match) {
  
  # get all the players in this group
  playerGroup = as.character(assignment[assignment$groupNum==group,'player'])
  matchup = as.data.frame(t(combn(playerGroup,2)))
  names(matchup) = c('Player1','Player2')
  
  # figure out which matches have been played
  matchup$hasPlayed = apply(matchup, 1, function(match) { hasPlayed(match[1],match[2], match) })
  
  # length of all 1v1 combinations
  combinations = dim(matchup)[1]

  # calculate the polygon layout
  matchup$Wi = 2*pi*(1:combinations)/combinations
  matchup$xPos = cos(matchup$Wi)
  matchup$yPos = sin(matchup$Wi)

  # informative labeling
  matchup$groupNum = as.integer(group)
  matchup$groupLabel = paste("GROUP",group)
  matchup$matchupLabel = apply(matchup, 1, function(p) { paste(p[1],"\nvs\n",p[2]) })
  
  return(matchup)
}

matchFeature <- function(games, assignment) {

  matchID = unique(games['matchID'])

  # count how many games were won
  win = sort(table(games$winner), decreasing = TRUE)

  # sanity checks
  if (length(win) > 2 ){ warning(paste(matchID,"had greater than two winners!! ????")) }
  if (length(win) > 1) { if (win[1]==win[2]) { warning(paste(matchID,"was a TIE")) } }
  if (win[1] < 3) { warning(paste(matchID,"fell short of best-of-5")) }
  
  # figure out who won and lost
  matchWinner = as.character(names(which.max(win)))
  matchLoser = max(games$reportedMatchLoser)

  # was this a sanctioned match?
  w = assignment$player==matchWinner
  l = assignment$player==matchLoser
  sameGroup = assignment[w,'groupNum'] == assignment[l,'groupNum']
  if((length(sameGroup) > 0) && sameGroup) {
    groupNum = assignment[w,'groupNum']
  } else {
    warning(paste(matchID,"was played out-of-group"))
    groupNum = NA
  }
  
  # misc stats on the match
  maxGame = max(games['gameNum'])
  meanScore = mean(games$score)
  zeroGame = sum(games$origScore=='0')

  # dependent on the form used to collect data
  penultimate = as.character(POINTS-1)
  deuceGame = sum(games$origScore==paste0(penultimate,'+'))
  
  feature = cbind.data.frame(
    matchID, groupNum, matchWinner, matchLoser,
    maxGame, meanScore, deuceGame, zeroGame,
    stringsAsFactors = FALSE
  )
  return(feature)
}

playerStat <- function(player, match, game, POINTS) {

  exists = (player %in% match$matchWinner) | (player %in% match$matchLoser)

  if(!exists) {
    feature = NULL
  } else {

    matchWin = dim(match[match$matchWinner==player,])[1]
    matchLoss = dim(match[match$matchLoser==player,])[1]
    matchWLRatio = matchWin/matchLoss
    
    gameWin = dim(game[game$winner==player,])[1]
    gameLoss = dim(game[game$loser==player,])[1]
    gameWLRatio = gameWin/gameLoss
    
    relGame = game[game$winner==player|game$loser==player,]
    relGame$scoreDiff = (POINTS - relGame$score) * ifelse(relGame$winner==player,1,-1)
    meanScoreDiff = mean(relGame$scoreDiff)
    
    maxMatchID = max(game[game$winner==player|game$loser==player,'matchID'])

    feature = cbind.data.frame(
    player,
    matchWin, matchLoss, matchWLRatio,
    gameWin, gameLoss, gameWLRatio,
    meanScoreDiff, maxMatchID)
  }
  return(feature)
}

playerRank <- function(player, match, game) {

  # sum up player wins
  player = lapply(player, playerStat, match, game, POINTS = 11)
  player = do.call(rbind, player)
  player$netGame = player$gameWin - player$gameLoss
  player$netMatch = player$matchWin - player$matchLoss
  
  # print warning about slackers
  hasPlayed = !is.na(player$matchWLRatio)
  if (min(hasPlayed)==0) {
    bad = paste(player[!hasPlayed,'player'],collapse=', ')
    warning(paste(bad,"HAVE NOT PLAYED ANY GAMES"))
  }
  player = player[hasPlayed,]

  # sort players by decreasing performance
  player = player[order(
    -player['netMatch'], -player['matchWLRatio'], # won more matches
    -player['netGame'], -player['gameWLRatio'], # lost fewer games
    -player['meanScoreDiff'], # scored more points in games
    player['maxMatchID'] # finished matches first
    ) ,]
  player$rank = 1:dim(player)[1]
  
  return(player)
}

