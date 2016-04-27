library(RCurl)

whoLost <- function(gameWinner, matchWinner, matchLoser) {
  if (matchWinner == gameWinner) { return(matchLoser)
  } else { return(matchWinner) }
}

lengthen <- function(v, maxLen) {
  missing = maxLen - length(v)
  v = c(v, rep('', missing))
  return(v)
}

readGoogleSheet <- function(url) {
  raw <- getURL(url, ssl.verifypeer=0L, followlocation=1L)
  d = strsplit(raw, "\r\n")[[1]]
  split_d = lapply(d, strsplit, ",")
  a = lapply(split_d, function(d) d[[1]])
  maxLen = max(sapply(a, length))
  adf = t(rbind.data.frame(lapply(a, lengthen, maxLen)))
  row.names(adf) = NULL
  colnames(adf) = adf[1,]
  
  adf = adf[28:90,c(-2,-15)]
  colnames(adf) = c('ts', 'matchWinner', 'g1Lscore', 'g2Winner', 'g2Lscore', 'g3Winner', 'g3Lscore', 'g4Winner', 'g4Lscore', 'g5Winner', 'g5Lscore', 'g1Winner', 'matchLoser')
  
  return(adf)  
}

getMatch <- function(nrmRow) {
  #nrmRow = rawData[1,]
  nrmRow = nrmRow[nrmRow!=""]
  playDate = as.Date(nrmRow['ts'],format = "%m/%d/%Y")
  matchID = gsub('[/: ]','',nrmRow['ts'])
  
  gameResult = list()
  gIndex = 1
  while (length(nrmRow[grepl(gIndex,names(nrmRow))]) > 0) {
    game = nrmRow[grepl(gIndex,names(nrmRow))]
    gameNum = gIndex
    winner = game[paste0('g',gIndex,'Winner')]
    origScore = game[paste0('g',gIndex,'Lscore')]

    if (grepl("[12]0\\+", origScore)) {
      adj_score = as.numeric(substr(origScore, 1, nchar(origScore)-1))-1
      score = as.character(adj_score)
    } else {
      score = origScore
    }
    
    loser = ""
    gameResult[[gIndex]] = data.frame(gameNum, winner, loser, origScore, score)
    gIndex = gIndex+1
  }
  matchResult = do.call(rbind, gameResult)
  matchResult$matchID = matchID
  matchResult$gameID = paste0(matchResult$matchID, matchResult$gameNum)
  
  #row.names(matchResult) <- NULL
  
  matchWinner = nrmRow['matchWinner']
  matchLoser = nrmRow['matchLoser']
  
  matchResult$loser = sapply(matchResult$winner, whoLost, matchWinner, matchLoser)
  
  matchResult$reportedMatchWinner = matchWinner
  matchResult$reportedMatchLoser = matchLoser
  
  return(matchResult)
}

getGameData <- function(rawData) {
  
  # getMatch returns a df with features of one game
  d = apply(rawData, 1, getMatch)
  d = do.call(rbind,d)
  
  d = data.frame(
    matchID = as.character(d$matchID),
    gameID = as.character(d$gameID),
    gameNum = as.integer(d$gameNum),
    winner = as.character(d$winner),
    loser = as.character(d$loser),
    origScore = as.character(d$origScore),
    score = as.numeric(as.character(d$score)),
    reportedMatchWinner = as.character(d$reportedMatchWinner),
    reportedMatchLoser = as.character(d$reportedMatchLoser),
    stringsAsFactors = FALSE
    )
  
  return(d)
}

# copied from google form (signup)
player = c(
  'DOUG KING','WILL HOLMES','BRIAN WATKINS','RYAN ROUNDY','MASON DOMINGUEZ',
  'AARON VAUGHN','RYAN LEMOINE','ALEX DITULLIO','JEREMY PLICHTA','DAVID KAUFMAN',
  'ADAM OLABY','SAURABH SODANI','MATT DITULLIO','JAI PRABHAKARAN',
  'BEN KATZ-MOSES','KEN WINTER','DIANA HOU','KYLE PERUSKI','ADAM EVANS',
  'ROBERT GREENBERG','TYLER JANES','MO CHEN','ADAM PICCONE',
  'BRIAN MONROE','MARCEL SCIOVILLE','DAVE MITCHELL','SEAN BROOKS','JOSEPH BENJAMIN',
  'CHRIS DISTLER','DAVE MORGAN','DAVID BAUR','JASON MEYER','JASON SAUER',
  'JEREMY MEADE','ALEX SAHYOUN','LE ZHANG','JOE STECH','JARED CARR','STEVEN BOGACZ',
  'EUGENE KIM','NATHAN PR','ERIC ANDREWS','IAN COLWELL','NIHAR SANGHVI')
save('player', file = 'player.RData')

# get data from the google sheet
url = 'https://docs.google.com/spreadsheets/d/1Ej_cv-awG6NEaDI80aH8TD8cq9D2BMVN-eHfhj5DYDI/pub?output=csv'
rawData = readGoogleSheet(url)

# normalize the game data
game = getGameData(rawData)
save('game', file = 'game.RData')
