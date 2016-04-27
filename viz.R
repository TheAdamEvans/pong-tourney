library(ggplot2)

leaderboard <- function(player, stat, advancingPlayer) {
  
  # find integer limits of stat
  top = ceiling(max(player[,stat]))
  bottom = floor(min(player[,stat]))

  # bold players advancing to elimination round
  boldPlayer = c(
  rep('plain', max(length(player$player) - advancingPlayer,0)),
  rep('bold', advancingPlayer)
  )

  # create labels and annotations
  player[,'rankLabel'] = paste0("(",player[,'rank'],")")
  player[,'record'] = paste0(player[,'gameWin'],"-",player[,'gameLoss'])
  player[,'label'] = paste(player[,'player'], player[,'rankLabel'], player[,'record'])
  player[,'label'] = reorder(player[,'label'], -player[,'rank'])

  # stat can be any numeric column of player 
  # 'meanScoreDiff' is probably the best one
  p = ggplot(player, aes_string(x = 'label', y=stat))

  # draw a vertical line in the middle
  p = p +
    geom_hline(yintercept=bottom:top, color='white') +
    geom_hline(yintercept=0, color='black', lty = 2) +
    coord_flip()
  
  p = p + geom_point(stat='identity')
  
  p = p + scale_y_continuous(breaks=bottom:top)
  
  p = p + labs(x='',y='Average Point Differential')

  p = p +
    theme(title = element_text(colour = "black", size = 10),
          axis.text.x = element_text(colour = "black", size = 8),
          axis.text.y = element_text(colour = "black", face = boldPlayer, size = 8),
          axis.title.y = element_text(colour = "black", size = 10, face = 'plain', vjust = 1),
          axis.title.x = element_text(colour = "black", size = 10, face = 'plain', vjust = -0.5),
          panel.grid = element_blank()
    )
  
  return(p)
}

matchupFacet <- function(assignment, match, scalingParameter = 1.4) {
  
  # given the group assignment layout out all the matches 
  matchup = lapply(1:max(assignment$groupNum), matchupBuilder, assignment, match)
  matchup = do.call(rbind, matchup)
  
  matchup$groupLabel = reorder(matchup$groupLabel, matchup$groupNum)
  
  # relative text position precalculated with matchupBuilder
  p = ggplot(matchup, aes(x = xPos, y = yPos, color = hasPlayed))
  p = p + geom_text(aes_string(label='matchupLabel'))
  
  p = p +
    scale_x_continuous(limits=c(-scalingParameter, scalingParameter)) +
    scale_y_continuous(limits=c(-scalingParameter, scalingParameter))
  
  p = p + scale_colour_gradient(low='black', high='grey')

  p = p +
    labs(x='',y='')+
    theme(
      legend.position = 'none',
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
    )

  # facet wrap on groups :)
  p = p + facet_wrap( ~ groupLabel, nrow = 4)
  
  return(p)
}
