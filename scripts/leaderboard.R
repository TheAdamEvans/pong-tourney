load('player.RData')
load('game.RData')

# assign players to groups
source('stat.R')
assignment = assignGroup(player)

# group games into matches
source('stat.R')
match = ddply(game, .var = 'matchID', .fun = matchFeature, assignment)

# calculate statistics to sort players
source('stat.R')
player = playerRank(player, match, game)

# generate leaderboard!! great for email updates
source('viz.R')
print(leaderboard(player, stat = 'meanScoreDiff', advancingPlayer = 8))

# # look at which matches have been played
# source('viz.R')
# print(matchupFacet(assignment, match))
