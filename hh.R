some = function(v) return(mean(v) > 0) # check at least one T in vector of booleans

#sim = function(players.per.team, matches.played, n.sims) {
sim.no.condition = function(n.sims) {
  num.accepted = 0
  counts = c(s=0, o=0)
  while (num.accepted < n.sims) {
    reading = sample(c('s', 'o'), 1)
    num.players = 12
  players.per.team = sample(1:6, 1)
  matches.played = sample(1:6, 1)
# from 2 teams to as many teams as possible given team size constraints
    players.in.matches = sapply(1:matches.played, FUN=function(i){return(sample(1:num.players, 2*players.per.team, replace=F))})
  sws.true = length(unique(as.vector(players.in.matches))) == num.players
  #  sws.true = length(unique(as.vector(players.in.matches))) > (num.players/2)

  ows.true = some(apply(players.in.matches, FUN=function(v){return(length(v) == num.players)}, MARGIN=2))
  #  ows.true = some(apply(players.in.matches, FUN=function(v){return(length(v) > num.players/2)}, MARGIN=2))
    if (reading == 's' && sws.true) {
      num.accepted = num.accepted + 1
      counts[1] = counts[1] + 1
    }
    if (reading == 'o' && ows.true) {
      num.accepted = num.accepted + 1
      counts[2] = counts[2] + 1
    }
  }
  return(counts)
}

no.condition = sim.no.condition(1000)

sim.fixing.n.matches = function(n.sims) {
  results = matrix(NA, nrow=(n.sims*10), ncol=3)
  rownames(results) = paste('sim', 1:(n.sims*10), sep='')
  colnames(results) = c('Reading', 'team.size', 'matches.played')
  for (matches.played in 1:10) {
    counter = (matches.played - 1) * n.sims
    num.accepted = 0
    while (num.accepted < n.sims) {
      reading = sample(c('s', 'o'), 1)
      num.players = 12
      players.per.team = sample(1:6, 1)
    # from 2 teams to as many teams as possible given team size constraints
      players.in.matches = sapply(1:matches.played, FUN=function(i){return(sample(1:num.players, 2*players.per.team, replace=F))})
    #  sws.true = length(unique(as.vector(players.in.matches))) == num.players
      sws.true = length(unique(as.vector(players.in.matches))) > (num.players/2)
    
    #  ows.true = some(apply(players.in.matches, FUN=function(v){return(length(v) == num.players)}, MARGIN=2))
      ows.true = some(apply(players.in.matches, FUN=function(v){return(length(v) > num.players/2)}, MARGIN=2))
      if (reading == 's' && sws.true) {
        num.accepted = num.accepted + 1
        results[counter + num.accepted,] = c('s', players.per.team, matches.played)
      }
      if (reading == 'o' && ows.true) {
        num.accepted = num.accepted + 1
        results[counter + num.accepted,] = c('o', players.per.team, matches.played)
      }
    }
  }
  results[,2] = as.numeric(results[,2])
  results[,3] = as.numeric(results[,3])
  return(results)
}

varying.matches = sim.fixing.n.matches(10000)
subj = rep(NA, 10)
obj = rep(NA, 10)
team.size = rep(NA, 10)
sws.team.size = rep(NA, 10)
ows.team.size = rep(NA, 10)
par(mfrow=c(1,2))
for (i in 1:10) {
  subj[i] = length(which(varying.matches[,1] == 's' & varying.matches[,3] == i))/length(which(varying.matches[,3] == i))
  obj[i] = length(which(varying.matches[,1] == 'o' & varying.matches[,3] == i))/length(which(varying.matches[,3] == i))
  team.size[i] = mean(as.numeric(varying.matches[which(varying.matches[,3]==i),2]))
  sws.team.size[i] = mean(as.numeric(varying.matches[which(varying.matches[,3]==i & varying.matches[,1]=='s'),2]))
  ows.team.size[i] = mean(as.numeric(varying.matches[which(varying.matches[,3]==i & varying.matches[,1]=='o'),2]))
}
par(mfrow=c(1,2))
plot(subj, type='l', col='red', ylim=c(.2,.7), xlab="Number of matches", ylab="Probability of reading", lwd=3, xaxt='n', lty=2)
axis(1, 1:10)
lines(obj, type='l', col='blue', lty=3, lwd=3)
legend('bottomleft', c("Subject wide scope", "Object wide scope"), text.col=c('red', 'blue'), lty=c(2,3), col=c('red', 'blue'), cex=.7)

plot(team.size, xaxt='n', col='black', ylim=c(2,6), xlab="Number of matches", ylab="Mean estimated team size", type='l', lwd=3)
axis(1, 1:10)
lines(sws.team.size, col='red', lty=2, lwd=3)
lines(ows.team.size, col='blue', lty=3, lwd=3)
legend('bottomleft', c("Marginal", "With subject wide scope", "With object wide scope"), text.col=c('black', 'red', 'blue'), lty=c(1,2,3), col=c('black', 'red', 'blue'), cex=.7)

# scalar implicature




