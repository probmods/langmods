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
num.matches = 3
players.per.team = 4
players = 1:12
# jane = 1
n.sims = 100000
played = sapply(1:n.sims, FUN=function(t){
		players = as.vector(sapply(1:num.matches, FUN=function(i){sample(players, size=players.per.team*2, replace=F)}))
		return(length(which(players==1)))
	})

#L0
par(mfrow=c(1,2))
prior.prob = table(played)/n.sims
l0.probabilities.given.some = c(0, prior.prob[2:4]/sum(prior.prob[2:4]))
l0.probabilities.given.all = c(0, 0, 0, 1)
plot(prior, type='b', pch=19, col='black', lwd=3, xlab="Number of matches Jane played in", ylab="Probability according to literal listener", xlim=c(0,3), ylim=c(0,1), main="Literal listener")
lines(0:3, l0.probabilities.given.some, type='b', lty=2, lwd=3, col='red')
lines(0:3, l0.probabilities.given.all, type='b', lty=3, lwd=3, col='blue')
legend(x=0, y=.6, c('Prior', 'Some', "All"), lty=c(1,2,3), col=c('black','red', 'blue'), text.col=c('black','red', 'blue'), cex=2)

#L1 probs
L1.posterior.some = prior * c(0,utt.some)
L1.posterior.some = L1.posterior.some/sum(L1.posterior.some)
L1.posterior.all = prior * c(0,utt.all)
L1.posterior.all = L1.posterior.all/sum(L1.posterior.all)
plot(prior, type='b', pch=19, col='black', lwd=3, xlab="Number of matches Jane played in", ylab="Probability according to reflective listener", xlim=c(0,3), ylim=c(0,1), main="Pragmatic listener")
#plot(0:3, L1.posterior.some, xaxt='n', type='b', pch=19, col='red', lwd=3, xlab="Number of matches Jane played in", ylab="Probability according to reflective listener", ylim=c(0,1), main="Pragmatic listener")
axis(1, 0:3)
lines(L1.posterior.some, type='b', lty=2, pch=19,lwd=3, col='red')
lines(L1.posterior.all, type='b', lty=3, pch=19,lwd=3, col='blue')
legend(x=0, y=.6,  c('Prior','Some', "All"), lty=c(1,2,3), col=c('black','red', 'blue'), text.col=c('black','red', 'blue'), cex=2)

# S1 probs
par(mfrow=c(1,1))
norm = l0.probabilities.given.all[4]+l0.probabilities.given.some[4]
utt.some = c(1, 1, l0.probabilities.given.some[4]/norm)
utt.all = c(0, 0, l0.probabilities.given.all[4]/norm)
plot(1:3, utt.some, xaxt='n', type='b', pch=19, col='red', lwd=3, xlab="Number of matches Jane played in", ylab="Probability that speaker chooses utterance", ylim=c(0,1), main="Speaker", xlim=c(-1,3))
axis(1, 0:3)
lines(utt.all, type='b', lty=2, lwd=3, col='blue')
lines(0:3, c(1, 0, 0, 0), col='black', lwd=3, lty=3)
#legend(x=0, y=.6, c('None', 'Some', "All"), lty=c(3, 1,2), col=c('black', 'red', 'blue'), text.col=c('black', 'red', 'blue'), cex=2)
legend('bottomleft', c('None', 'Some', "All"), lty=c(3, 1,2), col=c('black', 'red', 'blue'), text.col=c('black', 'red', 'blue'), cex=2)







