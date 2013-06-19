#explaining away, non-linguistic
# workaround since Church too slow:

num.players = 12

# bob prior
num.sims = 10000
bob.str.prior = rep(NA, num.sims)
for (i in 1:10000) {
	male.str = rnorm(1, 0, 2)
	female.str = rnorm(1, 0, 2)
	bob.gender.str = sample(c(male.str, female.str), 1)
	bob.str = rnorm(1, bob.gender.str, 1)
	bob.str.prior[i] = bob.str
}

# non-ling-version: jim.str unknown
num.sims = 10000
num.accepted = 0
bob.str.given.bob.wins.jim.unknown = rep(NA, num.sims)
while (num.accepted < num.sims) {
	male.str = rnorm(1, 0, 2)
	female.str = rnorm(1, 0, 2)
	player.gender.strengths = sample(c(male.str, female.str), num.players, replace=T)
	player.strengths = rep(NA, num.players)
	for (i in 1:num.players) {
		player.strengths[i] = rnorm(1, player.gender.strengths[i], 1)
	}
	bob.str = player.strengths[1]
	jim.str = player.strengths[2]
	bob.pulling = bob.str + rnorm(1, 0, .5)
	jim.pulling = jim.str + rnorm(1, 0, .5)
	if (bob.pulling > jim.pulling) {
		num.accepted = num.accepted + 1
		bob.str.given.bob.wins.jim.unknown[num.accepted] = bob.str 
	}
}

# non-ling-version: jim.str fixed, jim very weak
num.sims = 10000
num.accepted = 0
bob.str.given.jim.minus.3 = rep(NA, num.sims)
while (num.accepted < num.sims) {
	male.str = rnorm(1, 0, 2)
	female.str = rnorm(1, 0, 2)
	player.gender.strengths = sample(c(male.str, female.str), num.players, replace=T)
	player.strengths = rep(NA, num.players)
	for (i in 1:num.players) {
		player.strengths[i] = rnorm(1, player.gender.strengths[i], 1)
	}
	player.strengths[2] = -4
	bob.str = player.strengths[1]
	jim.str = player.strengths[2]
	bob.pulling = bob.str + rnorm(1, 0, .5)
	jim.pulling = jim.str + rnorm(1, 0, .5)
	if (bob.pulling > jim.pulling) {
		num.accepted = num.accepted + 1
		bob.str.given.jim.minus.3[num.accepted] = bob.str 
	}
}
plot(density(bob.str.given.bob.wins.jim.unknown), col='red', lwd=3, lty=2, xlab="Bob's strength")
lines(density(bob.str.prior), col='black', lwd=3, lty=1)
lines(density(bob.str.given.jim.minus.3), col='blue', lwd=3, lty=3)
legend('topright', c("No observations (prior)", "Observation: Bob beats Jim", "Observation: Bob beats Jim, Jim weak"), lty=c(1, 2, 3), lwd=3, cex=.9, col=c('black', 'red', 'blue'), text.col=c('black', 'red', 'blue'))

# ling version - jim weak condition differs:
num.sims = 10000
num.accepted = 0
bob.str.given.jim.weakest = rep(NA, num.sims)
while (num.accepted < num.sims) {
	male.str = rnorm(1, 0, 2)
	female.str = rnorm(1, 0, 2)
	player.gender.strengths = sample(c(male.str, female.str), num.players, replace=T)
	player.strengths = rep(NA, num.players)
	for (i in 1:num.players) {
		player.strengths[i] = rnorm(1, player.gender.strengths[i], 1)
	}
	bob.str = player.strengths[1]
	jim.str = player.strengths[2]
	bob.pulling = bob.str + rnorm(1, 0, .5)
	jim.pulling = jim.str + rnorm(1, 0, .5)
	if (jim.str == min(player.strengths) && bob.pulling > jim.pulling) {
		num.accepted = num.accepted + 1
		bob.str.given.jim.weakest[num.accepted] = bob.str 
	}
}
plot(density(bob.str.given.bob.wins.jim.unknown), col='red', lty=2, lwd=3, xlab="Bob's strength")
lines(density(bob.str.prior), col='black', lty=1, lwd=3)
lines(density(bob.str.given.jim.weakest), col='blue', lty=3, lwd=3)
legend('topright', c("After utterances 1-3", 'After utterances 1-4', 'After utterances 1-5'), lty=c(1, 2, 3), lwd=3, cex=.9, col=c('black', 'red', 'blue'), text.col=c('black', 'red', 'blue'))


