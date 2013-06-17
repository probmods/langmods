# explaining away
n=10000

# workaround since Church too slow:
gender.str.1 = rnorm(n, 0, 2)
bob.str = rnorm(n, gender.str.1, 1)
bob.pulling = bob.str + rnorm(n, 0, .5)
gender.str.2 = rnorm(n, 0, 2)
jim.str = rnorm(n, gender.str.2, 1)
jim.pulling = jim.str + rnorm(n, 0, .5)
bob.wins = bob.pulling > jim.pulling
accepted.indices = which(bob.wins)
bob.posterior.strength.jim.unknown = bob.str[accepted.indices]

gender.str = rnorm(n, 0, 2)
bob.str = rnorm(n, gender.str, 1)
bob.pulling = bob.str + rnorm(n, 0, .5)
jim.str = -3
jim.pulling = jim.str + rnorm(n, 0, .5)
bob.wins = bob.pulling > jim.pulling
accepted.indices = which(bob.wins)
bob.posterior.strength.jim.weak = bob.str[accepted.indices]

plot(density(bob.posterior.strength.jim.unknown), col='red', lwd=3, lty=2, xlab="Bob's strength", ylab="Density", xlim=c(-5, 10))
lines(density(bob.posterior.strength.jim.weak), col='blue', lwd=3, lty=3)
lines(density(bob.str), col='black', lwd=3, lty=1)
legend('topright', c('No observations (prior)', 'Observation: Bob beats Jim', 'Observation: Bob beats Jim, Jim weak'), text.col=c('black', 'red', 'blue'), col=c('black', 'red', 'blue'), box.lwd=2, lty=c(1, 2, 3), lwd=3)

mean(bob.str)
mean(bob.posterior.strength.jim.unknown)
mean(bob.posterior.strength.jim.weak)