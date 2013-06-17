#explaining away, non-linguistic
# workaround since Church too slow:
gender.str.1 = rnorm(1000, 0, 2)
bob.str = rnorm(1000, gender.str.1, 1)
bob.pulling = bob.str + rnorm(1000, 0, .5)
gender.str.2 = rnorm(1000, 0, 2)
jim.str = rnorm(1000, gender.str.2, 1)
jim.pulling = jim.str + rnorm(1000, 0, .5)
bob.wins = bob.pulling > jim.pulling
accepted.indices = which(bob.wins)
bob.posterior.strength.jim.unknown = bob.str[accepted.indices]

gender.str = rnorm(1000, 0, 2)
bob.str = rnorm(1000, gender.str, 1)
bob.pulling = bob.str + rnorm(1000, 0, .5)
jim.str = -2
jim.pulling = jim.str + rnorm(1000, 0, .5)
bob.wins = bob.pulling > jim.pulling
accepted.indices = which(bob.wins)
bob.posterior.strength.jim.weak = bob.str[accepted.indices]

#plot(density(bob.str.prior), col='black')
plot(density(bob.str.prior), col='black')
lines(density(bob.posterior.strength.jim.unknown), col='red')
lines(density(bob.posterior.strength.jim.weak), col='blue')

# ling version - jim weak condition differs:
gender.str = rnorm(1000, 0, 2)
bob.str = rnorm(1000, gender.str, 1)
bob.pulling = bob.str + rnorm(1000, 0, .5)
jim.str = -2
jim.pulling = jim.str + rnorm(1000, 0, .5)

gender.str.3 = rnorm(1000, 0, 2)
for (i in 1: )
alice.str = rnorm(1000, sample(c(gender.str, gender.str.2)), 1)

gender.str.4 = rnorm(1000, 0, 2)
sue.str = rnorm(1000, gender.str.2, 1)
jim.pulling = jim.str + rnorm(1000, 0, .5)

bob.wins = bob.pulling > jim.pulling
accepted.indices = which(bob.wins)
bob.posterior.strength.jim.weak = bob.str[accepted.indices]

