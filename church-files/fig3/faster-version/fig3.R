library("ggplot2")

prior=scan("prior.samples", sep=" ")
bob.beats.jim=scan("bob-beats-jim.samples", sep=" ")
bob.beats.weak.jim=scan("bob-beats-weakest-jim.samples", sep=" ")

values=c(prior, bob.beats.jim, bob.beats.weak.jim)
conds=c(rep("prior",length(prior)),
        rep("bob beats jim", length(bob.beats.jim)),
        rep("bob beats weakest jim", length(bob.beats.weak.jim)))
conds=factor(conds, levels=c("prior", "bob beats jim", "bob beats weakest jim"))

mydata=data.frame(values=values, conds=conds)

ggplot.rainbow = function(n) {
  return(sapply(0:(n-1), function(i) {
    #     return(hcl(h=(i*(360/n)), c=100, l=65))
    return(hcl(h=((360-(i+1))*(360/n)), c=100, l=65))
  }))
}

ggplot(mydata, aes(values, colour = conds)) + geom_line(size=3, stat="density", adjust=1.4) +
  xlim(-10,10) +
  theme_bw(28) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  ) +
  scale_colour_manual(name="Observations",
                      values=rev(ggplot.rainbow(5)[2:5]))#c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"))

# ggplot(mydata, aes(values, ..density.., colour = conds)) +
#   geom_freqpoly(binwidth = .5)
# #ggplot(mydata, aes(values, colour = conds)) + geom_line(size=3, stat="density") +
#   xlim(-10,10) +
#   theme_gray(24) +
#   scale_colour_manual(name="Observations",
#                       values=rev(ggplot.rainbow(5)[2:5]))#c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"))


# ggplot(mydata, aes(x=values, fill=conds))+ geom_bar(position="dodge")+ xlim(-6,6)
# ggplot(mydata, aes(values, fill = conds)) + geom_density(alpha = 0.2) + xlim(-10,10)
