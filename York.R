install.packages("party")
install.packages("lme4")
install.packages("rms")

library(party)
library(lme4)
library(rms)

york =  read.csv("http://individual.utoronto.ca/tagliamonte/downloads/york.csv",  header = TRUE)

view(york)

york.cforest = cforest(Form ~ Adjacency + Polarity + Age + Sex + Education + Modification + Proximate1.adj + Proximate1 + Proximate2 + Individual,
 data = york)
