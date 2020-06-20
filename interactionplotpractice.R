install.packages("merTools")
library(merTools)

library(lme4)



set.seed(1234)  # this will allow you to exactly duplicate your result
Ngroups = 100
NperGroup = 3
N = Ngroups*NperGroup
groups = factor(rep(1:Ngroups, each=NperGroup))
u = rnorm(Ngroups, sd=.5)
e = rnorm(N, sd=.25)
x = rnorm(N)
y = 2 + .5*x + u[groups] + e

d = data.frame(x, y, groups)

model = lmer(y ~ x + (1|groups), data=d)
summary(model)
confint(model)



library(ggplot2)
ggplot(aes(x, y), data=d) +
  geom_point()

re = ranef(model)$groups
qplot(x=re, geom='density', xlim=c(-3,3))