# clear working directory
rm(list = ls())

# set working directory
setwd("~/Dropbox/projects/contagion/")

# load packages
library(arm)
library(texreg)
library(compactr)

# load data
# d <- read.csv("data/data-nogdp.csv")
# r <- read.csv("data/data-gdp.csv")
d <- read.csv("data/data-contagion-merged.csv")

# create variables
# create trade dependence
d$td <- d$FLOW2/(d$imports) # flow from country w/ civil war to neighbor
d$td2 <- d$FLOW1/(d$exports) # flow from neighbor to country w/ civil war

# r$td <- r$FLOW2/r$TotTrade # flow from country w/ civil war to neighbor
# r$td2 <- r$FLOW1/r$TotTrade  # flow from neighbor to country w/ civil war


# estimate models
# including real gdp and gdp/capita
# m <- glm(ccode2_conflict ~ td + ccode1_conflict + td*ccode1_conflict + realgdp2013 + rgdppc2013 + as.factor(conttype), r, family = binomial)
# summary(m)
# display(m, detail = TRUE)

# trade flow from neighbor to country w/ civil war - excluding real gdp and gdp/capita
m <- glm(ccode2_conflict ~ td + ccode1_conflict + conttype, d, family = binomial)
summary(m)
display(m, detail = TRUE)

# trade flow from country w/ civil war to neighbor - excluding real gdp and gdp/capita
m2 <- glm(ccode2_conflict ~ td2 + ccode1_conflict + conttype, d, family = binomial)
summary(m2)
display(m2, detail = TRUE)

mlist <- list(m)
m2list <- list(m2)
list <- list(m, m2)
texreg(list)

# calculate and compare first difference & risk ratio
# set values to calculate first difference & risk ratio
x.conttype <- median(d$conttype, na.rm = TRUE) # separated by a land or river border
x.ccode1_conflict <- 1
x.td.hi <- quantile(d$td, 0.75, na.rm = TRUE)
x.td.lo <- quantile(d$td, 0.25, na.rm = TRUE)
x.td2.hi <- quantile(d$td2, 0.75, na.rm = TRUE)
x.td2.lo <- quantile(d$td2, 0.25, na.rm = TRUE)
x.tdconflict.hi <- x.ccode1_conflict*x.td.hi
x.tdconflict.lo <- x.ccode1_conflict*x.td.lo
x.td2conflict.hi <- x.ccode1_conflict*x.td2.hi
x.td2conflict.lo <- x.ccode1_conflict*x.td2.lo

# for m 
# create beta.hats; X.lo, X.hi, and X.c matrices; Sigma; and beta.tilde 
beta.hat <- coef(m)

X.hi <- c(1, x.td.hi, x.ccode1_conflict, x.conttype)
X.lo <- c(1, x.td.lo, x.ccode1_conflict, x.conttype)
X.c <- rbind(X.hi, X.lo)

Sigma <- vcov(m)
beta.tilde <- mvrnorm(10000, beta.hat, Sigma)

# first difference & risk ratio
p.hat <- plogis(X.c%*%(beta.hat))
fd.hat <- p.hat[1, ] - p.hat[2, ]
rr.hat <- p.hat[1, ]/p.hat[2, ]

# simulations
n.sims <- 1000
beta.tilde <- mvrnorm(n.sims, beta.hat, Sigma)
p.tilde <- plogis(X.c%*%t(beta.tilde))

# characterize simulations
mean(p.tilde)
sd(p.tilde)
quantile(p.tilde, c(0.05, 0.95))
fd.tilde <- p.tilde[1, ] - p.tilde[2, ]
rr.tilde <- p.tilde[1, ]/p.tilde[2, ]


# for m2 
# create beta.hats; X.lo, X.hi, and X.c matrices; Sigma; and beta.tilde 
beta.hat2 <- coef(m2)

X.hi2 <- c(1, x.td2.hi, x.ccode1_conflict, x.td2conflict.hi, x.conttype)
X.lo2 <- c(1, x.td2.lo, x.ccode1_conflict, x.td2conflict.lo, x.conttype)
X.c2 <- rbind(X.hi2, X.lo2)

Sigma2 <- vcov(m2)
beta.tilde2 <- mvrnorm(10000, beta.hat2, Sigma2)

# first difference & risk ratio
p.hat2 <- plogis(X.c%*%(beta.hat2))
fd.hat2 <- p.hat2[1, ] - p.hat2[2, ]
rr.hat2 <- p.hat2[1, ]/p.hat2[2, ]

# simulations
n.sims <- 1000
beta.tilde2 <- mvrnorm(n.sims, beta.hat2, Sigma2)
p.tilde2 <- plogis(X.c%*%t(beta.tilde2))

# characterize simulations
mean(p.tilde2)
sd(p.tilde2)
quantile(p.tilde2, c(0.05, 0.95))
fd.tilde2 <- p.tilde2[1, ] - p.tilde2[2, ]
rr.tilde2 <- p.tilde2[1, ]/p.tilde2[2, ]


# create plots 
# create first difference
par(mfrow = c(1,1), mar = c(3,1,1,1), oma = c(0,0,0,0))
eplot(xlim = c(-0.01, 0.05), ylim = c(1, 3),
      xlab = "First Difference", 
      anny = FALSE,
      main = "Effect of Trade Flows")
abline(v = 0, col = "grey50")

# add imports
ht <- 1.5
est <- median(fd.hat)
lwr <- quantile(fd.tilde, 0.05)
upr <- quantile(fd.tilde, 0.95)
points(est, ht, pch = 19)
lines(c(lwr, upr), c(ht,ht))
text(est, ht, "Imports (0.017)", pos = 3, cex = 1)

# add exports
ht <- 2.5
est <- median(fd.hat2)
lwr <- quantile(fd.tilde2, 0.05)
upr <- quantile(fd.tilde2, 0.95)
points(est, ht, pch = 19)
lines(c(lwr, upr), c(ht,ht))
text(est, ht, "Exports (0.009)", pos = 3, cex = 1)


# create risk ratios
par(mfrow = c(1,1), mar = c(3,1,1,1), oma = c(0,0,0,0))
eplot(xlim = c(0.9, 1.10), ylim = c(1, 2),
      xlab = "Risk Ratio", 
      anny = FALSE,
      main = "Effect of Trade Flows")
abline(v = 1, col = "grey50")

# add imports
ht <- 1.25
est <- median(rr.hat)
lwr <- quantile(rr.tilde, 0.05)
upr <- quantile(rr.tilde, 0.95)
points(est, ht, pch = 19)
lines(c(lwr, upr), c(ht,ht))
text(est, ht, "Imports (1.02)", pos = 3, cex = 1)

# add exports
ht <- 1.75
est <- median(rr.hat2)
lwr <- quantile(rr.tilde2, 0.05)
upr <- quantile(rr.tilde2, 0.95)
points(est, ht, pch = 19)
lines(c(lwr, upr), c(ht,ht))
text(est, ht, "Exports (1.01)", pos = 3, cex = 1)

# td.hist <- hist(d$td, plot = FALSE)
# # plot predicted probabilities
# trade <- seq(0, 1, length.out = 100)
# cont <- predict(m, d)
# cont1 <- apply(cont, 2, quantile, c(0.05, 0.50, 0.95))
# plot(td.hist, col = "grey80", axes = FALSE, border = NA, main = NA, ylab = NA, xlab = NA)
# par(new = TRUE)
# eplot(xlim = c(0, 1), ylim = c(0, 1),
#       xlab = "Trade Flow",
#       ylab = "Pr(Contagion)", ylabpos = 2,
#       main = "Increased Trade Flow & Risk of Contagion")
# abline(h = 0)
# pred.m <- predict(m, d)
# lines(d$td, pred.m, type = "1'")
# curve(pred.m, add = TRUE)
# 
# # lines(c(0, 1), predict(m))
# points(x.td.hi, p.hat[1, ], pch = 19)
# points(x.td.lo, p.hat[2, ], pch = 19)
# est.hi <- x.td.hi
# est.lo <- x.td.lo
# lwr.hi <- quantile(p.hat[1, ], 0.05)
# lwr.lo <- quantile(x.td.lo, 0.05)
# upr.hi <- quantile(p.hat[1, ], 0.95)
# upr.lo <- quantile(x.td.lo, 0.95)
# lines(c(x.td.hi, x.td.hi), c(lwr.hi, upr.hi), lwd = 2)
# 
# # for m2 
# # create beta.hats; X.lo, X.hi, and X.c matrices; Sigma; and beta.tilde 
# beta.hat2 <- coef(m2)
# 
# X.hi2 <- c(1, x.td2.hi, x.ccode1_conflict, x.td2conflict.hi, x.conttype)
# X.lo2 <- c(1, x.td2.lo, x.ccode1_conflict, x.td2conflict.lo, x.conttype)
# X.c2 <- rbind(X.hi2, X.lo2)
# 
# Sigma2 <- vcov(m2)
# beta.tilde2 <- mvrnorm(10000, beta.hat2, Sigma2)
# 
# # first difference & risk ratio
# p.hat2 <- plogis(X.c%*%(beta.hat2))
# fd.hat2 <- p.hat2[1, ] - p.hat2[2, ]
# rr.hat2 <- p.hat2[1, ]/p.hat2[2, ]
# 
# # plot predicted probabilities
# # plot predicted probabilities
# eplot(xlim = c(0, 1), ylim = c(min(p.hat2), max(p.hat2)),
#       xlab = "Trade Flow",
#       ylab = "Pr(Contagion)", ylabpos = 2,
#       main = "Increased Trade Flow & Risk of Contagion")
# # lines(c(0, 1), predict(m))
# points(x.td2.hi, p.hat2[1, ], pch = 19)
# points(x.td2.lo, p.hat2[2, ], pch = 19)
# lines(c(x.td2.hi, x.td2.lo), c(p.hat2[1, ], p.hat2[2, ]), lwd = 2)

