require(bbmle)

block1 <- read.csv("block1.csv")
block2 <- read.csv("block2.csv")
block3 <- read.csv("block3.csv")

#Dataset with only perfect evenness treatments
alleven <- rbind(block1[136:165, ], block2[136:165, ], block3[91:110, ])

# Likelihood function

LL1 <- function(mb, p0, p1, p2, p3, a1, a2, a3) {
  Y = mb * (p0 + (p1 * exp(-a1 * x1)) + (p2 * exp(-a2 * x2)) + (p3 * exp(-a3 * x3))) # this is Tobin's function
  -sum(dbinom(resp, size = initial, prob = Y, log = TRUE)) # Decided to use a binomial
}

# Optimization

# Makes more sense because of the boundaries in estimation, but does not work
MLL1 <- mle2(LL1, start = list(mb = 0.5, p0 = 0.1, p1 = 0.3, p2 = 0.3, p3 = 0.3, a1 = 0.001, a2 = 0.001, a3 = 0.001), 
             data = list(x1 = alleven$Sc, x2 = alleven$Hm, x3 = alleven$Bb, resp = alleven$Alive, initial = rep(10, 80)),
             lower = list(p0 = 0, p1 = 0, p2 = 0, p3 = 0), 
             upper = list(p0 = 1, p1 = 1, p2 = 1, p3 = 1), method = "L-BFGS-B")

# No boundaries but works (kind of..)
MLL1a <- mle2(LL1, start = list(mb = 0.5, p0 = 0.1, p1 = 0.3, p2 = 0.3, p3 = 0.3, a1 = 0.001, a2 = 0.001, a3 = 0.001), 
              data = list(x1 = alleven$Sc, x2 = alleven$Hm, x3 = alleven$Bb, resp = alleven$Alive, initial = rep(10, 80)),
              method = "Nelder-Mead")

summary(MLL1a)
