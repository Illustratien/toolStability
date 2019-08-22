
library(toolStability)
rm(list = ls())
trait <- c(2, 3, 20, 3, 10, 25, 2, 6, 23, 4, 18, 22)
genotype <- as.factor(rep(1:3, 4))
genotype1 <- rep(1:3, each = 4)
gn <- length(unique(genotype))
environment <- as.factor(rep(1:4, each = gn))
environment1 <- rep(1:4, gn)
evn <- length(unique(environment))
ser <- seq(evn - 1, 1, by = -1)
lambda <- 15


x <- 1:evn
comb1 <- rep(x[-evn], ser)
comb2 <- sequence(ser) + rep(x[-evn], ser)

data <- data.frame(trait = trait, genotype = genotype, environment = environment)

geno.i <- list()
bar.i <- c()
cor.trait <- c()
logvar <- c()
logbar <- c()
for (i in 1:gn) {
  geno.i[[i]] <- trait[genotype == unique(genotype)[i]]
  bar.i <- c(bar.i, mean(geno.i[[i]]))
  cor.trait <- c(cor.trait, geno.i[[i]] - bar.i[i] + mean(trait))
  logvar[i] <- log10(var(geno.i[[i]]))
  logbar[i] <- log10(bar.i[i])
}

ejbar <- c()
Mj <- c()
rank1 <- c()
for (i in 1:evn) {
  Ej <- trait[environment == unique(environment)[i]]
  CEj <- cor.trait[environment1 == unique(environment1)[i]]
  ejbar <- c(ejbar, mean(Ej))
  Mj <- c(Mj, max(Ej))
  rank1 <- c(rank1, rank(-CEj, na.last = "keep", ties.method = "min"))
}

geno.rank.i <- list()
bar.rank.i <- c()

for (i in 1:gn) {
  geno.rank.i[[i]] <- rank1[genotype == unique(genotype)[i]]
  bar.rank.i <- c(bar.rank.i, mean(geno.rank.i[[i]]))
}

# ok environmental variance
env.var <- c()
for (i in 1:gn) {
  dev1 <- c()
  dev1 <- c(dev1, ((geno.i[[i]] - bar.i[i])^2) / (evn - 1))
  env.var <- c(env.var, sum(dev1))
}
# change to mean

eco.val <- c()
eco.val1 <- c()

for (i in 1:gn) {
  f1 <- c()
  # print((geno.i[[i]]-bar.i[i]-ejbar+mean(trait))^2)
  f1 <- c(f1, (geno.i[[i]] - bar.i[i] - ejbar + mean(trait))^2)
  eco.val <- c(eco.val, mean(f1))
  eco.val1 <- c(eco.val1, sum(f1))
}
# ok
stb.var <- c()
for (i in 1:gn) {
  en1 <-
    sigma <- (eco.val1[i] * gn * (gn - 1) - sum(eco.val1)) / ((evn - 1) * (gn - 2) * (gn - 1))
  stb.var <- c(stb.var, sigma)
}
stb.var[stb.var < 0] <- 0

# ok
bi <- c()
for (i in 1:gn) {
  f1 <- c()
  f2 <- c()
  for (j in 1:evn) {
    f1 <- c(f1, (geno.i[[i]][j] - bar.i[i] - ejbar[j] + mean(trait)) * (ejbar[j] - mean(trait)))
    f2 <- c(f2, (ejbar[j] - mean(trait))^2)
  }
  bi <- c(bi, 1 + (sum(f1) / sum(f2)))
}

# ok
s2di <- c()
for (i in 1:gn) {
  f1 <- c()
  f2 <- c()
  for (j in 1:evn) {
    f1 <- c(f1, (geno.i[[i]][j] - bar.i[i] - ejbar[j] + mean(trait))^2)
    f2 <- c(f2, ((bi[i] - 1)^2) * (ejbar[j] - mean(trait))^2)
  }
  s2di <- c(s2di, (sum(f1) - sum(f2)) / (evn - 2))
}

# same but not equal to lm
r2 <- c()

for (i in 1:gn) {
  r2 <- c(r2, 1 - (s2di[i] / env.var[i]))
}


Bmin <- min(bi)
# ok
D2i <- c()
for (i in 1:gn) {
  f1 <- c()
  # print((geno.i[[i]]-bar.i[i]-Bmin*ejbar+Bmin*mean(trait))^2)
  f1 <- c(f1, (geno.i[[i]] - bar.i[i] - Bmin * ejbar + Bmin * mean(trait))^2)
  D2i <- c(D2i, (sum(f1)))
}

# ok

mrd <- c()
for (i in 1:gn) {
  f1 <- c()
  f1 <- c(f1, abs(geno.rank.i[[i]][comb1] - geno.rank.i[[i]][comb2]))
  mrd <- c(mrd, 2 * sum(f1) / ((evn - 1) * evn))
}

# ok
vor <- c()
for (i in 1:gn) {
  f1 <- c()
  f1 <- c(f1, (geno.rank.i[[i]] - bar.rank.i[i])^2)
  vor <- c(vor, sum(f1) / (evn - 1))
}
# ok
Pi <- c()
for (i in 1:gn) {
  f1 <- c()

  for (j in 1:evn) {
    f1 <- c(f1, (geno.i[[i]][j] - Mj[j])^2)
  }
  Pi <- c(Pi, sum(f1) / (evn * 2))
}

adj.cv <- c()


lm1 <- lm(logvar ~ logbar)

b <- summary(lm1)$coefficients[2, 1]
for (i in 1:gn) {
  adj.cv[i] <- 100 * (1 / bar.i[i]) * sqrt(10^((2 - b) * logbar[i] + (b - 2) * mean(logbar) + logvar[i]))
}

sft.fir <- c()
for (i in 1:gn) {
  dev1 <- c()
  dev1 <- c(dev1, pnorm((lambda - bar.i[i]) / sd(geno.i[[i]])))
  sft.fir <- c(sft.fir, sum(dev1))
}



tb <- table_stability(data, "trait", "genotype", "environment", lambda)


test_that("environmental_variance is calulated correctly", {
  expect_equal(env.var, environmental_variance(data, "trait", "genotype")$environmental.variance)
  expect_equal(env.var, tb$Environmental.variance)
})
test_that("environmental_variance is same between function and table", {
  expect_equal(environmental_variance(data, "trait", "genotype")$environmental.variance, tb$Environmental.variance)
})

test_that("ecovalence is calulated correctly", {
  expect_equal(eco.val, ecovalence(data, "trait", "genotype", "environment")$ecovalence)
  expect_equal(eco.val, tb$Ecovalence)
})
test_that("ecovalence is same between function and table", {
  expect_equal(ecovalence(data, "trait", "genotype", "environment")$ecovalence, tb$Ecovalence)
})

test_that("stability_variance is calulated correctly", {
  expect_equal(stb.var, stability_variance(data, "trait", "genotype", "environment")$stability.variance)
  expect_equal(stb.var, tb$Stability.variance)
})
test_that("stability_variance is same between function and table", {
  expect_equal(stability_variance(data, "trait", "genotype", "environment")$stability.variance, tb$Stability.variance)
})

test_that("coefficient_of_regression is calulated correctly", {
  expect_equal(bi, coefficient_of_regression(data, "trait", "genotype", "environment")$coefficient.of.regression)
  expect_equal(bi, tb$Coefficient.of.regression)
})
test_that("coefficient_of_regression is same between function and table", {
  expect_equal(
    coefficient_of_regression(data, "trait", "genotype", "environment")$coefficient.of.regression,
    tb$Coefficient.of.regression
  )
})
test_that("deviation_mean_squares is calulated correctly", {
  expect_equal(s2di, deviation_mean_squares(data, "trait", "genotype", "environment")$deviation.mean.squares)
  expect_equal(s2di, tb$Deviation.mean.squares)
})
test_that("deviation_mean_squares is same between function and table", {
  expect_equal(
    deviation_mean_squares(data, "trait", "genotype", "environment")$deviation.mean.squares,
    tb$Deviation.mean.squares
  )
})

test_that("coefficient_of_determination is calulated correctly", {
  expect_equal(r2, coefficient_of_determination(data, "trait", "genotype", "environment")$coefficient.of.determination)
  expect_equal(r2, tb$Coefficient.of.determination)
})
test_that("coefficient_of_determination is same between function and table", {
  expect_equal(
    coefficient_of_determination(data, "trait", "genotype", "environment")$coefficient.of.determination,
    tb$Coefficient.of.determination
  )
})


test_that("genotypic_stability is calulated correctly", {
  expect_equal(D2i, genotypic_stability(data, "trait", "genotype", "environment")$genotypic.stability)
  expect_equal(D2i, tb$Genotypic.stability)
})
test_that("genotypic_stability is same between function and table", {
  expect_equal(
    genotypic_stability(data, "trait", "genotype", "environment")$genotypic.stability,
    tb$Genotypic.stability
  )
})


test_that("mean_rank_difference is calulated correctly", {
  expect_equal(mrd, mean_rank_difference(data, "trait", "genotype", "environment")$mean.rank.difference)
  expect_equal(mrd, tb$Mean.rank.difference)
})
test_that("mean_rank_difference is same between function and table", {
  expect_equal(
    mean_rank_difference(data, "trait", "genotype", "environment")$mean.rank.difference,
    tb$Mean.rank.difference
  )
})


test_that("variance_of_ranks is calulated correctly", {
  expect_equal(vor, variance_of_rank(data, "trait", "genotype", "environment")$variance.of.rank)
  expect_equal(vor, tb$Variance.of.rank)
})
test_that("variance_of_ranks is same between function and table", {
  expect_equal(variance_of_rank(data, "trait", "genotype", "environment")$variance.of.rank, tb$Variance.of.rank)
})


test_that("adjusted_coefficient_of_variation is calulated correctly", {
  expect_equal(adj.cv, adjusted_coefficient_of_variation(data, "trait", "genotype", "environment")$adjusted.coefficient.of.variation)
  expect_equal(adj.cv, tb$Adjusted.coefficient.of.variation)
})
test_that("adjusted_coefficient_of_variation is same between function and table", {
  expect_equal(
    adjusted_coefficient_of_variation(data, "trait", "genotype", "environment")$adjusted.coefficient.of.variation,
    tb$Adjusted.coefficient.of.variation
  )
})

test_that("genotypic_superiority_measure is calulated correctly", {
  expect_equal(Pi, genotypic_superiority_measure(data, "trait", "genotype", "environment")$genotypic.superiority.measure)
  expect_equal(Pi, tb$Genotypic.superiority.measure)
})
test_that("genotypic_superiority_measure is same between function and table", {
  expect_equal(
    genotypic_superiority_measure(data, "trait", "genotype", "environment")$genotypic.superiority.measure,
    tb$Genotypic.superiority.measure
  )
})

test_that("safty_first_index is calulated correctly", {
  expect_equal(sft.fir, safty_first_index(data, "trait", "genotype", "environment", lambda)$safty.first.index)
  expect_equal(sft.fir, tb$Safty.first.index)
})
test_that("safty_first_index is same between function and table", {
  expect_equal(
    safty_first_index(data, "trait", "genotype", "environment", lambda)$safty.first.index,
    tb$Safty.first.index
  )
})
