# Simulation of systematic sampling with main and reserve sample

require(data.table)
require(sampling)

# Reset
rm(list = ls())
gc()

# Parameters
N <- 100L # population size
n <- 6L   # main sample size
m <- 1L   # reserve sample size

# Fix random number generator
set.seed(1)

# Population data
# this is a size measure
# for example, number of persons in a dwelling
pop <- sort(1L + floor(rchisq(N, 3)))
summary(pop)
table(pop)


# Sample inclusion probabilities
pik1 <- inclusionprobabilities(a = pop, n = n)
summary(pik1)
sum(pik1) # equals to sample size (n)

gen.samples <- function(sample.id = 1, pop, n, m) {
  # Main sample
  s1 <- UPsystematic(pik = pik1)

  # Reserve sample
  pop2 <- pop[!s1]
  pik2 <- inclusionprobabilities(a = pop2, n = m)
  s2 <- UPsystematic(pik = pik2)

  # Total sample
  s3 <- s1
  s3[!s1] <- s2

  data.table(sample.id = sample.id,
             unit.id = seq_along(pop),
             size = pop,
             sample.main = s1,
             sample.total = s3)
}


# Simulation
t1 <- Sys.time()
dat.sim0 <- rbindlist(lapply(1:1e5, gen.samples, pop = pop, n = n, m = m))
dat.sim1 <- rbindlist(lapply(1:1e5, gen.samples, pop = pop, n = n, m = m))
dat.sim2 <- rbindlist(lapply(1:1e5, gen.samples, pop = pop, n = n, m = m))
dat.sim3 <- rbindlist(lapply(1:1e5, gen.samples, pop = pop, n = n, m = m))
dat.sim4 <- rbindlist(lapply(1:1e5, gen.samples, pop = pop, n = n, m = m))
dat.sim5 <- rbindlist(lapply(1:1e5, gen.samples, pop = pop, n = n, m = m))
dat.sim6 <- rbindlist(lapply(1:1e5, gen.samples, pop = pop, n = n, m = m))
dat.sim7 <- rbindlist(lapply(1:1e5, gen.samples, pop = pop, n = n, m = m))
dat.sim8 <- rbindlist(lapply(1:1e5, gen.samples, pop = pop, n = n, m = m))
dat.sim9 <- rbindlist(lapply(1:1e5, gen.samples, pop = pop, n = n, m = m))
t2 <- Sys.time()
t2 - t1

gc()

dat.sim <- rbindlist(mget(paste0("dat.sim", 0:9)), idcol = T)
rm(list = paste0("dat.sim", 0:9))
gc()

dat.sim[, .N]

tab.sim <- dat.sim[, .(sim.count = .N,
                       prob_main_sim = mean(sample.main),
                       prob_total_sim = mean(sample.total)),
                   keyby = .(unit.id, size)]

tab.sim[, prob_main_true := inclusionprobabilities(a = size, n = n)]
tab.sim[, prob_total_true := inclusionprobabilities(a = size, n = n + m)]

tab.sim[, diff_main := abs(prob_main_sim - prob_main_true)]
tab.sim[, diff_total := abs(prob_total_sim - prob_total_true)]

tab.sim

fwrite(tab.sim, file = "tables/syst-sampl-sim.csv")
