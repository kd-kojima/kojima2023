#
# Kojima, K. (2023). Application of Cognitive Diagnosis Models (Senior thesis).
#   Script for analysis 1
#

rm(list=ls())

library(jagsUI)
library(MCMCvis)
library(GDINA)
library(Hmisc)
library(loo)

options(scipen = 100)

set.seed(1843)

# -------------------------------------
# import data
#
T <- 1

Q <- read.csv("./qmat1.csv", row.names = 1)
I <- nrow(Q)
K <- ncol(Q)

datacsv <- read.csv("./t1.csv", row.names = 1)
X <- datacsv[,1:I]
N <- nrow(X)

g2 <- datacsv[datacsv[,"YEAR"] == 2,]
g3 <- datacsv[datacsv[,"YEAR"] == 3,]
num <- as.data.frame(cbind(nrow(g2), nrow(g3), nrow(g2)+nrow(g3)))
colnames(num) <- c("2nd", "3rd", "sum")
rownames(num) <- c("num")

pcorr <- as.data.frame(cbind(c(colMeans(g2[,1:I]), mean(colMeans(g2[,1:I]))*I), c(colMeans(g3[,1:I]), mean(colMeans(g3[,1:I]))*I), c(colMeans(X), mean(colMeans(X))*I)))
colnames(pcorr) <- c("Year2", "Year3", "All")
rownames(pcorr) <- c(paste0("(", 1:I, ")"), "All")

# -----------------------------
# HO-DINA
#
mean.lambda0 <- c(-1, 0.5, 0.5)
pr.lambda0 <- c(0.5, 0.25, 0.25)
mean.lambda <- c(0.5, 1, 1)
pr.lambda <- c(0.25, 0.25, 0.25)

a.s <- 1
b.s <- 1
a.g <- 1
b.g <- 1

data <- list(
    N = N, 
    I = I, 
    K = K, 
    X = X, 
    Q = Q, 
    a.s = a.s,
    b.s = b.s,
    a.g = a.g,
    b.g = b.g,
    mean.lambda0 = mean.lambda0,
    pr.lambda0 = pr.lambda0,
    mean.lambda = mean.lambda,
    pr.lambda = pr.lambda
)
modfile <- "HO-DINA.jags"
cat("
    model {
        for (n in 1:N) {
            for (i in 1:I) {
                for (k in 1:K) {
                    w[n, i, k] <- pow(alpha[n, k], Q[i, k])
                }
                eta[n, i] <- prod(w[n, i, 1:K])
                p[n, i] <- g[i] + (1 - s[i] - g[i]) * eta[n, i]
                X[n, i] ~ dbern(p[n, i])
            }
        }
        for (n in 1:N) {
            for (k in 1:K) {
                logit(prob.a[n, k]) <- lambda[k] * theta[n] - lambda0[k]
                alpha[n, k] ~ dbern(prob.a[n, k])
            }
            theta[n] ~ dnorm(0, 1)
        }
        for (k in 1:K) {
            lambda0[k] ~ dnorm(mean.lambda0[k], pr.lambda0[k])
            lambda[k] ~ dnorm(mean.lambda[k], pr.lambda[k]) T(0, )
        }
        for (i in 1:I) {
            s[i] ~ dbeta(a.s, b.s)
            g[i] ~ dbeta(a.g, b.g) T(, 1 - s[i])
        }

        for (n in 1:N) {
            for (i in 1:I) {
                log_lik[n, i] <- logdensity.bern(X[n, i], p[n, i])

                resstat[n, i] <- pow(X[n, i] - p[n, i], 2) / (p[n, i] * (1 - p[n, i]))
                X_rep[n, i] ~ dbern(p[n, i])
                resstat_rep[n, i] <- pow(X_rep[n, i] - p[n, i], 2) / (p[n, i] * (1 - p[n, i]))
            }
        }
        resstat_sum <- sum(resstat[1:N, 1:I])
        resstat_rep_sum <- sum(resstat_rep[1:N, 1:I])
        ppp <- step(resstat_rep_sum - resstat_sum)
    }
", file = modfile)

params <- c("alpha", "g", "s", "lambda0", "lambda", "theta", "log_lik", "ppp", "X_rep", "resstat_sum", "resstat_rep_sum")

inits <- function(){
    list(
        s = runif(I, 0, 0.5),
        g = runif(I, 0, 0.5),
        lambda0 = rnorm(K, 0, 4),
        lambda = abs(rnorm(K, 0, 4))
    )
}

n_chains <- 5
n_iter <- 10000
n_burnin <- 6000
n_thin <- 2

res_mcmc_hodina <- jags(
    data = data,
    inits = inits,
    parameters.to.save = params,
    model.file = modfile,
    n.chains = n_chains,
    n.iter = n_iter,
    n.burnin = n_burnin,
    n.thin = n_thin,
    parallel = TRUE,
    n.cores = n_chains
)

# Rhats over 1.05
Rhats <- data.frame(rownames = rownames(res_mcmc_hodina$summary), Rhat = res_mcmc_hodina$summary[, "Rhat"])
Rhats_over <- data.frame(Rhat = Rhats[!is.na(Rhats$Rhat) & Rhats$Rhat > 1.05, "Rhat"])
rownames(Rhats_over) <- Rhats[!is.na(Rhats$Rhat) & Rhats$Rhat > 1.05, "rownames"]
Rhats_over

alpha_indices <- grep("alpha\\[", rownames(res_mcmc_hodina$summary))
ipar_indices <- grep("[gs]\\[", rownames(res_mcmc_hodina$summary))
kpar_indices <- grep("(lambda0|lambda)\\[", rownames(res_mcmc_hodina$summary))
theta_indices <- grep("^theta\\[", rownames(res_mcmc_hodina$summary))

HODINA_MCMC <- matrix(res_mcmc_hodina$summary[alpha_indices, "mean"], nrow = N, ncol = K)
alpha_mcmc_hodina <- matrix(res_mcmc_hodina$summary[alpha_indices, "50%"], nrow = N, ncol = K)
ipar_mcmc_hodina_mean <- matrix(res_mcmc_hodina$summary[ipar_indices, "mean"], nrow = I, ncol = 2)
ipar_mcmc_hodina_sd <- matrix(res_mcmc_hodina$summary[ipar_indices, "sd"], nrow = I, ncol = 2)
ipar_mcmc_hodina <- cbind(ipar_mcmc_hodina_mean[,1], ipar_mcmc_hodina_sd[,1], ipar_mcmc_hodina_mean[,2], ipar_mcmc_hodina_sd[,2])
colnames(ipar_mcmc_hodina) <- c("guessing", "SDg", "slipping", "SDs")
rownames(ipar_mcmc_hodina) <- c(paste0("Q", 1:I))
kpar_mcmc_hodina_mean <- matrix(res_mcmc_hodina$summary[kpar_indices, "mean"], nrow = K, ncol = 2)
kpar_mcmc_hodina_sd <- matrix(res_mcmc_hodina$summary[kpar_indices, "sd"], nrow = K, ncol = 2)
kpar_mcmc_hodina <- cbind(kpar_mcmc_hodina_mean[,1], kpar_mcmc_hodina_sd[,1], kpar_mcmc_hodina_mean[,2], kpar_mcmc_hodina_sd[,2])
colnames(kpar_mcmc_hodina) <- c("lambda0", "SD0", "lambda", "SD")
rownames(kpar_mcmc_hodina) <- c(paste0("A", 1:K))
theta_mcmc_hodina_mean <- matrix(res_mcmc_hodina$summary[theta_indices, "mean"], nrow = N, ncol = 1)
theta_mcmc_hodina_sd <- matrix(res_mcmc_hodina$summary[theta_indices, "sd"], nrow = N, ncol = 1)
theta_mcmc_hodina <- cbind(theta_mcmc_hodina_mean, theta_mcmc_hodina_sd)
colnames(theta_mcmc_hodina) <- c("theta", "SD")

i_theta <- cbind(theta_mcmc_hodina_mean, rowSums(X))
colnames(i_theta) <- c("theta", "sum")

log_lik_array <- res_mcmc_hodina$sims.list$log_lik
log_lik_mat <- matrix(0, nrow = dim(log_lik_array)[1], ncol = N*I)
for (m in 1:dim(log_lik_array)[1]) {
    log_lik_mat[m,] <- unlist(log_lik_array[m,,])
}

waic_mcmc_hodina <- waic(log_lik_mat)
ppp_mcmc_hodina <- pp.check(res_mcmc_hodina, observed = "resstat_sum", simulated = "resstat_rep_sum")
waic_mcmc_hodina
ppp_mcmc_hodina

pat_table <- apply(alpha_mcmc_hodina, 1, function(x){
    paste0(x[1],x[2],x[3])
})
pat <- c("000", "100", "010", "001", "110", "101", "011", "111", pat_table)
xtable <- table(pat) - 1
xtable <- c(xtable, sum(xtable))
names(xtable) <- c(names(xtable)[1:8], "SUM")

# ---------------------------
# output
#
MCMCtrace(res_mcmc_hodina, params = c("g", "s", "lambda0", "lambda", "theta"), filename = paste0("MCMCtrace_hodina_t", T, ".pdf"))

# write mastery probability
write.csv(HODINA_MCMC, paste0("prob_T", T, "_1.csv"), quote = FALSE)

# save RData
save(list = ls(), file = paste0("t", T, ".RData"))
