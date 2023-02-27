#
# Kojima, K. (2023). Application of Cognitive Diagnosis Models (Senior thesis).
#   Script for analysis 2
#

rm(list=ls())

library(jagsUI)
library(MCMCvis)
library(GDINA)
library(Hmisc)
library(loo)
library(tidyverse)

options(scipen = 100)

set.seed(1843)

# -------------------------------------
# import data
#
T <- 2

Qcsv <- read.csv("./qmat1.csv", row.names = 1)
I <- nrow(Qcsv)
K <- ncol(Qcsv)

Q <- array(0, dim = c(I, K, T), dimnames = list(rownames(Qcsv), colnames(Qcsv), paste0("T", 1:T)))
for (t in 1:T) {
    Q[,,t] <- as.matrix(Qcsv)
}

datacsv1 <- read.csv("./t1.csv")
datacsv2 <- read.csv("./t2.csv")
datacsv <- full_join(datacsv1, datacsv2, by = "ID")
N <- nrow(datacsv)

datafull <- array(0, dim = c(N, ncol(datacsv1) - 1, T), dimnames = list(datacsv[,"ID"], colnames(datacsv1)[-1], paste0("T", 1:T)))
X <- array(0, dim = c(N, I, T), dimnames = list(datacsv[,"ID"], rownames(Qcsv), paste0("T", 1:T)))
for (t in 1:T) {
    datafull[,,t] <- as.matrix(datacsv[,(2+(t-1)*(ncol(datacsv1)-1)):(ncol(datacsv1)+(t-1)*(ncol(datacsv1)-1))])
    X[,,t] <- datafull[,1:I,t]

    # confirm if YEARs match
    if (t > 1) {
        if (identical(datafull[,"YEAR",1], datafull[,"YEAR",t])) {
            print(paste0("YEARs match, OK. | t = ", t))
        }
    }
}

g2 <- 0
for (t in 1:T) {
    x <- datafull[datafull[,"YEAR",1] == 2,,t]
    g2 <- array(g2, dim = c(nrow(x), ncol(datafull[,,1]), T), dimnames = list(rownames(x), colnames(datafull[,,t]), paste0("T", 1:T)))
    g2[,,t] <- x
}

g3 <- 0
for (t in 1:T) {
    x <- datafull[datafull[,"YEAR",1] == 3,,t]
    g3 <- array(g3, dim = c(nrow(x), ncol(datafull[,,1]), T), dimnames = list(rownames(x), colnames(datafull[,,t]), paste0("T", 1:T)))
    g3[,,t] <- x
}

num <- as.data.frame(cbind(nrow(g2[,,1]), nrow(g3[,,1]), nrow(g2[,,1])+nrow(g3[,,1])))
for (t in 2:T) {
    num <- as.data.frame(rbind(num, cbind(nrow(g2[complete.cases(g2[,,t]),,t]), nrow(g3[complete.cases(g3[,,t]),,t]), nrow(g2[complete.cases(g2[,,t]),,t]) + nrow(g3[complete.cases(g3[,,t]),,t]))))
}
colnames(num) <- c("2nd", "3rd", "SUM")
rownames(num) <- paste0("T", 1:t)

pcorr <- as.data.frame(cbind(c(colMeans(g2[,1:I,1], na.rm = TRUE), mean(colMeans(g2[,1:I,1], na.rm = TRUE))*I), c(colMeans(g3[,1:I,1], na.rm = TRUE), mean(colMeans(g3[,1:I,1], na.rm = TRUE))*I), c(colMeans(X[,,1], na.rm = TRUE), mean(colMeans(X[,,1], na.rm = TRUE))*I)))
for (t in 2:T) {
    pcorr <- as.data.frame(cbind(pcorr, c(colMeans(g2[,1:I,t], na.rm = TRUE), mean(colMeans(g2[,1:I,t], na.rm = TRUE))*I), c(colMeans(g3[,1:I,t], na.rm = TRUE), mean(colMeans(g3[,1:I,t], na.rm = TRUE))*I), c(colMeans(X[,,t], na.rm = TRUE), mean(colMeans(X[,,t], na.rm = TRUE))*I)))
}
colnames(pcorr) <- c(paste0("T1", c("Y2", "Y3", "ALL")), paste0("T2", c("Y2", "Y3", "ALL")))
rownames(pcorr) <- c(paste0("(", 1:I, ")"), "All")

complete_indices <- !is.na(X[,1,1]) & !is.na(X[,1,2])

# -----------------------------
# Long-HO-DINA
#
mean.lambda0 <- c(-1, 0.5, 0.5)
pr.lambda0 <- c(0.5, 0.25, 0.25)
mean.lambda <- c(0.5, 1, 1)
pr.lambda <- c(0.25, 0.25, 0.25)

a.s <- c(1, 1)
b.s <- c(1, 1)
a.g <- c(1, 1)
b.g <- c(1, 1)

data <- list(
    N = N,
    I = I,
    K = K,
    T = T,
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
modfile <- "Long-HO-DINA.jags"
cat("
    model {
        for (t in 1:T) {
            for (n in 1:N) {
                for (i in 1:I) {
                    for (k in 1:K) {
                        w[n, i, k, t] <- pow(alpha[n, k, t], Q[i, k, t])
                    }
                    eta[n, i, t] <- prod(w[n, i, 1:K, t])
                    p[n, i, t] <- g[i, t] + (1 - s[i, t] - g[i, t]) * eta[n, i, t]
                    X[n, i, t] ~ dbern(p[n, i, t])
                }
            }
            for (n in 1:N) {
                for (k in 1:K) {
                    logit(prob.a[n, k, t]) <- lambda[k] * theta[n, t] - lambda0[k]
                    alpha[n, k, t] ~ dbern(prob.a[n, k, t])
                }
            }
        }
        for (n in 1:N) {
            theta[n, 1:T] ~ dmnorm.vcov(mu_theta[1:T], Sigma_theta[1:T, 1:T])
        }
        for (k in 1:K) {
            lambda0[k] ~ dnorm(mean.lambda0[k], pr.lambda0[k])
            lambda[k] ~ dnorm(mean.lambda[k], pr.lambda[k]) T(0, )
        }
        for (t in 1:T) {
            for (i in 1:I) {
                s[i, t] ~ dbeta(a.s[t], b.s[t])
                g[i, t] ~ dbeta(a.g[t], b.g[t]) T(, 1 - s[i, t])
            }
        }

        delta[1] <- 0
        mu_theta[1] <- 0
        for (t in 2:T) {
            delta[t] ~ dnorm(0, 0.5) T(0, )
            mu_theta[t] <- delta[t] + mu_theta[t-1]
        }

        for (tt in 1:T) {
            Sigma_theta[tt, tt] <- 1
            for (ttt in 1:(tt-1)) {
                Sigma_theta[tt, ttt] ~ dunif(-1, 1)
                Sigma_theta[ttt, tt] <- Sigma_theta[tt, ttt]
            }
        }

        for (t in 1:T) {
            for (n in 1:N) {
                for (i in 1:I) {
                    log_lik[n, i, t] <- logdensity.bern(X[n, i, t], p[n, i, t])

                    resstat[n, i, t] <- pow(X[n, i, t] - p[n, i, t], 2) / (p[n, i, t] * (1 - p[n, i, t]))
                    X_rep[n, i, t] ~ dbern(p[n, i, t])
                    resstat_rep[n, i, t] <- pow(X_rep[n, i, t] - p[n, i, t], 2) / (p[n, i, t] * (1 - p[n, i, t]))
                }
            }
        }
        resstat_sum <- sum(resstat[1:N, 1:I, 1:T])
        resstat_rep_sum <- sum(resstat_rep[1:N, 1:I, 1:T])
        ppp <- step(resstat_rep_sum - resstat_sum)
    }
", file = modfile)

params <- c("alpha", "g", "s", "theta", "lambda0", "lambda", "mu_theta", "Sigma_theta", "delta", "log_lik", "ppp", "X_rep", "resstat_sum", "resstat_rep_sum")

inits <- function(){
    list(
        g = matrix(runif(I*T, 0, 0.5), nrow = I, ncol = T),
        s = matrix(runif(I*T, 0, 0.5), nrow = I, ncol = T),
        lambda0 = rnorm(K, 0, 4),
        lambda = abs(rnorm(K, 0, 4))
    )
}

n_chains <- 5
n_iter <- 50000
n_burnin <- 15000
n_thin <- 4

res_mcmc_lhodina <- jags(
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
Rhats <- data.frame(rownames = rownames(res_mcmc_lhodina$summary), Rhat = res_mcmc_lhodina$summary[, "Rhat"])
Rhats_over <- data.frame(Rhat = Rhats[!is.na(Rhats$Rhat) & Rhats$Rhat > 1.05, "Rhat"])
rownames(Rhats_over) <- Rhats[!is.na(Rhats$Rhat) & Rhats$Rhat > 1.05, "rownames"]
Rhats_over

alpha_indices <- grep("alpha\\[", rownames(res_mcmc_lhodina$summary))
ipar_indices <- grep("[gs]\\[", rownames(res_mcmc_lhodina$summary))
kpar_indices <- grep("(lambda0|lambda)\\[", rownames(res_mcmc_lhodina$summary))
theta_indices <- grep("^theta\\[", rownames(res_mcmc_lhodina$summary))
delta_indices <- grep("delta\\[", rownames(res_mcmc_lhodina$summary))
mu_theta_indices <- grep("mu_theta\\[", rownames(res_mcmc_lhodina$summary))
Sigma_theta_indices <- grep("Sigma_theta\\[", rownames(res_mcmc_lhodina$summary))

LHODINA_MCMC <- array(res_mcmc_lhodina$summary[alpha_indices, "mean"], dim = c(N, K, T))
alpha_mcmc_lhodina <- array(res_mcmc_lhodina$summary[alpha_indices, "50%"], dim = c(N, K, T))
ipar_mcmc_lhodina_mean <- matrix(res_mcmc_lhodina$summary[ipar_indices, "mean"], nrow = I, ncol = 2*T)
ipar_mcmc_lhodina_sd <- matrix(res_mcmc_lhodina$summary[ipar_indices, "sd"], nrow = I, ncol = 2*T)
ipar_mcmc_lhodina <- cbind(ipar_mcmc_lhodina_mean[,1], ipar_mcmc_lhodina_sd[,1], ipar_mcmc_lhodina_mean[,2], ipar_mcmc_lhodina_sd[,2], ipar_mcmc_lhodina_mean[,3], ipar_mcmc_lhodina_sd[,3], ipar_mcmc_lhodina_mean[,4], ipar_mcmc_lhodina_sd[,4])
colnames(ipar_mcmc_lhodina) <- c("guessing T1", "SD", "guessing T2", "SD", "slipping T1", "SD", "slipping T2", "SD")
rownames(ipar_mcmc_lhodina) <- c(paste0("Q", 1:I))
kpar_mcmc_lhodina_mean <- matrix(res_mcmc_lhodina$summary[kpar_indices, "mean"], nrow = K, ncol = 2)
kpar_mcmc_lhodina_sd <- matrix(res_mcmc_lhodina$summary[kpar_indices, "sd"], nrow = K, ncol = 2)
kpar_mcmc_lhodina <- cbind(kpar_mcmc_lhodina_mean[,1], kpar_mcmc_lhodina_sd[,1], kpar_mcmc_lhodina_mean[,2], kpar_mcmc_lhodina_sd[,2])
colnames(kpar_mcmc_lhodina) <- c("lambda0", "SD0", "lambda", "SD")
rownames(kpar_mcmc_lhodina) <- c(paste0("A", 1:K))
theta_mcmc_lhodina_mean <- matrix(res_mcmc_lhodina$summary[theta_indices, "mean"], nrow = N, ncol = T)
theta_mcmc_lhodina_sd <- matrix(res_mcmc_lhodina$summary[theta_indices, "sd"], nrow = N, ncol = T)
theta_mcmc_lhodina <- cbind(theta_mcmc_lhodina_mean[,1], theta_mcmc_lhodina_sd[,1], theta_mcmc_lhodina_mean[,2], theta_mcmc_lhodina_sd[,2])
colnames(theta_mcmc_lhodina) <- c("T1", "SD1", "T2", "SD2")
delta_mcmc_lhodina_mean <- matrix(res_mcmc_lhodina$summary[delta_indices, "mean"], nrow = T, ncol = 1)
delta_mcmc_lhodina_sd <- matrix(res_mcmc_lhodina$summary[delta_indices, "sd"], nrow = T, ncol = 1)
delta_mcmc_lhodina <- cbind(delta_mcmc_lhodina_mean, delta_mcmc_lhodina_sd)
colnames(delta_mcmc_lhodina) <- c("delta", "SD")
rownames(delta_mcmc_lhodina) <- c(paste0("T", 1:T))
mu_theta_mcmc_lhodina_mean <- matrix(res_mcmc_lhodina$summary[mu_theta_indices, "mean"], nrow = T, ncol = 1)
mu_theta_mcmc_lhodina_sd <- matrix(res_mcmc_lhodina$summary[mu_theta_indices, "sd"], nrow = T, ncol = 1)
mu_theta_mcmc_lhodina <- cbind(mu_theta_mcmc_lhodina_mean, mu_theta_mcmc_lhodina_sd)
colnames(mu_theta_mcmc_lhodina) <- c("mu_theta", "SD")
rownames(mu_theta_mcmc_lhodina) <- c(paste0("T", 1:T))
Sigma_theta_mcmc_lhodina_mean <- matrix(res_mcmc_lhodina$summary[Sigma_theta_indices, "mean"], nrow = T, ncol = T)
Sigma_theta_mcmc_lhodina_sd <- matrix(res_mcmc_lhodina$summary[Sigma_theta_indices, "sd"], nrow = T, ncol = T)

sumcorr <- rowSums(X[,,1])
for (t in 2:T) {
    sumcorr <- cbind(sumcorr, rowSums(X[,,t]))
}
i_theta <- cbind(theta_mcmc_lhodina_mean, sumcorr)
colnames(i_theta) <- c(paste0("theta T", 1:T), paste0("sum T", 1:T))

log_lik_array <- res_mcmc_lhodina$sims.list$log_lik
log_lik_mat <- matrix(0, nrow = dim(log_lik_array)[1], ncol = N*I*T)
for (m in 1:dim(log_lik_array)[1]) {
    log_lik_mat[m,] <- unlist(log_lik_array[m,,,])
}

waic_mcmc_lhodina <- waic(log_lik_mat)
ppp_mcmc_lhodina <- pp.check(res_mcmc_lhodina, observed = "resstat_sum", simulated = "resstat_rep_sum")
waic_mcmc_lhodina
ppp_mcmc_lhodina

pat_table <- matrix(0, nrow = N, ncol = T)
for (t in 1:T) {
    pat_table[,t] <- apply(alpha_mcmc_lhodina[,,t], 1, function(x) {
        paste0(x[1],x[2],x[3])
    })
}
t1_pat <- c("000", "100", "010", "001", "110", "101", "011", "111", pat_table[complete_indices,1])
t2_pat <- c("000", "100", "010", "001", "110", "101", "011", "111", pat_table[complete_indices,2])
xtable_12 <- table(t1_pat, t2_pat)
diag(xtable_12) <- diag(xtable_12) - 1
xtable_12 <- cbind(xtable_12, table(t1_pat) - 1)
xtable_12 <- rbind(xtable_12, colSums(xtable_12))
colnames(xtable_12) <- c(colnames(xtable_12)[1:8], "SUM1")
rownames(xtable_12) <- c(rownames(xtable_12)[1:8], "SUM2")

# ---------------------------
# output
#
MCMCtrace(res_mcmc_lhodina, params = c("g", "s", "lambda0", "lambda", "theta", "mu_theta", "Sigma_theta", "delta"), filename = paste0("MCMCtrace_lhodina_t", T, ".pdf"))

# write mastery probability
for (t in 1:T) {
    write.csv(LHODINA_MCMC[,,t], paste0("prob_T", T, "_", t, ".csv"), quote = FALSE)
}
# write fullcsv
for (t in 1:T) {
    write.csv(datafull[,,t], paste0("full_T", T, "_", t, ".csv"), quote = FALSE)
}

# save RData
save(list = ls(), file = paste0("t", T, ".RData"))
