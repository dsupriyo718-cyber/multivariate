# ==========================================================
# Problem - 4: ICA - 01
# Extracting two independent tones from their linear mixture
# ==========================================================

# ---- (a) Generate two Tones (s1, s2) corrupted by Gaussian noise ----
library(MASS)   # To use mvrnorm()
set.seed(123)

# Tone 1 corrupted by noise
s1 <- 0.7*sin((1:1000)/19 + 0.57*pi) + mvrnorm(n = 1000, mu = 0, Sigma = 0.004)
s1 <- as.numeric(s1)

# Tone 2 corrupted by noise
s2 <- sin((1:1000)/33) + mvrnorm(n = 1000, mu = 0.03, Sigma = 0.005)
s2 <- as.numeric(s2)

# Plot to see Original (source) Tones
par(mfcol = c(2, 1))
plot(s1, main = "Additive Noise Corrupted Tone 1", xlab = "Time", ylab = "Amplitude")
plot(s2, main = "Additive Noise Corrupted Tone 2", xlab = "Time", ylab = "Amplitude")

# ---- (b) Create Source matrix (S) ----
S <- matrix(c(s1, s2), 1000, 2)
head(S, 5)

# ---- (c) Mix two tones with the Mixing matrix (A) ----
A <- matrix(c(1, 1.73, -2, 3.41), 2, 2, byrow = TRUE)
X <- S %*% A     # Mixing Process: X = SA
head(X)

# ---- (d) Draw scatter plots (joint distributions) ----
dev.off()   # reset the mfrow/mfcol parameter

# Scatter plot of S (source data)
plot(S, main = "Joint distribution of the source data with 2 independent components",
     xlab = "1st Dimension in S", ylab = "2nd Dimension in S")

# Scatter plot of X (mixed data)
plot(X, main = "Joint distribution of the observed linearly mixtures data, x1 and x2",
     xlab = "1st Dimension in X", ylab = "2nd Dimension in X")

# ---- (e) Estimate source tones using fastICA ----
library(fastICA)
?fastICA
est <- fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
               method = "C", row.norm = FALSE, maxit = 200, tol = 0.0001,
               verbose = TRUE)
str(est)

# Estimates of source tones
est$S

# ---- (f) Plot original and estimated tones ----
dev.off()   # reset the mfrow/mfcol parameter

ymin <- min(s1, s2, est$S[,1], est$S[,2])
ymax <- max(s1, s2, est$S[,1], est$S[,2])

plot(s1, col = "black", main = "Original and Estimated Tones",
     xlab = "Time", ylab = "Amplitude", ylim = c(ymin, ymax))
lines(est$S[,1], col = "red")     # first column of S_hat
lines(s2, col = "green")
lines(est$S[,2], col = "blue")    # second column of S_hat
mtext("Original Tones in Black and Green, Estimated Tones in Red and Blue")

# ---- Combination of 6 graphs: Original, Mixture, and Estimated Tones ----
par(mfcol = c(2, 3))
plot(1:1000, S[,1], type = "l", xlab = "Original, S1", ylab = "")
plot(1:1000, S[,2], type = "l", xlab = "Original, S2", ylab = "")
plot(1:1000, X[,1], type = "l", xlab = "Mixture, X1", ylab = "")
plot(1:1000, X[,2], type = "l", xlab = "Mixture, X2", ylab = "")
plot(1:1000, est$S[,1], type = "l", xlab = "Estimated, S1", ylab = "")
plot(1:1000, est$S[,2], type = "l", xlab = "Estimated, S2", ylab = "")
mtext("Comparison of Original, Mixture, and Estimated Tones",
      side = 3, line = -3, outer = TRUE)

# Comment:
# As can be seen from this Figure, the estimated tones are very close to the
# original source tones (their signs are reversed, but this has no significance.)

# ==========================================================
# Addition: Repeat (a) to (f) using a random mixing matrix
# ==========================================================
A_random <- matrix(rnorm(4, mean = 1, sd = 10), nrow = 2, ncol = 2)
X_random <- S %*% A_random
# ... repeat scatter plots, fastICA, and comparison plots using X_random