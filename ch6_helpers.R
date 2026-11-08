##########################################################################
# helpers.R  --  small stand-ins for functions of the packages used in the
# course code (lmtest, nortest, car, DescTools).  They use only base R, so
# the scripts run without installing anything.  If you have the packages
# installed you may delete this file and use library(...) as usual.
##########################################################################
.last <- new.env()            # remembers the last test results (used for the written conclusions)
.fig  <- function(name, w = 6.5, h = 4.2) pdf(paste0(name, ".pdf"), width = w, height = h)

# ---- lmtest::bptest : studentized Breusch-Pagan test ----
bptest <- function(formula, varformula = NULL, studentize = TRUE, data = list()) {
  dname <- paste(deparse(substitute(formula)), collapse = "")
  mod <- if (inherits(formula, "lm")) formula else lm(formula, data = data)
  if (inherits(formula, "lm")) dname <- deparse(substitute(formula)) else if (length(data)) dname <- paste0(dname, ", data = ", deparse(substitute(data)))
  if (!is.null(varformula)) dname <- paste0(dname, ", varformula = ", paste(deparse(varformula), collapse = ""))
  Z <- if (is.null(varformula)) model.matrix(mod) else model.matrix(varformula, data = data)
  e <- residuals(mod); n <- length(e)
  w <- e^2                                   # studentized version: regress e^2 on the regressors
  aux <- lm.fit(Z, w)
  r2 <- 1 - sum(aux$residuals^2) / sum((w - mean(w))^2)
  bp <- n * r2; df <- aux$rank - 1
  p <- pchisq(bp, df, lower.tail = FALSE)
  .last$bp <- p
  structure(list(statistic = c(BP = bp), parameter = c(df = df), p.value = p,
                 method = "studentized Breusch-Pagan test", data.name = dname), class = "htest")
}
# ---- nortest::ad.test : Anderson-Darling normality test ----
ad.test <- function(x) {
  dname <- deparse(substitute(x)); x <- sort(x[complete.cases(x)]); n <- length(x)
  p <- pnorm((x - mean(x)) / sd(x)); h <- (2 * seq(1:n) - 1) * (log(p) + log(1 - rev(p)))
  A <- -n - mean(h); AA <- (1 + 0.75/n + 2.25/n^2) * A
  pval <- if (AA < 0.2) 1 - exp(-13.436 + 101.14*AA - 223.73*AA^2)
          else if (AA < 0.34) 1 - exp(-8.318 + 42.796*AA - 59.938*AA^2)
          else if (AA < 0.6) exp(0.9177 - 4.279*AA - 1.38*AA^2)
          else if (AA < 10) exp(1.2937 - 5.709*AA + 0.0186*AA^2) else 3.7e-24
  .last$ad <- pval
  structure(list(statistic = c(A = A), p.value = pval, method = "Anderson-Darling normality test", data.name = dname), class = "htest")
}
# ---- nortest::lillie.test : Lilliefors (Kolmogorov-Smirnov) normality test ----
lillie.test <- function(x) {
  dname <- deparse(substitute(x)); x <- sort(x[complete.cases(x)]); n <- length(x)
  p <- pnorm((x - mean(x)) / sd(x)); Dplus <- max(seq(1:n)/n - p); Dminus <- max(p - (seq(1:n) - 1)/n); K <- max(Dplus, Dminus)
  if (n <= 100) { Kd <- K; nd <- n } else { Kd <- K * ((n/100)^0.49); nd <- 100 }
  pvalue <- exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 * Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598/sqrt(nd) + 1.67997/nd)
  if (pvalue > 0.1) {
    KK <- (sqrt(n) - 0.01 + 0.85/sqrt(n)) * K
    pvalue <- if (KK <= 0.302) 1
      else if (KK <= 0.5) 2.76773 - 19.828315*KK + 80.709644*KK^2 - 138.55152*KK^3 + 81.218052*KK^4
      else if (KK <= 0.9) -4.901232 + 40.662806*KK - 97.490286*KK^2 + 94.029866*KK^3 - 32.355711*KK^4
      else if (KK <= 1.31) 6.198765 - 19.558097*KK + 23.186922*KK^2 - 12.234627*KK^3 + 2.423045*KK^4 else 0 }
  .last$lillie <- pvalue
  structure(list(statistic = c(D = K), p.value = pvalue, method = "Lilliefors (Kolmogorov-Smirnov) normality test", data.name = dname), class = "htest")
}
# ---- car::durbinWatsonTest (normal approximation with the exact mean and variance of d) ----
durbinWatsonTest <- function(model, max.lag = 1, ...) {
  e <- residuals(model); n <- length(e); X <- model.matrix(model); p <- qr(X)$rank
  r <- sum(e[-1] * e[-n]) / sum(e^2); D <- sum(diff(e)^2) / sum(e^2)
  Q <- qr.Q(qr(X))[, 1:p, drop = FALSE]; M <- diag(n) - Q %*% t(Q)
  A <- matrix(0, n, n); diag(A) <- 2; A[1, 1] <- 1; A[n, n] <- 1
  for (i in 1:(n - 1)) { A[i, i + 1] <- -1; A[i + 1, i] <- -1 }
  MA <- M %*% A; t1 <- sum(diag(MA)); t2 <- sum(MA * t(MA)); df <- n - p
  pv <- NA
  if (df > 2) { Ed <- t1/df; V <- 2 * (t2 - t1 * Ed) / (df * (df + 2)); z <- (D - Ed)/sqrt(V); pv <- 2 * min(pnorm(z), 1 - pnorm(z)) }
  .last$dw <- pv; .last$dwD <- D
  structure(list(r = r, dw = D, p = pv), class = "dwt")
}
print.dwt <- function(x, ...) {
  cat(" lag Autocorrelation D-W Statistic p-value\n")
  cat(sprintf("   1  %13.7f    %10.6f   %7.3f\n", x$r, x$dw, x$p))
  cat(" Alternative hypothesis: rho != 0\n"); invisible(x)
}
# ---- car::qqPlot : normal Q-Q plot with a 95% pointwise envelope ----
qqPlot <- function(x, main = "Normal Q-Q plot with 95% envelope", ...) {
  n <- length(x); P <- ppoints(n); z <- qnorm(P); xs <- sort(x)
  q <- quantile(xs, c(.25, .75)); zq <- qnorm(c(.25, .75)); b <- diff(q)/diff(zq); a <- q[1] - b * zq[1]
  se <- (b / dnorm(z)) * sqrt(P * (1 - P) / n)
  plot(z, xs, xlab = "Normal quantiles", ylab = "Sample quantiles", main = main, pch = 19, col = "steelblue", ylim = range(xs, a + b*z + 2*se, a + b*z - 2*se))
  abline(a, b, col = "red", lwd = 2); lines(z, a + b*z + qnorm(.975)*se, lty = 2, col = "red"); lines(z, a + b*z - qnorm(.975)*se, lty = 2, col = "red")
}
# ---- DescTools::PostHocTest(method = "lsd") for an aov object ----
PostHocTest <- function(x, which = NULL, method = "lsd", conf.level = 0.95) {
  mf <- model.frame(x); y <- mf[[1]]; tl <- attr(terms(x), "term.labels"); if (is.null(which)) which <- tl
  dfe <- df.residual(x); MSE <- deviance(x) / dfe; res <- list()
  for (tm in which) {
    fs <- strsplit(tm, ":")[[1]]; g <- interaction(mf[fs], drop = TRUE, sep = ":", lex.order = FALSE)
    m <- tapply(y, g, mean); nn <- tapply(y, g, length); lv <- names(m); out <- NULL; k <- length(lv)
    if (k < 2) next
    for (i in 1:(k - 1)) for (j in (i + 1):k) {
      d <- m[j] - m[i]
      if (method == "hsd") { se <- sqrt(MSE / 2 * (1/nn[i] + 1/nn[j])); w <- qtukey(conf.level, k, dfe) * se; p <- ptukey(abs(d) / se, k, dfe, lower.tail = FALSE) }
      else { se <- sqrt(MSE * (1/nn[i] + 1/nn[j])); w <- qt(1 - (1 - conf.level)/2, dfe) * se; p <- 2 * pt(-abs(d/se), dfe) }
      out <- rbind(out, c(d, d - w, d + w, p)); rownames(out)[nrow(out)] <- paste(lv[j], lv[i], sep = "-") }
    colnames(out) <- c("diff", "lwr.ci", "upr.ci", "pval"); res[[tm]] <- out }
  structure(list(res = res, method = method, conf = conf.level, dfe = dfe), class = "PostHocTest")
}
print.PostHocTest <- function(x, digits = 4, ...) {
  cat("\n  Posthoc multiple comparisons of means : ", if (x$method == "hsd") "Tukey HSD" else "Fisher LSD", "\n    ", format(100 * x$conf, digits = 3), "% family-wise confidence level\n\n", sep = "")
  for (nm in names(x$res)) {
    cat("$", nm, "\n", sep = ""); o <- x$res[[nm]]
    st <- ifelse(o[, 4] < .001, "***", ifelse(o[, 4] < .01, "** ", ifelse(o[, 4] < .05, "*  ", ifelse(o[, 4] < .1, ".  ", "   "))))
    df <- data.frame(diff = format(round(o[, 1], digits)), lwr.ci = format(round(o[, 2], digits)), upr.ci = format(round(o[, 3], digits)),
                     pval = format(round(o[, 4], digits)), sig = st, row.names = rownames(o)); print(df); cat("\n") }
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n"); invisible(x)
}
# ---- multcomp::glht(..., mcp(F = "Dunnett")) for a one-way aov: comparisons with the first level, single-step adjusted p-values ----
mcp <- function(...) list(...)
glht <- function(model, linfct) structure(list(model = model, spec = linfct), class = "glht")
summary.glht <- function(object, ...) {
  m <- object$model; mf <- model.frame(m); y <- mf[[1]]; g <- mf[[2]]; fn <- names(mf)[2]; lv <- levels(g); dfe <- df.residual(m); s2 <- deviance(m) / dfe
  mu <- tapply(y, g, mean); n <- tapply(y, g, length); i <- 2:length(lv)
  est <- mu[i] - mu[1]; se <- sqrt(s2 * (1/n[i] + 1/n[1])); tv <- est / se
  R <- outer(i, i, function(a, b) (1/n[1]) / sqrt((1/n[a] + 1/n[1]) * (1/n[b] + 1/n[1]))); diag(R) <- 1
  set.seed(1); B <- 200000; Lc <- chol(R + diag(1e-10, nrow(R))); Z <- matrix(rnorm(B * length(i)), B) %*% Lc; Tm <- apply(abs(Z / sqrt(rchisq(B, dfe) / dfe)), 1, max)
  p <- sapply(abs(tv), function(t) mean(Tm >= t))
  structure(list(fn = fn, lv = lv, est = est, se = se, t = tv, p = p, dfe = dfe, model = m), class = "summary.glht")
}
print.summary.glht <- function(x, ...) {
  cat("\n\t Simultaneous Tests for General Linear Hypotheses\n\nMultiple Comparisons of Means: Dunnett Contrasts\n\n\n")
  cat("Fit: ", paste(deparse(x$model$call), collapse = ""), "\n\nLinear Hypotheses:\n", sep = "")
  nm <- paste(x$lv[-1], "-", x$lv[1], "== 0"); st <- ifelse(x$p < .001, "***", ifelse(x$p < .01, "** ", ifelse(x$p < .05, "*  ", ifelse(x$p < .1, ".  ", "   "))))
  print(data.frame(Estimate = round(x$est, 4), `Std. Error` = round(x$se, 4), `t value` = round(x$t, 3), `Pr(>|t|)` = ifelse(x$p < 1e-4, "<1e-04", format(round(x$p, 4))), sig = st, row.names = nm, check.names = FALSE))
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n(Adjusted p values reported -- single-step method; df =", x$dfe, ")\n"); invisible(x)
}
# ---- FSA::dunnTest (Dunn 1964 after Kruskal-Wallis, with tie correction) ----
dunnTest <- function(x, data, method = "bonferroni") {
  mf <- model.frame(x, data); y <- mf[[1]]; g <- factor(mf[[2]]); N <- length(y); r <- rank(y); lv <- levels(g)
  Rb <- tapply(r, g, mean); n <- tapply(r, g, length); tc <- sum(table(y)^3 - table(y)); A <- N * (N + 1) / 12 - tc / (12 * (N - 1))
  out <- NULL
  if (length(lv) > 1) for (i in 1:(length(lv) - 1)) for (j in (i + 1):length(lv)) {
    z <- (Rb[i] - Rb[j]) / sqrt(A * (1/n[i] + 1/n[j])); out <- rbind(out, data.frame(Comparison = paste(lv[i], "-", lv[j]), Z = z, P.unadj = 2 * pnorm(-abs(z)))) }
  out$P.adj <- p.adjust(out$P.unadj, method = method); rownames(out) <- NULL
  structure(list(method = method, res = out), class = "dunnTest")
}
print.dunnTest <- function(x, ...) {
  cat("Dunn (1964) Kruskal-Wallis multiple comparison\n  p-values adjusted with the", if (x$method == "bonferroni") "Bonferroni" else x$method, "method.\n")
  d <- x$res; d$Z <- round(d$Z, 6); d$P.unadj <- signif(d$P.unadj, 6); d$P.adj <- signif(d$P.adj, 6); print(d); invisible(x)
}
# ---- Lenth's pseudo standard error ----
lenth <- function(eff, alpha = 0.05) {
  ae <- abs(eff); s0 <- 1.5 * median(ae); pse <- 1.5 * median(ae[ae < 2.5 * s0]); m <- length(eff); d <- m/3
  ME <- qt(1 - alpha/2, d) * pse; SME <- qt((1 + (1 - alpha)^(1/m))/2, d) * pse
  structure(list(PSE = pse, ME = ME, SME = SME, df = d, effects = eff, t = eff/pse), class = "lenth")
}
print.lenth <- function(x, ...) {
  cat(sprintf("Lenth's method:  PSE = %.4f   ME (individual, 5%%) = %.4f   SME (simultaneous) = %.4f   df = %.2f\n", x$PSE, x$ME, x$SME, x$df))
  tb <- data.frame(effect = round(x$effects, 4), t.PSE = round(x$t, 3), active = ifelse(abs(x$effects) > x$SME, "yes (SME)", ifelse(abs(x$effects) > x$ME, "yes (ME)", "no")))
  print(tb); invisible(x)
}
# ---- coded (+1/-1) effects: effect = 2 x regression coefficient ----
codedFrame <- function(dat, fac) { cd <- data.frame(lapply(dat[fac], function(f) as.numeric(as.character(f)))); cd$y <- dat$y; cd }
effTable <- function(dat, fac, alpha = 0.05) {
  cd <- codedFrame(dat, fac); f <- as.formula(paste("y ~", paste(tolower(fac), collapse = "*"))); names(cd)[seq_along(fac)] <- tolower(fac)
  m <- lm(f, data = cd); cf <- coef(m)[-1]; nm <- gsub(":", "", toupper(names(cf))); N <- nrow(cd); eff <- 2 * cf
  tb <- data.frame(effect = eff, SS = N * eff^2 / 4, pct = 100 * (N * eff^2 / 4) / sum(N * eff^2 / 4), row.names = nm)
  dfe <- df.residual(m)
  if (dfe > 0) { se <- 2 * summary(m)$coefficients[-1, 2]; tq <- qt(1 - alpha/2, dfe)
    tb <- data.frame(effect = eff, SE = se, lower = eff - tq*se, upper = eff + tq*se, t = eff/se, p = 2 * pt(-abs(eff/se), dfe), SS = N * eff^2/4, pct = tb$pct, row.names = nm) }
  tb
}
activeTerms <- function(dat, fac, alpha = 0.05) {
  tb <- effTable(dat, fac, alpha)
  if ("p" %in% names(tb)) rownames(tb)[tb$p < alpha] else { L <- lenth(tb$effect, alpha); rownames(tb)[abs(tb$effect) > L$ME] }
}
# ---- model quality: R2, adjusted R2, PRESS, prediction R2 ----
fitStats <- function(m) {
  e <- residuals(m); h <- hatvalues(m); press <- sum((e/(1 - h))^2); y <- model.response(model.frame(m)); sst <- sum((y - mean(y))^2)
  s <- summary(m)
  c(R2 = s$r.squared, adjR2 = s$adj.r.squared, MSE = sum(e^2)/df.residual(m), PRESS = press, predR2 = 1 - press/sst)
}
eqn <- function(m, digits = 4, yname = "yhat") {
  cf <- coef(m); nm <- gsub(":", "", toupper(names(cf))); nm[1] <- ""
  s <- paste0(yname, " = ", format(round(cf[1], digits), nsmall = 0))
  for (i in seq_along(cf)[-1]) s <- paste0(s, ifelse(cf[i] < 0, " - ", " + "), format(round(abs(cf[i]), digits), nsmall = 0), " ", nm[i])
  s
}
# ---- plots: grouped box plots and interaction plots for every pair of factors ----
boxPair <- function(dat, a, b) {
  la <- sort(unique(dat[[a]])); lb <- sort(unique(dat[[b]])); cols <- c("steelblue", "orange"); grp <- list(); at <- c(); k <- 0
  for (i in seq_along(la)) for (j in seq_along(lb)) { k <- k + 1; grp[[k]] <- dat$y[dat[[a]] == la[i] & dat[[b]] == lb[j]]; at <- c(at, i * (length(lb) + 1) + j - length(lb) - 1) }
  yl <- range(dat$y); yl[2] <- yl[2] + 0.22 * diff(yl)
  boxplot(grp, at = at, col = rep(cols, length(la)), names = rep("", length(grp)), main = paste("Response by factors", a, "and", b), ylab = "Response", xlab = paste("Factor", a), ylim = yl)
  axis(1, at = sapply(seq_along(la), function(i) mean(at[((i - 1) * length(lb) + 1):(i * length(lb))])), labels = la, tick = FALSE, line = 0)
  legend("top", legend = paste(b, "=", lb), fill = cols, bty = "n", cex = .8, horiz = TRUE)
}
panelRows <- function(m, ncol = 2) c(ceiling(m / ncol), min(m, ncol))
cubePlot <- function(dat, f, ranges = FALSE, main = NULL) {
  m <- tapply(dat$y, list(dat[[f[1]]], dat[[f[2]]], dat[[f[3]]]), mean); r <- tapply(dat$y, list(dat[[f[1]]], dat[[f[2]]], dat[[f[3]]]), function(v) max(v) - min(v))
  lv <- dimnames(m)[[1]]; plot(NULL, xlim = c(0, 3.4), ylim = c(0, 3.2), axes = FALSE, xlab = "", ylab = "", asp = 1, main = ifelse(is.null(main), paste("Cube plot:", paste(f, collapse = ", ")), main))
  xy <- function(i, j, k) c(0.4 + (i - 1) * 1.7 + (k - 1) * 0.9, 0.3 + (j - 1) * 1.6 + (k - 1) * 0.8)
  for (i in 1:2) for (j in 1:2) for (k in 1:2) { p <- xy(i, j, k)
    if (i == 1) segments(p[1], p[2], xy(2, j, k)[1], xy(2, j, k)[2]); if (j == 1) segments(p[1], p[2], xy(i, 2, k)[1], xy(i, 2, k)[2]); if (k == 1) segments(p[1], p[2], xy(i, j, 2)[1], xy(i, j, 2)[2])
    lab <- format(round(m[i, j, k], 3)); if (ranges) lab <- paste0(lab, "\n(R=", format(round(r[i, j, k], 2)), ")")
    rect(p[1] - .3, p[2] - .12 - ifelse(ranges, .08, 0), p[1] + .3, p[2] + .12 + ifelse(ranges, .08, 0), col = "white", border = "grey40"); text(p[1], p[2], lab, cex = .75) }
  text(1.9, 0.02, paste(f[1], ":", lv[1], "->", lv[2]), cex = .8); text(0.0, 1.6, paste(f[2], ":", lv[1], "->", lv[2]), cex = .8, srt = 90); text(3.2, 3.15, paste(f[3], ":", lv[1], "->", lv[2]), cex = .8)
}
# ---- written conclusions (used for the "Interpretation" boxes on the site) ----
.cmp <- NULL
interp <- function(label, active, aovfit, corn = NULL, goal = "max", target = NA, unrep = FALSE, ordnote = "") {
  sm <- summary(aovfit)[[1]]; nm <- trimws(rownames(sm)); pv <- sm[["Pr(>F)"]]
  sig <- nm[!is.na(pv) & pv < 0.05 & nm != "Residuals"]
  cat("@@INT\n")
  if (length(active)) cat("Active effects (", ifelse(unrep, "Lenth's individual margin of error", "t tests in the replicated full model"), ", 5% level): ", paste(active, collapse = ", "), ".\n", sep = "") else cat("No effect is clearly active at the 5% level.\n")
  if (length(sig)) cat("In the reduced-model ANOVA the significant terms are ", paste(gsub(":", "", sig), collapse = ", "), " (P < 0.05).\n", sep = "") else if (any(!is.na(pv))) cat("No term of the reduced model is significant at the 5% level.\n")
  r <- rstandard(aovfit); sw <- shapiro.test(r)$p.value; ad <- .last$ad; li <- .last$lillie; bp <- .last$bp; dw <- .last$dw
  f <- function(p) ifelse(is.na(p), "n/a", ifelse(p < 0.0001, "<0.0001", sprintf("%.4f", p)))
  cat("Normality: Shapiro-Wilk P = ", f(sw), ", Anderson-Darling P = ", f(ad), ", Lilliefors P = ", f(li), " -> ", ifelse(min(sw, ad, li, na.rm = TRUE) < 0.05, "at least one test rejects normality at 5%.", "normality is not rejected at 5%."), "\n", sep = "")
  cat("Constant variance: Breusch-Pagan P = ", f(bp), " -> ", ifelse(is.na(bp), "not available.", ifelse(bp < 0.05, "equal variance is rejected at 5%.", "equal variance is not rejected at 5%.")), "\n", sep = "")
  cat("Independence: Durbin-Watson d = ", ifelse(is.null(.last$dwD), "n/a", sprintf("%.3f", .last$dwD)), ", P = ", f(dw), " -> ", ifelse(is.na(dw), "not available.", ifelse(dw < 0.05, "independence is rejected at 5%.", "independence is not rejected at 5%.")), ordnote, "\n", sep = "")
  if (!is.null(corn)) {
    o <- if (goal == "max") order(-corn$pred) else if (goal == "min") order(corn$pred) else order(abs(corn$pred - target))
    b <- corn[o[1], , drop = FALSE]; w <- corn[rev(o)[1], , drop = FALSE]
    nmz <- setdiff(names(corn), "pred")
    cat(ifelse(goal == "max", "Largest", ifelse(goal == "min", "Smallest", "Closest to the target")), " fitted mean: ", sprintf("%.4f", b$pred), " at ", paste(toupper(nmz), "=", unlist(b[nmz]), collapse = ", "), "; the opposite extreme is ", sprintf("%.4f", w$pred), " at ", paste(toupper(nmz), "=", unlist(w[nmz]), collapse = ", "), ".\n", sep = "")
  }
  .cmp <<- rbind(.cmp, data.frame(analysis = label, active = paste(active, collapse = " "), SW = round(sw, 4), BP = round(bp, 4), DW = round(dw, 4), stringsAsFactors = FALSE))
  cat("@@END\n")
}
dispFrame <- function(dat, fac) {                      # ln of the within-run variance for replicated runs
  dd <- aggregate(dat$y, by = dat[fac], FUN = var); names(dd)[ncol(dd)] <- "y"; dd$y <- log(dd$y); dd }
dispRatio <- function(dat, fac, res) {                 # Montgomery's residual-variance ratios F*_i = ln(s2(+)/s2(-))
  out <- sapply(fac, function(f) { x <- as.numeric(as.character(dat[[f]])); log(var(res[x > 0]) / var(res[x < 0])) }); out }
boxcoxLL <- function(m, lam = seq(-2, 2, 0.1)) {
  y <- model.response(model.frame(m)); n <- length(y); gm <- exp(mean(log(y))); X <- model.matrix(m)
  ll <- sapply(lam, function(l) { z <- if (abs(l) < 1e-8) gm * log(y) else (y^l - 1)/(l * gm^(l - 1)); -n/2 * log(sum(lm.fit(X, z)$residuals^2)/n) })
  list(lambda = lam, loglik = ll, best = lam[which.max(ll)], ci = range(lam[ll > max(ll) - qchisq(0.95, 1)/2]))
}
interpEff <- function(eff, alpha = 0.05) {                 # conclusions for problems that give only effect estimates
  L <- lenth(eff, alpha); cat("@@INT\n")
  cat(sprintf("Lenth: PSE = %.4f, ME = %.4f, SME = %.4f.\n", L$PSE, L$ME, L$SME))
  a1 <- names(eff)[abs(eff) > L$ME]; a2 <- names(eff)[abs(eff) > L$SME]
  cat("Effects exceeding the individual margin of error (ME): ", ifelse(length(a1), paste(a1, collapse = ", "), "none"), ".\n", sep = "")
  cat("Effects exceeding the simultaneous margin of error (SME): ", ifelse(length(a2), paste(a2, collapse = ", "), "none"), ".\n", sep = "")
  cat("@@END\n")
}
