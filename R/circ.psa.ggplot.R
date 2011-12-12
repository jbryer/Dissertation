circ.psa.ggplot <- function(response, treatment = NULL, strata = NULL, summary = FALSE, 
		statistic = "mean", trim = 0, revc = FALSE, confint = TRUE, 
		sw = 0.4, ne = 0.5, inc = 0.25, pw = 0.4, lab = TRUE, labcex = 1, 
		xlab = NULL, ylab = NULL, main = NULL) {
    if(dim(as.data.frame(response))[2] == 3) {
        treatment <- response[, 2]
        strata <- response[, 3]
        response <- response[, 1]
    }
    tr.mean <- function(x) {
        mean(x, trim = trim)
    }
    if (!trim == 0) {
        statistic <- tr.mean
    }
    if (!summary) {
        n <- length(response)
        nstrat <- dim(table(strata))
        ct.means <- tapply(response, list(strata, treatment), statistic)
        ncontrol <- as.data.frame(table(strata, treatment))[1:nstrat, 3]
        ntreat <- as.data.frame(table(strata, treatment))[(nstrat + 1):(2 * nstrat), 3]
        summary.strata <- cbind(ncontrol, ntreat, ct.means)
    } else {
        summary.strata <- response
        ct.means <- response[, 3:4]
        n <- sum(response[, 1:2])
        nstrat <- length(response[, 1])
    }
    wts <- rowSums(summary.strata[, 1:2])
    x <- ct.means[, 1]
    y <- ct.means[, 2]
    if (revc) {
        x <- ct.means[, 2]
        y <- ct.means[, 1]
    }
    d <- y - x
    xr <- range(x)
    yr <- range(y)
    min.xy <- min(xr[1], yr[1])
    max.xy <- max(xr[2], yr[2])
    lwb <- min.xy - 1.2 * ne * (max.xy - min.xy)
    upb <- max.xy + 0.5 * sw * (max.xy - min.xy)
    diff.wtd <- sum(d * wts)/n
    if (!summary) {
        o <- order(treatment)
        ord.strata <- strata[o]
        nc <- table(treatment)[1]
        nt <- table(treatment)[2]
        ord.response <- response[o]
        var.0 <- tapply(ord.response[1:nc], ord.strata[1:nc], var)
        ni.0 <- table(ord.strata[1:nc])
        frac.0 <- var.0/ncontrol
        ncp1 <- nc + 1
        ncpnt <- nc + nt
        var.1 <- tapply(ord.response[ncp1:ncpnt], ord.strata[ncp1:ncpnt], var)
        ni.1 <- table(ord.strata[ncp1:ncpnt])
        frac.1 <- var.1/ntreat
        se.wtd <- ((sum(frac.0) + sum(frac.1))^0.5)/nstrat
        strat.labels <- sort(unique(strata))
    }
    if (summary) {
        se.wtd <- NULL
        strat.labels <- rownames(response)
    }
    wtt <- wts/max(wts)
    dfrng <- diff(c(lwb, upb))
    radii <- (abs(dfrng/20)) * wtt
    #symbols(x, y, circles = radii^pw, inches = inc, xlim = c(lwb, 
    #    upb), ylim = c(lwb, upb), cex = 0.86, xlab = "", ylab = "", 
    #    main = main, lwd = 1.5)
    #if(lab) {
    #    for (i in 1:nstrat) {
    #        legend(x[i], y[i], strat.labels[i], bty = "n", xjust = 0.71, 
    #            yjust = 0.48, cex = labcex)
    #    }
    #}
    #abline(0, 1, lwd = 2)
    #abline(diff.wtd, 1, lwd = 3, col = 4, lty = 3)
    wtss <- wts/n
    mnx <- sum(x * wtss)
    mny <- sum(y * wtss)
    mnxy <- (mnx + mny)/2
    #abline(h = mny, v = mnx, lty = 3, col = 2)
    kp <- (3 * lwb + upb)/4
    ex <- 0.008 * (upb - lwb)
    for (i in 1:nstrat) {
        #segments(kp - d[i]/2 + ex, kp + d[i]/2 + ex, x[i], y[i], lty = 2, lwd = 0.8)
        #points(kp - d[i]/2 + ex, kp + d[i]/2 + ex, pch = 3, lwd = 1.4, col = 4, cex = 1.7)
    }
    ext <- 0.025 * (upb - lwb)
    #segments((lwb + upb)/2 + ext, lwb - ext, lwb - ext, (lwb + upb)/2 + ext, lwd = 2)
    #rug(x, side = 3)
    #rug(y, side = 4)
    dimnamez <- NULL
    dimnamezz <- unlist(dimnames(ct.means)[2])
    dimnamez <- unlist(dimnames(ct.means)[2])
    if(revc) 
        dimnamez <- dimnamez[c(2, 1)]
    if(is.null(xlab)) {
        #title(xlab = unlist(dimnamez)[1])
    } else {
        #title(xlab = xlab)
    }
    if (is.null(ylab)) {
        #title(ylab = unlist(dimnamez)[2])
    } else {
        #title(ylab = ylab)
    }
    Cname <- unlist(dimnamez)[1]
    Tname <- unlist(dimnamez)[2]
    C.wtd <- mnx
    T.wtd <- mny
    approx.t <- diff.wtd/se.wtd
    df <- n - 2 * nstrat
    if (!summary) {
        colnames(summary.strata) <- c(paste("n.", dimnamezz[1], 
            sep = ""), paste("n.", dimnamezz[2], sep = ""), paste("means.", 
            dimnamezz[1], sep = ""), paste("means.", dimnamezz[2], 
            sep = ""))
    }
    out <- list(summary.strata, C.wtd, T.wtd, diff.wtd, se.wtd, 
        approx.t = approx.t, df = df)
    names(out) <- c("summary.strata", paste("wtd.Mn.", Cname, 
        sep = ""), paste("wtd.Mn.", Tname, sep = ""), "ATE", 
        "se.wtd", "approx.t", "df")
    if (confint) {
        ci.diff <- -diff.wtd
        ci <- c(ci.diff - qt(0.975, df) * se.wtd, ci.diff + qt(0.975, 
            df) * se.wtd)
        ci.out <- -ci[c(2, 1)]
        xl <- 0.75 * lwb + 0.25 * upb - ext + ci[1]/2
        yl <- 0.75 * lwb + 0.25 * upb - ext - ci[1]/2
        xu <- 0.75 * lwb + 0.25 * upb - ext + ci[2]/2
        yu <- 0.75 * lwb + 0.25 * upb - ext - ci[2]/2
        segments(xl, yl, xu, yu, lwd = 5, col = "green3")
        segments(xl - 0.05 * ext + 0.7 * ext/2, yl + 0.05 * ext + 
            0.7 * ext/2, xl - 0.05 * ext - 0.7 * ext/2, yl + 
            0.05 * ext - 0.7 * ext/2, lwd = 2, col = "green3")
        segments(xu + 0.05 * ext + 0.7 * ext/2, yu - 0.05 * ext + 
            0.7 * ext/2, xu + 0.05 * ext - 0.7 * ext/2, yu - 
            0.05 * ext - 0.7 * ext/2, lwd = 2, col = "green3")
        out <- list(summary.strata, C.wtd, T.wtd, diff.wtd, se.wtd, 
            approx.t, df, ci.out)
        names(out) <- c("summary.strata", paste("wtd.Mn.", Cname, 
            sep = "", collapse = ""), paste("wtd.Mn.", Tname, 
            sep = ""), "ATE", "se.wtd", "approx.t", "df", "CI.95")
    }
    return(out)
}