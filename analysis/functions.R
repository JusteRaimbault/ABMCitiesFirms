

directedmodularity<-function(membership,adjacency){
  m=sum(adjacency)
  kout=rowSums(adjacency);kin=colSums(adjacency)
  res = 0;k=length(unique(membership))
  for(c in unique(membership)){
    #if(c%%100==0){show(c/k)}
    inds=which(membership==c)
    res = res + sum(adjacency[inds,inds]) - sum(kin[inds])*sum(kout[inds])/m 
    #gc()
  }
  return(res/m)
}

maxmodularity<-function(membership,adjacency){
  m=sum(adjacency)/2
  kout=rowSums(adjacency);kin=colSums(adjacency)
  res = 0;k=length(unique(membership))
  for(c in unique(membership)){
    inds=which(membership==c)
    res = res + 2*m - sum(kin[inds])*sum(kout[inds])/(2*m) 
  }
  return(res/(2*m))
}


fitDistrPowerLaw<-function(x,xlab='x',ylab='CDF',file='fitDistrPowerLaw.png'){
  degpowerlaw = conpl$new(x)
  est = estimate_xmin(degpowerlaw,xmax = max(x))
  degpowerlaw$setXmin(est)
  png(file,width=15,height=15,units='cm',res=300)
  plot(degpowerlaw,xlab=xlab,ylab=ylab);lines(degpowerlaw, col=2, lwd=2)
  degln = conlnorm$new(x)
  est = estimate_xmin(degln)
  degln$setXmin(est)
  lines(degln, col=3, lwd=2)
  text(x=min(x),y=0.005,adj=c(0,0),labels = paste0('Log-normal: mu=',round(degln$pars[1],digits=2),', sigma=',round(degln$pars[2],digits=2),', xmin=',round(degln$xmin,digits=2)),cex=0.6)
  text(x=min(x),y=0.003,adj=c(0,0),labels = paste0('Power law: alpha=',round(degpowerlaw$pars[1],digits=2),', xmin=',round(degpowerlaw$xmin,digits=2)),cex=0.6)
  dev.off()
  return(list(powerlaw=degpowerlaw,ln=degln))
}


map<- function(data,var,sizevar,filename,countries,discrete=FALSE,legendtitle=NULL,legendsizetitle=NULL,xlim=c(-130,150),ylim=c(-50, 60),width=30,height=12){
  #WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify
  #sizes = log10(data[[sizevar]]);sizes = (sizes - min(sizes,na.rm = T)) / (max(sizes,na.rm = T) - min(sizes,na.rm = T))
  #data[['sizes']]=sizes
  g=ggplot()+
    #geom_map(data = WorldData, map = WorldData,aes(group = group, map_id=region),fill = "white", colour = "#7f7f7f", size=0.1) + 
    geom_sf(data=countries,fill = "white", colour = "#7f7f7f", size=0.1)+
    geom_point(data=data,aes_string(x='X',y='Y',color=var,size=sizevar),alpha=0.5)+
    #scale_size_area(name=ifelse(is.null(legendsizetitle),sizevar,legendsizetitle))+#,trans="log10")+
    scale_size(name=ifelse(is.null(legendsizetitle),sizevar,legendsizetitle),range=c(1,8))+#,trans="log10")+
    #geom_map(data = areasmorph, map=WorldData,
    #         aes(fill=moran2015),#, map_id=region),
    #         colour="#7f7f7f", size=0.5) +
    #coord_map("mollweide")+ # coord_map("rectangular",lat0=lat0, xlim=xlim, ylim=ylim)
    #coord_sf("rectangular",xlim=xlim, ylim=ylim)+
    #scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
    theme_bw()+xlab("")+ylab("")+
    xlim(xlim)+ylim(ylim)+theme(axis.text = element_blank(),axis.ticks = element_blank())
  #scale_y_continuous(limits=ylim,breaks=c(),labels = c()) +
  #scale_x_continuous(limits=xlim,breaks=c(),labels = c())
  if(discrete){
    g+scale_color_discrete(name=ifelse(is.null(legendtitle),var,legendtitle))
  }else{
    g+scale_color_distiller(palette = 'Spectral',na.value ='white',name=ifelse(is.null(legendtitle),var,legendtitle))
  }
  ggsave(filename = filename,width=width,height=height,units='cm',dpi = 600)
}


#formula=intweight~log(distance)+log(from_turnover)+log(to_turnover)+log(sim)+from_country+to_country


zeroinfl_64 <- function (formula, data, subset, na.action, weights, offset, 
          dist = c("poisson", "negbin", "geometric"), link = c("logit", 
                                                               "probit", "cloglog", "cauchit", "log"), control = zeroinfl.control(...), 
          model = TRUE, y = TRUE, x = FALSE, ...) 
{
  ziPoisson <- function(parms, trunc.start = FALSE) {
    mu <- as.vector(exp(X %*% parms[1:kx] + offsetx))
    if (trunc.start) 
      phi <- rep(0, length(mu))
    else phi <- as.vector(linkinv(Z %*% parms[(kx + 1):(kx + 
                                                          kz)] + offsetz))
    loglik0 <- log(phi + exp(log(1 - phi) - mu))
    loglik1 <- log(1 - phi) + dpois(Y, lambda = mu, log = TRUE)
    if (trunc.start) 
      sum(weights[Y1] * loglik1[Y1]) - sum(weights[Y1] * 
                                             log(1 - exp(loglik0[Y1])))
    else sum(weights[Y0] * loglik0[Y0]) + sum(weights[Y1] * 
                                                loglik1[Y1])
  }
  ziNegBin <- function(parms, trunc.start = FALSE) {
    mu <- as.vector(exp(X %*% parms[1:kx] + offsetx))
    if (trunc.start) 
      phi <- rep(0, length(mu))
    else phi <- as.vector(linkinv(Z %*% parms[(kx + 1):(kx + 
                                                          kz)] + offsetz))
    theta <- exp(parms[(kx + kz) + 1])
    loglik0 <- log(phi + exp(log(1 - phi) + suppressWarnings(dnbinom(0, 
                                                                     size = theta, mu = mu, log = TRUE))))
    loglik1 <- log(1 - phi) + suppressWarnings(dnbinom(Y, 
                                                       size = theta, mu = mu, log = TRUE))
    if (trunc.start) 
      sum(weights[Y1] * loglik1[Y1]) - sum(weights[Y1] * 
                                             log(1 - exp(loglik0[Y1])))
    else sum(weights[Y0] * loglik0[Y0]) + sum(weights[Y1] * 
                                                loglik1[Y1])
  }
  ziGeom <- function(parms, trunc.start = FALSE) ziNegBin(c(parms, 
                                                            0), trunc.start)
  countGradPoisson <- function(parms) {
    eta <- as.vector(X %*% parms[1:kx] + offsetx)[Y1]
    mu <- exp(eta)
    colSums(((Y[Y1] - mu) - exp(ppois(0, lambda = mu, log.p = TRUE) - 
                                  ppois(0, lambda = mu, lower.tail = FALSE, log.p = TRUE) + 
                                  eta)) * weights[Y1] * X[Y1, , drop = FALSE])
  }
  countGradGeom <- function(parms) {
    eta <- as.vector(X %*% parms[1:kx] + offsetx)[Y1]
    mu <- exp(eta)
    colSums(((Y[Y1] - mu * (Y[Y1] + 1)/(mu + 1)) - exp(pnbinom(0, 
                                                               mu = mu, size = 1, log.p = TRUE) - pnbinom(0, mu = mu, 
                                                                                                          size = 1, lower.tail = FALSE, log.p = TRUE) - log(mu + 
                                                                                                                                                              1) + eta)) * weights[Y1] * X[Y1, , drop = FALSE])
  }
  countGradNegBin <- function(parms) {
    eta <- as.vector(X %*% parms[1:kx] + offsetx)[Y1]
    mu <- exp(eta)
    theta <- exp(parms[kx + 1])
    logratio <- pnbinom(0, mu = mu, size = theta, log.p = TRUE) - 
      pnbinom(0, mu = mu, size = theta, lower.tail = FALSE, 
              log.p = TRUE)
    rval <- colSums(((Y[Y1] - mu * (Y[Y1] + theta)/(mu + 
                                                      theta)) - exp(logratio + log(theta) - log(mu + theta) + 
                                                                      eta)) * weights[Y1] * X[Y1, , drop = FALSE])
    rval2 <- sum((digamma(Y[Y1] + theta) - digamma(theta) + 
                    log(theta) - log(mu + theta) + 1 - (Y[Y1] + theta)/(mu + 
                                                                          theta) + exp(logratio) * (log(theta) - log(mu + theta) + 
                                                                                                      1 - theta/(mu + theta))) * weights[Y1]) * theta
    c(rval, rval2)
  }
  gradPoisson <- function(parms) {
    eta <- as.vector(X %*% parms[1:kx] + offsetx)
    mu <- exp(eta)
    etaz <- as.vector(Z %*% parms[(kx + 1):(kx + kz)] + offsetz)
    muz <- linkinv(etaz)
    clogdens0 <- -mu
    dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) + 
                                                clogdens0)
    wres_count <- ifelse(Y1, Y - mu, -exp(-log(dens0) + log(1 - 
                                                              muz) + clogdens0 + log(mu)))
    wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz), 
                        (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
    colSums(cbind(wres_count * weights * X, wres_zero * weights * 
                    Z))
  }
  gradGeom <- function(parms) {
    eta <- as.vector(X %*% parms[1:kx] + offsetx)
    mu <- exp(eta)
    etaz <- as.vector(Z %*% parms[(kx + 1):(kx + kz)] + offsetz)
    muz <- linkinv(etaz)
    clogdens0 <- dnbinom(0, size = 1, mu = mu, log = TRUE)
    dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) + 
                                                clogdens0)
    wres_count <- ifelse(Y1, Y - mu * (Y + 1)/(mu + 1), -exp(-log(dens0) + 
                                                               log(1 - muz) + clogdens0 - log(mu + 1) + log(mu)))
    wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz), 
                        (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
    colSums(cbind(wres_count * weights * X, wres_zero * weights * 
                    Z))
  }
  gradNegBin <- function(parms) {
    eta <- as.vector(X %*% parms[1:kx] + offsetx)
    mu <- exp(eta)
    etaz <- as.vector(Z %*% parms[(kx + 1):(kx + kz)] + offsetz)
    muz <- linkinv(etaz)
    theta <- exp(parms[(kx + kz) + 1])
    clogdens0 <- dnbinom(0, size = theta, mu = mu, log = TRUE)
    dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) + 
                                                clogdens0)
    wres_count <- ifelse(Y1, Y - mu * (Y + theta)/(mu + theta), 
                         -exp(-log(dens0) + log(1 - muz) + clogdens0 + log(theta) - 
                                log(mu + theta) + log(mu)))
    wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz), 
                        (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
    wres_theta <- theta * ifelse(Y1, digamma(Y + theta) - 
                                   digamma(theta) + log(theta) - log(mu + theta) + 1 - 
                                   (Y + theta)/(mu + theta), exp(-log(dens0) + log(1 - 
                                                                                     muz) + clogdens0) * (log(theta) - log(mu + theta) + 
                                                                                                            1 - theta/(mu + theta)))
    colSums(cbind(wres_count * weights * X, wres_zero * weights * 
                    Z, wres_theta))
  }
  dist <- match.arg(dist)
  loglikfun <- switch(dist, poisson = ziPoisson, geometric = ziGeom, 
                      negbin = ziNegBin)
  gradfun <- switch(dist, poisson = gradPoisson, geometric = gradGeom, 
                    negbin = gradNegBin)
  linkstr <- match.arg(link)
  linkobj <- make.link(linkstr)
  linkinv <- linkobj$linkinv
  if (control$trace) 
    cat("Zero-inflated Count Model\n", paste("count model:", 
                                             dist, "with log link\n"), paste("zero-inflation model: binomial with", 
                                                                             linkstr, "link\n"), sep = "")
  cl <- match.call()
  if (missing(data)) 
    data <- environment(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "weights", 
               "offset"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  if (length(formula[[3]]) > 1 && identical(formula[[3]][[1]], 
                                            as.name("|"))) {
    ff <- formula
    formula[[3]][1] <- call("+")
    mf$formula <- formula
    ffc <- . ~ .
    ffz <- ~.
    ffc[[2]] <- ff[[2]]
    ffc[[3]] <- ff[[3]][[2]]
    ffz[[3]] <- ff[[3]][[3]]
    ffz[[2]] <- NULL
  }else {
    ffz <- ffc <- ff <- formula
    ffz[[2]] <- NULL
  }
  if (inherits(try(terms(ffz), silent = TRUE), "try-error")) {
    ffz <- eval(parse(text = sprintf(paste("%s -", deparse(ffc[[2]])), 
                                     deparse(ffz))))
  }
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  mtX <- terms(ffc, data = data)
  X <- model.matrix(mtX, mf)
  mtZ <- terms(ffz, data = data)
  mtZ <- terms(update(mtZ, ~.), data = data)
  Z <- model.matrix(mtZ, mf)
  Y <- model.response(mf, "numeric")
  if (length(Y) < 1) 
    stop("empty model")
  if (all(Y > 0)) 
    stop("invalid dependent variable, minimum count is not zero")
  if (!isTRUE(all.equal(Y, as.integer64(round(Y + 
                                                       0.001)))))
    stop("invalid dependent variable, non-integer values")
  Y <- as.integer64(round(Y + 0.001))
  if (any(Y < 0)) 
    stop("invalid dependent variable, negative counts")
  if (control$trace) {
    cat("dependent variable:\n")
    tab <- table(factor(Y, levels = 0:max(Y)), exclude = NULL)
    names(dimnames(tab)) <- NULL
    print(tab)
  }
  n <- length(Y)
  kx <- NCOL(X)
  kz <- NCOL(Z)
  Y0 <- Y <= 0
  Y1 <- Y > 0
  weights <- model.weights(mf)
  if (is.null(weights)) 
    weights <- 1
  if (length(weights) == 1) 
    weights <- rep.int(weights, n)
  weights <- as.vector(weights)
  names(weights) <- rownames(mf)
  offsetx <- pscl:::model_offset_2(mf, terms = mtX, offset = TRUE)
  if (is.null(offsetx)) 
    offsetx <- 0
  if (length(offsetx) == 1) 
    offsetx <- rep.int(offsetx, n)
  offsetx <- as.vector(offsetx)
  offsetz <- pscl:::model_offset_2(mf, terms = mtZ, offset = FALSE)
  if (is.null(offsetz)) 
    offsetz <- 0
  if (length(offsetz) == 1) 
    offsetz <- rep.int(offsetz, n)
  offsetz <- as.vector(offsetz)
  start <- control$start
  if (!is.null(start)) {
    valid <- TRUE
    if (!("count" %in% names(start))) {
      valid <- FALSE
      warning("invalid starting values, count model coefficients not specified")
      start$count <- rep.int(0, kx)
    }
    if (!("zero" %in% names(start))) {
      valid <- FALSE
      warning("invalid starting values, zero-inflation model coefficients not specified")
      start$zero <- rep.int(0, kz)
    }
    if (length(start$count) != kx) {
      valid <- FALSE
      warning("invalid starting values, wrong number of count model coefficients")
    }
    if (length(start$zero) != kz) {
      valid <- FALSE
      warning("invalid starting values, wrong number of zero-inflation model coefficients")
    }
    if (dist == "negbin") {
      if (!("theta" %in% names(start))) 
        start$theta <- 1
      start <- list(count = start$count, zero = start$zero, 
                    theta = as.vector(start$theta[1]))
    }
    else {
      start <- list(count = start$count, zero = start$zero)
    }
    if (!valid) 
      start <- NULL
  }
  method <- control$method
  hessian <- control$hessian
  ocontrol <- control
  control$method <- control$hessian <- control$EM <- control$start <- NULL
  if (is.null(start)) {
    if (control$trace) 
      cat("generating starting values...")
    model_zero <- glm.fit(Z, as.integer64(Y0), weights = weights, 
                          family = binomial(link = linkstr), offset = offsetz)
    countloglikfun <- function(parms) loglikfun(c(parms[1:kx], 
                                                  rep(0, kz), parms[-(1:kx)]), trunc.start = TRUE)
    countgradfun <- switch(dist, poisson = countGradPoisson, 
                           geometric = countGradGeom, negbin = countGradNegBin)
    lmstart <- lm.wfit(X[Y1, , drop = FALSE], log(Y[Y1]) - 
                         offsetx[Y1], weights[Y1])$coefficients
    lmstart <- ifelse(is.na(lmstart), 0, lmstart)
    fit <- tryCatch(optim(fn = countloglikfun, gr = countgradfun, 
                          par = c(lmstart, if (dist == "negbin") 0 else NULL), 
                          method = method, hessian = FALSE, control = control), 
                    error = function(e) list(convergence = 1))
    if (fit$convergence > 0) {
      model_count <- glm.fit(X, Y, family = poisson(), 
                             weights = weights, offset = offsetx)
      start <- list(count = model_count$coefficients, zero = model_zero$coefficients)
      if (dist == "negbin") 
        start$theta <- 1
    }
    else {
      start <- list(count = fit$par[1:kx], zero = model_zero$coefficients)
      if (length(fit$par) > kx) 
        start$theta <- exp(fit$par[-(1:kx)])
    }
    if (ocontrol$EM & dist == "poisson") {
      mui <- model_count$fitted
      probi <- model_zero$fitted
      probi <- probi/(probi + (1 - probi) * dpois(0, mui))
      probi[Y1] <- 0
      ll_new <- loglikfun(c(start$count, start$zero))
      ll_old <- 2 * ll_new
      while (abs((ll_old - ll_new)/ll_old) > control$reltol) {
        ll_old <- ll_new
        model_count <- glm.fit(X, Y, weights = weights * 
                                 (1 - probi), offset = offsetx, family = poisson(), 
                               start = start$count)
        model_zero <- suppressWarnings(glm.fit(Z, probi, 
                                               weights = weights, offset = offsetz, family = binomial(link = linkstr), 
                                               start = start$zero))
        mui <- model_count$fitted
        probi <- model_zero$fitted
        probi <- probi/(probi + (1 - probi) * dpois(0, 
                                                    mui))
        probi[Y1] <- 0
        start <- list(count = model_count$coefficients, 
                      zero = model_zero$coefficients)
        ll_new <- loglikfun(c(start$count, start$zero))
      }
    }
    if (ocontrol$EM & dist == "geometric") {
      mui <- model_count$fitted
      probi <- model_zero$fitted
      probi <- probi/(probi + (1 - probi) * dnbinom(0, 
                                                    size = 1, mu = mui))
      probi[Y1] <- 0
      ll_new <- loglikfun(c(start$count, start$zero))
      ll_old <- 2 * ll_new
      while (abs((ll_old - ll_new)/ll_old) > control$reltol) {
        ll_old <- ll_new
        model_count <- suppressWarnings(glm.fit(X, Y, 
                                                weights = weights * (1 - probi), offset = offsetx, 
                                                family = MASS::negative.binomial(1), start = start$count))
        model_zero <- suppressWarnings(glm.fit(Z, probi, 
                                               weights = weights, offset = offsetz, family = binomial(link = linkstr), 
                                               start = start$zero))
        start <- list(count = model_count$coefficients, 
                      zero = model_zero$coefficients)
        mui <- model_count$fitted
        probi <- model_zero$fitted
        probi <- probi/(probi + (1 - probi) * dnbinom(0, 
                                                      size = 1, mu = mui))
        probi[Y1] <- 0
        ll_new <- loglikfun(c(start$count, start$zero))
      }
    }
    if (ocontrol$EM & dist == "negbin") {
      mui <- model_count$fitted
      probi <- model_zero$fitted
      probi <- probi/(probi + (1 - probi) * dnbinom(0, 
                                                    size = start$theta, mu = mui))
      probi[Y1] <- 0
      ll_new <- loglikfun(c(start$count, start$zero, log(start$theta)))
      ll_old <- 2 * ll_new
      offset <- offsetx
      while (abs((ll_old - ll_new)/ll_old) > control$reltol) {
        ll_old <- ll_new
        model_count <- suppressWarnings(glm.nb(Y ~ 0 + 
                                                 X + offset(offset), weights = weights * (1 - 
                                                                                            probi), start = start$count, init.theta = start$theta))
        model_zero <- suppressWarnings(glm.fit(Z, probi, 
                                               weights = weights, offset = offsetz, family = binomial(link = linkstr), 
                                               start = start$zero))
        start <- list(count = model_count$coefficients, 
                      zero = model_zero$coefficients, theta = model_count$theta)
        mui <- model_count$fitted
        probi <- model_zero$fitted
        probi <- probi/(probi + (1 - probi) * dnbinom(0, 
                                                      size = start$theta, mu = mui))
        probi[Y1] <- 0
        ll_new <- loglikfun(c(start$count, start$zero, 
                              log(start$theta)))
      }
    }
    if (control$trace) 
      cat("done\n")
  }
  if (control$trace) 
    cat("calling optim() for ML estimation:\n")
  fit <- optim(fn = loglikfun, gr = gradfun, par = c(start$count, 
                                                     start$zero, if (dist == "negbin") log(start$theta) else NULL), 
               method = method, hessian = hessian, control = control)
  if (fit$convergence > 0) 
    warning("optimization failed to converge")
  coefc <- fit$par[1:kx]
  names(coefc) <- names(start$count) <- colnames(X)
  coefz <- fit$par[(kx + 1):(kx + kz)]
  names(coefz) <- names(start$zero) <- colnames(Z)
  vc <- tryCatch(-solve(as.matrix(fit$hessian)), error = function(e) {
    warning(e$message, call = FALSE)
    k <- nrow(as.matrix(fit$hessian))
    return(matrix(NA, k, k))
  })
  if (dist == "negbin") {
    np <- kx + kz + 1
    theta <- as.vector(exp(fit$par[np]))
    SE.logtheta <- as.vector(sqrt(diag(vc)[np]))
    vc <- vc[-np, -np, drop = FALSE]
  }
  else {
    theta <- NULL
    SE.logtheta <- NULL
  }
  colnames(vc) <- rownames(vc) <- c(paste("count", colnames(X), 
                                          sep = "_"), paste("zero", colnames(Z), sep = "_"))
  mu <- exp(X %*% coefc + offsetx)[, 1]
  phi <- linkinv(Z %*% coefz + offsetz)[, 1]
  Yhat <- (1 - phi) * mu
  res <- sqrt(weights) * (Y - Yhat)
  nobs <- sum(weights > 0)
  rval <- list(coefficients = list(count = coefc, zero = coefz), 
               residuals = res, fitted.values = Yhat, optim = fit, method = method, 
               control = ocontrol, start = start, weights = if (identical(as.vector(weights), 
                                                                          rep.int(1L, n))) NULL else weights, offset = list(count = if (identical(offsetx, 
                                                                                                                                                  rep.int(0, n))) NULL else offsetx, zero = if (identical(offsetz, 
                                                                                                                                                                                                          rep.int(0, n))) NULL else offsetz), n = nobs, df.null = nobs - 
                 2, df.residual = nobs - (kx + kz + (dist == "negbin")), 
               terms = list(count = mtX, zero = mtZ, full = mt), theta = theta, 
               SE.logtheta = SE.logtheta, loglik = fit$value, vcov = vc, 
               dist = dist, link = linkstr, linkinv = linkinv, converged = fit$convergence < 
                 1, call = cl, formula = ff, levels = .getXlevels(mt, 
                                                                  mf), contrasts = list(count = attr(X, "contrasts"), 
                                                                                        zero = attr(Z, "contrasts")))
  if (model) 
    rval$model <- mf
  if (y) 
    rval$y <- Y
  if (x) 
    rval$x <- list(count = X, zero = Z)
  class(rval) <- "zeroinfl"
  return(rval)
}







