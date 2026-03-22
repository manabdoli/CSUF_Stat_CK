# Adding indent to a print out
indent_print <- function(x, spaces = 4) {
  indent <- paste(rep(" ", spaces), collapse = "")
  output <- capture.output(print(x))
  # Add indent to EACH line
  indented <- paste0(indent, output)
  cat(indented, sep = "\n")
}

# Mimicing gf_model of coursekata::
gf_model <- function(object, model, width = 0.5, ...) {
  #browser()
  if (!inherits(object, c("gg", "ggplot"))) {
    rlang::abort("Layer on top of a ggformula/ggplot object.")
  }
  bar_args <- list(...)
  # infer x/y from the plot mapping
  mapping <- object$mapping
  if (is.null(mapping$x) || is.null(mapping$y)) {
    rlang::abort("Plot must map x and y aesthetics (e.g., gf_jitter(y ~ x, ...)).")
  }
  x <- rlang::as_name(mapping$x)
  y <- rlang::as_name(mapping$y)

  dat <- model$model
  cur_names <- names(dat)
  if (!x %in% cur_names){
    if(x %in% c('NULL', '.')) x <- 'Overall'
    dat[x] <- factor(1, levels=1, labels=rlang::as_name(mapping$x)) # keep the original value
    x <- setdiff(names(dat), cur_names)
  }
  if (!x %in% names(dat) || !y %in% names(dat)) {
    rlang::abort("Model variables don't match the plot variables.")
  }
  if (is.numeric(dat[[x]])) {
    rlang::abort("This helper is for categorical/group x only.")
  }

  # one row per group in model order
  levs <- levels(factor(dat[[x]]))
  newdata <- data.frame(setNames(list(factor(levs, levels = levs)), x), check.names = FALSE)

  # predicted group means
  newdata$pred <- as.numeric(stats::predict(model, newdata = newdata))
  newdata$ymin <- newdata$pred
  newdata$ymax <- newdata$pred

  # -------- mean "bars" as errorbars with ymin=ymax ----------
  # Build explicit aesthetics: ymin/ymax ~ x
  f_bar <- stats::as.formula(paste0("pred + ymin + ymax ~ ", x))
  base_bar_call <- list(
    object = object,
    gformula = f_bar,
    data = newdata,
    width = width
    , inherit = FALSE
  )
  bar_call <- utils::modifyList(base_bar_call, bar_args)
  p <- do.call(ggformula::gf_crossbar, bar_call)
  
  p
}

# Enhanced gf_model with added label/text
gf_model_mean <- function(p, model, color ='blue', n = 200, ...) {
  #browser()
  var <- as.character(formula(model))[2]
  if(is.null(var)){
    return(p + geom_vline(xintercept = predict(model)[1], color = color))
  }
  preds <- attr(terms(model), "term.labels")
  if(length(preds)==0){
    return(p + geom_hline(yintercept = predict(model)[1], color = color))
  }
  if(length(preds)==1){
    rng <- range(model$model[[preds]])
    new <- data.frame(x = seq(rng[1], rng[2], length.out = n))
    yhat <- predict(model, newdata = new, ...)
  } else{
    new = data.frame(x =  model$model[[preds]])
    y_hat <- predict(model)
  }
  new = data.frame(yhat = y_hat, x = new)
  colnames(new) <- c(var, preds)
  p + geom_line(data = new, aes_string(x = preds, y = "yhat"), color = color)
}


# A substitue for coursekata::gf_model since it is not ported on webr
gf_model_text <- function(object, model,
                           label = c('text', 'label'),
                           label_digits = 2,
                           label_args = list()) {

  if (!inherits(object, c("gg", "ggplot"))) {
    rlang::abort("Layer on top of a ggformula/ggplot object.")
  }

  # infer x/y from the plot mapping
  mapping <- object$mapping
  if (is.null(mapping$x) || is.null(mapping$y)) {
    rlang::abort("Plot must map x and y aesthetics (e.g., gf_jitter(y ~ x, ...)).")
  }
  x <- rlang::as_name(mapping$x)
  y <- rlang::as_name(mapping$y)

  dat <- model$model
  if (!x %in% names(dat) || !y %in% names(dat)) {
    rlang::abort("Model variables don't match the plot variables.")
  }
  if (is.numeric(dat[[x]])) {
    rlang::abort("This helper is for categorical/group x only.")
  }

    # one row per group in model order
    levs <- levels(factor(dat[[x]]))
    newdata <- data.frame(setNames(list(factor(levs, levels = levs)), x))

    # predicted group means
    newdata$pred <- as.numeric(stats::predict(model, newdata = newdata))
    newdata$lab  <- format(round(newdata$pred, label_digits), nsmall = label_digits)

    nudge_y <- 0
    if (!is.null(label_args$nudge_y)) {
        nudge_y <- label_args$nudge_y
        label_args$nudge_y <- NULL
    }
    newdata$pred_lab_y <- newdata$pred + nudge_y

    f_lab <- stats::as.formula(paste0("pred_lab_y ~ ", x))

    defaults <- list(
        label = ~ lab,
        vjust = -0.6,
        color = "black",
        size  = 3.5,
        inherit = FALSE
    )
    lab_call <- c(
        list(object = object, gformula = f_lab, data = newdata),
        utils::modifyList(defaults, label_args)
    )

    p <- if(label[1]=='label')  do.call(ggformula::gf_label, lab_call) else 
        do.call(ggformula::gf_text, lab_call)

  p
}

b1 <- function(mod, data=NULL){
  if(!is.null(data) && !is.data.frame(data)){
    data <- as.data.frame(data)
    stop('data should be a data.frame!')
  }
  if(is.character(mod)){
    mod <- as.formula(mod)
  }
  if("formula" == class(mod)){
    mod <- lm(mod, data=data)
  }
  mod$coefficients[2]
}

b0 <- function(mod, data=NULL){
  if(!is.null(data) && !is.data.frame(data)){
    data <- as.data.frame(data)
    stop('data should be a data.frame!')
  }
  if(is.character(mod)){
    mod <- as.formula(mod)
  }
  if("formula" == class(mod)){
    mod <- lm(mod, data=data)
  }
  mod$coefficients[1]
}

pre <- function(mod, data=NULL){
  if(!is.null(data) && !is.data.frame(data)){
    data <- as.data.frame(data)
    stop('data should be a data.frame!')
  }
  if(is.character(mod)){
    mod <- as.formula(mod)
  }
  if("formula" == class(mod)){
    mod <- lm(mod, data=data)
  }
  summary(mod)$r.squared
}


# Calude generated
cohensD <- function(x = NULL, y = NULL, data = NULL, method = "pooled", mu = 0, formula = NULL) {
  
  # Handle formula interface
  if (!is.null(formula)) {
    if (is.null(data)) stop("'data' must be provided when using a formula")
    mf <- model.frame(formula, data)
    if (ncol(mf) != 2) stop("Formula must be of the form: numeric ~ factor")
    x <- mf[[1]][mf[[2]] == levels(factor(mf[[2]]))[1]]
    y <- mf[[1]][mf[[2]] == levels(factor(mf[[2]]))[2]]
  }
  
  # Handle lm object passed as x
  if (inherits(x, "lm")) {
    model <- x
    mf <- model.frame(model)
    if (ncol(mf) != 2) stop("lm model must have exactly one binary predictor")
    outcome <- mf[[1]]
    predictor <- factor(mf[[2]])
    if (nlevels(predictor) != 2) stop("Predictor must have exactly 2 levels")
    x <- outcome[predictor == levels(predictor)[1]]
    y <- outcome[predictor == levels(predictor)[2]]
  }
  
  if (is.null(y)) {
    # One-sample case: compare x to mu
    d <- (mean(x, na.rm = TRUE) - mu) / sd(x, na.rm = TRUE)
    return(abs(d))
  }
  
  # Two-sample case
  nx <- length(x)
  ny <- length(y)
  mx <- mean(x, na.rm = TRUE)
  my <- mean(y, na.rm = TRUE)
  sx <- sd(x, na.rm = TRUE)
  sy <- sd(y, na.rm = TRUE)
  
  pooled_sd <- switch(method,
    pooled   = sqrt(((nx - 1) * sx^2 + (ny - 1) * sy^2) / (nx + ny - 2)),
    x        = sx,
    y        = sy,
    raw      = 1,
    paired   = sd(x - y, na.rm = TRUE),
    corrected = sqrt(((nx - 1) * sx^2 + (ny - 1) * sy^2) / (nx + ny - 2)) *
                  sqrt(2 * (nx + ny) / (nx * ny)),  # unbiased correction
    stop("Unknown method: '", method, "'. Choose from: pooled, x, y, raw, paired, corrected")
  )
  
  d <- (mx - my) / pooled_sd
  return(abs(d))
}

sse <- function(mod){
  sum(mod$residuals^2)
}

sst <- function(y){
  sum((y-mean(y))^2)
}

f <- function(mod) {
  anova(mod)[1, "F value"]
}

# 
gf_fun_fill <- function(object=NULL, fun=NULL, ..., from = -4, to = 4, n=100, fill='orange', alpha=.5) {
  if(!is.null(object)) {
    if("function" %in% class(object)){
        if("function" %in% class(fun)){
            stop('Two functions are passed!')
        } else{
            fun <- object
            object <- ggplot2::ggplot()
        }
    } else{
        if(!"ggplot" %in% class(object) && !"gf_ggplot" %in% class(object)) {
            stop('Object is not a ggplot object!')
        } else
            if(is.null(object)) object<- ggplot()
    }
  }

  x <- seq(from, to, length.out = n)
  object<- (object |> gf_area(y ~ x, 
  data=data.frame(x=x, y=fun(x)),
    fill = fill, alpha = alpha)) |>
    gf_function(fun = fun, ...)
  object
}

b1dt <- function(x, df, b1, w=1) {
    b1.n<- length(b1)
    b1.bar<- mean(b1)
    b1.sd<- sd(b1)
    dt((x-b1.bar)/b1.sd, df=df)/b1.sd*b1.n*w
}

#
rskw <- function(n, mu=0, sd=1, skw=2){
    x <- rnorm(n, mu, sd)
    if(length(skw)==1) 
        skw<- c(if(skw<0) c(abs(skw), NA),
                if(skw>0) c(NA, skw))
    if(!is.na(skw[1])){
        idx<- which(x<mu)
        x[idx] <- mu + (x[idx]-mu)*skw[1]
    }
    if(!is.na(skw[2])){
        idx<- which(x>mu)
        x[idx] <- mu + (x[idx]-mu)*skw[2]
    }
    x
}

# 