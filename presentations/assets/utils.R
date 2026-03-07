# Adding indent to a print out
indent_print <- function(x, spaces = 4) {
  indent <- paste(rep(" ", spaces), collapse = "")
  output <- capture.output(print(x))
  # Add indent to EACH line
  indented <- paste0(indent, output)
  cat(indented, sep = "\n")
}

# Mimicing gf_model of coursekata::
gf_model <- function(object, model,
                           width = 0.5,
                           bar_args = list()) {

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
  newdata$ymin <- newdata$pred
  newdata$ymax <- newdata$pred
  newdata$lab  <- format(round(newdata$pred, label_digits), nsmall = label_digits)

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
        list(object = p, gformula = f_lab, data = newdata),
        utils::modifyList(defaults, label_args)
    )

    p <- if(label=='label')  do.call(ggformula::gf_label, lab_call) else 
        do.call(ggformula::gf_text, lab_call)


  p
}
