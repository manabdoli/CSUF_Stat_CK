# adding model to graphs
library(ggformula)
library(mosaic)



gfm <- function(object, model,
                           width = 0.5,
                           ...) {
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
  } else{

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
    
  }
  p
}

gf_jitter(Thumb ~ Height, data = Fingers, width = 0.15, alpha = 0.6) |> 
gfm(lm(Thumb~Height, data=Fingers), color='orange')
