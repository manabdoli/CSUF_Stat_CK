# Adding indent to a print out
indent_print <- function(x, spaces = 4) {
  indent <- paste(rep(" ", spaces), collapse = "")
  output <- capture.output(print(x))
  # Add indent to EACH line
  indented <- paste0(indent, output)
  cat(indented, sep = "\n")
}

# Mimicing gf_model of coursekata::
gf_model <- function(p, model, color ='blue', n = 200, ...) {
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
