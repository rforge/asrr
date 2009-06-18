`tsls2` <-
function (formula, instruments, data, subset, na.action, contrasts = NULL, 
    ...) 
{
  require(sem)
    if (missing(na.action)) 
        na.action <- options()$na.action
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, sys.frame(sys.parent())))) 
        m$data <- as.data.frame(data)
    response.name <- deparse(formula[[2]])
    form <- as.formula(paste(paste(response.name, collapse = ""), 
        "~", paste(deparse(formula[[3]]), collapse = ""), "+", 
        paste(deparse(instruments[[2]]), collapse = "")))
    m$formula <- form
    m$instruments <- m$contrasts <- NULL
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, sys.frame(sys.parent()))
    na.act <- attr(mf, "na.action")
    Z <- model.matrix(instruments, data = mf, contrasts)
    y <- mf[, response.name]
    X <- model.matrix(formula, data = mf, contrasts)
    result <- tsls(y, X, Z, colnames(X))
    result$response.name <- response.name
    result$formula <- formula
    result$Z <- result$instruments
    result$instruments <- instruments
    if (!is.null(na.act)) 
        result$na.action <- na.act
    class(result) <- c("tsls2", "tsls")
    result
}
