getMercoef <- function(x){
  coef <- lme4::fixef(x)
  stderr <- sqrt(diag(vcov(x)))
  data.frame(coef=coef,stderr=stderr)
}
