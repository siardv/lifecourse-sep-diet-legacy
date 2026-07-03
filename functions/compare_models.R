.get_pvalue <- function(x, pattern) {
  x[[grep("^Pr", names(x), value = TRUE)]]
}

.r2_diff <- function(x, y) {
  comp <- stats::anova(x, y)
  df1 <- comp$Df[2]
  df2 <- y$df.residual
  m1 <- summary(x)
  m2 <- summary(y)
  r2.1 <- m1$r.squared
  r2.2 <- m2$r.squared
  r2.2 - r2.1
}

compare_models <- function(...) {
  dotlist <- list(...)[[1]]
  fit1 <- dotlist[[1]]
  fit2 <- dotlist[[2]]
  res <- tryCatch(
    {
      if (class(fit1) == "lmrob") {
        out_1 <- robustbase:::anova.lmrob(fit1, fit2, test = "Wald")
        out_2 <- robustbase:::anova.lmrob(fit1, fit2, test = "Deviance")
        out <- data.frame(
          df1 = as.numeric(out_1$pseudoDf)[[1]],
          df1 = as.numeric(out_1$pseudoDf)[[2]],
          wald_stat = out_1$Test.Stat,
          p_value_wald = out_1$`Pr(>chisq)`,
          deviance_stat = out_2$Test.Stat,
          p_value_deviance = out_2$`Pr(>chisq)`,
          df = out_1$Df
        )
        out <- out[complete.cases(out), ]
        return(out)
      } else {
        out_1 <- lmtest::waldtest(fit1, fit2, test = "Chisq")
        out_2 <- stats::anova(fit1, fit2)
        out <- data.frame(
          df1 = as.numeric(out_1$Res.Df)[[1]],
          df2 = as.numeric(out_1$Res.Df)[[2]],
          wald_stat = out_1$Chisq[2],
          p_value_wald = .get_pvalue(out_1),
          deviance_stat = .r2_diff(fit1, fit2),
          p_value_deviance = .get_pvalue(out_2),
          df = out_1$Df[2]
        )
        out <- out[complete.cases(out), ]
        return(out)
      }
      if (!identical(out_1[[1]], out_2[[1]])) {
        stop("Pseudo degrees of freedom do not match")
      }
      if (!identical(out_1$Df, out_2$Df)) {
        stop("Degrees of freedom do not match")
      }
    },
    error = function(e) {
      data.frame(matrix(ncol = 7, nrow = 1))
    }
  )
  res[!colSums(is.na(res)) == nrow(res)]
}
