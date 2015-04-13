print2.scorecard <- function(x, ...) {
  # check if ordered factor is provided. Not supported yet
  vtypes <- vapply(x[-1], function(x) x[['type']], character(1))
  if (any(vtypes == "ordered")) {
    warning("Printing ordered factors not yet supported")
  }

  # print the intercept
  cat(sprintf("final_score_0 = %.10f;\n", x$initF))

  # loop over values and print each
  for (k in seq_along(x[-1])) {
    var <- x[-1][[k]]

    cat(sprintf("\n\n/* variable: %s */\n", var$var))
    cat(sprintf("if missing(%s)\n\tthen final_score%i = %-.10f;",
                var$var, k, var$missing))

    if (var$type == 'numeric') {
      for (i in seq_along(var$value)) {

        lhs <- sprintf("else if %s < %s", var$var, var$value[i])
        rhs <- sprintf("\n\t then final_score%i = ", k)
        scr <- sprintf("%-.10f;", var$score[i])
        cat("\n", lhs, rhs, scr, sep = "")
      }
      rhs <- sprintf("\nelse final_score%i = ", k)
      scr <- sprintf("%-.10f;", var$score[i+1])
      cat(rhs, scr, sep = "")

    } else
      if (var$type == 'factor') {
        for (i in seq_along(var$value)) {

          lhs <- sprintf("if %s = %s", var$var, var$value[i])
          rhs <- sprintf("\n\t then final_score%i = ", k)
          scr <- sprintf("%-.10f;", var$score[i])
          cat("\n", lhs, rhs, scr, sep="")
        }
      } else
        if (var$type == 'ordered') {
          lhs <- sprintf("if %s = %s", var$var, var$value[i])
          rhs <- sprintf("\n\t then final_score%i = ", k)
          scr <- sprintf("%-.10f;", var$score[i])
          cat("\n", lhs, rhs, scr, sep="")
        }
  }
}
