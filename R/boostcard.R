getVarNames <- function(mod, n.trees) {
  # return variable id for each tree
  v <- sapply(mod$trees[n.trees], function(x) x[[1]][1])
  return(mod$var.names[v + 1])
}

# make a data.frame of the variable type info
getGBMVarData <- function(mod, n.trees) {
  # bundle the name, storage, measure, and position
  vdata <- data.frame(
    row.names = mod$var.names,
    storage   = vapply(mod$var.levels, function(x) substr(typeof(x), 1, 1), 'x'),
    measure   = ifelse(mod$var.type == 0, 'c', 'd'),
    position  = 1:length(mod$var.names),
    stringsAsFactors = FALSE)

  # based on these we can define the type as used by GBM
  vdata$type <- with(vdata, ifelse(storage == 'd' & measure == 'c', 'numeric',
                            ifelse(measure == 'c', 'ordered',
                                   'factor')))

  # filter out variables not in the final model:
  vdata[unique(getVarNames(mod, n.trees)), ]
}

### let's put this all into one mutha trucka! ###

# return list of data.frames containing split info for each variable
getGBMClassing <- function(mod, n.trees, newdata = NULL, precision,
                           corr){
  vdata <- getGBMVarData(mod, n.trees)

  # extract the split information for each tree
  sdata <- data.frame(t(sapply(mod$trees[n.trees], '[[', 2)))
  colnames(sdata) <- c('value', 'left', 'right', 'missing')

  # group the splits by variable
  sdata <- split(sdata, getVarNames(mod, n.trees))
  sdata <- lapply(sdata, function(x) x[order(x[,'value']), ])

  ### loop over the sdata types and convert to boostbin object ###
  classing <- list()
  for (nm in names(sdata)) {
    v <- vdata[nm,]
    s <- sdata[[nm]]

    if (v$type %in% c('factor', 'ordered')) {
      classed <- process.factor(s, v, mod)
    } else {
      classed <- process.numeric(s, v, mod, newdata, precision)
    }

    # calculate points lost based on 'corr'
    classed$pts.lost <- if (corr == -1) {
      min(classed$score) - classed$score
    } else {
      max(classed$score) - classed$score
    }

    classing[[nm]] <- classed
  }

  return(classing)
}

process.factor <- function(s, v, mod) {
  value <- mod$var.levels[[v$position]]

  if (v$type == 'factor') {
    # map -1/1 to GBM tree left/right column indices
    uniq <- unique(lapply(mod$c.splits, function(x) ifelse(x == -1, 2, 3)))

    # index the unique splits by the GBM value
    vals <- match(mod$c.splits[s$value + 1], unique(mod$c.splits))
  } else {
    uids <- seq_along(value)
    uniq <- lapply(unique(s$value), function(x) ifelse(uids < x + 1, 2, 3))
    vals <- match(s$value, unique(s$value))
  }

  # index the variable data.frame columns to pull in the score splits
  res  <- sapply(1:nrow(s), function(n) as.numeric(s[n, uniq[vals][[n]]]))

  # for each factor level, sum all scores to get the final summation
  scores <- apply(res, 1, sum)

  # output a list containing scores for each level of factor
  out <- structure(list(
    name  = row.names(v),
    type  = 'factor',
    missing = sum(s$missing),
    value = value,
    score = c(scores),
    pts.lost = min(scores) - scores),
    class = 'classed')

  return(out)
}

# process.ordered <- function(s, v, mod) {
#   # same result as process numeric with different levels
#   out <- process.numeric(s, v, mod, precision = NULL)
#
#   lvls <- mod$var.levels[[v$position]]
#   uids <- seq_along(lvls)
#   uniq <- lapply(unique(s$value), function(x) ifelse(uids < x + 1, 2, 3))
#
#   vals <- match(s$value, unique(s$value))
#
#   res  <- sapply(1:nrow(s), function(n) as.numeric(s[n, uniq[vals][[n]]]))
#
#   scores <- apply(res, 1, sum)
#
#   out$type  <- 'ordered'
#   return(out)
# }

process.numeric <- function(s, v, mod, newdata=NULL, precision) {

  if (!is.null(newdata)) {
    ux <- sort(unique(newdata[,row.names(v)]))
    s$value <- ux[findInterval(s$value, ux, rightmost.closed = T) + 1]
  }

  # aggregate the scores by the new values
  split <- aggregate(. ~ value, data.frame(s[1:4]), sum)

  # calc decum and cum sums for left and right splits - gonna add them
  split$left <- rev(cumsum(rev(split$left)))
  split$right <- cumsum(split$right)

  first <- split$left[1]
  last  <- rev(split$right)[1]

  # need to guard against binary splits
  if(length(split$value) == 1) {
    score <- c(first, last)
  } else {
    score <- c(split$left, 0) + c(0, split$right)
  }

  value <- c(split$value, Inf)

  # round numeric weights to the nearest <precision>
  if (!is.null(precision)) {
    rnd <- round(score / precision) * precision
    score <- tapply(score, rnd, FUN = mean)
    value <- tapply(value, rnd, max)
  }

  out <- structure(list(
      name  = row.names(v),
      type  = 'numeric',
      missing = sum(split$missing),
      value = value[order(value)],
      score = score[order(value)],
      pts.lost = min(score) - score),
      class = 'classed')

  return(out)
}

#' Create a scorecard object from a GBM model
#'
#' Given a GBM object with \code{n.minobsinnode == 1}, \code{gbm.to.scorecard}
#' aggregates the model stumps and generates a traditional main-effects
#' scorecard.
#'
#' The scorecard object supports generic predict and print methods.
#'
#' @param mod A GBM model object
#' @param n.trees A single number representing 1:n trees or a vector of trees
#' for which to extract scorecard logic
#' @param rnd an optional parameter used to round numeric split values. Can be
#' used to reduce the size of continuous scorecard predictors.
#'
#' @return A scorecard object
#' @export gbm.to.scorecard
gbm.to.scorecard <- function(mod, n.trees, newdata = NULL, precision = 0.25,
                             corr = -1) {
  # check interaction depth of GBM mod
  if (mod$interaction.depth > 1) {
    stop(sprintf("GBM interaction depth cannot exceed 1: %i",
                 mod$interaction.depth))
  }

  # check that corr field is in {-1, 1}
  if (!(corr %in% c(-1, 1))) {
    stop("corr must be -1 or 1")
  }

  # allow for single number or vector of trees
  if (length(n.trees) == 1) n.trees <- 1:n.trees

  classing <- c(initF=mod$initF, getGBMClassing(mod, n.trees, newdata,
                                                precision, corr))

  structure(classing, class="scorecard")
}

score.scorecard <- function(s, data) {
  var <- data[, s$name]

  out <- numeric(nrow(data))
  out[is.na(var)] <- s$missing

  if (s$type == 'factor') {
    for(i in seq_along(s$value)){
      out[var == s$value[i]] <- s$score[i]
    }
    return(out)
  } else
    if (s$type == 'ordered') {
      var <- as.integer(ordered(var, levels = s$levels)) - 1
      for(i in seq_along(s$value)) {
        if(i == 1) {
          out[var < s$value[i]] <- s$score[i]
        }
        out[var >= s$value[i]] <- s$score[i + 1]
      }
      return(out)
    }
  if (s$type == 'numeric') {
    for(i in seq_along(s$value)) {
      if(i == 1) {
        out[var < s$value[i]] <- s$score[i]
      }
      out[var >= s$value[i]] <- s$score[i + 1]
    }
    return(out)
  }
}

#' generic predict method for scorecard objects
#'
#' Generates a prediction based on a scorecard object. Equivalent to the using
#' \code{predict.gbm} on a stump model
#'
#' \code{predict.scorecard} uses the column names of \code{newdata} to generate
#' the scorecard predictions for each variable.
#'
#' @param object A scorecard object
#' @param newdata A data.frame containing the columns used to generate the
#' scorecard
#' @param ...  Arguments passed to \code{predict.default}
#'
#' @return a numeric vector of length \code{nrow(newdata))}
#' @export predict.scorecard
predict.scorecard <- function(object, newdata, ...) {
  # check that newdata has names attribute
  if (is.null(attr(newdata, "names"))) {
    stop("Supplied data.frame does not have column names")
  }

  # check that the names in the scorecard are found in newdata
  not.found <- !(names(object[-1]) %in% names(newdata))
  if (any(not.found)) {
    vars <- paste(names(object[-1])[not.found], collapse=', ')
    err <- sprintf("Vars not found in dataset: %s", vars)
    stop(err)
  }

  scored <- vapply(object[-1], score.scorecard, numeric(nrow(newdata)), newdata)
  scored <- apply(scored, 1, sum) + object$initF
  return(scored)
}
