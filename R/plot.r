#' Plot model trajectories
#'
#' This function use faceting to plot all trajectories in a data frame.
#' Convenient to see results of several simulations, or data. Also, if
#' \code{data} is present, then an additional plot is displayed with data and
#' potentially observation generated.
#' @param traj data.frame, output of \code{fitmodel$simulate} or
#'   \code{simulateModelReplicates}.
#' @param stateNames character vector. Names of the state variables to plot.
#'   Names must match \code{fitmodel$stateNames}. If \code{NULL} (default) all
#'   state variables are plotted.
#' @param data data frame. Observation times and observed data. The time column
#'   must be named as given by \code{timeColumn}, whereas the name of the data
#'   column should match one of \code{fitmodel$stateNames}.
#' @param timeColumn character vector. The column in the data that indicates
#'   time
#' @param linesData logical. If \code{TRUE}, the data will be plotted as lines
#' @param summary logical. If \code{TRUE}, the mean, median as well as the 50th
#'   and 95th percentile of the trajectories are plotted (default). If
#'   \code{FALSE}, all individual trajectories are plotted (transparency can be
#'   set with \code{alpha}).
#' @param replicateColumn character Vector. The column in the data that
#'   indicates the replicate (if muliple replicates are to be plotted, i.e. if
#'   \code{summary} is \code{FALSE}
#' @param nonExtinct character vector. Names of the infected states which must
#'   be non-zero so the epidemic is still ongoing.  When the names of these
#'   states are provided, the extinction probability is plotted by computing the
#'   proportion of faded-out epidemics over time.  An epidemic has faded-out
#'   when all the infected states (whose names are provided) are equal to 0.
#'   This is only relevant for stochastic models.  In addition, if \code{summary
#'   == TRUE}, the summaries of the trajectories conditioned on non-extinction
#'   are shown. Default to \code{NULL}.
#' @param alpha transparency of the trajectories (between 0 and 1).
#' @param plot if \code{TRUE} the plot is displayed, and returned otherwise.
#' @param colour character vector. If a character, will use that colour to plot
#'   trajectories. If "all", use all available colours. If \code{NULL}, don't
#'   set the colour.
#' @param initDate character. Date of the first point of the time series
#'   (default to \code{NULL}). If provided, the x-axis will be in calendar
#'   format. NB: currently only works if the unit of time is the day.
#' @param same logical (default: FALSE); if TRUE, trajectories will be plotted
#'   in the same panel.
#' @export
#' @importFrom stringr str_split
#' @importFrom ggplot2 ggplot facet_wrap geom_ribbon geom_line aes_string
#'    scale_alpha_manual scale_linetype guide_legend guides theme_bw theme
#'    geom_point
#' @importFrom dplyr mutate filter
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data inherits_any
#' @importFrom stats quantile
#' @seealso \code{\link{simulateModelReplicates}}
plotTraj <- function(traj = NULL, stateNames = NULL, data = NULL,
                     timeColumn = "time", linesData = FALSE, summary = TRUE,
                     replicateColumn = "replicate", nonExtinct = NULL,
                     alpha = 1, plot = TRUE, colour = "red", initDate = NULL,
                     same = FALSE) {
  if (!is.null(initDate)) {
    initDate <- as.Date(initDate)
  }

  if (is.null(traj) && is.null(data)) {
    stop("Nothing to plot")
  }

  if (!is.null(traj) && !any(duplicated(traj[[timeColumn]]))) {
    traj[[replicateColumn]] <- 1

    # Only 1 replicate to summarise: mean, median and CI of
    # the trajectories won't be plotted.
    summary <- FALSE
  }

  if (is.null(stateNames)) {
    numericNames <- names(traj)[sapply(names(traj), function(x) {
      inherits_any(traj[[x]], c("numeric", "integer"))
    })]
    stateNames <- setdiff(numericNames, c(timeColumn, replicateColumn))
  } else if (!is.character(stateNames)) {
    stop(sQuote("stateNames"), ", if given, must be a numeric vector")
  }

  if (!is.null(initDate)) {
    traj[[timeColumn]] <- traj[[timeColumn]] + initDate
    if (!is.null(data)) {
      data[[timeColumn]] <- data[[timeColumn]] + initDate
    }
  }

  if (colour == "all" && summary == TRUE) {
    warning(
      "Ignoring ", sQuote("colour = \"all\""), " which doesn't make sense if ",
      sQuote("summary == TRUE")
    )
    colour <- NULL
  }

  if (!is.null(traj)) {
    if (!is.null(nonExtinct)) {
      traj <- mutate(
        traj,
        infected = eval(parse(text = paste(nonExtinct, collapse = "+")), traj)
      )
      dfPExt <- split(traj, f = traj[[timeColumn]])
      dfPExt <- future_map(dfPExt, \(df) {
        tmp <- data.frame(value = sum(df$infected == 0) / nrow(df))
        tmp[[timeColumn]] <- unique(df[[timeColumn]])
        return(tmp)
      })
      dfPExt <- bind_rows(dfPExt)
      dfPExt$state <- "pExtinction"
      dfPExt[replicateColumn] <- 0

      if (summary) {
        traj <- filter(traj, .data$infected > 0)
        traj$infected <- NULL
      }
    }

    dfTraj <- pivot_longer(
      traj, all_of(stateNames), names_to = "state"
    )
    dfTraj <- filter(dfTraj, !is.na(.data$value))

    if (summary) {
      message("Compute confidence intervals")

      trajCI <- split(dfTraj, dfTraj[c(timeColumn, "state")])
      trajCI <- future_map(trajCI, \(df) {
        tmp <- as.data.frame(
          t(quantile(df$value, prob = c(0.025, 0.25, 0.5, 0.75, 0.975)))
        )
        names(tmp) <- c("low_95", "low_50", "median", "up_50", "up_95")
        tmp$mean <- mean(df$value)
        tmp[[timeColumn]] <- unique(df[[timeColumn]])
        tmp[["state"]] <- unique(df[["state"]])
        return(tmp)
      }, .progress = TRUE)
      trajCI <- bind_rows(trajCI)

      trajCILine <- pivot_longer(
        trajCI[c(timeColumn, "state", "mean", "median")],
        c(-timeColumn, -"state"),
        names_to = "variable"
      )
      trajCIArea <- pivot_longer(
        trajCI[c(timeColumn, "state", "low_95", "low_50", "up_50", "up_95")],
        c(-timeColumn, -"state"),
        names_to = "variable"
      )
      trajCIArea$type <- sapply(trajCIArea$variable, function(x) {
        str_split(x, "_")[[1]][1]
      })
      trajCIArea$CI <- sapply(trajCIArea$variable, function(x) {
        str_split(x, "_")[[1]][2]
      })
      trajCIArea$variable <- NULL
      trajCIArea <- tidyr::pivot_wider(trajCIArea, names_from = "type")

      p <- ggplot(trajCIArea)
      if (!same) {
        p <- p + facet_wrap(~state, scales = "free_y")
      }

      if (is.null(colour)) {
        p <- p + geom_ribbon(
          data = trajCIArea,
          aes_string(x = timeColumn, ymin = "low", ymax = "up", alpha = "CI")
        )
        p <- p + geom_line(
          data = trajCILine,
          aes_string(x = timeColumn, y = "value", linetype = "variable")
        )
      } else if (colour == "all") {
        p <- p + geom_ribbon(
          data = trajCIArea,
          aes_string(
            x = timeColumn, ymin = "low", ymax = "up", alpha = "CI",
            fill = "state"
          )
        )
        p <- p + geom_line(
          data = trajCILine,
          aes_string(
            x = timeColumn, y = "value", linetype = "variable",
            colour = "state"
          )
        )
      } else {
        p <- p + geom_ribbon(
          data = trajCIArea,
          aes_string(x = timeColumn, ymin = "low", ymax = "up", alpha = "CI"),
          fill = colour
        )
        p <- p + geom_line(
          data = trajCILine,
          aes_string(x = timeColumn, y = "value", linetype = "variable"),
          colour = colour
        )
      }
      p <- p + scale_alpha_manual(
        "Percentile", values = c("95" = 0.25, "50" = 0.45),
        labels = c("95" = "95th", "50" = "50th")
      )
      p <- p + scale_linetype("Stats")
      p <- p + guides(linetype = guide_legend(order = 1))
    } else {
      p <- ggplot(dfTraj)
      if (!same) {
        p <- p + facet_wrap(~state, scales = "free_y")
      }

      if (is.null(colour)) {
        if (same) {
          p <- p + geom_line(
            data = dfTraj,
            aes_string(
              x = timeColumn, y = "value", group = "state", color = "state"
            ),
            alpha = alpha
          )
        } else {
          p <- p + geom_line(
            data = dfTraj,
            aes_string(x = timeColumn, y = "value", group = replicateColumn),
            alpha = alpha
          )
        }
      } else if (colour == "all") {
        p <- p + geom_line(
          data = dfTraj,
          aes_string(
            x = timeColumn, y = "value", group = replicateColumn,
            color = replicateColumn
          ),
          alpha = alpha
        )
      } else {
        p <- p + geom_line(
          data = dfTraj,
          aes_string(x = timeColumn, y = "value", group = replicateColumn),
          alpha = alpha, colour = colour
        )
      }
    }

    if (!is.null(nonExtinct)) {
      p <- p + geom_line(
        data = dfPExt, aes_string(x = timeColumn, y = "value"),
        color = "black", alpha = 1
      )
    }
  } else {
    p <- ggplot()
  }

  if (!is.null(data)) {
    obsNames <- grep("obs", names(data), value = TRUE)
    if (length(obsNames) == 0) {
      obsNames <- setdiff(names(data), timeColumn)
    }
    data <- pivot_longer(data, all_of(obsNames), names_to = "state")
    if (linesData) {
      p <- p + geom_line(
        data = data, aes_string(x = timeColumn, y = "value"), colour = "black"
      )
    } else {
      p <- p + geom_point(
        data = data, aes_string(x = timeColumn, y = "value"), colour = "black"
      )
    }
  }

  p <- p + theme_bw() +
    theme(legend.position = "top", legend.box = "horizontal")

  if (plot) {
    print(p)
  } else {
    return(p)
  }
}

#' Plot fit of model to data
#'
#' This function simulates the model under \code{theta}, generates observation
#' and plot them against the data. Since simulation and observation processes
#' can be stochastic, \code{nReplicates} can be plotted.
#' @param nReplicates numeric, number of replicated simulations.
#' @param allVars logical, if \code{FALSE} only the observations are plotted.
#'   Otherwise, all state variables are plotted.
#' @inheritParams testFitmodel
#' @inheritParams plotTraj
#' @inheritParams simulateModelReplicates
#' @export
#' @return if \code{plot == FALSE}, a list of 2 elements is returned:
#' \itemize{
#'     \item \code{simulations} \code{data.frame} of \code{nReplicates}
#'   simulated observations.
#'     \item \code{plot} the plot of the fit.
#' }
plotFit <- function(fitmodel, theta, initState, data, nReplicates = 1,
                    summary = TRUE, alpha = min(1, 10 / nReplicates),
                    allVars = FALSE, nonExtinct = NULL, observation = TRUE,
                    plot = TRUE) {
  times <- c(0, data$time)

  if (nReplicates > 1) {
    cat("Simulate ", nReplicates, " replicate(s)\n")
  }
  traj <- simulateModelReplicates(
    fitmodel = fitmodel, theta = theta, initState = initState, times = times,
    n = nReplicates, observation = observation
  )

  if (allVars) {
    stateNames <- NULL
  } else {
    stateNames <- grep("obs", names(traj), value = TRUE)
  }

  p <- plotTraj(
    traj = traj, stateNames = stateNames, data = data, summary = summary,
    alpha = alpha, nonExtinct = nonExtinct, plot = FALSE
  )

  if (plot) {
    print(p)
  } else {
    return(list(traj = traj, plot = p))
  }
}


#' Plot result of SMC
#'
#' Plot the observation generated by the filtered trajectories together with the
#' data.
#' @param smc output of \code{\link{particleFilter}}
#' @inheritParams plotTraj
#' @inheritParams plotFit
#' @importFrom furrr future_map
#' @importFrom dplyr bind_rows left_join
#' @export
#' @seealso particleFilter
plotSMC <- function(smc, fitmodel, theta, data = NULL, summary = TRUE,
                    alpha = 1, allVars = FALSE, plot = TRUE) {
  traj <- smc$traj
  names(traj) <- seq_along(traj)

  traj <- future_map(traj, function(df) {
    obs <- apply(df, 1, \(x) fitmodel$rPointObs(x, theta = theta))
    trajObs <- left_join(df, obs, by = "time")

    return(trajObs)
  })
  traj <- bind_rows(traj, .id = "replicate")

  if (allVars) {
    stateNames <- NULL
  } else {
    stateNames <- grep("obs", names(traj), value = TRUE)
  }

  p <- plotTraj(
    traj = traj, stateNames = stateNames, data = data, summary = summary,
    alpha = alpha, plot = FALSE
  )


  if (plot) {
    print(p)
  } else {
    return(p)
  }
}


#' Plot MCMC trace
#'
#' Plot the traces of all estimated variables.
#' @param trace a \code{data.frame} with one column per estimated parameter, as
#'   returned by \code{\link{burnAndThin}}
#' @param estimatedOnly logical, if \code{TRUE} only estimated parameters are
#'   displayed.
#' @export
#' @importFrom ggplot2 ggplot aes facet_wrap geom_line
#' @seealso burnAndThin
#' @examples
#' data(mcmc.epi)
#' plotTrace(mcmc.epi1$trace)
plotTrace <- function(trace, estimatedOnly = FALSE) {
  if (estimatedOnly) {
    isFixed <- apply(trace, 2, function(x) {
      length(unique(x)) == 1
    })
    trace <- trace[, -which(isFixed)]
  }

  trace$iteration <- seq_len(nrow(trace))
  df <- pivot_longer(trace, -"iteration")

  # density
  p <- ggplot(df, aes(x = .data$iteration, y = .data$value)) +
    facet_wrap(~name, scales = "free")
  p <- p + geom_line(alpha = 0.75)
  print(p)
}

#' Plot MCMC posterior densities
#'
#' Plot the posterior density.
#' @param trace either a \code{data.frame} or a \code{list} of \code{data.frame}
#'   with all variables in column, as outputed by \code{\link{mcmcMH}}. Accept
#'   also an \code{mcmc}, a \code{mcmc.list} object or a \code{list} of
#'   \code{mcmc.list} .
#' @param prior a \code{data.frame} containing the prior density. It must have
#'   the three following columns:
#' \itemize{
#'     \item \code{theta} names of the parameters
#'     \item \code{x} value of the parameters
#'     \item \code{density} density of the prior at \code{x}
#' }
#' @param colour named vector of two characters and containing colour names for
#'   posterior and prior distributions. Vector names must be \code{posterior}
#'   and \code{prior}.
#' @inheritParams plotTraj
#' @export
#' @importFrom rlang inherits_any
#' @importFrom furrr future_map
#' @importFrom dplyr n_distinct bind_rows
#' @importFrom ggplot2 ggplot aes facet_wrap geom_density geom_histogram
#'   geom_area theme_bw xlab after_stat
#' @seealso burnAndThin
#' @examples
#' data(mcmc.epi)
#' plotPosteriorDensity(mcmc.epi1$trace)
plotPosteriorDensity <- function(trace, prior = NULL, colour = NULL,
                                 plot = TRUE) {
  if (is.null(colour)) {
    colour <- c(posterior = "#7570b3", prior = "#d95f02")
  }

  if (inherits_any(trace, c("mcmc.list", "list"))) {
    if (all(sapply(trace, function(x) {
      inherits(x, "mcmc.list")
    }))) {
      trace <- future_map(trace, \(x) {
        names(x) <- NULL
        bind_rows(x)
      })
    }

    if (is.null(names(trace))) {
      names(trace) <- seq_along(trace)
    }
    trace <- bind_rows(trace, .id = "chain")
  } else {
    trace$chain <- 1
  }

  dfPosterior <- pivot_longer(
    trace, -.data$chain, values_to = "x", names_to = "theta"
  )

  p <- ggplot(dfPosterior, aes(x = .data$x)) +
    facet_wrap(~theta, scales = "free")

  if (n_distinct(dfPosterior$chain) > 1) {
    p <- p + geom_density(
      data = dfPosterior,
      aes(y = after_stat(.data$density), colour = .data$chain)
    )
  } else {
    p <- p + geom_histogram(
      data = dfPosterior, mapping = aes(y = after_stat(.data$density)),
      fill = colour[["posterior"]], colour = colour[["posterior"]], alpha = 0.5
    )
  }

  if (!is.null(prior)) {
    p <- p + geom_area(
      data = prior, aes(x = .data$x, y = .data$density),
      fill = colour[["prior"]], alpha = 0.5
    )
  }

  p <- p + theme_bw() + xlab("value")

  if (plot) {
    print(p)
  } else {
    return(p)
  }
}


#' 2D highest posterior density region
#'
#' Given a sample from a multivariate posterior distribution, plot the bivariate
#' region of highest marginal posterior density (HPD) for two variables with
#' defined levels.
#' @param trace either a \code{data.frame} or \code{mcmc} object.
#' @inheritParams plotPosteriorDensity
#' @inheritParams emdbook::HPDregionplot
#' @note HPD levels are computed using the function
#'   \code{\link[emdbook]{HPDregionplot}} from the package \code{emdbook}.
#' @export
#' @importFrom ggplot2 ggplot stat_density2d aes_string scale_alpha xlab ylab
#'   theme_bw guides guide_legend
#' @importFrom emdbook HPDregionplot
plotHPDregion2D <- function(trace, vars, prob = c(0.95, 0.75, 0.5, 0.25, 0.1),
                            xlab = NULL, ylab = NULL, plot = TRUE) {
  if (length(vars) != 2) {
    stop(sQuote("vars"), " is not a vector of length 2", call. = FALSE)
  }

  listHPD <- HPDregionplot(trace, vars = vars, prob = prob, n = 100)
  levelsHPD <- unique(sapply(listHPD, function(x) {
    x$level
  }))
  names(levelsHPD) <- paste0(prob * 100, "%")

  p <- ggplot(trace, aes_string(x = vars[1], y = vars[2]))
  p <- p + stat_density2d(
    aes(alpha = .data$..level..), fill = "red", colour = "black",
    geom = "polygon", breaks = levelsHPD
  )
  p <- p + scale_alpha(
    "HPD", breaks = levelsHPD, guide = "legend", range = c(0.1, 0.45)
  )
  if (!is.null(xlab)) {
    p <- p + xlab(xlab)
  }
  if (!is.null(ylab)) {
    p <- p + ylab(ylab)
  }

  p <- p + theme_bw() +
    guides(
      alpha = guide_legend(
        override.aes = list(
          colour = NA, alpha = seq(0.1, 0.9, length = length(levelsHPD))
        )
      )
    )

  if (plot) {
    print(p)
  }

  invisible(p)
}


#' Plot MCMC posterior fit
#'
#' Plot posterior distribution of observation generated under model's posterior
#' parameter distribution.
#' @param posteriorSummary character. Set to \code{"sample"} to plot
#'   trajectories from a sample of the posterior (default). Set to
#'   \code{"median"}, \code{"mean"} or \code{"max"} to plot trajectories
#'   corresponding to the median, mean and maximum of the posterior density.
#' @param summary logical, if \code{TRUE} trajectories are summarised by their
#'   mean, median, 50\% and 95\% quantile distributions. Otheriwse, the
#'   trajectories are ploted.
#' @param sampleSize number of theta sampled from posterior distribution (if
#'   \code{posterior.summary == "sample"}). Otherwise, number of replicated
#'   simulations.
#' @inheritParams testFitmodel
#' @inheritParams plotTrace
#' @inheritParams plotTraj
#' @inheritParams plotFit
#' @importFrom dplyr bind_rows
#' @importFrom furrr future_map furrr_options
#' @importFrom stats median
#' @importFrom rlang .data
#' @export
#' @return If \code{plot == FALSE}, a list of 2 elements is returned:
#' \itemize{
#'    \item \code{theta} the \code{theta}(s) used for plotting (either a
#'   \code{vector} or a \code{data.frame})
#'    \item \code{traj} a \code{data.frame} with the trajectories (and
#'   observations) sampled from the posterior distribution.
#'    \item \code{plot} the plot of the fit displayed.
#' }
#' @examples
#' data(FluTdC1971)
#' data(epi)
#' data(mcmc.epi)
#' data(models)
#' initState <- c(S = 999, I = 1, R = 0)
#' plotPosteriorFit(
#'   trace = mcmc.epi1$trace, fitmodel = SIR_deter, initState = initState,
#'   data = epi1
#'  )
plotPosteriorFit <- function(trace, fitmodel, initState, data,
                             posteriorSummary =
                               c("sample", "median", "mean", "max"),
                             summary = TRUE, sampleSize = 100,
                             nonExtinct = NULL,
                             alpha = min(1, 10 / sampleSize), plot = TRUE,
                             allVars = FALSE, initDate = NULL) {
  posteriorSummary <- match.arg(posteriorSummary)

  if (inherits(trace, "mcmc")) {
    trace <- as.data.frame(trace)
  } else if (inherits(trace, "mcmc.list")) {
    trace <- bind_rows(trace)
  }

  # names of estimated theta
  thetaNames <- fitmodel$thetaNames

  # time sequence (must include initial time)
  times <- c(0, data$time)

  message("Compute posterior fit")

  if (posteriorSummary == "median") {
    theta <- apply(trace[thetaNames], 2, median)
    traj <- simulateModelReplicates(
      fitmodel = fitmodel, initState = initState, theta = theta,
      times = times, n = sampleSize, observation = TRUE
    )
  } else if (posteriorSummary == "mean") {
    theta <- apply(trace[thetaNames], 2, mean)
    traj <- simulateModelReplicates(
      fitmodel = fitmodel, initState = initState, theta = theta,
      times = times, n = sampleSize, observation = TRUE
    )
  } else if (posteriorSummary == "max") {
    ind <- which.max(trace$logPosterior)
    theta <- trace[ind, thetaNames]
    traj <- simulateModelReplicates(
      fitmodel = fitmodel, initState = initState, theta = theta,
      times = times, n = sampleSize, observation = TRUE
    )
  } else {
    sampleSize <- min(c(sampleSize, nrow(trace)))

    index <- sample(seq_len(nrow(trace)), sampleSize, replace = TRUE)

    traj <- future_map(index, function(ind) {
      # extract posterior parameter set
      theta <- trace[ind, thetaNames]

      # simulate model at successive observation times of data
      traj <- rTrajObs(fitmodel, theta, initState, times)
      traj$replicate <- ind

      return(traj)
    }, .progress = TRUE, .options = furrr_options(seed = TRUE))
    names(traj) <- index
    traj <- bind_rows(traj)

    theta <- trace[index, thetaNames]
  }


  if (allVars) {
    stateNames <- NULL
  } else {
    stateNames <- grep("obs", names(traj), value = TRUE)
  }

  traj <- subset(traj, traj$time > 0)

  p <- plotTraj(
    traj = traj, stateNames = stateNames, data = data, summary = summary,
    alpha = alpha, nonExtinct = nonExtinct, plot = FALSE,
    initDate = initDate
  )

  if (plot) {
    print(p)
  } else {
    return(list(theta = theta, traj = traj, plot = p))
  }
}

##' Plot Effective Sample Size (ESS) against burn-in
##'
##' Takes an mcmc trace and tests the ESS at different values of burn-in
##' @param trace either a \code{data.frame} or a \code{list} of
##'   \code{data.frame} with all variables in column, as outputed by
##'   \code{\link{mcmcMH}}. Accept also \code{mcmc} or \code{mcmc.list} objects.
##' @param longestBurnIn The longest burn-in to test. Defaults to half the
##'   length of the trace
##' @param stepSize The size of the steps of burn-in to test. Defaults to
##'   1/50th of \code{longest.burn.in}
##' @return a plot of the ESS against burn.in
##' @export
##' @importFrom coda is.mcmc
##' @importFrom dplyr bind_rows
##' @importFrom furrr future_map
##' @importFrom tidyr pivot_longer
##' @importFrom coda effectiveSize as.mcmc
##' @importFrom ggplot2 ggplot facet_wrap geom_line aes theme_bw
##' @examples
##' data(mcmc.epi)
##' plotESSBurn(mcmc.epi1$trace)
plotESSBurn <- function(trace, longestBurnIn = ifelse(
  is.data.frame(trace) | is.mcmc(trace), nrow(trace), nrow(trace[[1]])
) / 2, stepSize = round(longestBurnIn / 50)) {
  testBurnIn <- seq(0, longestBurnIn, stepSize) # test values

  if (!inherits_any(trace, c("mcmc.list", "list"))) {
    trace <- list("chain1" = trace)
    noColour <- TRUE
  } else {
    noColour <- FALSE
  }

  if (is.null(names(trace))) {
    names(trace) <- seq_along(trace)
  }

  dfESSBurnIn <- future_map(trace, function(oneTrace) {
    # initialise data.frame of ess estimates
    essBurnIn <- data.frame(t(effectiveSize(oneTrace)))
    for (burnIn in testBurnIn[-1]) {
      # loop over all test values after 0
      # test burn-in
      testTrace <- burnAndThin(oneTrace, burn = burnIn)
      # estimate ESS and at to vector of ess estimates
      essBurnIn <- rbind(essBurnIn, t(effectiveSize(as.mcmc(testTrace))))
    }
    essBurnIn$burnIn <- testBurnIn

    return(essBurnIn)
  })
  dfESSBurnIn <- bind_rows(dfESSBurnIn, .id = "chain")

  essLong <- pivot_longer(
    dfESSBurnIn, c(-.data$chain, -.data$burnIn), values_to = "ESS",
    names_to = "parameter"
  )

  p <- ggplot(essLong, aes(x = .data$burnIn, y = .data$ESS))
  p <- p + facet_wrap(~parameter)
  if (noColour) {
    p <- p + geom_line()
  } else {
    p <- p + geom_line(aes(color = .data$chain))
  }
  p <- p + theme_bw()

  print(p)
}
