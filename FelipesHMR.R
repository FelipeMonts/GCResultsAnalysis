function (filename, series = NA, dec = ".", sep = ";", SatPct = NA, 
          SatTimeMin = NA, pfvar = NA, pfalpha = 0.05, LR.always = FALSE, 
          FollowHMR = FALSE, IfNoValidHMR = "No flux", IfNoFlux = "No flux", 
          IfNoSignal = "No flux", Display.Message = TRUE) 
{
  HMRmessage <- function() {
    cat("", fill = TRUE)
    cat("***************************************************************************", 
        fill = TRUE)
    cat("*****       HMR version 1.0.1 comes with important new features       *****", 
        fill = TRUE)
    cat("***************************************************************************", 
        fill = TRUE)
    cat("", fill = TRUE)
    cat("Flux limiting", fill = TRUE)
    cat("-------------", fill = TRUE)
    cat("The general family of non-linear HMR models is clearly too large as any", 
        fill = TRUE)
    cat("kappa>0 is permissible, although large values correspond to huge and", 
        fill = TRUE)
    cat("unrealistic fluxes. A priori, however, no upper limit for kappa can be", 
        fill = TRUE)
    cat("provided, but interpretable upper limits can be computed from assumptions", 
        fill = TRUE)
    cat("about the *earliest time for chamber saturation*. This may, particularly,", 
        fill = TRUE)
    cat("be important with small data series, where random patterns in data may", 
        fill = TRUE)
    cat("exhibit strong exponential curvature and, erronesouly, produce huge fluxes.", 
        fill = TRUE)
    cat("", fill = TRUE)
    cat("Prefiltering", fill = TRUE)
    cat("------------", fill = TRUE)
    cat("A statistical prefiltering test is applied before actual HMR data analysis", 
        fill = TRUE)
    cat("in order to detect data series that are compatible with pure measurement", 
        fill = TRUE)
    cat("variation and hence no flux. These may important to detect in order to", 
        fill = TRUE)
    cat("avoid flux estimation bias.", fill = TRUE)
    cat("", fill = TRUE)
    cat("Automatic model selection", fill = TRUE)
    cat("-------------------------", fill = TRUE)
    cat("Options for user configured automatic model selection are provided. These", 
        fill = TRUE)
    cat("include controls for automatic handling of HMR recommendations from the", 
        fill = TRUE)
    cat("MSE criterion, prefiltering outcomes and flux limiting. Type R command", 
        fill = TRUE)
    cat("help(HMR,help_type='html') for an overview over the user configured", 
        fill = TRUE)
    cat("decision tree for automatic model selection with HMR.", 
        fill = TRUE)
    cat("", fill = TRUE)
    cat("NOTES", fill = TRUE)
    cat("-----", fill = TRUE)
    cat("* Details in the PDF manual and the help page (preferably HMTL).", 
        fill = TRUE)
    cat("* Suppress this message by option \"Display.Message=FALSE\".", 
        fill = TRUE)
    cat("", fill = TRUE)
  }
  if (Display.Message) {
    HMRmessage()
    flush.console()
  }
  MSE.zero <- 10 * max(.Machine$double.eps, .Machine$double.neg.eps)
  bracketing.tol <- 1e-07
  bracketing.maxiter <- 1000
  ngrid <- 1000
  xtxt <- "Time since deployment"
  ytxt <- "Chamber concentration"
  kappa.fixed <- FALSE
  xOK <- function(x) {
    OK <- TRUE
    if (sum(is.na(x)) > 0) {
      OK <- FALSE
    }
    else {
      if (max(abs(x)) == Inf) {
        OK <- FALSE
      }
    }
    OK
  }
  mysprintf <- function(x) {
    if (!is.na(x)) {
      d <- unlist(strsplit(x = sprintf("%.3e", x), split = ".", 
                           fixed = TRUE))
      dum <- paste(d[1], d[2], sep = dec)
    }
    else {
      dum <- "NA"
    }
    dum
  }
  if ((length(filename) != 1) | (!is.character(filename))) {
    FATAL <- TRUE
  }
  else {
    if (!(((sum(is.na(series)) == 1) & (length(series) == 
                                        1)) | ((length(series) > 0) & (is.character(series))))) {
      FATAL <- TRUE
    }
    else {
      if (!((is.character(dec)) & (length(dec) == 1) & 
            (is.character(sep)) & (length(sep) == 1))) {
        FATAL <- TRUE
      }
      else {
        if (!(((dec == ".") | (dec == ",")) & ((sep == 
                                                ";") | (sep == ",")))) {
          FATAL <- TRUE
        }
        else {
          if ((dec == ",") & (sep == ",")) {
            FATAL <- TRUE
          }
          else {
            if (!(is.logical(FollowHMR) & is.logical(LR.always) & 
                  is.logical(Display.Message))) {
              FATAL <- TRUE
            }
            else {
              if (!((is.numeric(pfalpha)) & (length(pfalpha) == 
                                             1) & ((is.na(pfvar)) | ((!is.na(pfvar)) & 
                                                                     (is.numeric(pfvar)) & (length(pfvar) == 
                                                                                            1))))) {
                FATAL <- TRUE
              }
              else {
                if (!(xOK(pfalpha) & (pfalpha > 0) & 
                      (pfalpha < 1) & (((!is.na(pfvar)) & 
                                        (xOK(pfvar)) & (pfvar > 0)) | (is.na(pfvar))))) {
                  FATAL <- TRUE
                }
                else {
                  if (!((is.character(IfNoValidHMR)) & 
                        (is.character(IfNoSignal)) & (is.character(IfNoFlux)))) {
                    FATAL <- TRUE
                  }
                  else {
                    if (!(((IfNoValidHMR == "LR") | (IfNoValidHMR == 
                                                     "No flux")) & ((IfNoSignal == "LR") | 
                                                                    (IfNoSignal == "No flux")) & ((IfNoFlux == 
                                                                                                   "LR") | (IfNoFlux == "No flux")))) {
                      FATAL <- TRUE
                    }
                    else {
                      if (!(((is.numeric(SatPct)) & (length(SatPct) == 
                                                     1) & (!is.na(SatPct)) & (is.numeric(SatTimeMin)) & 
                             (length(SatTimeMin) == 1) & (!is.na(SatTimeMin))) | 
                            (is.na(SatPct) & is.na(SatTimeMin)))) {
                        FATAL <- TRUE
                      }
                      else {
                        if (!((xOK(SatPct) & (SatPct > 
                                              0) & (SatPct < 100) & xOK(SatTimeMin) & 
                               (SatPct > 0)) | (is.na(SatPct) & 
                                                is.na(SatTimeMin)))) {
                          FATAL <- TRUE
                        }
                        else {
                          FATAL <- FALSE
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  if (FATAL) {
    Comment <- "Error in input parameters"
  }
  else {
    oldOutDec <- getOption("OutDec")
    options(OutDec = dec)
    testread <- .HMR.read(filename = filename, dec = dec, 
                          sep = sep)
    options(OutDec = oldOutDec)
    if (testread$FATAL) {
      FATAL <- TRUE
      Comment <- "Data file could not be read"
    }
    else {
      dataserier <- rep(NA, testread$nserier)
      for (i in 1:testread$nserier) dataserier[i] <- testread$HMRdata[[i]]$serie
      if ((sum(is.na(series)) == 1) & (length(series) == 
                                       1)) {
        userie <- dataserier
      }
      else {
        userie <- unique(series)
      }
      nserie <- length(userie)
      HMRdata <- vector(mode = "list", length = nserie)
      nJA <- 0
      for (i in 1:nserie) {
        if (sum(dataserier == userie[i]) > 0) {
          datai <- min((1:length(dataserier))[dataserier == 
                                                userie[i]])
          if (testread$HMRdata[[datai]]$status > 0) {
            nJA <- nJA + 1
            HMRdata[[i]] <- testread$HMRdata[[datai]]
          }
          else {
            HMRdata[[i]] <- list(serie = userie[i], status = 0)
          }
        }
        else {
          HMRdata[[i]] <- list(serie = userie[i], status = 0)
        }
      }
      if (nJA < 1) {
        FATAL <- TRUE
        Comment <- "Errors in all selected data series"
      }
      else {
        frame()
        oldmfrow <- par("mfrow")
        oldoma <- par("oma")
        oldbty <- par("bty")
        oldpty <- par("pty")
        if (LR.always) {
          OUTPUT <- data.frame(Series = "", f0 = 0, f0.se = 0, 
                               f0.p = 0, f0.lo95 = 0, f0.up95 = 0, Method = "", 
                               Warning = "", Prefilter = "", Prefilter.p = 0, 
                               SatCrit.Warning = "", LR.f0 = 0, LR.f0.se = 0, 
                               LR.f0.p = 0, LR.f0.lo95 = 0, LR.f0.up95 = 0, 
                               LR.Warning = "", stringsAsFactors = FALSE)
          colnames(OUTPUT) <- c("Series", "f0", "f0.se", 
                                "f0.p", "f0.lo95", "f0.up95", "Method", "Warning", 
                                "Prefilter", "Prefilter.p", "SatCrit.Warning", 
                                "LR.f0", "LR.f0.se", "LR.f0.p", "LR.f0.lo95", 
                                "LR.f0.up95", "LR.Warning")
        }
        else {
          OUTPUT <- data.frame(Series = "", f0 = 0, f0.se = 0, 
                               f0.p = 0, f0.lo95 = 0, f0.up95 = 0, Method = "", 
                               Warning = "", Prefilter = "", Prefilter.p = 0, 
                               SatCrit.Warning = "", stringsAsFactors = FALSE)
          colnames(OUTPUT) <- c("Series", "f0", "f0.se", 
                                "f0.p", "f0.lo95", "f0.up95", "Method", "Warning", 
                                "Prefilter", "Prefilter.p", "SatCrit.Warning")
        }
        STOP <- FALSE
        for (i in 1:nserie) if (!STOP) {
          if (HMRdata[[i]]$status > 0) {
            oHMR <- .HMR.fit1(tid = HMRdata[[i]]$tid, 
                              konc = HMRdata[[i]]$konc, A = HMRdata[[i]]$A, 
                              V = HMRdata[[i]]$V, serie = HMRdata[[i]]$serie, 
                              ngrid = ngrid, SatPct = SatPct, SatTimeMin = SatTimeMin, 
                              LR.always = LR.always, FollowHMR = FollowHMR, 
                              IfNoValidHMR = IfNoValidHMR, IfNoSignal = IfNoSignal, 
                              IfNoFlux = IfNoFlux, xtxt = xtxt, ytxt = ytxt, 
                              pcttxt = paste(" (", round(100 * i/nserie, 
                                                         0), "%)", sep = ""), MSE.zero = MSE.zero, 
                              bracketing.tol = bracketing.tol, bracketing.maxiter = bracketing.maxiter, 
                              kappa.fixed = kappa.fixed, pfvar = pfvar, 
                              pfalpha = pfalpha, dec = dec)
            if (LR.always) 
              OUTPUT <- rbind(OUTPUT, c(HMRdata[[i]]$serie, 
                                        mysprintf(oHMR$f0), mysprintf(oHMR$f0.se), 
                                        mysprintf(oHMR$f0.p), mysprintf(oHMR$f0.lo95), 
                                        mysprintf(oHMR$f0.up95), oHMR$method, 
                                        oHMR$warning, oHMR$prefilter, mysprintf(oHMR$pfpval), 
                                        oHMR$SatCritWarning, mysprintf(oHMR$LR.f0), 
                                        mysprintf(oHMR$LR.f0.se), mysprintf(oHMR$LR.f0.p), 
                                        mysprintf(oHMR$LR.f0.lo95), mysprintf(oHMR$LR.f0.up95), 
                                        oHMR$LR.warning))
            else OUTPUT <- rbind(OUTPUT, c(HMRdata[[i]]$serie, 
                                           mysprintf(oHMR$f0), mysprintf(oHMR$f0.se), 
                                           mysprintf(oHMR$f0.p), mysprintf(oHMR$f0.lo95), 
                                           mysprintf(oHMR$f0.up95), oHMR$method, oHMR$warning, 
                                           oHMR$prefilter, mysprintf(oHMR$pfpval), 
                                           oHMR$SatCritWarning))
            if (oHMR$warning == "Cancelled") {
              STOP <- TRUE
            }
          }
          else {
            if (LR.always) 
              OUTPUT <- rbind(OUTPUT, c(HMRdata[[i]]$serie, 
                                        NA, NA, NA, NA, NA, "None", "Data error", 
                                        "None", NA, NA, NA, NA, NA, NA, NA, "Data error"))
            else OUTPUT <- rbind(OUTPUT, c(HMRdata[[i]]$serie, 
                                           NA, NA, NA, NA, NA, "None", "Data error", 
                                           "None", NA, NA))
          }
        }
        par(mfrow = oldmfrow, oma = oldoma, bty = oldbty, 
            pty = oldpty)
      }
    }
  }
  if (FATAL) {
    Comment
  }
  else {
    oldOutDec <- getOption("OutDec")
    options(OutDec = dec)
    OUTPUT <- OUTPUT[-1, ]
    rownames(OUTPUT) <- paste(1:dim(OUTPUT)[1], sep = "")
    write.table(x = OUTPUT, file = paste("HMR - ", filename, 
                                         sep = ""), append = FALSE, quote = FALSE, dec = dec, 
                sep = sep, row.names = FALSE, col.names = TRUE)
    options(OutDec = oldOutDec)
    if (FollowHMR) {
      cat(sep = "\n")
      flush.console()
    }
    OUTPUT
  }
}
<bytecode: 0x000001fc7444b898>
  <environment: namespace:HMR>