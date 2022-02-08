#' Handle partial dates
#'
#' In CDISC datasets, dates can sometimes be incomplete due to missing
#' information.
#'
#' @param x A character vector containing a representation of a vector of dates
#' in `yyyy-mm-dd` format, some of which may be incomplete.
#' @param impute Whether and how to impute the missing information (see Details).
#'
#' @return A [Date] object.
#' @export
partial_date <- function(x, impute=c("missing", "first", "mid")) {
    impute <- match.arg(impute)
    x <- as.character(x)
    x <- gsub("-UNK$", "", x)
    x <- gsub("-UNK$", "", x)
    i <- grepl("^\\d\\d\\d\\d$", x)   # Year only
    x[i] <- paste0(x[i],
        switch(impute,
            mid   = "-06-01",
            first = "-01-01",
            ""))
    i <- grepl("^\\d\\d\\d\\d-\\d\\d$", x)   # Year-month
    x[i] <- paste0(x[i],
        switch(impute,
            mid   = "-15",
            first = "-01",
            ""))
    as.Date(strptime(x, "%Y-%m-%d", tz="UTC"))
}

#' Convert ISO8601 date/time reprensentation to `POSIXct`
#'
#' @param x A character vector containing dates/times in their ISO8601 representation.
#' @param impute Whether to impute missing time information.
#' @return A [POSIXct] object.
#' @export
convert_dtc <- function(x, impute=FALSE) {
    x <- as.character(x)
    if (impute) {
        i <- !is.na(x) & nchar(x) == 10
        x[i] <- paste0(x[i], "T00:00:00")
    }
    i <- !is.na(x) & nchar(x) == 16
    x[i] <- paste0(x[i], ":00")
    as.POSIXct(strptime(x, "%Y-%m-%dT%H:%M:%S", tz="UTC"))
}

#' Format a date/time object as its ISO8601 reprensentation
#'
#' @param x A date/time object (i.e., an object which can be converted to [POSIXlt]).
#' @return x A character vector containing the ISO8601 representation of `x`.
#' @seealso [strftime]
#' @export
format_dtc <- function(x) {
    strftime(x, format="%Y-%m-%dT%H:%M", tz=attr(x, "tzone"))
}

#' Convert a date and time to `POSIXct`
#'
#' @param date A character vector containing dates in either 'yyyy-mm-dd' or
#' 'mm/dd/yyyy' (note: US convension, with the month first) format.
#' @param time A character vector containing times in 24h format 'HH:MM' (note:
#' no seconds).
#' @return A [POSIXct] object.
#' @examples
#' convert_date_and_time(c("2022-01-21", "01/21/2022"), c("21:14", "9:14"))
#' @export
convert_date_and_time <- function(date, time) {
    date <- as.character(date)
    i <- grepl("/", date)
    temp1 <- as.POSIXct(strptime(paste(date[i], time[i]), "%m/%d/%Y %H:%M", tz="UTC"))
    temp2 <- as.POSIXct(strptime(paste(date[!i], time[!i]), "%Y-%m-%d %H:%M", tz="UTC"))
    res <- rep(temp1[1], length(date)) 
    res[i] <- temp1
    res[!i] <- temp2
    res
}

#' Convert time in seconds since midnight to hours and minutes
#'
#' @param x A vector containing time in seconds since midnight.
#' @return A character vector containing times in 24h format 'HH:MM' (note: no seconds).
#' @examples
#' convert_secs(1024)
#' @export
convert_secs <- function(x) {
    if (is.factor(x)) {
        x <- as.character(x)
    }
    if (is.character(x)) {
        x <- as.numeric(sub("([[:digit:]]+).*", "\\1", x))
    }
    # Note: no seconds
    if (any(!is.na(x) & x > 60*60*24)) {
        warning(sprintf("Maximum seconds %d exceeds number of %d seconds in 24 hours", max(x), 60*60*24))
    }
    stopifnot(is.na(x) | x %% 60 == 0)
    x <- as.POSIXct(strptime("00:00", "%H:%M", tz="UTC")) + as.difftime(x, units="secs")
    format(x, "%H:%M")
}

#' Helper function to read data from files
#'
#' @param x A file name.
#' @param path Optionally, a path in which to look for `file`.
#' @param ... Further arguments passed to the function that does the actual reading.
#' @param .fun A function that reads a file passed as its first argument. The
#' default is to use an appropriate function depending on the file extension.
#' @param .lc If `TRUE`, then convert all names to lower case in the returned object.
#' @param .dt If `TRUE`, then then returned a `data.table`.
#' @return If `.dt` is `TRUE`, then a `data.table` is returned. Otherwise, a
#' `data.frame` is returned unless `.fun` is specified, in which case that
#' function determines the return type.
#' @export
read_data <- function(file, path=NULL, ..., .fun=NULL, .lc=TRUE, .dt=TRUE) {

    if (!is.null(path)) {
        file <- file.path(path, file)
    }

    if (!file.exists(file)) {
        stop(sprintf("File does not exist: %s", file))
    }

    if (!is.null(.fun)) {
        x <- .fun(file, ...)
    } else if (grepl("\\.xpt$|\\.sas7bdat$", file)) {
        x <- read_data_sas(file=file, ...)
    } else if (grepl("\\.xls$|\\.xlsx$", file)) {
        x <- read_data_excel(file=file, ...)
    } else if (grepl("\\.csv$", file)) {
        if (.dt) {
            x <- data.table::fread(file, ...)
        } else {
            x <- read.csv(file, ...)
        }
    } else {
        stop("Unsupported filetype")
    }

    if (.dt) {
        x <- data.table::as.data.table(x)
    }

    if (.lc) {
        names(x) <- tolower(names(x))
    }

    x
}

#' Helper function to read data from SAS files
#'
#' @param x A file name.
#' @param path Optionally, a path in which to look for `file`.
#' @param ... Further arguments passed to the function that does the actual reading.
#' @return A `data.frame`.
#' @export
read_data_sas <- function(file, path=NULL, ...) {
    if (!is.null(path)) {
        file <- file.path(path, file)
    }
    if (!file.exists(file)) {
        stop(sprintf("File does not exist: %s", file))
    }
    if (grepl("\\.xpt$", file)) {
        x <- tryCatch({
                SASxport::read.xport(file, ...)
        }, error=function(e) e)
        if (inherits(x, "error")) {
            # Give up
            stop(sprintf("Unable to read sas dataset: %s", x$message))
        }
        for (i in seq_len(ncol(x))) {
            class(x[[i]]) <- class(x[[i]])[class(x[[i]]) != "labelled"] 
        }
    } else if (grepl("\\.sas7bdat$", file)) {
        x <- tryCatch(
            haven::read_sas(file, ...),  # Requires package 'haven'
            error=function(e) e)
        if (inherits(x, "error")) {
            require(sas7bdat.parso)
            x <- tryCatch(
                sas7bdat.parso::read.sas7bdat.parso(file, stringsAsFactors=F),  # Requires package 'sas7bdat.parso'
                error=function(e) e)
        }
        if (inherits(x, "error")) {
            # Give up
            stop(sprintf("Unable to read sas dataset: %s", x$message))
        }
        names(x) <- tolower(names(x))
        for (i in seq_len(ncol(x))) {
            if (class(x[[i]])[1] == "character") {
                x[[i]][x[[i]] == ""] <- NA
            }
            if (inherits(x[[i]], "hms")) {
                l <- attr(x[[i]], "label")
                j <- which(is.na(x[[i]]))
                x[[i]] <- format(x[[i]])
                x[[i]][j] <- NA
                attr(x[[i]], "label") <- l 
            }
            # Remove line breaks in labels
            attr(x[[i]], "label") <- gsub("\r|\n", "", attr(x[[i]], "label"))
            format.sas <- attr(x[[i]], "format.sas")
            if (!is.null(format.sas) && format.sas == "E8601DA" && is.numeric(x[[i]])) {
                x[[i]] <- as.Date("1960-01-01") + x[[i]]
            }
        }
        class(x) <- "data.frame"
    } else {
        stop("Unsupported filetype")
    }
    x
}

#' Helper function to read data from Excel files
#'
#' @param x A file name.
#' @param path Optionally, a path in which to look for `file`.
#' @param ... Further arguments passed to the function that does the actual reading.
#' @return A `data.frame`.
#' @export
read_data_excel <- function(file, path=NULL, ...) {
    if (!is.null(path)) {
        file <- file.path(path, file)
    }
    if (!file.exists(file)) {
        stop(sprintf("File does not exist: %s", file))
    }
    if (grepl("\\.xls$", file)) {
        x <- tryCatch(
            readxl::read_xls(file, ...),  # Requires package 'readxl'
            error=function(e) e)
    } else if (grepl("\\.xlsx$", file)) {
        x <- tryCatch(
            openxlsx::read.xlsx(file, ...),  # Requires package 'openxlsx'
            error=function(e) e)
    } else {
        stop("Unsupported filetype")
    }

    if (inherits(x, "error")) {
        # Give up
        stop(sprintf("Unable to read excel dataset: %s", x$message))
    }
    onames <- names(x)
    names(x) <- make.names(tolower(names(x)))
    for (i in seq_len(ncol(x))) {
        #l <- attr(x[[i]], "label")
        l <- onames[i]
        if (class(x[[i]])[1] == "character") {
            x[[i]][x[[i]] == ""] <- NA
            x[[i]] <- as.factor(x[[i]])
        }
        if (inherits(x[[i]], "hms")) {
            j <- which(is.na(x[[i]]))
            x[[i]] <- format(x[[i]])
            x[[i]][j] <- NA
        }
        # Remove line breaks in labels
        attr(x[[i]], "label") <- gsub("\r|\n", "", l)
    }
    class(x) <- "data.frame"
    x
}

#' Merge SDTM "supp" data
#'
#' @param base A `data.frame` containing the "base" data (e.g. vc, lb, pc, ...).
#' @param supp A `data.frame` containing the "supp" data (e.g. suppvc, supplb, supppc, ...).
#' @param .lc If `TRUE`, then convert all new column names added to `base` to lower case.
#' @return A `data.frame`.
#' @export
merge_supp <- function(base, supp, .lc=TRUE) {
    if (is.null(supp) || nrow(supp) == 0) {
        return(base)
    }
    nm <- names(base)
    names(base) <- tolower(names(base))
    names(supp) <- tolower(names(supp))
    ul <- unique(supp[, c("idvar", "qnam", "qlabel")])
    label.mapping <- mappings::mapping(ul$qnam, ul$qlabel)
    for (j in 1:nrow(ul)) {
        idvar <- ul$idvar[j]
        qnam <- ul$qnam[j]
        if (.lc) {
            newv <- tolower(qnam)
        } else {
            newv <- qnam
        }
        if (is.na(idvar)) {
            temp <- supp[supp$qnam==qnam,]
            id1 <- with(temp, paste(usubjid))
            id2 <- paste(base$usubjid)
        } else {
            temp <- supp[supp$qnam==qnam & supp$idvar==idvar,]
            id1 <- with(temp, paste(usubjid, idvarval, sep="|"))
            id2 <- paste(base$usubjid, paste(base[[tolower(idvar)]]), sep="|")
        }
        if (any(table(id1) > 1)) {
            message(sprintf("%s is many-to-one and will not be merged. Skipping.", newv))
            next
        }
        rownames(temp) <- id1
        if (!is.null(base[[newv]])) {
            message(sprintf("%s already exists. Skipping.", newv))
        } else {
            base[[newv]] <- temp[ifelse(id2 %in% id1, id2, NA), "qval"]
            temp <- suppressWarnings(as.numeric(as.character(base[[newv]])))
            if (all(is.na(base[[newv]]) | !is.na(temp))) {
                message(sprintf("Converting %s to numeric.", newv))
                base[[newv]] <- temp
            }
            attr(base[[newv]], "label") <- as.character(label.mapping(qnam))
        }
    }
    names(base)[seq_along(nm)] <- nm
    base
}

#' Read SDTM data
#'
#' @param domain An SDTM "domain" (usually 2 letters, such as "dm" or "vs").
#' @param path A path in which to search for SDTM files.
#' @param extension File extension(s) to search for (can be a regular expression).
#' @param mergesupp If `TRUE` (the default), search for and merge an associated "supp" file.
#' @param verbose If `TRUE` (the default), output informative messages.
#' @return A `data.frame`.
#' @export
read_sdtm <- function(domain, path=getOption("path.sdtm", "."), extension="(sas7bdat|xpt)", mergesupp=TRUE, verbose=TRUE) {
    file.base <- dir(path, pattern=paste0("^", domain, "\\.", extension), full.names=TRUE)[1]
    file.supp <- dir(path, pattern=paste0("^supp", domain, "\\.", extension), full.names=TRUE)[1]
    if (isTRUE(verbose) && file.exists(file.base)) {
        message(sprintf("Reading %s", file.base))
    }
    base <- read_data(file.base)
    if (mergesupp && file.exists(file.supp)) {
        if (isTRUE(verbose)) {
            message(sprintf("Found %s. Merging.", paste0("supp", domain)))
        }
        supp <- read_data(file.supp)
        if (verbose) {
            showtable <- function(x) knitr::kable(as.data.frame(table(droplevels(as.factor(x)))), row.names=F)
            print(with(supp, showtable(qlabel)))
        }
        merge_supp(base, supp)
    } else {
        base
    }
}

#' Read ADaM data
#'
#' @param domain An ADaM "domain" (usually 4 letters starting with "ad", such as "adsl" or "adae").
#' @param path A path in which to search for ADaM files.
#' @param extension File extension(s) to search for (can be a regular expression).
#' @param verbose If `TRUE` (the default), output informative messages.
#' @return A `data.frame`.
#' @export
read_adam <- function(domain, path=getOption("path.adam", "."), extension="(sas7bdat|xpt)", verbose=TRUE) {
    read_sdtm(domain=domain, path=path, extension=extension, verbose=verbose, mergesupp=FALSE)
}

#' Safely bind rows
#'
#' @param ... Objects (typically `data.frame`s) that contain rows to bind.
#' @param .prefer What to do when column types don't match (such as when the
#' same column is numeric in one `data.frame` and textual in another).
#' @return A `data.frame`.
#' @seealso [rbind]
#' @export
safe_rbind <- function(..., .prefer=c("factor", "character", "error")) {
    .prefer <- match.arg(.prefer)
    l <- list(...)
    l <- l[!sapply(l, is.null)]
    l <- l[!sapply(l, function(x) nrow(x) == 0)]
    n <- length(l)
    if (n==1) {
        return(l[[1]])
    }
    vars <- unique(do.call(c, lapply(l, names)))
    for (v in vars) {
        ichar <- unlist(sapply(l, function(x) is.character(x[[v]])))
        ifact <- unlist(sapply(l, function(x) is.factor(x[[v]])))
        inumb <- unlist(sapply(l, function(x) is.numeric(x[[v]])))
        if ((any(ichar) || any(ifact)) && any(inumb) && .prefer != "error") {
            message(paste0("Promoting ", v, " from numeric to ", .prefer))
            for (i in seq_len(n)) {
                if (.prefer == "character" && inumb[i]) {
                    l[[i]][[v]] <- as.character(l[[i]][[v]])
                }
                if (.prefer == "factor" && inumb[i]) {
                    l[[i]][[v]] <- as.factor(l[[i]][[v]])
                }
            }
        }
        ichar <- unlist(sapply(l, function(x) is.character(x[[v]])))
        ifact <- unlist(sapply(l, function(x) is.factor(x[[v]])))
        if (any(ichar) && any(ifact) && .prefer != "error") {
            for (i in seq_len(n)) {
                if (.prefer == "character" && ifact[i]) {
                    l[[i]][[v]] <- as.character(l[[i]][[v]])
                }
                if (.prefer == "factor" && ichar[i]) {
                    l[[i]][[v]] <- as.factor(l[[i]][[v]])
                }
            }
        }
        iposix <- unlist(sapply(l, function(x) inherits(x[[v]], "POSIXt")))
        if (any(iposix)) {
            for (i in seq_len(n)) {
                if (!is.null(l[[i]][[v]])) {
                    # Do this silently
                    l[[i]][[v]] <- as.POSIXct(l[[i]][[v]])
                }
            }
        }
        myclass <- function(x) { if (is.numeric(x)) "numeric" else class(x) }
        iclass <- lapply(l, function(x) myclass(x[[v]]))
        iallna <- unlist(sapply(l, function(x) sum(!is.na(x[[v]])) == 0))
        if (!all(iallna)) {
            cclass <- unique(Reduce(intersect, iclass[!iallna]))
            if (length(cclass) == 0) {
                stop(paste0("Attempting to rbind column '", v, "' of mismatched types: ", paste0(unique(unlist(iclass)), collapse=", ")))
            }
        }
    }
    l2 <- lapply(l, function(x) {
            for (v in vars) {
                if (is.null(x[[v]])) {
                    x[[v]] <- NA
                } else if (is.factor(x[[v]])) {
                    x[[v]] <- as.character(x[[v]])
                }
            }
            x[, vars, drop=FALSE]
          })
    l3 <- do.call(rbind, l2)
    for (v in vars) {
        lvls <- unique(do.call(c, lapply(l, function(x) levels(x[[v]]))))
        if (!is.null(lvls)) {
            l3[[v]] <- factor(l3[[v]], levels=lvls)
        }
        lbls <- unique(do.call(c, lapply(l, function(x) attr(x[[v]], "label"))))
        if (!is.null(lbls)) {
            if (length(lbls) > 1) {
                attr(l3[[v]], "label") <- paste0("MULTIPLE LABELS: ", paste0(lbls, collapse=";"))
            } else {
                attr(l3[[v]], "label") <- lbls
            }
        }
    }
    l3
}

# read_from_path <- function(path, read.fun=read_data, lcnames=T, verbose=T) {
#     if (verbose) {
#         message(paste0("Reading from: ", path), "\n")
#         message(paste0("Contents: "), "\n")
#         message(dir(path), "\n")
#     }
#     function(file, ..., path=path, read.fun=read.fun, lcnames=lcnames, verbose=verbose) {
#         fixnames <- function(x) {
#             if (lcnames) {
#                 if (verbose) {
#                     message("Setting names to lowercase")
#                 }
#                 setNames(x, tolower(names(x)))
#             } else {
#                 x
#             }
#         }
#         fixnames(read.fun(file.path(path, file), ...))
#     }
# }

#' Check for the value "Y", ignoring `NA`
#' @param x A vector of type `character` or `factor`.
#' @return A vector of type `logical`.
#' @export
isY <- function(x) {
    !is.na(x) & x == "Y"
}

#' Check for the value "N", ignoring `NA`
#' @param x A vector of type `character` or `factor`.
#' @return A vector of type `logical`.
#' @export
isN <- function(x) {
    !is.na(x) & x == "N"
}

#' Check for the value "0" (string or numeric), ignoring `NA`
#' @param x A vector of type `numeric`, `character` or `factor`.
#' @return A vector of type `logical`.
#' @export
is0 <- function(x) {
    !is.na(x) & as.character(x) == "0"
}

#' Check for the value "1" (string or numeric), ignoring `NA`
#' @param x A vector of type `numeric`, `character` or `factor`.
#' @return A vector of type `logical`.
#' @export
is1 <- function(x) {
    !is.na(x) & as.character(x) == "1"
}

#' Check for the value "NOT DONE", ignoring `NA`
#' @param x A vector of type `character` or `factor`.
#' @return A vector of type `logical`.
#' @export
isND <- function(x) {
    !is.na(x) & x == "NOT DONE"
}

#' Shorthand Shorthand for `is.na(x) | x`.
#'
#' @param x A vector of type `logical`.
#' @return A vector of type `logical`.
#' @export
includeNA <- function(x) {
    is.na(x) | x
}

#' Shorthand Shorthand for `!is.na(x) & x`.
#'
#' @param x A vector of type `logical`.
#' @return A vector of type `logical`.
#' @export
excludeNA <- function(x) {
    !is.na(x) & x
}

#' Generate a database summary
#'
#' Using R markdown, generate and HTML file containing a summary of the
#' contents of a clinical study database (STDM and ADaM), similar to a data
#' specification.  This is useful to search for specific information in the
#' absense of a true data specification.
#'
#' @param path.sdtm The path to the STDM files.
#' @param path.adam The path to the ADaM files.
#' @param outfile The name of the output file to generate.
#' @return Called for its side effects (returns `NULL`).
#' @examples
#' \dontrun{
#' #options(
#' #    path.sdtm = system.file("cdiscpilot01/sdtm", package="CDISC"),
#' #    path.adam = system.file("cdiscpilot01/adam", package="CDISC"))
#'
#' temp <- tempfile("demo_cdisc_database_summary", fileext=".html")
#' generate_database_summary(outfile=temp)
#' browseURL(temp)
#' unlink(temp)
#' }
#' @export
generate_database_summary <- function(
    path.sdtm = getOption("path.sdtm", NULL),
    path.adam = getOption("path.adam", NULL),
    outfile   = paste0("cdisc_database_summary_", format(Sys.time(), "%Y%m%d%H%M%S"), ".html")
) {
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        stop("Requires package `rmarkdown`. Please install it.", call.=F)
    }

    if (!requireNamespace("lumos", quietly = TRUE)) {
        stop("Requires package `lumos`. Please install it.", call.=F)
    }

    temp <- tempfile("cdisc_database_summary", fileext=".R")
    catf <- function(...) cat(..., file=temp, append=TRUE)

    catf('#\' ---
#\' title:  "CDISC Database Summary"
#\' author: "Generated by the R CDISC Package"
#\' date:   "`r format(Sys.time(), \'%d-%b-%Y\')`"
#\' output: 
#\'   html_document:
#\'     toc: true
#\'     toc_float: true
#\'     toc_collapsed: true
#\'     toc_depth: 3
#\' ---

#+ eval=T, echo=F, results=\'hide\'
knitr::opts_chunk$set(echo=F, results=\'asis\', message=T, warning=T, error=T)
#+

suppressPackageStartupMessages({
    library(CDISC)
    library(lumos)
})

')


    # SDTM

    if (!is.null(path.sdtm)) {

        files <- dir(path.sdtm, pattern="\\.(sas7bdat|xpt)")
        if (length(files) > 0) {

            catf('#+ SDTM\n\n')
            catf('#\' # SDTM\n\n')

            domains <- sub("\\..*$", "", files)
            domains <- domains[!grepl("^supp", domains)] # Exclude supp
            for (d in domains) {
                catf(sprintf('#+ %s\n\n', d))
                catf(sprintf('#\' ## %s\n\n', d))
                catf(sprintf('%s <- read_sdtm("%s", path="%s", verbose=F)\n', d, d, path.sdtm))
                catf(sprintf('lumos(%s)\n\n', d))
                catf('\n')
                catf(sprintf('for (v in grep("^.*test$|^param$", names(%s), value=T)) {\n', d))
                catf('cat(sprintf("### %s\n\n", v))\n')
                catf(sprintf('    x <- %s[[v]]\n', d))
                catf('    attr(x, "label")  <- v\n')
                catf(sprintf('    print(lumos(%s[[v]], .max=Inf, .pct=T, .order.by.freq=F))\n', d))
                catf('}\n')
            }
        }
    }


    # ADaM

    if (!is.null(path.adam)) {

        files <- dir(path.adam, pattern="\\.(sas7bdat|xpt)")
        if (length(files) > 0) {

            catf('#+ ADaM\n\n')
            catf('#\' # ADaM\n\n')

            domains <- sub("\\..*$", "", files)
            for (d in domains) {
                catf(sprintf('#+ %s\n\n', d))
                catf(sprintf('#\' ## %s\n\n', d))
                catf(sprintf('%s <- read_adam("%s", path="%s", verbose=F)\n', d, d, path.adam))
                catf(sprintf('lumos(%s)\n\n', d))
                catf('\n')
                catf(sprintf('for (v in grep("^.*test$|^param$", names(%s), value=T)) {\n', d))
                catf('cat(sprintf("### %s\n\n", v))\n')
                catf(sprintf('    print(lumos(%s[[v]], .max=Inf, .pct=T, .order.by.freq=F))\n', d))
                catf('}\n')
            }
        }
    }

    rmarkdown::render(temp, knit_root_dir=getwd(), output_file=outfile, output_dir=getwd(), quiet=TRUE)
    unlink(temp)

    invisible(NULL)
}

