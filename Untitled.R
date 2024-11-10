

if (!"ISO" %in% colnames(combined_data)) {
  print("ISO column not found, doing something...")
  # 其他操作代码
}

if (!"ISO" %in% colnames(combined_data)) {
  # 如果不存在 "ISO" 列，则执行某些操作
  dictionary <- countrycode::codelist
  attr(dictionary, "origin_valid") <- c(
    "cctld", "country.name", "country.name.de", "country.name.fr", "country.name.it",
    "cowc", "cown", "dhs", "ecb", "eurostat", "fao", "fips", "gaul",
    "genc2c", "genc3c", "genc3n", "gwc", "gwn", "imf", "ioc", "iso2c",
    "iso3c", "iso3n", "p5c", "p5n", "p4c", "p4n", "un", "un_m49", "unicode.symbol",
    "unhcr", "unpd", "vdem", "wb", "wb_api2c", "wb_api3c", "wvs",
    "country.name.en.regex", "country.name.de.regex", "country.name.fr.regex", "country.name.it.regex"
  )
  attr(dictionary, "origin_regex") <- c(
    "country.name.de.regex",
    "country.name.en.regex",
    "country.name.fr.regex",
    "country.name.it.regex"
  )
} else {
  dictionary <- spatial_data["ISO"]
}
custom_dict <- spatial_data["ISO"]
countrycode <- function(sourcevar, origin, destination, warn = TRUE, nomatch = NA,
                        custom_dict = NULL, custom_match = NULL, origin_regex = NULL) {
  
  # default dictionary
  if (is.null(spatial_data["ISO"])) {
    dictionary <- countrycode::codelist
    attr(dictionary, "origin_valid") <- c(
      "cctld", "country.name", "country.name.de", "country.name.fr", "country.name.it",
      "cowc", "cown", "dhs", "ecb", "eurostat", "fao", "fips", "gaul",
      "genc2c", "genc3c", "genc3n", "gwc", "gwn", "imf", "ioc", "iso2c",
      "iso3c", "iso3n", "p5c", "p5n", "p4c", "p4n", "un", "un_m49", "unicode.symbol",
      "unhcr", "unpd", "vdem", "wb", "wb_api2c", "wb_api3c", "wvs",
      "country.name.en.regex", "country.name.de.regex", "country.name.fr.regex", "country.name.it.regex")
    attr(dictionary, "origin_regex") <- c("country.name.de.regex",
                                          "country.name.en.regex",
                                          "country.name.fr.regex",
                                          "country.name.it.regex")
  } else {
    dictionary <- custom_dict
  }
  
  # default country names (only for default dictionary)
  if (is.null(ISO)) {
    if (origin == 'country.name') {
      origin <- 'country.name.en'
    }
    if (origin %in% c('country.name.en', 'country.name.de', 'country.name.it', 'country.name.fr')) {
      origin <- paste0(origin, '.regex')
    }
    destination[destination == "country.name"] <- 'country.name.en'
  }
  
  # dictionary attributes
  if (is.null(attr(dictionary, "origin_valid"))) {
    origin_valid <- colnames(dictionary)
  } else {
    origin_valid <- attr(dictionary, "origin_valid")
  }
  
  if (is.null(attr(dictionary, "destination_valid"))) {
    destination_valid <- colnames(dictionary)
  } else {
    destination_valid <- attr(dictionary, "destination_valid")
  }
  
  if (is.null(origin_regex)) { # user can override
    if (!is.null(attr(dictionary, "origin_regex"))) {
      origin_regex <- origin %in% attr(dictionary, "origin_regex")
    } else {
      origin_regex <- FALSE
    }
  }

  
  # Allow tibbles as conversion dictionary
  if('tbl_df' %in% class(dictionary)){ # allow tibble
    dictionary <- as.data.frame(dictionary)
  }
  
  # Sanity checks
  if (missing(sourcevar)) {
    stop('sourcevar is NULL (does not exist).')
  }
  
  if (!mode(sourcevar) %in% c('character', 'numeric')) {
    stop('sourcevar must be a character or numeric vector. This error often arises when users pass a tibble (e.g., from dplyr) instead of a column vector from a data.frame (i.e., my_tbl[, 2] vs. my_df[, 2] vs. my_tbl[[2]]). This can also happen when `sourcevar` is entirely composed of `NA`, which `R` treats as entries of class logical.')
  }
  
  if (!is.null(nomatch) & (length(nomatch) != 1) & (length(nomatch) != length(sourcevar))) {
    stop('nomatch needs to be NULL, or of length 1 or ', length(sourcevar), '.')
  }
  
  if (!is.character(origin) ||
      length(origin) != 1 ||
      !origin %in% origin_valid) {
    stop(sprintf("The `origin` argument must be a string of length 1 equal to one of these values: %s.",
                 paste(origin_valid, collapse = ", ")))
  }
  
  if (!is.character(destination) ||
      !all(destination %in% destination_valid)) {
    stop("The `destination` argument must be a string or a vector of strings where each element is equal to one of the column names in the conversion directory (by default: `codelist`).")
  }
  
  if(!inherits(dictionary, "data.frame")) {
    stop("The `dictionary` argument must be a data frame or tibble with codes as columns.")
  }
  
  dups = any(duplicated(stats::na.omit(dictionary[, origin])))
  if(dups){
    stop("Countrycode cannot accept dictionaries with duplicated origin codes")
  }
  
  # Copy origin_vector for later re-use
  origin_vector <- sourcevar
  
  # Case-insensitive matching
  if(is.null(custom_dict)){ # only for built-in dictionary
    # unicode.symbol breaks uppercase on Windows R-devel 2022-02-02; rejected by CRAN
    if(inherits(origin_vector, 'character') & !grepl('country|unicode.symbol', origin)){
      # only apply toupper() on unique values and match after.
      # much faster than applying toupper() on the whole vector
      # when vector is very large
      uniques <- unique(origin_vector)
      uppercase <- toupper(uniques)
      origin_vector <- unname(uppercase[match(origin_vector, uniques)])
    }
  }
  
  out <- rep(NA, length(sourcevar))
  for (dest in destination) {
    if (length(destination) == 1) {
      out <- countrycode_convert(
        ## user-supplied arguments
        sourcevar = sourcevar,
        origin = origin,
        destination = dest,
        warn = warn,
        nomatch = nomatch,
        custom_dict = custom_dict,
        custom_match = custom_match,
        origin_regex = origin_regex,
        ## countrycode-supplied arguments
        origin_vector = origin_vector,
        dictionary = dictionary)
    } else {
      out <- ifelse(is.na(out),
                    countrycode_convert(
                      ## user-supplied arguments
                      sourcevar = sourcevar,
                      origin = origin,
                      destination = dest,
                      warn = warn,
                      nomatch = nomatch,
                      custom_dict = custom_dict,
                      custom_match = custom_match,
                      origin_regex = origin_regex,
                      ## countrycode-supplied arguments
                      origin_vector = origin_vector,
                      dictionary = dictionary),
                    out)
    }
  }
  return(out)
}


#' internal function called by `countrycode()`
#'
#' @keywords internal
#' @noRd
countrycode_convert <- function(# user-supplied arguments
  sourcevar,
  origin,
  destination,
  warn,
  nomatch,
  custom_dict,
  custom_match,
  origin_regex,
  # countrycode-supplied arguments
  origin_vector,
  dictionary
) {
  
  
  # Convert
  if (origin_regex) { # regex codes
    dict <- stats::na.omit(dictionary[, c(origin, destination)])
    sourcefctr <- factor(origin_vector)
    
    # possibilities (add NA so there's at least one item)
    choices <- c(levels(sourcefctr), NA)
    # sometimes an error is triggered by encoding issues
    choices <- tryCatch(trimws(choices), error = function(e) choices)
    
    # Apply all regexes on all inputs. This gives a matrix where rows
    # are the inputs and columns are the regexes.
    # For each row, the `TRUE` values indicate the matches.
    matchidx <- sapply(dict[[origin]], grepl, x = choices,
                       perl = TRUE, ignore.case = TRUE)
    if (all(is.na(choices))) {
      matches <- vector("list", length = length(choices))
    } else {
      # Issue reported to Vincent by email
      # simplify=FALSE was introduced in R 4.1.0. we want coverage before
      # out <- try(apply(matchidx, 1, which, simplify = FALSE))
      out <- apply(matchidx, 1, which)
      if (length(out) == 0) {
        out <- rep(list(NULL), nrow(matchidx))
      }
      names(out) <- choices
      matches <- lapply(out, function(x) dict[x, destination])
    }
    
    
    # fill elements that have zero matches with the appropriate NA
    matches[sapply(matches, length) == 0] <- `class<-`(NA, class(dict[[destination]]))
    
    # create destination_list with elements that have more than one match
    destination_list <- matches[sapply(matches, length) > 1]
    
    # add origin_vector value to beginning of match results to replicate previous behavior
    destination_list <- Map(c, names(destination_list), destination_list)
    
    # set elements with multiple matches to the appropriate NA
    matches[sapply(matches, length) > 1] <- `class<-`(NA, class(dict[[destination]]))
    
    # remove all but last match to replicate previous behavior
    matches <- sapply(matches, function(x) { x[length(x)] })
    
    # replace with custom matches if set
    if (!is.null(custom_match)) {
      matchidxs <- match(names(matches), names(custom_match))
      cust_matched <- !is.na(matchidxs)
      matches[cust_matched] <- custom_match[matchidxs][cust_matched]
    }
    
    # apply new levels to sourcefctr and unname
    destination_vector <- unname(matches[as.numeric(sourcefctr)])
    
  } else { # non-regex codes
    
    # sanity check
    if (is.character(origin_vector) && is.numeric(dictionary[[origin]])) {
      msg <- sprintf("To convert a `%s` code, `sourcevar` must be numeric.", origin)
      stop(msg, call. = FALSE)
    }
    
    dict <- stats::na.omit(dictionary[, c(origin, destination)])
    
    sourcefctr <- factor(origin_vector)
    
    # match levels of sourcefctr
    if (identical(origin, "cctld")) {
      matchidxs <- match(levels(sourcefctr), toupper(dict[[origin]]))
    } else {
      matchidxs <- match(levels(sourcefctr), dict[[origin]])
    }
    matches <- dict[[destination]][matchidxs]
    
    # replace with custom matches if set
    if (!is.null(custom_match)) {
      matchidxs <- match(levels(sourcefctr), names(custom_match))
      cust_matched <- !is.na(matchidxs)
      matches[cust_matched] <- custom_match[matchidxs][cust_matched]
    }
    
    # apply new levels to sourcefctr
    destination_vector <- matches[as.numeric(sourcefctr)]
  }
  
  # Filling-in failed matches
  sane_sourcevar <- class(sourcevar)[1] == class(destination_vector)[1]
  sane_nomatch <- class(nomatch)[1] == class(destination_vector)[1]
  idx <- is.na(destination_vector)
  if (is.null(nomatch)) {
    if (sane_sourcevar) {
      destination_vector[idx] <- sourcevar[idx]
    } else if (class(sourcevar)[1] == "factor" & class(destination_vector)[1] == "character") {
      destination_vector[idx] <- as.character(sourcevar[idx])
    } else {
      warning("The origin and destination codes are not of the same
                    class. Filling-in bad matches with NA instead.", call. = FALSE)
    }
  } else if ((length(nomatch) == 1) && is.na(nomatch)) { # NA
  } else if ((length(nomatch) == 1) && sane_nomatch) { # single replacement
    destination_vector[idx] <- nomatch
  } else if ((length(nomatch) == length(sourcevar)) & sane_sourcevar) { # vector replacement
    destination_vector[idx] <- nomatch[idx]
  } else {
    warning("The argument `nomatch` must be NULL, NA, or of the same class
                as the destination vector. Filling-in bad matches with NA instead.", call. = FALSE)
  }
  
  # Warnings
  if(warn){
    badmatch <- sort(unique(origin_vector[is.na(destination_vector)]))
    badmatch <- badmatch[!badmatch %in% names(custom_match)]  # do not report <NA>'s that were set explicitly by custom_match
    if(length(badmatch) > 0){
      warning("Some values were not matched unambiguously: ", paste(badmatch, collapse=", "), "\n", call. = FALSE)
    }
    if(origin_regex){
      if(length(destination_list) > 0){
        destination_list <- lapply(destination_list, function(k) paste(k, collapse=','))
        destination_list <- sort(unique(do.call('c', destination_list)))
        warning("Some strings were matched more than once, and therefore set to <NA> in the result: ", paste(destination_list, collapse="; "), "\n", call. = FALSE)
      }
    }
  }
  return(destination_vector)
}

library(dplyr)
# install.packages("sf")  # 安装 sf 包
library(sf)             # 加载 sf 包
library(readr)
spatial_data <- st_read("/Users/huu77/Desktop/term1/0005GISS/wk4/hw/World_Countries_(Generalized)_9029012925078512962.geojson")
gender_data <- read_csv("/Users/huu77/Desktop/term1/0005GISS/wk4/hw/gender_data.csv", locale = locale(encoding = "latin1"), na = "n/a")
names(gender_data)
names(spatial_data)
names(gender_data) <- toupper(names(gender_data))
gender2019 <-  
  gender_data %>% 
  filter(YEAR=='2019')
head(gender2019)
gender2010 <-  
  gender_data %>% 
  filter(YEAR=='2010')
head(gender2010)
common_countries <- intersect(gender2019$COUNTRYISOCODE, gender2010$COUNTRYISOCODE)
# print(common_categories)
gender2019_common <- gender2019[gender2019$COUNTRYISOCODE %in% common_countries, ]
gender2010_common <- gender2010[gender2010$COUNTRYISOCODE %in% common_countries, ]
gender2019_common <- gender2019_common[order(gender2019_common$COUNTRYISOCODE), ]
gender2010_common <- gender2010_common[order(gender2010_common$COUNTRYISOCODE), ]
library(dplyr)

gender_diff <- gender2019_common %>%
  mutate(across(where(is.numeric), ~ . - gender2010_common[[cur_column()]]))
# Assuming your data is in a data frame called gender_diff
gender_diff <- gender_diff[ , !names(gender_diff) %in% "YEAR"]
combined_data <- spatial_data %>% 
  left_join(gender_diff, by = "COUNTRY")
