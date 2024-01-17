#' Read in .dp or .pjn files as data frames
#'
#' @importFrom utils read.csv
#' @param fpath A file path string to the `.dp` input file
#' @returns a data frame
read_dp <- function(fpath) {
  df <- read.csv(fpath,
                 sep = ",",
                 colClasses = "character",
                 na.strings = "")
  df <- as.data.frame(df)
  return(df)
}

#' Given a tag, finds the first index marking the start of the tagged data
#'
#' @param dp A DemoProj data frame
#' @param tag A tag string
#' @param tagcol The column number of dp to search for the tag. Defaults to `1`.
#' @returns an integer
dp_findstartindex <- function(dp, tag, tagcol = 1) {
  i_start <- which(dp[, tagcol] == tag)

  return(i_start)
}

#'
dp_findendindex <- function(dp, tag, tagcol = 2, offset_idx = 0) {
  notna_indices <- which(!is.na(dp[, tagcol]))
  i_end <- notna_indices[which(notna_indices == which(dp[, tagcol] == tag)) + 1 + offset_idx] - 2

  return(i_end)
}

dp_findindex <- function(dp, tag, tagcol = 2) {
  i <- which(dp[, tagcol] == tag)

  return(i)
}

# given keyword (kw) for a taglist (or similar column), what is the
# index of the first instance encountered for that kw?
vec_findnextindex <- function(vec, kw) {
  i <- which(vec == kw)[1]

  return(i)
}

dp_ext <- function(dp, tag, rows, cols = 4, tagcol = 2, use_grep = FALSE) {
  if (use_grep) {
    dp[which(grepl(tag, dp[, tagcol], ignore.case = TRUE)) + rows, cols]
  } else {
    dp[which(dp[, tagcol] == tag) + rows, cols]
  }
}

convert_popdf_to_list <- function(raw_df, n_proj_cols) {
  kw_list <- c("^both", "^male", "^female")
  pop_list <- list()

  for (i in seq_len(nrow(raw_df))) {
    if (grepl("age", raw_df[i, 1], ignore.case = TRUE)) {
      age_label <- raw_df[i, 1]
      temp_df <- raw_df[i:(i+6), ]

      age_poplist <- lapply(kw_list, function(kw) {
        as.numeric(dp_ext(dp = temp_df,
                          tag = kw,
                          rows = 1,
                          cols = (2:(n_proj_cols + 1)),
                          tagcol = 1,
                          use_grep = TRUE))
      })
      names(age_poplist) <- c("Both", "Male", "Female")
      pop_list[[age_label]] <- age_poplist
    }
  }

  return(pop_list)
}

# extracts population data from dp files
dp_extpop <- function(dp, tag, tagcol = 2, disagg_var, n_proj_cols, proj_years) {
  # check if pop data has a separate urban section
  has_urban <- dp_ext(dp = dp, tag = "Use urban/rural projection", rows = 1)

  i_start <- dp_findstartindex(dp, tag)

  if (has_urban == "1") {
    i_end <- dp_findindex(dp, "Urban", tagcol = 3) - 1
  } else {
    i_end <- dp_findendindex(dp, tag, tagcol = tagcol)
  }

  raw_df <- dp[i_start:i_end, -(1:tagcol)] # subset from dp, the sub df
  raw_df <- raw_df[rowSums(is.na(raw_df)) != ncol(raw_df), ] # remove rows with all NAs across
  rownames(raw_df) <- 1:nrow(raw_df) # row indices retain the order prior to subset. need to reassign indices.

  pop_list <- convert_popdf_to_list(raw_df, n_proj_cols)

  disagg_pop <- data.frame(do.call(rbind,
                                   lapply(names(pop_list),
                                          function(age_label) pop_list[[age_label]][[disagg_var]])))

  names(disagg_pop) <- proj_years

  disagg_pop <- cbind(`start age` = as.numeric(gsub("age=", "", names(pop_list), ignore.case = TRUE)),
                      disagg_pop)
  return(disagg_pop)
}

# extracts ASFR data from dp files, 5 year age groups (8 groups)
dp_extASFR <- function(dp, tag, tagcol = 2, proj_years) {
  i_start <- dp_findstartindex(dp, tag, tagcol = tagcol)
  i_end <- dp_findendindex(dp, tag, tagcol = tagcol)

  raw_df <- dp[i_start:i_end, -(1:tagcol)] # subset from dp, the sub df

  # clean up any columns or rows that are all NAs
  raw_df <- raw_df[, colSums(is.na(raw_df)) != nrow(raw_df)] # remove NA cols
  raw_df <- raw_df[rowSums(is.na(raw_df)) != ncol(raw_df), ] # remove NA rows
  rownames(raw_df) <- 1:nrow(raw_df) # row indices retain the order prior to subset. need to reassign indices.
  raw_df <- data.frame(sapply(raw_df, as.numeric))

  if (ncol(raw_df) != length(proj_years)) {
    stop("Error: Extracted ASFR dataframe has wrong dimensions!")
  } else {
    names(raw_df) <- proj_years
    asfr_df <- cbind(`start age` = seq(10, 45, by = 5),
                     raw_df)
  }

  return(asfr_df)
}

# extracts ASFR data from dp files, 5 year age groups (8 groups)
# age groups = "single" or "five"
dp_extdeaths <- function(dp, tag, age_group, tagcol = 2, offset_idx = c(0, 0), proj_years) {
  i_start <- dp_findstartindex(dp, tag, tagcol = tagcol) + offset_idx[1]
  i_end <- dp_findendindex(dp, tag, tagcol = tagcol) + offset_idx[2]

  raw_df <- dp[i_start:i_end, -(1:tagcol)] # subset from dp, the sub df

  # clean up any columns or rows that are all NAs
  raw_df <- raw_df[, colSums(is.na(raw_df)) != nrow(raw_df)] # remove NA cols
  raw_df <- raw_df[rowSums(is.na(raw_df)) != ncol(raw_df), ] # remove NA rows
  rownames(raw_df) <- 1:nrow(raw_df) # row indices retain the order prior to subset. need to reassign indices.
  raw_df <- data.frame(sapply(raw_df, as.numeric))

  if (age_group == "five") {
    `start age` <- seq(0, 80, by = 5)
  } else if (age_group == "single") {
    `start age` <- seq(0, 80)
  }

  if (ncol(raw_df) != length(proj_years) | nrow(raw_df) != length(`start age`)) {
    stop("Error: Extracted ASFR dataframe has wrong dimensions!")
  } else {
    names(raw_df) <- proj_years
    deaths_df <- cbind(`start age`,
                       raw_df)
  }

  return(deaths_df)
}

convert_migdf_to_list <- function(raw_df, n_proj_cols) {
  mig_list <- list()

  for (i in seq_len(nrow(raw_df))) {
    if (grepl("age", raw_df[i, 1], ignore.case = TRUE)) {
      age_label <- raw_df[i, 1]
      temp_vec <- as.numeric(dp_ext(dp = raw_df,
                                    tag = age_label,
                                    rows = 1,
                                    cols = (2:(n_proj_cols + 1)),
                                    tagcol = 1,
                                    use_grep = FALSE))
      mig_list[[age_label]] <- temp_vec
    }
  }

  return(mig_list)
}

dp_extmig <- function(dp, tag, tagcol = 2, offset_idx = c(4, 1), n_proj_cols, proj_years) {
  i_start <- dp_findstartindex(dp, tag, tagcol = 2) + offset_idx[1]
  i_end <- dp_findendindex(dp, tag, offset_idx = 2) + offset_idx[2]

  raw_df <- dp[i_start:i_end, -(1:tagcol)] # subset from dp, the sub df

  # clean up any columns or rows that are all NAs
  raw_df <- raw_df[, colSums(is.na(raw_df)) != nrow(raw_df)] # remove NA cols
  raw_df <- raw_df[rowSums(is.na(raw_df)) != ncol(raw_df), ] # remove NA rows
  rownames(raw_df) <- 1:nrow(raw_df) # row indices retain the order prior to subset. need to reassign indices.

  mig_lst <- convert_migdf_to_list(raw_df, n_proj_cols)

  mig_df <- data.frame(do.call(rbind, mig_lst))

  names(mig_df) <- proj_years
  rownames(mig_df) <- 1:nrow(mig_df)

  mig_df <- cbind(`start age` = as.numeric(gsub("age=", "", names(mig_lst), ignore.case = TRUE)),
                  mig_df)

  return(mig_df)
}
