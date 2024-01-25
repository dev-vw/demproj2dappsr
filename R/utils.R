#' @description Determines if a tag exists within the DemProj data.
#'
#' @author Vania Wang
#'
#' @param dp A DemProj data frame.
#' @param tag A tag string.
#' @param tagcol The column number (or columns) of the DemProj data frame to search for the tag. Defaults to `c(1:2)`.
#' @returns A logical.
#' @noRd
exists_label_or_tag <- function(dp, tag, tagcol = c(1:2)) {
  any(dp[, tagcol] == tag)
}

#' @title read_dp
#' @description Read in .dp or .pjn files as data frames
#'
#' @author Vania Wang
#'
#' @importFrom utils read.csv
#' @param fpath A file path string to the `.dp` input file.
#' @returns A data frame.
#' @export
read_dp <- function(fpath) {
  df <- read.csv(fpath,
                 sep = ",",
                 colClasses = "character",
                 na.strings = "")
  df <- as.data.frame(df)

  return(df)
}

#' @description Given a tag, finds the first index marking the start of the tagged data.
#'
#' @author Vania Wang
#'
#' @param dp A DemProj data frame.
#' @param tag A tag string.
#' @param tagcol The column number of dp to search for the tag. Defaults to `1`.
#' @returns An integer.
#' @noRd
dp_findstartindex <- function(dp, tag, tagcol = 1) {
  i_start <- which(dp[, tagcol] == tag)

  return(i_start)
}

#' @description Given a tag, finds the index marking the end of the tagged data.
#'
#' @author Vania Wang
#'
#' @param dp A DemProj data frame.
#' @param tag A tag string.
#' @param tagcol The column number of dp to search for the tag. Defaults to `2`.
#' @param offset_idx An integer indicating an offset of the returned index.
#' @returns An integer.
#' @noRd
dp_findendindex <- function(dp, tag, tagcol = 2, offset_idx = 0) {
  notna_indices <- which(!is.na(dp[, tagcol]))
  i_end <- notna_indices[which(notna_indices == which(dp[, tagcol] == tag)) + 1 + offset_idx] - 2

  return(i_end)
}

#' @description Given a tag, finds the index marking the position of the input tag
#' @author Vania Wang
#'
#' @param dp A DemProj data frame.
#' @param tag A tag string.
#' @param tagcol The column number of dp to search for the tag. Defaults to `2`.
#' @returns An integer.
#' @noRd
dp_findindex <- function(dp, tag, tagcol = 2) {
  i <- which(dp[, tagcol] == tag)

  return(i)
}

#' @description Given keyword (kw) for a taglist (or similar column), finds is next
#' index after the first instance for the input kw.
#'
#' @author Vania Wang
#'
#' @param vec A vector of keywords.
#' @param kw A keyword string.
#' @returns An integer.
#' @noRd
vec_findnextindex <- function(vec, kw) {
  i <- which(vec == kw)[1]

  return(i)
}

#' @description Takes a `dp` data frame and returns a subset data frame according to
#' params.
#'
#' @author Vania Wang
#'
#' @param dp A DemProj data frame.
#' @param tag A tag string.
#' @param rows An integer for the number of rows to extract.
#' @param cols A numeric or vector to indicate number of columns to extract Defaults to `4`.
#' @param tagcol A numeric or vector to indicate column or columns to query for tags. Defaults to `2`.
#' @param use_grep Defaults to `FALSE`.
#' @noRd
dp_ext <- function(dp, tag, rows, cols = 4, tagcol = 2, use_grep = FALSE) {
  if (use_grep) {
    dp[which(grepl(tag, dp[, tagcol], ignore.case = TRUE)) + rows, cols]
  } else {
    dp[which(dp[, tagcol] == tag) + rows, cols]
  }
}


#' @description Takes a subset of a DemProj data frame and returns a list of population vectors
#' indexed by year and keyword.
#'
#' @author Vania Wang
#'
#' @param raw_df Subset of a DemProj data frame.
#' @param n_proj_cols The number of projection years.
#' @returns a hierarchical list
#' @noRd
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

#' @description Subsets population data from DemProj data frames, given a population tag.
#'
#' @author Vania Wang
#'
#' @param dp A DemProj data frame.
#' @param tag A tag string.
#' @param tagcol A numeric or vector to indicate column or columns to query for tags. Defaults to `2`.
#' @param disagg_var A vector containing strings that indicate disaggregation vars (most likely, gender)
#' @param n_proj_cols The number of projection years.
#' @param proj_years A vector containing the projection years.
#' @param offset_idx Integer indicating the offset for indexing main dp data frame. 
#' Defaults to `0`.
#' @returns A hierarchical list
#' @noRd
# extracts population data from dp files
dp_extpop <- function(dp, tag, tagcol = 2, disagg_var, n_proj_cols, proj_years, offset_idx = 0) {
  # check if pop data has a separate urban section
  has_urban <- dp_ext(dp = dp, tag = "Use urban/rural projection", rows = 1)
  
  i_start <- dp_findstartindex(dp, tag) + offset_idx
  
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
  
  # five-year age groups, hard-coded for now
  disagg_pop <- cbind(`start age` = as.numeric(gsub("age=", "", names(pop_list), ignore.case = TRUE)) * 5 - 5,
                      disagg_pop)
  
  return(disagg_pop)
}

#' @description Subsets ASFR data from DemProj data frames, given a ASFR tag.
#' Assumes 8 standardized 5-year age groups. See reference for age-group definitions.
#'
#' @author Vania Wang
#'
#' @param dp A DemProj data frame.
#' @param tag A tag string.
#' @param tagcol A numeric or vector to indicate column or columns to query for tags. Defaults to `2`.
#' @returns a data frame
#' @noRd
#'
#' @seealso \href{https://data.unaids.org/topics/epidemiology/manuals/demproj_manual_v_4_en.pdf}{DemProj Version 4: A Computer Program for Making Population Projections}
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

#' @description Extracts death data from DemProj data frames, given a death tag.
#' Assumes 8 standardized 5-year age groups OR single age groups (0-80).
#'
#' @author Vania Wang
#'
#' @param dp A DemProj data frame.
#' @param tag A tag string.
#' @param age_group Either "single" or "five", to indicate single or five-year age groups.
#' @param tagcol A numeric or vector to indicate column or columns to query for tags. Defaults to `2`.
#' @param offset_idx A vector of length two. First number is the offset for the start index while
#' the second number if the offset for the end index. Defaults to `c(0, 0)`.
#' @param proj_years A vector containing the projection years.
#' @returns a data frame
#' @noRd
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

#' @description Takes a subset of a DemProj data frame and returns a list of migration vectors
#' indexed by year and keyword.
#'
#' @author Vania Wang
#'
#' @param raw_df Subset of a DemProj data frame.
#' @param n_proj_cols The number of projection years.
#' @returns a hierarchical list
#' @noRd
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

#' @description Extracts migration data from DemProj data frames, given a death tag.
#' Assumes single age groups (0-17).
#'
#' @author Vania Wang
#'
#' @param dp A DemProj data frame.
#' @param tag A tag string.
#' @param tagcol A numeric or vector to indicate column or columns to query for tags. Defaults to `2`.
#' @param offset_idx A vector of length two. First number is the offset for the start index while
#' the second number if the offset for the end index. Defaults to `c(4, 1)`.
#' @param n_proj_cols The number of projection years.
#' @param proj_years A vector containing the projection years.
#' @returns a data frame
#' @noRd
dp_extmig <- function(dp, tag, tagcol = 2, offset_idx = c(6, 1), n_proj_cols, proj_years) {
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
  
  mig_df <- cbind(`start age` = as.numeric(gsub("age=", "", names(mig_lst), ignore.case = TRUE)) * 5 - 5,
                  mig_df)
  
  return(mig_df)
}
