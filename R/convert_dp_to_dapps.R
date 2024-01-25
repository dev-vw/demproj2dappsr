#' Wrapper function that converts a `.dp` file into a DAPPSR projection output
#' list format.
#'
#' @param dp_fpath A file path string to the `.dp` input file
#' @returns a list of five keys: "data", "summary_measures", "age_summary", "warnings", and "error". The "data" key contains a list of six data frames for each year of the projection:
#' male and female population by age, male and female births by age of mother, male and female deaths by age, male and female migration by age,
#' male and female life tables by age. Years correspond to the mid-year. The "summary_measures" key contains a set of summary tables about the vital events of the
#' projection. The "age_summary" key contains a set of summary data by age group and by year. The "warnings" and "error" keys contain a vector of warnings encountered
#' or the error that caused the projection process to stop. In the event of an error, the "data" key should contain partial results.
#' @export
convert_dp_to_dapps <- function(dp_fpath) {
  dp <- read_dp(dp_fpath)
  offsetcol <- which(names(dp) == "Data") - 1

  year_start <- as.integer(dp_ext(dp, tag = "First year", rows = 1, tagcol = 2))
  year_end <- as.integer(dp_ext(dp, tag = "Final year", rows = 1, tagcol = 2))
  proj_years <- as.numeric(year_start:year_end) # create vector of years projected
  n_proj_cols <- length(proj_years) # number of years projected

  input_data <- "Spectrum"
  warnings <- list()
  error <- list()

  # ----- summary measures
  # List 1.1: Vital Events
  Vital_Events <- data.frame(
    year = proj_years,
    total_pop = NA,
    total_births = as.numeric(dp_ext(dp, tag = "Births", rows = 1, cols = (1:n_proj_cols) + offsetcol)),
    total_deaths = NA,
    total_net_migration = NA
  )

  names(Vital_Events) <- c("Year", "Total Population", "Total Births", "Total Deaths", "Total Net Migration")

  # List 1.2: Vital Events by Sex
  Vital_Events_by_Sex <- data.frame(
    year = proj_years,
    total_male_pop = NA,
    total_female_pop = NA,
    total_male_births = NA,
    total_female_births = NA,
    total_male_deaths = as.numeric(dp_ext(dp, tag = "Deaths - Male", rows = 1, cols = (1:n_proj_cols) + offsetcol)),
    total_female_deaths = as.numeric(dp_ext(dp, tag = "Deaths - Female", rows = 1, cols = (1:n_proj_cols) + offsetcol)),
    total_mig_m = as.numeric(dp_ext(dp, tag = "Male Migration", rows = 2, cols = (1:n_proj_cols) + offsetcol)),
    total_mig_f = as.numeric(dp_ext(dp, tag = "Female Migration", rows = 2, cols = (1:n_proj_cols) + offsetcol)),
    median_age_m = NA,
    median_age_f = NA
  )

  names(Vital_Events_by_Sex) <- c("Year",
                                  "Total Male Population",
                                  "Total Female Population",
                                  "Total Male Births",
                                  "Total Female Births",
                                  "Total Male Deaths",
                                  "Total Female Deaths",
                                  "Total Net Male Migration",
                                  "Total Net Female Migration",
                                  "Male Median Age",
                                  "Female Median Age")

  # List 1.3: Vital Rates
  Vital_Rates <- data.frame(
    year = proj_years,
    cbr = NA,
    cdr = NA,
    rni = NA,
    net_migration_rate = NA,
    annual_growth_rate = NA,
    grr = NA
  )

  names(Vital_Rates) <- c("Year",
                          "CBR",
                          "CDR",
                          "RNI",
                          "Net Migration Rate",
                          "Annual Growth Rate",
                          "GRR")

  # List 1.4: Fertility Summary
  Fertility_Summary <- data.frame(
    year = proj_years,
    asfr_1014 = as.numeric(dp_ext(dp, tag = "Age Specific Fertility Distribution", rows = 1, cols = (1:n_proj_cols) + offsetcol)),
    asfr_1519 = as.numeric(dp_ext(dp, tag = "Age Specific Fertility Distribution", rows = 2, cols = (1:n_proj_cols) + offsetcol)),
    asfr_2024 = as.numeric(dp_ext(dp, tag = "Age Specific Fertility Distribution", rows = 3, cols = (1:n_proj_cols) + offsetcol)),
    asfr_2529 = as.numeric(dp_ext(dp, tag = "Age Specific Fertility Distribution", rows = 4, cols = (1:n_proj_cols) + offsetcol)),
    asfr_3034 = as.numeric(dp_ext(dp, tag = "Age Specific Fertility Distribution", rows = 5, cols = (1:n_proj_cols) + offsetcol)),
    asfr_3539 = as.numeric(dp_ext(dp, tag = "Age Specific Fertility Distribution", rows = 6, cols = (1:n_proj_cols) + offsetcol)),
    asfr_4044 = as.numeric(dp_ext(dp, tag = "Age Specific Fertility Distribution", rows = 7, cols = (1:n_proj_cols) + offsetcol)),
    asfr_4549 = as.numeric(dp_ext(dp, tag = "Age Specific Fertility Distribution", rows = 8, cols = (1:n_proj_cols) + offsetcol)),
    tfr = as.numeric(dp_ext(dp, tag = "TFR", rows = 1, cols = (1:n_proj_cols) + offsetcol)),
    sxrb = NA
  )

  names(Fertility_Summary) <- c("Year",
                                "ASFR 10-14",
                                "ASFR 15-19",
                                "ASFR 20-14",
                                "ASFR 25-29",
                                "ASFR 30-34",
                                "ASFR 35-39",
                                "ASFR 40-44",
                                "ASFR 45-49",
                                "TFR",
                                "SXRB")

  # List 1.5: Mortality Summary
  # e0 = life expectancy at birth
  Mortality_Summary <- data.frame(
    year = proj_years,
    m_e0 = as.numeric(dp_ext(dp, tag = "LE - Male - Input Assumption", rows = 1, cols = (1:n_proj_cols) + offsetcol)),
    f_e0 = as.numeric(dp_ext(dp, tag = "LE - Female - Input Assumption", rows = 1, cols = (1:n_proj_cols) + offsetcol)),
    m_imr = as.numeric(dp_ext(dp, tag = "IMR - Male", rows = 1, cols = (1:n_proj_cols) + offsetcol)),
    f_imr = as.numeric(dp_ext(dp, tag = "IMR - Female", rows = 1, cols = (1:n_proj_cols) + offsetcol)),
    m_cmr = NA,
    f_cmr = NA,
    m_u5mr = as.numeric(dp_ext(dp, tag = "U5MR - Male", rows = 1, cols = (1:n_proj_cols) + offsetcol)),
    f_u5mr = as.numeric(dp_ext(dp, tag = "U5MR - Female", rows = 1, cols = (1:n_proj_cols) + offsetcol)),
    m_id = NA,
    f_id = NA
  )

  names(Mortality_Summary) <- c("Year",
                                "Male e0",
                                "Female e0",
                                "Male IMR",
                                "Female IMR",
                                "Male CMR",
                                "Female CMR",
                                "Male U5MR",
                                "Female U5MR",
                                "Male Infant Deaths",
                                "Female Infant Deaths")

  # List 1.6: Mortality Summary
  Migration_Summary <- data.frame(
    year = proj_years,
    total_mig_m = as.numeric(dp_ext(dp, tag = "Male Migration", rows = 2, cols = (1:n_proj_cols) + offsetcol)),
    total_mig_f = as.numeric(dp_ext(dp, tag = "Female Migration", rows = 2, cols = (1:n_proj_cols) + offsetcol))
  )

  names(Migration_Summary) <- c("Year",
                                "Male_Total_NetMigration_stream_1",
                                "Female_Total_NetMigration_stream_1")

  # List 1.7: Growth Rate
  Growth_Rate <- data.frame(
    year = proj_years,
    annual_growth_rate = NA
  )

  names(Growth_Rate) <- c("Year", "Annual Growth Rate")

  # List 1.7: Rate Components of Change
  Rate_Components_of_Change <- data.frame(
    year = proj_years,
    cbr = NA,
    cdr = NA,
    net_migration_rate = NA
  )

  names(Rate_Components_of_Change) = c("Year",
                                       "CBR",
                                       "CDR",
                                       "Net Migration Rate")

  # List 1.8: Event Components of Change
  Event_Components_of_Change <- data.frame(
    year = proj_years,
    total_births = as.numeric(dp_ext(dp, tag = "Births", rows = 1, cols = (1:n_proj_cols) + offsetcol)),
    total_deaths = NA,
    total_net_migration = NA
  )

  names(Event_Components_of_Change) <- c("Year",
                                         "Total Births",
                                         "Total Deaths",
                                         "Total Net Migration")

  # List 1.9: Median Age
  Median_Age <- data.frame(
    year = proj_years,
    m_median_age = NA,
    f_median_age = NA
  )

  names(Median_Age) <- c("Year", "Male Median Age", "Female Median Age")

  # List 1.10: TFR
  TFR <- data.frame(
    year = proj_years,
    tfr = as.numeric(dp_ext(dp, tag = "TFR", rows = 1, cols = (1:n_proj_cols) + offsetcol))
  )

  names(TFR) = c("Year", "TFR")

  # List 1.11: SXRB
  SXRB <- data.frame(
    year = proj_years,
    sxrb = NA
  )

  names(SXRB) = c("Year", "SXRB")

  # List 1.12: Life Expectancy at Birth
  Life_Expectancy_at_Birth <- data.frame(
    year = proj_years,
    m_e0 = as.numeric(dp_ext(dp, tag = "LE - Male - Input Assumption", rows = 1, cols = (1:n_proj_cols) + offsetcol)),
    f_e0 = as.numeric(dp_ext(dp, tag = "LE - Female - Input Assumption", rows = 1, cols = (1:n_proj_cols) + offsetcol))
  )

  names(Life_Expectancy_at_Birth) <- c("Year", "Male e0", "Female e0")

  # List 1.13: IMR
  IMR <- data.frame(
    year = proj_years,
    m_imr = as.numeric(dp_ext(dp, tag = "IMR - Male", rows = 1, cols = (1:n_proj_cols) + offsetcol)),
    f_imr = as.numeric(dp_ext(dp, tag = "IMR - Female", rows = 1, cols = (1:n_proj_cols) + offsetcol))
  )

  names(IMR) <- c("Year", "Male IMR", "Female IMR")

  # List 1.14: CMR
  CMR <- data.frame(
    year = proj_years,
    m_cmr = NA,
    f_cmr = NA
  )

  names(CMR) <- c("Year", "Male CMR", "Female CMR")

  # List 1.15: U5MR
  U5MR <- data.frame(
    year = proj_years,
    m_u5mr = as.numeric(dp_ext(dp, tag = "U5MR - Male", rows = 1, cols = (1:n_proj_cols) + offsetcol)),
    f_u5mr = as.numeric(dp_ext(dp, tag = "U5MR - Female", rows = 1, cols = (1:n_proj_cols) + offsetcol))
  )

  names(U5MR) <- c("Year", "Male U5MR", "Female U5MR")

  summary_tables <- list(Vital_Events = Vital_Events,
                         Vital_Events_by_Sex = Vital_Events_by_Sex,
                         Vital_Rates = Vital_Rates,
                         Fertility_Summary = Fertility_Summary,
                         Mortality_Summary = Mortality_Summary,
                         Migration_Summary = Migration_Summary,
                         Growth_Rate = Growth_Rate,
                         Rate_Components_of_Change = Rate_Components_of_Change,
                         Event_Components_of_Change = Event_Components_of_Change,
                         Median_Age = Median_Age,
                         TFR = TFR,
                         SXRB = SXRB,
                         Life_Expectancy_at_Birth = Life_Expectancy_at_Birth,
                         IMR = IMR,
                         CMR = CMR,
                         U5MR = U5MR)

  # ----- detailed tables by year

  # population
  Population_Single_Year_Ages_by_Year_for_Male <- NA
  Population_Single_Year_Ages_by_Year_for_Female <- NA
  Population_Five_Year_Ages_by_Year_for_Male <- dp_extpop(dp = dp, tag = '<TotPop2>', disagg_var = "Male", n_proj_cols = n_proj_cols, proj_years = proj_years, offset_idx = 10)
  Population_Five_Year_Ages_by_Year_for_Female <- dp_extpop(dp = dp, tag = '<TotPop2>', disagg_var = "Female", n_proj_cols = n_proj_cols, proj_years = proj_years, offset_idx = 10)
  Population_Special_Ages_by_Year_for_Male <- NA
  Population_Special_Ages_by_Year_for_Female <- NA

  # ASFR
  ASFR_Single_Year_Ages_by_Year <- NA
  ASFR_Five_Year_Ages_by_Year <- dp_extASFR(dp, "Age Specific Fertility Distribution", tagcol = 2, proj_years = proj_years)

  # Deaths
  Deaths_Single_Year_Ages_by_Year_for_Male <- dp_extdeaths(dp, "Single Age Deaths Male", age_group = "single", offset_idx = c(1, 1), proj_years = proj_years)
  Deaths_Single_Year_Ages_by_Year_for_Female <- dp_extdeaths(dp, "Single Age Deaths FeMale", age_group = "single", offset_idx = c(1, 1), proj_years = proj_years)
  Deaths_Five_Year_Ages_by_Year_for_Male <- dp_extdeaths(dp, "Deaths - Male", age_group = "five", offset_idx = c(2, 1), proj_years = proj_years)
  Deaths_Five_Year_Ages_by_Year_for_Female <- dp_extdeaths(dp, "Deaths - Female", age_group = "five", offset_idx = c(2, 1), proj_years = proj_years)
  Deaths_Special_Ages_by_Year_for_Male <- NA
  Deaths_Special_Ages_by_Year_for_Female <- NA
  nMx_Male_Single_Year_Ages_by_Year <- NA
  nMx_Female_Single_Year_Ages_by_Year <- NA
  nMx_Male_Abridged_by_Year <- NA
  nMx_Female_Abridged_by_Year <- NA

  # Migration
  Migration_Totals_Single_Year_Ages_by_Year_for_Male <- NA
  Migration_Totals_Single_Year_Ages_by_Year_for_Female <- NA
  Migration_Totals_Five_Year_Ages_by_Year_for_Male <- dp_extmig(dp, "Male Migration", n_proj_cols = n_proj_cols, proj_years = proj_years)
  Migration_Totals_Five_Year_Ages_by_Year_for_Female <- dp_extmig(dp, "Female Migration", n_proj_cols = n_proj_cols, proj_years = proj_years)
  Migration_Totals_Special_Ages_by_Year_for_Male <- NA
  Migration_Totals_Special_Ages_by_Year_for_Female <- NA
  Migration_stream_1_Single_Year_Ages_by_Year_for_Male <- NA
  Migration_stream_1_Single_Year_Ages_by_Year_for_Female <- NA
  Migration_stream_1_Five_Year_Ages_by_Year_for_Male <- NA
  Migration_stream_1_Five_Year_Ages_by_Year_for_Female <- NA
  Migration_stream_1_Special_Ages_by_Year_for_Male <- NA
  Migration_stream_1_Special_Ages_by_Year_for_Female <- NA

  split_age_summary_tables <- list(Population_Single_Year_Ages_by_Year_for_Male = Population_Single_Year_Ages_by_Year_for_Male,
                                   Population_Single_Year_Ages_by_Year_for_Female = Population_Single_Year_Ages_by_Year_for_Female,
                                   Population_Five_Year_Ages_by_Year_for_Male = Population_Five_Year_Ages_by_Year_for_Male,
                                   Population_Five_Year_Ages_by_Year_for_Female = Population_Five_Year_Ages_by_Year_for_Female,
                                   Population_Special_Ages_by_Year_for_Male = Population_Special_Ages_by_Year_for_Male,
                                   Population_Special_Ages_by_Year_for_Male = Population_Special_Ages_by_Year_for_Male,
                                   ASFR_Single_Year_Ages_by_Year = ASFR_Single_Year_Ages_by_Year,
                                   ASFR_Five_Year_Ages_by_Year = ASFR_Five_Year_Ages_by_Year,
                                   Deaths_Single_Year_Ages_by_Year_for_Male = Deaths_Single_Year_Ages_by_Year_for_Male,
                                   Deaths_Single_Year_Ages_by_Year_for_Female = Deaths_Single_Year_Ages_by_Year_for_Female,
                                   Deaths_Special_Ages_by_Year_for_Male = Deaths_Special_Ages_by_Year_for_Male,
                                   Deaths_Special_Ages_by_Year_for_Female = Deaths_Special_Ages_by_Year_for_Female,
                                   nMx_Male_Single_Year_Ages_by_Year = nMx_Male_Single_Year_Ages_by_Year,
                                   nMx_Female_Single_Year_Ages_by_Year = nMx_Female_Single_Year_Ages_by_Year,
                                   nMx_Male_Abridged_by_Year = nMx_Male_Abridged_by_Year,
                                   nMx_Female_Abridged_by_Year = nMx_Female_Abridged_by_Year,
                                   Migration_Totals_Single_Year_Ages_by_Year_for_Male = Migration_Totals_Single_Year_Ages_by_Year_for_Male,
                                   Migration_Totals_Single_Year_Ages_by_Year_for_Female = Migration_Totals_Single_Year_Ages_by_Year_for_Female,
                                   Migration_Totals_Five_Year_Ages_by_Year_for_Male = Migration_Totals_Five_Year_Ages_by_Year_for_Male,
                                   Migration_Totals_Five_Year_Ages_by_Year_for_Female = Migration_Totals_Five_Year_Ages_by_Year_for_Female,
                                   Migration_Totals_Special_Ages_by_Year_for_Male = Migration_Totals_Special_Ages_by_Year_for_Male,
                                   Migration_Totals_Special_Ages_by_Year_for_Female = Migration_Totals_Special_Ages_by_Year_for_Female,
                                   Migration_stream_1_Single_Year_Ages_by_Year_for_Male = Migration_stream_1_Single_Year_Ages_by_Year_for_Male,
                                   Migration_stream_1_Single_Year_Ages_by_Year_for_Female = Migration_stream_1_Single_Year_Ages_by_Year_for_Female,
                                   Migration_stream_1_Five_Year_Ages_by_Year_for_Male = Migration_stream_1_Five_Year_Ages_by_Year_for_Male,
                                   Migration_stream_1_Five_Year_Ages_by_Year_for_Female = Migration_stream_1_Five_Year_Ages_by_Year_for_Female,
                                   Migration_stream_1_Special_Ages_by_Year_for_Male = Migration_stream_1_Special_Ages_by_Year_for_Male,
                                   Migration_stream_1_Special_Ages_by_Year_for_Female = Migration_stream_1_Special_Ages_by_Year_for_Female)

  # ----- detailed tables by age
  age_table_lst <- list()

  for (year in proj_years) {
    year_label <- as.character(year)

    # Population
    pop_m <- subset(Population_Five_Year_Ages_by_Year_for_Male,
                    select = c("start age", year_label))
    pop_f <- subset(Population_Five_Year_Ages_by_Year_for_Female,
                    select = c("start age", year_label))
    pop_tot <- merge(pop_m, pop_f, by = "start age")
    names(pop_tot) <- c("start age", "male", "female")

    # Births
    births_tot <- NA

    # Deaths
    deaths_m <- subset(Deaths_Single_Year_Ages_by_Year_for_Male,
                       select = c("start age", year_label))
    deaths_f <- subset(Deaths_Single_Year_Ages_by_Year_for_Female,
                       select = c("start age", year_label))
    deaths_tot <- merge(deaths_m, deaths_f, by = "start age")
    names(deaths_tot) <- c("start age", "male", "female")

    # Life Tables
    lt_m <- NA
    lt_f <- NA

    # Migration Totals
    mig_m <- subset(Migration_Totals_Five_Year_Ages_by_Year_for_Male,
                    select = c("start age", year_label))
    mig_f <- subset(Migration_Totals_Five_Year_Ages_by_Year_for_Female,
                    select = c("start age", year_label))
    mig_tot <- merge(mig_m, mig_f, by = "start age")
    names(mig_tot) <- c("start age", "male", "female")

    # Migration Stream 1
    migstream_tot <- NA

    age_tables <- list(
      Population = pop_tot,
      Births = births_tot,
      Deaths = deaths_tot,
      Male_Life_Table = lt_m,
      Female_Life_Table = lt_f,
      Migration_Totals = mig_tot,
      Migration_stream_1 = migstream_tot
    )

    age_table_lst[[year_label]] <- age_tables
  }

  # ----- prepare the output
  output <- list(summary_measures = summary_tables,
                 detailed_tables_by_year = split_age_summary_tables,
                 output_data = age_table_lst,
                 input_data = input_data,
                 warnings = warnings,
                 error = error)

  return(output)
}
