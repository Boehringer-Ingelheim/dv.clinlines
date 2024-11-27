data_list <- add_ids(prep_dummy_data())
data_list$adcm$CMSTDTC[1] <- NA # Introduce a missing start date in CM dataset

basic_list <- set_basics(
  data_list = data_list,
  subjid_var = "USUBJID"
)

prepped_events <- set_events_intern(
  list(adsl = data_list$adsl, adae = data_list$adae, adcm = data_list$adcm),
  subjid_var = "USUBJID"
)
prepped_exp_intervals <- set_exp_intervals(
  list(exp = data_list$exp),
  subjid_var = "USUBJID"
)

combined_data <- combine_data(
  df_list = list(prepped_events, prepped_exp_intervals),
  basic_info_df = basic_list$data,
  trtstart = "TRTSDT",
  trtend = "TRTEDT",
  icf_date = "RFICDT",
  subjid_var = "subject_id"
)


# Tests for add_ids() ----
test_that("add_ids() returns a named list", {
  expect_named(data_list, c("adsl", "adae", "adcm", "exp", "exp2"), ignore.order = TRUE)
  expect_type(data_list, "list")
})

test_that("add_ids() adds an ID column to each data frame of data_list", {
  purrr::walk(data_list, function(x) expect_true("set_id" %in% names(x)))
})

test_that("add_ids() uses unique IDs for each data frame", {
  purrr::walk(data_list, function(x) expect_length(x$set_id, length(unique(x$set_id))))
})


# Tests for prep_data() ----
prepped_df <- prep_data(data_list = data_list)

test_that(
  "prep_data() returns a data frame containing data from all events defined in the mapping parameter" %>%
    vdoc[["add_spec"]](specs$plot_specs$events),
  {
    prepped_w_filter <- prep_data(
      data_list = data_list,
      filter = list(
        ae_filter = list(
          dataset_name = "adae",
          label = "Adverse Events",
          soc_var = "AESOC",
          serious_ae_var = "AESER"
        )
      )
    )
    expected <- c(
      "subject_id", "set_id", "group", "set", "start_dt_var", "end_dt_var", "detail_var",
      "TRTSDT", "TRTEDT", "arrow_right", "earliest", "start_dy_var", "end_dy_var",
      "resetted", "trt_var", "start_exp", "end_exp", "start_exp_day", "end_exp_day",
      "exp_dose", "start_missing", "end_missing", "arrow_left", "date_min", "day_min"
    )
    expected_w_filter <- c(expected, "AESER", "AESOC", "AEDECOD", "AESTDTC", "AEENDTC")

    expect_named(prepped_df, expected, ignore.order = TRUE)
    expect_named(prepped_w_filter, expected_w_filter, ignore.order = TRUE)
    expect_true("data.frame" %in% class(prepped_df))

    # test that all events were included
    map_sets <- names(default_mapping())
    prepped_sets <- unique(prepped_df$set)
    expect_true(all(map_sets %in% prepped_sets))

    # test that all labels are available
    labels <- purrr::map_dfr(default_mapping(), ~ data.frame(labels = names(.x)))
    expect <- data.frame(labels = c(
      "Treatment Start", "Treatment End", "Informed Consent", "Adverse Events", "Concomitant Medications"
    ))
    expect_equal(labels, expect, ignore_attr = TRUE)

    # test that drug_admin placeholder is available
    prep <- prep_data(data_list, drug_admin = NULL)
    expect_false("exp" %in% names(prep))
  }
)

test_that("should return date columns as type date", {
  expect_equal(class(prepped_df$start_dt_var), c("POSIXct", "POSIXt"))
  expect_equal(class(prepped_df$end_dt_var), c("POSIXct", "POSIXt"))
})

test_that("prep_data() throws an error if local filter parameters are not set properly", {
  expect_error(prep_data(data_list = data_list, filter = list("filter_soc")))
})

test_that(
  "prep_data() throws an error if specified data is not available" %>%
    vdoc[["add_spec"]](specs$app_creation_specs$errors_def),
  {
    wrong <- default_mapping()
    names(wrong) <- c("adsl", "adae", "adcn")

    expect_error(prep_data(data_list = data_list, mapping = wrong))
  }
)

test_that(
  "prep_data() does not drop data values related to AE local filters for ongoing data during data preparation steps" %>%
    vdoc[["add_spec"]](specs$sidebar_specs$event_type_filter),
  {
    prepped_w_filter <- prep_data(
      data_list = data_list,
      filter = list(
        ae_filter = list(
          dataset_name = "adae",
          label = "Adverse Events",
          soc_var = "AESOC",
          serious_ae_var = "AESER"
        )
      )
    )

    expected <- data_list$adae %>%
      dplyr::filter(!is.na(TRTSDT)) %>%
      dplyr::select(dplyr::all_of(c(subject_id = "USUBJID", "AESOC", "AESER")))
    to_check <- dplyr::filter(prepped_w_filter, group == "Adverse Events") %>%
      dplyr::select(dplyr::all_of(c("subject_id", "AESOC", "AESER")))

    expect_equal(to_check, expected, ignore_attr = TRUE)
  }
)


# Tests for set_basics() ----
test_that("set_basics() returns a list with fixed names", {
  df <- prep_dummy_data(10)$adsl
  basic_list <- set_basics(
    data_list = list(adsl = df),
    basic_info = list(
      subject_level_dataset_name = "adsl",
      trt_start_var = "TRTSDT",
      trt_end_var = "TRTEDT",
      icf_date_var = "RFICDT"
    ),
    subjid_var = "USUBJID"
  )

  expected <- c("data", "trt_start", "trt_end", "icf_date")
  expect_named(basic_list, expected, ignore.order = TRUE)
  expect_type(basic_list, "list")
})


# Tests for set_exp_intervals() ----
test_that("set_exo_intervals() returns a data.frame with fixed column names", {
  df_exp <- set_exp_intervals(data_list, subjid_var = "USUBJID")

  expect_true("data.frame" %in% class(df_exp))
  expect_named(df_exp, c(
      "subject_id",
      "start_exp",
      "end_exp",
      "set_id",
      "exp_dose",
      "detail_var",
      "trt_var",
      "group",
      "set"
  ))
})


# Tests for set_events_intern() ----
test_that("set_events_intern() names output columns correctly", {
  expected <- c(
    "subject_id", "start_dt_var", "end_dt_var", "detail_var", "set_id", "group",
    "set", "arrow_right", "arrow_left", "start_missing", "end_missing"
  )
  expect_named(prepped_events, expected, ignore.order = TRUE)
  expect_true("data.frame" %in% class(prepped_events))
})

test_that("set_events_intern() returns date columns as type date", {
  expect_equal(class(prepped_events$start_dt_var), c("POSIXct", "POSIXt"))
  expect_equal(class(prepped_events$end_dt_var), c("POSIXct", "POSIXt"))
})

test_that("set_events_intern() adds flags for arrows and missing values", {
  df <- prepped_events

  # flags should be boolean
  expect_type(df$arrow_left, "logical")
  expect_type(df$arrow_right, "logical")
  expect_type(df$start_missing, "logical")
  expect_type(df$end_missing, "logical")

  # arrow_left should be TRUE, when start was missing
  expect_equal(df$arrow_left, df$start_missing)

  # arrow_right should be TRUE, when end was missing
  expect_equal(df$arrow_left, df$start_missing)
})


# Tests for combine_data() ----
test_that("combine_data() names output columns correctly", {
  expected <- c(
    "subject_id", "group", "trt_var", "start_dt_var", "end_dt_var", "detail_var",
    "TRTSDT", "TRTEDT", "earliest", "set", "set_id", "start_exp", "end_exp",
    "exp_dose", "arrow_right", "start_missing", "end_missing", "arrow_left"
  )
  expect_named(combined_data, expected, ignore.order = TRUE)
})

test_that("combine_data() includes all rows of all input datasets", {
  expect_equal(nrow(combined_data), nrow(prepped_events) + nrow(prepped_exp_intervals))
})


# Tests for complete_events() ----
adapted_data <- complete_events(combined_data, trt_start = "TRTSDT", trt_end = "TRTEDT")

test_that("complete_events() returns date columns as type date", {
  expect_equal(class(adapted_data$start_dt_var), c("POSIXct", "POSIXt"))
  expect_equal(class(adapted_data$end_dt_var), c("POSIXct", "POSIXt"))
})

test_that("complete_events() names output columns correctly", {
  expected <- c(
    "subject_id", "group", "trt_var", "start_dt_var", "end_dt_var", "detail_var",
    "TRTSDT", "TRTEDT", "earliest", "set", "set_id", "arrow_right",
    "start_dy_var", "end_dy_var", "start_exp", "end_exp", "resetted",
    "start_exp_day", "end_exp_day", "exp_dose", "start_missing", "end_missing", "arrow_left"
  )
  expect_named(adapted_data, expected, ignore.order = TRUE)
  expect_true("data.frame" %in% class(adapted_data))
})

test_that(
  "complete_events() replaces missing start values by informed consent dates",
  {
    data <- combined_data %>% dplyr::filter(is.na(start_dt_var), !is.na(earliest), set == "adcm")
    out <- complete_events(data, trt_start = "TRTSDT", trt_end = "TRTEDT")

    expect_equal(out$start_dt_var, out$earliest)
  }
)

test_that("complete_events() replaces missing end dates", {
  data <- combined_data %>%
    dplyr::filter(is.na(end_dt_var), set == "adae") %>%
    dplyr::slice(1:20) %>%
    dplyr::mutate(TRTEDT = dplyr::if_else(subject_id == "01-701-1111", as.POSIXct(NA_real_), TRTEDT))
  out <- complete_events(data, trt_start = "TRTSDT", trt_end = "TRTEDT")
  expected <- data$TRTEDT
  expected[14:16] <- Sys.time()

  # Cut off seconds (too precise) and convert date to unix timestamp (just in case of turn of the day/year)
  res <- out$end_dt_var
  lubridate::second(res) <- 0
  lubridate::second(expected) <- 0

  expect_equal(res, expected, ignore_attr = TRUE)
})

test_that("complete_events() calculates study relative dates, if missing", {
  data <- combined_data %>%
    dplyr::filter(set %in% c("adsl", "adae")) %>%
    dplyr::slice_sample(n = 20)
  out <- complete_events(data, trt_start = "TRTSDT", trt_end = "TRTEDT")
  expect_true(any(c("start_dy_var", "end_dy_var") %in% names(out)))

  # check that they will not be touched in case they are available
  data <- combined_data %>%
    dplyr::filter(set %in% c("adsl", "adae"), !is.na(TRTSDT)) %>%
    dplyr::slice_sample(n = 20) %>%
    dplyr::mutate(start_dy_var = as.double(seq_len(nrow(.))), end_dy_var = as.double(rev(seq_len(nrow(.)))))
  out <- complete_events(data, trt_start = "TRTSDT", trt_end = "TRTEDT")

  expect_equal(out$start_dy_var, data$start_dy_var)
  expect_equal(out$end_dy_var, data$end_dy_var)
})


# Tests for set_filter_dataset() ----
test_that(
  "set_filter_dataset() selects columns according to the user defined parameters",
  {
    filter_data <- set_filter_dataset(
      filter = list(
        ae_filter = list(
          dataset_name = "adae",
          label = "Adverse Events",
          soc_var = "AESOC",
          serious_ae_var = "AESER",
          pref_term_var = "AEDECOD",
          drug_rel_ae_var = "AEREL"
        )
      ),
      data_list = data_list,
      mapping = list(
        adsl = list(
          "Treatment Start" = list(
            start_dt_var = "TRTSDT",
            end_dt_var = NULL,
            start_dy_var = NULL,
            end_dy_var = NULL,
            detail_var = NULL
          ),
          "Treatment End" = list(
            start_dt_var = "TRTEDT",
            end_dt_var = NULL,
            start_dy_var = NULL,
            end_dy_var = NULL,
            detail_var = NULL
          ),
          "Informed Consent" = list(
            start_dt_var = "RFICDT",
            end_dt_var = NULL,
            start_dy_var = NULL,
            end_dy_var = NULL,
            detail_var = NULL
          )
        ),
        adae = list(
          "Adverse Events" = list(
            start_dt_var = "AESTDTC",
            end_dt_var = "AEENDTC",
            start_dy_var = NULL,
            end_dy_var = NULL,
            detail_var = "AEDECOD"
          )
        ),
        adcm = list(
          "Concomitant Medications" = list(
            start_dt_var = "CMSTDTC",
            end_dt_var = "CMENDTC",
            start_dy_var = NULL,
            end_dy_var = NULL,
            detail_var = "CMDECOD"
          )
        )
      ),
      subjid_var = "USUBJID"
    )

    expected <- c("set_id", "subject_id", "AESTDTC", "AEENDTC", "AEDECOD", "AESOC", "AESER", "AEREL")

    expect_named(filter_data, expected, ignore.order = TRUE)
    expect_true("data.frame" %in% class(filter_data))
  }
)

test_that(
  "set_filter_dataset() throws an error when required parameters are missing {did:internal;tid:NA;WB:WB;NOR:N;}",
  {
    expect_error(
      set_filter_dataset(
        filter = list(ae_filter = list(dataset_name = "adae")),
        data_list = data_list,
        mapping = default_mapping(),
        subjid_var = "USUBJID"
      )
    )

    expect_error(
      set_filter_dataset(
        filter = list(ae_filter = list(dataset_name = "xyz", soc_var = "AESOC", label = "Adverse Events")),
        data_list = data_list,
        mapping = default_mapping(),
        subjid_var = "USUBJID"
      )
    )
  }
)


# Tests for check_*() functions ----
df <- data.frame(id = 1, char_col = "a", date_col = Sys.Date())

test_that("check_*() throws an error when columns names do not exist" %>%
  vdoc[["add_spec"]](specs$app_creation_specs$errors_def), {
  expect_error(check_names(df, var_names = c("char_col", "not_existing"), subjid_var = "id"))
})
test_that("check_*() throws an error when date columns are not of type date" %>%
  vdoc[["add_spec"]](specs$app_creation_specs$errors_data), {
  expect_error(check_date_type(df, var_names = c("char_col", "date_col")))
})

test_that("check_*() produces a warning when it can’t find the receiver module" %>%
  vdoc[["add_spec"]](c(specs$app_creation_specs$errors_def, specs$integration_specs$jumping)), {
  expect_warning(check_receiver("mod3", c("mod1", "mod2")))
})
