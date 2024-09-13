# Tests for get_filter_status() ----
all <- c("filter_serious_ae", "filter_soc", "filter_pref_term", "filter_drug")
chosen <- as.list(paste0(all[1:3], "_var"))
outcome <- get_filter_status(all_filters = all, chosen_filters = chosen)

test_that(
  "get_filter_status() returns a logical vector that indicates which local filters where defined" %>%
    vdoc[["add_spec"]](specs$sidebar_specs$AE_filter),
  {
    expect_type(outcome, "logical")
    expect_length(outcome, length(all))
    expect_named(outcome, all)
    expect_false(outcome[[all[4]]])
  }
)


# Tests for set_pts() ----
status <- list(soc = TRUE)
filter <- list(
  ae_filters = list(dataset_name = "adae", label = "Adverse Events", soc_var = "AESOC", pref_term_var = "AEDECOD")
)
data <- prep_data(add_ids(prep_dummy_data()), filter = filter)
socs <- unique(data[["AESOC"]])

test_that(
  "set_pts() returns a character vector of preferred terms available in the data" %>%
    vdoc[["add_spec"]](specs$sidebar_specs$AE_filter),
  {
    soc <- socs[3]
    pts <- set_pts(status, soc = soc, data, filter$ae_filters)

    expect_type(pts$AEDECOD, "character")
    expect_length(pts, length(unique(pts)))
  }
)

test_that(
  "set_pts() provides only PT's of the specified SOC('s)" %>%
    vdoc[["add_spec"]](specs$sidebar_specs$AE_filter),
  {
    soc <- socs[c(5, 7)]
    pts <- set_pts(status, soc = soc, data, filter$ae_filters)
    pt_compare <- unique(data[which(data$AESOC %in% soc), "AEDECOD"])

    expect_equal(pts, pt_compare)
  }
)

test_that(
  "set_pts() returns all available PT's if no SOC is specified" %>%
    vdoc[["add_spec"]](specs$sidebar_specs$AE_filter),
  {
    pts <- set_pts(status, soc = NULL, data, filter$ae_filters)
    pt_compare <- sort(unique(data[which(!is.na(data$AESOC)), ]$AEDECOD))

    expect_equal(pts, pt_compare)
  }
)


# Tests for filter_data() ----
status <- list(serious_ae = FALSE, soc = FALSE, pref_term = FALSE, drug_rel_ae = FALSE)
input <- list(serious_ae = NULL, soc = NULL, pref_term = NULL, drug_rel_ae = NULL)
data_list <- add_ids(prep_dummy_data())
df <- prep_data(data_list)

test_that(
  "filter_data() returns filtered AE data according to the parameters" %>%
    vdoc[["add_spec"]](specs$sidebar_specs$AE_filter),
  {
    # One filter
    status$serious_ae <- TRUE
    input$serious_ae <- "Y"
    filter <- list(ae_filter = list(
      dataset_name = "adae",
      label = "Adverse Events",
      serious_ae_var = "AESER"
    ))
    df_w_ae <- prep_data(data_list, filter = filter)

    df_one_filter <- filter_data(df_w_ae, status, input, filter)
    df_filter_man <- df_w_ae[which(df_w_ae$AESER == "Y" | df_w_ae$set != "adae"), ]

    expect_equal(nrow(df_one_filter), nrow(df_filter_man))

    # All filters
    status$soc <- status$pref_term <- status$drug_rel_ae <- TRUE
    input$soc <- c("cl B", "cl C")
    input$pref_term <- c("dcd B.2.2.3.1", "dcd B.1.1.1.1", "dcd C.1.1.1.3")
    input$drug_rel_ae <- "N"
    filter <- list(
      ae_filter = list(
        dataset_name = "adae",
        label = "Adverse Events",
        soc_var = "AESOC",
        serious_ae_var = "AESER",
        pref_term_var = "AEDECOD",
        drug_rel_ae_var = "AEREL"
      )
    )

    df_w_ae <- prep_data(data_list, filter = filter)

    df_all_filters <- filter_data(df_w_ae, status, input, filter)
    df_filter_man <- df_w_ae[which(
      df_w_ae$AESER == "Y" & df_w_ae$AESOC %in% input$soc & df_w_ae$AEDECOD %in% input$pref_term &
        df_w_ae$AEREL == input$drug_rel_ae | df_w_ae$set != "adae"
    ), ]

    expect_equal(nrow(df_all_filters), nrow(df_filter_man))
    expect_equal(names(df_all_filters), names(df_filter_man))
  }
)

test_that(
  "filter_data() does not filter if no filters were set" %>%
    vdoc[["add_spec"]](specs$sidebar_specs$AE_filter),
  {
    # No filter
    status <- list(
      serious_ae = FALSE,
      soc = FALSE,
      pref_term = FALSE,
      drug_rel_ae = FALSE
    )
    input <- list(
      serious_ae = NULL,
      soc = NULL,
      pref_term = NULL,
      drug_rel_ae = NULL
    )
    filter <- NULL
    df_no_filter <- filter_data(df, status, input, filter)

    expect_equal(nrow(df_no_filter), nrow(df))
  }
)


# Tests for check_filters() ----
test_that(
  "check_filters() throws an error when a non-existing filter is specified" %>%
    vdoc[["add_spec"]](c(specs$app_creation_specs$errors_def)),
  {
    all <- c("serious_ae", "soc", "pref_term", "drug")
    chosen <- as.list(c(all[2:3], "serios"))

    expect_error(check_filters(chosen, all))
  }
)
