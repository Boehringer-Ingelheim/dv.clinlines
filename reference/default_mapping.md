# Set default values for `mapping`

Set default values for `mapping`

## Usage

``` r
default_mapping()
```

## Value

A list that contains lists of events for ADSL, ADAE, and ADCM datasets.
Events defined are:

- For ADSL: Treatment Start (`TRTSDT` variable), Treatment End (`TRTEDT`
  variable), and Informed Consent (`RFICDT` variable)

- For ADAE: Adverse Events with `AESTDTC` (start_dt_var), `AEENDTC`
  (end_dt_var), and `AEDECOD` (detail_var)

- For ADCM: Concomitant Medications with `CMSTDTC` (start_dt_var),
  `CMENDTC` (end_dt_var), and `CMDECOD` (detail_var)
