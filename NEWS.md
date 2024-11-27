# dv.clinlines 1.0.5

* Display drug administration information in different colors, depending on the treatment name.
* Allowing for customized color palettes.

# dv.clinlines 1.0.4

* Adapt basic_info, filter, and drug_admin parameter to adhere module standard

# dv.clinlines 1.0.3

* Initial release of dv.clinlines package to GitHub.


# dv.clinlines 1.0.2

* Catch error that occurred in case of empty datasets.
* Improve usage of default values for plot settings.
* Improve checks for date variable types.
* Add informative error message in case of the plot being to big.


# dv.clinlines 1.0.1

* Fix issue with local filters getting reset unintentionally (occurred especially in combination with dv.filter >= 2.1.0)
* Solve incompatibility with dplyr >= 1.1.0 (note: **dv.clinlines** now requires dplyr >= 1.1.0 due to updates within the `dplyr::case_when()` function)
* Update code to not use deprecated `afmm$dataset_name` parameter but `afmm$dataset_metadata$name` from **dv.manager** >= 2.1.0
* Standardize module handling by renaming the subject identifier and using the receiver module's ID for inter-module communication
* Introduce the possibility to set default plot settings
* Structure plot legend vertically instead of horizontally to avoid cropping


# dv.clinlines 1.0.0

-   This is the first productive release of clinical timelines
-   Primary interface: `mod_clinical_timelines()`


# dv.clinlines 0.0.4

* Refactor package
* Restructure wrapper
* Generalize between-module communication (not only to patient profile modules)
* Adaptions for better orientation
  - Add plot title, legend title, and x-axis label for better orientation
  - Remove participant dropdown ("Subject View") and rearrange sidebar
  - Impute missing start and end dates by informed consent dates and end of participation dates or today for ongoing 
  - Make drug administration event optional
  

# dv.clinlines 0.0.3

* Remove detail view completely
* Enable communication with a patient profile module when used within **dv.manager**
* Shorten hover box to display only relevant data points 
* Fix a variety of bugs
* Refine error messages that are thrown when setting up an app 
* Prepare code export functionality


# dv.clinlines 0.0.2

* Improve interaction with **dv.manager**/ **dv.filter**:
  - Prevent module from being restarted when global filters in the module manager are used.
  - Ensure controls/filters are resetted when a dataset in module manager is changed.
  - Ensure that all controls/filters are being restored properly when bookmarking happened.
* Add `ms` parameter to `mod_clinical_timelines()` so that users can specify the amount of time 
  that passes by before the plot on the main view re-renders.
  

# dv.clinlines 0.0.1

* Change package name from **clinicaltimelines** to **dv.clinlines**.


# clinicaltimelines 0.0.1

* Added a `NEWS.md` file to track changes to the package.
