# Use a list to declare the specs
specs_list <- list

#nolint start line_length_linter
output_specs <- specs_list(
  "general_display" = "The module displays a plot and a sidebar."
)

plot_specs <- specs_list(
  "events" = "The plot displays events of all subjects of a certain clinical trial over time (x-axis) and grouped by subject (y-axis).",
  "event_types" = "The plot displays events that reflect a milestone (timepoint) as bullet, and events that reflect a time period (interval) as horizontal line on the timeline.",
  "drug_admin_event" = "Drug administration events additionally indicate trough different marker shapes if the drug dose changed compared to the last administration (equal, decrease, increase).",
  "colors" = "Events are displayed in a type specific color. A legend indicates which color refers to which event type.",
  "hovering" = "The plot provides a hover-over function in the sense that hovering with the mouse over a subject row triggers a tooltip to appear to list events in direct proximity to the mouse position, including event type, date(s) and further details (e.g. preferred term, medication name, …).",
  "errors" = "If the plot cannot be generated, the module displays an informative message why this is the case and how to proceed."
)

sidebar_specs <- specs_list(
  "date_day" = "The sidebar of the module provides the option to switch the x-axis scale of the plot from 'Date' to 'Study Day' and back.",
  "time_range" = "The sidebar of the module provides the option to modify the time range of the x-axis of the plot, depending on whether the user selected 'Date' or 'Study Day' before.",
  "sorting" = "The sidebar of the module provides the option to sort the y-axis of the plot, either by earliest event or alphanumerically.",
  "event_type_filter" = "The sidebar of the module provides the option to de-activate certain event categories (e.g. randomization, adverse events, …). If an event category is de-activated, none of the events falling under this category are displayed in the plot.",
  "boxheight" = "The sidebar of the module provides the option to adapt the amount of vertical space that is used for each subject in the plot.",
  "AE_filter" = "If configured accordingly, the sidebar of the module provides the option to filter adverse events by seriousness, drug relation, system organ class, and preferred term."
)

default_values <- specs_list(
  "plot_settings_x_param" = "The module should allow to set the default value for the parameter to be displayed on the x-axis. If not specified explicitly, study days are displayed by default.",
  "plot_settings_start_day" = "The module should allow to set the default value for the lower x-axis limit in case of study day display. If not specified explicitly, the minimum study day is chosen.",
  "plot_settings_boxheight_val" = "The module should allow to set the default value for the indivdual timeline plot's heights."
)

integration_specs <- specs_list(
  "bookmarking" = "The module is compatible with the bookmarking feature of the dv.manager.",
  "jumping" = "The module is able to send a unique subject identifier to another DaVinci module which will then display the corresponding information (depending on the receiver module) for that subject identifier.",
  "reset_filters" = "When using the module in combination with dv.manager, local filters are getting reset to the default values (only) at dataset switch."
)

app_creation_specs <- specs_list(
  "errors_def" = "In case the module definition does not match with the underlying data, the app creator should be informed accordingly.",
  "errors_data" = "In case the data is not available in the necessary format, the app creator should be informed accordingly. "

)
#nolint end line_length_linter

specs <- specs_list(
  output_specs = output_specs,
  plot_specs = plot_specs,
  sidebar_specs = sidebar_specs,
  default_values = default_values,
  integration_specs = integration_specs,
  app_creation_specs = app_creation_specs
)
