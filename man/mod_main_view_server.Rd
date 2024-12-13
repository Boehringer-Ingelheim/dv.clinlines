% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_main_view.R
\name{mod_main_view_server}
\alias{mod_main_view_server}
\title{Server logic of the main view module of \pkg{dv.clinlines}}
\usage{
mod_main_view_server(
  module_id,
  initial_data,
  changed,
  colors_groups,
  start_day = NULL,
  ms = 100
)
}
\arguments{
\item{module_id}{A unique ID string to create a namespace. Must match the ID of
\code{mod_main_view_UI()}.}

\item{initial_data}{A metaReactive data frame returned by
\code{mod_local_filter_server()}.}

\item{changed}{A reactive whose actualization indicates if the underlying dataset has changed.}

\item{colors_groups}{A reactive named vector holding hexcode colors.}

\item{start_day}{\verb{[integer(1)]} A single integer indicating the lower x-axis limit in case of study day display.
Defaults to NULL, using the day of the earliest event to be displayed.}

\item{ms}{A single numeric value indicating how many milliseconds to be used for
debouncing the main view plot.}
}
\value{
A list of reactives to be used to communicate with other DaVinci modules,
if available.
}
\description{
Server logic of the main view module of \pkg{dv.clinlines}
}
\keyword{internal}
