% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Metric_subclasses.R
\name{GlobSumTimeAvgTable}
\alias{GlobSumTimeAvgTable}
\title{GlobSumTimeAvgTable}
\description{
GlobSumTimeAvgTable metric.
See \link{Metric} for the documentation of metrics in general.
}
\section{Super class}{
\code{\link[lpjmlstats:Metric]{lpjmlstats::Metric}} -> \code{GlobSumTimeAvgTable}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{m_options}}{List of metric options specific to this metric:
\itemize{
\item \code{font_size}: integer, font size of the table (default 7)
\item \code{disp_digits}: integer, number of significant digits to display (default 4)
\item \code{year_subset}: character vector, defines which calendar years the metric considers,
i.e., a data subset that the metric works with; e.g., c("1995", "1996") (default 1991:2000).
\item \code{cell_subset}: character vector, defines which cells to subset (default NULL)
}}

\item{\code{title}}{Section header used in the report}

\item{\code{description}}{Description used in the report}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-GlobSumTimeAvgTable-summarize}{\code{GlobSumTimeAvgTable$summarize()}}
\item \href{#method-GlobSumTimeAvgTable-compare}{\code{GlobSumTimeAvgTable$compare()}}
\item \href{#method-GlobSumTimeAvgTable-plot}{\code{GlobSumTimeAvgTable$plot()}}
\item \href{#method-GlobSumTimeAvgTable-arrange_plots}{\code{GlobSumTimeAvgTable$arrange_plots()}}
\item \href{#method-GlobSumTimeAvgTable-clone}{\code{GlobSumTimeAvgTable$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="lpjmlstats" data-topic="Metric" data-id="add_compare_meta"><a href='../../lpjmlstats/html/Metric.html#method-Metric-add_compare_meta'><code>lpjmlstats::Metric$add_compare_meta()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lpjmlstats" data-topic="Metric" data-id="add_comparisons"><a href='../../lpjmlstats/html/Metric.html#method-Metric-add_comparisons'><code>lpjmlstats::Metric$add_comparisons()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lpjmlstats" data-topic="Metric" data-id="capture_summary"><a href='../../lpjmlstats/html/Metric.html#method-Metric-capture_summary'><code>lpjmlstats::Metric$capture_summary()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lpjmlstats" data-topic="Metric" data-id="generate_report_content"><a href='../../lpjmlstats/html/Metric.html#method-Metric-generate_report_content'><code>lpjmlstats::Metric$generate_report_content()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lpjmlstats" data-topic="Metric" data-id="print_metric_description"><a href='../../lpjmlstats/html/Metric.html#method-Metric-print_metric_description'><code>lpjmlstats::Metric$print_metric_description()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lpjmlstats" data-topic="Metric" data-id="print_metric_header"><a href='../../lpjmlstats/html/Metric.html#method-Metric-print_metric_header'><code>lpjmlstats::Metric$print_metric_header()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lpjmlstats" data-topic="Metric" data-id="print_year_subset"><a href='../../lpjmlstats/html/Metric.html#method-Metric-print_year_subset'><code>lpjmlstats::Metric$print_year_subset()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lpjmlstats" data-topic="Metric" data-id="store_summary"><a href='../../lpjmlstats/html/Metric.html#method-Metric-store_summary'><code>lpjmlstats::Metric$store_summary()</code></a></span></li>
<li><span class="pkg-link" data-pkg="lpjmlstats" data-topic="Metric" data-id="transform_lpjml_calcs"><a href='../../lpjmlstats/html/Metric.html#method-Metric-transform_lpjml_calcs'><code>lpjmlstats::Metric$transform_lpjml_calcs()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GlobSumTimeAvgTable-summarize"></a>}}
\if{latex}{\out{\hypertarget{method-GlobSumTimeAvgTable-summarize}{}}}
\subsection{Method \code{summarize()}}{
First take global weighted sum, then average over all time steps
of the simulation period. The result is a scalar for each band.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GlobSumTimeAvgTable$summarize(data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{LPJmLDataCalc object to be summarized}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A summarized \link{LPJmLDataCalc} object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GlobSumTimeAvgTable-compare"></a>}}
\if{latex}{\out{\hypertarget{method-GlobSumTimeAvgTable-compare}{}}}
\subsection{Method \code{compare()}}{
Calculate difference and relative difference to the baseline.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GlobSumTimeAvgTable$compare(var_grp)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{var_grp}}{variable group}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GlobSumTimeAvgTable-plot"></a>}}
\if{latex}{\out{\hypertarget{method-GlobSumTimeAvgTable-plot}{}}}
\subsection{Method \code{plot()}}{
Create a table of the results.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GlobSumTimeAvgTable$plot()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A tibble with the results
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GlobSumTimeAvgTable-arrange_plots"></a>}}
\if{latex}{\out{\hypertarget{method-GlobSumTimeAvgTable-arrange_plots}{}}}
\subsection{Method \code{arrange_plots()}}{
Style the table to be displayed in the report.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GlobSumTimeAvgTable$arrange_plots(table)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{table}}{A tibble with the results}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GlobSumTimeAvgTable-clone"></a>}}
\if{latex}{\out{\hypertarget{method-GlobSumTimeAvgTable-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GlobSumTimeAvgTable$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
