#' Default settings for the Benchmarking
#' @export

default_settings <- list(
  vegc  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  soilc = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  litc = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  vegn  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  soiln  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  soilnh4  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  soilno3  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  mleaching  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  mn_immo  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  mn_mineralization  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  mn_volatilization  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  mn2_emis  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  mn2o_denit  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  mn2o_nit  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  mnuptake  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  mbnf  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  firec = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  flux_estab = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  fpc = c(GlobSumTimeAvgTableFPC, GlobSumAnnTimeseriesFPC, TimeAvgMapWithAbsUndertest,
          TimeAvgMapTreeCover),
  mgpp = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  mnpp = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  anbp = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  mrh = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  mevap = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  mtransp = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  minterc = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  mrunoff = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMapWithAbsUndertest),
  `pft_harvest.pft$rainfed rice;
  rainfed maize;
  rainfed oil crops soybean;
  rainfed grassland`  = c(GlobSumTimeAvgTablePFT_harvest,
                          GlobSumAnnTimeseriesPFT_harvest,
                          TimeAvgMapWithAbsUndertest)
)
