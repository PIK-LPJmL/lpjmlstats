#' Default settings for the Benchmarking
#' @export

default_settings <- list(
  vegc  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  soilc = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  litc = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  vegn  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  soiln  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  soilnh4  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  soilno3  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  mleaching  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  mn_immo  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  mn_mineralization  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  mn_volatilization  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  mn2_emis  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  mn2o_denit  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  mn2o_nit  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  mnuptake  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  mbnf  = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  firec = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  flux_estab = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  fpc = c(GlobSumTimeAvgTableFPC, GlobSumAnnTimeseriesFPC, TimeAvgMap, TimeAvgMapTreeCover),
  mgpp = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  mnpp = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  anbp = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  mrh = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  mevap = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  mtransp = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  minterc = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  mrunoff = c(GlobSumTimeAvgTable, GlobSumAnnAvgTimeseries, TimeAvgMap),
  `pft_harvest.pft$rainfed rice;
  rainfed maize;
  rainfed oil crops soybean;
  rainfed grassland`  = c(GlobSumTimeAvgTablePFT_harvest,
                          GlobSumAnnTimeseriesPFT_harvest,
                          TimeAvgMap)
)
