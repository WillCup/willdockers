#!/usr/bin/env r

repos <- "http://mirrors.tuna.tsinghua.edu.cn/CRAN/"

lib.loc <- "/usr/lib64/R/library"

install.packages("data.table", lib.loc, repos)
install.packages("rJava", lib.loc, repos)
install.packages("dplyr", lib.loc, repos)
install.packages("bit64", lib.loc, repos)
install.packages("stringr", lib.loc, repos)
install.packages("ggplot2", lib.loc, repos)
install.packages("RODBC", lib.loc, repos)
install.packages("RJDBC", lib.loc, repos)
install.packages("timeSeries", lib.loc, repos)
install.packages("fUnitRoots", lib.loc, repos)
install.packages("forecast", lib.loc, repos)
install.packages("sqldf", lib.loc, repos)
install.packages("lubridate", lib.loc, repos)
