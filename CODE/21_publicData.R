#**********************************************************************
# Export Data For Public Use
#**********************************************************************

geo_adj <- fread(file.path(dataout, "geo_stacked_long.csv"))

geo_adj[protEver == -Inf, protEver := NA]

# columns to keep
vars <- c("outside_perc_wk1", "outside_perc_wk2", "outside_perc_wk3", "outside_perc_wk4",
        "Protest", "protEver", "xcoord", "ycoord", "city", "state")
for (i in 1:4) { # append repetitive column names
  for (j in seq(200,500,100)) {
  vars <- c(vars, str_c("outside_perc_wk", i, "_above", j))
  }
}
geo_adj[,(vars) := lapply(.SD, as.numeric), .SDcols = vars] # convert to numeric

geo_adj <- geo_adj[,lapply(.SD, mean, na.rm = T), by = c("dest_cbg", "ds", "city", "state"), .SDcols = vars] # collapse onto destination cbg and date

fwrite(geo_adj, file.path(dataout, "floydDeviceTraces_public.csv.gz"))

