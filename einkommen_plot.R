source("data_preparation.R")
source("rolling_correlation.R")
source("incidence_lk.R")
einkommen_cor <- rolling_correlation("xlsx/einkommen.xlsx")
auszahlungen_cor <- rolling_correlation("xlsx/auszahlungen.xlsx")

inzidenz <- einkommen_cor
inzidenz$cts <- calc_sti_germany()/500

plot(einkommen_cor, type = "l")
lines(auszahlungen_cor,col="green")
lines(inzidenz,col="red")
