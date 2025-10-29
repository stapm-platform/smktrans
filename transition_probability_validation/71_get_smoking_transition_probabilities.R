
## get the estimated smoking transition probabilities from OSF

#install.packages("curl")
#install.packages("readxl")

library("curl")
library("readxl")

url <- "https://osf.io/download/zy9q8/"
temp <- tempfile()
temp <- curl_download(url = url,
                      destfile = temp,
                      quiet = FALSE,
                      mode = "wb")

## read in probabilities

## INITIATION

init_forecast_data_2011_2100 <- read_excel(temp, 
                                           sheet = "Initiation", 
                                           range="A2:E18622", col_names = TRUE) %>% setDT
saveRDS(init_forecast_data_2011_2100, "intermediate_data/init_forecast_data_2011_2100.rds")

## QUIT

quit_forecast_data_2011_2100 <- read_excel(temp, 
                                           sheet = "Quit", 
                                           range="A2:E75462",  col_names = TRUE) %>% setDT
saveRDS(quit_forecast_data_2011_2100, "intermediate_data/quit_forecast_data_2011_2100.rds")

## RELAPSE

rel_forecast_data_2011_2100 <- read_excel(temp, 
                                          sheet = "Relapse", col_names = TRUE) %>% setDT

cn <- rel_forecast_data_2011_2100[1,]
cn <- as.character(as.vector(unlist(rel_forecast_data_2011_2100[1,])))
rel_forecast_data_2011_2100 <- rel_forecast_data_2011_2100[-1,]
colnames(rel_forecast_data_2011_2100) <- cn
rel_forecast_data_2011_2100[ , age := as.numeric(as.vector(age))]
rel_forecast_data_2011_2100[ , year := as.numeric(as.vector(year))]
rel_forecast_data_2011_2100[ , p_relapse := as.numeric(as.vector(p_relapse))]
rel_forecast_data_2011_2100[ , time_since_quit := as.numeric(as.vector(time_since_quit))]

saveRDS(rel_forecast_data_2011_2100, "intermediate_data/relapse_forecast_data_2011_2100.rds")
