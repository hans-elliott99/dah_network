

library(data.table)


d <- fread(here::here("data", "IHME_DAH_DATABASE_1990_2021_Y2023M03D06.CSV"))

#
# select vars
#
d <- d[,
  .(year,
    source,
    channel,
    elim_ch,
    recipient_isocode,
    recipient_country,
    gbd_region,
    gbd_superregion,
    dah_21 = as.numeric(gsub("-", "", dah_21)) * 1e3
  )]

#
# filter rows
#

# not real recipients - admin expenses, unallocable, and non-specified global
d <- d[! recipient_isocode %in% c("INKIND","QZA","WLD")]

# rm sources that are not a real entity or too broad
d <- d[! source %in% c(
  "Debt_repayments",
  "Non_OECD_DAC_countries",
  "Other",
  "Other_OECD_DAC_countries",
  "Unallocable"
)]

# rm channels that are too broad
d <- d[! channel %in% c("INTLNGO", "US_FOUND")]



#
# save
#
fwrite(d, here::here("data", "dah_clean.csv"))
