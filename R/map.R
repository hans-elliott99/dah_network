
library(data.table)
library(ggplot2)
library(sf)



db <- fread(here::here("data", "dah_clean.csv"))
setnafill(db, fill = 0, cols = "dah_21")


country_polys <- sf::read_sf(here::here("data", "country_sf"))
country_polys$ADMIN <- fcase(
  country_polys$ADMIN == "Cabo Verde", "Cape Verde",
  country_polys$ADMIN == "Republic of the Congo", "Congo",
  country_polys$ADMIN == "Ivory Coast", "Cote d'Ivoire",
  country_polys$ADMIN == "Czechia", "Czech Republic",
  country_polys$ADMIN == "Republic of Serbia", "Serbia",
  country_polys$ADMIN == "Gambia", "The Gambia",
  country_polys$ADMIN == "East Timor", "Timor-Leste",
  country_polys$ADMIN == "Wallis and Futuna", "Wallis and Futuna Islands",
  rep_len(TRUE, nrow(country_polys)), country_polys$ADMIN
)

# tst <- merge(
#   data.table(loc = db$recipient_country),
#   data.table(loc = country_polys$ADMIN, map = 1),
#   all.x = TRUE
# )
# ## just a few islands
# tst[is.na(map), unique(loc)]

#
# construct data
#
recip_dah <- db[year == 2020,
                .(dah = round(sum(dah_21, na.rm = TRUE))),
                by = .(recipient_country)]


mapdt <- merge(country_polys,
               recip_dah[, .(ADMIN = recipient_country, dah)],
               all.x = TRUE)

mapdt[, dah := dah / 1e9]
ggplot(mapdt) +
  geom_sf() + #aes(fill = dah)) +
  geom_point(
    data = mapdt[!is.na(mapdt$dah), ],
    aes(color = dah, size = dah, geometry = geometry),
    stat = "sf_coordinates"
  ) +
  geom_sf_text(aes(label = ADM0_A3), size = 3) +
  theme_bw()
