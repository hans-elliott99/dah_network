library(data.table)
library(igraph)
library(ggraph)


db <- fread(here::here("data", "dah_clean.csv"))
setnafill(db, fill = 0, cols = "dah_21")

#
# construct data
#
flow <- db[year == 2020,
           .(traffic = round(sum(dah_21))),
           by = .(origin = source,
                  dest = recipient_isocode
                  )]
flow <- flow[! origin %in% c("Private_other", "BMGF", "Corporate_donations")]
flow[, origin := gsub("_", " ", origin)]



flow <- flow[traffic != 0]

opp <- data.table(origin = flow$dest,
                  dest = flow$origin, traffic = 0)
flow <- rbind(flow, opp)


# map
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
  country_polys$ADMIN == "United States of America", "United States",
  rep_len(TRUE, nrow(country_polys)), country_polys$ADMIN
)



#
# define flow network
#
# adjaceny matrix:
W <- xtabs(traffic ~ origin + dest, data = flow)
flownet <- igraph::graph_from_adjacency_matrix(W,
                                               weighted = TRUE)

# features
V(flownet)$inflow <- strength(flownet, mode = "in")
V(flownet)$outflow <- strength(flownet, mode = "out")


regmap <- rbind(
  unique(db[, .(loc = recipient_isocode, gbd_superregion)]),
  unique(db[, .(loc = source, gbd_superregion = "Donor")])
)

V(flownet)$region <- vapply(V(flownet)$name,
                            \(x) regmap[loc == x, gbd_superregion],
                            character(1))

in.flow <- strength(flownet, mode = "in")
out.flow <- strength(flownet, mode = "out")


#
# plots
#
vsize <- sqrt(in.flow + out.flow) / 100
pie.vals <- lapply(1:vcount(flownet),
                   \(i) c(in.flow[i], out.flow[i]))

ewidth <- E(flownet)$weight

plot(flownet,
     vertex.size = vsize,
     vertex.shape = "pie",
     vertex.pie = pie.vals,
     edge.width = 1,
     edge.arrow.size = 0.1,
     scale = FALSE
     )

plot(flownet, layout = layout.fruchterman.reingold(flownet))


ggraph(flownet, layout = "fr") +
  geom_edge_arc(aes(alpha = E(flownet)$weight),
                arrow = arrow(type = "closed", length = unit(2, "mm")),
                alpha = 0.05) +
  geom_node_point(aes(color = region),
                  alpha = 0.5) +
  hrbrthemes::theme_ipsum_pub()

#






