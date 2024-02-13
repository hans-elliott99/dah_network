#
# install project dependencies (if needed)
#

if (!require("pacman")) install.packages("pacman")


pkgs <- c(
  "here",
  "data.table",
  "ggplot2",
  "ergm",
  # sudo apt-get -y install libglpk-dev
  "igraph",
  "sf"
)

# iterate in case of errors
# p_load installs package if it can't be found
for (pkg in pkgs) {
  print(pkg)
  pacman::p_load(pkg, character.only = TRUE)
}



# notes:
# https://www.sciencedirect.com/science/article/abs/pii/S0305750X23000220?via%3Dihub
# https://www.biorxiv.org/content/10.1101/678268v1.full



# data:
# https://www.naturalearthdata.com/downloads/10m-cultural-vectors/