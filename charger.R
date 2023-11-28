charger <- function(matricule) {
  set.seed(matricule)
  mondata <- read.csv2("DevoirD_A23.csv")[sample(280,205),]
}
