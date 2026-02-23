library(adehabitatHS)

#sediment
sedused <- c(17, 3, 1)
names(sedused) <- c("Silt", "Sand", "Boulder")
sedavail <- c(0.82, 0.15, 0.02)
names(sedavail) <- names(sedused)
sedresults <- widesI(sedused, sedavail, avknown = TRUE, alpha = 0.05)
plot(sedresults)
print(sedresults)

#microhabitat
# sedused <- c(17, 3, 1)
# names(sedused) <- c("Silt", "Sand", "Boulder")
# sedavail <- c(0.82, 0.15, 0.02)
# names(sedavail) <- names(sedused)
# sedresults <- widesI(sedused, sedavail, avknown = TRUE, alpha = 0.05)
# plot(sedresults)
# print(sedresults)

#cover
covused <- c(1, 2, 2, 8, 8, 0)
names(covused) <- c("Boulder", "CWD", "E Veg", "None",
                    "O Veg", "S Veg")
covavail <- c(0.03, 0.01, 0.02, 0.83, 0.08, 0.03)
names(covavail) <- names(covused)
covresults <- widesI(covused, covavail, avknown = TRUE, alpha = 0.05)
plot(covresults)
print(covresults)
