library(adehabitatHS)

#design II studies with only one observation of use per animal are 
  #a special case of design I studies

#With sampling protocol A (SPA), selection is studied by comparing a sample of
  #used resource units with a sample or census of available resource units

#note that all these methods rely on the following hypotheses: 
  #(i) independence between animals, and 
  #(ii) all animals are selecting habitat in the same way 
    #(no territoriality, all animals having equal access to all available resource units, etc.)
#the above works because these fish do not have territorial home ranges that exclude other fish
  #all fish have access to the stream

##### sediment #####
sedused <- c(17, 3, 1)
sednames <- c("Silt", "Sand", "Boulder")
names(sedused) <- c("Silt", "Sand", "Boulder")
sedavail <- c(0.86315789, 0.12000000, 0.01684211)
sedavail2 <- c(410, 57, 8)
names(sedavail) <- names(sedused)
sedresults <- widesI(sedused, sedavail, avknown = TRUE, alpha = 0.05)
plot(sedresults)
print(sedresults)

n_boot <- 1000
u_raw <- rep(sednames, times = sedused)
a_raw <- rep(sednames, times = sedavail2)

boot_wi <- matrix(NA, nrow = n_boot, ncol = length(sednames))
colnames(boot_wi) <- sednames

for(i in 1:n_boot) {
  
  u_sample <- sample(u_raw, size = length(u_raw), replace = TRUE)
  a_sample <- sample(a_raw, size = length(a_raw), replace = TRUE)
  
  u_boot <- table(factor(u_sample, levels = sednames))
  a_boot <- table(factor(a_sample, levels = sednames))
  
  try({
    mod <- widesI(u = as.numeric(u_boot), a = as.numeric(a_boot))
    boot_wi[i, ] <- mod$wi
  }, silent = TRUE)
}

boot_wi <- boot_wi[complete.cases(boot_wi), ]

final_summary <- data.frame(
  Habitat = sednames,
  Mean_wi = colMeans(boot_wi),
  Lower_CI = apply(boot_wi, 2, quantile, 0.025),
  Upper_CI = apply(boot_wi, 2, quantile, 0.975)
)

print(final_summary)

ggplot(final_summary, aes(x = reorder(Habitat, Mean_wi), y = Mean_wi)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkorange3", linewidth = 1) +
  geom_pointrange(aes(ymin = Lower_CI, ymax = Upper_CI), size = 0.8) +
  coord_flip() +
  labs(subtitle = "Values > 1 indicate preference; Values < 1 indicate avoidance",
       x = "Sediment",
       y = "Manly's Selection Index (wi) +/- 95% CI") +
  theme_bw()

#microhabitat
# sedused <- c(17, 3, 1)
# names(sedused) <- c("Silt", "Sand", "Boulder")
# sedavail <- c(0.82, 0.15, 0.02)
# names(sedavail) <- names(sedused)
# sedresults <- widesI(sedused, sedavail, avknown = TRUE, alpha = 0.05)
# plot(sedresults)
# print(sedresults)

#####cover#####
covused <- c(1, 2, 2, 8, 3, 5, 0)
names(covused) <- c("Boulder", "Coarse Woody Debris", "Emergent Vegetation", "None",
                    "Undercut Bank", "Overhanging Vegetation", "Submersed Vegetation")
covnames <- c("Boulder", "Coarse Woody Debris", "Emergent Vegetation", "None",
              "Undercut Bank", "Overhanging Vegetation", "Submersed Vegetation")
covavail <- c(0.018947368, 0.018947368, 0.128421053, 0.747368421, 0.002105263, 0.065263158,
              0.018947368)
covavail2 <- c(9, 9, 61, 355, 1, 31, 9)
names(covavail) <- names(covused)
covresults <- widesI(covused, covavail, avknown = TRUE, alpha = 0.05)
plot(covresults)
print(covresults)

n_boot <- 1000

u_raw <- rep(covnames, times = covused)
a_raw <- rep(covnames, times = covavail2)

boot_wi <- matrix(NA, nrow = n_boot, ncol = length(covnames))
colnames(boot_wi) <- covnames

for(i in 1:n_boot) {
  
  u_sample <- sample(u_raw, size = length(u_raw), replace = TRUE)
  a_sample <- sample(a_raw, size = length(a_raw), replace = TRUE)
  
  u_boot <- table(factor(u_sample, levels = covnames))
  a_boot <- table(factor(a_sample, levels = covnames))
  
  try({
    mod <- widesI(u = as.numeric(u_boot), a = as.numeric(a_boot))
    boot_wi[i, ] <- mod$wi
  }, silent = TRUE)
}

boot_wi <- boot_wi[complete.cases(boot_wi), ]

final_summary <- data.frame(
  Habitat = covnames,
  Mean_wi = colMeans(boot_wi),
  Lower_CI = apply(boot_wi, 2, quantile, 0.025),
  Upper_CI = apply(boot_wi, 2, quantile, 0.975)
)

print(final_summary)

ggplot(final_summary, aes(x = reorder(Habitat, Mean_wi), y = Mean_wi)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkorange3", linewidth = 1) +
  geom_pointrange(aes(ymin = Lower_CI, ymax = Upper_CI), size = 0.8) +
  coord_flip() +
  labs(subtitle = "Values > 1 indicate preference; Values < 1 indicate avoidance",
       x = "Sediment",
       y = "Manly's Selection Index (wi) +/- 95% CI") +
  theme_minimal()

#####depth#####
availdat <- read.csv("availabilitydat.csv", header = TRUE)
availdat$Vel_m <- as.numeric(availdat$Vel_m)
availonly <- availdat %>% 
  filter(Type == "Availability")
useonly <- availdat %>% 
  filter(Type == "Use")

depthused <- c(5, 13, 2)
names(depthused) <- c("<0.30", "0.30-0.60", ">0.60")
depthnames <- c("<0.30", "0.30-0.60", ">0.60")
depthavail <- c(0.754237288, 0.205508475, 0.040254237)
depthavail2 <- c(356, 97, 19)
names(depthavail) <- names(depthused)
depthresults <- widesI(depthused, depthavail, avknown = TRUE, alpha = 0.05)
plot(depthresults)
print(depthresults)

n_boot <- 1000

u_raw <- rep(depthnames, times = depthused)
a_raw <- rep(depthnames, times = depthavail2)

boot_wi <- matrix(NA, nrow = n_boot, ncol = length(depthnames))
colnames(boot_wi) <- depthnames

for(i in 1:n_boot) {
  
  u_sample <- sample(u_raw, size = length(u_raw), replace = TRUE)
  a_sample <- sample(a_raw, size = length(a_raw), replace = TRUE)
  
  u_boot <- table(factor(u_sample, levels = depthnames))
  a_boot <- table(factor(a_sample, levels = depthnames))
  
  try({
    mod <- widesI(u = as.numeric(u_boot), a = as.numeric(a_boot))
    boot_wi[i, ] <- mod$wi
  }, silent = TRUE)
}

boot_wi <- boot_wi[complete.cases(boot_wi), ]

final_summary <- data.frame(
  Habitat = depthnames,
  Mean_wi = colMeans(boot_wi),
  Lower_CI = apply(boot_wi, 2, quantile, 0.025),
  Upper_CI = apply(boot_wi, 2, quantile, 0.975)
)

print(final_summary)
final_summary$Habitat <- factor(final_summary$Habitat, levels = depthnames)

ggplot(final_summary, aes(x = Habitat, y = Mean_wi)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkorange3", linewidth = 1) +
  geom_pointrange(aes(ymin = Lower_CI, ymax = Upper_CI), size = 0.8) +
  coord_flip() +
  labs(subtitle = "Values > 1 indicate preference; Values < 1 indicate avoidance",
       x = "Depth",
       y = "Manly's Selection Index (wi) +/- 95% CI") +
  theme_bw()

#####velocity#####
velused <- c(18, 2, 0)
names(velused) <- c("<0.15", "0.15-0.30", ">0.30")
velnames <- c("<0.15", "0.15-0.30", ">0.30")
velavail <- c(0.716080402, 0.16080402, 0.123115578) #remember to exclude NA values from percentage
velavail2 <- c(285, 64, 49)
names(velavail) <- names(velused)
velresults <- widesI(velused, velavail, avknown = TRUE, alpha = 0.05)
plot(velresults)
print(velresults)

n_boot <- 1000

u_raw <- rep(velnames, times = velused)
a_raw <- rep(velnames, times = velavail2)

boot_wi <- matrix(NA, nrow = n_boot, ncol = length(velnames))
colnames(boot_wi) <- velnames

for(i in 1:n_boot) {
  
  u_sample <- sample(u_raw, size = length(u_raw), replace = TRUE)
  a_sample <- sample(a_raw, size = length(a_raw), replace = TRUE)
  
  u_boot <- table(factor(u_sample, levels = velnames))
  a_boot <- table(factor(a_sample, levels = velnames))
  
  try({
    mod <- widesI(u = as.numeric(u_boot), a = as.numeric(a_boot))
    boot_wi[i, ] <- mod$wi
  }, silent = TRUE)
}

boot_wi <- boot_wi[complete.cases(boot_wi), ]

final_summary <- data.frame(
  Habitat = velnames,
  Mean_wi = colMeans(boot_wi),
  Lower_CI = apply(boot_wi, 2, quantile, 0.025),
  Upper_CI = apply(boot_wi, 2, quantile, 0.975)
)

print(final_summary)
final_summary$Habitat <- factor(final_summary$Habitat, levels = velnames)


ggplot(final_summary, aes(x = Habitat, y = Mean_wi)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkorange3", linewidth = 1) +
  geom_pointrange(aes(ymin = Lower_CI, ymax = Upper_CI), size = 0.8) +
  coord_flip() +
  labs(subtitle = "Values > 1 indicate preference; Values < 1 indicate avoidance",
       x = "Velocity",
       y = "Manly's Selection Index (wi) +/- 95% CI") +
  theme_bw()


