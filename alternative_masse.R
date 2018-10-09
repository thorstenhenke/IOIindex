v <- c(1, 1, 2, 2, 2, 3, 3, 3, 3)

# Anzahl der moeglichen wahlen innerhlab der eigenen gruppe
ig <- as.vector(table(v))[v] - 1
og <- length(v) - ig - 1 # Anzahl der moeglichen Wahlen außerhalb der eignenen Gruppe

# Block diagonal matrix => kann man dann auch gut zusammenfassen!

outer(v,v, "==")

rowSums(outer(v,v, "=="))
rowSums(outer(v,v, "!="))






n <- 6
v <- rep(1:2, each = 3)
m <- matrix(0, n, n)

social_all(m, v)

m[1, c(2,6)] <- 1
m[2, c(1,3)] <- 1
m[3, c(5,6)] <- 1
m[4, c(1:3)] <- 1
m[5, 1] <- 1
m[6, 5] <- 1

social_all(m, v)

# stimmt der index wirklich? Was exakt muss denn im nenner eigentlich stehen? Gegen was wird relativiert?
# die wahlen der anderen oder die theoretisch moeglichen wahlen

wa <- rowSums(m * outer(v,v,"=="))
we <- rowSums(m * outer(v,v,"!="))
log(wa + 1) - log(we + 1)

ig <- as.vector(table(v))[v] - 1
og <- length(v) - ig - 1

# log odds ratio???
na <- log(wa + 1) - log(ig + 1)
ne <- log(we + 1) - log(og + 1)
na - ne