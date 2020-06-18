library(EpiABC)

out <- get_posterior(wave = 25, input = "out/abc/8/")
summary(out, digits = 3)

boxplot(out, type = "stats")

plot(out, type = "stats")

plot(out, type = "param")

boxplot(out, type = "param")
