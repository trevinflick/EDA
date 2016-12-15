# load the necessary packages
library(MASS)
library(ggplot2)
library(LearnEDA)
library(dplyr)

# read in the data
cabrera <- read.csv("cabrera.csv")
cabrera <- cabrera[, c("events", "hit_distance_sc", "hit_speed", "hit_angle")]

# remove balls that didn't record a distance
cabrera <- cabrera[cabrera$hit_distance_sc != "null", ]

cabrera$hit_speed <- as.numeric(levels(cabrera$hit_speed))[cabrera$hit_speed]
cabrera$hit_distance_sc <- as.numeric(levels(cabrera$hit_distance_sc))[cabrera$hit_distance_sc]
cabrera$hit_angle <- as.numeric(levels(cabrera$hit_angle))[cabrera$hit_angle]

# get rid of result that was recorded as a walk
cabrera[cabrera$events == "Walk", ] <- NA

# make a new column for hit, out, or error
cabrera$result <- grepl("Double Play|Flyout|Forceout|Grounded Into DP|Groundout|Lineout|Pop Out|Sac Fly", cabrera$events)
cabrera$result[cabrera$result == TRUE] <- "out"
cabrera$result[cabrera$events == "Field Error"] <- "error"
cabrera$result[cabrera$result == FALSE] <- "hit"

# get rid of NA's
cabrera <- na.omit(cabrera)

# c2 = all batted ball events (+balls statcast misses)
c1 <- read.csv("cabrera.csv")
c1$result <- grepl("Double Play|Flyout|Forceout|Grounded Into DP|Groundout|Lineout|Pop Out|Sac Fly|Strikeout - DP", c1$events)
c1$result[c1$result == TRUE] <- "out"
c1$result[c1$events == "Field Error"] <- "error"
c1$result[c1$result == FALSE] <- "hit"
c2 <- c1[c1$type == "X", ]

# missed tracked balls
n <- c2[c2$hit_distance_sc == "null", ]

# batted ball distance histogram
p1 <- ggplot(data = cabrera, aes(hit_distance_sc)) + geom_histogram(bins=21)
p1 + labs(title = "Batted Balls by Distance", x="Distance (ft.)", y="count")

# batted ball speed histogram
b <- round(sqrt(length(cabrera$hit_speed)))
p2 <- ggplot(data = cabrera, aes(hit_speed)) + geom_histogram(bins = b)
p2 + labs(title = "Batted Balls by Speed", x="Speed (MPH)", y="count")

# number the batted balls in order
cabrera$id <- seq(435, 1, by=-1)
cabrera <- arrange(cabrera, id)

# smoothed batted ball speed over entire season
detach("package:dplyr", unload=TRUE)
smooth.3RSS <- smooth(cabrera$hit_speed, kind="3RSS")
smooth.3RSSH <- han(smooth.3RSS)
with(cabrera,
     plot(id, smooth.3RSSH, type="l",col="red",lwd=2, 
     xlab="Batted Ball Number", ylab="Hit Speed (MPH)", 
     main="3RSSH SMOOTH", ylim = c(50,110)))

# batted ball angle graph
p3 <- ggplot(cabrera, aes(x=hit_angle)) + geom_histogram()
p3 <- p3 + coord_polar(start = pi/2, direction = -1) + scale_x_continuous(limits=c(-180,180),breaks=seq(-90, 90, 45))
p3 + labs(title = "Batted Balls by Angle", x="Angle", y="count")

# batted ball angle histogram
bins = seq(-60, 70, 6.5)
m = 13.82 # median
s = (26.2 - 0.22) / 1.35 # standard dev.
truehist(cabrera$hit_angle, breaks=bins, xlab="Hit Angle")
curve(dnorm(x, 13.82, 19.244), add=TRUE, col="red", lwd=3)

# make a normal fit for batted ball angle with residuals
fit = fit.gaussian(cabrera$hit_angle, bins, m, s)
bin.mids = seq(-56.75,66.75,6.5) # bin mid-points
plot(bin.mids, fit$residual, xlab="bin midpoints", ylab="residual")
abline(h=0)

# classify hits as single, double, or home run
h <- c("Single", "Double", "Home Run")
hits <- cabrera[cabrera$events %in% h, ]
cabrera$result <- factor(cabrera$result)

# make a new result data frame
result <- c("hit", "out")
r <- cabrera[cabrera$result %in% result, ]
r$result <- factor(r$result)

# distance boxplot by result
p <- ggplot(data=r, aes(hit_distance_sc, result))
p <- p + geom_boxplot() + coord_flip()
p + labs(title="Result by Hit Distance", x="Result", y="Hit Distance (ft.)") 

# hit speed vs. hit distance
p <- ggplot(data = cabrera, aes(hit_speed, hit_distance_sc))
p <- p + geom_point() + labs(title="Batted Balls (Speed vs. Distance)", x="Hit Speed (MPH)", y="Hit Distance (ft.)")

# measuring "the zone"
zone <- cabrera[cabrera$hit_angle <= 35 & cabrera$hit_angle >= 20, ]
fit <- rlm(zone$hit_distance_sc~zone$hit_speed)
c <- fit$coefficients
p <- ggplot(data = zone, aes(hit_speed, hit_distance_sc)) + geom_point()
p <- p + labs(title="Batted Balls in \"The Zone\"", x="Hit Speed (MPH)", y="Hit Distance (ft.)" )
p + geom_abline(intercept = c[1], slope = c[2], color = "red")

plot(zone$hit_speed, fit$residuals, xlab = "Hit Speed (MPH)", ylab = "residual")
abline(h = 0, col = "red")