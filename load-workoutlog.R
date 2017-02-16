library(data.table)
library(lubridate)
library(ggplot2)
library(scales)

## 1RM calculations
b.brzycki <- 0.0278
k.brzycki <- 1 + b.brzycki

calc.1rm.brzycki <- function (reps, weight) {
    return (weight / (k.brzycki - b.brzycki * reps))
}

calc.nrm.brzycki <- function (rm1, reps) {
    return ( rm1 * (k.brzycki - b.brzycki * reps))
}

calc.reps.brzycki <- function (rm1, weight) {
    return ( (k.brzycki - weight / rm1) / b.brzycki )
}

b.epley <- 0.0333

calc.1rm.epley <- function (reps, weight) {
    return (weight * (1 + reps * b.epley))
}

b.lander <- 0.0267123
k.lander <- 1.013

calc.1rm.lander <- function ( reps, weight ) {
    return (weight / (k.lander - b.lander * reps))
}

## Load up data file.
strength <- fread("~/Downloads/strength_mod.csv")
weight <- fread("~/Downloads/weight.csv")

## Parse dates
strength[,DateTime:=mdy_hm(paste(Date, Time))]
weight[,DateTime:=ymd_hm(paste(Date, Time))]

## Correct ill-named column.
setnames(strength,"# of Reps", "Reps")
power.lifts <- c("Standing Overhead Press", "Bench Press", "Deadlift",
                 "Squat", "Power Clean")
# power.lifts <- c("Standing Overhead Press", "Bench Press", "Deadlift",
#                  "Squat", "Bent-over Row")
strength[, est.1rm := calc.1rm.lander(Reps, Weight)]

## Styles
style <- list(scale_x_date(breaks = date_breaks("2 week"),
                           # minor_breaks = date_breaks("1 day"),
                           labels=date_format("%m-%d")),
              geom_point(),
              facet_wrap(~Exercise, scales = "free_y"),
              theme(axis.text.x = element_text(angle = 90))
              )

## Plot
p <- ggplot(strength[Reps >=5 & Exercise == "Squat"],
            aes(x=DateTime, y=Weight)) + geom_point()

## Find max weight set of exercise per day
max.work <- strength[Exercise %in% power.lifts,
                     .(MaxWeight=max(Weight)),
                     by=.(Exercise=Exercise, Date=as.Date(DateTime))]
q <- ggplot(max.work, aes(x=Date, y=MaxWeight)) +
              stat_smooth() +
              style

## Est 1RM from sets of day
max.1rm <- strength[Reps <= 10 & Exercise %in% power.lifts,
                    .(MaxEst=max(est.1rm)),
                    by=.(Exercise=Exercise, Date=as.Date(DateTime))]
r <- ggplot(max.1rm, aes(x=Date, y=MaxEst)) +
              stat_smooth() +
              style

wplot <- ggplot(weight, aes(x=DateTime, y=Weight)) + geom_line()
