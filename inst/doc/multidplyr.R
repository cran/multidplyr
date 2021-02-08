## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
set.seed(1014)

## ----setup--------------------------------------------------------------------
library(multidplyr)
library(dplyr, warn.conflicts = FALSE)
library(nycflights13)

## -----------------------------------------------------------------------------
cluster <- new_cluster(2)
cluster

## -----------------------------------------------------------------------------
flights1 <- flights %>% group_by(dest) %>% partition(cluster)
flights1

## -----------------------------------------------------------------------------
path <- tempfile()
dir.create(path)

flights %>% 
  group_by(month) %>% 
  group_walk(~ vroom::vroom_write(.x, sprintf("%s/month-%02i.csv", path, .y$month)))

## -----------------------------------------------------------------------------
files <- dir(path, full.names = TRUE)
cluster_assign_partition(cluster, files = files)

## -----------------------------------------------------------------------------
cluster_send(cluster, flights2 <- vroom::vroom(files))

flights2 <- party_df(cluster, "flights2")
flights2

## -----------------------------------------------------------------------------
flights1 %>% 
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  collect()

## -----------------------------------------------------------------------------
by_dest <- flights %>% group_by(dest)

# Local computation
system.time(by_dest %>% summarise(mean(dep_delay, na.rm = TRUE)))

# Remote: partitioning
system.time(flights2 <- flights %>% partition(cluster))
# Remote: computation
system.time(flights3 <- flights2 %>% summarise(mean(dep_delay, na.rm = TRUE)))
# Remote: retrieve results
system.time(flights3 %>% collect())

## -----------------------------------------------------------------------------
daily_flights <- flights %>%
  count(dest) %>%
  filter(n >= 365)

common_dest <- flights %>% 
  semi_join(daily_flights, by = "dest") %>% 
  mutate(yday = lubridate::yday(ISOdate(year, month, day))) %>% 
  group_by(dest)

nrow(common_dest)

## -----------------------------------------------------------------------------
by_dest <- common_dest %>% partition(cluster)
by_dest

## ---- message = FALSE---------------------------------------------------------
cluster_library(cluster, "mgcv")
system.time({
  models <- by_dest %>% 
    do(mod = gam(dep_delay ~ s(yday) + s(dep_time), data = .))
})

## -----------------------------------------------------------------------------
system.time({
  models <- common_dest %>% 
    group_by(dest) %>% 
    do(mod = gam(dep_delay ~ s(yday) + s(dep_time), data = .))
})

