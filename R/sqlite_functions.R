# library(RSQLite)
# library(tidyverse)
# library(nycflights13)


# flights %>% 
  # tbl(conn, "vuelos") %>% 
  # select(year, month, day, dep_time, arr_time, origin, dest) %>% 
  # mutate(tiempo_vuelo = arr_time - dep_time,
  #        tiempo_vuelo = if_else((arr_time %% 100 >= dep_time %% 100), tiempo_vuelo, tiempo_vuelo-40L)) %>% 
  # filter(tiempo_vuelo < 230) %>% 
  # show_query()

conn <- dbConnect(RSQLite::SQLite(), "data/cursoGI.db")

# dbWriteTable(conn, "vuelos", flights)
# 
# dbListTables(conn)


# alt1 <- function(){
#   conn <- dbConnect(RSQLite::SQLite(), "data/cursoGI.db")
#   data <- tbl(conn, "vuelos") %>% 
#     select(year, month, day, dep_time, arr_time, origin, dest) %>% 
#     mutate(tiempo_vuelo = arr_time - dep_time,
#            tiempo_vuelo = if_else((arr_time %% 100 >= dep_time %% 100), tiempo_vuelo, tiempo_vuelo-40L)) %>% 
#     filter(tiempo_vuelo < 230) %>% 
#     collect()
#   dbDisconnect(conn)
#   data
# }

# alt2 <- function(){
#   conn <- dbConnect(RSQLite::SQLite(), "data/cursoGI.db")
#   data <- tbl(conn, "vuelos") %>% 
#     collect() %>% 
#     select(year, month, day, dep_time, arr_time, origin, dest) %>% 
#     mutate(tiempo_vuelo = arr_time - dep_time,
#            tiempo_vuelo = if_else((arr_time %% 100 >= dep_time %% 100), tiempo_vuelo, tiempo_vuelo-40L)) %>% 
#     filter(tiempo_vuelo < 230) 
#   dbDisconnect(conn)
#   data
# }
# 
# alt1()
# alt2()
# 
# microbenchmark::microbenchmark(alt1(), alt2())


