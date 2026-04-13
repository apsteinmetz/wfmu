library(tidyverse)
library(dtplyr)
library(duckdb)
library(bench)
library(data.table)
load("data/playlists.Rdata")
#convert to data.table
playlists_dt<-lazy_dt(playlists)
con <- dbConnect(duckdb())
duckdb_register(con, "playlists_duckdb", playlists)

playlists_arrow <- arrow::read_parquet("data/playlists.parquet",as_data_frame = FALSE)

dplyr_a <- function(){
playlists |>
group_by(ArtistToken) |>
summarise(playcount = n()) |>
slice_max(n=10, order_by=playcount)
}
duck_a <- function(){
tbl(con, "playlists_duckdb") |>
group_by(ArtistToken) |>
summarise(playcount = n()) |>
slice_max(n=10, order_by=playcount) |>
collect()
}
dt_a <- function(){
playlists_dt |>
group_by(ArtistToken) |>
summarise(playcount = n()) |>
slice_max(n=10, order_by=playcount) |>
as_tibble()
}

arrow_a <- function(){
  playlists_arrow |>
    group_by(ArtistToken) |>
    summarise(playcount = n()) |>
    slice_max(n=10, order_by=playcount,with_ties = FALSE) |>
    as_tibble()
}

dplyr_a()
dt_a()
duck_a()
arrow_a()

#benchmark all functions
bench::mark(
# dplyr_a(),
dt_a(),
duck_a(),
arrow_a(),
iterations = 10,
check = FALSE
)

#A tibble: 3 × 13
#expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory               time       gc      
#<bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>               <list>     <list>  
#  1 dt_a()        139ms    146ms      6.88   75.57MB    6.88      5     5   726.78ms <NULL> <Rprofmem [108 × 3]> <bench_tm> <tibble>
#  2 duck_a()      184ms    187ms      5.29    1.26MB    5.29      5     5   946.05ms <NULL> <Rprofmem>           <bench_tm> <tibble>
#  3 arrow_a()     112ms    113ms      8.82  169.88KB    0.980     9     1      1.02s <NULL> <Rprofmem [428 × 3]> <bench_tm> <tibble>
#  > 
