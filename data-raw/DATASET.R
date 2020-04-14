## code to prepare `DATASET` dataset goes here

vertex4 <- tibble::tibble(
  degseqx = list(
    c(0,0,0,0),
    c(0,0,1,1),
    c(0,1,1,2),
    c(1,1,1,1),
    c(0,2,2,2),
    c(1,1,1,3),
    c(1,1,1,2),
    c(1,2,2,3),
    c(2,2,2,2),
    c(2,2,3,3),
    c(3,3,3,3)
  ),
  combinations = c(
    1,
    6,
    12,
    3,
    4,
    4,
    12,
    12,
    3,
    6,
    1
  )
)


usethis::use_data(vertex4)
