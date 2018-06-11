context("ggd_plot")

context("ggd_create_phylos")

test_that("use", {

  df <- get_daisie_data("Galapagos_datatable.RData")
  phylos <- ggd_create_phylos(df, island_age = 4)
  ggd_plot(phylos)
})

test_that("use Non_endemic", {

  island_age <- 4
  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Status == "Non_endemic", ]
  df <- rbind(df, df)
  df$Branching_times[2] <- 3
  phylos <- ggd_create_phylos(df, island_age = island_age)
  ggd_plot(phylos)
})

test_that("use Endemic", {

  island_age <- 4
  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Status == "Endemic", ]
  phylos <- ggd_create_phylos(df, island_age = island_age)
  ggd_plot(phylos)
})

test_that("use Endemic, Finches", {

  island_age <- 4
  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Clade_name == "Finches", ]
  phylos <- ggd_create_phylos(df, island_age = island_age)
  ggd_plot(phylos)
})

test_that("use Endemic, Mimus", {

  island_age <- 4
  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Clade_name == "Mimus", ]
  phylos <- ggd_create_phylos(df, island_age = island_age)
  ggd_plot(phylos)
})

test_that("use Endemic, Mimus sub", {

  island_age <- 4
  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Clade_name == "Mimus", ]
  df$Branching_times[1] <- "3.958,3.422,2.884,0.459"
  df$Branching_times[1] <- "3.958,3.422,2.884"
  df$Branching_times[1] <- "3.958,3.422"
  phylos <- ggd_create_phylos(df, island_age = island_age)
  #branchlist <- ape::which.edge(phylo, 3)
  #phylos[[1]]$edge[,2] %in% which(phylos[[1]]$tip.label == "X")
  ggd_plot(phylos)
})

test_that("use Endemic, Zenaida", {

  island_age <- 4
  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Clade_name == "Zenaida", ]
  phylos <- ggd_create_phylos(df, island_age = island_age)
  ggd_plot(phylos)
})
