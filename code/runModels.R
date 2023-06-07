#devtools::install_github("Alessandra23/pineR")
library(pineR)

# Run the models for weighted data ans save the results

# set up the depth
depth <- 1

Hortland <- quantileValues(data = readWeightedData$Hortland, N = 50, npop = 20, ntimes = 10, ngen = 1,
                           species = 1, depth = depth, correction = F)
Ballinagee <- quantileValues(data = readWeightedData$Ballinagee, N = 50, npop = 20, ntimes = 10, ngen = 1,
                             species = 2, depth = depth, correction = F)
Oakwood <- quantileValues(data = readWeightedData$Oakwood, N = 50, npop = 20, ntimes = 10, ngen = 1,
                          species = 1, depth = depth, correction = F)
Glendine1 <- quantileValues(data = readWeightedData$Glendine1, N = 50, npop = 20, ntimes = 10, ngen = 1,
                         species = 1, depth = depth, correction = F)
Glendine2 <- quantileValues(data = readWeightedData$Glendine2, N = 50, npop = 20, ntimes = 10, ngen = 1,
                         species = 1, depth = depth, correction = F)
Lackenrea1 <- quantileValues(data = readWeightedData$Lackenrea1, N = 50, npop = 20, ntimes = 10, ngen = 1,
                          species = 1, depth = depth, correction = F)
Lackenrea2 <- quantileValues(data = readWeightedData$Lackenrea2, N = 50, npop = 20, ntimes = 10, ngen = 1,
                          species = 1, depth = depth, correction = F)
Summerhill <- quantileValues(data = readWeightedData$Summerhill, N = 50, npop = 20, ntimes = 10, ngen = 1,
                          species = 1, depth = depth, correction = F)
Deerpark <- quantileValues(data = readWeightedData$Deerpark, N = 50, npop = 20, ntimes = 10, ngen = 1,
                        species = 2, depth = depth, correction = F)
Ballymacshaneboy <- quantileValues(data = readWeightedData$Ballymacshaneboy, N = 50, npop = 20, ntimes = 10, ngen = 1,
                                species = 1, depth = depth, correction = F)
Kilduff <- quantileValues(data = readWeightedData$Kilduff, N = 50, npop = 20, ntimes = 10, ngen = 1,
                       species = 1, depth = depth, correction = F)
Rossnagad <- quantileValues(data = readWeightedData$Rossnagad, N = 50, npop = 20, ntimes = 10, ngen = 1,
                         species = 1, depth = depth, correction = F)
Ballyroan1 <- quantileValues(data = readWeightedData$Ballyroan1, N = 50, npop = 20, ntimes = 10, ngen = 1,
                          species = 1, depth = depth, correction = F)
Ballyroan2 <- quantileValues(data = readWeightedData$Ballyroan2, N = 50, npop = 20, ntimes = 10, ngen = 1,
                          species = 1, depth = depth, correction = F)
Donadea <- quantileValues(data = readWeightedData$Donadea, N = 50, npop = 20, ntimes = 10, ngen = 1,
                       species = 1, depth = depth, correction = F)
Cloondara <- quantileValues(data = readWeightedData$Cloondara, N = 50, npop = 20, ntimes = 10, ngen = 1,
                         species = 1, depth = depth, correction = F)
Knockaville <- quantileValues(data = readWeightedData$Knockaville, N = 50, npop = 20, ntimes = 10, ngen = 1,
                           species = 1, depth = depth, correction = F)
Killurney <- quantileValues(data = readWeightedData$Killurney, N = 50, npop = 20, ntimes = 10, ngen = 1,
                         species = 1, depth = depth, correction = F)
Doon <- quantileValues(data = readWeightedData$Doon, N = 50, npop = 20, ntimes = 10, ngen = 1,
                    species = 1, depth = depth, correction = F)
Clonoghil <- quantileValues(data = readWeightedData$Clonoghil, N = 50, npop = 20, ntimes = 10, ngen = 1,
                         species = 1, depth = depth, correction = F)
Tigroney <- quantileValues(data = readWeightedData$Tigroney, N = 50, npop = 20, ntimes = 10, ngen = 1,
                        species = 1, depth = depth, correction = F)
Gurtnapisha <- quantileValues(data = readWeightedData$Gurtnapisha, N = 50, npop = 20, ntimes = 10, ngen = 1,
                           species = 1, depth = depth, correction = F)
Rickardstown <- quantileValues(data = readWeightedData$Rickardstown, N = 50, npop = 20, ntimes = 10, ngen = 1,
                            species = 1, depth = depth, correction = F)
Longfordpass <- quantileValues(data = readWeightedData$Longfordpass, N = 50, npop = 20, ntimes = 10, ngen = 1,
                            species = 1, depth = depth, correction = F)
Cashelduff <- quantileValues(data = readWeightedData$Cashelduff, N = 50, npop = 20, ntimes = 10, ngen = 1,
                          species = 1, depth = depth, correction = F)
Woodford <- quantileValues(data = readWeightedData$Woodford, N = 50, npop = 20, ntimes = 10, ngen = 1,
                        species = 1, depth = depth, correction = F)
Ballybrittas <- quantileValues(data = readWeightedData$Ballybrittas, N = 50, npop = 20, ntimes = 10, ngen = 1,
                            species = 1, depth = depth, correction = F)

# set the enf of the name as the depth  (runs_weighted_depth_1 or runs_weighted_depth_2 or runs_weighted_depth_3)
runs_weighted_depth_3 <- list(Hortland = Hortland ,
                              Ballinagee = Ballinagee,
                              Oakwood = Oakwood ,
                              Glendine1 = Glendine1,
                              Glendine2 = Glendine2 ,
                              Lackenrea1 = Lackenrea1,
                              Lackenrea2 = Lackenrea2,
                              Summerhill = Summerhill,
                              Deerpark = Deerpark,
                              Ballymacshaneboy = Ballymacshaneboy,
                              Kilduff = Kilduff,
                              Rossnagad = Rossnagad,
                              Ballyroan1 = Ballyroan1,
                              Ballyroan2 = Ballyroan2,
                              Donadea = Donadea,
                              Cloondara = Cloondara,
                              Knockaville = Knockaville,
                              Killurney = Killurney,
                              Doon = Doon,
                              Clonoghil = Clonoghil,
                              Tigroney = Tigroney,
                              Gurtnapisha = Gurtnapisha,
                              Rickardstown = Rickardstown,
                              Longfordpass = Longfordpass,
                              Cashelduff = Cashelduff,
                              Woodford = Woodford,
                              Ballybrittas = Ballybrittas
                              )

#save(runs_weighted_depth_3, file="data/run models/runs_weighted_depth_3.Rda")




