package_folder <- "C:\\Users\\mclef\\Documents\\Research\\Clinical Trials\\Package"

initiate_trials_file <- "Initiate Clinical Trials.R"

initiate_trials_path <- paste0(package_folder, "\\", initiate_trials_file)

source(initiate_trials_path)


Trial_1 <- MultisiteTrial(NSimulations = 10
                          
                          , RecruiteTarget = 600
                          
                          , Randomization = "center_unstratified"
                          , BlockLength = 4
                          
                          , NArms = 2
                          , CentersInRegion = c(4,4,4,12,6)
                          
                          , ManufactureTime = 0
                          , ManufactureToDepotTime = c(3,20,15,10,8)
                          , DepotToCenterTime = rep(1,30)
                          , DaysPatientsWait = 3
                          , WhenToPredictRegion = 50
                          
                          , InitialManufactureSupply = 30000
                          , InitialDepotSupply = c(4,4,4,12,6) * 10
                          , InitialCenterSupply = rep(2,30)
                          
                          , DepotOrderQuantity = 50
                          , CenterOrderQuantity = 1
                          , CenterOrderThreshold = 10
                          , StockoutRiskLevel = .05
                          
                          , ManufactureToDepotPerOrderCost = c(1, 4, 1.5, 1.5, 1) * 10000
                          , ManufactureToDepotPerPackageCost = c(2, 5, 7.5, 5, 4) * 100
                          
                          , MeanArrivalRates = c(0.02,0.04,0.05,0.08,
                                                 
                                                 0.03,0.06,0.06,0.28,
                                                 
                                                 0.02,0.04,0.05,0.06,
                                                 
                                                 0.03,0.04,0.05,0.06,0.08,
                                                 0.08,0.11,0.11,0.14,
                                                 0.14,0.14,0.16,0.18,
                                                 
                                                 0.01,0.02,0.04,0.04,0.06)
                          
                          , VerboseSimulation = TRUE)


Trial_1