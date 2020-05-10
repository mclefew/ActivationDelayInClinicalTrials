MultisiteTrial <- function(NSimulations
                           
                           , RecruiteTarget
                           
                           , Randomization
                           , BlockLength
                           
                           , NArms
                           , CentersInRegion
                           
                           , ManufactureTime
                           , ManufactureToDepotTime
                           , DepotToCenterTime
                           , DaysPatientsWait
                           , WhenToPredictRegion
                           
                           , InitialManufactureSupply
                           , InitialDepotSupply
                           , InitialCenterSupply
                           
                           , DepotOrderQuantity
                           , CenterOrderQuantity
                           , CenterOrderThreshold
                           , StockoutRiskLevel
                           
                           , ManufactureToDepotPerOrderCost
                           , ManufactureToDepotPerPackageCost
                           
                           , MeanArrivalRates
                           
                           , VerboseSimulation
                           , RandomizationSeed = 42
) {
  
  
  
  NCenters <- sum(CentersInRegion)
  NRegions <- length(CentersInRegion)
  
  
  
  Centers <- paste0("center",(1:NCenters)-1)
  CentersAndDrugs <- c(sapply(Centers,
                              function(center) 
                                paste0(center,"_drug",seq(NArms))
  )
  )
  
  
  
  Depots <- paste0("depot",(1:NRegions)-1)
  DepotsAndDrugs <- sapply(Depots,
                           function(center)
                             paste0(center,"_drug",seq(NArms))
  )
  DepotInfo <- as.data.frame(cbind(Depots
                                   , ManufactureToDepotTime)
  )
  
  
  
  CenterDepotMapping <- cbind(Centers
                              , unlist(sapply(seq(NRegions)-1,
                                              function(Region) 
                                                rep( paste0("depot",Region)
                                                     , CentersInRegion[[Region+1]])
                              )
                              )
  )
  colnames(CenterDepotMapping) <- c("center","depot")
  
  
  TreatmentSeq <- GenerateTreatmentSequence(Randomization = Randomization
                                            , BlockLength = BlockLength
                                            , NArms = NArms
                                            , RecruiteTarget = RecruiteTarget
                                            , NCenters = NCenters
                                            , RandomizationSeed = RandomizationSeed)
  
  
  CenterStates <- InstantiateCenterStates(InitialCenterSupply = InitialCenterSupply
                                            , NArms = NArms
                                            , CentersAndDrugs = CentersAndDrugs
                                            , NCenters = NCenters
                                            , DaysPatientsWait = DaysPatientsWait
                                            , Centers = Centers
                                            , DepotToCenterTime = DepotToCenterTime
                                            , CenterOrderThreshold = CenterOrderThreshold
                                            , CenterOrderQuantity = CenterOrderQuantity)
  
  CenterOrders <- InstantiateCenterOrders()
  
  
  DepotStates <- InstantiateDepotStates(InitialDepotSupply = InitialDepotSupply
                                        , NArms = NArms
                                        , DepotsAndDrugs = DepotsAndDrugs
                                        , ManufactureToDepotTime = ManufactureToDepotTime
                                        , Depots = Depots)
  
  DepotOrders <- InstantiateDepotOrders()
  
  
  TrialDay <- 0
  RecruitedTotal <- 0
  
  
  Trial <- list(NSimulations = NSimulations
                
                , RecruiteTarget = RecruiteTarget
                
                , Randomization = Randomization
                , BlockLength = BlockLength
                , TreatmentSeq = TreatmentSeq
                
                , NArms = NArms
                , CentersInRegion = CentersInRegion
                , NCenters = NCenters
                , NRegions = NRegions
                
                , Centers = Centers
                , CentersAndDrugs = CentersAndDrugs
                , Depots = Depots
                , DepotsAndDrugs = DepotsAndDrugs
                , DepotInfo = DepotInfo
                , CenterDepotMapping = CenterDepotMapping
                
                
                , ManufactureTime = ManufactureTime
                , ManufactureToDepotTime = ManufactureToDepotTime
                , DepotToCenterTime = DepotToCenterTime
                , DaysPatientsWait = DaysPatientsWait
                , WhenToPredictRegion = WhenToPredictRegion
                
                , InitialManufactureSupply = InitialManufactureSupply
                , InitialDepotSupply = InitialDepotSupply
                , InitialCenterSupply = InitialCenterSupply
                
                , DepotOrderQuantity = DepotOrderQuantity
                , CenterOrderQuantity = CenterOrderQuantity
                , CenterOrderThreshold = CenterOrderThreshold
                , StockoutRiskLevel = StockoutRiskLevel
                
                , ManufactureToDepotPerOrderCost = ManufactureToDepotPerOrderCost
                , ManufactureToDepotPerPackageCost = ManufactureToDepotPerPackageCost
                
                , MeanArrivalRates = MeanArrivalRates
                
                , CenterStates = CenterStates
                , CenterOrders = CenterOrders
                , DepotStates = DepotStates
                , DepotOrders = DepotOrders
                
                , TrialDay = TrialDay
                , RecruitedTotal = RecruitedTotal
                
                , VerboseSimulation = VerboseSimulation
                , RandomizationSeed = RandomizationSeed
                )
  
  
  
  class(Trial) <- append(class(Trial),"MultisiteTrial")
  
  return(Trial)  
  
}




GenerateTreatmentSequence <- function(Randomization
                                      , BlockLength
                                      , NArms
                                      , RecruiteTarget
                                      , NCenters
                                      , RandomizationSeed){
  
  
  if(Randomization == "center_unstratified"){
    PermutedBlockRandomization <- randomizeR::pbrPar(bc=BlockLength
                                                     , K=NArms)
    RandomizedSequence <- randomizeR::genSeq(obj=PermutedBlockRandomization
                                             , r=RecruiteTarget*2
                                             , seed = RandomizationSeed)
    TreatmentSeq <- as.vector(t(RandomizedSequence$M))+1
  } else{
    
    PermutedBlockRandomization <- randomizeR::pbrPar(bc=BlockLength
                                                     , K=NArms)
    RandomizedSequence <- randomizeR::genSeq(obj=PermutedBlockRandomization
                                             , r=RecruiteTarget*NCenters/BlockLength
                                             , seed=RandomizationSeed)
    TreatmentSeq <- data.frame(matrix(as.vector(t(RandomizedSequence$M))+1
                                      ,ncol=NCenters))
    colnames(TreatmentSeq) <- Centers
  }
  
  return(TreatmentSeq)
  
}



InstantiateCenterStates <- function(InitialCenterSupply
                                      , NArms
                                      , CentersAndDrugs
                                      , NCenters
                                      , DaysPatientsWait
                                      , Centers
                                      , DepotToCenterTime
                                      , CenterOrderThreshold
                                      , CenterOrderQuantity
                                      ){
  
  SupplyByCenter <- as.data.frame(t(rep(InitialCenterSupply,each=NArms)))
  colnames(SupplyByCenter) <- CentersAndDrugs
  
  
  WaitingPatients <- as.data.frame(matrix(0,ncol=NCenters*NArms,nrow=DaysPatientsWait))
  colnames(WaitingPatients) <- CentersAndDrugs
  
  
  RejectionByCenter <- as.data.frame(t(rep(0,NCenters)))
  colnames(RejectionByCenter) <- Centers
  
  
  RecruitmentByCenter <- as.data.frame(t(rep(0,NCenters)))
  colnames(RecruitmentByCenter) <- Centers
  

  DepotToCenter <- as.data.frame(t(DepotToCenterTime))
  colnames(DepotToCenter) <- Centers
  
  
  ArrivalsByCenter <- as.data.frame(t(rep(0,NCenters)))
  colnames(ArrivalsByCenter) <- Centers
  

  CenterStates <- list('supply'=SupplyByCenter,
                        'waiting'=WaitingPatients,
                        'recruitment'=RecruitmentByCenter,
                        'rejection'=RejectionByCenter,
                        'depot_to_center'=DepotToCenter,
                        'threshold'=CenterOrderThreshold,
                        'order_quantity'=CenterOrderQuantity,
                        'arrivals'=ArrivalsByCenter)
  
  
  return(CenterStates)
  
}



InstantiateCenterOrders <- function(){
  return(data.frame('center_drug'=character(),
                    'quantity'=numeric(),
                    'days_waiting'=numeric(),
                    'days_to_arrive'=numeric()
  ))
}



InstantiateDepotStates <- function(InitialDepotSupply
                                   , NArms
                                   , DepotsAndDrugs
                                   , ManufactureToDepotTime
                                   , Depots
                                   ){
  
  SupplyByDepot <- as.data.frame(t(rep(InitialDepotSupply,each=NArms)))
  colnames(SupplyByDepot) <- DepotsAndDrugs
  
  
  ManufactureToDepot <- as.data.frame(t(ManufactureToDepotTime))
  colnames(ManufactureToDepot) <- Depots
  
  
  TotalShipmentsOrdered <- as.data.frame(t(rep(1,length(Depots))))
  colnames(TotalShipmentsOrdered) <- Depots
  
  
  TotalPackagesOrdered <- as.data.frame(t(InitialDepotSupply*NArms))
  colnames(TotalPackagesOrdered) <- Depots
  
  
  DepotStates <- list('supply'=SupplyByDepot,
                       'manufacture_to_depot'=ManufactureToDepot,
                       'total_shipments_ordered'=TotalShipmentsOrdered,
                       'total_packages_ordered'=TotalPackagesOrdered)
  
  
  return(DepotStates)
  
}




InstantiateDepotOrders <- function(){
  return(data.frame('depot_drug'=character(),
                    'quantity'=numeric(),
                    'days_waiting'=numeric(),
                    'days_to_arrive'=numeric()
  ))
}
