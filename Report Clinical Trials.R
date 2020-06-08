ReportMultisiteTrial.MultisiteTrial <- function(TrialInstance){

  result_cols <-    c("Supply.Leftover","Rejected.Total","Recruited.Total","Total.Shipments.Ordered",
                      "Total.Packages.Ordered","Trial.Days.Taken","Recruitment.Target","Randomization",
                      "Stockout.Risk.Level","Center.Order.Threshold","Initial.Depot.Supply",
                      "Depot.Order.Quantity","Days.Patients.Wait","When.to.Predict.Region",
                      "Number.of.Arms","Total.Cost")
  
  results_df <- data.frame(matrix(ncol=length(result_cols),nrow=1))
  colnames(results_df) <- result_cols
  
  results_df[["Supply.Leftover"]] <- TrialInstance$SupplyLeftover
  results_df[["Rejected.Total"]] <- TrialInstance$RejectedTotal
  results_df[["Recruited.Total"]] <- TrialInstance$RecruitedTotal
  results_df[["Total.Shipments.Ordered"]] <- sum(TrialInstance$DepotStates$total_shipments_ordered)
  results_df[["Total.Packages.Ordered"]] <- sum(TrialInstance$DepotStates$total_packages_ordered)
  results_df[["Trial.Days.Taken"]] <- TrialInstance$TrialDay
  results_df[["Recruitment.Target"]] <- TrialInstance$RecruiteTarget
  results_df[["Randomization"]] <- TrialInstance$Randomization
  results_df[["Stockout.Risk.Level"]] <- TrialInstance$StockoutRiskLevel
  results_df[["Center.Order.Threshold"]] <- TrialInstance$CenterOrderThreshold
  results_df[["Initial.Depot.Supply"]] <- sum(TrialInstance$InitialDepotSupply)
  results_df[["Depot.Order.Quantity"]] <- TrialInstance$DepotOrderQuantity
  results_df[["Days.Patients.Wait"]] <- TrialInstance$DaysPatientsWait
  results_df[["When.to.Predict.Region"]] <- TrialInstance$WhenToPredictRegion
  results_df[["Number.of.Arms"]] <- TrialInstance$NArms
  results_df["Total.Cost"] <- sum(TrialInstance$DepotStates$total_shipments_ordered * 
                              TrialInstance$ManufactureToDepotPerOrderCost) +
                              sum(TrialInstance$DepotStates$total_packages_ordered * 
                              TrialInstance$ManufactureToDepotPerPackageCost)
  
  return(results_df)
  
}