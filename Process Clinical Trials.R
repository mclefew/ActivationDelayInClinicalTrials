ProcessMultisiteTrial.MultisiteTrial <- function(TrialInstance){

  
  while(TrialInstance$RecruitedTotal < TrialInstance$RecruiteTarget){
    
    
    TrialInstance$TrialDay <- TrialInstance$TrialDay + 1
    

        
    ArrivalsByCenter <- sapply(TrialInstance$MeanArrivalRates,
                                 function(l) rpois(1,l))
    
    TrialInstance$DepotStates <- Depot_Order_Arrivals_Update_Supply(TrialInstance$DepotStates
                                                                    , TrialInstance$DepotOrders)
    
    if(TrialInstance$TrialDay > TrialInstance$WhenToPredictRegion){
      try(TrialInstance$DepotOrders <- Update_DepotOrders(TrialInstance$DepotStates
                                                           , TrialInstance$DepotOrders
                                                           , TrialInstance$CenterStates),
          silent = TRUE)
    }
    
    
    
    
    TrialInstance$CenterStates <- Center_Order_Arrivals_Update_Supply(TrialInstance$CenterStates
                                                                      , TrialInstance$CenterOrders)
    TrialInstance$CenterOrders <- Update_Center_Orders(TrialInstance$CenterStates
                                                       , TrialInstance$CenterOrders
                                                       , TrialInstance$DepotStates
                                                       , TrialInstance$CenterDepotMapping)
    TrialInstance$DepotStates  <- Center_Orders_Update_Depot_Supply(TrialInstance$CenterOrders
                                                                    , TrialInstance$DepotStates
                                                                    , TrialInstance$CenterDepotMapping)
    TrialInstance$CenterStates <- attend_waiting_patients(TrialInstance$CenterStates)
    TrialInstance$CenterStates <- update_waiting_patients(TrialInstance$CenterStates)
    
    
    
    for(center in TrialInstance$Centers){
      
      TrialInstance$CenterStates <- attend_arrivals(center=center
                                                    , arrivals=ArrivalsByCenter[which(centers==center)]
                                                    , CenterStates=TrialInstance$CenterStates)
      TrialInstance$RecruitedTotal <- sum(unlist(TrialInstance$CenterStates['recruitment']))  
      
    }
    
  }
  

  
    
  TrialInstance$SupplyLeftover <- sum(TrialInstance$DepotStates$supply) + sum(TrialInstance$CenterStates$supply)
  TrialInstance$RejectedTotal <- sum(TrialInstance$CenterStates$rejection)
  
  
  
  return(TrialInstance)
  
}




Depot_Order_Arrivals_Update_Supply <- function(Depot_States,Depot_Orders){
  
  
  supply_by_depot <- Depot_States[['supply']]
  manufacture_to_depot <- Depot_States[['manufacture_to_depot']]
  total_shipments_ordered <- Depot_States[['total_shipments_ordered']]
  total_packages_ordered <- Depot_States[['total_packages_ordered']]
  
  orders_depot_drug <- Depot_Orders[['depot_drug']]
  orders_days_waiting <- Depot_Orders[['days_waiting']]
  orders_days_to_arrive <- Depot_Orders[['days_to_arrive']]
  orders_quantity <- Depot_Orders[['quantity']]
  
  orders_days_waiting[orders_days_waiting >= 0] <- orders_days_waiting[
    orders_days_waiting >= 0] + 1
  orders_arriving <- orders_days_waiting == orders_days_to_arrive
  if(sum(orders_arriving) > 0){
    for(order_index in seq(nrow(Depot_Orders[orders_arriving,]))){
      order <- Depot_Orders[orders_arriving,][order_index,]
      supply_by_depot[as.character(order[['depot_drug']]) ] <- supply_by_depot[
        order[['depot_drug']] ] +
        order[['quantity']]
    }
  }
  
  depot_drug_new_orders <- as.character(Depot_Orders$depot_drug[Depot_Orders$days_waiting==0])
  
  if(length(depot_drug_new_orders) > 0){
    
    depot_new_orders <- sapply(depot_drug_new_orders, function(depot_drug) depot_from_depot_drug(depot_drug))
    
    #if multiple drugs are shipped on the same day, only count one shipment
    for(depot in unique(depot_new_orders)){
      total_shipments_ordered[depot] <- total_shipments_ordered[depot] + 1
    }
    
    order_quantity_new_orders <- Depot_Orders$quantity[Depot_Orders$days_waiting==0]
    
    for(i in seq(length(order_quantity_new_orders))){
      total_packages_ordered[depot_new_orders[i]] <- total_packages_ordered[depot_new_orders[i]] + order_quantity_new_orders[i]
    }
    
  }
  
  Updated_Depot_States <- list('supply'=supply_by_depot,
                               'manufacture_to_depot'=manufacture_to_depot,
                               'total_shipments_ordered'=total_shipments_ordered,
                               'total_packages_ordered'=total_packages_ordered)
  
  return(Updated_Depot_States)
  
}





Update_Depot_Orders <- function(Depot_States,Depot_Orders,Center_States){
  
  recruitment_by_center <- Center_States[['recruitment']]
  
  supply_by_depot <- Depot_States[['supply']]
  manufacture_to_depot <- Depot_States[['manufacture_to_depot']]
  
  orders_depot_drug <- Depot_Orders[['depot_drug']]
  orders_days_waiting <- Depot_Orders[['days_waiting']]
  orders_days_to_arrive <- Depot_Orders[['days_to_arrive']]
  orders_quantity <- Depot_Orders[['quantity']]
  
  orders_days_waiting[orders_days_waiting >= 0] <- orders_days_waiting[orders_days_waiting >= 0] + 1
  
  Depot_Orders[['days_waiting']] <- orders_days_waiting
  
  orders_arriving <- orders_days_waiting == orders_days_to_arrive
  
  Updated_Depot_Orders <- Depot_Orders[!orders_arriving,]
  
  if(nrow(Updated_Depot_Orders)==0){
    total_on_order <- 0
  } else{
    total_on_order <- sapply(colnames(supply_by_depot),
                             function(depot_drug){
                               #for depot_drug combination, if drug needs
                               #to on order for depot, total order quantity
                               if(depot_drug %in%
                                  unique(
                                    Updated_Depot_Orders[['center_drug']])){
                                 return(
                                   sum(
                                     Updated_Depot_Orders[
                                       Updated_Depot_Orders[['depot_drug']]==
                                         depot_drug,][['quantity']])
                                 )
                               } else{
                                 return(0)
                               } 
                             })
  }
  
  supply_plus_order <- supply_by_depot + total_on_order
  
  
  depot_drug_new_orders <- colnames(supply_by_depot)[sapply(colnames(supply_by_depot),
                                                            function(depot_drug){
                                                              depot <- strsplit(depot_drug,"_")[[1]][1]
                                                              prediction_interval <- Depot_States$manufacture_to_depot[depot][[1]]
                                                              
                                                              predicted_demand <-  predict_recruitments_treatment(
                                                                recruited_total=recruited_total,
                                                                M=N,
                                                                Ms=centers_in_region[depots==depot],
                                                                alpha=estimate_alpha(recruited_by_center_total = Center_States$arrivals),
                                                                tau=trial_day,
                                                                forward_interval=Depot_States$manufacture_to_depot[[depot]],
                                                                precision=.0000001,
                                                                stockout_risk = stockout_risk)
                                                              
                                                              
                                                              supply <- Depot_States$supply[depot_drug]
                                                              on_order <- sum(
                                                                Depot_Orders$days_waiting[Depot_Orders$depot_drug == depot_drug])
                                                              
                                                              order_more <- predicted_demand >= supply + on_order
                                                              
                                                              return(order_more)
                                                            })]
  
  
  if(length(depot_drug_new_orders) > 0){
    
    days_waiting_new_orders <- rep(0,length(depot_drug_new_orders))
    

    days_to_arrive_new_orders <- sapply(depot_drug_new_orders,
                                        function(depot_drug){
                                          depot <-
                                            strsplit(depot_drug,'_')[[1]][1]
                                          return(manufacture_to_depot[[depot]])
                                        })
    
    
    New_Orders <- data.frame('depot_drug'=depot_drug_new_orders,
                             'quantity'=depot_order_quantity,
                             'days_waiting'= days_waiting_new_orders,
                             'days_to_arrive'=days_to_arrive_new_orders)
    
    
    
  } else{
    New_Orders <- NULL
  }
  
  Updated_Depot_Orders <- rbind(Updated_Depot_Orders,New_Orders)
  
  return(Updated_Depot_Orders)
  
}




Update_Center_Orders <- function(Center_States,Center_Orders,Depot_States,center_depot_mapping){
  
  supply_by_center <- Center_States[['supply']]
  waiting_patients <- Center_States[['waiting']]
  recruitment_by_center <- Center_States[['recruitment']]
  rejection_by_center <- Center_States[['rejection']]
  depot_to_center <- Center_States[['depot_to_center']]
  supply_threshold <- Center_States[['threshold']]
  center_order_quantity <- Center_States[['order_quantity']]
  
  
  supply_by_depot <- Depot_States[['supply']]
  manufacture_to_depot <- Depot_States[['manufacture_to_depot']]
  
  orders_center_drug <- Center_Orders[['center_drug']]
  orders_days_waiting <- Center_Orders[['days_waiting']]
  orders_days_to_arrive <- Center_Orders[['days_to_arrive']]
  orders_quantity <- Center_Orders[['quantity']]
  
  orders_days_waiting[orders_days_waiting >= 0] <- 
    orders_days_waiting[orders_days_waiting >= 0] + 1
  
  
  
  check_depot_supply <- Center_Orders[orders_days_waiting < 0,]
  if(nrow(check_depot_supply) > 0){
    for(order_index in seq(nrow(check_depot_supply))){
      center_drug <- as.character(check_depot_supply[['center_drug']][order_index])
      drug_available <- depot_supply_available(center_drug,
                                               check_depot_supply[['quantity']][order_index],
                                               supply_by_depot, 
                                               center_depot_mapping)
      
      if(drug_available){
        check_depot_supply[['days_waiting']][order_index] <- 0
        supply_by_depot[[center_drug_to_depot_drug(center_drug)]] <- 
          supply_by_depot[[center_drug_to_depot_drug(center_drug)]] - check_depot_supply[[
            'quantity']][order_index]
      }
    }  
  }
  
  orders_days_waiting[orders_days_waiting < 0] <- check_depot_supply[['days_waiting']]
  Center_Orders[['days_waiting']] <- orders_days_waiting
  
  orders_arriving <- orders_days_waiting == orders_days_to_arrive
  
  Updated_Center_Orders <- Center_Orders[!orders_arriving,]
  
  if(nrow(Updated_Center_Orders)==0){
    total_on_order <- 0
  } else{
    total_on_order <- sapply(colnames(supply_by_center),
                             function(center_drug){
                               #for center_drug combination, if drug needs
                               #to on order for center, total order quantity
                               if(center_drug %in%
                                  unique(
                                    Updated_Center_Orders[['center_drug']])){
                                 return(
                                   sum(
                                     Updated_Center_Orders[
                                       Updated_Center_Orders[['center_drug']]==
                                         center_drug,][['quantity']])
                                 )
                               } else{
                                 return(0)
                               } 
                             })
  }
  
  supply_plus_order <- supply_by_center + total_on_order
  
  center_drug_new_orders <- colnames(supply_by_center)[supply_plus_order <=
                                                         supply_threshold]
  
  if(length(center_drug_new_orders) > 0){
    
    days_waiting_new_orders <- rep(0,length(center_drug_new_orders))
    
    for(order_index in seq(length(center_drug_new_orders))){
      center_drug <- center_drug_new_orders[order_index]
      if(depot_supply_available(center_drug,
                                center_order_quantity,
                                supply_by_depot, 
                                center_depot_mapping)){
        depot_drug <- center_drug_to_depot_drug(center_drug, center_depot_mapping)
        supply_by_depot[depot_drug] <- supply_by_depot[depot_drug] -
          center_order_quantity
      } else{
        days_waiting_new_orders[order_index] <- -1
      }
    }
    
    days_to_arrive_new_orders <- sapply(center_drug_new_orders,
                                        function(center_drug){
                                          center <-
                                            strsplit(center_drug,'_')[[1]][1]
                                          return(depot_to_center[[center]])
                                        })
    
    
    New_Orders <- data.frame('center_drug'=center_drug_new_orders,
                             'quantity'=supply_threshold - sapply(center_drug_new_orders
                                                                  , function(center_drug) 
                                                                    Center_States$supply[[center_drug]]),
                             'days_waiting'= days_waiting_new_orders,
                             'days_to_arrive'=days_to_arrive_new_orders)
  } else{
    New_Orders <- NULL
  }
  
  Updated_Center_Orders <- rbind(Updated_Center_Orders,New_Orders)
  
  return(Updated_Center_Orders)
  
}



depot_supply_available <- function(center_drug, quantity,supply_by_depot, center_depot_mapping) {
  depot_drug <- center_drug_to_depot_drug(center_drug, center_depot_mapping)
  if(supply_by_depot[depot_drug] >= quantity){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


center_drug_to_depot_drug <- function(center_drug, center_depot_mapping){
  center <- center_from_center_drug(center_drug)
  drug <- drug_from_center_drug(center_drug)
  depot_for_center <- lookup_depot(center, center_depot_mapping)
  depot_drug <- paste0(depot_for_center,"_",drug)
  return(depot_drug)
}

center_from_center_drug <- function(center_drug){
  return(strsplit(center_drug,'_')[[1]][1])
}

drug_from_center_drug <- function(center_drug){
  return(strsplit(center_drug,'_')[[1]][2])
}

lookup_depot <- function(center, center_depot_mapping){
  
  return(center_depot_mapping[center_depot_mapping[,1]==center,2])
}




Center_Orders_Update_Depot_Supply <- function(Center_Orders,Depot_States, center_depot_mapping){
  
  supply_by_depot <- Depot_States[['supply']]
  manufacture_to_depot <- Depot_States[['manufacture_to_depot']]
  total_shipments_ordered <- Depot_States[['total_shipments_ordered']]
  total_packages_ordered <- Depot_States[['total_packages_ordered']]
  
  new_orders <- Center_Orders[Center_Orders[['days_waiting']] == 0,]
  if(nrow(new_orders)>0){
    for(order_index in seq(nrow(new_orders))){
      center_drug <- as.character(new_orders[['center_drug']][order_index])
      depot_drug <- center_drug_to_depot_drug(center_drug, center_depot_mapping)
      quantity <- new_orders[['quantity']][order_index]
      supply_by_depot[depot_drug] <- supply_by_depot[depot_drug] - quantity
    }
  }
  
  Updated_Depot_States <- list('supply'=supply_by_depot,
                               'manufacture_to_depot'=manufacture_to_depot,
                               'total_shipments_ordered'=total_shipments_ordered,
                               'total_packages_ordered'=total_packages_ordered)
  
  return(Updated_Depot_States)
}



Center_Order_Arrivals_Update_Supply <- function(Center_States,Center_Orders){
  supply_by_center <- Center_States[['supply']]
  waiting_patients <- Center_States[['waiting']]
  recruitment_by_center <- Center_States[['recruitment']]
  rejection_by_center <- Center_States[['rejection']]
  depot_to_center <- Center_States[['depot_to_center']]
  supply_threshold <- Center_States[['threshold']]
  center_order_quantity <- Center_States[['order_quantity']]
  center_order_threshold <- Center_States[['threshold']]
  total_arrivals_by_center <- Center_States[['arrivals']]
  
  orders_center_drug <- Center_Orders[['center_drug']]
  orders_days_waiting <- Center_Orders[['days_waiting']]
  orders_days_to_arrive <- Center_Orders[['days_to_arrive']]
  orders_quantity <- Center_Orders[['quantity']]
  
  orders_days_waiting <- orders_days_waiting + 1
  orders_arriving <- orders_days_waiting == orders_days_to_arrive
  
  if(sum(orders_arriving) > 0){
    for(order_index in seq(sum(orders_arriving))){
      order <- Center_Orders[orders_arriving,][order_index,]
      supply_by_center[as.character(order[['center_drug']]) ] <- supply_by_center[
        as.character(order[['center_drug']]) ] +
        order[['quantity']]
    }
  }
  
  Updated_Center_States <- list('supply'=supply_by_center,
                                'waiting'=waiting_patients,
                                'recruitment'=recruitment_by_center,
                                'rejection'=rejection_by_center,
                                'depot_to_center'=depot_to_center,
                                'threshold'=center_order_threshold,
                                'order_quantity'=center_order_quantity,
                                'arrivals'=total_arrivals_by_center)
  
  return(Updated_Center_States)
  
}



attend_waiting_patients <- function(Center_States){
  
  supply_by_center <- Center_States[['supply']]
  waiting_patients <- Center_States[['waiting']]
  recruitment_by_center <- Center_States[['recruitment']]
  rejection_by_center <- Center_States[['rejection']]
  depot_to_center <- Center_States[['depot_to_center']]
  supply_threshold <- Center_States[['threshold']]
  center_order_quantity <- Center_States[['order_quantity']]
  total_arrivals_by_center <- Center_States[['arrivals']]
  
  if(days_patients_wait > 0){
    
    for(center_drug in colnames(waiting_patients)){
      center <- strsplit(center_drug,"_")[[1]][1]
      
      while((supply_by_center[center_drug] > 0) & (sum(waiting_patients[center_drug]) >0)){
        
        patient_cohort <- max(seq(days_patients_wait)[waiting_patients[center_drug] > 0])
        
        patients_served <- min(c(waiting_patients[[center_drug]][[patient_cohort]],
                                 supply_by_center[[center_drug]]))
        
        if((patients_served > 0) & sim_verbose){
          drug <- drug_from_center_drug(center_drug)
          print(paste(patients_served, "waiting patients taking",
                      drug, " at ", center))
        }
        
        waiting_patients[[center_drug]][[patient_cohort]] <- waiting_patients[[center_drug]][[patient_cohort]] - patients_served
        
        supply_by_center[[center_drug]] <- supply_by_center[[center_drug]] - patients_served
        
        recruitment_by_center[center] <- recruitment_by_center[center] + patients_served
        
      }
      
    }
    
  }
  
  
  return(list('supply'=supply_by_center,
              'waiting'=waiting_patients,
              'recruitment'=recruitment_by_center,
              'rejection'=rejection_by_center,
              'depot_to_center'=depot_to_center,
              'threshold'=center_order_threshold,
              'order_quantity'=center_order_quantity,
              'arrivals'=total_arrivals_by_center))
  
}



update_waiting_patients <- function(Center_States){
  
  supply_by_center <- Center_States[['supply']]
  waiting_patients <- Center_States[['waiting']]
  recruitment_by_center <- Center_States[['recruitment']]
  rejection_by_center <- Center_States[['rejection']]
  depot_to_center <- Center_States[['depot_to_center']]
  supply_threshold <- Center_States[['threshold']]
  center_order_quantity <- Center_States[['order_quantity']]
  total_arrivals_by_center <- Center_States[['arrivals']]
  
  if(days_patients_wait > 0){
    
    rejected_today <- sapply(colnames(rejection_by_center),
                             function(center){
                               center_indices <- grepl(center,
                                                       colnames(waiting_patients))
                               return(sum(waiting_patients[days_patients_wait,
                                                           center_indices]))
                             })
    
    if(sim_verbose == TRUE){  
      for( i in seq(length(rejected_today))[rejected_today > 0]){
        print(paste("Rejecting",rejected_today[i],
                    "from",colnames(rejection_by_center)[i]))
      }
    }
    
    rejection_by_center <- rejection_by_center + rejected_today
    
    if(days_patients_wait > 1){
      waiting_patients[seq(2,days_patients_wait),] <- waiting_patients[seq(1,(days_patients_wait-1)),]
    }
    
    waiting_patients[1,] <- 0
    
  } 
  
  return(list('supply'=supply_by_center,
              'waiting'=waiting_patients,
              'recruitment'=recruitment_by_center,
              'rejection'=rejection_by_center,
              'depot_to_center'=depot_to_center,
              'threshold'=center_order_threshold,
              'order_quantity'=center_order_quantity,
              'arrivals'=total_arrivals_by_center))
  
}
