#
# Reduced version of "hitrate.function.r
# 
# Calculates only "Alternative 2" of the original 6 different alternative
# hitrates (HR), false alarm rates (FAR) and success indices (SI)
# for two input series (with equal lengths)
# according to NVE rapport 84-2014
#
# The result is a 1x3 matrix where
# the columns are HR, FAR and SI
#

# obs=all.ts.2.11_Narsjo$Observed
# sim=all.ts.2.11_Narsjo$HBV


hrs.f <- function(obs, sim){ 
  
  # Making 0 matrix with row & column names to store actual hits, misses and false alarms
 
  # calculate Qm for the complete series, use only that value, not changing at each step
  
  library(hydroTSM)
  
  mean.obs.i=mean(daily2annual.zoo(obs, FUN=max, na.rm=TRUE))
  mean.sim.i=mean(daily2annual.zoo(sim, FUN=max, na.rm=TRUE))

  # convert "obs" and "sim" to numeric values, loose zoo feature
  
  obs=coredata(obs)
  sim=coredata(sim)
  
  HMF = matrix(0,1,3, dimnames = list(c("Alt2"), c("Hit","Miss","False")))
  
  for (i in 2:(length(obs)-1)){
   
    if (!is.na(obs[i]) & !is.na(sim[i]) & !is.na(sim[i+1]) & !is.na(sim[i-1]) & !is.na(mean.obs.i) & !is.na(mean.sim.i)){
    
    # print(paste("i=",i))
    # print(paste("obs[i]=",obs[i]))
    # print(paste("sim[i]=",sim[i]))
    # print(paste("sim[i+1]=",sim[i+1]))
    # print(paste("sim[i-1]=",sim[i-1]))
    # print(paste("mean.obs.i=",mean.obs.i))
    # print(paste("mean.sim.i=",mean.sim.i))
 
    
# alt 1 in Table 1 in NVE report 84-2014
# Q > Qm; T=0; Int=0
    
      # if ((obs[i] > mean.obs.i) & (sim[i] > mean.sim.i)){
      #   HMF[1,1] = HMF[1,1] + 1
      # }
      # 
      # if ((obs[i] > mean.obs.i) & (sim[i] <= mean.sim.i)){
      #   HMF[1, 2] = HMF[1, 2] + 1
      # }
      #   
      # if ((obs[i] <= mean.obs.i) & (sim[i] > mean.sim.i)){
      #   HMF[1, 3] = HMF[1, 3] + 1
      # }

# alt 2 in Table 1 in NVE report 84-2014
# Q > Qm; T=1; Int=0
    
    if ((obs[i] > mean.obs.i) & (sim[i] > mean.sim.i) & (sim[i - 1] <= mean.sim.i)){
      HMF[1, 1] = HMF[1, 1] + 1
    }
    
    if ((obs[i] > mean.obs.i) & (sim[i + 1] > mean.sim.i) & (sim[i] <= mean.sim.i)){
      HMF[1, 1] = HMF[1, 1] + 1
    }
    
    if ((obs[i] > mean.obs.i) & (sim[i] <= mean.sim.i) & (sim[i + 1] <= mean.sim.i)){ 
      HMF[1,2] = HMF[1,2] + 1
    }
    
    if ((obs[i] <= mean.obs.i) & (sim[i] > mean.sim.i)){
      HMF[1,3] = HMF[1,3] + 1
    }
    
    if ((obs[i] <= mean.obs.i) & (sim[i + 1] > mean.sim.i)){
      HMF[1,3] = HMF[1,3] + 1
    }
    
# alt 3 in Table 1 in NVE report 84-2014
# Q > 1.2*Qm; T=0; Int=0
    
    # if ((obs[i] > (1.2 * mean.obs.i)) & (sim[i] > (1.2 * mean.sim.i))){
    #   HMF[3,1] = HMF[3,1] + 1
    # }
    # 
    # if ((obs[i] > (1.2 * mean.obs.i)) & (sim[i] <= (1.2 * mean.sim.i))){ 
    #   HMF[3,2] = HMF[3,2] + 1
    # }
    # 
    # if ((obs[i] <= (1.2 * mean.obs.i)) & (sim[i] > (1.2 * mean.sim.i))){
    #   HMF[3,3] = HMF[3,3] + 1
    #     }

# alt 4 in Table 1 in NVE report 84-2014
# Q > 1.2*Qm; T=1; Int=0
    
    # if ((obs[i] > (1.2 * mean.obs.i)) & (sim[i] > (1.2 * mean.sim.i))){
    #   HMF[4, 1] = HMF[4, 1] + 1
    # }
    # 
    # if ((obs[i] > (1.2 * mean.obs.i)) & (sim[i + 1] > (1.2 * mean.sim.i)) & (sim[i] <= (1.2 * mean.sim.i)) & (sim[i - 1] <= (1.2 * mean.sim.i))){
    #   HMF[4, 1] = HMF[4, 1] + 1
    # }
    # 
    # if ((obs[i] > (1.2 * mean.obs.i)) & (sim[i] <= (1.2 * mean.sim.i)) & (sim[i + 1] <= (1.2 * mean.sim.i))){ 
    #   HMF[4,2] = HMF[4,2] + 1
    # }
    # 
    # if ((obs[i] <= (1.2 * mean.obs.i)) & (sim[i] > (1.2 * mean.sim.i))){
    #   HMF[4,3] = HMF[4,3] + 1
    # }
    # 
    # if ((obs[i] <= (1.2 * mean.obs.i)) & (sim[i + 1] > (1.2 * mean.sim.i))){
    #   HMF[4,3] = HMF[4,3] + 1
    # }
    
# alt 5 in Table 1 in NVE report 84-2014
# Q > Qm; T=0; Int= +/- 20%
    
    # if ((obs[i] > mean.obs.i) & (sim[i] > mean.sim.i) & (sim[i] < (1.2 * obs[i])) & (sim[i] > (0.8 * obs[i]))){
    #   HMF[5, 1] = HMF[5, 1] + 1
    # }
    # 
    # if ((obs[i] > mean.obs.i) & (sim[i] <= mean.sim.i)){
    #   HMF[5,2] = HMF[5,2] + 1
    # }
    # 
    # if ((obs[i] > mean.obs.i) & (sim[i] > mean.sim.i) & (sim[i] >= (1.2 * obs[i]))){
    #   HMF[5,2] = HMF[5,2] + 1
    # }
    # 
    # if ((obs[i] > mean.obs.i) & (sim[i] > mean.sim.i) & (sim[i] <= (0.8 * obs[i]))){
    #   HMF[5,2] = HMF[5,2] + 1
    # }
    #  
    # if ((obs[i] <= mean.obs.i) & (sim[i] > mean.sim.i)){
    #   HMF[5,3] = HMF[5,3] + 1
    # }
    # 
# alt 6 in Table 1 in NVE report 84-2014
# Q > Qm; T=1; Int= +/- 20%
    
#     if ((obs[i] > mean.obs.i) & (sim[i] > mean.sim.i) & (sim[i] < (1.2 * obs[i])) & (sim[i] > (0.8 * obs[i]))){
#       HMF[6, 1] = HMF[6, 1] + 1
#     }
#     
#     if ((obs[i] > mean.obs.i) & (sim[i + 1] > mean.sim.i) & (sim[i + 1] < (1.2 * obs[i])) & (sim[i + 1] > (0.8 * obs[i])) & (sim[i - 1] <= mean.sim.i)){
#       HMF[6, 1] = HMF[6, 1] + 1
#     }
#     
#     if ((obs[i] > mean.obs.i) & (sim[i] <= mean.sim.i) & sim[i+1] <= mean.sim.i){
#       HMF[6,2] = HMF[6,2] + 1
#     }
#     
#     if ((obs[i] > mean.obs.i) & (sim[i] > mean.sim.i) & (sim[i] >= (1.2 * obs[i]))){
#       HMF[6,2] = HMF[6,2] + 1
#     }
#     
#     if ((obs[i] > mean.obs.i) & (sim[i + 1] > mean.sim.i) & (sim[i + 1] >= (1.2 * obs[i]))){
#       HMF[6,2] = HMF[6,2] + 1
#     }
#     
#     if ((obs[i] > mean.obs.i) & (sim[i] > mean.sim.i) & (sim[i] <= (0.8 * obs[i]))){
#       HMF[6,2] = HMF[6,2] + 1
#     }
#     
#     if ((obs[i] > mean.obs.i) & (sim[i + 1] > mean.sim.i) & (sim[i + 1] <= (0.8 * obs[i]))){
#       HMF[6,2] = HMF[6,2] + 1
#     }
#     
#     if ((obs[i] <= mean.obs.i) & (sim[i] > mean.sim.i)){
#       HMF[6,3] = HMF[6,3] + 1
#     }
   }    
 }
# merge final results together in matrix "b"
# by calculating hitrate (HR), false alarm rate (FAR) and success index (SI)
# from single components in HMF
    
  b = matrix(0, nrow = 1, ncol = 3, dimnames = list(c("Alt2"), c("HR", "FAR", "SI")))
  
  # for (i in 1:6){
    b[1,]= c((HMF[1,1]/(HMF[1,1]+HMF[1,2])), (HMF[1,3]/(HMF[1,1]+HMF[1,3])), (HMF[1,1]/(HMF[1,1]+HMF[1,2]+HMF[1,3])))
  # }

return(b)

}