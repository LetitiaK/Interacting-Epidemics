##################################################################################
###############################        SETUP       ############################### 
################################################################################## 
library(igraph) 
library(animation)
library(xtable)

# Set the working directory
setwd("[Insert your working directory here]")

# Import the dataset (.csv)
dat=read.csv(file.choose(),header=TRUE)
el=as.matrix(dat)
el[,1]=as.character(el[,1])
el[,2]=as.character(el[,2])
n=graph.edgelist(el,directed=FALSE) 

# Create an edgelist and set the names to FALsE
edgelist = as_edgelist(n, names = FALSE)

# Set n as the number of vertices
N = vcount(n)

##################################################################################
############################### SET THE PARAMETERS ############################### 
################################################################################## 

# Set the number of initially infected individuals
init_inf_A = 1
init_inf_B = 1

# Set the number of initially recovered individuals
init_rec_A = 0
init_rec_B = 0

# Set the normal infection rates alpha
# In this case, the normal transmission rate is equal for both diseases
# alpha_A = Pony Stealer
alpha_A = 0.13 
# alpha_B = Angler
alpha_B = 0.13

# Set the increase of the infection rate for disease A and B
# Using the increase of the infection rate enables to choose whether the model is used for
# symetrically increasing transmission risks or for an unilateral increase
# In this case, the transmission of disease B is increased,
# if the other individual is currently infected with disease A
# The transmission of disease A, however, is not increased, 
# if the other individual is currently infected with disease B
beta_A = 0
beta_B = 0.49

# Set the recovery rates gamma
# In this case, the recovery rate is equal for both diseases
gamma_A = 0.14
gamma_B = 0.14

# Set the length of the simulation
simlength = 70
# Set how often the simulation is repeated
simnumber = 500

# Set whether the network is ment to be plotted
plot.spread = FALSE

# Set whether the infected individuals can recover from timestep 0 to timestep 1
recovery.wait_A = TRUE
recovery.wait_B = TRUE

# Create 6 lists in order to save the susceptible, infected and recovered data
# for both diseases A an B for each simulation round
infdata_sum_A = list()
infdata_sum_B = list()
susdata_sum_A = list()
susdata_sum_B = list()
recdata_sum_A = list()
recdata_sum_B = list()

# Create 2 lists in order to save the incidence of both diseases A and B
# for each simulation round
incidence_sum_A = list()
incidence_sum_B = list()

complete_infected_sum_A = list()
complete_infected_sum_B = list()

peaktime_A_sum = list()
peaktime_B_sum = list()

peaknumber_A_sum = list()
peaknumber_B_sum = list()

incidence_peaktime_A_sum = list()
incidence_peaktime_B_sum = list()

incidence_peaknumber_A_sum = list()
incidence_peaknumber_B_sum = list()

no_new_infection_A_sum = list()
no_new_infection_B_sum = list()

complete_infected_sum_A_100 = list()
complete_infected_sum_A_500 = list()
complete_infected_sum_A_1000 = list()

complete_infected_sum_B_100 = list()
complete_infected_sum_B_500 = list()
complete_infected_sum_B_1000 = list()


##################################################################################
###############################     SIMULATION     ############################### 
##################################################################################

###################    PARAMETERS FOR EACH SIMULATION ROUND     ##################

for (a in 1:simnumber){
  print(a)
  
  # Create a logical vector for infection data for both diseases A and B
  infected_A = logical(N)
  infected_B = logical(N)
  
  # Create a logical vecotr for recovered data for both diseases A and B
  recovered_A = logical(N)
  recovered_B = logical(N)
  
  # Set, which vertices are initially infected - in this case randomly
  patientzero_A = sample(N,init_inf_A)
  patientzero_B = sample(N,init_inf_B)
  
  # Set, which vertices are initally recovered - in this case randomly
  # In this specific case there are no initially infected individuals
  recoverzero_A = sample(N, init_rec_A)
  recoverzero_B = sample(N, init_rec_B)
  
  # Set the infected vertices to TRUE
  infected_A[patientzero_A] = TRUE
  infected_B[patientzero_B] = TRUE
  
  # Set the recovered vertices to NA
  infected_A[recoverzero_A] = NA
  infected_B[recoverzero_B] = NA
  
  # Create a logical vector in order to store those vertices, which are infected with both A and B
  bothinfected = logical(N)
  # Create a logical vector in order to store those vertices, which are recovered from both A and B
  bothrecovered = logical(N)
  # Create a logical vector in order to store those vertices, which are infected with A and recovered from B
  infA_recB = logical(N)
  # Create a logical vector in order to store those vertices, which are infected with B and recovered from A
  infB_recA = logical(N)
  
  # Since the recovered data is stored as NA, create a logical vector for recovered data for
  # both diseases A and B using the is.na function
  recovered_A = is.na(infected_A)
  recovered_B = is.na(infected_B)
  
  # Set the number of susceptibles as N minus the sum of infected individuals minus the sum of recovered vertices
  susceptible_A = N - sum(infected_A %in% TRUE, na.rm = TRUE) - sum(recovered_A == TRUE)
  susceptible_B = N - sum(infected_B %in% TRUE, na.rm = TRUE) - sum(recovered_B == TRUE)
  
  # For loop in order to find those vertices, which are infected with both A and B
  # Set those vertices which are dually infected to TRUE
  for (b in 1:length(infected_A)) {
    if(infected_A[[b]] %in% TRUE && infected_B[[b]] %in% TRUE){
      bothinfected[[b]] = TRUE
    }
  }
  
  # For loop in order to find those vertices, which are infected with A and recovered from B
  # Only used if there are initially recovered vertices
  # Set the respective vertices to TRUE
  if(length(recoverzero_B) > 0){
    for (f in 1:length(infected_A)){
      if(infected_A[[f]] %in% TRUE && recovered_B[[f]] == TRUE){
        infA_recB[[f]] = TRUE
      }
    }
  }
  
  # For loop in order to find those vertices, which are infected with B and recovered from A
  # Only used if there are initially recovered vertices
  # Set the respective vertices to TRUE
  if(length(recoverzero_A) > 0){
    for (h in 1:length(infected_B)){
      if(infected_B[[h]] %in% TRUE && recovered_A[[h]] == TRUE){
        infB_recA[[h]] = TRUE
      }
    }
  }
  
  # for loop in order to find those vertices, which are recovered from both A and B
  # Only used if there are initially recovered vertices
  # Set the respective vertices to TRUE
  # Important: If a vertex is recovered from both diseases set the respective vertex in infA_recB and infB_rec A
  # to FALSE!
  if(length(recoverzero_A) > 0){
    for (d in 1:length(recovered_A)) {
      if(recovered_A[[d]] == TRUE && recovered_B[[d]] == TRUE){
        bothrecovered[[d]] = TRUE
        infA_recB[[t]] = FALSE
        infB_recA[[t]] = FALSE
      }
    }
  }
  
  # Create 6 lists in order to save the susceptible, infected and recovered data
  # for both diseases A and B for each timestep of the simulation
  infdata_A = list(sum(infected_A %in% TRUE, na.rm = TRUE))
  infdata_B = list(sum(infected_B %in% TRUE, na.rm = TRUE))
  susdata_A = list(susceptible_A)
  susdata_B = list(susceptible_B)
  recdata_A = list(sum(recovered_A))
  recdata_B = list(sum(recovered_B))
  
  # Create 2 lists in order to save the incidence of both diseases A and B
  # for each timestep of the simulation
  incidence_A = list()
  incidence_B = list()
  
  complete_infected_A_100 = 0
  complete_infected_A_500 = 0
  complete_infected_A_1000 = 0
  
  complete_infected_B_100 = 0
  complete_infected_B_500 = 0
  complete_infected_B_1000 = 0
  
  no_new_infection_A = 0
  no_new_infection_B = 0
  
  # If the network is plotted
  # Set the layout of the graph - in this case kamada.kawai
  # Define the colors for the vertices corresponding to their state
  # Name the plot sim000 - important if a gif is created later on
  # Important: Set dev.off() after creating the plot
  if (plot.spread) {
    network.x = graph.edgelist(edgelist, directed=FALSE)
    fixlayout = layout.kamada.kawai(network.x)  
    node.colour = rep("SkyBlue2",N) 
    node.colour[patientzero_A] = "red"
    node.colour[patientzero_B] = "orange"
    node.colour[bothinfected] = "yellow"
    node.colour[recoverzero_A] = "forestgreen" 
    node.colour[recoverzero_B] = "grey"
    node.colour[bothrecovered] = "white"
    node.colour[infA_recB] = "hotpink"
    node.colour[infB_recA] = "chocolate4"
    png(paste("sim", 000, ".png", sep = "."))
    plot(network.x,layout=fixlayout, main="Time = 0", vertex.color=node.colour)
    #dev.off()
  }
  
  # Required padding for the name of the automatically saved screenshot of the network
  pad_int = function(t,scale){
    out_string = paste(10*scale + t,sep='')
    out_string = substr(out_string,2,nchar(out_string))
    return(out_string)
  }
  
  ###################    FIND THE RESPECTIVE EDGES     ###################  
  
  for (k in 1:simlength) {
    
    # Create 4 lists in order to sort the edges according to their transmission risk
    highrisk.edges_A = list()
    highrisk.edges_B = list()
    normal.edges_A = list()
    normal.edges_B = list()
    
    # Set counter variables in order to use the lists as defined above
    l = 1 # Normal risk A
    m = 1 # Normal risk B
    n = 1 # High risk A
    o = 1 # High risk B
    
    # Depending on recovery.wait the vertices recover from disease A
    # Therefore, first those vertices which are infected with disease A are extracted
    # Then, those vertices, which recover are selected with probability gamma
    # The recovered vertices are set to NA
    if (recovery.wait_A == FALSE || k > 1){
      infected.vertices_A = which(infected_A %in% TRUE)
      recover_A = rbinom(sum(infected_A %in% TRUE, na.rm = TRUE),1,gamma_A)
      recover.vertices_A = infected.vertices_A[recover_A == 1]
      infected_A[recover.vertices_A] = NA
    }
    
    # The same is repeated for disease B
    if (recovery.wait_B == FALSE || k > 1){  
      infected.vertices_B = which(infected_B %in% TRUE)
      recover_B = rbinom(sum(infected_B %in% TRUE, na.rm = TRUE),1,gamma_B)
      recover.vertices_B = infected.vertices_B[recover_B == 1]
      infected_B[recover.vertices_B] = NA
    }
    
    # After recovery, the logical vector for the recovered vertices is updated
    recovered_A = is.na(infected_A)
    recovered_B = is.na(infected_B)
    
    # Iterate through the edgelist in order to sort the vertices according to their state of disease
    # and to sort the edges respectively
    # Therefore one assumes two vertices (left, right) which are connected by an edge
    # Then, all possibilites of transmission are represented
    for(i in 1:nrow(edgelist)){
      
      # Find those "left" vertices, which are ONLY INFECTED WITH DISEASE A
      if(infected_A[edgelist[i,1]] %in% TRUE && infected_B[edgelist[i,1]] %in% FALSE){
        # Find those "right" vertices, which are ONLY INFECTED WITH DISEASE B
        if(infected_A[edgelist[i,2]] %in% FALSE && infected_B[edgelist[i,2]] %in% TRUE){
          # High risk A
          # High risk B
          highrisk.edges_A[[n]] = i
          highrisk.edges_B[[o]] = i
          n = n + 1
          o = o + 1
          next
          # Find those "right" vertices, which are INFECTED WITH BOTH DISEASE A AND B
        } else if(infected_A[edgelist[i,2]] %in% TRUE && infected_B[edgelist[i,2]] %in% TRUE) {
          # High risk B
          highrisk.edges_B[[o]] = i
          o = o + 1
          next
          # Find those "right" vertices, which are INFECTED WITH DISEASE B AND RECOVERED FROM DISEASE A
        } else if(infected_B[edgelist[i,2]] %in% TRUE && recovered_A[edgelist[i,2]] == TRUE) {
          # High risk B
          highrisk.edges_B[[o]] = i
          o = o + 1
          next
          # Find those "right" vertices, which are NOT INFECTED AT ALL OR
          # Find those "right" vertices, which are ONLY RECOVERED FROM DISEASE B
        } else if((infected_A[edgelist[i,2]] %in% FALSE && infected_B[edgelist[i,2]] %in% FALSE)||
                  (infected_A[edgelist[i,2]] %in% FALSE && recovered_B[edgelist[i,2]] == TRUE)) {
          # Normal risk A
          normal.edges_A[[l]] = i
          l = l + 1
          next
        }
        next
        
        # Find those "left" vertices, which are INFECTED WITH BOTH DISEASES A AND B
      }else if(infected_A[edgelist[i,1]] %in% TRUE && infected_B[edgelist[i,1]] %in% TRUE){
        # Find those "right" vertices, which are ONLY INFECTED WITH DISEASE B
        if(infected_A[edgelist[i,2]] %in% FALSE && infected_B[edgelist[i,2]] %in% TRUE){
          # High risk A
          highrisk.edges_A[[n]] = i
          n = n + 1
          next
          # Find those "right" edges, which are ONLY INFECTED WITH DISEASE A
        } else if(infected_A[edgelist[i,2]] %in% TRUE && infected_B[edgelist[i,2]] %in% FALSE){
          # High risk B  
          highrisk.edges_B[[o]] = i
          o = o + 1
          next
          # Find those "right" edges, which are NOT INFECTED AT ALL
        } else if(infected_A[edgelist[i,2]] %in% FALSE && infected_B[edgelist[i,2]] %in% FALSE){
          # Normal risk A
          # Normal risk B
          normal.edges_A[[l]] = i
          normal.edges_B[[m]] = i
          l = l + 1
          m = m + 1
          next
          # Find those "right" edges, which are ONLY RECOVERED FROM DISEASE B
        } else if(infected_A[edgelist[i,2]] %in% FALSE && recovered_B[edgelist[i,2]] == TRUE){
          # Normal risk A
          normal.edges_A[[l]] = i
          l = l + 1
          next
          # Find those "right" edges, which are ONLY RECOVERED FROM DISEASE A
        } else if(infected_B[edgelist[i,2]] %in% FALSE && recovered_A[edgelist[i,2]] == TRUE){
          # Normal risk B
          normal.edges_B[[m]] = i
          m = m + 1
          next
        }
        next
        # Find those "left" vertices, which are INFECTED WITH DISEASE A and RECOVERED FROM DISEASE B
      }else if(infected_A[edgelist[i,1]] %in% TRUE && recovered_B[edgelist[i,1]] == TRUE){
        
        # Find those "right" vertices, which are NOT INFECTED AT ALL OR
        # Find those "right" vertices, which are ONLY RECOVERED FROM DISEASE B
        if((infected_A[edgelist[i,2]] %in% FALSE && infected_B[edgelist[i,2]] %in% FALSE)||
           (infected_A[edgelist[i,2]] %in% FALSE && recovered_B[edgelist[i,2]] == TRUE)){
          # Normal risk A
          normal.edges_A[[l]] = i
          l = l + 1
          next
          # Find those "right" vertices, which are ONLY INFECTED WITH DISEASE B
        } else if(infected_A[edgelist[i,2]] %in% FALSE && infected_B[edgelist[i,2]] %in% TRUE){
          # High risk A
          highrisk.edges_A[[n]] = i
          n = n + 1
          next
        }
        next
        # Find those "left" vertices, which are ONLY INFECTED WITH DISEASE B
      }else if(infected_A[edgelist[i,1]] %in% FALSE && infected_B[edgelist[i,1]] %in% TRUE){
        
        # Find those "right" vertices, which are ONLY INFECTED WITH DISEASE A
        if(infected_A[edgelist[i,2]] %in% TRUE && infected_B[edgelist[i,2]] %in% FALSE){
          # High risk A
          # High risk B
          highrisk.edges_A[[n]] = i
          highrisk.edges_B[[o]] = i
          n = n + 1
          o = o + 1
          next
          # Find those "right" vertices, which are ONLY infected with disease A and RECOVERED from B
        } else if(infected_A[edgelist[i,2]] %in% TRUE && recovered_B[edgelist[i,2]] == TRUE){
          # High risk A
          highrisk.edges_A[[n]] = i
          n = n + 1
          next
          # Find those "right" vertices, which are INFECTED WITH BOTH DISEASE A AND B
        } else if(infected_A[edgelist[i,2]] %in% TRUE && infected_B[edgelist[i,2]] %in% TRUE){
          # High risk A
          highrisk.edges_A[[n]] = i
          n = n + 1
          next
          # Find those "right" vertices, which are NOT INFECTED AT ALL OR
          # Find those "right" vertices, which are ONLY RECOVERED FROM DISEASE A
        }else if((infected_A[edgelist[i,2]] %in% FALSE && infected_B[edgelist[i,2]] %in% FALSE)||
                 (infected_B[edgelist[i,2]] %in% FALSE && recovered_A[edgelist[i,2]] == TRUE)){
          # Normal risk B
          normal.edges_B[[m]] = i
          m = m + 1
          next
        }
        next
        # Find those "left" vertices, which are INFECTED WITH DISEASE B AND RECOVERED FROM DISEASE A
      }else if(infected_B[edgelist[i,1]] %in% TRUE && recovered_A[edgelist[i,1]] == TRUE){
        
        # Find those "right" vertices, which are NOT INFECTED AT ALL OR
        # Find those "right" vertices, which are RECOVERED FROM DISEASE A
        if((infected_A[edgelist[i,2]] %in% FALSE && infected_B[edgelist[i,2]] %in% FALSE)||
           (infected_B[edgelist[i,2]] %in% FALSE && recovered_A[edgelist[i,2]] == TRUE)){
          # Normal risk B
          normal.edges_B[[m]] = i
          m = m + 1
          next
          # Find those "right" vertices, which are ONLY INFECTED WITH DISEASE A
        }else if(infected_A[edgelist[i,2]] %in% TRUE && infected_B[edgelist[i,2]] %in% FALSE){
          # High risk B
          highrisk.edges_B[[o]] = i
          o = o + 1
          next
        }
        next
        # Find those "left" vertices, which are NOT INFECTED AT ALL
      }else if(infected_A[edgelist[i,1]] %in% FALSE && infected_B[edgelist[i,1]] %in% FALSE){
        
        # Find those "right" vertices, which are ONLY INFECTED WITH DISEASE A OR
        # Find those "right" vertices, which are INFECTED WITH DISEASE A AND RECOVERED FROM DISEASE B
        if((infected_A[edgelist[i,2]] %in% TRUE && infected_B[edgelist[i,2]] %in% FALSE)||
           (infected_A[edgelist[i,2]] %in% TRUE && recovered_B[edgelist[i,2]] == TRUE)){
          # Normal risk A
          normal.edges_A[[l]] = i
          l = l + 1
          next
          # Find those "right" vertices, which are ONLY INFECTED WITH DISEASE B OR
          # Find those "right" vertices, which are INFECTED WITH DISEASE B AND RECOVERED FROM DISEASE A
        }else if((infected_A[edgelist[i,2]] %in% FALSE && infected_B[edgelist[i,2]] %in% TRUE)||
                 (infected_B[edgelist[i,2]] %in% TRUE && recovered_A[edgelist[i,2]] == TRUE)){
          # Normal risk B
          normal.edges_B[[m]] = i
          m = m + 1
          next
          # Find those "right" vertices, which are infected with BOTH DISEASE A AND B
        }else if(infected_A[edgelist[i,2]] %in% TRUE && infected_B[edgelist[i,2]] %in% TRUE){
          # Normal risk A
          # Normal risk B
          normal.edges_A[[l]] = i
          normal.edges_B[[m]] = i
          l = l + 1
          m = m + 1
          next
        }
        next
      }
      
      # Find those "left" vertices, which are ONLY RECOVERED FROM DISEASE B
      if(recovered_B[edgelist[i,1]] == TRUE && infected_A[edgelist[i,1]] %in% FALSE){
        
        # Find those "right" vertices, which are ONLY INFECTED WITH DISEASE A OR
        # Find those "right" vertices, which are INFECTED WITH DISEASE A AND RECOVERED FROM DISEASE B OR
        # Find those "right" vertices, which are INFECTED WITH BOTH DISEASES A AND B
        if((infected_A[edgelist[i,2]] %in% TRUE && infected_B[edgelist[i,2]] %in% FALSE)||
           (infected_A[edgelist[i,2]] %in% TRUE && recovered_B[edgelist[i,2]] == TRUE)||
           (infected_A[edgelist[i,2]] %in% TRUE && infected_B[edgelist[i,2]] %in% TRUE)){
          # Normal risk A
          normal.edges_A[[l]] = i
          l = l + 1
          next
        }
        next
      }
      
      # Find those "left" vertices, which are ONLY RECOVERED FROM DISEASE A
      if(recovered_A[edgelist[i,1]] == TRUE && infected_B[edgelist[i,1]] %in% FALSE){
        
        # Find those "right" vertices, which are ONLY INFECTED WITH DISEASE B OR
        # Find those "right" vertices, which are INFECTED WITH DISEASE B RECOVERED FROM DISEASE A OR
        # Find those "right" vertices, which are INFECTED WITH BOTH DISEASES A AND B
        if((infected_A[edgelist[i,2]] %in% FALSE && infected_B[edgelist[i,2]] %in% TRUE)||
           (infected_B[edgelist[i,2]] %in% TRUE && recovered_A[edgelist[i,2]] == TRUE)||
           (infected_A[edgelist[i,2]] %in% TRUE && infected_B[edgelist[i,2]] %in% TRUE)){
          # Normal risk B
          normal.edges_B[[m]] = i
          m = m + 1
          next
        }
        next
      }
    }
    
    
    ###################    TRANSMISSION OF THE DISEASES     ###################  
    
    
    # After sorting the edges, unlist the respectivew lists
    highrisk.edges_A = unlist(highrisk.edges_A, recursive = TRUE, use.names = TRUE)
    highrisk.edges_B = unlist(highrisk.edges_B, recursive = TRUE, use.names = TRUE)
    normal.edges_A = unlist(normal.edges_A, recursive = TRUE, use.names = TRUE)
    normal.edges_B = unlist(normal.edges_B, recursive = TRUE, use.names = TRUE)
    
    # Use the rbinom function to determine the edges upon which the disease is transmitted
    # There one differentiates between those edges, which have normal probability and those edges, which 
    # have an increase probability
    transmit_normal_A = rbinom(length(normal.edges_A),1,alpha_A) 
    transmit_normal_B = rbinom(length(normal.edges_B),1,alpha_B)
    transmit_high_A = rbinom(length(highrisk.edges_A),1,(alpha_A+beta_A))
    transmit_high_B = rbinom(length(highrisk.edges_B),1,(alpha_B+beta_B))
    
    # Then those edges upon which the diseases are transmitted are selected
    transmitter.edges_normal_A = normal.edges_A[transmit_normal_A == 1]
    transmitter.edges_normal_B = normal.edges_B[transmit_normal_B == 1]
    transmitter.edges_high_A = highrisk.edges_A[transmit_high_A == 1]
    transmitter.edges_high_B = highrisk.edges_B[transmit_high_B == 1]
    
    # Based on the selected edges the corresponding vertices are selected
    vertices.transmitter.edges_normal_A = unique(as.vector(edgelist[transmitter.edges_normal_A,1:2]))
    vertices.transmitter.edges_normal_B = unique(as.vector(edgelist[transmitter.edges_normal_B,1:2]))
    vertices.transmitter.edges_high_A = unique(as.vector(edgelist[transmitter.edges_high_A,1:2]))
    vertices.transmitter.edges_high_B = unique(as.vector(edgelist[transmitter.edges_high_B,1:2]))
    
    
    ###################    UPDATE VERTEX STATUS     ###################  
    
    
    # All vertices, which are now newly infected are set to TRUE in the corresponding logical vector  
    infected_A[vertices.transmitter.edges_normal_A] = TRUE
    infected_A[vertices.transmitter.edges_high_A] = TRUE
    infected_B[vertices.transmitter.edges_normal_B] = TRUE
    infected_B[vertices.transmitter.edges_high_B] = TRUE
    
    # As in the beginning a for loop is used in order to find those vertices, which are infected with both A and B
    for (p in 1:length(infected_A)) {
      if(infected_A[[p]] %in% TRUE && infected_B[[p]] %in% TRUE){
        bothinfected[[p]] = TRUE
      }
    }
    
    # See above
    for (r in 1:length(infected_A)){
      if(infected_A[[r]] %in% TRUE && recovered_B[[r]] %in% TRUE){
        infA_recB[[r]] = TRUE
      }
    }
    
    # See above
    for (s in 1:length(infected_B)){
      if(infected_B[[s]] %in% TRUE && recovered_A[[s]] %in% TRUE){
        infB_recA[[s]] = TRUE
      }
    }
    
    #See above
    for (t in 1:length(recovered_A)) {
      if(recovered_A[[t]] == TRUE && recovered_B[[t]] == TRUE){
        bothrecovered[[t]] = TRUE
        infA_recB[[t]] = FALSE
        infB_recA[[t]] = FALSE
      }
    }
    
    
    ###################    SAVE THE DATA FOR EACH TIMESTEP     ###################  
    
    
    # After each timestep the susceptible, infected and recovered data are saved in the respective lists
    # For the calculation see above for more details
    infdata_A[[k+1]] = sum(infected_A %in% TRUE, na.rm = TRUE)
    infdata_B[[k+1]] = sum(infected_B %in% TRUE, na.rm = TRUE)
    susdata_A[[k+1]] = N - sum(infected_A %in% TRUE, na.rm = TRUE) - sum(is.na(infected_A))
    susdata_B[[k+1]] = N - sum(infected_B %in% TRUE, na.rm = TRUE) - sum(is.na(infected_B))
    recdata_A[[k+1]] = sum(is.na(infected_A))
    recdata_B[[k+1]] = sum(is.na(infected_B))
    
    # Then, the incidence is calculated as the number of newly infected vertices per timestep
    incidence_A[[k]] = susdata_A[[k]] - susdata_A[[k+1]]
    incidence_B[[k]] = susdata_B[[k]] - susdata_B[[k+1]]
    
    if(incidence_A[[k]] == 0 && no_new_infection_A == 0){
      no_new_infection_A = k
    }
    
    if(incidence_B[[k]] == 0 && no_new_infection_B == 0){
      no_new_infection_B = k
    }
    
    if (N - sum(infected_A %in% FALSE, na.rm = TRUE) >= 100 && complete_infected_A_100 == 0){
      complete_infected_A_100 = k
    }
    
    if (N - sum(infected_A %in% FALSE, na.rm = TRUE) >= 500 && complete_infected_A_500 == 0){
      complete_infected_A_500 = k
    }
    
    if (N - sum(infected_A %in% FALSE, na.rm = TRUE) >= 1000 && complete_infected_A_1000 == 0){
      complete_infected_A_1000 = k
    }
    
    ###############################
    
    if (N - sum(infected_B %in% FALSE, na.rm = TRUE) >= 100 && complete_infected_B_100 == 0){
      complete_infected_B_100 = k
    }
    
    if (N - sum(infected_B %in% FALSE, na.rm = TRUE) >= 500 && complete_infected_B_500 == 0){
      complete_infected_B_500 = k
    }
    
    if (N - sum(infected_B %in% FALSE, na.rm = TRUE) >= 1000 && complete_infected_B_1000 == 0){
      complete_infected_B_1000 = k
    }
    
    
    if (k == simlength){
      complete_infected_A = N - sum(infected_A %in% FALSE, na.rm = TRUE)
      complete_infected_B = N - sum(infected_B %in% FALSE, na.rm = TRUE)
      
      peaklist_A = unlist(infdata_A, recursive = TRUE, use.names = TRUE)
      peaktime_A = which.max(peaklist_A)
      peaknumber_A = peaklist_A[[peaktime_A]]
      
      incidence_peaklist_A = unlist(incidence_A, recursive = TRUE, use.names = TRUE)
      incidence_peaktime_A = which.max(incidence_peaklist_A)
      incidence_peaknumber_A = incidence_peaklist_A[[incidence_peaktime_A]]
      
      peaklist_B = unlist(infdata_B, recursive = TRUE, use.names = TRUE)
      peaktime_B = which.max(peaklist_B)
      peaknumber_B = peaklist_B[[peaktime_B]]
      
      incidence_peaklist_B = unlist(incidence_B, recursive = TRUE, use.names = TRUE)
      incidence_peaktime_B = which.max(incidence_peaklist_B)
      incidence_peaknumber_B = incidence_peaklist_B[[incidence_peaktime_B]]
      
    }
    
    
    ###################    UPDATE PLOTTING PARAMETERS     ###################  
    
    # See above for more details
    if (plot.spread) {
      node.colour[infected_A] = "red"
      node.colour[infected_B] = "orange"
      node.colour[bothinfected] = "yellow"
      node.colour[recovered_A] = "forestgreen"
      node.colour[recovered_B] = "grey"
      node.colour[bothrecovered] = "white"
      node.colour[infA_recB] = "hotpink"
      node.colour[infB_recA] = "chocolate4"
      padded<-pad_int(k,100)
      png(paste("sim",padded,".png", sep=""))
      plot(network.x,layout=fixlayout, main=paste("Time =", k), vertex.color=node.colour)
    }
    # Remeber to set dev.off() into comments when setting plot.spred to FALSE!
    #dev.off()
  }
  
  
  ###################    SAVE THE DATA FOR EACH SIMULATION ROUND     ###################  
  
  complete_infected_sum_A[[a]] = complete_infected_A 
  complete_infected_sum_B[[a]] = complete_infected_B
  
  peaktime_A_sum[[a]] = peaktime_A
  peaktime_B_sum[[a]] = peaktime_B
  
  peaknumber_A_sum[[a]] = peaknumber_A
  peaknumber_B_sum[[a]] = peaknumber_B
  
  incidence_peaktime_A_sum[[a]] = incidence_peaktime_A
  incidence_peaktime_B_sum[[a]] = incidence_peaktime_B
  
  incidence_peaknumber_A_sum[[a]] = incidence_peaknumber_A
  incidence_peaknumber_B_sum[[a]] = incidence_peaknumber_B
  
  no_new_infection_A_sum[[a]] = no_new_infection_A
  no_new_infection_B_sum[[a]] = no_new_infection_B
  
  complete_infected_sum_A_100[[a]] = complete_infected_A_100
  complete_infected_sum_A_500[[a]] = complete_infected_A_500
  complete_infected_sum_A_1000[[a]] = complete_infected_A_1000
  
  complete_infected_sum_B_100[[a]] = complete_infected_B_100
  complete_infected_sum_B_500[[a]] = complete_infected_B_500
  complete_infected_sum_B_1000[[a]] = complete_infected_B_1000
  
  # After each simulation round the lists for the susceptible, infected and recovered data are unlisted
  # This step is only required if the average is ment to be calculated
  infdata_A = unlist(infdata_A, recursive = TRUE, use.names = TRUE)
  infdata_B = unlist(infdata_B, recursive = TRUE, use.names = TRUE)
  susdata_A = unlist(susdata_A, recursive = TRUE, use.names = TRUE)
  susdata_B = unlist(susdata_B, recursive = TRUE, use.names = TRUE)
  recdata_A = unlist(recdata_A, recursive = TRUE, use.names = TRUE)
  recdata_B = unlist(recdata_B, recursive = TRUE, use.names = TRUE)
  incidence_A = unlist(incidence_A, recursive = TRUE, use.names = TRUE)
  incidence_B = unlist(incidence_B, recursive = TRUE, use.names = TRUE)
  
  # The complete data for this simulation round is denn saved in the respective lists
  infdata_sum_A[[a]] = infdata_A
  infdata_sum_B[[a]] = infdata_B
  susdata_sum_A[[a]] = susdata_A
  susdata_sum_B[[a]] = susdata_B
  recdata_sum_A[[a]] = recdata_A
  recdata_sum_B[[a]] = recdata_B
  incidence_sum_A[[a]] = incidence_A
  incidence_sum_B[[a]] = incidence_B
}


###################    CONVERT ALL DATA INTO USABLE FORMAT     ###################  


# After all simulations are done, create matrices for the data
# Infection A
infdata_A_table = as.data.frame(do.call(rbind,infdata_sum_A))
colnames(infdata_A_table) = paste("Time", 0:simlength)
rownames(infdata_A_table) = paste("Sim Inf A", 1:simnumber)
infdata_A_table = as.matrix(infdata_A_table)
write.csv(infdata_A_table, "PA_7_1_random_Infection_A.csv")

# Infection B
infdata_B_table = as.data.frame(do.call(rbind,infdata_sum_B))
colnames(infdata_B_table) = paste("Time", 0:simlength)
rownames(infdata_B_table) = paste("Sim Inf B", 1:simnumber)
infdata_B_table = as.matrix(infdata_B_table)
write.csv(infdata_B_table, "PA_7_1_random_Infection_B.csv")

# Susceptible A
susdata_A_table = as.data.frame(do.call(rbind,susdata_sum_A))
colnames(susdata_A_table) = paste("Time", 0:simlength)
rownames(susdata_A_table) = paste("Sim Sus A", 1:simnumber)
susdata_A_table = as.matrix(susdata_A_table)
write.csv(susdata_A_table, "PA_7_1_random_Susceptible_A.csv")

# Susceptible B
susdata_B_table = as.data.frame(do.call(rbind,susdata_sum_B))
colnames(susdata_B_table) = paste("Time", 0:simlength)
rownames(susdata_B_table) = paste("Sim Sus B", 1:simnumber)
susdata_B_table = as.matrix(susdata_B_table)
write.csv(susdata_B_table, "PA_7_1_random_Susceptible_B.csv")

# Recovered A
recdata_A_table = as.data.frame(do.call(rbind,recdata_sum_A))
colnames(recdata_A_table) = paste("Time", 0:simlength)
rownames(recdata_A_table) = paste("Sim Rec A", 1:simnumber)
recdata_A_table = as.matrix(recdata_A_table)
write.csv(recdata_A_table, "PA_7_1_random_Recovered_A.csv")

# Recovered B
recdata_B_table = as.data.frame(do.call(rbind,recdata_sum_B))
colnames(recdata_B_table) = paste("Time", 0:simlength)
rownames(recdata_B_table) = paste("Sim Rec B", 1:simnumber)
recdata_B_table = as.matrix(recdata_B_table)
write.csv(recdata_A_table, "PA_7_1_random_Recovered_B.csv")

# The same procedure must also be repeated for the incidence data
# Incidence A
incidence_A_table = as.data.frame(do.call(rbind,incidence_sum_A))
colnames(incidence_A_table) = paste("Time", 1:simlength)
rownames(incidence_A_table) = paste("Sim Inc A", 1:simnumber)
incidence_A_table = as.matrix(incidence_A_table)
write.csv(incidence_A_table, "PA_7_1_random_Incidence_A.csv")

# Incidence B
incidence_B_table = as.data.frame(do.call(rbind, incidence_sum_B))
colnames(incidence_B_table) = paste("Time", 1:simlength)
rownames(incidence_B_table) = paste("Sim Inc B", 1:simnumber)
incidence_B_table = as.matrix(incidence_B_table)
write.csv(incidence_B_table, "PA_7_1_random_Incidence_B.csv")


###################    CALCULATE AVERAGE OF DATA & FORMAT DATA     ###################  

# To caculate the averages, calculate the column sums of the respective matrices and divide them by the number
# of simulations
avg_infdata_sum_A = colSums(infdata_A_table, na.rm = FALSE, dims = 1L)/simnumber
avg_infdata_sum_B = colSums(infdata_B_table, na.rm = FALSE, dims = 1L)/simnumber
avg_susdata_sum_A = colSums(susdata_A_table, na.rm = FALSE, dims = 1L)/simnumber
avg_susdata_sum_B = colSums(susdata_B_table, na.rm = FALSE, dims = 1L)/simnumber
avg_recdata_sum_A = colSums(recdata_A_table, na.rm = FALSE, dims = 1L)/simnumber
avg_recdata_sum_B = colSums(recdata_B_table, na.rm = FALSE, dims = 1L)/simnumber

avg_incidence_sum_A = colSums(incidence_A_table, na.rm = FALSE, dims = 1L)/simnumber
avg_incidence_sum_B = colSums(incidence_B_table, na.rm = FALSE, dims = 1L)/simnumber

# Combine all average data together for plotting purposes
avg_combined = rbind(avg_infdata_sum_A, avg_infdata_sum_B, avg_susdata_sum_A, avg_susdata_sum_B, avg_recdata_sum_A, avg_recdata_sum_B)

# Combine all data together for plotting purposes
combined = rbind(infdata_A_table, infdata_B_table, susdata_A_table, susdata_B_table, recdata_A_table, recdata_B_table)

# Combine all average incidence data together for plotting purposes
avg_incidence_combined = rbind(avg_incidence_sum_A, avg_incidence_sum_B)

# Combine all incidence data together for plotting purposes
incidence_combined = rbind(incidence_A_table, incidence_B_table)

# For coloring purposes, save the row names of avg_combined
avg.plot.rownames = row.names(avg_combined)

# For coloring purposes, save the row names of combined
plot.rownames = row.names(combined)

# For coloring purposes, save the row names of the combined incidence
inc.plot.rownames = row.names(incidence_combined)

avg.inc.plot.rownames = row.names(avg_incidence_combined)

# Find all row names for the respective data
# This works best using the grep function and then using the wild card *
# Because we use ifelse, we do not need to define all, but only all minus 1
inf_A_row = plot.rownames[grep("Sim Inf A *", plot.rownames)]
inf_B_row = plot.rownames[grep("Sim Inf B *", plot.rownames)]
sus_A_row = plot.rownames[grep("Sim Sus A *", plot.rownames)]
sus_B_row = plot.rownames[grep("Sim Sus B *", plot.rownames)]
rec_A_row = plot.rownames[grep("Sim Rec A* ", plot.rownames)]

# Repeat this procedure also for the incidence
inc_A_row = inc.plot.rownames[grep("Sim Inc A *", inc.plot.rownames)]


###################    PREVALENCE - ALL    ###################  

# Plot all data - combined
setEPS()
postscript("PA_7_1_random_Prevalence.eps")
matplot(t(combined), type="l", lty = 1, col = ifelse(plot.rownames %in% inf_A_row, "red", 
                                                     ifelse(plot.rownames %in% inf_B_row, "orange", 
                                                            ifelse(plot.rownames %in% sus_A_row, "blue",
                                                                   ifelse(plot.rownames %in% sus_B_row, "deepskyblue",
                                                                          ifelse(plot.rownames %in% rec_A_row, "forestgreen", "green"))))), 
        xlab = "Time", ylab = "Number of Individuals")
title(main = "Prevalence")
legend("right", inset = 0.03, cex = 0.6, legend=c("Infected A", "Infected B", "Susceptible A", "Susceptible B", "Recovered A", "Recovered B"), 
       col=c("red","orange","blue","deepskyblue", "forestgreen", "green"), lty = 1:1)
dev.off()

###################    PREVALENCE - AVERAGE     ###################  

# Plot the average data - avg_combined
setEPS()
postscript("PA_7_1_random_Prevalence_Average.eps")
matplot(t(avg_combined), type="l", lty = 1, lwd = 3, col = ifelse(avg.plot.rownames == "avg_infdata_sum_A", "red", 
                                                                  ifelse(avg.plot.rownames == "avg_infdata_sum_B", "orange", 
                                                                         ifelse(avg.plot.rownames == "avg_susdata_sum_A", "blue", 
                                                                                ifelse(avg.plot.rownames == "avg_susdata_sum_B", "deepskyblue",
                                                                                       ifelse(avg.plot.rownames == "avg_recdata_sum_A", "forestgreen", "green"))))), 
        xlab = "Time", ylab = "Number of Individuals")
title(main = paste("Average Prevalence over", simnumber, "Simulations"))
legend("right", inset = 0.03, cex = 0.6, legend=c("Infected A", "Infected B", "Susceptible A", "Susceptible B", "Recovered A", "Recovered B"),
       col=c("red","orange","blue","deepskyblue", "forestgreen", "green"), lty = 1:1)
dev.off()

###################    INCIDENCE - ALL     ###################  

# Plot the incidence
setEPS()
postscript("PA_7_1_random_Incidence.eps")
matplot(t(incidence_combined), type="l", lty = 1, col = ifelse(inc.plot.rownames %in% inc_A_row, "darkorchid", "deeppink"), xlab = "Time", ylab = "Number of Individuals")
title(main = "Incidence: New Cases per Day")
legend("topright", inset = .10, legend=c("Disease A", "Disease B"), col=c("darkorchid", "deeppink"), lty = 1)
dev.off()

###################    INCIDENCE - AVERAGE     ###################  

# Plot the incidence
setEPS()
postscript("PA_7_1_random_Incidence_Average.eps")
matplot(t(avg_incidence_combined), type="l", lty = 1, lwd = 3, 
        col = ifelse(avg.inc.plot.rownames == "avg_incidence_sum_A", "darkorchid", "deeppink"), 
        xlab = "Time", ylab = "Number of newly infected Individuals")
title(main = paste("Average Incidence over", simnumber, "Simulations"))
legend("topright", inset = .10, legend=c("Disease A", "Disease B"), col=c("darkorchid", "deeppink"), lty = 1, lwd = 3)
dev.off()

###################    TOTAL NUMBER OF INFECTED INDIVIDUALs - AVERAGE   ###################  

# Disease A
complete_A_avg = Reduce("+", complete_infected_sum_A)/simnumber

# Disease B
complete_B_avg = Reduce("+", complete_infected_sum_B)/simnumber

# Combination of both
combined_complete_avg = c(complete_A_avg, complete_B_avg)

# Barplot
setEPS()
postscript("PA_7_1_random_Average_Number.eps")
bp = barplot(combined_complete_avg, xaxs = "r" , ylim = c(0,1300), space = c(1.1,1.1), width = 0.4, xlim = c(0,2), 
             names.arg = c("Malware A","Malware B"), horiz = FALSE, 
             main = paste("Total number of infected individuals \n during the Epidemic Outbreak \n (Average Number over", simnumber, "Simulations)"), 
             cex.main = 0.98, col=c("red","darkorange"))
text(bp, 1200, labels = round(combined_complete_avg, digits=0))
dev.off()

###################    TOTAL NUMBER OF INFECTED INDIVIDUALs - PERCENTAGE   ################### 

# Disease A
complete_A_per = complete_A_avg/N*100
complete_A_per = round(complete_A_per, digits = 2)

# Disease B
complete_B_per = complete_B_avg/N*100
complete_B_per = round(complete_B_per, digits = 2)

# Combination of both
combined_complete_per = c(complete_A_per, complete_B_per)

# Barplot
setEPS()
postscript("PA_7_1_random_Percentage.eps")
bp_p = barplot(combined_complete_per, xaxs = "r" , ylim = c(0,100), space = c(1.1,1.1), width = 0.4, xlim = c(0,2), 
               names.arg = c("Malware A","Malware B"), horiz = FALSE, 
               main = paste("Percentage of infected individuals \n during the Epidemic Outbreak"), 
               col=c("red","darkorange"))
text(bp_p, 90, labels = combined_complete_per)
dev.off()

###################    PEAK TIME  ################### 

# Disease A
complete_peaktime_A = Reduce("+", peaktime_A_sum)/simnumber

# Disease B
complete_peaktime_B = Reduce("+", peaktime_B_sum)/simnumber

# Combination of both
combined_peaktime = c(complete_peaktime_A, complete_peaktime_B)

# Barplot
setEPS()
postscript("PA_7_1_random_Peaktime.eps")
bp_p = barplot(combined_peaktime, xaxs = "r" , xlim = c(0,10), space = c(0.2,1.1), width = 0.2, 
               names.arg = c("Malware A","Malware B"), horiz = TRUE, 
               main = ("Peak"), 
               col=c("red","darkorange"))
text(9, bp_p, labels = round(combined_peaktime, digits=1))
dev.off()

###################    PEAK NUMBER  ################### 

# Disease A
complete_peaknumber_A = Reduce("+", peaknumber_A_sum)/simnumber

# Disease B
complete_peaknumber_B = Reduce("+", peaknumber_B_sum)/simnumber

# Combination of both
combined_peaknumber = c(complete_peaknumber_A, complete_peaknumber_B)

# Barplot
setEPS()
postscript("PA_7_1_random_Peaknumber.eps")
bp_p = barplot(combined_peaknumber, xaxs = "r" , xlim = c(0,1300), space = c(0.2,1.1), width = 0.2, 
               names.arg = c("Malware A","Malware B"), horiz = TRUE, 
               main = ("Peak Number of Infected Vertices"), 
               col=c("red","darkorange"))
text(1100, bp_p, labels = round(combined_peaknumber, digits=0))
dev.off()

###################   INCIDENCE PEAK TIME  ################### 

# Disease A
complete_incidence_peaktime_A = Reduce("+", incidence_peaktime_A_sum)/simnumber

# Disease B
complete_incidence_peaktime_B = Reduce("+", incidence_peaktime_B_sum)/simnumber

# Combination of both
combined_incidence_peaktime = c(complete_incidence_peaktime_A, complete_incidence_peaktime_B)

# Barplot
setEPS()
postscript("PA_7_1_random_Peaktime_Incidence.eps")
bp_p = barplot(combined_incidence_peaktime, xaxs = "r" , xlim = c(0,6.5), space = c(0.2,1.1), width = 0.2, 
               names.arg = c("Malware A","Malware B"), horiz = TRUE, 
               main = ("Peak Time Incidence"), 
               col=c("red","darkorange"))
text(5.8, bp_p, labels = round(combined_incidence_peaktime, digits=1))
dev.off()

###################    INCIDENCE PEAK NUMBER  ################### 

# Disease A
complete_incidence_peaknumber_A = Reduce("+", incidence_peaknumber_A_sum)/simnumber

# Disease B
complete_incidence_peaknumber_B = Reduce("+", incidence_peaknumber_B_sum)/simnumber

# combination of both
combined_incidence_peaknumber = c(complete_incidence_peaknumber_A, complete_incidence_peaknumber_B)

# Barplot
setEPS()
postscript("PA_7_1_random_Peaknumber_Incidence.eps")
bp_p = barplot(combined_incidence_peaknumber, xaxs = "r" , ylim = c(0,600), space = c(0.2,1.1), width = 0.2, 
               names.arg = c("Malware A","Malware B"), horiz = FALSE, 
               main = ("Peak Number Incidence"), 
               col=c("red","darkorange"))
text(bp_p, 530, labels = round(combined_incidence_peaknumber,digits=0))
dev.off()

###################    HOW LONG DOES IT TAKE UNTIL 100,500,1000 vertices are infected  ################### 

avg_complete_infected_sum_A_100 = Reduce("+", complete_infected_sum_A_100)/simnumber
avg_complete_infected_sum_A_500 = Reduce("+", complete_infected_sum_A_500)/simnumber
avg_complete_infected_sum_A_1000 = Reduce("+", complete_infected_sum_A_1000)/simnumber

avg_complete_infected_sum_B_100 = Reduce("+", complete_infected_sum_B_100)/simnumber
avg_complete_infected_sum_B_500 = Reduce("+", complete_infected_sum_B_500)/simnumber
avg_complete_infected_sum_B_1000 = Reduce("+", complete_infected_sum_B_1000)/simnumber

combined_100 = c(avg_complete_infected_sum_A_100, avg_complete_infected_sum_B_100)
combined_500 = c(avg_complete_infected_sum_A_500, avg_complete_infected_sum_B_500)
combined_1000 = c(avg_complete_infected_sum_A_1000, avg_complete_infected_sum_B_1000)

all_combined = rbind(combined_100, combined_500, combined_1000)
all_combined = as.data.frame(t(all_combined))
all_combined = as.matrix(all_combined)
colnames(all_combined) = c("100 Vertices", "500 Vertices", "1000 Vertices")

setEPS()
postscript("PA_7_1_random_Time_till_Infection.eps")
bp_p = barplot(all_combined, xaxs = "r", ylim = c(0,8.5), horiz = FALSE, 
               main = ("Time Steps until Vertices are Infected"), 
               col=c("red", "darkorange"), beside = TRUE)
text(bp_p, c(4.5,4.5,6,6,7.5,7.5), labels = round(all_combined, digits = 1))
legend("topleft", inset = 0.09, legend=c("Malware A", "Malware B"),
       col=c("red","orange"), fill = c("red", "darkorange"))
dev.off()

###################    HOW LONG DOES IT TAKE UNTIL THERE ARE NO NEW INFECTIONS  ###################

avg_no_new_infections_A = Reduce("+", no_new_infection_A_sum)/simnumber
avg_no_new_infections_B = Reduce("+", no_new_infection_B_sum)/simnumber

combined_no_new_infections = c(avg_no_new_infections_A, avg_no_new_infections_B)

setEPS()
postscript("PA_7_1_random_No_New_Infection.eps")
bp_p = barplot(combined_no_new_infections, xaxs = "r" , ylim = c(0,12), space = c(0.2,1.1), width = 0.2, 
               names.arg = c("Malware A","Malware B"), horiz = FALSE, 
               main = ("Time Step: No new infections"), 
               col=c("red","darkorange"))
text(bp_p, 11, labels = round(combined_no_new_infections,digits=0))
dev.off()
