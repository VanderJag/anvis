################################################################################

# Load all packages
library(RcppCNPy)           # Used to save and load numpy files
library(qgraph)             # Used to create the network
library(igraph)             # Used to create the network
library(RCy3)               # Used to open the network in cytoscape
library(missForest)         # Used to impute missing data
library(stringr)            # Used to edit the names

set.seed(1)

################################################################################

# Load the data
data <- read.csv("/media/sanjee/R_Working_Directory/MGM_Paper/MGM_paper/rscripts/Provided Data/Merged/Bacterial_genes+Clinical.csv",
                 check.names = FALSE)
rownames(data) <- data[,1]
data <- data[,-1]

################################################################################

# Select samples that have gene measurements
to_remove <- NULL
for (sample in 1:nrow(data)) {
  if (is.na(data$`A0A023KWQ1_ECOLX; Escherichia coli`[sample])==TRUE) {
    to_remove <- c(to_remove, sample)
  }
}
data <- data[-unique(to_remove),]

# Keep only samples with NSTI I or II
data <- data[-which(is.na(data$Case_type)),]
data <- data[,-which(colnames(data)=='Microbiological agens - conclusion')]
data <- data[,-which(colnames(data)=='Conclusion_micro')]

data <- data[,-which(colnames(data)=="Patient ID")]
data <- data[,-which(colnames(data)=="PatientId")]

################################################################################

# Keep only columns which have less than a limit missing data
to_keep <- NULL
limit <- 10 # percentage of missing data that is allowed per variable
for (i in 1:ncol(data)){
  percentage <- 100*sum(is.na(data[,i]))/nrow(data)
  if (percentage<=limit) {
    # print(percentage)
    to_keep <- c(to_keep, i)
  }
}
if (limit < 100) {
  data <- data[,to_keep]
}

################################################################################

# Assign a column to annotation based on the name
discrete_columns <- grepl('=', colnames(data))
# Add these columns to the Annotation matrix
Annotation <- data[,which(discrete_columns==TRUE)]
# Remove these columns from the data matrix
data <- data[,-which(discrete_columns==TRUE)] 
# Select factors to the Annotation 
for (column in colnames(data)) {
  if (class(data[,column])=="character") {
    if (is.na(match(column, colnames(data)))==FALSE) {
      # Add this column to the Annotation matrix
      Annotation <- cbind(Annotation, data[,column])
      colnames(Annotation)[ncol(Annotation)] <- column
      # Remove this column from the data matrix
      data <- data[,-which(colnames(data)==column)]
    }
  }
}
# Select left over columns
for (column in c('Alcohol Consumption','Mechanical ventilation Baseline',
                 "Need For reconstructive Surgery ((if not in hospital day 90)",
                 'NSTI_type', 'Sex', 'Comorbidity', "amputation", "Septicshock", 
                 "IVIG", "Clindamycin", 'Death',"Other Antibiotics Day 1",
                 "Other Antibiotics Given Before ICU Admission", "sample_1",
                 "Other Antibiotics Day 2","Other Antibiotics Day 3","surgery_3", 
                 "Other Antibiotics Day 4", "Other Antibiotics Day 5", 
                 "Other Antibiotics Day 6", "Other Antibiotics Day 7", 
                 "Bodypart affected at arrival - conclusion","HIV","surgery_2",
                 "Microbiological species found in tissue sample","collection_mode_1",
                 "Microbiological agens - conclusion","LRINEC Risk of NSTI",
                 "LRINEC risk of NSTI dichotomous","RRT at baseline",
                 "Amputation of Limb (Time: from diagnosis to ICU day 7)",
                 "Amputation (0=no, 1=yes)")) {
  if (is.na(match(column, colnames(data)))==FALSE) {
    # Add this column to the Annotation matrix
    Annotation <- cbind(Annotation, data[,column])
    colnames(Annotation)[ncol(Annotation)] <- column
    # Remove this column from the data matrix
    data <- data[,-which(colnames(data)==column)]
  }
}

# Remove columns that have missing data
NA_columns <- NULL
for (column in 1:ncol(Annotation)) {
  if (any(is.na(Annotation[,column]))==TRUE) {
    NA_columns <- c(NA_columns, column)
  }
}
Annotation <- Annotation[,-NA_columns]

# # Remove samples that have missing data points in the annotation columns
# NA_samples <- NULL
# for (sample in 1:nrow(Annotation)) {
#   if (any(is.na(Annotation[sample,]))==TRUE) {
#     # print(rownames(Annotation)[sample])
#     NA_samples <- c(NA_samples, sample)
#   }
# } # When no limit is set, this step removes all samples
# if (is.null(NA_samples)==FALSE) {
#   data <- data[-NA_samples,]
#   Annotation <- Annotation[-NA_samples,]
# }

# See how many level each discrete variable has
levels <- NULL
remove_level <- NULL
for (column in 1:ncol(Annotation)) {
  level <- length(unique(Annotation[,column]))
  levels <- c(levels, level)
  if (level==1) {
    remove_level <- c(remove_level, column)
  }
}
if (is.null(remove_level)==FALSE) {
  Annotation <- Annotation[,-remove_level]
  levels <- levels[-remove_level]
}

# Remove continuous variables with only one level
remove_level <- NULL
for (column in 1:ncol(data)) {
  level <- length(unique(data[,column]))
  if (level==1) {
    remove_level <- c(remove_level, column)
  }
}
if (is.null(remove_level)==FALSE) {
  data <- data[,-remove_level]
}

################################################################################

# Print some patient characteristics
n.male <- length(which(Annotation$`Sex (0=female, 1=male)` == 1))
n.amp <- length(which(Annotation$`Amputation (0=no, 1=yes)` == 1))
n.death <- length(which(Annotation$`Patient Dead Day 90 (0=no, 1=yes)` == 1))
n.ii <- length(which(Annotation$Case_type=='NSTI II'))
n.i <- length(which(Annotation$Case_type=='NSTI I'))
pat.char <- rbind(nrow(data)-n.male,
                  n.male,
                  n.i,
                  n.ii,
                  n.amp,
                  n.death)
rownames(pat.char) <- c('Female', 'Male', 'Type II', 'Type I', 'Amputation', 'Death')
print(nrow(data))
print(paste(round(mean(data$`Age (years)`),2), '±', round(sd(data$`Age (years)`),2)))
pat.char

################################################################################

# # Remove clinical variables with missing data
# to_remove <- NULL
# limit <- 0 # percentage of missing data that is allowed per variable
# for (i in c(1,2,1621:1968)){ # THIS CAN CHANGE!!!!
#   percentage <- 100*sum(is.na(data[,i]))/nrow(data)
#   # print(paste(i,'=',percentage))
#   if (percentage>limit) {
#     # print(percentage)
#     to_remove <- c(to_remove, i)
#   }
# }
# if (limit < 100) {
#   data <- data[,-to_remove]
# }

# Impute missing data in the remaining columns
if (any(is.na(data))==TRUE) {
  data_imp <- missForest(xmis = as.matrix(data), verbose = TRUE, ntree = 100)
  data_imp$OOBerror
  data <- as.data.frame(data_imp$ximp)
}

# Remove continuous variables with only one level
remove_level <- NULL
for (column in 1:ncol(data)) {
  level <- length(unique(data[,column]))
  if (level==1) {
    remove_level <- c(remove_level, column)
  }
}
# If the variable has only 1 level, remove it from the data
if (is.null(remove_level)==FALSE) {
  data <- data[,-remove_level]
}

# Scale the continuous variables
data <- lapply(data, as.numeric)
data <- as.data.frame(data)

################################################################################
## MAKE THE D MATRIX FROM THE ANNOTATION MATRIX

# Make a discrete variable matrix
D <- NULL
colnames_D <- NULL
# For each discrete entry in the Annotation data frame
for (colnum in 1:ncol(Annotation)) {
  name <- str_remove_all(string = colnames(Annotation)[colnum], pattern = '\\((\\w+\\=[\\w\\s\\-\\.]+[\\,\\s\\)]+)+')
  name <- trimws(name, which = 'both')
  # If the variable has names as levels
  # if (is.null(levels(Annotation[,colnum]))==FALSE) {
  if (class(Annotation[,colnum])=='character') {
    # print(paste(colnum, 'character', levels[colnum]))
    for (i in sort(unique(Annotation[,colnum]))) {
      # Make all samples 0
      Dtemp <- rep(0,nrow(Annotation))
      # If the sample is this level, change it to 1
      Dtemp[Annotation[,colnum]==i] <- 1
      D <- rbind(D,Dtemp)
      # Add the colname
      # if (levels[colnum]==2) {
      #   colnames_D <- c(colnames_D,name)
      # } else 
      if (grepl('=',colnames(Annotation)[colnum])==TRUE) {
        splitted<-strsplit(colnames(Annotation)[colnum], split = '[=]|[(]|[)]|[,]')[[1]]
        for (split in 1:length(splitted)) {
          splitted[split] <- trimws(splitted[split])
        }
        level<-splitted[which(splitted==i)+1]
        colnames_D <- c(colnames_D,paste(name,'=',level)) 
      } else { # if (levels[colnum]!=2) {
        colnames_D <- c(colnames_D,paste(colnames(Annotation)[colnum],'=',i))}
      # print(colnames_D[length(colnames_D)])
    }
    
  } else {
    # print(paste(colnum,'integer', levels[colnum]))
    # If the variable has numbers as levels
    # for(i in 0:(levels[colnum]-1)){
    for (i in sort(unique(Annotation[,colnum]))) {
      # Start at the minimum level
      # mini <- min(Annotation[,colnum])
      # i <- i+mini
      # Make all samples 0
      Dtemp <- rep(0,nrow(Annotation))
      # If the sample is this level, change it to 1
      Dtemp[as.integer(Annotation[,colnum])==i] <- 1
      D <- rbind(D,Dtemp)
      # Add the colname
      # if (levels[colnum]==2) {
      #   colnames_D <- c(colnames_D,name)
      # } else 
      if (grepl('=',colnames(Annotation)[colnum])==TRUE) {
        splitted<-strsplit(colnames(Annotation)[colnum], split = '[=]|[(]|[)]|[,]')[[1]]
        for (split in 1:length(splitted)) {
          splitted[split] <- trimws(splitted[split])
        }
        # if ('no' %in% splitted) {
        #   splitted <- c(splitted, 2, 'unknown')
        # }
        level<-splitted[which(splitted==i)+1]
        if (level == 'yes'){#} || level == 'yes') {
          colnames_D <- c(colnames_D,name)
        }
        if (level != 'yes'){#&& level != 'yes') {
          colnames_D <- c(colnames_D,paste(name,'=',level))
        }
      } else { # if (levels[colnum]!=2)
        if (i == 'yes') {
          colnames_D <- c(colnames_D,name)
        }
        else {
          colnames_D <- c(colnames_D,paste(colnames(Annotation)[colnum],'=',i))}
      }
      # print(colnames_D[length(colnames_D)])
    }
  }
}
# Transpose D so it looks the same as the original dataset 
# (samples as rows, variables as columns)
D <- as.data.frame(t(D))
# Add names to the columns
colnames(D) <- colnames_D

data <- as.data.frame(data)
################################################################################

# Save matrices as npy-files in Python-script folder (here: "MGM_algorithm")
myPath <- "/media/sanjee/R_Working_Directory/MGM_Paper/MGM_paper/rscripts/Try_Sanjee/MGM_algorithm"
setwd(myPath)
# Continuous variable matrix with rows = samples, columns = genes:
npySave("Xsc.npy", as.matrix(data))
# Discrete variable matrix with rows = samples, columns = discrete levels:
npySave("Dsc.npy", as.matrix(D))
# Save the levels vector
npySave("levelssc.npy", as.integer(levels))
# number of samples
n <- nrow(data)
# number of continuous variables
p <- ncol(data)
# number of discrete variables
q <- ncol(Annotation)
# Define lambda sequence for penalization according to (Lee and Hastie, 2015):
delta <- 1
la_seq <- delta*sqrt(log(p+q)/n)
# Save lambda sequence
npySave("lam_seqsc.npy", la_seq)
# Carry out MGM
system(paste0("python3 apply_functions_command_line.py",
              " --iterations=1000",           # set number of iterations to 1000
              " --oTol=1e-4",                 # set precision to 1e-4
              " --X_file=Xsc.npy",            # continuous variable matrix
              " --D_file=Dsc.npy",            # discrete variable matrix
              " --lamseq_file=lam_seqsc.npy", # lambda sequence
              " --levels_file=levelssc.npy",  # levels of discrete variables
              " --results_folder=temp"),      # output folder
       wait=TRUE)

# Move to the output folder
setwd(paste0(myPath,"/temp"))
# Matrix of edges between continuous - continuous variables:
B0 = npyLoad("B_0.npy")
# Matrix of edges between continuous - discrete variables:
Rho0 = npyLoad("Rho_0.npy")
# Matrix of edges between discrete - discrete variables:
Phi0 = npyLoad("Phi_0.npy")

################################################################################

# Combine single result matrices into one adjacency matrix:
adj_matrix <- rbind(cbind(B0,t(Rho0)), cbind(Rho0,Phi0))
colnames(adj_matrix) <- rownames(adj_matrix) <- c(colnames(data),colnames(D))

# Make different groups
groups <- as.vector(c(rep("continuous",ncol(data)), rep("discrete",ncol(D))))
# Shape discrete variables nodes differently
shapes <- as.vector(c(rep("circle",ncol(data)), rep("sqaure",ncol(D))))
# Colour discrete variables nodes differently
colours <- as.vector(c(rep("#56B4E9",ncol(data)), rep("#56B4E9",ncol(D))))

# Get the data sets
bact_gene_data <- read.csv("~/School/WUR/SSB-80336 - Thesis/Provided Data/Merged/Bacterial_gene_data.csv", 
                           check.names = FALSE)

# Colour the genes
for (bact in colnames(bact_gene_data)) {
  colours[which(colnames(data)==bact)] <- '#0072B2'
}
# Change the shape and colour of continuous clinical parameters
for (clinical in c("Severity", "SAPSII", 'Age', 'SOFA1')) {
  colours[which(colnames(data)==clinical)] <- "#56B4E9"
}

# Remove disconnected nodes
disconnected.nodes <- which(apply(adj_matrix, 1, function(x){all(x==0)}))
if (length(disconnected.nodes)!=0) {
  adj_matrix <- adj_matrix[-disconnected.nodes,-disconnected.nodes]
  groups <- groups[-disconnected.nodes]
  shapes <- shapes[-disconnected.nodes]
  colours <- colours[-disconnected.nodes]
}

# Save the adjacency matrix
write.csv(adj_matrix, "/media/sanjee/R_Working_Directory/MGM_Paper/MGM_paper/rscripts/Try_Sanjee/Adj/Adjecency_matrix_Bacterial_genes+Clinical.csv")

# Create a qgraph with layout options
qgraph_adj_mat <- qgraph(input=adj_matrix,
                         labels=colnames(adj_matrix),
                         groups=groups,
                         DoNotPlot=TRUE,
                         borders=FALSE,
                         palette="colorblind",
                         label.font='sans',
                         posCol="#009E73",  # colour of positive edges
                         negCol="#D55E00",  # colour of negative edges
                         color=colours,     # colour of groups
                         shape=shapes,      # shapes of groups
                         fade=FALSE,        # edge transparency based on weight
                         esize=2)

# Convert qgraph to igraph object
igraph_adj <- as.igraph(qgraph_adj_mat, attributes = TRUE)
V(igraph_adj)$name <- colnames(adj_matrix)

# Connect to cytoscape (Make sure cytoscape is opened)
cytoscapePing()
# Create the network
createNetworkFromIgraph(igraph = igraph_adj,
                        title=paste0("Full network (delta = ", 
                                     delta,'; limit = ', limit, ')'),
                        collection="Bacterial genes and clinical")

################################################################################
# 
# # Make first order neighbourhood networks of the clinical variables (Cytoscape)
# # How many non-clinical variables are left
# non.clinical <- which(colours!='#56B4E9')
# # Turn all edges that are not connected to a clinical variable into 0
# for (i in non.clinical) {
#   for (j in non.clinical) {
#     adj_matrix[i,j]<-0
#   }
# }
# 
# # Remove the disconnected nodes
# disconnected.nodes <- which(apply(adj_matrix, 1, function(x){all(x==0)}))
# if (length(disconnected.nodes)!=0) {
#   adj_matrix <- adj_matrix[-disconnected.nodes,-disconnected.nodes]
#   groups <- groups[-disconnected.nodes]
#   shapes <- shapes[-disconnected.nodes]
#   colours <- colours[-disconnected.nodes]
# }
# 
# # Create a qgraph with layout options
# qgraph_adj_mat <- qgraph(input=adj_matrix,
#                          labels=colnames(adj_matrix),
#                          groups=groups,
#                          DoNotPlot=TRUE,
#                          borders=FALSE,
#                          label.font='sans',
#                          posCol="#009E73",   # colour of positive edges
#                          negCol="#D55E00",   # colour of negative edges
#                          color=colours,      # colour of groups
#                          shape=shapes,       # shapes of groups
#                          fade=FALSE,         # edge transparency based on weight
#                          esize=2)
# # Convert qgraph to igraph object
# igraph_adj <- as.igraph(qgraph_adj_mat, attributes = TRUE)
# V(igraph_adj)$name <- colnames(adj_matrix)
# 
# # Connect to cytoscape (Make sure cytoscape is opened)
# cytoscapePing()
# # Create the network
# createNetworkFromIgraph(igraph = igraph_adj,
#                         title=paste0('First-order neighbourhoods of clinical parameters (delta = ',
#                                      delta,'; limit = ', limit, ')'),
#                         collection="Bacterial genes and clinical")
# # 
# # ################################################################################
# 
# # Make first order neighbourhood networks of the clinical variables (plot in R)
# # 
# # # # Use a different green for the R plots
# # # colours[which(colours=='green')] <- 'darkgreen'
# # # Create a qgraph with layout options
# # qgraph_adj_mat <- qgraph(input=adj_matrix,
# #                          labels=colnames(adj_matrix),
# #                          groups=groups,
# #                          DoNotPlot=TRUE,
# #                          borders=FALSE,
# #                          label.font='sans',
# #                          posCol="#009E73", # colour of positive edges
# #                          negCol="#D55E00",       # colour of negative edges
# #                          color=colours,      # colour of groups
# #                          shape=shapes,       # shapes of groups
# #                          fade=FALSE,         # edge transparency based on weight
# #                          esize=2)
# # # Convert qgraph to igraph object
# # igraph_adj <- as.igraph(qgraph_adj_mat, attributes = TRUE) 
# # V(igraph_adj)$name <- colnames(adj_matrix)
# 
# # # Extract the first order neighborhoods:
# # nei_node_graphs_adj <- make_ego_graph(graph = igraph_adj, order = 1)
# # # How many clinical variables are left
# # clinical <- which(colours=='#56B4E9') # Or #E69F00???
# # # Plot the first order neighbourhood for each discrete variable
# # for (i in clinical) {
# #   total <- names(nei_node_graphs_adj[[i]][1])
# #   circle <- layout_in_circle(nei_node_graphs_adj[[i]], 
# #                              # Remove the first node from the circle
# #                              order=total[-which(total==colnames(adj_matrix)[i])])
# #   # Adjust the networks when there is a small number of nodes
# #   if (length(total)<4) {
# #     # Make sure that the vertices are on one line
# #     for (coord in 1:nrow(circle)) {
# #       if (circle[coord,1]==-1) {
# #         circle[coord,2] <- 0
# #       }
# #     }
# #     # Reorder so that a vertical instead of a horizontal network is created
# #     circle <- cbind(circle[,2], circle[,1])
# #   }
# #   # Plot the network
# #   plot(nei_node_graphs_adj[[i]], main=colnames(adj_matrix)[i], layout=circle)
# # }
# 


##################################################################################################################

setwd("/media/sanjee/R_Working_Directory/MGM_Paper")
library(dplyr)
library(RCy3)
library(data.table)

Clin_Bac <- read.table("Adjecency_matrix_Bacterial_genes+Clinical.csv", sep = ",", header = TRUE, row.names = 1)
Clin_Cyt <- read.table("Adjecency_matrix_Cytokine+Clinical.csv", sep = ",", header = TRUE, row.names = 1)
Clin_Hum <- read.table("Adjecency_matrix_Human_genes+Clinical.csv", sep = ",", header = TRUE, row.names = 1)

Clin_Bac[1:36, 1:36] <- 0
Clin_Bac[37:933, 37:933] <- 0
Clin_Bac[934:938, 934:938] <- 0
Clin_Cyt[1:45, 1:45] <- 0
Clin_Cyt[46:75, 46:75] <- 0
Clin_Cyt[76:86, 76:86] <- 0
Clin_Hum[1:31, 1:31] <- 0
Clin_Hum[32:699, 32:699] <- 0
Clin_Hum[700, 700] <- 0


#NodeTable1

Conn1 <- as.data.frame(rowSums(abs(Clin_Bac)))
setDT(Conn1, keep.rownames = TRUE)[]
Clin1 <- Conn1[c(1:36, 934:938), 1:2]
colnames(Clin1) <- c("Clin", "Conn1")

Conn2 <- as.data.frame(rowSums(abs(Clin_Cyt)))
setDT(Conn2, keep.rownames = TRUE)[]
Clin2 <- Conn2[c(1:45, 76:86), 1:2]
colnames(Clin2) <- c("Clin", "Conn2")
NodeTable1 <- full_join(Clin1, Clin2, by = NULL, suffix = c(".x",".y"))

Conn3 <- as.data.frame(rowSums(abs(Clin_Hum)))
setDT(Conn3, keep.rownames = TRUE)[]
Clin3 <- Conn3[c(1:31, 700), 1:2]
colnames(Clin3) <- c("Clin", "Conn3")
NodeTable1 <- full_join(NodeTable1, Clin3, by = NULL, suffix = c(".x",".y"))
NodeTable1 <- as.data.frame(NodeTable1)
NodeTable1[is.na(NodeTable1)] <- 0
NodeTable1 <- mutate(NodeTable1, Conn = (Conn1 + Conn2 + Conn3)/3)
NodeTable1 <- filter(NodeTable1, Conn > 0)

frac = as.vector(c(2, 3, 4, 6, 10, 15, 24, 36))
E = nrow(NodeTable1)
M = NULL
for (i in 1:length(frac)) {
  f = frac[i]
  mes = NULL
  mes = round((f * E)/100, 0)
  M <- as.vector(append(M, mes))
}
diff = (sum(M)) - (nrow(NodeTable1))
ifelse(diff == 0, print("perfect!"), M[8] <- M[8] - diff)

wids <- as.vector(c(30, 25, 18, 15, 12, 10, 8, 6))
wid = NULL
for (j in 1:length(M)) {
  times = M[j]
  value = wids[j]
  wid <- as.vector(append(wid, c(rep(value, times))))
}

wid <- as.data.frame(wid)

NodeTable1 <- NodeTable1[sort(abs(NodeTable1$Conn), decreasing = T, index.return=T)[[2]],]
NodeTable1 <- cbind(NodeTable1, wid)

X = NULL
Y = NULL

obs = 18
seq = NULL
for (i in 0:(obs-1)) {
  totale = NULL
  totalo = NULL
  M = NULL
  print(i)
  ifelse(i == 0, print("0 skipped"), seq <- as.vector(append(seq, i)))
  
  for (j in seq_along(seq)) {
    if(seq[j] %% 2 == 0) totale[j] <- 1
  }
  totale <- na.omit(totale)
  tote = length(totale)
  
  for (jj in seq_along(seq)) {
    if(seq[jj] %% 2 != 0) totalo[jj] <- 1
  }
  totalo <- na.omit(totalo)
  toto = length(totalo)
  
  ifelse(i == 0, M <- round((obs-1)/2), ifelse(i %% 2 == 0, M <- (round((obs-1)/2))+(tote), M <- (round((obs-1)/2))-(toto)))
  
  x = (-10*sqrt(30)*(1-(M/(obs-1)))) + (10*sqrt(30)*(M/(obs-1)))
  X <- as.vector(append(X, x)) 
  y = ((-10*sqrt(30))*(1-(M/(obs-1)))) + ((10*sqrt(30))*(M/(obs-1)))
  Y <- as.vector(append(Y, y))

}
df <- as.data.frame(cbind(X,Y))

#plot(df$X, df$Y)
#labels <- as.vector(row.names(df))
#text(df$Y ~df$X, labels=labels,data=df, cex=0.9, font=2)

NodeTable1 <- cbind(NodeTable1, df)
NodeTable1 <- NodeTable1[-c(2:4)]
colnames(NodeTable1) <- c("Node", "Conn", "wid", "X", "Y")
Groups <- as.vector(rep("Group1", nrow(NodeTable1)))
NodeTable1 <- cbind(NodeTable1, Groups)

#NodeTable2


Bac_Gene <- Conn1[37:933, 1:2]
colnames(Bac_Gene) <- c("Bac", "Conn")
Bac_Gene <- filter(as.data.frame(Bac_Gene), Conn > 0)
NodeTable2 <- as.data.frame(Bac_Gene)

frac = as.vector(c(2, 3, 4, 6, 10, 15, 24, 36))
E = nrow(NodeTable2)
M = NULL
for (i in 1:length(frac)) {
  f = frac[i]
  mes = NULL
  mes = round((f * E)/100, 0)
  M <- as.vector(append(M, mes))
}
diff = (sum(M)) - (nrow(NodeTable2))
ifelse(diff == 0, print("perfect!"), M[8] <- M[8] - diff)

wids <- as.vector(c(30, 25, 18, 15, 12, 10, 8, 6))
wid = NULL
for (j in 1:length(M)) {
  times = M[j]
  value = wids[j]
  wid <- as.vector(append(wid, c(rep(value, times))))
}

wid <- as.data.frame(wid)

NodeTable2 <- NodeTable2[sort(abs(NodeTable2$Conn), decreasing = T, index.return=T)[[2]],]
NodeTable2 <- cbind(NodeTable2, wid)

X = NULL
Y = NULL

for (i in 1:6) {
  print(i)
  x = -141.4213562+(60*cos((i*2*3.14159265359)/12))
  X <- as.vector(append(X, x)) 
  y = 141.4213562+(60*sin((i*2*3.14159265359)/12))
  Y <- as.vector(append(Y, y))
}
df <- as.data.frame(cbind(X,Y))

plot(df$X, df$Y, asp=1)
labels <- as.vector(row.names(df))
text(df$Y ~df$X, labels=labels,data=df, cex=0.9, font=2)


NodeTable2 <- cbind(NodeTable2, df)

colnames(NodeTable2) <- c("Node", "Conn", "wid", "X", "Y")
Groups <- as.vector(rep("Group2", nrow(NodeTable2)))
NodeTable2 <- cbind(NodeTable2, Groups)

#NodeTable3

Cytokines <- Conn2[46:75, 1:2]
colnames(Cytokines) <- c("Cyt", "Conn")
Cytokines <- filter(as.data.frame(Cytokines), Conn > 0)
NodeTable3 <- as.data.frame(Cytokines)

frac = as.vector(c(2, 3, 4, 6, 10, 15, 24, 36))
E = nrow(NodeTable3)
M = NULL
for (i in 1:length(frac)) {
  f = frac[i]
  mes = NULL
  mes = round((f * E)/100, 0)
  M <- as.vector(append(M, mes))
}
diff = (sum(M)) - (nrow(NodeTable3))
ifelse(diff == 0, print("perfect!"), M[8] <- M[8] - diff)

wids <- as.vector(c(30, 25, 18, 15, 12, 10, 8, 6))
wid = NULL
for (j in 1:length(M)) {
  times = M[j]
  value = wids[j]
  wid <- as.vector(append(wid, c(rep(value, times))))
}

wid <- as.data.frame(wid)

NodeTable3 <- NodeTable3[sort(abs(NodeTable3$Conn), decreasing = T, index.return=T)[[2]],]
NodeTable3 <- cbind(NodeTable3, wid)

X = NULL
Y = NULL

for (i in 1) {
  print(i)
  x = -51.76380902+(10*cos((i*2*3.14159265359)/2))
  X <- as.vector(append(X, x)) 
  y = -193.1851653+(10*sin((i*2*3.14159265359)/2))
  Y <- as.vector(append(Y, y))
}
df <- as.data.frame(cbind(X,Y))

plot(df$X, df$Y, asp=1)
labels <- as.vector(row.names(df))
text(df$Y ~df$X, labels=labels,data=df, cex=0.9, font=2)


NodeTable3 <- cbind(NodeTable3, df)

colnames(NodeTable3) <- c("Node", "Conn", "wid", "X", "Y")
Groups <- as.vector(rep("Group3", nrow(NodeTable3)))
NodeTable3 <- cbind(NodeTable3, Groups)

#NodeTable4

Hum_Gene <- Conn3[32:699, 1:2]
colnames(Hum_Gene) <- c("Hum", "Conn")
Hum_Gene <- filter(as.data.frame(Hum_Gene), Conn > 0)
NodeTable4 <- as.data.frame(Hum_Gene)

frac = as.vector(c(2, 3, 4, 6, 10, 15, 24, 36))
E = nrow(NodeTable4)
M = NULL
for (i in 1:length(frac)) {
  f = frac[i]
  mes = NULL
  mes = round((f * E)/100, 0)
  M <- as.vector(append(M, mes))
}
diff = (sum(M)) - (nrow(NodeTable4))
ifelse(diff == 0, print("perfect!"), M[8] <- M[8] - diff)

wids <- as.vector(c(30, 25, 18, 15, 12, 10, 8, 6))
wid = NULL
for (j in 1:length(M)) {
  times = M[j]
  value = wids[j]
  wid <- as.vector(append(wid, c(rep(value, times))))
}

wid <- as.data.frame(wid)

NodeTable4 <- NodeTable4[sort(abs(NodeTable4$Conn), decreasing = T, index.return=T)[[2]],]
NodeTable4 <- cbind(NodeTable4, wid)

X = NULL
Y = NULL

for (i in 1:2) {
  print(i)
  x = 193.1851653+(20*cos((i*2*3.14159265359)/4))
  X <- as.vector(append(X, x)) 
  y = 51.76380902+(20*sin((i*2*3.14159265359)/4))
  Y <- as.vector(append(Y, y))
}
df <- as.data.frame(cbind(X,Y))

plot(df$X, df$Y, asp=1)
labels <- as.vector(row.names(df))
text(df$Y ~df$X, labels=labels,data=df, cex=0.9, font=2)


NodeTable4 <- cbind(NodeTable4, df)

colnames(NodeTable4) <- c("Node", "Conn", "wid", "X", "Y")
Groups <- as.vector(rep("Group4", nrow(NodeTable4)))
NodeTable4 <- cbind(NodeTable4, Groups)

NodeTable <- rbind(NodeTable1, NodeTable2, NodeTable3, NodeTable4)

#EdgeTable1

type = 1

diag(Clin_Bac) = 0
Clin_Bac[lower.tri(Clin_Bac, diag=TRUE)] <- 0

Source = NULL
Target = NULL
Weight = NULL
for (row in 1:nrow(Clin_Bac)) {
  for (col in 1:ncol(Clin_Bac)) {
    if (Clin_Bac[row, col] != 0) {
      Source <- as.vector(append(Source, rownames(Clin_Bac[row, ])))
      Target <- as.vector(append(Target, colnames(Clin_Bac[col])))
      Weight <- as.vector(append(Weight, as.numeric(Clin_Bac[row, col])))
    } else {}
  }
}
Interaction <- as.vector(rep("interacts", length(Weight)))
EdgeTable1 <- as.data.frame(cbind(Source, Target, Weight, Interaction))
EdgeTable1$Weight <- as.numeric(as.character(EdgeTable1$Weight))

frac = as.vector(c(2, 3, 4, 6, 10, 15, 24, 36))
E = nrow(EdgeTable1)
M = NULL
for (i in 1:length(frac)) {
  f = frac[i]
  mes = NULL
  mes = round((f * E)/100, 0)
  M <- as.vector(append(M, mes))
}
diff = (sum(M)) - (nrow(EdgeTable1))
ifelse(diff == 0, print("perfect!"), M[8] <- M[8] - diff)

wids <- as.vector(c(10, 8, 4, 2, 1, 0.5, 0.25, 0.25))
wid = NULL
for (j in 1:length(M)) {
  times = M[j]
  value = wids[j]
  wid <- as.vector(append(wid, c(rep(value, times))))
}

ifelse(type == 1, EdgeTable1 <- mutate(EdgeTable1, width = ifelse(Weight > 0.5, 10, ifelse(Weight < -0.5, 10, ifelse(Weight < 0.5 & Weight > 0.4, 8, ifelse(Weight > -0.5 & Weight < -0.4, 8, ifelse(Weight < 0.4 & Weight > 0.3, 4, ifelse(Weight > -0.4 & Weight < -0.3, 4, ifelse(Weight < 0.3 & Weight > 0.2, 2, ifelse(Weight > -0.3 & Weight < -0.2, 2, ifelse(Weight < 0.2 & Weight > 0.15, 1, ifelse(Weight > -0.2 & Weight < -0.15, 1, ifelse(Weight < 0.15 & Weight > 0.1, 0.5, ifelse(Weight > -0.15 & Weight < -0.1, 0.5, 0.25))))))))))))), ifelse(type == 2, EdgeTable1 <- mutate(EdgeTable1, width = ifelse(Weight > 0.9, 10, ifelse(Weight < -0.9, 10, ifelse(Weight < 0.9 & Weight > 0.8, 8, ifelse(Weight > -0.9 & Weight < -0.8, 8, ifelse(Weight < 0.8 & Weight > 0.7, 4, ifelse(Weight > -0.8 & Weight < -0.7, 4, ifelse(Weight < 0.7 & Weight > 0.6, 2, ifelse(Weight > -0.7 & Weight < -0.6, 2, ifelse(Weight < 0.6 & Weight > 0.5, 1, ifelse(Weight > -0.6 & Weight < -0.15, 1, ifelse(Weight < 0.5 & Weight > 0.4, 0.5, ifelse(Weight > -0.5 & Weight < -0.4, 0.5, 0.25))))))))))))), if(type == 3){
  wid <- as.data.frame(wid)
  EdgeTable1 <- EdgeTable1[sort(abs(EdgeTable1$Weight), decreasing=T, index.return=T)[[2]],]
  EdgeTable1 <- cbind(EdgeTable1, wid)
  colnames(EdgeTable1)[5] <- "width"
} else{
  print("type not selected")}))

ifelse(type == 1, EdgeTable1 <- mutate(EdgeTable1, Stroke = ifelse(Weight > 0.5, "#DC1C13", ifelse(Weight < -0.5, "#1F1FFF", ifelse(Weight < 0.5 & Weight > 0.4, "#EA4C46", ifelse(Weight > -0.5 & Weight < -0.4, "#4949FF", ifelse(Weight < 0.4 & Weight > 0.3, "#F07470", ifelse(Weight > -0.4 & Weight < -0.3, "#7879FF", ifelse(Weight < 0.3 & Weight > 0.2, "#F1959B", ifelse(Weight > -0.3 & Weight < -0.2, "#A3A3FF", ifelse(Weight < 0.2 & Weight > 0.15, "#F6BDC0", ifelse(Weight > -0.2 & Weight < -0.15, "#BFBFFF", ifelse(Weight < 0.15 & Weight > 0.1, "#F6BDC0", ifelse(Weight > -0.15 & Weight < -0.1, "#BFBFFF", "#A6A6A6"))))))))))))), ifelse(type == 2, EdgeTable1 <- mutate(EdgeTable1, Stroke = ifelse(Weight > 0.9, "#DC1C13", ifelse(Weight < -0.9, "#1F1FFF", ifelse(Weight < 0.9 & Weight > 0.8, "#EA4C46", ifelse(Weight > -0.9 & Weight < -0.8, "#4949FF", ifelse(Weight < 0.8 & Weight > 0.7, "#F07470", ifelse(Weight > -0.8 & Weight < -0.7, "#7879FF", ifelse(Weight < 0.7 & Weight > 0.6, "#F1959B", ifelse(Weight > -0.7 & Weight < -0.6, "#A3A3FF", ifelse(Weight < 0.6 & Weight > 0.5, "#F6BDC0", ifelse(Weight > -0.6 & Weight < -0.5, "#BFBFFF", ifelse(Weight < 0.5 & Weight > 0.4, "#F6BDC0", ifelse(Weight > -0.5 & Weight < -0.4, "#BFBFFF", "#A6A6A6"))))))))))))), ifelse(type == 3, EdgeTable1 <- mutate(EdgeTable1, Stroke = ifelse(width == 10 & Weight > 0, "#DC1C13", ifelse(width == 10 & Weight < 0, "#1F1FFF", ifelse(width == 8 & Weight > 0, "#EA4C46", ifelse(width == 8 & Weight < 0, "#4949FF", ifelse(width == 4 & Weight > 0, "#F07470", ifelse(width == 4 & Weight < 0, "#7879FF", ifelse(width == 2 & Weight > 0, "#F1959B", ifelse(width == 2 & Weight < 0, "#A3A3FF", ifelse(width == 1 & Weight > 0, "#F6BDC0", ifelse(width == 1 & Weight < 0, "#BFBFFF", ifelse(width == 0.5 & Weight > 0, "#F6BDC0", ifelse(width == 0.5 & Weight < 0, "#BFBFFF", "#A6A6A6"))))))))))))), "type not selected")))

EdgeTable1$sharedname <- paste(EdgeTable1$Source, "(interacts)", EdgeTable1$Target)

#EdgeTable2

type = 1

diag(Clin_Cyt) = 0
Clin_Cyt[lower.tri(Clin_Cyt, diag=TRUE)] <- 0

Source = NULL
Target = NULL
Weight = NULL
for (row in 1:nrow(Clin_Cyt)) {
  for (col in 1:ncol(Clin_Cyt)) {
    if (Clin_Cyt[row, col] != 0) {
      Source <- as.vector(append(Source, rownames(Clin_Cyt[row, ])))
      Target <- as.vector(append(Target, colnames(Clin_Cyt[col])))
      Weight <- as.vector(append(Weight, as.numeric(Clin_Cyt[row, col])))
    } else {}
  }
}
Interaction <- as.vector(rep("interacts", length(Weight)))
EdgeTable2 <- as.data.frame(cbind(Source, Target, Weight, Interaction))
EdgeTable2$Weight <- as.numeric(as.character(EdgeTable2$Weight))

frac = as.vector(c(2, 3, 4, 6, 10, 15, 24, 36))
E = nrow(EdgeTable2)
M = NULL
for (i in 1:length(frac)) {
  f = frac[i]
  mes = NULL
  mes = round((f * E)/100, 0)
  M <- as.vector(append(M, mes))
}
diff = (sum(M)) - (nrow(EdgeTable2))
ifelse(diff == 0, print("perfect!"), M[8] <- M[8] - diff)

wids <- as.vector(c(10, 8, 4, 2, 1, 0.5, 0.25, 0.25))
wid = NULL
for (j in 1:length(M)) {
  times = M[j]
  value = wids[j]
  wid <- as.vector(append(wid, c(rep(value, times))))
}

ifelse(type == 1, EdgeTable2 <- mutate(EdgeTable2, width = ifelse(Weight > 0.5, 10, ifelse(Weight < -0.5, 10, ifelse(Weight < 0.5 & Weight > 0.4, 8, ifelse(Weight > -0.5 & Weight < -0.4, 8, ifelse(Weight < 0.4 & Weight > 0.3, 4, ifelse(Weight > -0.4 & Weight < -0.3, 4, ifelse(Weight < 0.3 & Weight > 0.2, 2, ifelse(Weight > -0.3 & Weight < -0.2, 2, ifelse(Weight < 0.2 & Weight > 0.15, 1, ifelse(Weight > -0.2 & Weight < -0.15, 1, ifelse(Weight < 0.15 & Weight > 0.1, 0.5, ifelse(Weight > -0.15 & Weight < -0.1, 0.5, 0.25))))))))))))), ifelse(type == 2, EdgeTable2 <- mutate(EdgeTable2, width = ifelse(Weight > 0.9, 10, ifelse(Weight < -0.9, 10, ifelse(Weight < 0.9 & Weight > 0.8, 8, ifelse(Weight > -0.9 & Weight < -0.8, 8, ifelse(Weight < 0.8 & Weight > 0.7, 4, ifelse(Weight > -0.8 & Weight < -0.7, 4, ifelse(Weight < 0.7 & Weight > 0.6, 2, ifelse(Weight > -0.7 & Weight < -0.6, 2, ifelse(Weight < 0.6 & Weight > 0.5, 1, ifelse(Weight > -0.6 & Weight < -0.15, 1, ifelse(Weight < 0.5 & Weight > 0.4, 0.5, ifelse(Weight > -0.5 & Weight < -0.4, 0.5, 0.25))))))))))))), if(type == 3){
  wid <- as.data.frame(wid)
  EdgeTable2 <- EdgeTable2[sort(abs(EdgeTable2$Weight), decreasing=T, index.return=T)[[2]],]
  EdgeTable2 <- cbind(EdgeTable2, wid)
  colnames(EdgeTable2)[5] <- "width"
} else{
  print("type not selected")}))

ifelse(type == 1, EdgeTable2 <- mutate(EdgeTable2, Stroke = ifelse(Weight > 0.5, "#DC1C13", ifelse(Weight < -0.5, "#1F1FFF", ifelse(Weight < 0.5 & Weight > 0.4, "#EA4C46", ifelse(Weight > -0.5 & Weight < -0.4, "#4949FF", ifelse(Weight < 0.4 & Weight > 0.3, "#F07470", ifelse(Weight > -0.4 & Weight < -0.3, "#7879FF", ifelse(Weight < 0.3 & Weight > 0.2, "#F1959B", ifelse(Weight > -0.3 & Weight < -0.2, "#A3A3FF", ifelse(Weight < 0.2 & Weight > 0.15, "#F6BDC0", ifelse(Weight > -0.2 & Weight < -0.15, "#BFBFFF", ifelse(Weight < 0.15 & Weight > 0.1, "#F6BDC0", ifelse(Weight > -0.15 & Weight < -0.1, "#BFBFFF", "#A6A6A6"))))))))))))), ifelse(type == 2, EdgeTable2 <- mutate(EdgeTable2, Stroke = ifelse(Weight > 0.9, "#DC1C13", ifelse(Weight < -0.9, "#1F1FFF", ifelse(Weight < 0.9 & Weight > 0.8, "#EA4C46", ifelse(Weight > -0.9 & Weight < -0.8, "#4949FF", ifelse(Weight < 0.8 & Weight > 0.7, "#F07470", ifelse(Weight > -0.8 & Weight < -0.7, "#7879FF", ifelse(Weight < 0.7 & Weight > 0.6, "#F1959B", ifelse(Weight > -0.7 & Weight < -0.6, "#A3A3FF", ifelse(Weight < 0.6 & Weight > 0.5, "#F6BDC0", ifelse(Weight > -0.6 & Weight < -0.5, "#BFBFFF", ifelse(Weight < 0.5 & Weight > 0.4, "#F6BDC0", ifelse(Weight > -0.5 & Weight < -0.4, "#BFBFFF", "#A6A6A6"))))))))))))), ifelse(type == 3, EdgeTable2 <- mutate(EdgeTable2, Stroke = ifelse(width == 10 & Weight > 0, "#DC1C13", ifelse(width == 10 & Weight < 0, "#1F1FFF", ifelse(width == 8 & Weight > 0, "#EA4C46", ifelse(width == 8 & Weight < 0, "#4949FF", ifelse(width == 4 & Weight > 0, "#F07470", ifelse(width == 4 & Weight < 0, "#7879FF", ifelse(width == 2 & Weight > 0, "#F1959B", ifelse(width == 2 & Weight < 0, "#A3A3FF", ifelse(width == 1 & Weight > 0, "#F6BDC0", ifelse(width == 1 & Weight < 0, "#BFBFFF", ifelse(width == 0.5 & Weight > 0, "#F6BDC0", ifelse(width == 0.5 & Weight < 0, "#BFBFFF", "#A6A6A6"))))))))))))), "type not selected")))

EdgeTable2$sharedname <- paste(EdgeTable2$Source, "(interacts)", EdgeTable2$Target)

#EdgeTable3

type = 1

diag(Clin_Hum) = 0
Clin_Hum[lower.tri(Clin_Hum, diag=TRUE)] <- 0

Source = NULL
Target = NULL
Weight = NULL
for (row in 1:nrow(Clin_Hum)) {
  for (col in 1:ncol(Clin_Hum)) {
    if (Clin_Hum[row, col] != 0) {
      Source <- as.vector(append(Source, rownames(Clin_Hum[row, ])))
      Target <- as.vector(append(Target, colnames(Clin_Hum[col])))
      Weight <- as.vector(append(Weight, as.numeric(Clin_Hum[row, col])))
    } else {}
  }
}
Interaction <- as.vector(rep("interacts", length(Weight)))
EdgeTable3 <- as.data.frame(cbind(Source, Target, Weight, Interaction))
EdgeTable3$Weight <- as.numeric(as.character(EdgeTable3$Weight))

frac = as.vector(c(2, 3, 4, 6, 10, 15, 24, 36))
E = nrow(EdgeTable3)
M = NULL
for (i in 1:length(frac)) {
  f = frac[i]
  mes = NULL
  mes = round((f * E)/100, 0)
  M <- as.vector(append(M, mes))
}
diff = (sum(M)) - (nrow(EdgeTable3))
ifelse(diff == 0, print("perfect!"), M[8] <- M[8] - diff)

wids <- as.vector(c(10, 8, 4, 2, 1, 0.5, 0.25, 0.25))
wid = NULL
for (j in 1:length(M)) {
  times = M[j]
  value = wids[j]
  wid <- as.vector(append(wid, c(rep(value, times))))
}

ifelse(type == 1, EdgeTable3 <- mutate(EdgeTable3, width = ifelse(Weight > 0.5, 10, ifelse(Weight < -0.5, 10, ifelse(Weight < 0.5 & Weight > 0.4, 8, ifelse(Weight > -0.5 & Weight < -0.4, 8, ifelse(Weight < 0.4 & Weight > 0.3, 4, ifelse(Weight > -0.4 & Weight < -0.3, 4, ifelse(Weight < 0.3 & Weight > 0.2, 2, ifelse(Weight > -0.3 & Weight < -0.2, 2, ifelse(Weight < 0.2 & Weight > 0.15, 1, ifelse(Weight > -0.2 & Weight < -0.15, 1, ifelse(Weight < 0.15 & Weight > 0.1, 0.5, ifelse(Weight > -0.15 & Weight < -0.1, 0.5, 0.25))))))))))))), ifelse(type == 2, EdgeTable3 <- mutate(EdgeTable3, width = ifelse(Weight > 0.9, 10, ifelse(Weight < -0.9, 10, ifelse(Weight < 0.9 & Weight > 0.8, 8, ifelse(Weight > -0.9 & Weight < -0.8, 8, ifelse(Weight < 0.8 & Weight > 0.7, 4, ifelse(Weight > -0.8 & Weight < -0.7, 4, ifelse(Weight < 0.7 & Weight > 0.6, 2, ifelse(Weight > -0.7 & Weight < -0.6, 2, ifelse(Weight < 0.6 & Weight > 0.5, 1, ifelse(Weight > -0.6 & Weight < -0.15, 1, ifelse(Weight < 0.5 & Weight > 0.4, 0.5, ifelse(Weight > -0.5 & Weight < -0.4, 0.5, 0.25))))))))))))), if(type == 3){
  wid <- as.data.frame(wid)
  EdgeTable3 <- EdgeTable3[sort(abs(EdgeTable3$Weight), decreasing=T, index.return=T)[[2]],]
  EdgeTable3 <- cbind(EdgeTable3, wid)
  colnames(EdgeTable3)[5] <- "width"
} else{
  print("type not selected")}))

ifelse(type == 1, EdgeTable3 <- mutate(EdgeTable3, Stroke = ifelse(Weight > 0.5, "#DC1C13", ifelse(Weight < -0.5, "#1F1FFF", ifelse(Weight < 0.5 & Weight > 0.4, "#EA4C46", ifelse(Weight > -0.5 & Weight < -0.4, "#4949FF", ifelse(Weight < 0.4 & Weight > 0.3, "#F07470", ifelse(Weight > -0.4 & Weight < -0.3, "#7879FF", ifelse(Weight < 0.3 & Weight > 0.2, "#F1959B", ifelse(Weight > -0.3 & Weight < -0.2, "#A3A3FF", ifelse(Weight < 0.2 & Weight > 0.15, "#F6BDC0", ifelse(Weight > -0.2 & Weight < -0.15, "#BFBFFF", ifelse(Weight < 0.15 & Weight > 0.1, "#F6BDC0", ifelse(Weight > -0.15 & Weight < -0.1, "#BFBFFF", "#A6A6A6"))))))))))))), ifelse(type == 2, EdgeTable3 <- mutate(EdgeTable3, Stroke = ifelse(Weight > 0.9, "#DC1C13", ifelse(Weight < -0.9, "#1F1FFF", ifelse(Weight < 0.9 & Weight > 0.8, "#EA4C46", ifelse(Weight > -0.9 & Weight < -0.8, "#4949FF", ifelse(Weight < 0.8 & Weight > 0.7, "#F07470", ifelse(Weight > -0.8 & Weight < -0.7, "#7879FF", ifelse(Weight < 0.7 & Weight > 0.6, "#F1959B", ifelse(Weight > -0.7 & Weight < -0.6, "#A3A3FF", ifelse(Weight < 0.6 & Weight > 0.5, "#F6BDC0", ifelse(Weight > -0.6 & Weight < -0.5, "#BFBFFF", ifelse(Weight < 0.5 & Weight > 0.4, "#F6BDC0", ifelse(Weight > -0.5 & Weight < -0.4, "#BFBFFF", "#A6A6A6"))))))))))))), ifelse(type == 3, EdgeTable3 <- mutate(EdgeTable3, Stroke = ifelse(width == 10 & Weight > 0, "#DC1C13", ifelse(width == 10 & Weight < 0, "#1F1FFF", ifelse(width == 8 & Weight > 0, "#EA4C46", ifelse(width == 8 & Weight < 0, "#4949FF", ifelse(width == 4 & Weight > 0, "#F07470", ifelse(width == 4 & Weight < 0, "#7879FF", ifelse(width == 2 & Weight > 0, "#F1959B", ifelse(width == 2 & Weight < 0, "#A3A3FF", ifelse(width == 1 & Weight > 0, "#F6BDC0", ifelse(width == 1 & Weight < 0, "#BFBFFF", ifelse(width == 0.5 & Weight > 0, "#F6BDC0", ifelse(width == 0.5 & Weight < 0, "#BFBFFF", "#A6A6A6"))))))))))))), "type not selected")))

EdgeTable3$sharedname <- paste(EdgeTable3$Source, "(interacts)", EdgeTable3$Target)

EdgeTable <- rbind(EdgeTable1, EdgeTable2, EdgeTable3)

#write.table(NodeTable, "NodeTable.csv", sep = ",")
#write.table(EdgeTable, "EdgeTable.csv", sep = ",")
#NodeTable <- read.table("NodeTable.csv", sep = ",", header = TRUE, row.names = 1)
#EdgeTable <- read.table("EdgeTable.csv", sep = ",", header = TRUE, row.names = 1)

NodeTable <- as.data.frame(NodeTable)
EdgeTable <- as.data.frame(EdgeTable)

Network_name = "Visual_Network_1"
Network_Collection = "Visual_Networks_1"

nodes = NULL
edges = NULL

nodes <- data.frame(id=as.vector(NodeTable$Node), group=as.vector(NodeTable$Groups), stringsAsFactors = FALSE)
edges <- data.frame(source=as.vector(EdgeTable$Source), target=as.vector(EdgeTable$Target), interaction=as.vector(EdgeTable$Interaction), weight=as.vector(EdgeTable$Weight), stringsAsFactors = FALSE)

createNetworkFromDataFrames(nodes, edges, title="Visual_Network", collection="Visual_Networks", style.name = "SanjeeNetworkStyle")


Colour_palette <- as.vector(c("#0073C2", "#EFC000", "#CD534C", "#7AA6DC", "#8F7700", "#003C67", "#868686", "#3B3B3B", "#A73030", "#4A6990"))

style.name = "SanjeeNetworkStyle"
defaults <- list(NODE_SHAPE="Ellipse", EDGE_TRANSPARENCY=255 , NODE_LABEL_POSITION="W,E,c,0.00,0.00", NODE_BORDER_PAINT="#FFFFFF")
nodeLabels <- mapVisualProperty("Node Label", "id", "p")
nodecolour <- mapVisualProperty("Node Fill Color", "group", "d", as.vector(unique(NodeTable$Groups)), as.vector(Colour_palette[1:length(unique(NodeTable$Groups))]))
nodeXlocation <- mapVisualProperty("Node X Location", "id", "d", as.vector(NodeTable$Node), as.vector(NodeTable$X))
nodeYlocation <- mapVisualProperty("Node Y Location", "id", "d", as.vector(NodeTable$Node), as.vector(NodeTable$Y))
nodesize <- mapVisualProperty("Node Size", "shared name", "d", as.vector(NodeTable$Node), as.vector(as.numeric(as.character(NodeTable$wid))))
edgeline <- mapVisualProperty("Edge Line Type", "interaction", "d", as.vector(unique(EdgeTable$Interaction)), as.vector(c("Solid")))
edgewidth <- mapVisualProperty("Edge Width", "shared name", "d", as.vector(EdgeTable$sharedname), as.vector(EdgeTable$width))
edgestroke <- mapVisualProperty("Edge Stroke Unselected Paint", "shared name", "d", as.vector(EdgeTable$sharedname), as.vector(EdgeTable$Stroke))

createVisualStyle(style.name, defaults, list(nodeLabels, nodecolour, nodeXlocation, nodeYlocation, edgeline, edgewidth, edgestroke))
setVisualStyle("SanjeeNetworkStyle")

fitContent(selected.only = FALSE)

########################################################################################################

for(n in (1:maxn)){
  print(n)
  if (file.exists("D:/Matlab_Working_directory/PCA-PMI-master/lib/PMI_result.txt")) {
    PMImat <- as.matrix(read.table(file = "PMI_result.txt", header = FALSE))
  } else {
    Sys.sleep(time)
  }
}