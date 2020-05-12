library(data.table)
library(igraph)
library(lsa)
library(dplyr)

setwd("C:/Users/OWNER/OneDrive - Emory University/Fall 2019/Social Network Analytics/Excel Files")
totalnetwork <- fread("Player and provider network.csv", header=TRUE)
players <- fread("Player network.csv", header = TRUE)
stats <- fread("Player stats.csv", header = TRUE)

names <- as.matrix(totalnetwork[,1])


totalnetwork <- as.matrix(totalnetwork)
class(totalnetwork) <- "numeric"
rownames(totalnetwork) <- names
totalnetwork <- totalnetwork[,-1]

players <- as.matrix(players)
rownames(players) <- names
players <- players[,-1]

# make the co-membership matrix for drug dealer
totalnetwork <- as.matrix(totalnetwork)
transpose <- t(totalnetwork)
dealers <- (transpose)%*%(totalnetwork)

getNetStats=function(net)
{
  deg_in = degree(net, mode = "in")
  deg_out = degree(net, mode = "out")
  close= closeness(net, mode = "total")
  betw = betweenness(net)
  prank = page_rank(net)$vector # page_rank creates a list object with some other items in it, we just want the actual scores, which are contained in the object called "vector"
  eigen.cent = eigen_centrality(net)$vector
  id=V(net)$name
  stats= as.data.table(list(id = id, deg_in = deg_in, deg_out = deg_out, close = close, betw = betw, prank = prank, eigen.cent = eigen.cent))
  return(stats)
}


# totalnetwork
g_total <- graph.incidence(totalnetwork,directed = FALSE)
plot(g_total, vertex.color=V(g_total)$type)
netstats_total <- getNetStats(g_total)
jaccard_total <- similarity(g_total, loops = FALSE, method="jaccard")
# does it make sense to calculate cosine similarity here?

# dealers
g_dealers <- graph_from_adjacency_matrix(dealers,mode="undirected")
g_dealers <- simplify(g_dealers)
plot(g_dealers, vertex.color=V(g_dealers)$type)
netstats_dealers <- getNetStats(g_dealers)
jaccard_dealers <- similarity(g_dealers, loops = FALSE, method="jaccard")
cosine_dealers <- cosine(dealers)

# players
g_players <- graph_from_adjacency_matrix(as.matrix(players),mode="undirected")
g_players <- simplify(g_players)
plot(g_players, vertex.color=V(g_players)$type)
netstats_players <- getNetStats(g_players)
jaccard_players <- similarity(g_players, loops = FALSE, method= "jaccard")
class(players) <- "numeric"
cosine_players <- cosine(players)

# regressions
# Are certain players "better" because they got drugs from certain dealers?
# control for age in 2007
totalnetwork_2 <- totalnetwork
totalnetwork_2 <- data.frame(Player = row.names(totalnetwork_2), totalnetwork_2)
stats_totalnetwork <- left_join(totalnetwork_2, stats, by ="Player")
model_dealers <- lm(WAR ~ BALCO + Greg_Anderson + Palm_Beach_Rejuvenation_Centre + New_Hope_Health_Centre + Signature_Pharmacy + 
                      American_Pharmaceutical_Group + Health_Rejuvenation_Centre + Applied_Pharmacy_Services + Arizona_AntiAging_Clinic + 
                      Health_Watch_Clinic + BrianMcNamee + `2007 age`, data=stats_totalnetwork)
summary(model_dealers)

# network statistics
colnames(netstats_total)[1] <- "Player"
totalnetwork_3 <- left_join(netstats_total, stats, by="Player") 
model_network <- lm(deg_in ~ WAR + `WS title`, data=totalnetwork_3)
summary(model_network)
colnames(netstats_players)[1] <- "Player"
totalnetwork_4 <- left_join(netstats_players, stats, by="Player") 
model_network <- lm(eigen.cent ~ WAR, data=totalnetwork_4)
summary(model_network)

# How does similarity among players influence whether they won a WS or not?
# LEMONS PROBLEM
jaccard_average <- cbind(totalnetwork_4$Player, unlist(lapply(seq_along(1:nrow(jaccard_players)), function(y) mean(as.numeric(jaccard_players[y,])))))
jaccard_average <- as.data.frame(jaccard_average)
colnames(jaccard_average) <- c("Player","average_Jaccard")
totalnetwork_4 <- left_join(totalnetwork_4, jaccard_average, by = "Player")
totalnetwork_4$average_Jaccard <- as.numeric(as.character(totalnetwork_4$average_Jaccard))
model_jaccard <- lm(WAR ~ average_Jaccard, data=totalnetwork_4)
summary(model_jaccard)

temp <- totalnetwork_4[!is.na(totalnetwork_4$`Last year`),]
cor(temp[,c(2:7,10:19)])

# Predict WAR using network statistics
model_network_stats <- lm(WAR ~ deg_in + betw + close, data=totalnetwork_4)
summary(model_network_stats)

# predict if a player got drugs from a certain dealer based on their WAR
# BALCO
model_BALCO <- glm(BALCO ~ WAR, data = stats_totalnetwork)
summary(model_BALCO)
model_BALCO_lm <- lm(WAR ~ BALCO, data=stats_totalnetwork)
summary(model_BALCO_lm)

# Greg_Anderson
model_anderson <- glm(Greg_Anderson ~ WAR, data = stats_totalnetwork)
summary(model_anderson)
model_anderson_lm <- lm(WAR ~ Greg_Anderson, data=stats_totalnetwork)
summary(model_anderson_lm)

# Palm_Beach_Rejuvenation_Centre - negative both ways
# one of the dealers of HGH
model_PBRC <- glm(Palm_Beach_Rejuvenation_Centre ~ WAR, data = stats_totalnetwork)
summary(model_PBRC)
model_PBRC_lm <- lm(WAR ~ Palm_Beach_Rejuvenation_Centre, data=stats_totalnetwork)
summary(model_PBRC_lm)

# New_Hope_Health_Centre
model_NHHC <- glm(New_Hope_Health_Centre ~ WAR, data = stats_totalnetwork)
summary(model_NHHC)
model_NHHC_lm <- lm(WAR ~ New_Hope_Health_Centre, data=stats_totalnetwork)
summary(model_NHHC_lm)

# Signature_Pharmacy - negative both ways
model_SP <- glm(Signature_Pharmacy ~ WAR, data = stats_totalnetwork)
summary(model_SP)
model_SP_lm <- lm(WAR ~ Signature_Pharmacy, data=stats_totalnetwork)
summary(model_SP_lm)

# American_Pharmaceutical_Group - negative both ways
model_APG <- glm(American_Pharmaceutical_Group ~ WAR, data = stats_totalnetwork)
summary(model_APG)
model_APG_lm <- lm(WAR ~ American_Pharmaceutical_Group, data=stats_totalnetwork)
summary(model_APG_lm)

# Health_Rejuvenation_Centre
model_HRC <- glm(Health_Rejuvenation_Centre ~ WAR, data = stats_totalnetwork)
summary(model_HRC)
model_HRC_lm <- lm(WAR ~ Health_Rejuvenation_Centre, data=stats_totalnetwork)
summary(model_HRC_lm)

# Applied_Pharmacy_Services
model_APS <- glm(Applied_Pharmacy_Services ~ WAR, data = stats_totalnetwork)
summary(model_APS)
model_APS_lm <- lm(WAR ~ Applied_Pharmacy_Services, data=stats_totalnetwork)
summary(model_APS_lm)

# Arizona_AntiAging_Clinic - negative
model_AAC <- glm(Arizona_AntiAging_Clinic ~ WAR, data = stats_totalnetwork)
summary(model_AAC)
model_AAC_lm <- lm(WAR ~ Arizona_AntiAging_Clinic, data=stats_totalnetwork)
summary(model_AAC_lm)

# Health_Watch_Clinic
model_HWC <- glm(Health_Watch_Clinic ~ WAR, data = stats_totalnetwork)
summary(model_HWC)
model_HWC_lm <- lm(WAR ~ Health_Watch_Clinic, data=stats_totalnetwork)
summary(model_HWC_lm)

# BrianMcNamee - huge increase in WAR
model_BM <- glm(BrianMcNamee ~ WAR, data = stats_totalnetwork)
summary(model_BM)
model_BM_lm <- lm(WAR ~ BrianMcNamee, data=stats_totalnetwork)
summary(model_BM_lm)

# Kirk_Radomski - the worse players are the ones getting from Kirk Radomski, they were already bad beforehand, and most players who got
# drugs from Radomski only got them from him
model_KR <- glm(Kirk_Radomski ~ WAR, data = stats_totalnetwork)
summary(model_KR)
model_KR_lm <- lm(WAR ~ Kirk_Radomski, data=stats_totalnetwork)
summary(model_KR_lm)

WAR_years <- fread("formatted.csv", header = TRUE)
totalnetwork_4 <- left_join(totalnetwork_4, WAR_years, by="Player")

# formatting and adding flags
pre95 <- totalnetwork_4[,c("1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994")]
pre95$Pre95 <- rowMeans(pre95, na.rm=TRUE)
pre95 <- pre95[,12]

steroids <- totalnetwork_4[,c("1995","1996","1997","1998","1999","2000","2001","2002","2003")]
steroids$steroid_era <- rowMeans(steroids, na.rm = TRUE)
steroids <- steroids[,10]

post03 <- totalnetwork_4[,c("2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")]
post03$Post03 <- rowMeans(post03, na.rm=TRUE)
post03 <- post03[,12]

totalnetwork_4 <- cbind(totalnetwork_4, pre95, steroids, post03)

# create flags
totalnetwork_4$greater_in_steroid_era <- ifelse(totalnetwork_4$steroids > totalnetwork_4$pre95, 1, 0)
totalnetwork_4$less_after_steroid_era <- ifelse(totalnetwork_4$steroids > totalnetwork_4$post03, 1, 0)

# regressions
# predict if WAR is greater in steroid era than it was pre-1995
model_greater <- glm(greater_in_steroid_era ~ eigen.cent + average_Jaccard, data= totalnetwork_4)
summary(model_greater)

# predict if WAR is less after steroid era, post-2003
model_less <- glm(less_after_steroid_era ~ eigen.cent + average_Jaccard, data= totalnetwork_4)
summary(model_less)

model <- lm(eigen.cent ~ greater_in_steroid_era, data=totalnetwork_4)
summary(model)
