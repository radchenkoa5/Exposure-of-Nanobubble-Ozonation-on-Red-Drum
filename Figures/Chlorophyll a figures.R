# setup the R enviornment for kniting markdown doc properly
library(knitr)
opts_knit$set(root.dir='../')
chlwq <- read.csv("./Data/ChlaWQReplicateAcuteData2.csv")
