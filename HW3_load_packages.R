#HW3

##### install packages #####


packages <- c("ggplot2","GGally","rpart","caret","leaps","boot","reshape2")

install.packages(packages)

lapply(as.list(packages),library, character.only = TRUE)
