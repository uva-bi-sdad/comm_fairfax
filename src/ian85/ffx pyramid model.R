
{if (!require(rgeos)) {
    install.packages("rgeos", repos = "http://cran.us.r-project.org")
    require(rgeos)
}
if (!require(rgdal)) {
    install.packages("rgdal", repos = "http://cran.us.r-project.org")
    require(rgdal)
}
if (!require(raster)) {
    install.packages("raster", repos = "http://cran.us.r-project.org")
    require(raster)
}
if(!require(ggplot2)) {
    install.packages("ggplot2", repos="http://cloud.r-project.org")
    require(ggplot2)
}
if(!require(viridis)) {
    install.packages("viridis", repos="http://cloud.r-project.org")
    require(viridis)
}
if(!require(dplyr)) {
    install.packages("dplyr", repos = "https://cloud.r-project.org/")
    require(dplyr)
}
if(!require(gtable)) {
    install.packages("gtable", repos = "https://cloud.r-project.org/")
    require(gtable)
}
if(!require(grid)) {
    install.packages("grid", repos = "https://cloud.r-project.org/")
    require(grid)
}
if(!require(readxl)) {
    install.packages("readxl", repos = "https://cloud.r-project.org/")
    require(readxl)
}
if(!require(magrittr)) {
    install.packages("magrittr", repos = "https://cloud.r-project.org/")
    require(magrittr)
}
if (!require(extrafont)) {
    install.packages("extrafont", repos = "http://cran.us.r-project.org")
    require(extrafont)
}
if (!require(tigris)) {
    install.packages("tigris", repos = "http://cran.us.r-project.org")
    require(tigris)
}
if (!require(sp)) {
    install.packages("sp", repos = "http://cran.us.r-project.org")
    require(sp)
}
if (!require(data.table)) {
    install.packages("data.table", repos = "http://cran.us.r-project.org")
    require(data.table)
}}
library(dplyr)
vip2 = function (x, sort = TRUE, n.var = min(30, nrow(x$importance)),
                 type = NULL, class = NULL, scale = TRUE, main = deparse(substitute(x)),
                 ...){
    if (!inherits(x, "randomForest"))
        stop("This function only works for objects of class `randomForest'")
    imp <- importance(x, class = class, scale = scale, type = type,
                      ...)
    if (ncol(imp) > 2)
        imp <- imp[, -(1:(ncol(imp) - 2))]
    nmeas <- ncol(imp)
    if (nmeas > 1) {
        op <- par(mfrow = c(1, 2), mar = c(4, 5, 4, 1), mgp = c(2,
                                                                0.8, 0), oma = c(0, 0, 2, 0), no.readonly = TRUE)
        on.exit(par(op))
    }
    for (i in 1:nmeas) {
        ord <- if (sort)
            rev(order(imp[, i], decreasing = TRUE)[1:n.var])
        else 1:n.var
        xmin <- if (colnames(imp)[i] %in% c("IncNodePurity",
                                            "MeanDecreaseGini"))
            0
        else min(imp[ord, i])
        dotchart(imp[ord, i], ylab = "",
                 main = if (nmeas == 1)
                     main
                 else NULL, xlim = c(xmin, max(imp[, i])), ...)
    }
    if (nmeas > 1)
        mtext(outer = TRUE, side = 3, text = main, cex = 1.2)
    invisible(imp)
}



# Pull in the datas

# vicki's file for the proportion of snap retailers which are grocery stores
propHealthySnap = read.csv("./data/comm_fairfax/pyramid level data/propHealthySNAP.csv")

# ffx pyramid level summary
ffxysData <- read.csv('data/comm_fairfax/working/formatted_youth_survey/2015_supplemental_8_10_12_pyramid.csv')
colnames(ffxysData)[2] = "SCHOOL_NAM"

exercise.cols = c("Physical_Activity_None", "Physical_Activity_5Plus")
food.cols = c("Fruit_Veg_5", "Combined_sugary_drinks")
weight.cols = "Unhealthy_Weight_Loss"
all.cols = c(exercise.cols, food.cols, weight.cols, "Food_Insecurity")

# Data for amount of land devoted to parks, trails, etc

recData = read.csv("./data/comm_fairfax/working/pyramid_data/pyramid_data_complete.csv")[,-1]
recData = recData[,!grepl("PERCENTAGE", colnames(recData))]
areas <- readOGR("data/comm_fairfax/working/pyramid_data/pyramid_shp")@data[,c("SCHOOL_NAM", "Shape_Area")]

recData[,-1] = apply(recData[,-1], 2, function(x) x/areas[,2])

# lat/long of the high schools

ffxHsLatLong = read.csv("/home/sdal/projects/ffx/comm_fairfax/pyramid level data/ffxHsLatLong.csv")[,c(1, 3, 2)]

# Josh's data for distances to things
library(geosphere)
distPath = "/home/sdal/projects/ffx/comm_fairfax/nearest_distance/"
distFiles = list.files(distPath, pattern = "distances")
distances = matrix(NA, 25, 7)
colnames(distances) = gsub(".csv", "", distFiles)

t0 = Sys.time()
for(i in 1:7){
    nearDist = read.csv(paste0(distPath, distFiles[i]))
    distRows = numeric(25)
    for(j in 1:25){
        dists = distm(ffxHsLatLong[j, 2:3], nearDist[,1:2])
        distRows[j] = which.min(dists)
    }
    distances[,i] = nearDist[distRows, 7]
}
Sys.time() - t0

distFoodSchool = data.frame(SCHOOL_NAM = ffxHsLatLong$highSchool, distances)

# Put it all together

propHealthySnap[,-1] %>%
    merge(recData) %>%
    merge(distFoodSchool) ->
    pyramidFeatures

ffxysData %>%
    filter(Demographic == "Overall") %>%
    dplyr::select(c("SCHOOL_NAM", all.cols)) ->
    pyramidResponses

ffxysData %>%
    filter(Demographic == "Overall") %>%
    dplyr::select(-one_of(c(all.cols, "Pyramid_Number"))) ->
    ffxysFeatures

pyramidFeatures = merge(pyramidFeatures, ffxysFeatures[,-2])


rownames(pyramidFeatures) = pyramidFeatures[,1]
pyramidFeatures = as.matrix(dplyr::select(pyramidFeatures, -SCHOOL_NAM))

rownames(pyramidResponses) = pyramidResponses[,1]
pyramidResponses = as.matrix(dplyr::select(pyramidResponses, -SCHOOL_NAM))

#CART
library(randomForest)
pyramidFeatures = scale(pyramidFeatures)
pyramidResponses = scale(pyramidResponses)

for(i in 2:7){
    dat = cbind(y = pyramidResponses[,i], pyramidFeatures)
    fit.rf = randomForest(y~ ., data = dat, ntree = 1000)
    print(colnames(pyramidResponses)[i])
    print(mean(fit.rf$rsq))
    #print(fit.rf)
    #pdf(paste0("./CART images/", colnames(responses)[i], "impvar.pdf"), width = 12, height = 8)
    # layout(matrix(c(1,1,2,1,1,3,1, 1, 4), nrow = 3, ncol = 3, byrow = TRUE))
    # vip2(fit.rf, main = '', cex = 1.2, xlab = "Decrease in MSE from Split")
    imp = importance(fit.rf)
    impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
    colors = c("black", "blue", "red")
    # for(j in 1:3){
    #     partialPlot(fit.rf, dat, impvar[j], main=paste("Partial Dependence"), xlab = impvar[j], lwd= 3, cex.lab = 2, n.pt = 100)
    #
    # }
    #dev.off()
}






# Lasso
library(glmnet)
for(i in 2:7){
    y = pyramidResponses[,i]
    fit.lasso = cv.glmnet(pyramidFeatures, y, grouped = F)
    #print(fit.lasso)
    row = which(fit.lasso$glmnet.fit$lambda == fit.lasso$lambda.min)
    print(i)
    print(fit.lasso$glmnet.fit$beta[row,])


    # layout(matrix(c(1,1,2,1,1,3,1, 1, 4), nrow = 3, ncol = 3, byrow = TRUE))
    # vip2(fit.rf, main = '', cex = 1.2, xlab = "Decrease in MSE from Split")
    # imp = importance(fit.rf)
    # impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
    # colors = c("black", "blue", "red")
    # for(j in 1:3){
    #     partialPlot(fit.rf, dat, impvar[j], main=paste("Partial Dependence"), xlab = impvar[j], lwd= 3, cex.lab = 2, n.pt = 100)
    #
    # }
    # #dev.off()
}

# PLS
library(pls)

pyramidPLS <- plsr(as.matrix(pyramidResponses[,-1]) ~ pyramidFeatures, ncomp = 10, validation = "LOO")
summary(pyramidPLS)
plot(RMSEP(pyramidPLS), legendpos = "topright", bty = 'n')

dimnames(tmp$val)$response = c("No Physical Activity", "5+ Days of Physical Activity", "Five or More Fruit and Veg", "At Least One Sugary Drink Per Day", "Unhealthy Weight Loss", "Food Insecurity")
tmp$type = NULL
plot(tmp, cex.axis = 2, cex.main = 2, xlab = "")




#
# Report on modeling efforts. Make plots and some kinds of summaries for the 3 models, as well as the pairs plot for the response variables
#


# pairs plot for the response variables
library(GGally)

plot.out.dir = "./output/ffxysMaps/"

pr = pyramidResponses[-9,-1]
rownames(pr) = pyramidResponses[-9,1]
colLabs = c("No Physical Activity", "5+ Days of Physical Activity", "Five or More Fruit and Veg", "1+ Sugary Drink Per Day", "Unhealthy Weight Loss", "Food Insecurity")
colLabs = paste0("R ", 1:6)

pdf(paste0(plot.out.dir, "pairs", ".pdf"), height = 8, width = 12)
pm = ggpairs(pr, columnLabels = colLabs, showStrips = FALSE, mapping = ggplot2::aes())
pm + theme(text = element_text(size = 15))
dev.off()

# lollipops!

ffxysFilter = filter(ffxysData[,c(all.cols, "Demographic", "SCHOOL_NAM")], Demographic == "Overall", SCHOOL_NAM != "THOMAS JEFFERSON")
ffxysFilter = dplyr::select(ffxysFilter, -Demographic)
lolliPlot = melt(ffxysFilter, id.vars = "SCHOOL_NAM")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

variable_names = c("Physical_Activity_None" = "No Physical Activity",
                   "Physical_Activity_5Plus" = "5+ Days of Physical Activity",
                   "Fruit_Veg_5" = "Five or More Fruit and Veg",
                   "Combined_sugary_drinks" = "1+ Sugary Drink Per Day",
                   "Unhealthy_Weight_Loss" = "Unhealthy Weight Loss",
                   "Food_Insecurity" = "Food Insecurity")

# Aggregate to get the dashed lines for the means

means = stats::aggregate(list(Percent = lolliPlot$value),list(variable = lolliPlot$variable), mean)

# Get strings to lower case

lolliPlot$SCHOOL_NAM = tolower(lolliPlot$SCHOOL_NAM)
lolliPlot$SCHOOL_NAM = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", lolliPlot$SCHOOL_NAM, perl=TRUE)

# Order the factor by food insecurity

lolliPlot$SCHOOL_NAM = ordered(lolliPlot$SCHOOL_NAM, levels = arrange(filter(lolliPlot, variable == "Food_Insecurity"), value)$SCHOOL_NAM)
lolliPlot$variable = factor(lolliPlot$variable, levels = c("Food_Insecurity", "Physical_Activity_None", "Combined_sugary_drinks", "Unhealthy_Weight_Loss", "Physical_Activity_5Plus", "Fruit_Veg_5"))

# Split into 2 parts

lp1 = lolliPlot[1:72, ]
lp2 = lolliPlot[73:144, ]

# par(mgp=c(3,0,0))
# ggplot(lp1, aes(x=SCHOOL_NAM, y=value)) +
#     geom_segment(aes(x=SCHOOL_NAM, xend=SCHOOL_NAM, y=0, yend=value), color=cbPalette[1]) +
#     geom_point(shape=21, colour="black", fill=cbPalette[7], size=4) +
#     coord_flip() +
#     theme_bw() +
#     facet_wrap(~variable, scales="free", labeller=as_labeller(variable_names)) +
#     geom_hline(data = means[1:3, ], aes(yintercept=Percent), colour="black", linetype="longdash") +
#     geom_point(shape=21, colour="black", fill=cbPalette[7], size=4) +
#     theme(strip.text=element_text(size=16, face="bold"),
#           strip.background=element_rect(fill=cbPalette[5]),
#           axis.text.x=element_text(size=15),
#           axis.text.y=element_text(size=15)) +
#     xlab("") +
#     ylab("")
#
# ggplot(lp2, aes(x=SCHOOL_NAM, y=value)) +
#     geom_segment(aes(x=SCHOOL_NAM, xend=SCHOOL_NAM, y=0, yend=value), color=cbPalette[1]) +
#     geom_point(shape=21, colour="black", fill=cbPalette[7], size=4) +
#     coord_flip() +
#     theme_bw() +
#     facet_wrap(~variable, scales="free", labeller=as_labeller(variable_names)) +
#     geom_hline(data = means[4:6, ], aes(yintercept=Percent), colour="black", linetype="longdash") +
#     geom_point(shape=21, colour="black", fill=cbPalette[7], size=4) +
#     theme(strip.text=element_text(size=16, face="bold"),
#           strip.background=element_rect(fill=cbPalette[5]),
#           axis.text.x=element_text(size=15),
#           axis.text.y=element_text(size=15)) +
#     xlab("") +
#     ylab("")
par(mgp=c(3,0,0), mar=c(4.1,4.1,4.1,4.1))
p = ggplot(lolliPlot, aes(x=SCHOOL_NAM, y=value)) +
    geom_segment(aes(x=SCHOOL_NAM, xend=SCHOOL_NAM, y=0, yend=value), color=cbPalette[1]) +
    geom_point(shape=21, colour="black", fill=cbPalette[7], size=4.5) +
    coord_flip() +
    theme_bw() +
    facet_wrap(~variable, scales="free", labeller=as_labeller(variable_names)) +
    geom_hline(data = means, aes(yintercept=Percent), colour="black", linetype="longdash") +
    geom_point(shape=21, colour="black", fill=cbPalette[7], size=4.5) +
    theme(strip.text=element_text(size=18, face="bold"),
          strip.background=element_rect(fill=cbPalette[5]),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=17)) +
    xlab("") +
    ylab("")


pdf("./output/obesity variables Lollipop Plot.pdf",width=18,height=11)
p
dev.off()







