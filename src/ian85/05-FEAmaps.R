#
# This code reads, maps, and analyzes data from the USDA food environment atlas.
#

library(ggplot2)
library(maps)
library(dplyr)
library(glmnet)
library(randomForest)

# Set the directory, load the fea table names, and start a scheme to index the various files for later looping.

data.dir = "./data/comm_fairfax/working/usda_data/"
fea.tables = list.files(data.dir, pattern = "FEA")

table.index = 1


# Some preprocessing is needed to make sure the fea data merges nicely with the map data.

clean.maps.counties = function(){

    # Minor changes to get counties in working form
    counties <- map_data("county")
    counties$subregion = gsub('\\.','',counties$subregion)
    counties$subregion = gsub("'",'', counties$subregion)
    counties$subregion = gsub("\\s",'', counties$subregion)
    return(counties)
}
clean.state.fips = function(){
    data("state.fips")
    # Minor changes to get state.fips into usable form
    state.fips$polyname[20:22] = "massachusetts"
    state.fips$polyname[23:24] = "michigan"
    state.fips$polyname[34:37] = "new york"
    state.fips$polyname[38:40] = "north carolina"
    state.fips$polyname[53:55] = "virginia"
    state.fips$polyname[56:60] = "washington"
    return(state.fips)
}
read.clean.fea.data = function(data.dir, fea.tables, index){
    fea.data = read.csv(paste0(data.dir, fea.tables[index]), stringsAsFactors = F)

    # Minor changes to get county_obesity into usable form
    fea.data$County[which(fea.data$County == "Doña Ana")] = "dona ana"
    fea.data$County = tolower(fea.data$County)
    # Have to get rid of the . behind counties with the name like St. Mary's
    fea.data$County = gsub('\\.','',fea.data$County)
    fea.data$County = gsub("'",'', fea.data$County)
    fea.data$County = gsub("\\s",'', fea.data$County)
    return(fea.data)
}
merge.maps.and.data = function(fea.data){
    counties = clean.maps.counties()
    state.fips = clean.state.fips()

    # Join counties and state.fips
    # Need the state abbreviation from the stat.fips frame
    counties_st_ab <- left_join(counties, state.fips, by = c("region" = "polyname"))

    # Merge on two variables so counties with the same name correspond to the right state
    # Got line 54 from looking through ?map_data
    fea.map.data <- merge(counties_st_ab, fea.data, by.x = c("subregion", "abb"), by.y = c("County", "State"))
    fea.map.data <- fea.map.data[order(fea.map.data$order), ]
    return(fea.map.data)
}

# This will plot the map data with some predefined graphical parameters. As we develop the code, we'll probably end up adding more arguments to the function to allow us finer control over the output.

plot.usda = function(data, colname, state, plot = T, title){

    limits = range(data[,colname], na.rm = T)
    breaks = round(seq(min(data[,colname], na.rm = T), max(data[,colname], na.rm = T), length = 5), 1)
    if(missing(title)) title = colname
    if(!missing(state)) data = subset(data, data$abb == state)

    usda.map = ggplot() +
        geom_polygon(data=data, aes(x=long, y=lat, group=group, fill=data[,colname]), color="black", size=.25) +
        geom_polygon(data=data, aes(x=long, y=lat, group=group), fill=NA, color="gray48", size=.25) +
        scale_fill_gradient(low = "#E69F00", high = "#56B4E9", limits = limits, breaks = breaks) +
        coord_fixed(1.3) +
        labs(title = title, fill = colname) +
        theme(axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              text = element_text(size = 20))

    if(plot) {
        plot(usda.map)
    }else{
        return(usda.map)
    }
}

# Load the cleaned data files with our helper functions
# here, 3 indicates the health table

fea.data = read.clean.fea.data(data.dir, fea.tables, 3)
fea.map.data = merge.maps.and.data(fea.data)
plot.usda(fea.map.data, "PCT_OBESE_ADULTS08", "VA")

# Also load in the environmental atlas variables for interpretability of the column names
fea.vars = read.csv("./data/comm_fairfax/working/usda_data/env atlas variables.csv", stringsAsFactors = F)

# Helper function to return descriptions given a column name
find.var.description = function(varname){
    varnames = fea.vars[,5]
    n = length(varname)
    out = character(n)
    for(i in 1:n){
        row = which(varnames == varname[i])
        out[i] = fea.vars[row, 4]
    }

    return(cbind(varname, out))
}


fea.cor = cor(fea.data[,-c(1:3)], use = 'pair')
image(fea.cor)
colnames(fea.data)

# Look at everything at once

all.fea = read.clean.fea.data(data.dir, fea.tables, 1)

for(i in 2:length(fea.tables)){
    all.fea = cbind(all.fea, read.clean.fea.data(data.dir, fea.tables, i)[,-c(1:3)])
}

# Sort by correlation to PCT_OBESE_ADULTS13
fea.preds = all.fea[,-c(1:3)]
y = fea.preds[,colnames(fea.preds) %in% "PCT_OBESE_ADULTS13"]
# remove SNAP_REPORTSIMPLE16, which has sd = 0, obesity and diabetes, all % change variables
pct.change.vars = fea.vars$Variable.Code[which(fea.vars$Units == "% change")]
acres.vars = fea.vars$Variable.Code[which(fea.vars$Units == "acres")]
count.vars = fea.vars$Variable.Code[which(fea.vars$Units == "Count")]
cols.to.remove = c("PCT_OBESE_ADULTS13", "PCT_OBESE_ADULTS08", "SNAP_REPORTSIMPLE16", "PCT_DIABETES_ADULTS08", "PCT_DIABETES_ADULTS13", "SNAP_REPORTSIMPLE09", pct.change.vars, acres.vars, count.vars)

fea.preds = fea.preds[,!(colnames(fea.preds) %in% cols.to.remove)]

obese13cor = cor(y, fea.preds, use = 'pair')
fea.order = order(obese13cor)
fea.cor = cor(fea.preds, use = 'pair')[fea.order, fea.order]
image(fea.cor)

sorted.names = (colnames(obese13cor)[fea.order])
sortcor = cbind(sorted.names, obese13cor[fea.order])
vars = c(head(sorted.names, 10), tail(sorted.names, 10))
find.var.description(vars)
arrange(merge(find.var.description(vars), sortcor, by.x = "varname", by.y = "sorted.names"), V2)
# Investigate a model with these 12 predictors
X = as.matrix(fea.preds[,vars])


lm1 = lm(y ~ X)
summary(lm1)
hist(lm1$residuals)
plot(lm1$model$y, lm1$fitted.values)

# Try random forest
no.na.rows = !apply(cbind(y, X), 1, anyNA)
fea.rf = randomForest(X[no.na.rows,], y[no.na.rows])
plot(fea.rf)
fea.rf
varImpPlot(fea.rf)

# Amazing results, but hand selecting by correlation ahead of time seems against the spirit of Neyman-Pearson. Let's lasso it.
# I'm removing some columns due to missingness. I pick a threshold of 'missingness' and remove any columns which have more missingness than that. THe threshold is chosen so that the variables selected above are all retained, except for FMRKT_CREDIT16, which had 28% missing. Every other one is below 5%, which I take to be the threshold.

y = fea.preds[,90]
Xfull = fea.preds[,-90]
propNA = apply(Xfull, 2, function(x) sum(is.na(x)/length(x)))
thresh = .05
Xcleaned = Xfull[,which(propNA < thresh)]
no.na.rows = !apply(cbind(y, Xcleaned), 1, anyNA)
ynona = y[no.na.rows]
Xnona = as.matrix(Xcleaned[no.na.rows,])

lasso1 = cv.glmnet(Xnona,ynona)

# Do my variable survival thing from the SCHEV project

# Bootstrap a lasso, keeping track of which variables make it in.

prop.in.model = numeric(ncol(Xnona))
B = 2000
n = length(ynona)
Sys.time()
for(j in 1:B){
    bs = sample(1:n, n, T)
    fit.en = cv.glmnet(Xnona[bs,], ynona[bs], alpha = 1, nfolds = 5)
    beta.col = with(fit.en, which(lambda == lambda.1se)) # finds the row corresponding to the lambda 1se above the best cv
    in.model = (fit.en$glmnet.fit$beta[,beta.col] > 0)
    prop.in.model = prop.in.model + in.model
    if(j %% 100 == 0) {
        print(j)
        print(Sys.time())
    }
}

prop.in.model = prop.in.model / B

o = order(prop.in.model, decreasing = T)
names = colnames(Xnona)[o]
prop.in.model = prop.in.model[o]
names(prop.in.model) = names

nvars = 30
plot.dat = prop.in.model[1:nvars]
dotchart(rev(plot.dat), main = '', xlim = c(0, 1))
find.var.description(names(prop.in.model)[1:nvars])



