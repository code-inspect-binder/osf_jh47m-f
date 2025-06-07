################## GATEKEEPERS ANALYSIS ####################

# Required packages
library(stringr)
library(plyr)
library(aod)     
library(ggplot2) 
library(censReg) #tobit

# you'll need to change these paths
politics <- read.csv( "bias rating results-Analysis.csv" )
publications <- read.csv( "gatekeeper_all_abstracts.csv" )


# select out non_native speakers who learned English after 10, and people who 
# say they didn't read carefully
politics$target.f<-factor(politics$target, labels=c("con","lib"))
politics$english_age<-as.numeric(politics$english_age)
politics<-subset( politics,
                  subset=( (native_english == "Yes" | english_age<10) & 
                             (read_carefully == "Yes") 
                  )
)


# copy and recode politics variable
politics$rater.pol<-politics$political_classification
politics$rater.pol[politics$rater.pol>7] <- NA


# get rid of some -1 values, which mean "missing"
politics$eval[politics$eval == -1] <- NA
politics$explain[politics$explain == -1] <- NA


# create new id variables needed to create average ratings for each abstract on each of the four variables: abstract X liberal
politics$AbstractID_ideology = paste0(politics$AbstractID, politics$target)


# Create averge ratings: Explanatory difference
ex_long = aggregate(explain ~ AbstractID_ideology, data = politics, mean, na.action = na.omit)
ex_long$target <- str_sub(ex_long$AbstractID_ideology,-1,-1)
ex_long$AbstractID = as.numeric(substr(ex_long$AbstractID_ideology, 1, nchar(ex_long$AbstractID_ideology)-1))
ex_long$AbstractID_ideology = NULL

ex_wide = reshape(ex_long, idvar = "AbstractID", timevar = "target", direction = "wide") # legend: .1 is liberal; .0 is conservative


# Create average ratings: Evaluative difference
ev_long = aggregate(eval ~ AbstractID_ideology, data = politics, mean, na.action = na.omit)
ev_long$target <- str_sub(ev_long$AbstractID_ideology,-1,-1)
ev_long$AbstractID = as.numeric(substr(ev_long$AbstractID_ideology, 1, nchar(ev_long$AbstractID_ideology)-1))
ev_long$AbstractID_ideology = NULL

ev_wide = reshape(ev_long, idvar = "AbstractID", timevar = "target", direction = "wide") # legend: .1 is liberal; .0 is conservative

remove(ev_long, ex_long)


# Gen differences in evaluative and explanatory ratings (by subtracting "liberal" ratings from "conservative" ratings)
average_ratings_wide <- merge(ex_wide, ev_wide,by="AbstractID")


# "conservatives" are labelled with 0 and "liberals" with 1
average_ratings_wide$diff_ev = average_ratings_wide$eval.0 - average_ratings_wide$eval.1
average_ratings_wide$diff_ex = average_ratings_wide$explain.0 - average_ratings_wide$explain.1


# Merge impact factor data
FullDataset <- merge(publications,average_ratings_wide,by="AbstractID")

# Remove abstracts without publication data
FullDataset <- subset(FullDataset, !is.na(published) )

# Drop useless datasets
remove(ev_wide, ex_wide, politics, publications)


# Analysis - Linear Probability Model on Published Dummy

FullDataset$dummy_pub = 1
FullDataset$dummy_pub[FullDataset$published == "No"] = 0

fit_ev <- lm(dummy_pub ~ diff_ev + factor(year), data=FullDataset)
summary(fit_ev) # show results

fit_ex <- lm(dummy_pub ~ diff_ex + factor(year), data=FullDataset)
summary(fit_ex) # show results

# Analysis - Linear Probability Model on Impact Factor

fit_ev_if <- lm(X1.year.Impact.Factor ~ diff_ev + factor(year), data=FullDataset)
summary(fit_ev_if) # show results

fit_ex_if <- lm(X1.year.Impact.Factor ~ diff_ex + factor(year), data=FullDataset)
summary(fit_ex_if) # show results

# Tobit Model as robusntess check
# assign value 0 to Impact factors for unpublished studies
FullDataset$X1.year.Impact.Factor[FullDataset$published == "No"] = 0

tobit_ev_if <- censReg( X1.year.Impact.Factor ~ diff_ev + factor(year), data = FullDataset, left = 0)
summary(tobit_ev_if)

tobit_ex_if <- censReg( X1.year.Impact.Factor ~ diff_ex + factor(year), data = FullDataset, left = 0)
summary(tobit_ex_if)

# Reviewer request: dichotomize explanatory/evaluative bias
### Reviewer request: code evaluative/explanatory differences categorically. Uncomment to run these analyses
# one "explain" difference and one "evaluative" difference are exactly 0

FullDataset$diff_ev_cat <- ifelse(FullDataset$diff_ev<0, 0, 1)
FullDataset$diff_ex_cat <- ifelse(FullDataset$diff_ex<0, 0, 1)

FullDataset$diff_ev_cat[FullDataset$diff_ev=="0"]<-NA
FullDataset$diff_ex_cat[FullDataset$diff_ex=="0"]<-NA

table(FullDataset$diff_ev_cat, exclude = NULL)
table(FullDataset$diff_ex_cat, exclude = NULL)


fit_ev_cat <- lm(dummy_pub ~ diff_ev_cat + factor(year), data=FullDataset)
summary(fit_ev_cat) # show results

fit_ex_cat <- lm(dummy_pub ~ diff_ex_cat + factor(year), data=FullDataset)
summary(fit_ex_cat) # show results


tobit_ev_if_cat <- censReg( X1.year.Impact.Factor ~ diff_ev_cat + factor(year), data = FullDataset, left = 0)
summary(tobit_ev_if_cat)

tobit_ex_if_cat <- censReg( X1.year.Impact.Factor ~ diff_ex_cat + factor(year), data = FullDataset, left = 0)
summary(tobit_ex_if_cat)

### End reviewer request