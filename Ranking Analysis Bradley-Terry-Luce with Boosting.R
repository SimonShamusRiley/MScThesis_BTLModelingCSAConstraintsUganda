##############################################################################################
# Bradley-Terry-Luce model takes individual ranking data and provides an overall ranking     #
# in the form of log-odds estimates that any one item 'wins' (or is chosen) over another.    #
# The package used in this script, ordBTL, also makes it easy to also consider ordinal       #
# covariates, such as the age class or farm size, of each respondent making a ranking, and   # 
# to thus identify systematic differences in the way items are ranked among sub-populations. #
#                                                                                            #
# Here, 'practices' are the different things ABOUT which we are ranking, 'constraints'       #
# are the actual things being ranked for each practice, and 'covariates' are demographic     #
# or socio-economic variables. Thus 'practices' could for example be crops, with the         #
# 'constraints' being things people like most about each crop ('very filling', 'good market',#
# 'high yielding', etc), and covariates could be aspects of the farm (location, size, etc.)  #
# or the farmer ('age', 'sex','education', etc.). Some of the functions have a 'status'      #
# setting, with status being a covariate associated with each separate practice. If your     #
# has no such variable, simply set status = F in the appropriate functions. If your data has #
# more than one such practice-linked covariate, the code will need to be amended accordingly.#
#                                                                                            #
# This script was created by Simon Riley in 2017 for IITA Uganda. Thanks should be given     #
# to Leroy (and probably others) over at CIAT for the inspiration I drew from his earlier,   #
# far more sophisticated ranking work in R.                                                  #
##############################################################################################

#### To Do ####

# Adjust BTL.PLOT to supress warning messages about the replaced y axis

#### Load Libraries ####

library(ordBTL)
library(ggplot2)
library(ggrepel)
library(svglite)

setwd() # Set working directory

#### Define Research Parameters ####

# Enumerate practice names as they appear in the data set     
practice.vec = c('weeding','herbicide','mulch','pruning','shade_planting','suckering','bctb','cwd','manure',
                 'gap_filling','trenching','de_silting','basins','fert','shade_management')

# Enumerate practice names as they appear in the data set     
practice_names.vec = c('Mechanical Weeding','Herbicide Use','Mulching','Pruning','Shade Tree Planting','De-Suckering','BCTB Control','CWD Control','Manure Application',
                 'Gap Filling','Trenching','De-Silting Trenches','Basins','Fertilizer Use','Shade Tree Mgmt.')

# Enumerate constraints as they appear in the data set
constraint.vec = c('cost','labor','access','skill','time','other')

# Enumerate covariates as they appear in the data set
covariate.vec = c('sex','status','age','primacy_coffee','coffee_count','subcounty')

covariate_names.vec = c('Sex',"Use History","Age","Primacy of Coffee","Extent of Coffee","Sub-County")


#### Define Functions ####

# Transform ordered ranking into numerical rankings; address non-ranked constraints
DM.TRANSFORM = function(df, covariate.vec, practice.vec, status = TRUE){ 
  
  # Create new dataframe containing key for cross-reference with covariates,
  # as well as one column for each practice x constraint combination
  
  columns = c("ID", covariate.vec)
  
  df.t = data.frame(df[,which(colnames(df) %in% columns)])
  
  if (status == TRUE){
    for (p in 1:length(practice.vec)){
      df.t[,paste0(practice.vec[p],"_status")] = df[,paste0(practice.vec[p],"_status")]
      
    }
  }
  
  for (p in 1:length(practice.vec)){
    for (c in 1:length(constraint.vec)){
      df.t[,paste0(practice.vec[p],'_',constraint.vec[c],'_rank')] = NA
    }
  }
  
  # Transform rank ordering into numerical ranking
  for (i in 1:nrow(df)){
    for (p in 1:length(practice.vec)){
      for (c in 1:length(constraint.vec)){
        for (n in 1:7){
          ifelse(df[i,paste0(practice.vec[p],'_constraint_',n)] == constraint.vec[c], df.t[i, paste0(practice.vec[p],'_',constraint.vec[c],'_rank')] <- n, next)
        }
      }
    }
  }
  
  # Assign all constraints not ranked for each practice as 7 (e.g. tied for last)
  # Identify value of lowest rank
  minrank = length(grep(pattern = paste0(practice.vec[1],"_constraint"),x=colnames(df)))
  
  for (p in 1:length(practice.vec)){
    start = min(grep(pattern = paste0(practice.vec[p],"_constraint"),x=colnames(df)))
    for (i in 1:nrow(df)){
      if (is.na(df[i,start]) | df[i,start] == 'none') {next}
      
      listed.constraints = as.matrix(df[i,c(start:(start+minrank-1))])
      
      listed.constraints = listed.constraints[-which(is.na(listed.constraints))]
      rank.last = constraint.vec[-which(constraint.vec %in% listed.constraints)]
      if (is.null(rank.last) | length(rank.last) == 0) {next}
      else {
        for (l in 1:length(rank.last)){
          df.t[i,paste0(practice.vec[p],'_',rank.last[l],'_rank')] = minrank
        }
      }
    }
  }
  return(df.t)
} 

# For each responsdent, enumerate all possible constraint combinations and determine winner in each combination
DM.LONG.STRUCT = function(df, covariate.vec, practice, constraint.vec){
  
  # Redefine covariate list for specific practice in question
  covariates = gsub(pattern = 'status', replacement = paste0(practice,"_status"), x = covariate.vec)
  
  # Enumerate combinations of constraints 
  comb=matrix(ncol=2,nrow=sum((length(constraint.vec)-1):1))
  count = 1
  for (c1 in 1:(length(constraint.vec)-1)){
    for (c2 in (c1+1):length(constraint.vec)){
      comb[count,] = c(c1,c2)
      count = count+1
    }
  }
  
  # Create data frame of HH ID's
  long = data.frame(ID = rep(1:length(df$ID), sum((length(constraint.vec)-1):1)))
  
  # Add covariate data corresponding to HH ID 
  long[,covariates] = df[match(long$ID,df$ID),covariates]
  
  # Sort the data frame by HH ID and correct row names
  long = long[sort(long$ID),];  rownames(long) = 1:nrow(long)
  
  # Add all constraint combinations for each HH
  long$player1 = rep(constraint.vec[comb[,1]])
  long$player2 = rep(constraint.vec[comb[,2]])
  
  # Convert constraint names from text to factors
  long$player1 = as.factor(long$player1)
  long$player2 = as.factor(long$player2)
  
  # Correct factors to agree 
  levels(long$player1) = c(levels(long$player1), constraint.vec[-which(constraint.vec %in% levels(long$player1))])
  levels(long$player2) = c(levels(long$player2), constraint.vec[-which(constraint.vec %in% levels(long$player2))])
  
  # Add empty columns for 'winning' constraints
  long$win1 = NA
  long$win2 = NA
  
  # Determine which ranks higher in all pairs of constraints for all HH's
  for (i in 1:nrow(long)){
    long$win1[i] = ifelse(df[match(long$ID[i], df$ID), paste0(practice,"_",long$player1[i],"_rank")] < df[match(long$ID[i], df$ID), paste0(practice,"_",long$player2[i],"_rank")],1,
                            ifelse(df[match(long$ID[i], df$ID), paste0(practice,"_",long$player1[i],"_rank")] > df[match(long$ID[i], df$ID), paste0(practice,"_",long$player2[i],"_rank")],0,
                                   ifelse(df[match(long$ID[i], df$ID), paste0(practice,"_",long$player1[i],"_rank")] == df[match(long$ID[i], df$ID), paste0(practice,"_",long$player2[i],"_rank")],0.5, NA)))
    long$win2[i] = ifelse(is.na(long$win1[i]),NA,abs(1-long$win1[i]))
  }
  
  colnames(long) = c("ID",covariate.vec,'player1','player2','win1','win2')
  
  return(long)
} 

# Prepare dataframes for plotting outputs based on significance of covariates
BTL.PLOT.PREP = function(ranks,significant, constraint.vec){
  
if (length(significant) == 0){
  
  prepplot = data.frame("Factor" = constraint.vec)
  
  base.estimates = ranks[match(prepplot$Factor,rownames(ranks)), 1]
  
  prepplot[,"All Farmers"] = base.estimates + intercept

  } else {
    
  prepplot = data.frame("Factor" = constraint.vec)
  
  base.estimates = ranks[match(prepplot$Factor,rownames(ranks)), 1]
  
  for (s in 1:length(significant)) {
    
      sig.factor.count = length(levels(design[,significant[s]]))
    
      sig.factor.levels = levels(design[,significant[s]])
    
    for (l in 1:sig.factor.count){
        
      prepplot[,sig.factor.levels[l]] = base.estimates + intercept
      
        for (c in 1:length(constraint.vec)){
          
              interaction = grep(pattern = paste0(constraint.vec[c],':',significant[s],sig.factor.levels[l]), x = rownames(ranks))
              
              skip.interaction = ifelse(length(interaction) == 0, TRUE, FALSE)
          
              if (skip.interaction == TRUE) {next} else {
                
                relevant.constraint = which(prepplot$Factor == constraint.vec[c])
                
                prepplot[relevant.constraint, sig.factor.levels[l]] = prepplot[relevant.constraint, sig.factor.levels[l]] + ranks[interaction, 1]
            }
        }

    }
  }  
} 
  
  return(prepplot)
}

# Plot graphs
BTL.PLOT = function(plot.df, significant, significant_names, design, practice = practice_names.vec[p]) {
  
  p = ggplot(plot.df)
  
  if (length(significant) == 0){
    
 plot = p + geom_point(aes(x="All Farmers", y=plot.df[,'All Farmers']))+
      scale_y_continuous(limits=c(NA,NA))+
      geom_text_repel(aes(x="All Farmers", y=plot.df[,"All Farmers"],label=round(plot.df[,"All Farmers"], digits=2)), 
                      direction = "y",
                      nudge_x = rep(-0.2,5),
                      segment.colour = NA,
                      force = 0.01, 
                      cex=3)+
      geom_text_repel(aes(x="All Farmers", y=plot.df[,"All Farmers"],label=plot.df[,"Factor"]), 
                      direction = "y",
                      nudge_x = 0.25*(nchar(as.character(plot.df[1:5,'Factor']))/max(nchar(as.character(plot.df[1:5,'Factor'])))), #rep(-0.07,5),
                      segment.colour = NA,
                      force = 0.01, 
                      cex=3)+
   ylab(expression(paste(lambda, " (Log Odds)", sep = " ")))+
   xlab(paste0("Constraints to ",practice)) + theme(axis.title.x=element_text(size=8)) 
 
 suppressWarnings(print(plot))
 ggsave(paste0(practice," - All Farmers.png"), plot = plot, scale = 1, device = "png", width = 3, height = 3, units = "in")
    
  } else {
    
  for (s in 1:length(significant))  {
      
      FactorCount = length(levels(design[,significant[s]]))
      
      if (FactorCount < 2 | FactorCount > 4) {
        stop("Error: Significant interaction factors must have 2 to 4 levels")
        
      } else if (FactorCount == 2) {
        
        xfactor1 = levels(design[,significant[s]])[1]
        xfactor2 = levels(design[,significant[s]])[2]
        yfactor1 = plot.df[,levels(design[,significant[s]])[1]]
        yfactor2 = plot.df[,levels(design[,significant[s]])[2]]
        
        plot = p + geom_point(aes(x=xfactor1, y=yfactor1))+
          scale_y_continuous(limits=c(NA,NA))+
          geom_text_repel(aes(x=xfactor1, y=yfactor1,label=round(yfactor1,digits=2)), 
                          direction = "y",
                          nudge_x = rep(-0.2,5),
                          segment.colour = NA,
                          force = 0.01, 
                          cex=3)+
          geom_text_repel(aes(x=xfactor1, y=yfactor1,label=plot.df[,'Factor']), 
                          direction = "y",
                          nudge_x = 0.25*(nchar(as.character(plot.df[1:5,'Factor']))/max(nchar(as.character(plot.df[1:5,'Factor'])))), #rep(-0.07,5),
                          segment.colour = NA,
                          force = 0.01, 
                          cex=3)+
          
          geom_point(aes(x=xfactor2, y=yfactor2))+
          scale_y_continuous(limits=c(NA,NA))+
          geom_text_repel(aes(x=xfactor2, y=yfactor2,label=round(yfactor2,digits=2)), 
                          direction = "y",
                          nudge_x = rep(-0.2,5),
                          force = 0.01, segment.colour = NA,
                          cex=3)+
          geom_text_repel(aes(x=xfactor2, y=yfactor2,label=plot.df[,'Factor']), 
                          direction = "y",
                          force = 0.01, segment.colour = NA,
                          nudge_x = 0.25*(nchar(as.character(plot.df[1:5,'Factor']))/max(nchar(as.character(plot.df[1:5,'Factor'])))), #rep(-0.07,5),
                          cex=3)+
          
          ylab(expression(paste(lambda, " (Log Odds)", sep = " ")))+
          xlab(paste0("Constraints to ",practice,"\n (by ",significant_names[s],")"))+
          theme(axis.title.x=element_text(size=8))
        
        suppressWarnings(print(plot))
        ggsave(paste0(practice," - ",significant_names[s],".png"), plot = plot, scale = 1, device = "png", width = 3, height = 3, units = "in")
        
      } else if (FactorCount == 3){
        
        xfactor1 = levels(design[,significant[s]])[1]
        xfactor2 = levels(design[,significant[s]])[2]
        xfactor3 = levels(design[,significant[s]])[3]
        yfactor1 = plot.df[,levels(design[,significant[s]])[1]]
        yfactor2 = plot.df[,levels(design[,significant[s]])[2]]
        yfactor3 = plot.df[,levels(design[,significant[s]])[3]]
        
        plot = p + geom_point(aes(x=xfactor1, y=yfactor1))+
          scale_y_continuous(limits=c(NA,NA))+
          geom_text_repel(aes(x=xfactor1, y=yfactor1,label=round(yfactor1,digits=2)), 
                          direction = "y",
                          nudge_x = rep(-0.2,5),
                          segment.colour = NA,
                          force = 0.01, 
                          cex=2)+
          geom_text_repel(aes(x=xfactor1, y=yfactor1,label=plot.df[,'Factor']), 
                          direction = "y",
                          nudge_x = 0.25*(nchar(as.character(plot.df[1:5,'Factor']))/max(nchar(as.character(plot.df[1:5,'Factor'])))), #rep(-0.07,5),
                          segment.colour = NA,
                          force = 0.01, 
                          cex=2)+
          
          geom_point(aes(x=xfactor2, y=yfactor2))+
          scale_y_continuous(limits=c(NA,NA))+
          geom_text_repel(aes(x=xfactor2, y=yfactor2,label=round(yfactor2,digits=2)), 
                          direction = "y",
                          nudge_x = rep(-0.2,5),
                          force = 0.01, segment.colour = NA,
                          cex=2)+
          geom_text_repel(aes(x=xfactor2, y=yfactor2,label=plot.df[,'Factor']), 
                          direction = "y",
                          force = 0.01, segment.colour = NA,
                          nudge_x = 0.25*(nchar(as.character(plot.df[1:5,'Factor']))/max(nchar(as.character(plot.df[1:5,'Factor'])))), #rep(-0.07,5),
                          cex=2)+
          
          geom_point(aes(x=xfactor3, y=yfactor3))+
          scale_y_continuous(limits=c(NA,NA))+
          geom_text_repel(aes(x=xfactor3, y=yfactor3,label=round(yfactor3,digits=2)), 
                          direction = "y",
                          nudge_x = rep(-0.2,5),
                          force = 0.01, segment.colour = NA,
                          cex=2)+
          geom_text_repel(aes(x=xfactor3, y=yfactor3,label=plot.df[,'Factor']), 
                          direction = "y",
                          force = 0.01, segment.colour = NA,
                          nudge_x = 0.25*(nchar(as.character(plot.df[1:5,'Factor']))/max(nchar(as.character(plot.df[1:5,'Factor'])))), #rep(-0.07,5),
                          cex=2)+
          
          ylab(expression(paste(lambda, " (Log Odds)", sep = " ")))+
          xlab(paste0("Constraints to ",practice,"\n (by ",significant_names[s],")"))+
          theme(axis.title.x=element_text(size=8))
        
        suppressWarnings(print(plot))
        ggsave(paste0(practice," - ",significant_names[s],".png"), plot = plot, scale = 1, device = "png", width = 3, height = 3, units = "in")
        
      } else if(FactorCount == 4) {
        
        xfactor1 = levels(design[,significant[s]])[1]
        xfactor2 = levels(design[,significant[s]])[2]
        xfactor3 = levels(design[,significant[s]])[3]
        xfactor4 = levels(design[,significant[s]])[4]
        yfactor1 = plot.df[,levels(design[,significant[s]])[1]]
        yfactor2 = plot.df[,levels(design[,significant[s]])[2]]
        yfactor3 = plot.df[,levels(design[,significant[s]])[3]]
        yfactor4 = plot.df[,levels(design[,significant[s]])[3]]
        
        plot = p + geom_point(aes(x=xfactor1, y=yfactor1))+
          scale_y_continuous(limits=c(NA,NA))+
          geom_text_repel(aes(x=xfactor1, y=yfactor1,label=round(yfactor1,digits=2)), 
                          direction = "y",
                          nudge_x = rep(-0.2,5),
                          segment.colour = NA,
                          force = 0.01, 
                          cex=2)+
          geom_text_repel(aes(x=xfactor1, y=yfactor1,label=plot.df[,'Factor']), 
                          direction = "y",
                          nudge_x = 0.25*(nchar(as.character(plot.df[1:5,'Factor']))/max(nchar(as.character(plot.df[1:5,'Factor'])))), #rep(-0.07,5),
                          segment.colour = NA,
                          force = 0.01, 
                          cex=2)+
          
          geom_point(aes(x=xfactor2, y=yfactor2))+
          scale_y_continuous(limits=c(NA,NA))+
          geom_text_repel(aes(x=xfactor2, y=yfactor2,label=round(yfactor2,digits=2)), 
                          direction = "y",
                          nudge_x = rep(-0.2,5),
                          force = 0.01, segment.colour = NA,
                          cex=2)+
          geom_text_repel(aes(x=xfactor2, y=yfactor2,label=plot.df[,'Factor']), 
                          direction = "y",
                          force = 0.01, segment.colour = NA,
                          nudge_x = 0.25*(nchar(as.character(plot.df[1:5,'Factor']))/max(nchar(as.character(plot.df[1:5,'Factor'])))), #rep(-0.07,5),
                          cex=2)+
          
          geom_point(aes(x=xfactor3, y=yfactor3))+
          scale_y_continuous(limits=c(NA,NA))+
          geom_text_repel(aes(x=xfactor3, y=yfactor3,label=round(yfactor3,digits=2)), 
                          direction = "y",
                          nudge_x = rep(-0.2,5),
                          force = 0.01, segment.colour = NA,
                          cex=2)+
          geom_text_repel(aes(x=xfactor3, y=yfactor3,label=plot.df[,'Factor']), 
                          direction = "y",
                          force = 0.01, segment.colour = NA,
                          nudge_x = 0.25*(nchar(as.character(plot.df[1:5,'Factor']))/max(nchar(as.character(plot.df[1:5,'Factor'])))), #rep(-0.07,5),
                          cex=2)+
          
          geom_point(aes(x=xfactor4, y=yfactor4))+
          scale_y_continuous(limits=c(NA,NA))+
          geom_text_repel(aes(x=xfactor4, y=yfactor4,label=round(yfactor4,digits=2)), 
                          direction = "y",
                          nudge_x = rep(-0.2,5),
                          force = 0.01, segment.colour = NA,
                          cex=2)+
          geom_text_repel(aes(x=xfactor4, y=yfactor4,label=plot.df[,'Factor']), 
                          direction = "y",
                          force = 0.01, segment.colour = NA,
                          nudge_x = 0.25*(nchar(as.character(plot.df[1:5,'Factor']))/max(nchar(as.character(plot.df[1:5,'Factor'])))), #rep(-0.07,5),
                          cex=2)+
          
          ylab(expression(paste(lambda, " (Log Odds)", sep = " ")))+
          xlab(paste0("Constraints to ",practice,"\n (by ",significant_names[s],")"))+
          theme(axis.title.x=element_text(size=8))
        
        suppressWarnings(print(plot))
        ggsave(paste0(practice," - ",significant_names[s],".png"), plot = plot, scale = 1, device = "png", width = 3, height = 3, units = "in")
        
      }
      
    }
  }
}

#### Load, Process and Transform Data ####

# NOTE: The data sets can be formatted in two ways: 
# either columns refer to a practice and constraint combination, with rank numbers as values
# (e.g. column name is: 'weeding_cost_rank', with values: '1', '2', etc.) in which case no need to transform:
df.t = read.csv(file = , header = T, sep = ",", row.names = 1, stringsAsFactors = T) # read in your csv and then skip to the next section

# otherwise, columns refer to constraint ranks for each practice, with constraints themselves as values
# (e.g. column name is: 'weeding_constraint_1, with values: 'cost', 'time', etc.) in which case proceed from here:
df.original = read.csv(file = "CSIP_Constraint_ranking_Results_For_Distribution.csv", header = T, sep = ",", stringsAsFactors = T)

# NOTE: the following transformations may not be relevent to your specific data set, but
# bear in mind that you WILL need to convert continuous data to ordinal or categorical for this
# analysis

# set blanks to NA 
df.cleaned = df.original
df.cleaned[df.cleaned == ''] = NA

# Standardize extent of coffee holdings (assume standard spacing of 3m x 3m)
for (i in 1:nrow(df.cleaned)){
  ifelse(df.cleaned$coffee_count_unit[i] == 'acres', df.cleaned$coffee_count[i] <- df.cleaned$coffee_count[i] * 450, next)
}
df.cleaned$coffee_count_unit = 'trees'

# Convert tree count to categorical variable (less than or equal to 1 acre/450 trees, greater than 1 acre/450 trees)
df.cleaned$coffee_count[which(df.cleaned$coffee_count <= 450)] = 0
df.cleaned$coffee_count[which(df.cleaned$coffee_count > 450)] = 1
df.cleaned$coffee_count=as.factor(df.cleaned$coffee_count)
levels(df.cleaned$coffee_count)=list("< 1 acre"= 0,"> 1 acre"=1)

# Convert age to categorical (less than or equal to median (45 years), above median (45))
df.cleaned$age[which(df.cleaned$age <= median(df.cleaned$age, na.rm = T))] = 0
df.cleaned$age[which(df.cleaned$age > median(df.cleaned$age, na.rm = T))] = 1
df.cleaned$age=as.factor(df.cleaned$age)
levels(df.cleaned$age) = list("< 45"=0,"> 45"=1)

# Clarify levels for other covariates
levels(df.cleaned$sex) = list("Male" = 'm',"Female" = "f")
levels(df.cleaned$primacy_coffee) = list("Yes" = 'y',"No" = "n")
levels(df.cleaned$subcounty) = list("Lwanda"="LWANDA", "Kagamba"="KAGAMBA")

for (p in 1:length(practice.vec)){
  levels(df.cleaned[,paste0(practice.vec[p],"_status")]) = list("Currently" = "current","Never" = 'never','Previously'='previous')
}

# Transform data for modelling (one column for each practice + constraint combination, with value its ranking)
df.t = DM.TRANSFORM(df = df.cleaned, covariate.vec = covariate.vec, practice.vec = practice.vec, status = T)

#### Define Formulas ####

# Define reference category (i.e. a constraint), and create abbreviated constraint list
reference = 'other'
constraint.abrev = constraint.vec[-which(constraint.vec == reference)]

form.min = paste0(constraint.abrev, collapse = "+")

form.full = paste0("(",form.min,") + (",form.min,"):(", paste0(covariate.vec,collapse = "+"),")")

form.min = paste0("Y ~", form.min)
form.min = formula(form.min)

form.full = paste0("Y~",form.full)
form.full = formula(form.full)

#### Remove Temporary Data Sets ####
rm(df.cleaned, df.original)

#### BTL Analysis ####

BT = list() # Create a wrapper for your data

# Indicate the practice to model from your constraint list 
for (p in 1:length(practice.vec)){
  print(practice.vec[p])

# Transform ranking data to long-form
  counts = DM.LONG.STRUCT(df.t, covariate.vec=covariate.vec, practice = practice.vec[p], constraint.vec = constraint.vec)

# Add Response variable "Y" (required for ordBTL)
  counts$Y = NA
  counts$Y[which(counts$win1 == 1)] = 1
  counts$Y[which(counts$win1 == 0.5)] = 2
  counts$Y[which(counts$win1 == 0)] = 3
  counts$Y = as.factor(counts$Y)
  counts$Y = as.ordered(counts$Y)

# Sub-set and reformat long-form data for modeling in ordBTL
  design = design(counts[-is.na(counts$Y), ], var1="player1",var2="player2", use.vars=c("Y", covariate.vec), reference = "other")

# Remove prefixes
  colnames(design)=gsub(x=colnames(design),pattern="GAMMA.", replacement="")

# Boosting
  blanks = unique (unlist (lapply (design, function (x) which (is.na (x)))))
  design = design[-blanks,]
  boost = BTLboost(formula = form.full, 
                   data = design, 
                   #groupVars = covariate.vec, 
                   selection = "AIC", 
                   objects = constraint.abrev,
                   mstop = 30,
                   nu = 1,
                   verbose = T )
  
# Create parsimonious Formula from boosting results
  sig.interactions = names(boost$BEST)
  sig.interactions = sig.interactions[-which(sig.interactions == "(Intercept)")]
  
  length.cov = length(covariate.vec)
  
  for (c in 1:length.cov){
    
    length.factor = length(levels(design[,covariate.vec[c]]))
    
    for (l in 1:length.factor){
      
      factor.level = levels(design[,covariate.vec[c]])
      sig.interactions = gsub(pattern = paste0(covariate.vec[c],factor.level[l]), replacement = covariate.vec[c], x = sig.interactions)
    }
  }
  
  sig.interactions = unique(sig.interactions)
  
 
  parsimonious = paste0(sig.interactions, collapse = "+")
  #parsimonious = paste0("(",parsimonious,") + (",parsimonious,"):(", sig.interactions[6:length(sig.interactions)],")")
  parsimonious = paste0("Y ~ ",parsimonious)
  parsimonious = as.formula(parsimonious)

# Run ordinal BTL model
  model = ordBTL(formula = parsimonious, data = design, family="acat", family.control=list(reverse=TRUE))

# Extract coefficients, etc, from model
  ranks=as.data.frame(getRank(model))
  
# Identify significant covariates for plotting
  significant = c()
  significant_names = c()
  for (v in 1:length(covariate.vec)){
    sig.interaction.count = length(grep(covariate.vec[v],sig.interactions))
    if (sig.interaction.count > 0) {
      significant = c(significant, covariate.vec[v])
      significant_names = c(significant_names, covariate_names.vec[v])
    }
  }
  
# Add the full model and ranking data to your BT wrapper
  BT[[paste0(practice.vec[p],"_ranks")]] = as.data.frame(ranks) 
  BT[[paste0(practice.vec[p],"_model")]] = model
  
  
  
#### Plot the Results ####

# Identify the intercept value
  intercept = ranks["(Intercept)","Estimate"]

# Create a simplified table of relevent results
  plot.df=BTL.PLOT.PREP(ranks = ranks, significant = significant, constraint.vec = constraint.vec)

# Plot them in ggplot (note that the x and y values will need to be adjusted to reflect 
# different covariates of concern, but the template below should otherwise serve as is)

BTL.PLOT(plot.df = plot.df,significant = significant, significant_names = significant_names, 
         design = design, practice = practice_names.vec[p])

rm(counts, design, blanks, boost,plot.df,ranks, factor.level, parsimonious, sig.interactions, sig.interaction.count,significant, significant_names)
}

# Save the model (plots are alrady saved)
saveRDS(BT, file = "BTL_Model")


#### Miscellaneous Functions ####

# Extract frequency of rankings and practice statuses: 
statuses = data.frame()
for (p in 1:length(practice.vec)){
  statuses=rbind(statuses, table(df.cleaned[,paste0(practice.vec[p],"_status")], exclude = NULL))
}
rownames(statuses)=practice.vec
colnames(statuses)=c("","current","never","previous")

# Alternative formula generation
if (exists("form.min")) {rm(form.min)}
if (exists("form.full")) {rm(form.full)}

# Define minimum formula with only constraints as factors

form.min = paste0("Y~", paste0(constraint.abrev,collapse = "+"))

# Define maximum formula with all constraints, covariates, and all possible interactions as factors
form.full = form.min

form.full = paste0(form.full,"+", paste0(covariate.vec, collapse = "+"))

for (c in 1:(length(covariate.vec))){
  form.full = paste0(form.full, "+", paste0(covariate.vec[c],":",constraint.abrev, collapse = "+"))
}

# Convert text to formula
form.min = formula(form.min)
form.full = formula(form.full)