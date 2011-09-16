## Simon Task: data cleaning and coding
## Cari Bogulski and Jason Gullifer
## Last Update: 7.30.2010
 
## This program will clean up simon data (remove outliers below 200ms
## and above 1500ms and also 2.5SD above/below the mean). The absolute
## outlier cutoffs may be altered below, if needed.
## Calculates (for each subject):
##  * Simon Score
##  * Congruent, Incongruent, Central trial RTs
##  * Congruent, Incongruent, Central trial Accuracies
 
#################  You must set up your data sheet this way  ######################
#
#	How columns should be named
#	Subject ID = Subject
#	Reaction time column = Stimulus.RT
#	correct/incorrect/outlier column = Stimulus.ACC
#	Congruency column = Congruency
#	Block column = Block
###################################################################################

##################  You must modify these values  #################################
#	Load file = your input file name
#	save file = your output file name
#	absolute outlier cutoffs 
load_filename = 'simon_merged_100-103_106-108.txt' # filename to read data from
save_filename = 'Simon_merged_100-103_106-108_with_outliers.csv' #filename to save data to

absolute_outlier_low = 200 # cutoff for low absolute outliers
absolute_outlier_high = 1500 # cutoff for high absolute outliers 

Trial_1_Block_1 = 25
Trial_1_Block_2 = 67
Trial_1_Block_3 = 109

###################################################################################
#
#create a .csv file called "load_filename_cleaned.csv", which is the raw data with outliers identified
cleaned_filename = paste(strsplit(load_filename,".txt"),"_cleaned.csv",sep="") 

###	Read file and import data into a data frame
#	read in data file and mark practice trials, and filter out practice trials into "data"

data_unfiltered = read.table(load_filename, sep="\t", skip=1, header=TRUE)	#the "skip=1" portion of this line of code skips the first line in your data file
data = data_unfiltered[data_unfiltered$Running!='PracTrials',] 					#Filter out practice trials

#	refactorize Congruency to remove n/a's
#     NOTE: Any time you remove levels from your data set, it is a good idea to re-factor to get rid of the unused levels (in this case, NAs)
data$Congruency = factor(data$Congruency)

#factor subjects
data$Subject = factor(data$Subject)

#	In this script, "Accuracy" refers to E-prime's definition of "correct," coutning outliers as incorrect trials. "accuracy2" also identifies outliers, 
#	and counts them as incorrect, but it also excludes first trials and recovery trials in the RT analyses.
#
#	factorize Accuracy and accuracy2 and name levels
data$accuracy2=data$Stimulus.ACC
data$acc_for_acc=data$Stimulus.ACC

data$Stimulus.ACC<-factor(data$Stimulus.ACC,levels=0:3)	#0 - "incorrect", 1 - "correct", 2 - "absolute outliers,", 3 - "relative outliers"
data$accuracy2<-factor(data$accuracy2,levels=0:5)		#0 - "incorrect", 1 - "correct", 2 - "absolute outliers,", 3 - "relative outliers", 
											#4 - "first trial of a block", 5 - "recover trial"

levels(data$Stimulus.ACC)=c('incorrect','correct','ab_outlier','rel_outlier')					#Define the four levels in Stimulus.ACC
levels(data$accuracy2)=c('incorrect','correct','ab_outlier','rel_outlier','first_trial','recovery')	#Define the 6 levels in accuracy2

###	Filters
#	mark first trial of each Block
data$accuracy2[data$Block==Trial_1_Block_1]<-'first_trial'
data$accuracy2[data$Block==Trial_1_Block_2]<-'first_trial'
data$accuracy2[data$Block==Trial_1_Block_3]<-'first_trial'

#	mark recovery trials
data$recovery = 0
data$recovery[grep('incorrect',data$Stimulus.ACC)+1] = 5
data$accuracy2[data$accuracy2 == "correct" & data$recovery == 5] = "recovery"

#	find absolute outliers in Stimulus.ACC and accuracy
data$Stimulus.ACC[data$Stimulus.ACC =='correct' & (data$Stimulus.RT > absolute_outlier_high | data$Stimulus.RT < absolute_outlier_low)] = 'ab_outlier'
data$accuracy2[data$accuracy2 =='correct' & (data$Stimulus.RT > absolute_outlier_high | data$Stimulus.RT < absolute_outlier_low)] = 'ab_outlier'

	####	Find relative outliers using 1 mean

splitlist = split(data$Stimulus.RT[data$accuracy2=='correct'],list(data$Subject[data$accuracy2=='correct'],data$accuracy2[data$accuracy2=='correct']),drop = TRUE) #gives you a split list by subject (can add more here)
#splitlist = split(data$Stimulus.RT,list(data$Subject)) #gives you a split list by subject (can add more here)
cuthigh = lapply(splitlist,function(x){mean(x)+2.5*sd(x)}) #gives high cutoffs per subject
cutlow = lapply(splitlist,function(x){mean(x)-2.5*sd(x)}) #gives high cutoffs per subject
data$cuthigh = unsplit(cuthigh,list(data$Subject))
data$cutlow = unsplit(cutlow,list(data$Subject))

	####	Find relative outliers using 3 means (per each trial type)

splitlist = split(data$Stimulus.RT[data$accuracy2=='correct'],list(data$Subject[data$accuracy2=='correct'],data$accuracy2[data$accuracy2=='correct'],data$Congruency[data$accuracy2=='correct']),drop = TRUE) #gives you a split list by subject (can add more here)
#splitlist = split(data$Stimulus.RT,list(data$Subject)) #gives you a split list by subject (can add more here)
cuthigh = lapply(splitlist,function(x){mean(x)+2.5*sd(x)}) #gives high cutoffs per subject
cutlow = lapply(splitlist,function(x){mean(x)-2.5*sd(x)}) #gives high cutoffs per subject
data$cuthigh3means = unsplit(cuthigh,list(data$Subject,data$Congruency))
data$cutlow3means = unsplit(cutlow,list(data$Subject,data$Congruency))

####

data$Stimulus.ACC[data$Stimulus.ACC == "correct" & (data$Stimulus.RT < data$cutlow | data$Stimulus.RT > data$cuthigh)] = "rel_outlier"
data$accuracy2[data$accuracy2 == "correct" & (data$Stimulus.RT < data$cutlow | data$Stimulus.RT > data$cuthigh)] = "rel_outlier"
#data$accuracy2[data$Stimulus.ACC == "rel_outlier" & data$accuracy2 == "correct"] = "rel_outlier"

###	Calculate Subject Accuracies
ACC_bySubject_byCongruency = xtabs(~data$Stimulus.ACC+data$Congruency+data$Subject) #xtabs stands for cross-tab,
															#here we calculate accuracy by subject

#	Reformat Accuracy
final_data_acc = t(as.data.frame(ACC_bySubject_byCongruency[2,,]))  #get tha table only for correct scores and transpose

###	Calculate Subject Means
# Get means by Subject by Congruency with only correct responses
#final_data_Stimulus.RT = tapply(data$Stimulus.RT[data$accuracy2 == 'correct'],list(data$Subject[data$accuracy2 == 'correct'],data$Congruency[data$accuracy2 == 'correct']),mean, na.rm=TRUE)
final_data_Stimulus.RT = with(data[data$accuracy2 == 'correct',], tapply(Stimulus.RT, list(Subject,Congruency),mean,na.rm=TRUE))
final_data_Stimulus.RT = as.data.frame(final_data_Stimulus.RT)

###	Calculate Simon score
simon_score = as.data.frame(final_data_Stimulus.RT$incongruent - final_data_Stimulus.RT$congruent)

###     Calculate percentage of trials that are errors and percentage that are outliers

ACC_bySubject = xtabs(~data$Stimulus.ACC+data$Subject)	#get the accuracy by subject
ACC_bySubject2 = xtabs(~data$accuracy2+data$Subject)	#get the accuracy by subject

Total_Trials = xtabs(~data$Subject)	#get total number of trials for each subject, should be the same
prop_outliers = as.data.frame((ACC_bySubject[3,] + ACC_bySubject[4,]) / Total_Trials) #get the proportion of outliers
prop_errors = as.data.frame(ACC_bySubject[1,] / Total_Trials) #get the proportion of errors

prop_removed = as.data.frame((Total_Trials - ACC_bySubject2[2,])/Total_Trials)


###	Assemble final data matrix and change column names
final_simon_data = cbind(final_data_acc,final_data_Stimulus.RT,simon_score,prop_outliers[2], prop_errors[2],prop_removed[2])
colnames(final_simon_data) = c("Central_acc","Congruent_acc","Incongruent_acc","Central_RT","Congruent_RT","Incongruent_RT","Simon_Score","Proportion_of_Outliers","Proportion_of_Errors","Proportion_Removed_From_RT")

###	Write final data frame out to file specified under save_filename
write.csv(final_simon_data,save_filename)
write.csv(data,cleaned_filename)
