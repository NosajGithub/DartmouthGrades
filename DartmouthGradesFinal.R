library(plyr)
library(reshape)
library(googleVis)
library(rCharts)
library(XML)

######################
##   Definitions    ##
######################
## Grade to GradeValue Map
MedianGrade <- c("A", "A/A-", "A-", "A-/B+", "B+", "B+/B", "B", "B/B-", "B-", "B-/C+", "C+")
values <- rev(seq(14,24, by = 1))/6
gradeVals <- data.frame(MedianGrade,values)

## Department to Division Map
humanities <- c("AMEL","ARAB","ARTH","CHIN","CLST","DRAM","ENGL","FILM","FREN","FRIT","GERM","GRK","GRS",
                "HEBR","HUM","ITAL","JAPN","KORN","LAT","MUS","PHIL","PORT","REL","RUSS","SART","SPAN","THEA")
interdisc <- c("AAAS","AMES","COGS","COLT","ENVS","JWST","LACS","LATS","LING","M&SS","NAS","SPEE","WGST","WRIT")
socialSciences <- c("ANTH","ECON","EDUC","GEOG","GOVT","HIST","PSYC","SOCY","SSOC")
sciences <- c("ASTR","BIOL","CHEM","COSC","EARS","ENGG","ENGS","MATH","PHYS")        
nones <- c("COCO","ECS","GENE","HUBI","INTS","MALS","MICR","PBPL","QBS","SCI","TUCK","WPS")

Department <- c(humanities, interdisc, socialSciences, sciences, nones)
Division <- c(
        rep("Humanities", length(humanities)),
        rep("Interdisciplinary", length(interdisc)),
        rep("Social Sciences", length(socialSciences)),
        rep("Sciences", length(sciences)),
        rep("Other", length(nones))
)
divisionsMap <- data.frame(Department,Division)

######################
## Process CSV Data ##
######################
## Start with data from before
medians <- read.csv("median00s10s.csv")

## Turn one-digit years into calendar years 
medians$Year <- as.numeric(paste0('2',formatC(medians$Year, width = 3, format = "d", flag = "0")))

## Take out 2000 because only three terms of data exist, and 2010 because that data is coming later
medians <- medians[((medians$Year != '2000') & (medians$Year != '2010')),]

## Remove duplicates
medians2 <- medians[!(duplicated(medians[,-grep("Section",names(medians))])
                      & grepl("ECON|MATH",medians$Department)),] 

############################
## Process Dartmouth Data ##
############################

## Download from Dartmouth website (Dartmouth website is missing 09W, 08W, 00W)
ends <- apply(merge(formatC(10:13, width = 2, format = "d", flag = "0"), c("F","W","S","X")),
              1,paste,collapse="")
# urls <- paste0("http://www.dartmouth.edu/~reg/transcript/medians/",ends,".html")
# tables <- sapply(urls,readHTMLTable)
# 
# ## Combine downloaded tables
# mediansRaw <- data.frame(Term=character(),
#                          Course=character(),
#                          Enrollment=numeric(),
#                          MedianGrade=character(), stringsAsFactors = FALSE)
# 
# for(i in 1:length(tables)){        
#         mediansRaw <- rbind(mediansRaw,setNames(tables[[i]],names(mediansRaw)))
# }

# write.csv(mediansRaw,file="./mediansRaw.csv",row.names = FALSE)
mediansRaw <- read.csv("./mediansRaw.csv")

## Clean Data
mRaw1 <- mediansRaw[mediansRaw$Term %in% ends,] 
mRaw1$Course <- as.character(mRaw1$Course)                      
mRaw1$MedianGrade <- gsub(" ","",mRaw1$MedianGrade)             
mRaw1 <- mRaw1[mRaw1$MedianGrade %in% MedianGrade,]             

#Add hyphens to entries without them for parsing in next step 
for(i in 1:length(mRaw1$Course)){
        if(substr(mRaw1$Course[i],5,5) != "-"){
                mRaw1$Course[i] <- paste0(substr(mRaw1$Course[i],1,4),"-",
                                          substr(mRaw1$Course[i],5,7),"-",
                                          substr(mRaw1$Course[i],8,9))                        
        }        
}

## Some departments will curve across multiple sections and then report the enrollment of each section as 
## the enrollment of all sections, overstating enrollment - this code eliminates these duplicate sections
mRaw1$attr <- strsplit(mRaw1$Course,"-")
mRaw1$Department <- sapply(mRaw1$attr,function(x) unlist(x)[1])
mRaw1$Course <- sapply(mRaw1$attr,function(x) unlist(x)[2])
mRaw1$Section <- sapply(mRaw1$attr,function(x) unlist(x)[3])

mRaw1 <- mRaw1[!(duplicated(mRaw1[,-grep("attr|Section",names(mRaw1))])
                 & grepl("ECON|MATH|GOVT|TUCK",mRaw1$Department)),] 

## Split out year and term
mRaw1$Year <- as.numeric(paste0('2',formatC(as.numeric(substr(mRaw1$Term,1,2)), width = 3, format = "d", flag = "0")))
mRaw1$Term <- substr(mRaw1$Term,3,3)

#Keep select columns for merge
mRaw1 <- mRaw1[,grep("Term|Course|Year|Section|Enrollment|MedianGrade|Department",names(mRaw1))] 

############################
##    Combine data sets   ##
############################
final <- rbind(medians2,mRaw1)

## Format resuting columns
final$Enrollment <- as.numeric(final$Enrollment)
final$Course <- as.numeric(final$Course)
final$Section <- as.numeric(final$Section)
final$Department[final$Department == "WST" ] <- "WGST"          ## Convert "WST" to "WGST" (Code used in the early years)
final$Department <- gsub(" ","",final$Department)

############################
##    Rename Departments  ##
############################
final$DepartmentLong <- as.character(sapply(as.character(final$Department),switch,
                                            "AAAS" = "AAAS: African and African-American Studies",
                                            "AMEL" = "AMEL: Asian and Middle Eastern Languages and Literatures",
                                            "AMES" = "AMES: Asian and Middle Eastern Studies",
                                            "ANTH" = "ANTH: Anthropology",
                                            "ARAB" = "ARAB: Arabic",
                                            "ARTH" = "ARTH: Art History",
                                            "ASTR" = "ASTR: Astronomy",
                                            "BIOL" = "BIOL: Biology",
                                            "CHEM" = "CHEM: Chemistry",
                                            "CHIN" = "CHIN: Chinese",
                                            "CLST" = "CLST: Classics",
                                            "COCO" = "COCO: College Course",
                                            "COGS" = "COGS: Cognitive Sciences",
                                            "COLT" = "COLT: Comparative Literature",
                                            "COSC" = "COSC: Computer Science",
                                            "DRAM" = "DRAM: Drama (now Theater)",
                                            "EARS" = "EARS: Earth Sciences",
                                            "ECON" = "ECON: Economics",
                                            "ECS" = "ECS: Evaluative Clinical Sciences",
                                            "EDUC" = "EDUC: Education",
                                            "ENGG" = "ENGG: Engineering (no major credit)",
                                            "ENGL" = "ENGL: English",
                                            "ENGS" = "ENGS: Engineering",
                                            "ENVS" = "ENVS: Environmental Studies",
                                            "FILM" = "FILM: Film and Median Studies",
                                            "FREN" = "FREN: French",
                                            "FRIT" = "FRIT: French and Italian",
                                            "GENE" = "GENE: Genetics",
                                            "GEOG" = "GEOG: Geography",
                                            "GERM" = "GERM: German",
                                            "GOVT" = "GOVT: Government",
                                            "GRK" = "GRK: Greek",
                                            "GRS" = "GRS: Greek Studies",
                                            "HEBR" = "HEBR: Hebrew",
                                            "HIST" = "HIST: History",
                                            "HUBI" = "HUBI: Human Biology",
                                            "HUM" = "HUM: Humanities",
                                            "INTS" = "INTS: International Studies",
                                            "ITAL" = "ITAL: Italian",
                                            "JAPN" = "JAPN: Japanese",
                                            "JWST" = "JWST: Jewish Studies",
                                            "KORN" = "KORN: Korean",
                                            "LACS" = "LACS: Latin American, Latino, and Caribbean Studies",
                                            "LAT" = "LAT: Latin",
                                            "LATS" = "LATS: Latino Studies",
                                            "LING" = "LING: Linguistics",
                                            "M&SS" = "M&SS: Math and Social Sciences",
                                            "MALS" = "MALS: Master of Arts in Liberal Sciences",
                                            "MATH" = "MATH: Mathematics",
                                            "MICR" = "MICR: Microbiology and Immunology",
                                            "MUS" = "MUS: Music",
                                            "NAS" = "NAS: Native American Studies",
                                            "PBPL" = "PBPL: Public Policy",
                                            "PHIL" = "PHIL: Philosophy",
                                            "PHYS" = "PHYS: Physics",
                                            "PORT" = "PORT: Portuguese",
                                            "PSYC" = "PSYC: Psychology",
                                            "QBS" = "QBS: Quantitative Biomedical Sciences",
                                            "REL" = "REL: Religion",
                                            "RUSS" = "RUSS: Russian",
                                            "SART" = "SART: Studio Art",
                                            "SCI" = "SCI: Sciences",
                                            "SOCY" = "SOCY: Sociology",
                                            "SPAN" = "SPAN: Spanish",
                                            "SPEE" = "SPEE: Speech",
                                            "SSOC" = "SSOC: Social Sciences Department",
                                            "THEA" = "THEA: Theater",
                                            "TUCK" = "TUCK: Tuck Undergraduate",
                                            "WGST" = "WGST: Womens and Gender Studies",
                                            "WPS" = "WPS: War and Peace Studies",
                                            "WRIT" = "WRIT: Writing"))

###################################
##    Compute aggregate values   ##
###################################
## Add in grade values
final2 <-merge(final,gradeVals)
# finalArranged <- arrange(final2, Year, Term, Department, Course, Section)
# write.csv(finalArranged,file="./FinalGradeData.7.9.14.csv")

## Take mean of medians
final2$mult <- with(final2,values*Enrollment)
depYearAvg <- ddply(final2,.(Department, DepartmentLong, Year),summarise,enrollment=sum(Enrollment),grade=sum(mult)/sum(Enrollment))

## Add in divisions
depYearAvgDiv <- merge(depYearAvg,divisionsMap)
depYearAvgDiv$mult <- with(depYearAvgDiv,grade*enrollment)

## Find college average
all <- ddply(depYearAvgDiv,.(Year),summarise,enrollment=sum(enrollment),grade=sum(mult)/sum(enrollment))
all$DepartmentLong <- "Whole College"
all$divs <- "Other"
all$grade <- round(all$grade,3)

## Find division summaries
divisionSums <- ddply(depYearAvgDiv,.(Division,Year),summarise,enrollment=sum(enrollment),grade=sum(mult)/sum(enrollment))
divisionSums$DepartmentLong <- sapply(as.character(divisionSums$Division),switch,
                                      "Humanities" = "All Humanities", 
                                      "Sciences" = "All Sciences", 
                                      "Interdisciplinary"= "All Interdisciplinary", 
                                      "Social Sciences"= "All Social Sciences", 
                                      "Other"= "All Other")
divSummaries <- rbind(divisionSums)#,all) #Toggle to include 'whole college' or not
deptSummaries <- depYearAvgDiv[,-grep("mult",names(depYearAvgDiv))]

## Take out departments with less than 1000 enrolled students over timeframe (13 years)
enrTemp <- ddply(deptSummaries,.(DepartmentLong),mutate,allSum=sum(enrollment))
deptSummaries2 <- enrTemp[enrTemp$allSum > 1000,-grep("allSum",names(enrTemp))]

deptSummaries2$grade <- round(deptSummaries2$grade,3)
divSummaries$grade <- round(divSummaries$grade,3)

divSummariesNoOther <- divSummaries[divSummaries$Division != "Other",]
divSummariesNoOther <- rbind(divSummariesNoOther[1:26,],divSummariesNoOther[40:52,],divSummariesNoOther[27:39,])

#######################
##    Create Charts  ##
#######################

## Chart 1
allPlot <- nPlot(
        grade ~ Year, 
        data = all, 
        group = "DepartmentLong",
        type = "lineChart")

allPlot$set(width = 550, height = 600)
allPlot$xAxis(axisLabel = "Year")
allPlot$yAxis(axisLabel = "Average Grade", width = 55)
allPlot$chart(forceY = c(3.10,3.80))

allPlot$save('1.html', cdn = TRUE)

## Chart 2

divPlot <- nPlot(
        grade ~ Year, 
        data = divSummariesNoOther, 
        group = "Division",
        type = "lineChart")

divPlot$set(width = 550, height = 600)
divPlot$xAxis(axisLabel = "Year")
divPlot$yAxis(axisLabel = "Average Grade", width = 55, tickFormat = "#!function(d) {return d3.format('0.2f')(d);}!#")
divPlot$chart(forceY = c(3.10,3.80))
divPlot$chart(color=c('blue','deepskyblue','orange','gold'))

divPlot$save('2.html', cdn = TRUE)

## Chart 3
myStateSettings3 <- '\n{"xZoomedDataMax":1356998400000,"orderedByX":false,"uniColorForNonSelected":false,"sizeOption":"_UNISIZE","yLambda":1,"yZoomedDataMax":4,"xAxisOption":"_TIME","yAxisOption":"3","iconKeySettings":[{"key":{"dim0":"ANTH: Anthropology"},"trailStart":"2001","LabelY":39,"LabelX":55}],"xZoomedDataMin":978307200000,"orderedByY":false,"playDuration":2000,"colorOption":"4","dimensions":{"iconDimensions":["dim0"]},"nonSelectedAlpha":0.1,"xZoomedIn":false,"iconType":"BUBBLE","time":"2013","xLambda":1,"yZoomedIn":false,"duration":{"multiplier":1,"timeUnit":"Y"},"showTrails":true,"yZoomedDataMin":3}\n'
chart3 <- gvisMotionChart(deptSummaries2,idvar="DepartmentLong",
                          timevar="Year",xvar="Year",yvar="grade",date.format="%Y",colorvar="Division",
                          options=list(state=myStateSettings3,height=600,width=550))
plot(chart3)
print(chart3, "chart", file = "./3.txt")

## Chart 4
myStateSettings4 <- '\n{"uniColorForNonSelected":false,"xZoomedIn":false,"yZoomedDataMin":0,"iconKeySettings":[],"yZoomedIn":false,"yZoomedDataMax":5,"yAxisOption":"3","xZoomedDataMin":0,"nonSelectedAlpha":0.1,"time":"2013","sizeOption":"_UNISIZE","xLambda":1,"duration":{"timeUnit":"Y","multiplier":1},"xZoomedDataMax":50,"xAxisOption":"3","orderedByX":true,"dimensions":{"iconDimensions":["dim0"]},"showTrails":false,"colorOption":"4","playDuration":8333.333333333328,"yLambda":1,"iconType":"VBAR","orderedByY":false}\n'
chart4 <- gvisMotionChart(deptSummaries2,idvar="DepartmentLong",
                          timevar="Year",xvar="grade",yvar="grade",date.format="%Y",colorvar="Division",
                          options=list(state=myStateSettings4,height=600,width=550))
plot(chart4)
print(chart4, "chart", file = "./4.txt")

## Chart 5

divPlotE <- nPlot(
        enrollment ~ Year, 
        data = divSummariesNoOther, 
        group = "Division",
        type = "lineChart")

divPlotE$set(width = 550, height = 600)
divPlotE$xAxis(axisLabel = "Year")
divPlotE$yAxis(axisLabel = "Enrollment", width = 64, tickFormat = "#!function(d) {return d3.format('0.0f')(d);}!#")
divPlotE$chart(forceY = c(0,15000))
divPlotE$chart(color=c('blue','deepskyblue','orange','gold'))

divPlotE$save('5.html', cdn = TRUE)

## Chart 6
myStateSettings6 <- '\n{"uniColorForNonSelected":false,"orderedByX":false,"orderedByY":false,"time":"2013","playDuration":2000,"nonSelectedAlpha":0.1,"xZoomedIn":false,"xZoomedDataMin":978307200000,"colorOption":"4","yZoomedIn":false,"yZoomedDataMin":46,"xZoomedDataMax":1356998400000,"sizeOption":"_UNISIZE","iconKeySettings":[{"key":{"dim0":"ENGL: English"},"trailStart":"2001"}],"duration":{"multiplier":1,"timeUnit":"Y"},"yZoomedDataMax":2958,"iconType":"BUBBLE","xAxisOption":"_TIME","xLambda":1,"yAxisOption":"2","yLambda":1,"dimensions":{"iconDimensions":["dim0"]},"showTrails":true}\n'
chart6 <- gvisMotionChart(deptSummaries2,idvar="DepartmentLong",
                          timevar="Year",xvar="enrollment",yvar="enrollment",date.format="%Y",colorvar="Division",
                          options=list(state=myStateSettings6,height=600,width=550))
plot(chart6)
print(chart6, "chart", file = "./6.txt")

## Chart 7

myStateSettings7 <- '\n{"uniColorForNonSelected":false,"orderedByX":true,"orderedByY":false,"time":"2013","playDuration":2000,"nonSelectedAlpha":0.1,"xZoomedIn":false,"xZoomedDataMin":0,"colorOption":"4","yZoomedIn":false,"yZoomedDataMin":0,"xZoomedDataMax":50,"sizeOption":"_UNISIZE","iconKeySettings":[],"duration":{"multiplier":1,"timeUnit":"Y"},"yZoomedDataMax":3000,"iconType":"VBAR","xAxisOption":"2","xLambda":1,"yAxisOption":"2","yLambda":1,"dimensions":{"iconDimensions":["dim0"]},"showTrails":false}\n'
chart7 <- gvisMotionChart(deptSummaries2,idvar="DepartmentLong",
                          timevar="Year",xvar="enrollment",yvar="enrollment",date.format="%Y",colorvar="Division",
                          options=list(state=myStateSettings7,height=600,width=550))
plot(chart7)
print(chart7, "chart", file = "./7.txt")

## Chart 8

myStateSettings8 <- '\n{"uniColorForNonSelected":false,"orderedByX":false,"orderedByY":false,"time":"2013","playDuration":6855.555555555555,"nonSelectedAlpha":0.1,"xZoomedIn":false,"xZoomedDataMin":46,"colorOption":"4","yZoomedIn":false,"yZoomedDataMin":3,"xZoomedDataMax":2958,"sizeOption":"_UNISIZE","iconKeySettings":[],"duration":{"multiplier":1,"timeUnit":"Y"},"yZoomedDataMax":4,"iconType":"BUBBLE","xAxisOption":"2","xLambda":1,"yAxisOption":"3","yLambda":1,"dimensions":{"iconDimensions":["dim0"]},"showTrails":true}\n'
chart8 <- gvisMotionChart(deptSummaries2,idvar="DepartmentLong",
                          timevar="Year",xvar="enrollment",yvar="grade",date.format="%Y",colorvar="Division",
                          options=list(state=myStateSettings8,height=600,width=550))
plot(chart8)
print(chart8, "chart", file = "./8.txt")

#######################
##    Create Image   ##
#######################

## Separate GPAs for introductory and nonmajor classes vs major classes
all2013 <- subset(final2, Year == 2013)
all2013$major <- "nonmajor"
all2013$keep <- 0   ## Only keep departments that have been majorized

majorize <- function(inputDepartment, inputCourse){
        all2013[all2013$Department == inputDepartment & all2013$Course > inputCourse,"major"] <<- "major"        
        all2013[all2013$Department == inputDepartment,"keep"] <<- 1
}

## thecount <- count(all2013, "Department")
## subset(thecount, freq > 10) ## List of departments to include

majorize("AAAS",11)
majorize("AMES",19)
majorize("ANTH",9)
majorize("ARAB",23)
majorize("ARTH",7)
majorize("BIOL",11)
majorize("CHEM",10)
majorize("CHIN",23)
majorize("CLST",7)
majorize("COLT",10)
majorize("COSC",19)
majorize("EARS",9)
majorize("ECON",10)
majorize("ENGL",9)
majorize("ENGS",21)
majorize("ENVS",2)
majorize("FILM",7)
majorize("FREN",8)
majorize("GEOG",3)
majorize("GERM",13)
majorize("GOVT",19)
majorize("HIST",9)
majorize("ITAL",7)
majorize("LACS",1)
majorize("LING",7)
majorize("MATH",24)
majorize("MUS",23)
majorize("NAS",8)
majorize("PHIL",7)
majorize("PHYS",24)
majorize("PSYC",10)
majorize("REL",19)
majorize("SART",16)
majorize("SOCY",2)
majorize("SPAN",9)
majorize("THEA",7)
majorize("WGST",10)

all2013_post <- subset(all2013, keep ==1)

all2013_post$mult <- with(all2013_post,values*Enrollment)
all2013_depMajAvg <- ddply(all2013_post,.(Department, DepartmentLong, major),summarise,enrollment=sum(Enrollment),grade=sum(mult)/sum(Enrollment))
all_test <- ddply(all2013_post,.(major),summarise,enrollment=sum(Enrollment),grade=sum(mult)/sum(Enrollment))

library(reshape2)
all2013_split <- dcast(all2013_depMajAvg, Department + DepartmentLong ~ major, value.var="grade")
all2013_split$diff <- with(all2013_split, major - nonmajor)
all2013_split <- all2013_split[order(-all2013_split$major),]
all2013_split$row <- 1:nrow(all2013_split)

all2013_sorted <- merge(all2013_depMajAvg, all2013_split[,c("Department","row")])
all2013_sorted <- merge(all2013_sorted, divisionsMap)
all2013_sorted <- arrange(all2013_sorted, row, major)

library(ggplot2)
png("majors_ordered.png", width = 1800, height = 800)
ggplot(data=all2013_sorted, aes(x=row, y=grade)) +
        geom_line(aes(group = row), color = "grey") + theme_bw() +
        geom_text(aes(label=round(grade,2), color = major),hjust=.5, vjust=0) + 
        geom_text(aes(label=c(rbind(unique(all2013_sorted$Department),rep("",37)))),hjust=.5, vjust=-1.5) + 
        ggtitle(expression(atop(bold("Grades by Department, 2013"), atop(italic("Major vs Non-major"), "")))) +
        labs(x = "Department", y = "Grade") +
        scale_x_continuous(labels = unique(all2013_sorted$Department), breaks = unique(all2013_sorted$row)) +
        scale_color_discrete(name = "Course Type")
dev.off()

