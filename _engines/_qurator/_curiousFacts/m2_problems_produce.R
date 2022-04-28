
problems <- read.csv('possibleM2Problems.csv', 
                     header = T,
                     check.names = F,
                     stringsAsFactors = F)
problems <- problems[problems$Selected, ]
problems <- problems[complete.cases(problems), ]
problems <- problems[, c('Property_.value')]
problems <- data.frame(num = 1:length(problems), 
                       targetProperty = problems, 
                       stringsAsFactors = F)
problems$targetProperty <- gsub("http://www.wikidata.org/entity/", 
                                "", problems$targetProperty)
write.csv(problems, 
          "m2_problems.csv")
