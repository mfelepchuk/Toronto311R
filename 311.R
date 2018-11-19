library(tidyverse)
library(ggplot2)
library(purrr)

#Download CSVs from Toronto

years=2010:2018
data=as.tibble()

for (year in years) {
  temp <- tempfile()
  file <- paste("https://www.toronto.ca/ext/open_data/catalog/data_set_files/SR",year,".zip",sep="")
  print(paste("downloading file: ", file,sep=""))
  download.file(file,temp) 
  temp_data <- read_csv(unz(temp,paste("SR",year,".csv",sep="")))
  data <- data %>% bind_rows(temp_data)
}


#Data Wrangling
names(data) <- c('Date', 'Status', 'FSA', 'Intersection_1', 'Intersection_2', 'Ward', 'Service', 'Division','Section')

#Was having an issue with Mutate so I moved the Ward Cleaning out of mutate and used Purrr
cleanWard <- function(name) {
  location = str_locate(name,"\\(")[1]
  if (is.na(location)) {
    return(name)
  } else {
    return(str_sub(name,1,str_locate(name,"\\(")[1]-2))
  }
}

map_ward = map_chr(data$Ward, cleanWard)


#Cleaning and factorization of variables
data <- data %>% mutate(Ward = map_ward)%>% 
                mutate(Year = format(Date, "%Y"), Status=as.factor(Status), FSA = as.factor(FSA), Ward = as.factor(Ward), 
                        Service = as.factor(Service), Division = as.factor(Division), Section=as.factor(Section), 
                        Complete = (Status=="Closed"|Status=="Cancelled"))



#DPLYR summaries
by_year_ward <- data %>% group_by(Ward,Year) %>% summarise(calls = n()) %>% arrange(desc(calls))

by_ward <- data %>% group_by(Ward, Division) %>% summarise(calls = n()) %>% arrange(desc(calls)) %>% 
  filter(Ward != "Unknown", Division !="Unknown" )

by_FSA <- data %>% filter(FSA != "Intersection") %>% group_by(FSA, Ward, Division, Service) %>% summarise(calls = n()) %>% arrange(desc(calls))

by_service <- data %>% group_by(Division, Service) %>% summarise(calls = n()) %>% arrange(desc(calls))

by_sd <- data %>% group_by(Service, Division) %>% summarise(calls=n(), complete=sum(Complete), complete_pct = complete/calls) %>%
  arrange(complete_pct) %>% filter(calls>100)

incomplete <- data %>% filter(Complete==FALSE) %>% group_by(Year, Service, Division, Ward) %>% 
  summarise(incomplete = n()) %>% arrange(desc(incomplete)) %>% filter(incomplete>=10)

wildlife <- data %>% filter(Service=="CADAVER WILDLIFE") %>% group_by(Ward) %>% summarise(calls = n()) %>% arrange(desc(calls))

wildlife_complete <- data %>% group_by(Service, Year) %>% summarise(calls=n(), complete=sum(Complete), complete_pct = complete/calls) %>%
  arrange(complete_pct)  %>% filter(Service %in% 'CADAVER WILDLIFE')

#Plots
ggplot(by_year_ward[by_year_ward$Ward=='Beaches-East York (31)',], aes(y=calls,x=Year)) + geom_bar(stat='identity')

ggplot(wildlife, aes(y=calls,x=reorder(Ward,-calls))) + geom_bar(stat='identity', aes(fill=Ward)) + 
  ggtitle("Number of Wildlife Cadaver Calls, by Ward") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + xlab('Ward') + theme(legend.position="none")
  
ggplot(by_ward, aes(y=calls,x=reorder(Ward,-calls))) + geom_bar(stat='identity', aes(fill=Division)) + 
  ggtitle("Calls by Division and Ward") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + xlab('Ward') + theme(legend.position="bottom")
