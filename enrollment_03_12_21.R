library(tidyverse)
library(lubridate)
library(glue)
library(scales)

outdir="/Users/heatherwelch/Dropbox/BWLW/whitman_enrollment/plots"
master=read.csv("/Users/heatherwelch/Dropbox/BWLW/whitman_enrollment/Master_enrollment2.csv")
# enroll=master %>% filter(dat.type=="Enrollment") %>% gather(gender, number,-c(Period, dat.type,What,No.data)) %>% 
#   filter(What!="Total"&What!="Percent"&What!="Enrollment+OCS") %>% mutate(Period=as.Date(Period)) %>% 
#   mutate(number=as.numeric(number))
# 
# ggplot(enroll)+geom_area(aes(x=Period,y=number,fill=interaction(What,gender)))
# ggplot(enroll)+geom_bar(aes(x=Period,y=number,fill=gender),stat = "identity")+
#   facet_wrap(~What)

## enrollment ####
enroll=master %>% filter(dat.type=="Enrollment") %>% gather(gender, number,-c(Period, dat.type,What,No.data)) %>% 
  filter(What!="Total"&What!="Percent"&What!="Enrollment+OCS") %>% mutate(Period=as.Date(Period)) %>% 
  mutate(number=as.numeric(number)) %>% group_by(Period,gender) %>% summarise(total=sum(number)) %>%
  mutate(month=month(Period),year=year(Period))%>% mutate(season=case_when(month==9~"fall",
                                                                           month==2~"spring",
                                                                           month==1~"spring")) %>% 
  mutate(gender_season=glue("{gender} {season}"))
                         
enroll_plot=ggplot(enroll)+
  geom_smooth(aes(x=Period,y=total,group=gender),se=F,color="grey",size=3,alpha=0.5)+
  geom_point(aes(x=Period,y=total,fill=gender_season),color="black",size = 4, shape = 21) +
  scale_fill_manual("",values = c("Men fall"="yellow","Men spring"="#EFC000FF", "Woman spring"="#003C67FF","Woman fall"="#0073C2FF")) +
  theme_classic()+scale_x_date(date_breaks="year",date_labels ="%Y")+
  xlab("Year")+ylab("Total enrollment")+
   ggtitle("Semester enrollment for Whitman College by gender")
  
enroll_plot

png(glue("{outdir}/total_enrollment.png"),width=15,height=12,units='cm',res=400)
par(ps=10)
par(cex=1)
enroll_plot
dev.off()

## ethnicity ####
# ethnicity=master %>% filter(dat.type=="Ethnicity") %>% gather(gender, number,-c(Period, dat.type,What,No.data)) %>% 
#   mutate(Period=as.Date(Period)) %>% 
#   mutate(number=as.numeric(number))  %>%
#   mutate(month=month(Period),year=year(Period))%>% mutate(season=case_when(month==9~"fall",
#                                                                            month==2~"spring",
#                                                                            month==1~"spring")) %>% 
#   mutate(cleaned_ethnicity=case_when(What=="Black/African-American"~"Black/African American",
#                                      What=="Black/African American"~"Black/African American",
#                                      What=="Hispanic"~"Hispanic/Latino",
#                                      What=="Asian/Pacific Islander"~"Asian/Pacific Islander",
#                                      What=="International"~"Unknown",
#                                      What=="Asian"~"Asian/Pacific Islander",
#                                      What=="Unknown"~"Unknown",
#                                      What=="Hispanic/Latino"~"Hispanic/Latino",
#                                      What=="Pacific Islander"~"Asian/Pacific Islander",
#                                      What=="American Indian/Alaska Native"~"American Indian/Alaskan Native",
#                                      What=="American Indian/Alaskan Native"~"American Indian/Alaskan Native",
#                                      What=="Two or More Races"~"Two or more races",
#                                      What=="Two or more races"~"Two or more races",
#                                      What=="Native American"~"American Indian/Alaskan Native",
#                                      What=="Native Hawaiian / Pacific Islander"~"Native Hawaiian/Pacific Islander",
#                                      What=="Native Hawaiian/Pacific Islander"~"Native Hawaiian/Pacific Islander",
#                                      What=="White"~"Caucasian",
#                                      What=="Caucasian"~"Caucasian"
#          )) %>% 
#   filter(season=="fall") %>% 
#   group_by(year,cleaned_ethnicity,gender) %>% summarise(total=sum(number)) %>% ungroup() %>% 
#   mutate(ethnicity_gender=glue("{cleaned_ethnicity} ({gender})"))
# 
# ethnicity_summary=ethnicity %>% group_by(ethnicity_gender) %>% summarise(mean=mean(total))
# ethnicity2=left_join(ethnicity,ethnicity_summary) %>% mutate(change=total-mean)
# 
# ggplot(ethnicity2)+
#    geom_line(aes(x=year,y=change,group=ethnicity_gender,color=ethnicity_gender))+
#   geom_point(aes(x=year,y=change,fill=ethnicity_gender),color="black",size = 4, shape = 21) +
#   # scale_fill_manual("",values = c("Men fall"="yellow","Men spring"="#EFC000FF", "Woman spring"="#003C67FF","Woman fall"="#0073C2FF")) +
#   theme_classic()+
#   # scale_x_date(date_breaks="year",date_labels ="%Y")+
#   xlab("Year")+ylab("Total enrollment")+
#   ggtitle("Semester enrollment for Whitman College")

ethnicity=master %>% filter(dat.type=="Ethnicity")%>% gather(gender, number,-c(Period, dat.type,What,No.data)) %>%  mutate(cleaned_ethnicity=case_when(What=="Black/African-American"~"Black/African American",
                                                                                           What=="Black/African American"~"Black/African American",
                                                                                           What=="Hispanic"~"Hispanic/Latino",
                                                                                           What=="Asian/Pacific Islander"~"Asian/Pacific Islander",
                                                                                           What=="International"~"Unknown",
                                                                                           What=="Asian"~"Asian/Pacific Islander",
                                                                                           What=="Unknown"~"Unknown",
                                                                                           What=="Hispanic/Latino"~"Hispanic/Latino",
                                                                                           What=="Pacific Islander"~"Asian/Pacific Islander",
                                                                                           What=="American Indian/Alaska Native"~"American Indian/Alaskan Native",
                                                                                           What=="American Indian/Alaskan Native"~"American Indian/Alaskan Native",
                                                                                           What=="Two or More Races"~"Two or more races",
                                                                                           What=="Two or more races"~"Two or more races",
                                                                                           What=="Native American"~"American Indian/Alaskan Native",
                                                                                           What=="Native Hawaiian / Pacific Islander"~"Native Hawaiian/Pacific Islander",
                                                                                           What=="Native Hawaiian/Pacific Islander"~"Native Hawaiian/Pacific Islander",
                                                                                           What=="White"~"Caucasian",
                                                                                           What=="Caucasian"~"Caucasian")) %>% 
  
  mutate(number=as.numeric(number))  %>%
  group_by(Period,cleaned_ethnicity) %>% summarise(number=sum(number)) %>% ungroup() %>% 
  mutate(Period=as.Date(Period)) %>% 
  mutate(month=month(Period),year=year(Period))%>% mutate(season=case_when(month==9~"fall",
                                                                           month==2~"spring",
                                                                           month==1~"spring")) %>% 
  mutate(covid=case_when(Period<as.Date("2020-02-06")~"Pre-Covid (Fall 2015 - Fall 2019)",
                         Period>as.Date("2019-09-17")&Period<as.Date("2021-02-12")~"Covid (Spring 2020 - Fall 2020)",
                         Period>as.Date("2020-09-11")~"Post-Covid (Spring 2021)")) %>% filter(cleaned_ethnicity!="Unknown"&cleaned_ethnicity!="Native Hawaiian/Pacific Islander"&cleaned_ethnicity!="Two or more races") %>% 
  # group_by(cleaned_ethnicity,covid) %>% summarise(mean=mean(number)) %>% 
  mutate(covid=factor(covid,levels=c("Pre-Covid (Fall 2015 - Fall 2019)","Covid (Spring 2020 - Fall 2020)","Post-Covid (Spring 2021)")))

enroll_ethnicity=ggplot(ethnicity,aes(x=Period,y=number,group=cleaned_ethnicity))+geom_line()+geom_point(aes(fill=covid),color="black",size = 4, shape = 21)+
  facet_wrap(~cleaned_ethnicity,scales = "free")+theme_classic()+
  scale_fill_manual("",values = c("#8a131f","#EFC000FF", "#003C67FF")) +
  theme(legend.position = c(0.85, 0.2))+xlab("Number enrolled")+ylab("Year")+
  ggtitle("Semester enrollment for Whitman College by ethnicity")

png(glue("{outdir}/enrollmentXethnicity.png"),width=20,height=12,units='cm',res=400)
par(ps=10)
par(cex=1)
enroll_ethnicity
dev.off()

