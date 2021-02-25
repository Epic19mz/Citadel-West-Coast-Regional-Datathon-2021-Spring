library(data.table)
library(countrycode)
library(RcppCNPy)
setwd("~/Documents/WC-Datathon2021Spring")

#ecdc_notification=read.csv("2_ecdc/notification.csv")
# intervention=read.csv("2_ecdc/country_response_measures.csv")

#data1=ecdc_notification[ecdc_notification$continent=='Europe' & ecdc_notification$indicator=='deaths',c('country','year_week','rate_14_day')]
#countries_name=intersect(data1$country,intervention$Country)

# countries_intervention=c('StayHomeOrder')
# countries_name_with_intervention=intersect(countries_name,intervention[intervention$Response_measure %in% countries_intervention,c('Country')])
# data2=intervention[(intervention$Response_measure %in% countries_intervention) & (intervention$Country %in% countries_name_with_intervention),]
# data2$date_start=as.character(data2$date_start)
# for(i in 1:dim(data2)[1])
# {
#   data2$date_start[i]=week(data2$date_start[i])
# }
# data2=data2[as.numeric(data2$date_start)<=20,]
# for(i in 1:dim(data2)[1])
# {
#   country_name=data2$Country[i]
#   death_rate=data1[as.character(data1$country)==country_name,c("year_week","rate_14_day")]
#   week_of_death_rate=c()
#   for(j in 1:dim(death_rate)[1])
#   {
#     week_of_death_rate[j]=as.numeric(strsplit(as.character(death_rate[j,1]),'2020-')[[1]][2])
#   }
#   death_rate=death_rate[,2]
#   time_of_intervention=data2$date_start[data2$Country==country_name]
#   plot(week_of_death_rate,death_rate,type="l",xlab = "week")
#   abline(v=time_of_intervention,col=2)
# }

# max_week=c()
# for(i in 1:dim(data2)[1])
# {
#   country_name=data2$Country[i]
#   death_rate=data1[as.character(data1$country)==country_name,c("year_week","rate_14_day")]
#   week_of_death_rate=c()
#   for(j in 1:dim(death_rate)[1])
#   {
#     week_of_death_rate[j]=as.numeric(strsplit(as.character(death_rate[j,1]),'2020-')[[1]][2])
#   }
#   death_rate=death_rate[,2]
#   max_week[i]=week_of_death_rate[death_rate==max(death_rate[week_of_death_rate<=30],na.rm=TRUE) & !is.na(death_rate==max(death_rate[week_of_death_rate<=30],na.rm=TRUE))]
# }
# plot(as.numeric(data2$date_start),max_week,xlab="week of intervention")
# max_week-as.numeric(data2$date_start)

##############################################################################
owid_covid_data=read.csv("1_owid/owid-covid-data.csv")
europe=owid_covid_data[owid_covid_data$continent=='Europe',c("iso_code","date","new_cases_smoothed")]
europe$date=as.character(europe$date)
europe$iso_code=as.character(europe$iso_code)
for(i in 1:dim(europe)[1])
{
  europe$date[i]<-as.Date(europe$date[i],format=c("%m/%d/%y"))-as.Date("2020-01-01")+1
}
countries_iso_code<-unique(europe$iso_code)

intervention=read.csv("2_ecdc/country_response_measures.csv")
intervention$Country=as.character(intervention$Country)
intervention$Response_measure=as.character(intervention$Response_measure)
intervention$date_start=as.character(intervention$date_start)
intervention$date_end=as.character(intervention$date_end)

#countrycode("intervention$Country","country.name","iso3c")
temp=c()
for(i in 1:length(intervention$Country))
{
  temp[i]=countrycode(intervention$Country[i],"country.name","iso3c")
}
intervention$Country=temp
temp=c()
for(i in 1:length(intervention$date_start))
{
  temp[i]=as.Date(intervention$date_start[i])-as.Date("2020-01-01")+1
}
intervention$date_start=temp
temp=c()
for(i in 1:length(intervention$date_end))
{
  temp[i]=as.Date(intervention$date_end[i])-as.Date("2020-01-01")+1
}
intervention$date_end=temp
intervention[1:4,]

c_codes1=intersect(unique(countries_iso_code),unique(intervention$Country))

#intervention_considered=c("StayHomeOrder","SocialCircle","MasksMandatoryAllSpaces")
intervention_considered=c("StayHomeOrder")
c_codes2<-c()
for(i in 1:length(c_codes1))
{
  temp_code=c_codes1[i]
  #select dates between 57 and 175
  #s_date_temp=57
  #e_date_temp=175
  
  ncases_temp=europe[europe$iso_code==temp_code,]
  s_date_temp=as.numeric(ncases_temp$date[ncases_temp$new_cases_smoothed!=0 & (!is.na(ncases_temp$new_cases_smoothed))][1])
  e_date_temp=s_date_temp+119
  ncases_temp1=ncases_temp$new_cases_smoothed[as.numeric(ncases_temp$date)<=e_date_temp & as.numeric(ncases_temp$date)>=s_date_temp]
  ncases_temp1[is.na(ncases_temp1)]=0
  
  intervention_temp1=intervention$date_start[(intervention$Country==temp_code) & (intervention$Response_measure %in% intervention_considered)]
  intervention_temp2=intervention$date_end[(intervention$Country==temp_code) & (intervention$Response_measure %in% intervention_considered)]
  intervention_temp=as.numeric(na.omit(union(union(intervention_temp1,intervention_temp2),intervention_temp1+14)))
  intervention_temp=sort(intervention_temp)
  intervention_temp3=intervention_temp
  intervention_temp=sort(union(intervention_temp3,intervention_temp))
  intervention_temp=as.numeric(intervention_temp)-(s_date_temp-1)
  intervention_temp=intervention_temp[intervention_temp<120 & intervention_temp>0]
  if(length(intervention_temp)==0)
  {
    next
  }
  intervention_temp=union(intervention_temp,c(120))
  intervention_date_final_temp=diff(intervention_temp)
  intervention_date_final_temp=c(intervention_temp[1],intervention_date_final_temp)
  dir.create(paste0("countries/",temp_code))
  write.csv(intervention_date_final_temp,paste0("countries/",temp_code,"/knots.csv"), row.names = FALSE)
  npySave(paste0("countries/",temp_code,"/train_data.npy"),ncases_temp1)
  c_codes2=c(c_codes2,temp_code)
}
## python command generate function

cmd_gen = function(c_codes2,i,knots, t0max=14, t0min=14){
  country_code_temp = c_codes2[i]
  ## set initial_a
  initial_a1 = rep(0.2, (length(knots)+1))
  initial_a = paste(initial_a1,collapse = ',')
  ## file path
  inputpath = paste0("/Users/zhen/Documents/WC-Datathon2021Spring/countries/",country_code_temp,"/train_data.npy")
  outputpath = paste0("/Users/zhen/Documents/WC-Datathon2021Spring/countries/",country_code_temp,"/")
  ##generate command
  paste0("python run_single_machine_model.py --knots=",paste(knots,collapse = ',')," --input_path='", inputpath, "' --output_path='",outputpath, "' --max_epochs=1000 --initial_a=", initial_a, " --learning_rate=0.01 --test_duration=120 --min_t0=", t0max, " --max_t0=", t0min)
}

command_vec = vector()
for(i in 1:length(c_codes2))
{
  temp_code=c_codes2[i]
  knots_temp=read.csv(paste0("countries/",temp_code,"/knots.csv"))
  knots_temp=as.numeric(knots_temp$x)
  command_vec[i]=cmd_gen(c_codes2,i,knots_temp)
}
library(dplyr)
input_info = tibble(c_codes2, command_vec)
#View(input_info)
library(readr)
write_csv(input_info, "input_info.csv")
#############################################################
# fitted_new_cases_smoothed=matrix(NA,length(c_codes2),120)
# a_t=matrix(NA,length(c_codes2),120)
# observed_new_cases_smoothed=matrix(NA,length(c_codes2),120)
# for(i in 1:length(c_codes2))
# {
#   
#   temp_code=c_codes2[i]
#   knots_temp=read.csv(paste0("countries/",temp_code,"/knots.csv"))
#   intervention_temp=cumsum(knots_temp[,2])
#   temp1=npyLoad(paste0("countries/",temp_code,"/predicted_infection_rate.npy"))
#   a_t[i,]=temp1
#   temp2=npyLoad(paste0("countries/",temp_code,"/predicted_daily_observed.npy"))
#   fitted_new_cases_smoothed[i,]=temp2
#   temp3=npyLoad(paste0("countries/",temp_code,"/train_data.npy"))
#   observed_new_cases_smoothed[i,]=temp3
#   plot(temp2,col=1)
#   points(temp3,col=2)
#   points(max(temp2)*temp1,col=3)
#   abline(v=intervention_temp)
# }

# for(i in 1:length(c_codes2))
# {
#   
#   temp_code=c_codes2[i]
#   if(temp_code=='DNK' | temp_code=='ISL' | temp_code=='DEU')
#   {
#     next
#   }
#   temp2=fitted_new_cases_smoothed[i,]
#   temp1=a_t[i,]
#   temp3=read.csv()
# }


library(tidyverse)
library(ggplot2)
# plot train, pred with t0=14
for (i in 1:length(c_codes2)){
  temp_code=c_codes2[i]
  t0=14
  filepath = paste0("countries/",temp_code)
  pred_obs = npyLoad(paste0(filepath,"/predicted_daily_observed.npy"))
  pred_rate = npyLoad(paste0(filepath,"/predicted_infection_rate.npy"))
  train_obs = npyLoad(paste0(filepath,"/train_data.npy"))
  
  ncases_temp=europe[europe$iso_code==temp_code,]
  s_date_temp=as.numeric(ncases_temp$date[ncases_temp$new_cases_smoothed!=0 & (!is.na(ncases_temp$new_cases_smoothed))][1])
  e_date_temp=s_date_temp+119
  date_start_temp=as.Date("2020-1-1")+s_date_temp
  date_end_temp=date_start_temp+119
  
  start_train = date_start_temp
  train_len=120
  train_date=start_train+(0:(train_len-1))
  pred_df = tibble(date = train_date, new_obs =pred_obs, pred = "Fitted Cases")
  train_df = tibble(date = train_date, new_obs =train_obs, pred = "Observed Cases")
  rate_df = tibble(date = train_date, new_obs =pred_rate, pred = "Transmission Rate")
  knots_temp=read.csv(paste0("countries/",temp_code,"/knots.csv"))
  intervention_temp=cumsum(knots_temp$x)
  knots_date = start_train + c(intervention_temp[1:(length(intervention_temp)-1)])-1
  
  knots_df1=tibble(date=c(knots_date[1],knots_date[1]), new_obs=c(min(train_obs),max(train_obs)), pred = "Stay-At-Home-Order Starts")
  knots_df2=tibble(date=c(knots_date[2],knots_date[2]), new_obs=c(min(train_obs),max(train_obs)), pred = "Post Intervention Date")
  knots_df3=tibble(date=c(knots_date[3],knots_date[3]), new_obs=c(min(train_obs),max(train_obs)), pred = "Stay-At-Home-Order Ends")
  
  rate_df_temp=rate_df
  rate_df_temp$new_obs=max(train_obs)*rate_df_temp$new_obs
  dat = bind_rows(pred_df,train_df,rate_df_temp,knots_df1,knots_df2,knots_df3)
  names(dat)[3]="label"
  names(dat)[2]="Daily_Confirmed_Cases"
  dat$label = factor(dat$label,levels=c("Fitted Cases","Observed Cases","Transmission Rate","Stay-At-Home-Order Starts","Post Intervention Date","Stay-At-Home-Order Ends"))
  
  p1 = ggplot(dat, aes(x=date, y=Daily_Confirmed_Cases, color = label, linetype = label)) + 
    geom_line() + 
    scale_y_continuous(
      # Features of the first axis
      name = "Daily Confirmed Cases",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./max(train_obs), name="Transmission Rate")
    ) + 
    scale_linetype_manual(values=c("solid", "twodash", "solid", "dotted", "dotted","dotted")) +
    scale_colour_manual(values=c("darkgreen","red","orange","blue", "green", "purple")) +
    # geom_vline(xintercept = knots_date[1], linetype="dotted",col=1) +
    # geom_vline(xintercept = knots_date[2], linetype="dotted",col=2) +
    # geom_vline(xintercept = knots_date[3], linetype="dotted",col=3) +
    labs(title = paste0("Daily Confirmed New Cases"), 
         subtitle = paste0("Country Code: ", temp_code))
  #p1
  plotname = paste0("tex files/plots/",temp_code,"_transmission.pdf")
  pdf(plotname, width = 10, height = 6)
  print(p1)
  dev.off()
}

temp_code=c_codes2[12]
t0=14
filepath = paste0("countries/",temp_code)
pred_obs = npyLoad(paste0(filepath,"/predicted_daily_observed.npy"))
pred_rate = npyLoad(paste0(filepath,"/predicted_infection_rate.npy"))
train_obs = npyLoad(paste0(filepath,"/train_data.npy"))

ncases_temp=europe[europe$iso_code==temp_code,]
s_date_temp=as.numeric(ncases_temp$date[ncases_temp$new_cases_smoothed!=0 & (!is.na(ncases_temp$new_cases_smoothed))][1])
e_date_temp=s_date_temp+119
date_start_temp=as.Date("2020-1-1")+s_date_temp
date_end_temp=date_start_temp+119

start_train = date_start_temp
train_len=120
train_date=start_train+(0:(train_len-1))
pred_df = tibble(date = train_date, new_obs =pred_obs, pred = "Fitted Cases")
train_df = tibble(date = train_date, new_obs =train_obs, pred = "Observed Cases")
rate_df = tibble(date = train_date, new_obs =pred_rate, pred = "Transmission Rate")
knots_temp=read.csv(paste0("countries/",temp_code,"/knots.csv"))
intervention_temp=cumsum(knots_temp$x)
knots_date = start_train + c(intervention_temp[1:(length(intervention_temp)-1)])-1

knots_df1=tibble(date=c(knots_date[1],knots_date[1]), new_obs=c(min(train_obs),max(train_obs)), pred = "Stay-At-Home-Order Starts")
#knots_df2=tibble(date=c(knots_date[2],knots_date[2]), new_obs=c(min(train_obs),max(train_obs)), pred = "Post Intervention Date")
knots_df3=tibble(date=c(knots_date[3],knots_date[3]), new_obs=c(min(train_obs),max(train_obs)), pred = "Stay-At-Home-Order Ends")

rate_df_temp=rate_df
rate_df_temp$new_obs=max(train_obs)*rate_df_temp$new_obs
dat = bind_rows(train_df,knots_df1,knots_df3)
names(dat)[3]="label"
names(dat)[2]="Daily_Confirmed_Cases"
dat$label = factor(dat$label,levels=c("Observed Cases","Stay-At-Home-Order Starts","Stay-At-Home-Order Ends"))

p1 = ggplot(dat, aes(x=date, y=Daily_Confirmed_Cases, color = label, linetype = label)) + 
  geom_line() + 
  scale_y_continuous(
    # Features of the first axis
    name = "Daily Confirmed Cases",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./max(train_obs), name="Transmission Rate")
  ) + 
  scale_linetype_manual(values=c("twodash", "dotted","dotted")) +
  scale_colour_manual(values=c("red", "blue","purple")) +
  # geom_vline(xintercept = knots_date[1], linetype="dotted",col=1) +
  # geom_vline(xintercept = knots_date[2], linetype="dotted",col=2) +
  # geom_vline(xintercept = knots_date[3], linetype="dotted",col=3) +
  labs(title = paste0("Daily Confirmed New Cases"), 
       subtitle = paste0("Country Code: ", temp_code))
p1
plotname = paste0("tex files/plots/",temp_code,"_observed.pdf")
pdf(plotname, width = 10, height = 6)
print(p1)
dev.off()

change_of_slopes=matrix(NA,length(c_codes2),2)
for (i in 1:length(c_codes2)){
  temp_code=c_codes2[i]
  t0=14
  filepath = paste0("countries/",temp_code)
  pred_obs = npyLoad(paste0(filepath,"/predicted_daily_observed.npy"))
  pred_rate = npyLoad(paste0(filepath,"/predicted_infection_rate.npy"))
  train_obs = npyLoad(paste0(filepath,"/train_data.npy"))
  
  ncases_temp=europe[europe$iso_code==temp_code,]
  s_date_temp=as.numeric(ncases_temp$date[ncases_temp$new_cases_smoothed!=0 & (!is.na(ncases_temp$new_cases_smoothed))][1])
  e_date_temp=s_date_temp+119
  date_start_temp=as.Date("2020-1-1")+s_date_temp
  date_end_temp=date_start_temp+119
  
  start_train = date_start_temp
  train_len=120
  train_date=start_train+(0:(train_len-1))
  pred_df = tibble(date = train_date, new_obs =pred_obs, pred = "Fitted Cases")
  train_df = tibble(date = train_date, new_obs =train_obs, pred = "Observed Cases")
  rate_df = tibble(date = train_date, new_obs =pred_rate, pred = "Transmission Rate")
  knots_temp=read.csv(paste0("countries/",temp_code,"/knots.csv"))
  intervention_temp=cumsum(knots_temp$x)
  knots_date = start_train + c(intervention_temp[1:(length(intervention_temp)-1)])-1
  
  knots_df1=tibble(date=c(knots_date[1],knots_date[1]), new_obs=c(min(train_obs),max(train_obs)), pred = "Stay-At-Home-Order Starts")
  knots_df2=tibble(date=c(knots_date[2],knots_date[2]), new_obs=c(min(train_obs),max(train_obs)), pred = "Post Intervention Date")
  knots_df3=tibble(date=c(knots_date[3],knots_date[3]), new_obs=c(min(train_obs),max(train_obs)), pred = "Stay-At-Home-Order Ends")
  
  rate_df_temp=rate_df
  rate_df_temp$new_obs=max(train_obs)*rate_df_temp$new_obs
  dat = bind_rows(pred_df,train_df,rate_df_temp,knots_df1,knots_df2,knots_df3)
  names(dat)[3]="label"
  names(dat)[2]="Daily_Confirmed_Cases"
  dat$label = factor(dat$label,levels=c("Fitted Cases","Observed Cases","Transmission Rate","Stay-At-Home-Order Starts","Post Intervention Date","Stay-At-Home-Order Ends"))
  
  change_of_slopes[i,1]=(rate_df$new_obs[rate_df$date==(knots_date[1]+1)]-rate_df$new_obs[rate_df$date==(knots_date[1])])-(rate_df$new_obs[rate_df$date==knots_date[1]]-rate_df$new_obs[rate_df$date==(knots_date[1]-1)])
  change_of_slopes[i,2]=(rate_df$new_obs[rate_df$date==(knots_date[3]+1)]-rate_df$new_obs[rate_df$date==(knots_date[3])])-(rate_df$new_obs[rate_df$date==knots_date[3]]-rate_df$new_obs[rate_df$date==(knots_date[3]-1)])
}
change_of_slopes_df=t(change_of_slopes)
colnames(change_of_slopes_df)<-c_codes2
rownames(change_of_slopes_df)<-c("start of lockdown","end of lockdown")
library(xtable)
xtable(change_of_slopes_df,digits=3)


wilcox.test(as.numeric(na.omit(change_of_slopes[,1])),alternative=c("less"))
wilcox.test(as.numeric(na.omit(change_of_slopes[,2])),alternative=c("greater"))