library(tidyverse)

df<-read.csv("sturgeon_DepthProfile_data.csv") %>% #load in data
  distinct() %>% 
  select(SAMPLE_DATE,profile_depth,DISSOLVED.OXYGEN_DP_NA) %>% #keep relevant columns
  arrange(profile_depth) %>% 
  arrange(SAMPLE_DATE) #group profiling/sampling events

df2 <- df %>% 
  group_by(SAMPLE_DATE) %>% 
  group_split() #split sampling events

crosses <- data.frame(crosses=as.double()) #empty dataframe to record when x=2

for(i in 1:length(df2)){
  temp <- df2[[i]] #create loop dataframe
  pos1 <- max(which(temp$DISSOLVED.OXYGEN_DP_NA > 2)) #find lowest index position with DO higher than 2
  pos2 <- min(which(temp$DISSOLVED.OXYGEN_DP_NA < 2)) #find highest index position with DO lower than 2
  x1 <- temp$DISSOLVED.OXYGEN_DP_NA[pos1] #x of index
  x2 <- temp$DISSOLVED.OXYGEN_DP_NA[pos2]
  y1 <- temp$profile_depth[pos1] #y of index
  y2 <- temp$profile_depth[pos2]
  m <- (y2-y1)/(x2-x1) #calculate slope
  b <- y2-(m*x2) #calculate y-intercept
  y3 <- (m*2)+b #solve for x=2
  crosses[nrow(crosses) + 1, ] <- y3 #save y for x=2
  ggplot(temp,aes(x=DISSOLVED.OXYGEN_DP_NA,y=profile_depth))+ #plot profile and x=2 to verify
    geom_point()+
    scale_y_reverse()+
    geom_vline(xintercept = 2, 
               linetype="dotted", 
               color = "blue", 
               size=0.5)+
    geom_point(aes(x=2, y=y3), colour="blue",shape=4)}

y4 <- mean(crosses$crosses) #calculate mean of all y values


ggplot(df,aes(x=DISSOLVED.OXYGEN_DP_NA,y=profile_depth,color=SAMPLE_DATE))+ #plot all profiles and the mean depth
  geom_point()+
  geom_line()+
  geom_hline(yintercept = y4, 
             linetype="dotted",
             size=0.5)+
  geom_vline(xintercept = 2, 
             linetype="dotted",
             size=0.5)+
  geom_point(aes(x=2, y=y4),shape=8,color="black",size=5)+
  scale_y_reverse()
