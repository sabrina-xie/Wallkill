df<-read.csv("sturgeon_DepthProfile_data.csv") %>% 
  distinct() %>% 
  select(SAMPLE_DATE,profile_depth,DISSOLVED.OXYGEN_DP_NA) %>% 
  arrange(profile_depth) %>% 
  arrange(SAMPLE_DATE)

df2 <- df %>% 
  group_by(SAMPLE_DATE) %>% 
  group_split()

crosses <- data.frame(crosses=as.double())

for(i in 1:length(df2)){
  temp <- df2[[i]]
  pos1 <- max(which(temp$DISSOLVED.OXYGEN_DP_NA > 2))
  pos2 <- min(which(temp$DISSOLVED.OXYGEN_DP_NA < 2))
  x1 <- temp$DISSOLVED.OXYGEN_DP_NA[pos1]
  x2 <- temp$DISSOLVED.OXYGEN_DP_NA[pos2]
  y1 <- temp$profile_depth[pos1]
  y2 <- temp$profile_depth[pos2]
  m <- (y2-y1)/(x2-x1)
  b <- y2-(m*x2)
  y3 <- (m*2)+b
  crosses[nrow(crosses) + 1, ] <- y3
  ggplot(temp,aes(x=DISSOLVED.OXYGEN_DP_NA,y=profile_depth))+
         geom_point()+
         geom_vline(xintercept = 2, 
                    linetype="dotted", 
                    color = "blue", 
                    size=0.5)+
         geom_point(aes(x=2, y=y3), colour="blue",shape=4)}

y4 <- mean(crosses$crosses)


ggplot(df,aes(x=DISSOLVED.OXYGEN_DP_NA,y=profile_depth,color=SAMPLE_DATE))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = y4, 
             linetype="dotted",
             size=0.5)+
  geom_vline(xintercept = 2, 
             linetype="dotted",
             size=0.5)+
  geom_point(aes(x=2, y=y4),shape=8,color="black",size=5)
