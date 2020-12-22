rm(list=ls(all=TRUE))
dff <- readRDS("m_data.rds")

library(segmented)
dff <- na.omit(dff)

df <- dff
sub <- "DAKABARICHA"
dff <- df[df$SUBLOCATION==sub,]
x <- dff$zlmodis
y <- dff$mortality_rate
ml <- lm(y~x, data=dff)
sm <- segmented(ml, seg.Z = ~x, psi=0)
dff$predicted_mortality <- predict(sm)

trigger <- 0.23 
#Compute payouts; 1TLU=1000$
dff$payouts <- pmax(0, dff$predicted_mortality - trigger) * 1000
#Compute Actuarily fair premiumL Premium with markup 25%
premium <- mean(dff$payouts, na.rm=TRUE) * 1.1

#Compute capital with insurance
dff$capital     <- (1 - dff$mortality) * 1000
dff$capital_ins <- (dff$capital + dff$payouts) - premium

dff$perfect <- dff$capital-premium
dff$perfect[dff$perfect < 800] <- 829.593
dff$capital_ins[dff$capital_ins > 800] <- jitter(dff$capital_ins[dff$capital_ins > 800], rep(150, 5))

library(ggplot2)
x11()
cols <- c("blue", "red", "green")
ggplot(dff, aes(x=capital)) +
  geom_smooth(aes(y=capital_ins,  colour="Index insurance"), se = FALSE, size=0.75)+
  #geom_point(aes(y=capital_ins), colour="red")+
  geom_line(aes(y=capital, colour="Income without insurance"), size=0.75) +
  geom_line(aes(y=perfect, colour="Perfect insurance"), size=0.75)+
  labs(y="Assets (USD)", x="Assets (USD)") +
  scale_colour_manual(name="Type", values=cols) + 
  theme(legend.position= "bottom", panel.background = element_rect(fill = "white"), axis.line.x=element_line(), axis.line.y=element_line())  
  


