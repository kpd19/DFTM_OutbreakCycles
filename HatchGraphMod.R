library(tidyverse)
library(gridExtra)



x <- seq(1,320,1)
y <- exp(-42.8866 + 15.0204*log(x)-0.308125*x + 0.0010419*x^2 - 0.00000155*x^3)


crackerjack <- as.Date("10-31-19", format = "%m-%d-%y")
pattee <- as.Date("11-13-19", format = "%m-%d-%y")
today <- as.Date("01-30-20", format = "%m-%d-%y")

c_date <- seq.Date(from = crackerjack, by = "day", length.out = 320)
p_date <- seq.Date(from = pattee, by = "day", length.out = 320)


dat <- data.frame(x,y,c_date,p_date)


plot0 <- dat %>% ggplot() + aes(x = x, y = y) + geom_line() + theme_bw() + 
  xlab("Days in Storage (4.5C)") + ylab("% Hatch") +
  ylim(0,90) + geom_vline(xintercept=120, linetype="dashed", color = "blue") + 
  ggtitle("Reproduced hatch graph")

plot1 <- dat %>% filter(x > 75) %>% filter(x<150)%>% ggplot() + aes(x = c_date, y = y) + geom_line() + theme_bw() + 
  xlab("Days in Storage (4.5C)") + ylab("% Hatch") +
  ylim(0,90) + scale_x_date(date_breaks = "months" , date_labels = "%b-%d") +
  ggtitle("Cracker Jack") + geom_vline(xintercept=as.Date(today), linetype="dashed", color = "red") + 
  geom_vline(xintercept=as.Date(crackerjack + 120), linetype="dashed", color = "blue")

plot2 <- dat %>% filter(x > 60) %>% filter(x<150)%>% ggplot() + aes(x = p_date, y = y) + geom_line() + theme_bw() + 
  xlab("Days in Storage (4.5C)") + ylab("% Hatch") +
  ylim(0,90) + scale_x_date(date_breaks = "months" , date_labels = "%b-%d") +
  ggtitle("Pattee Canyon") + geom_vline(xintercept=as.numeric(today), linetype="dashed", color = "red") + 
  geom_vline(xintercept=as.Date(pattee + 120), linetype="dashed", color = "blue")

pdf("hatch.pdf", height = 4, width =10)
grid.arrange(plot0, plot1, plot2, nrow=1)
dev.off()

