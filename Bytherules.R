install.packages("ggpubr")
install.packages("plm")
install.packages("xtable")
install.packages("psych")
install.packages("knitr")
install.packages("doBy")
install.packages("skimr")
install.packages("reporttools")
library(readxl)
library(stargazer)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(xtable)
library(psych)

PT <- read_excel("~/Desktop/Winterscheidt_CLEANDATA.xlsx")
View(PT)  
PT$Primary <- factor(PT$Type, labels=c("Open", "Closed", "Mixed"))



s <- describe(PT$Turnout, skew = FALSE)
s1 <- do.call("rbind",describeBy(PT$Turnout,PT$Primary, skew=FALSE))
s2 <- do.call("rbind",describeBy(PT$Turnout,PT$Year, skew=FALSE))

df2latex(s, caption="Turnout Summary Statistics", heading="")
df2latex(s1, caption="Turnout Summary Statistics by Type",heading="")
df2latex(s2, caption="Turnout Summary Statistics by Year",heading="")

h <- ggplot(PT, aes(Turnout), type = "density") +
  geom_histogram(fill = "steelblue", color = "grey30", aes(y=..density..)) +
  ylab("Density") + xlab("Turnout") + ggtitle("Histogram of Turnout") +
  geom_density(col="orange");h
ggsave("H.png", units="in", path = "~/Desktop/DSP",width=10, height=10, dpi=300)

ggplot(PT, aes(x=Type, y=Turnout,color=Primary)) + 
  geom_point()
ggsave("scatter.png", units="in", path = "~/Desktop/DSP",width=10, height=10, dpi=300)

df <- PT %>% #data from of open and closed
  filter(Type == "1" | Type == "2") %>%
  select(Type, Turnout)

df2 <- PT %>% #data frame of open and mixed
  filter(Type=="1"|Type=="3")%>%
  select(Type,Turnout)

df3 <- PT %>% #data frame of closed and mixed
  filter(Type=="2"|Type=="3")%>%
  select(Type,Turnout)

p <- ggplot(PT, aes(Turnout)) +
  geom_histogram(fill = "steelblue", color = "grey30",aes(y=..density..)) +
  facet_wrap(~ Type)+
  geom_density(col="orange");p
ggsave("hs.png", units="in", path = "~/Desktop/DSP",width=20, height=10, dpi=300)
t.test(Turnout ~ Type, data = df, var.equal=FALSE)
t.test(Turnout ~ Type, data = df2,var.equal=FALSE)
t.test(Turnout ~ Type, data = df3,var.equal=FALSE)

reg <- lm(Turnout~Type+as.factor(Year)-1, PT);summary(reg)
reg2 <- lm(Turnout~Type+Month+White+Unemp+as.factor(Year)-1, PT);summary(reg2)
reg3 <- lm(Turnout~Type+Month+Unemp+as.factor(Year)-1, PT);summary(reg3)
reg4 <- lm(Turnout~Type+Unemp+as.factor(Year)-1, PT);summary(reg4)

a1 <- anova(reg, reg4)
a2 <- anova(reg4,reg3)
a3 <- anova(reg4,reg2)
a4 <- anova(reg,reg2)

stargazer(reg, reg2,reg3,reg4, title="Regressions of Turnout on Type",font.size="small",omit.stat="f")
print(xtable(a1, "ANOVA of Models 1 and 4"), type = "latex")
print(xtable(a2, "ANOVA of Models 4 and 3"), type = "latex")
print(xtable(a3, "ANOVA of Models 4 and 2"), type = "latex")
print(xtable(a4, "ANOVA of Models 1 and 2"), type = "latex")
?xtable
group_by(PT, Type) %>%
  summarise(
    count = n(),
    mean = mean(Turnout, na.rm = TRUE),
    sd = sd(Turnout, na.rm = TRUE))
    

ggboxplot(PT, x = "Type", y = "Turnout", 
          color = "Primary", palette = c("Red", "Blue", "Green"),
          ylab = "Turnout", xlab = "Type")
ggsave("box.png", units="in", path = "~/Desktop/DSP",width=10, height=10, dpi=300)
ggplot(PT, aes(x=Year, y=Turnout, color=Primary)) + 
  geom_point() +
  scale_colour_hue(breaks= c("Open", "Closed", "Mixed")) + 
  geom_smooth(method=lm,   
              se=FALSE) + 
  theme_bw() + 
  #theme_classic() + 
  labs(title = "Turnout by Year and Primary Type",
       x = "Election Year",
       y = "Turnout Rate") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("line.png", units="in", path = "~/Desktop/DSP",width=10, height=10, dpi=300)
