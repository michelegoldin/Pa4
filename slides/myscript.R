devtools::install_github("jvcasillas/untidydata")
devtools::install_github("yihui/xaringan")
library(untidydata)
library(xaringan)
library(plot3D)
library(tidyverse)

fricatives <- read.csv('./data_raw/fricatives/fricatives.csv', header = TRUE, sep = ',')

str(fricatives)
head(fricatives)

#Descriptives

mean(fricatives$s_cog)
sd(fricatives$s_cog)
mean(fricatives$sh_cog)
sd(fricatives$sh_cog)
mean(fricatives$s_skewness)
sd(fricatives$s_skewness)
mean(fricatives$sh_skewness)
sd(fricatives$sh_skewness)

#Boxplot cog ~ phoneme

fricatives%>%
  select('s_cog', 'sh_cog')%>%
  gather(., key = cog, val = cogvalue)%>%
  separate(., col = cog, into = c('phoneme', 'cog'), sep="_")%>%
  ggplot(., aes(x=cogvalue, y = phoneme)) +
  geom_boxplot()

#Plot skewness ~ phoneme

fricatives%>%
  select('s_skewness', 'sh_skewness')%>%
  gather(., key = skewness, val = skewvalue)%>%
  separate(., col = skewness, into = c('phoneme2', 'skewness'), sep="_")%>%
  ggplot(., aes(x=skewvalue, y = phoneme2)) +
  geom_point()

#8. Boxplot cog ~ skewness of S

myfrics <- fricatives%>%
  gather(., 's_cog', 'sh_cog', key = cog, val = cogvalue)%>%
  separate(., col = cog, into = c('phoneme', 'cog'), sep="_")%>%
  gather(., 's_skewness', 'sh_skewness', key = skewness, val = skewvalue)%>%
  separate(., col = skewness, into = c('phoneme2', 'skewness'), sep="_")%>%
  filter(., phoneme2 == 's')

myfrics%>%
  ggplot(., aes(x = cogvalue, y = skewvalue)) +
  geom_boxplot()

#Make a table

```{r, table_print}
myfrics %>% 
  group_by(., cyl) %>% 
  summarize(., mean_mpg = mean(mpg)) %>% 
  knitr::kable(., format = 'html', digits = 2)
````

#Scatterplot

myfrics%>%
  ggplot(., aes(x = cogvalue, y = skewvalue)) +
  geom_point()

  my_mod <- lm(cogvalue ~ skewvalue, data = myfrics)
summary(my_mod)

head(myfrics)

  
#Description

#The mean cog of phoneme 's' is 5690.524, SD = 913.25 and the mean cog of phoneme 'sh' is 
#3994.823, SD = 697.09. The mean skewness of phoneme 's' is -0.7714211, SD = 0.28 and
#the mean skewness of phoneme 'sh' is 0.5527295, SD = 0.44. The scatterplot shows us that
#center of gravity as a function of 's' skewness. There is a positive correlation, as the center
#of gravity of both 's' and 'sh' phonemes increases, so does the skewness of 's' (B = 2664.3, SE = 526.7, t value = 5.06, p<.01)


