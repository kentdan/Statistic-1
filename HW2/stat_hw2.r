# %% codecell
getwd()
library(tidyverse)
library(gganimate)
library(dplyr)
library(grid)
df <- read.csv("/Users/danielkent/Desktop/Statistic_1/HW2/H2RandomWalk.csv")
glimpse(df)
rw_plot1 = ggplot(df, aes(x=x.axis, y=y.axis)) +
						geom_text(aes(label=step)) +
						geom_line()
print(rw_plot1)
# %% codecell
#Draw a diagram, a box plot and a pie plot that discribe the frequency of the data in H2Q2.csv.
#(You may classify the data using the interval with range 5 if needed.)
df2 <- read.csv("/Users/danielkent/Desktop/Statistic_1/HW2/H2Q2.csv")
df2 <- df2 %>%
				mutate(
				  # Create categories
				  age_group = dplyr::case_when(
				    age < 5            ~ "0-5",
				    age > 5  & age <= 10 ~ "6-10",
				    age > 10 & age <= 15 ~ "11-15",
						age > 15 & age <= 20 ~ "16-20",
				    age > 20 & age <= 25 ~ "21-25",
				    age > 25 & age <= 30 ~ "26-30",
				    age > 30 & age <= 35 ~ "31-35",
						age > 35 & age <= 40 ~ "36-40",
				    age > 40 & age <= 45 ~ "41-45",
						age > 45 & age <= 50 ~ "46-50"
				  ),
				  # Convert to factor
				  age_group = factor(
				    age_group,
				    level = c("0-5","6-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50")
				  )
				)
# %% codecell
glimpse(df2)
tail(df2)
head(df2)
# %% codecell
qplot(df2$age_group)
# %% codecell
qplot(df2$age)
# %% codecell
df2prob <-df2 %>%
  count(age_group, age) %>%
  group_by(age_group) %>%          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n))
# %% codecell
boxplot(age~age_group,data=df2,main="Different age in age group",xlab="age group",ylab="age")
boxplot(df2$age)
# %% codecell
#piechart
ggplot(df2, aes(x="", y=factor(age), fill=age_group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
	theme_void()
# %% codecell
# There is a 4-dimension dataset (y, x1, x2, x3)
# where x1 is the categorical data (e.g species, sex...)
# and y, x2, x3 are continuous data  (e.g temperture, age...). How would you visualize the data?
glimpse(iris)
# %% codecell
ggplot(data = iris, aes(x = Petal.Width, y = Petal.Length))+
  xlab("Petal Length")+
  ylab("Petal Width") +
  geom_point(aes(color = Species,shape=Species))+
  ggtitle("Petal Width vs Length")
# %% codecell
ggplot(iris, aes(Species, Petal.Length, fill=Species)) +
  geom_boxplot()+
  scale_y_continuous("Petal Length (cm)", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Petal Length Box Plot", x = "Species")
# %% codecell
par(mfrow=c(2,2))
ggplot(iris, aes(Species, Petal.Width, fill=Species)) +
  geom_boxplot()+
  scale_y_continuous("Petal Width", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Petal Width Box Plot", x = "Species")
ggplot(iris, aes(Species, Petal.Length, fill=Species)) +
  geom_boxplot()+
  scale_y_continuous("Petal Length", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Petal Length Box Plot", x = "Species")

ggplot(iris, aes(Species, Sepal.Width, fill=Species)) +
  geom_boxplot()+
  scale_y_continuous("Sepal Width", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Sepal Width Box Plot", x = "Species")

ggplot(iris, aes(Species, Sepal.Length, fill=Species)) +
  geom_boxplot()+
  scale_y_continuous("Sepal Length", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Sepal Length Box Plot", x = "Species")
# %% codecell
