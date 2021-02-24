#Load packages -----------------------------------------------------------------

library(tidyverse)

#read data----------------------------------------------------------------------

iris
iris <- as_tibble(iris)
iris

# Question 1--------------------------------------------------------------------

Q1<-rename(iris,sepal_length=Sepal.Length,sepal_width=Sepal.Width,petal_length=Petal.Length,petal_width=Petal.Width,species=Species)
Q1

# Question 2--------------------------------------------------------------------

Q2<-select(Q1, sepal_length, sepal_width, petal_length, petal_width)
Q2
mutate(Q1, Q2 = Q2 * 10)

# Question 3--------------------------------------------------------------------

Q3 <-mutate(Q1, sepal_area=sepal_length*sepal_width,petal_area=petal_length*petal_width)
Q3
Q3.1 <-select(Q3,sepal_area,petal_area)
Q3.1

# Question 4--------------------------------------------------------------------

summarize(
  Q2,
  sampl_size = n(),
  max=max(sepal_length),
  min=min(sepal_length),
  range=range(sepal_length),
  median=median(sepal_length),
  q1=quantile(sepal_length, probs = 0.25),
  q2=quantile(sepal_length, probs = 0.75),
  IQR=IQR(sepal_length)
)

# Question 5--------------------------------------------------------------------

Q5<-group_by(Q1,species)

iris_sum<-summarize(
  Q5,
  mean=mean(petal_width),
  sampl_size = n(),
  sd=sd(petal_width),
  var=var(petal_width),
  sem=sd / sqrt(sampl_size), 
  ci_upper=mean+2 * sem,
  ci_lower=mean-2 * sem)
iris_sum

# Question 6--------------------------------------------------------------------

ggplot(data = Q1) + 
  geom_jitter(mapping = aes(x=species,y=petal_width))

# Question 7--------------------------------------------------------------------

ggplot(data=Q1) +
  geom_jitter(mapping=aes(x=species,y=petal_width)) +
  geom_crossbar(
    data=iris_sum,
    mapping=aes(
      x=species,
      y=mean,
      ymax=ci_upper,
      ymin=ci_lower),
    color="red"
  )

# Question 8--------------------------------------------------------------------

ggplot(data=Q1) +
  geom_point(mapping=aes(x=petal_length,y=petal_width,color=species))

