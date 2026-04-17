library(ggplot2)
library(nycflights13)
library(tidyverse)
flights

# 1) In a single pipeline for each condition, find all flights that meet the condition:

# - Had an arrival delay of two or more hours
flights |>
  filter(arr_delay >= 120)

# - Flew to Houston (IAH or HOU)
flights |>
  filter(dest == "IAH" | dest == "HOU")

# - Were operated by United, American, or Delta

flights |>
  filter(carrier == "UA" | carrier == "AA" | carrier == "DL")

# - Departed in summer (July, August, and September)
flights |>
  filter(month == 7 | month == 8 | month == 9)

# - Arrived more than two hours late but didn’t leave late
flights |>
  filter(arr_delay > 120 & dep_delay <= 0)

# - Were delayed by at least an hour, but made up over 30 minutes in flight
flights |>
  filter(dep_delay >= 60 & (dep_delay - arr_delay) > 30)

#2) Sort flights to find the flights with the longest departure delays. 
flights |>
  arrange(desc(dep_delay))
# Find the flights that left earliest in the morning.
flights |>
  arrange(dep_time)

#3) Sort flights to find the fastest flights. (Hint: Try including a math calculation inside of your function.)

flights |>
  arrange(desc(distance / air_time))

#4. Was there a flight on every day of 2013?

flights |>
  distinct(month, day) |>
  count()
# si

#5)Which flights traveled the farthest distance? Which traveled the least distance?
flights |>
  arrange(desc(distance))  #mas lejos

flights |>
  arrange(distance) #mas cerca

#6) Does it matter what order you used filter() and arrange() if you’re using 
# both? Why/why not? Think about the results and how much work the functions 
# would have to do.

#no, porque filter es la unica funcion que elimina datos, arrange simplemente los ordena.
# Por practicidad, sin embargo, puede resultar mas comodo primero usar filter.

#7) Compare dep_time, sched_dep_time, and dep_delay. How would you expect those 
#three numbers to be related?

dep_time - sched_dep_time == dep_delay

#8) Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights.

flights |>
  select(dep_time, dep_delay, arr_time, arr_delay)

flights |>
  select(starts_with("dep"), starts_with("arr"))

flights |>
  select(ends_with("time"), ends_with("delay"))

flights |>
  select(contains("time"), contains("delay"))
#9)What happens if you specify the name of the same variable multiple times in a select() call?

flights |>
  select(dep_time, dep_time, arr_time)
#selecciona esa variable e ignora que se haya repetido la seleccion

#10)What does the any_of() function do? Why might it be helpful in conjunction with this vector?

variables <- c("year", "month", "day", "dep_delay", "arr_delay")
flights|>
select(any_of(variables))
# sirve para seleccionar las variables definidas dentro de un vector

#11) Does the result of running the following code surprise you? How do the select helpers deal with upper and lower case by default? How can you change that default?

flights |> select(contains("TIME"))
# no sorprende, ya que la funcion select no es case sensitive. para cambiarlo, 
# deberiamos usar ignore.case == FALSE

#12) Rename air_time to air_time_min to indicate units of measurement and move
# it to the beginning of the data frame.

flights |>
  rename(air_time_min = air_time) |>
  relocate (air_time_min, .before = 1)

#13) Why doesn’t the following work, and what does the error mean?

flights |> 
  select(tailnum) |> 
  arrange(arr_delay)
#> Error in `arrange()`:
#> ℹ In argument: `..1 = arr_delay`.
#> Caused by error:
#> ! object 'arr_delay' not found

# no funciona porque arr_delay no esta presente en el data frame resultante de
# select(tailnum) 


#14) Which carrier has the worst average delays? Challenge: can you disentangle
# the effects of bad airports vs. bad carriers? Why/why not? (Hint: think about
#flights |> group_by(carrier, dest) |> summarize(n()))

flights |>
  group_by(carrier) |>
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) |>
  arrange(desc(avg_arr_delay))

#15) Find the flights that are most delayed upon departure to each destination.

flights |>
  group_by(dest) |>
  slice_max(dep_delay, n = 1)

#16) How do delays vary over the course of the day? Illustrate your answer with a plot.

flights |>
  group_by(hour) |>
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) |>
  ggplot(aes(x = hour, y = avg_dep_delay)) +
  geom_line() +
  labs(x = "Hora", y = "Demora media de partida (minutes)", title = "Denmora media de partida segun hora del dia") +
  theme_minimal()

#17.What happens if you supply a negative n to slice_min() and friends?

#Elimina una cantidad N de filas de valor mas alto.

#18.Explain what count() does in terms of the dplyr verbs you just learned. What does the sort argument to count() do?

# count() es una funcion que combina group_by() y summarize() para contar el numero de filas por grupo.

#19) Suppose we have the following tiny data frame:

df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)

#a) Write down what you think the output will look like, then check if you were correct, and describe what group_by() does.

df |>
  group_by(y)
#group_by(y) agrupa el data frame por la variable y, lo que permite realizar operaciones de resumen o mutación por grupo.

#b) Write down what you think the output will look like, then check if you were correct, and describe what arrange() does. Also, comment on how it’s different from the group_by() in part (a).

df |>
  arrange(y)
# arrange(y) ordena el data frame por la variable y, haciendo que esta aparezca primero.

# c)Write down what you think the output will look like, then check if you were correct, and describe what the pipeline does.

df |>
  group_by(y) |>
  summarize(mean_x = mean(x))
# el pipe agrupa el data frame por la variable y una vez agrupado, calcula la
#media de x para cada grupo y devuelve un nuevo data frame con las variables y y mean_x.



#.2.4 Exercises
#20) We forgot to draw the relationship between weather and airports in
# Figure 19.1. What is the relationship and how should it appear in the diagram?

# se relacionan mediante la tabla flights por la variable origin.

#21) weather only contains information for the three origin airports in NYC. 
# If it contained weather records for all airports in the USA, what additional
# connection would it make to flights?

# se relacionaria con flights por la variable dest, ademas de origin.

#22) The year, month, day, hour, and origin variables almost form a compound key
#for weather, but there’s one hour that has duplicate observations. Can you
#figure out what’s special about that hour?


#23) We know that some days of the year are special and fewer people than usual
# fly on them (e.g., Christmas eve and Christmas day). How might you represent 
# that data as a data frame? What would be the primary key? How would it connect
# to the existing data frames?

holidays<- flights|>
  filter(month == 12 & day %in% c(24, 25))
holidays

#la primary key seria year, month, day.

#24) Find the 48 hours (over the course of the whole year) that have the worst
# delays. Cross-reference it with the weather data. Can you see any patterns?

worst_hours <- flights |>
  group_by(year, month, day, hour) |>
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) |>
  arrange(desc(avg_arr_delay)) |>
  slice_head(n = 48)
worst_hours |> 
  left_join(weather)|>
  summarise(
    visib = mean(visib, na.rm = TRUE),
    wind = mean(wind_speed, na.rm = TRUE),
    precip = mean(precip, na.rm = TRUE)
  )

flights2 <- flights |> 
  select(year, time_hour, origin, dest, tailnum, carrier)
#25)Imagine you’ve found the top 10 most popular destinations using this code:

top_dest <- flights2 |>
  count(dest, sort = TRUE) |>
  head(10)

#How can you find all flights to those destinations?
flights2 |>
  semi_join(top_dest, by = "dest") 

#26) Does every departing flight have corresponding weather data for that hour?

flights |>
  anti_join(weather, by = c("year", "month", "day", "hour", "origin"))
# No

#27) What do the tail numbers that don’t have a matching record in planes have
# in common? (Hint: one variable explains ~90% of the problems.)

flights |>
  anti_join(planes, by = "tailnum") |>
  count(carrier, sort = TRUE)

#MQ y AA explican la mayoria de los faltantes.

#28) Add a column to planes that lists every carrier that has flown that plane. 
#You might expect that there’s an implicit relationship between plane and 
#airline, because each plane is flown by a single airline. Confirm or reject
#this hypothesis using the tools you’ve learned in previous chapters.

planes |>
  left_join(flights, by = "tailnum") |>
  group_by(tailnum) |>
  summarize(carriers = n_distinct(carrier)) |>
  filter(carriers > 1)
# hay 83 aviones que han sido operados por mas de una aerolinea, por lo que la hipotesis es rechazada.















