install.packages("tidyverse")
library(tidyverse)

install.packages("palmerpenguins")
install.packages("ggthemes")
library(palmerpenguins)
library(ggthemes)

penguins

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  theme_economist() +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

#1)How many rows are in penguins? How many columns?

length(penguins)
nrow(penguins)
print(paste("Hay ", nrow(penguins), " filas y ", length(penguins), " columnas"))

#2)What does the bill_depth_mm variable in the penguins data frame describe? 
#Read the help for ?penguins to find out.

help(penguins)
#es un dato numerico sobre la profundidad del pico de los pinguinos

#3)Make a scatterplot of bill_depth_mm vs. bill_length_mm. That is, make a
#scatterplot with bill_depth_mm on the y-axis and bill_length_mm on the x-axis.
#Describe the relationship between these two variables.

ggplot(data = penguins,
  mapping = aes(x = bill_length_mm, y = bill_depth_mm)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  theme_economist() +
  labs(
    title = "Relation between bill depth and bill length",
    subtitle = "Bill dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Bill length (mm)", y = "Bill depth (mm)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()
#La relacion es debil pero negativa, mientras mas largo es el pico, menos profundo es.

#4) What happens if you make a scatterplot of species vs. bill_depth_mm? 
#What might be a better choice of geom?

ggplot(data = penguins,
       mapping = aes(x =species, y =bill_depth_mm)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  theme_economist() +
  labs(
    title = "Relation between bill depth and species",
    subtitle = "Bill depth for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Species", y = "Bill depth (mm)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()
#En este caso un scatterplot no sirve. Seria mas util usar un Boxplot para
#analizar la distribucion de la profundidad del pico por cada especie.

#5) Why does the following give an error and how would you fix it?

ggplot(data = penguins) + 
  geom_point()
#Da error porque no se le esta diciendo a ggplot que variables mapear para los 
#ejes del grafico.

#6) What does the na.rm argument do in geom_point()? What is the default value
#of the argument? Create a scatterplot where you successfully use this argument 
#set to TRUE.
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species), na.rm = TRUE) +
  geom_smooth(method = "lm") +
  theme_economist() +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()
#El argumento na.rm sirve para eliminar los valores NA del grafico. El valor por
#defecto es FALSE.

#7) Add the following caption to the plot you made in the previous exercise: 
#“Data come from the palmerpenguins package.” Hint: Take a look at the 
#documentation for labs().

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species), na.rm = TRUE) +
  geom_smooth(method = "lm") +
  theme_economist() +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species",
    caption = "Data come from the palmerpenguins package."
  ) +
  scale_color_colorblind()


#8) Recreate the following visualization. What aesthetic should bill_depth_mm be 
# mapped to? And should it be mapped at the global level or at the geom level?
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = bill_depth_mm), na.rm = TRUE) +    
  geom_smooth(method = "loess") + #loess hace que el grafico ajuste con una linea de regresion suave
  theme_economist() +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Bill depth (mm)"
  ) +
  scale_color_gradient()

# 9) Run this code in your head and predict what the output will look like. 
# Then, run the code in R and check your predictions.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)
#Se tratara de un graffico de dispersion en el que se ve la relacion entre 
# longitud de las aletas y masa corporal de cada pinguino, clasificando por isla y con una linea de regresion para cada isla.


#10) Will these two graphs look different? Why/why not?

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )
#No, ambos graficos se veran iguales. En el primero, se mapea a nivel global, 
# por lo que no es necesario mapear los mismos datos por separado en los geoms sucesivos. 