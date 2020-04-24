 ################## ggplot#################3

# libraries
library(ggplot2)

#A?adir un segundo eje (de color azul) a un grafico de barras
coeff <-1000
ggplot(data = data.plot.mortalidad, mapping = aes(x = Intervalo, y = Infectados)) +  
  geom_col()+
  geom_point(aes(y=Tasa*coeff*100),color="blue") + #por cien ya que es un porcentaj
  scale_y_continuous(
    # Features of the first axis
    name = "Personas identificadas como infectadas",
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans=~./coeff, name="Tasa mortandad (%)")) +
  labs(x = "Horas desde 8/3 12:00 PM", 
       title ="Personas reconocid@s como infectad@s y fallecid@s hasta la fecha",
       caption = "Data source: @SaludPublicaEs" ) +
  theme(axis.text.y.right = element_text(color = "blue"))

#Reordenar el eje X en funcion de los valores que toma la ordenada
ggplot(data = a, mapping = aes(x = reorder(Comunidad, -Coef2),Comunidad, y = Coef2)) +  
  geom_col()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Comunidad", 
       y = "Fallecidos vs Casos diagnosticados",
       title =paste("Actualizado en ",format(Sys.Date())),
       caption = "Data source: @SaludPublicaEs" )



#Grafico de columnas donde hay und rill down de columnas
#install.packages("viridis")  # Install
library("viridis")           # Load

ggplot(data = data.plot.infectados.com, mapping = aes(x = Intervalo, y = Total_Infectados)) +  
  geom_col(aes(fill=Comunidad),
           color = "navy",size=0.05,alpha=0.3)+
  scale_fill_viridis(discrete= TRUE, option="D")+
  labs(x = "Horas transcurridas desde 8/3 12:00 PM", 
       y = "Numero de personas",
       title =paste("Personas reconocid@s como infectad@s - Actualizado en ",format(Sys.Date())),
       caption = "Data source: @SaludPublicaEs" )

# Viridis Key functions:
#
#  scale_color_viridis(): Change the color of points, lines and texts
#  scale_fill_viridis(): Change the fill color of areas (box plot, bar plot, etc)

#
ggplot(data=UL_Descuentos_simplified, aes(x=customer)) +
  geom_bar(stat="count")  ### histograma de barras, contando para cada elemento

# Diagrama de bolas
ggplot(data=data.plot, aes(x=FT_Category,y=N))+
  geom_count(aes(size=Revenue_2018))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y = "# players", 
       x ="Business category", 
       title ="EU 1000-Fastest growing companies - Opportunity seizing",
       caption = "Data source: FT")

#There are two types of bar charts: geom_bar() and geom_col(). 
#geom_bar() makes the height of the bar proportional to the number of cases 
#in each group (or if the weight aesthetic is supplied, the sum of the weights). 
#If you want the heights of the bars to represent values in the data, use geom_col() 
#instead. 
#geom_bar() uses stat_count() by default: it counts the number of cases at each x 
#position. 
#geom_col() uses stat_identity(): it leaves the data as is.

#Gafico de columnas donde cada barra es 'stacked' en funcion del valor de Comunidad
ggplot(data = data.plot.infectados.com, mapping = aes(x = Intervalo, y = Total_Infectados)) +  
  geom_col(aes(fill=Comunidad))


#histograma donde cada barra es 'stacked' en funcion del factor drv
ggplot(mpg,aes(fl))+geom_bar(aes(y=..count.., fill=drv))
#histograma donde el 'stacked' se elimina (position=identity), y se hacen trasparente (alpha) para vobservar el solape
ggplot(mpg,aes(fl))+
  geom_bar(aes(y=..count.., fill=drv),alpha=1/5, position="identity")
#histograma donde el 'stacked' se mantiene (position=fill) a lo largo de los grupos, pudiendo compararlos
ggplot(mpg,aes(fl))+
  geom_bar(aes(y=..count.., fill=drv),position="fill")
#histograma donde el 'stacked' se elimina (position=dodge) y se pueden comparar los valores individuales
ggplot(mpg,aes(fl))+
  geom_bar(aes(y=..count.., fill=drv),position="dodge")


ggplot(data=UL_Consumos, aes(x=consumo_bruto_2017)) +
  geom_histogram(stat="bin", binwidth = 1000, fill=concesion)  ###histograma sobre datos continuos



#geom point with the data grouped by colors dependign on class variable
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour= class))

#geom point with smooth line and data grouped by drv
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +  
  geom_point(mapping = aes (color= drv)) +
  geom_smooth(mapping = aes (linetype= drv))


#same as above, geom point with the data split by graph(facet wrap)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

#facet wrap with flexible y axis
ggplot(data=data.plot, aes(y=Total_Infectados,x=Fallecidos))+
  geom_point()+
  geom_smooth(method="lm") +
  facet_wrap(Comunidad~ ., scales="free_y")+
  )
#facet wrap with flexible x,y axis
ggplot(data=data.plot, aes(y=Total_Infectados,x=Fallecidos))+
  geom_point()+
  geom_smooth(method="lm") +
  facet_wrap(Comunidad~ ., scales="free")+
  )
#put labels in a line, at the end
library(directlabels)
ggplot(data=data.plot, aes(y=Total.Dead.cumsum,x=Total.Cases.cumsum,group=countries, color=countries))+
  geom_line()+
  labs(y = "Cummulative Known Deaths", 
       x ="Cummulative Known Cases", 
       title =paste("Cases Fatality rate - Deaths on a daily basis vs total known cases. Date=",Sys.Date()),
       caption = "Data source: European Centre for Disease Prevention and Control")+
  geom_dl(aes(label = countries), method = list("last.points")) 


#geom point where the plot is faced on the combination of two variables(facet grid)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl) # can be used for facet only rows or columns by using (. ~ cyl) or (cyl ~ .) 


#Grafico donde los t?tulos de X se giran
ggplot(data=data.plot, aes(x=DateRep,y=NewConfCases,colour= CountryExp))+
  geom_smooth(method="lm") +
  geom_point(size=2,shape=1 )+
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_brewer(palette="Spectral")


############# Box plot
ggplot(data=data.plot, aes(x=as.factor(Week),y=NewConfCases))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 11, outlier.size=2)+
  facet_wrap(CountryExp ~ .)+
  labs(x = "Week", 
       y ="New Confirmed Cases", 
       title =paste("New confirmed cases on a week-rolling basis by main countries/territories in ",Sys.Date()),
       subtitle = paste("Only  countries that reported more than ", LevelInfection, " total cases in total"),
       caption = "Data source: European Centre for Disease Prevention and Control"
  )





#############
#grafico con una envolvente sobre los ejes X,Y para observar distribuci?n de datos
p<- ggplot(data=data.plot, aes(x=DeltaP2*100,y=DeltaP3*100,color = Poblacion,label = Comunidad))+
  #geom_point(aes(size=Densidad))+
  geom_point()+
  scale_color_gradient(low="grey", high="red")+
  geom_text(size=3) +
  coord_cartesian(xlim = c(0, xmax), ylim=c(0,ymax))+
  geom_segment(x=100,xend=250,y=100,yend=100, color= "red",size=2, linetype="dotted")+
  geom_segment(x=100,xend=100,y=100,yend=ymax, color= "red",size=2, linetype="dotted")+
  labs(x = paste("Crecimiento (%) en las ?ltimas 40 horas desde  " , fecha, " Hora: 1 pm" ), 
       y =paste("Crecimiento (%) en las ?timas 72 horas desde ",fecha, " Hora: 1 pm" ), 
       title =paste("Incremento porcentual del total de infectad@s a ", fecha, " Hora: 1 pm"),
       caption = "Data source: @SaludPublicaEs"
  )

ggMarginal(p, size=4, colour= "red")


###Doign a geom_tale , or clustering in a graphical way
center_reshape <- gather(center_df, features, values, Food. : Pets.)

#plot data
ggplot(data = center_reshape, aes(x = features, y = cluster, fill = values)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradient(low="brown", high="yellow")+
  
  
  

#####Animated gifs
##Install gganimate
install.packages("devtools")
install.packages("bitops")
install.packages("gganimate")
install.packages("gapminder")
library(devtools)
library(RCurl)
library(httr)

library("gganimate")
library(gapminder)
library(ggplot2)

theme_set(theme_bw())
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
  geom_point() +
  scale_x_log10()
gganimate(p, interval=2)
###gganimate uses the variable frame, teh time dimension, to change
### the graph in each animation




