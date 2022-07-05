
# Carregar bibliotecas
library(tidyverse)
library(hrbrthemes)

library(ggstatsplot)



  janitor::tabyl(df$tempo_de_uso)

  
 
  

  cor(df$tempo_de_uso[df$Infec.Bacteriana == 'Não']
      ,df$tempo_de_uso[df$Infec.Bacteriana == 'Sim'])
  

df |> colnames()


+ labs(
  ,
  title = df$comorbidades
)

df$Qtd_dias_internado


# Teste cruzado categoricos



p2 =  ggbarstats(
    data = df
    ,x = Infec.Bacteriana
    ,y = comorbidades
    ,results.subtitle = TRUE
    ,type  = "nonparametric"
  #  ,palette = palette("ggplot2")
  ) 
  
  p2 + 
    ggplot2::theme_grey(base_size = 8,base_rect_size = 5) +
  + labs(caption = "",
        title = "dsdd") # remove caption +



## Relação da idade com a infecç
library(ggstatsplot)
plt <- ggbetweenstats(
  data = df,
  x = desvolveu_bac,
  y = idade
) 
plt <- plt + 
  # Add labels and title
  labs(
    y = "Idade",
    x = "Teve infecção bacteriana",
    title = "Distribuição da idade das pacientes"
  )
plt

# Tempo de uso
plt <- ggbetweenstats(
  data = df,
  x = desvolveu_bac,
  y = tempo_de_uso
) 
plt <- plt + 
  # Add labels and title
  labs(
    y = "Tempo de uso do cater Dublo J (em dias)",
    x = "Teve infecção bacteriana",
    title = "Distribuição da idade das pacientes"
  )
plt


plt <- ggbetweenstats(
  data = df,
  x = desvolveu_bac,
  y = dias_de_internamento) 

plt <- plt + 
  # Add labels and title
  labs(
    y = "Nº de dias internado",
    x = "Teve infecção bacteriana",
    title = "Distribuição da idade das pacientes"
  )
plt

library(gmodels)
CrossTable(df$desvolveu_bac,df$comorbidades,
           prop.r=FALSE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, expected=TRUE,
           digits=0)




ggplot(df, aes(x=comorbidades , fill = desvolveu_bac)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(y = "Desenvolveu infecção", y = "Comorbidade")

descr::CrossTable(df$desvolveu_bac,df$comorbidades,chisq = T)$CST %>%
  pander::pander()

library(ggstatsplot)
library(ggplot2)


 
ggstatsplot::extract_stats(a)$one_sample_data$p.value    

+ a / a+a
 
ggstatsplot::extract_stats(a)$subtitle_data

labs(caption = "") # remove caption


plot_xtab(df$desvolveu_bac, df$comorbidades, margin = "row", bar.pos = "stack",
         show.summary = TRUE, coord.flip = TRUE)

ggstatsplot::subtitle_contigency_tab(df,desvolveu_bac,comorbidades)


ggstatsplot::extract_stats()



df %>%
  group_by(desvolveu_bac,comorbidades) %>%
  summarize(counts = n()) %>% 
  mutate(perc = (counts / sum(counts))) -> tempdf
# only real changes are geom bar and percent y axis
ggplot(tempdf, aes(fill=desvolveu_bac, y=perc, x=comorbidades)) +
  geom_bar(stat="identity", position="fill") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.10)) +
  geom_label(aes(label = paste0(round(x = perc*100, digits = 1), "%")), show.legend = FALSE, position = position_fill(vjust = 0.5)) +
  ggtitle("test", subtitle = ggstatsplot::extract_stats(a)$subtitle_data)

#  ggtitle("test", subtitle = subtitle_contigency_tab(Titanic_full,Class,Survived))


ggstatsplot::ggscatterstats( data = df
                             ,x = desvolveu_bac
                             ,y = comorbidades)




set.seed(123)
library(ggstatsplot)

# in case of group comparisons
p <- ggbetweenstats(mtcars, cyl, mpg)
extract_stats(p)

y# the exact details depend on the function
extract_stats(ggbarstats(mtcars, cyl, am)

tapply(df$tempo_de_uso, df$Infec.Bacteriana, median)
table(df$sexo) 



