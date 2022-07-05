# Dados pra graficos
library(ggplot2)
library(sjlabelled)
library(sjPlot)

#########################################
# Sexo x Cultura 
df %>% janitor::tabyl(Infec.Bacteriana, sexo) %>% 
  clipr::write_clip()

plot_xtab(df$sexo, df$Infec.Bacteriana, margin = "row",
          bar.pos = "stack", coord.flip = FALSE) +
  theme_ipsum_pub() +
  labs(title = 'Distribuição amostral por gênero'
       ,x = 'Gênero') +
  theme(
    plot.title = element_text(face = "bold",size = 13L, hjust = 0.5)
    )

######################################################
## Incidencia da bacteria  

# Ajustar o dataSet
incidencia <-  df %>%
  mutate(across(urocul_e_coli:s_distal_outro, ~ if_else(.x == 'Não',0,1))) %>% 
  filter(sexo != 'M') %>%
  summarise(across(urocul_e_coli:s_distal_outro, ~ sum(.x))) %>% 
  pivot_longer(cols = urocul_e_coli:s_distal_outro, names_to = "variavel", values_to = "valor") %>% 
  mutate(
    grupo = case_when(
      str_detect(variavel, 'urocul') == TRUE ~  'Urocultura'
      ,str_detect(variavel, 'c_p_prox') == TRUE ~ 'C.P.F. Proximal'
      ,str_detect(variavel, 'c_p_dist') == TRUE ~ 'C.P.F.  Distal'
      ,str_detect(variavel, 's_prox') == TRUE ~ 'S.F.  Proximal'
      ,str_detect(variavel, 's_distal') == TRUE ~ 'S.F.  Distal'
    )
    , variavel = str_remove_all(string =variavel,pattern = "urocul_|c_p_prox_|c_p_dist_|s_prox_|s_distal_") 
    %>% str_to_upper()
  ) %>% 
  arrange(valor) %>% 
  mutate(variavel = str_replace_all(str_to_upper(variavel), "_", " ") 
         ,variavel = fct_reorder(variavel, valor)
  )


# Grafico
ggplot(incidencia, aes(x=variavel, y=valor)) +
  geom_segment(
    aes(x=variavel, xend=variavel, y=0, yend=valor), 
    color=ifelse(incidencia$valor == 0, "orange", "#1B9E77"), 
    size=ifelse(incidencia$valor == 0, 1.1, 1.2)
  ) +
  geom_point(
    color=ifelse(incidencia$valor == 0, "#D95F02", "#1B9E77"), 
    size=ifelse(incidencia$valor == 0, 3, 2)
  ) +
  coord_flip() +
  labs(
    x = "Tipo bacteria",
    y = "Nº de Ocorrênica",
    title = "Local de Analise"
    #subtitle = ""
  ) +
  facet_wrap(vars(grupo), scales = "free_x", ncol = 5L) +
  theme_gray() +
  theme(plot.title = element_text(face = "bold")
        #    ,axis.title.x = element_text(hjust = 1)
        ,axis.title.y = element_text(hjust = 1)
  )




df1 <- incidencia %>% group_by(grupo) %>% summarise(valor = sum(valor)) 

ggplot(df1) +
  aes(x = grupo, weight = valor) +
  geom_bar(fill = "#1F78B4") +
  labs(
    x = "",
    y = "",
    title = "Numero de culturas positivas",
    caption = "C.P.F. (cultura padrão fragmento); S.F. (sonicação fragmento)."
  ) +
  theme_ipsum_pub() +
  theme(
    plot.title = element_text(size = 13L,hjust = .5),
    plot.caption = element_text(size = 7L)
  ) + geom_text(aes(grupo,1,label=paste("n =",valor)))


####################################
# Causa internação

plot_xtab(df$dx_da_causa_de_insercao
         ,df$Infec.Bacteriana
         ,margin = "row"
         ,bar.pos = "stack"
         ,coord.flip = FALSE
         ,show.summary = TRUE
         ,summary.pos = "r"
         ) +
  theme_ipsum_pub()+
  labs(title = 'Motivo de inserção por infecção bacteriana'
       ,x = 'Motivo') +
  theme(
    plot.title = element_text(face = "bold",size = 13L, hjust = 0.5)
  ) + ylim(0, 1)


###########################
# 
## Função dos graficos
plot_qui2 <- function(y)
  {
    plot_xtab(df[[y]]
             ,df$Infec.Bacteriana
             ,margin = "row"
             ,bar.pos = "stack"
             ,coord.flip = TRUE
             ,show.summary = TRUE
  ) +
    theme_ipsum_pub() +
   # ylim(0,1.1) +
    theme(axis.title.x = element_text(hjust = .51)) +
    labs(x = str_to_title(y))
}

a = plot_qui2("comorbidades") 

a +
(plot_qui2("diabetes") / plot_qui2("hipertensao") / plot_qui2("insuficiencia_renal")) +  
  plot_layout(guides = "collect") & 
  theme(legend.position='bottom') &
  plot_annotation(title = "Comorbidades x Infecção") 

###

##################################
## Tempo de uso
plot_xtab(df$Qtd_dias_internado
          ,df$Infec.Bacteriana
          ,margin = "row"
          ,bar.pos = "stack"
          ,coord.flip = FALSE
          ,show.summary = TRUE
          ,summary.pos = "r"
          ,ylim = c(0,1.05)
) +
  theme_ipsum_pub()+
  labs(title = 'Tempo de internação'
       ,x = '') +
  theme(
    plot.title = element_text(face = "bold",size = 13L, hjust = 0.5)
  ) +   theme(legend.position='bottom') 



