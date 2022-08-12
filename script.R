### importação de pacotes

pacotes <- c("sf", "rnaturalearth", "rnaturalearthdata","Rtsne","factoextra","cluster","party","caret","randomForest","settings","moments","gridExtra","PerformanceAnalytics","caret","fastDummies", "nortest","olsrr","blorr","pryr", "car","readxl","tidyr","dbplyr","ggplot2","plotly","tidyverse","knitr","kableExtra","reshape2","ggrepel",
             "fastDummies","lmtest","splines","jtools","questionr","MASS",
             "pscl","overdisp","magick","cowplot","beepr","rgl","car",
             "nlme","msm","lmeInfo","jtools", "mfx", "ggcorrplot")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


### importação e manipulação da base dos casos de COVID
casos<-read.csv("caso_full.csv")
head(casos)
str(casos)
casos%>%count(date)
table(casos$date)

### Filtro dos casos que aconteceram até dia 30/11/2021
casos_filtro_data<-filter(casos,date=="2021-11-30")
head(casos_filtro_data)

### Filtro dos casos referente apenas às cidades
table(casos_filtro_data$place_type)
casos_filtro_data<-filter(casos_filtro_data,place_type=="city")
head(casos_filtro_data)

### Seleção das variáveispara estudo (código do IBGE e número de casos de COVID para cada 100 mil habitantes)
casos_filtro3<-casos_filtro_data%>%dplyr::select(city_ibge_code,last_available_confirmed_per_100k_inhabitants)
head(casos_filtro3)

###renomeação da coluna
casos_filtro3<-rename(casos_filtro3, COD_IBGE="city_ibge_code")

### importação da base de dados do resultado das eleições
eleicao<-read_xlsx("eleicao3.xlsx")
head(eleicao)

### importação e manipulação da base de dados do índice de Gini
gini<-read_xlsx("Gini.xlsx")
head(gini)
str(gini)
colnames(gini)
gini2<-data.frame(gini)
str(gini2)
gini2$COD_IBGE<-as.numeric(gini2$COD_IBGE)

###junção das bases  por meio da variável código do IBGE dos municípios
dados<-inner_join(casos_filtro3, eleicao, by="COD_IBGE")
head(dados)
colnames(dados)
str(dados)

dados2<-inner_join(gini2,dados,by="COD_IBGE")
dim(dados2)
head(dados2)


### Seleção das variáveis relevantes ao estudo
dados3<-dados2%>%dplyr::select(COD_IBGE,GINI_2010,
                        last_available_confirmed_per_100k_inhabitants,
                        NM_URNA_CANDIDATO,Porcentagem)

### Renomeação das variáveis
dados4<-dados3%>%rename(Codigo_Cidade_IBGE="COD_IBGE",
                        Casos_COVID_100K="last_available_confirmed_per_100k_inhabitants",
                        Candidato_Eleito="NM_URNA_CANDIDATO",
                        Porcentagem_Candidato="Porcentagem")
head(dados4)

###Transformando o código dos municípios em índice
rownames(dados4)<-dados4$Codigo_Cidade_IBGE
str(dados4)

### Tirando a variável código do IBGE, ja que ela passou a ser índice
dados5<-dados4%>%dplyr::select(Casos_COVID_100K,GINI_2010,
                        Porcentagem_Candidato,Candidato_Eleito)
head(dados5)
dim(dados5)
###Análise descritiva dos dados
summary(dados5) #médias, medianas, quartis, mínimo e máximo das variáveis numéricas

ggplot(dados5,aes(x=Casos_COVID_100K)) +geom_histogram(col="black", fill="blue")+
  labs(x="Casos de Covid",y = "Frequência")+theme_bw()

table(dados5$Casos_COVID_100K)/nrow(dados5)
ggplot(dados5, aes(y=Casos_COVID_100K))+
  geom_boxplot(notch = T,, varwidth = T ,fill="blue")+
  labs(y = "Casos de COVID")+ theme(axis.ticks.x = element_blank(),
                                    axis.text.x = element_blank(),
                                    panel.background=element_rect(fill="white",colour = "grey50"))
## Baseado no gráfico de Box Plot, percebe-se presença de outiliers
+theme_bw()
moments::skewness(dados5$Casos_COVID_100K)
moments::kurtosis(dados5$Casos_COVID_100K)

ggplot(dados5,aes(x=GINI_2010)) +geom_histogram(col="black", fill="blue")+
  labs(x="Índice de Gini",y = "Frequência")+theme_bw()

ggplot(dados5, aes(y=GINI_2010))+
  geom_boxplot(fill="red")+
  labs(y = "Índice de Gini")+theme(axis.ticks.x = element_blank(),
                                                axis.text.x = element_blank(),
                                                panel.background=element_rect(fill="white",colour = "grey50"))

cor(dados6[,1:3], method="spearman") #correlação das variáveis
corr<-round(cor(dados6[,1:3], method="spearman"),2)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE, legend.title="Correlação",
           ggtheme = ggplot2::theme_bw)
chart.Correlation(dados5[,1:3], histogram = TRUE)
cor.test(dados5$Casos_COVID_100K, dados5$GINI_2010, method = "spearman")
cor.test(dados5$Casos_COVID_100K, dados5$Porcentagem_Candidato, method = "spearman")
### Pelo cálculo acima, percebe-se uma baixa correlação entre as variáveis numéricas preditoras e a váriavel dependente


dados5%>%group_by(Candidato_Eleito)%>%summarise(Mediana_Casos_Covid=median(Casos_COVID_100K))%>%kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18) #mediana de número de casos de COVID por Candidato
##Percebe-se no cálculo acima que em média as cidades que elegeram Bolsonaro apresentaram masi númerod de casos

dados5%>%group_by(Candidato_Eleito)%>%summarise(Mediana_GINI=median(GINI_2010))%>%kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18) #mediana de número de casos de COVID por Candidato

###Análise dos dados separado por candidato eleito
dados_bolsonaro<-dados5%>%filter(Candidato_Eleito=="JAIR BOLSONARO")
dados_haddad<-dados5%>%filter(Candidato_Eleito=="FERNANDO HADDAD")

###Gráficos comparando o comportamento de cada variável preditora com a variável dependente para cada candidato
g1<-ggplot(dados_haddad, aes(x=GINI_2010, y=Casos_COVID_100K))+
  geom_point( colour="red")+
  labs(x = "Índice de Gini",y = "Casos de COVID")+ggtitle("Candidato Haddad")+theme_bw()
g2<-ggplot(dados_bolsonaro, aes(x=GINI_2010, y=Casos_COVID_100K))+
  geom_point( colour="blue")+labs(x = "Índice de Gini",y = "Casos de COVID")+ggtitle("Candidato Bolsonaro")+theme_bw()
grid.arrange(g1,g2, nrow=1)

g3<-ggplot(dados_haddad, aes(x=Porcentagem_Candidato, y=Casos_COVID_100K))+
  geom_point( colour="red")+
  labs(x = "Porcentagem_Eleição",y = "Casos de COVID")+ggtitle("Candidato Haddad")+theme_bw()
g4<-ggplot(dados_bolsonaro, aes(x=Porcentagem_Candidato, y=Casos_COVID_100K))+
  geom_point( colour="blue")+labs(x = "Porcentagem_Eleição",y = "Casos de COVID")+ggtitle("Candidato Bolsonaro")+theme_bw()
grid.arrange(g3,g4, nrow=1)

### Correlação das variáveis separado por candidato eleito
corr_bolsonaro<-cor(dados_bolsonaro[,1:3], method = "spearman")
ggcorrplot(corr_bolsonaro, hc.order = TRUE, type = "lower",
           lab = TRUE, legend.title="Correlação",
           ggtheme = ggplot2::theme_bw, title = "Candidato Bolsonaro")
cor.test(dados_bolsonaro$Casos_COVID_100K, dados_bolsonaro$GINI_2010, method = "spearman")
corr_haddad<-cor(dados_haddad[,1:3], method = "spearman")
ggcorrplot(corr_haddad, hc.order = TRUE, type = "lower",
           lab = TRUE, legend.title="Correlação",
           ggtheme = ggplot2::theme_bw, title = "Candidato Haddad")
cor.test(dados_haddad$Casos_COVID_100K, dados_haddad$GINI_2010, method = "spearman")

##Percebe-se que há uma baixa correlação entre as variáveis
options(scipen = 999)

settings::reset(options)
### Testes Mann-Whitney
wilcox.test(GINI_2010~Candidato_Eleito, dados5)
## Dado que o p-value é mnor que 0.05, podemos afirmar que as medianas do
## índice de Gini para cada grupo são estatísitcamente diferentes entre si

wilcox.test(Casos_COVID_100K~Candidato_Eleito, dados5)
## Dado que o p-value é menor que 0.05, podemos afirmar que as medianas de
## casos de COVID por 100 mil para cada grupo são estatísitcamente diferentes entre si

#Transformando a variável categórica em variável dummy
dados6<-dummy_columns(.data=dados5, select_columns = "Candidato_Eleito",
                      remove_selected_columns = T,
                              remove_most_frequent_dummy = T)
## Candidato 1 = Bolsonaro; Candidato 0 = Haddad
dados6<-dados6%>%rename(Candidato_Eleito="Candidato_Eleito_JAIR BOLSONARO")
str(dados6)
colnames(dados6)

##regressão logistica utilizando a variável Candidato eleito como variável independente
reg_log<-glm(Candidato_Eleito~Casos_COVID_100K+GINI_2010, data=dados6, family = binomial)
summary(reg_log)
predicted<-predict(reg_log,dados5,type="response")
predicted<-as.data.frame(predicted)

## Matrix de Confusão
confusionMatrix(table(predict(reg_log, type = "response") >= 0.5,
                      dados6$Candidato_Eleito == 1)[2:1, 2:1])
## Análise do Qui2
Qui2 <- function(x) {
  maximo <- logLik(x)
  minimo <- logLik(update(x, ~1, trace = F))
  Qui.Quadrado <- -2*(minimo - maximo)
  pvalue <- pchisq(Qui.Quadrado, df = 1, lower.tail = F)
  df <- data.frame()
  df <- cbind.data.frame(Qui.Quadrado, pvalue)
  return(df)
}
Qui2(reg_log)
### o modelo é estatisticamente significativo dado que o p-valor é menor que 0,05


zWald <- (summary(reg_log)$coefficients / 
                            summary(reg_log)$standard.errors)

zWald
round((pnorm(abs(zWald), lower.tail = F) * 2), 4)
summary(reg_log)$sta


### regressão logistica para analisar a chance de cada variável
logitor(Candidato_Eleito~Casos_COVID_100K+GINI_2010, dados6)
logitor(Candidato_Eleito~Casos_COVID_100K+GINI_2010+Porcentagem_Candidato, dados6)


#######################################################
#Análise não supervisionada
dados7<-dados5
dados7$Candidato_Eleito<-as.factor(dados7$Candidato_Eleito)
dados8<-dados7
dados_scale$Candidato_eleito<-dados7$Candidato_Eleito
dados8$Candidato_eleito<-dados7$Candidato_Eleito

## Cirando as distância de Gower
gower_df<-daisy(dados8, metric = "gower")
summary(gower_df)
head(gower_df)

## Clusterização usando o algoritmo PAM - 2 Grupos
pam_clusters2 <- pam(as.matrix(gower_df),
                   diss = TRUE,
                   k = 2)

## Clusterização usando o algoritmo PAM - 3 Grupos
pam_clusters3 <- pam(as.matrix(gower_df),
                     diss = TRUE,
                     k = 3)

## Clusterização usando o algoritmo PAM - 4 Grupos
pam_clusters4 <- pam(as.matrix(gower_df),
                     diss = TRUE,
                     k = 4)

## Clusterização usando o algoritmo PAM - 5 Grupos
pam_clusters5 <- pam(as.matrix(gower_df),
                     diss = TRUE,
                     k = 5)

## Clusterização usando o algoritmo PAM - 6 Grupos
pam_clusters6 <- pam(as.matrix(gower_df),
                     diss = TRUE,
                     k = 6)

## Valores do coeficiente de Silhueta para cada clusterrização
pam_clusters2$silinfo$avg.width
pam_clusters3$silinfo$avg.width
pam_clusters4$silinfo$avg.width
pam_clusters5$silinfo$avg.width
pam_clusters6$silinfo$avg.width

dados_cluster<-dados7

## foi escolhido separar as cidades em 4 clusters, 
## pois agregaria mais na análise dos dados e os grupos ainda se 
## apresentaram distintos entre si, visto que o coeficiente de Silhueta
## foi igual à 0,4.
dados_cluster$Grupos<-pam_clusters4$clustering

## Resumo estatísico de cada grupo
dados_cluster%>%group_by(Grupos)%>%count(Candidato_Eleito)
table(dados7$Candidato_Eleito)
resultado<-dados_cluster%>%group_by(Grupos)%>%do(resumo=summary(.))
resultado$resumo[1]
resultado$resumo[2]
resultado$resumo[3]
resultado$resumo[4]

## gráfico bidimensional dos clusters
tsne_object <- Rtsne(gower_df, is_distance = TRUE)
tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(dados_cluster$Grupos))
ggplot(aes(x = X, y = Y), data = tsne_df) +
  geom_point(aes(color = cluster))+theme_bw()
