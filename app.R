library(shiny)
library(shinyjs)
library(dplyr)
library(lsa)
library(fst)
library(shinythemes)


## On à selectionné en amont 10.000 mots parmis les milliers présents dans le jeu de données originel.
# mots <- read.table('mots.txt')
# rownames(mots) <- mots[,1]
# mots <- mots[,-1]
# dim(mots)

## Pour le bon fonctionnement de la fonction, les données des dimensions de chaque mots doivent être en ligne.
## On utilise donc la transporsée.
# mots <- t(mots)

## on applique la fonction cosine pour calculer la "distance" entre les mots.
# mots <- cosine(mots[,1:dim(mots)[2]])
# mots <- as.data.frame(mots)

## On peut aussi implémenter nous même la fonction cosine, il suffit de recopier la formule mathématique issue de Wikipédia:

## cosine <- function(x) {
##   y <- t(x) %*% x
##   res <- (y / (sqrt(diag(y)) %*% t(sqrt(diag(y)))))
##   return(res)
## }


## On récupère la matrice cosine (implémentation longue) et on la transforme en format .fst pour charger beaucoup
## plus rapidement les données dans le cadre de l'application Shiny.
#write.fst(mots,"mots.fst")
options(max.print = 10000)
options(width = 500)
mots <- read_fst("mots.fst")
rownames(mots) <- colnames(mots)


`%notin%` <- Negate(`%in%`)

y <- as.vector(colnames(mots))

## tirage
tirage <- y[sample(1:dim(mots)[2],size=1)]

res <- mots[which(colnames(mots)==tirage)]
res %>% arrange(desc(res[,1])) -> res

## tableau final
final <- data.frame("Mot" = rownames(res)[1:1000],
                    "Distance" = res[1:1000,],
                    "Score" = seq(1000,1,-1))


g <- data.frame(" "," "," "," ")
colnames(g) <- c("Mot","Score","Commentaire","Progression")
rownames(g) <- "         "

d <- data.frame(" ", " "," "," ")
d[1,] <- c(" "," "," "," ")
d[2,] <- c("Mot","Score","Commentaire","Progression")
d[3,] <- c(" "," "," "," ")
d[4,] <- c("———————————————","——————————————","——————————————————","—————————————————————")
rownames(d) <- c(" ","  ","   ","          ")
colnames(d) <- c(" "," "," "," ")



Play <- function(x){
  x <- tolower(x)
  if(x %in% c(""," ","  ","   ","    ","     ")){
    d
  }else{
    if(x ==  final[2,1]){
      if(x %in% g[,1]){
        d[4,] <- g[g[,1]==x,]
        rownames(d)[4] <- paste0("n°",rownames(g)[g[,1]==x])
        d <<- d
        d
      }else{
        tt <- c(x, final[final==x,3], "Ça Brûle !"," ■■■■■■■■■■■■■■■■■■  ")
        g <- rbind(g,tt)
        rownames(g)[nrow(g)] <- as.numeric(rownames(g)[nrow(g)])-1
        g %>% arrange(desc(as.numeric(Score))) -> g
        d <- rbind(c(" "," "," "," "),c("Mot","Score","Commentaire","Progression"),c(" "," "," "," "),c(x, final[final==x,3], "Ça Brûle !"," ■■■■■■■■■■■■■■■■■■  "),c("———————————————","——————————————","——————————————————","—————————————————————"),g[1:nrow(g),])
        rownames(d) <- c("          ","        ","       ",paste0("n°",rownames(g)[g[,1]==x]),"   ",rownames(g[1:nrow(g),]))
        colnames(d) <- c(" ","  ","   ","    ")
        g <<- g
        b <<- b + 1
        d <<- d
        d
        
      }
    }else{
      if(x %in% final[2:11,1]){
        if(x %in% g[,1]){
          d[4,] <- g[g[,1]==x,]
          rownames(d)[4] <- paste0("n°",rownames(g)[g[,1]==x])
          d <<- d
          d
        }else{
          tt <- c(x, final[final==x,3], "C'est très chaud !"," ■■■■■■■■■■■■■■■■■■  ")
          g <- rbind(g,tt)
          rownames(g)[nrow(g)] <- as.numeric(rownames(g)[nrow(g)])-1
          g %>% arrange(desc(as.numeric(Score))) -> g
          d <- rbind(c(" "," "," "," "),c("Mot","Score","Commentaire","Progression"),c(" "," "," "," "),c(x, final[final==x,3], "C'est très chaud !"," ■■■■■■■■■■■■■■■■■■  "),c("———————————————","——————————————","——————————————————","—————————————————————"),g[1:nrow(g),])
          rownames(d) <- c("          ","        ","       ",paste0("n°",rownames(g)[g[,1]==x]),"   ",rownames(g[1:nrow(g),]))
          colnames(d) <- c(" ","  ","   ","    ")
          g <<- g
          b <<- b + 1
          d <<- d
          d
        }
        
      }else{
        if(x %in% final[12:101,1]){
          if(x %in% g[,1]){
            d[4,] <- g[g[,1]==x,]
            rownames(d)[4] <- paste0("n°",rownames(g)[g[,1]==x])
            d <<- d
            d
          }else{
            if(x %in% final[12:21,1]){
              tt <- c(x, final[final==x,3], "C'est Chaud !"," ■■■■■■■■■■■■■■■■    ")
              g <- rbind(g,tt)
              rownames(g)[nrow(g)] <- as.numeric(rownames(g)[nrow(g)])-1
              g %>% arrange(desc(as.numeric(Score))) -> g
              d <- rbind(c(" "," "," "," "),c("Mot","Score","Commentaire","Progression"),c(" "," "," "," "),c(x, final[final==x,3], "C'est Chaud !"," ■■■■■■■■■■■■■■■■    "),c("———————————————","——————————————","——————————————————","—————————————————————"),g[1:nrow(g),])
              rownames(d) <- c("          ","        ","       ",paste0("n°",rownames(g)[g[,1]==x]),"   ",rownames(g[1:nrow(g),]))
              colnames(d) <- c(" ","  ","   ","    ")
              g <<- g
              b <<- b + 1
              d <<- d
              d
            }else{
              if(x %in% final[22:31,1]){
                tt <- c(x, final[final==x,3], "Tiède !"," ■■■■■■■■■■■■■■      ")
                g <- rbind(g,tt)
                rownames(g)[nrow(g)] <- as.numeric(rownames(g)[nrow(g)])-1
                g %>% arrange(desc(as.numeric(Score))) -> g
                d <- rbind(c(" "," "," "," "),c("Mot","Score","Commentaire","Progression"),c(" "," "," "," "),c(x, final[final==x,3], "Tiède !"," ■■■■■■■■■■■■■■      "),c("———————————————","——————————————","——————————————————","—————————————————————"),g[1:nrow(g),])
                rownames(d) <- c("          ","        ","       ",paste0("n°",rownames(g)[g[,1]==x]),"   ",rownames(g[1:nrow(g),]))
                colnames(d) <- c(" ","  ","   ","    ")
                g <<- g
                b <<- b + 1
                d <<- d
                d
              }else{
                if(x %in% final[32:41,1]){
                  tt <- c(x, final[final==x,3], "Tiède !"," ■■■■■■■■■■■■        ")
                  g <- rbind(g,tt)
                  rownames(g)[nrow(g)] <- as.numeric(rownames(g)[nrow(g)])-1
                  g %>% arrange(desc(as.numeric(Score))) -> g
                  d <- rbind(c(" "," "," "," "),c("Mot","Score","Commentaire","Progression"),c(" "," "," "," "),c(x, final[final==x,3], "Tiède !"," ■■■■■■■■■■■■        "),c("———————————————","——————————————","——————————————————","—————————————————————"),g[1:nrow(g),])
                  rownames(d) <- c("          ","        ","       ",paste0("n°",rownames(g)[g[,1]==x]),"   ",rownames(g[1:nrow(g),]))
                  colnames(d) <- c(" ","  ","   ","    ")
                  g <<- g
                  b <<- b + 1
                  d <<- d
                  d
                }else{
                  if(x %in% final[42:51,1]){
                    tt <- c(x, final[final==x,3], "Tiède !"," ■■■■■■■■■■          ")
                    g <- rbind(g,tt)
                    rownames(g)[nrow(g)] <- as.numeric(rownames(g)[nrow(g)])-1
                    g %>% arrange(desc(as.numeric(Score))) -> g
                    d <- rbind(c(" "," "," "," "),c("Mot","Score","Commentaire","Progression"),c(" "," "," "," "),c(x, final[final==x,3], "Tiède !"," ■■■■■■■■■■          "),c("———————————————","——————————————","——————————————————","—————————————————————"),g[1:nrow(g),])
                    rownames(d) <- c("          ","        ","       ",paste0("n°",rownames(g)[g[,1]==x]),"   ",rownames(g[1:nrow(g),]))
                    colnames(d) <- c(" ","  ","   ","    ")
                    g <<- g
                    b <<- b + 1
                    d <<- d
                    d
                  }else{
                    if(x %in% final[52:61,1]){
                      tt <- c(x, final[final==x,3], "Tiède !"," ■■■■■■■■            ")
                      g <- rbind(g,tt)
                      rownames(g)[nrow(g)] <- as.numeric(rownames(g)[nrow(g)])-1
                      g %>% arrange(desc(as.numeric(Score))) -> g
                      d <- rbind(c(" "," "," "," "),c("Mot","Score","Commentaire","Progression"),c(" "," "," "," "),c(x, final[final==x,3], "Tiède !"," ■■■■■■■■            "),c("———————————————","——————————————","——————————————————","—————————————————————"),g[1:nrow(g),])
                      rownames(d) <- c("          ","        ","       ",paste0("n°",rownames(g)[g[,1]==x]),"   ",rownames(g[1:nrow(g),]))
                      colnames(d) <- c(" ","  ","   ","    ")
                      g <<- g
                      b <<- b + 1
                      d <<- d
                      d
                    }else{
                      if(x %in% final[62:71,1]){
                        tt <- c(x, final[final==x,3], "Tiède !"," ■■■■■■              ")
                        g <- rbind(g,tt)
                        rownames(g)[nrow(g)] <- as.numeric(rownames(g)[nrow(g)])-1
                        g %>% arrange(desc(as.numeric(Score))) -> g
                        d <- rbind(c(" "," "," "," "),c("Mot","Score","Commentaire","Progression"),c(" "," "," "," "),c(x, final[final==x,3], "Tiède !"," ■■■■■■              "),c("———————————————","——————————————","——————————————————","—————————————————————"),g[1:nrow(g),])
                        rownames(d) <- c("          ","        ","       ",paste0("n°",rownames(g)[g[,1]==x]),"   ",rownames(g[1:nrow(g),]))
                        colnames(d) <- c(" ","  ","   ","    ")
                        g <<- g
                        b <<- b + 1
                        d <<- d
                        d
                      }else{
                        if(x %in% final[72:81,1]){
                          tt <- c(x, final[final==x,3], "Tiède !"," ■■■■                ")
                          g <- rbind(g,tt)
                          rownames(g)[nrow(g)] <- as.numeric(rownames(g)[nrow(g)])-1
                          g %>% arrange(desc(as.numeric(Score))) -> g
                          d <- rbind(c(" "," "," "," "),c("Mot","Score","Commentaire","Progression"),c(" "," "," "," "),c(x, final[final==x,3], "Tiède !"," ■■■■                "),c("———————————————","——————————————","——————————————————","—————————————————————"),g[1:nrow(g),])
                          rownames(d) <- c("          ","        ","       ",paste0("n°",rownames(g)[g[,1]==x]),"   ",rownames(g[1:nrow(g),]))
                          colnames(d) <- c(" ","  ","   ","    ")
                          g <<- g
                          b <<- b + 1
                          d <<- d
                          d
                        }else{
                          if(x %in% final[82:91,1]){
                            tt <- c(x, final[final==x,3], "Tiède !"," ■■                  ")
                            g <- rbind(g,tt)
                            rownames(g)[nrow(g)] <- as.numeric(rownames(g)[nrow(g)])-1
                            g %>% arrange(desc(as.numeric(Score))) -> g
                            d <- rbind(c(" "," "," "," "),c("Mot","Score","Commentaire","Progression"),c(" "," "," "," "),c(x, final[final==x,3], "Tiède !"," ■■                  "),c("———————————————","——————————————","——————————————————","—————————————————————"),g[1:nrow(g),])
                            rownames(d) <- c("          ","        ","       ",paste0("n°",rownames(g)[g[,1]==x]),"   ",rownames(g[1:nrow(g),]))
                            colnames(d) <- c(" ","  ","   ","    ")
                            g <<- g
                            b <<- b + 1
                            d <<- d
                            d
                          }else{
                            tt <- c(x, final[final==x,3], "Tiède !"," ")
                            g <- rbind(g,tt)
                            rownames(g)[nrow(g)] <- as.numeric(rownames(g)[nrow(g)])-1
                            g %>% arrange(desc(as.numeric(Score))) -> g
                            d <- rbind(c(" "," "," "," "),c("Mot","Score","Commentaire","Progression"),c(" "," "," "," "),c(x, final[final==x,3], "Tiède !"," "),c("———————————————","——————————————","——————————————————","—————————————————————"),g[1:nrow(g),])
                            rownames(d) <- c("          ","        ","       ",paste0("n°",rownames(g)[g[,1]==x]),"   ",rownames(g[1:nrow(g),]))
                            colnames(d) <- c(" ","  ","   ","    ")
                            g <<- g
                            b <<- b + 1
                            d <<- d
                            d
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }else{
          if(x %in% final[101:1000,1]){
            if(x %in% g[,1]){
              d[4,] <- g[g[,1]==x,]
              rownames(d)[4] <- paste0("n°",rownames(g)[g[,1]==x])
              d <<- d
              d
            }else{
              tt <- c(x, final[final==x,3], "Froid !"," ")
              g <- rbind(g,tt)
              rownames(g)[nrow(g)] <- as.numeric(rownames(g)[nrow(g)])-1
              g %>% arrange(desc(as.numeric(Score))) -> g
              d <- rbind(c(" "," "," "," "),c("Mot","Score","Commentaire","Progression"),c(" "," "," "," "),c(x, final[final==x,3], "Froid !"," "),c("———————————————","——————————————","——————————————————","—————————————————————"),g[1:nrow(g),])
              rownames(d) <- c("          ","        ","       ",paste0("n°",rownames(g)[g[,1]==x]),"   ",rownames(g[1:nrow(g),]))
              colnames(d) <- c(" ","  ","   ","    ")
              g <<- g
              b <<- b + 1
              d <<- d
              d
            }
          }else{
            if(x %notin% final[1:nrow(final),1]){
              if(x %notin% y){
                
                colnames(d) <- c(x, "n'est pas dans", "mon dictionnaire  "," ")
                d
              }else{
                if(x %in% g[,1]){
                  d[4,] <- g[g[,1]==x,]
                  rownames(d)[4] <- paste0("n°",rownames(g)[g[,1]==x])
                  d <<- d
                  d
                }else{
                  tt <- c(x, 0, "Glacé !"," ")
                  g <- rbind(g,tt)
                  rownames(g)[nrow(g)] <- as.numeric(rownames(g)[nrow(g)])-1
                  g %>% arrange(desc(as.numeric(Score))) -> g
                  d <- rbind(c(" "," "," "," "),c("Mot","Score","Commentaire","Progression"),c(" "," "," "," "),c(x, 0, "Glacé !"," "),c("———————————————","——————————————","——————————————————","—————————————————————"),g[1:nrow(g),])
                  rownames(d) <- c("          ","        ","       ",paste0("n°",rownames(g)[g[,1]==x]),"   ",rownames(g[1:nrow(g),]))
                  colnames(d) <- c(" ","  ","   ","    ")
                  g <<- g
                  b <<- b + 1
                  d <<- d
                  d
                }
              }
            }else{
              if(x == tirage){
                if(x %in% g[,1]){
                  d[4,] <- g[g[,1]==x,]
                  rownames(d)[4] <- paste0("n°",rownames(g)[g[,1]==x])
                  d <<- d
                  d
                }else{
                  tt <- c(x, 1000, "C'est gagné !"," ■■■■■■■■■■■■■■■■■■■■")
                  g <- rbind(g,tt)
                  rownames(g)[nrow(g)] <- as.numeric(rownames(g)[nrow(g)])-1
                  d <- rbind(c(" "," "," "," "),c("Mot","Score","Commentaire","Progression"),c(" "," "," "," "),g[nrow(g),],g[-nrow(g),])
                  rownames(d) <- c("     ","    ","            ",rownames(g)[nrow(g)],rownames(g)[1:(nrow(g)-1)])
                  colnames(d) <- c("Fin de partie !","( ͡•ʖ͜ ͡•)"," "," ")
                  d <<- d
                  d
                }
              }
            }
          }
        }
      }
    }
  }
}

NouvellePartie <- function(){
  k <<- 96
  i <<- 1
  b <<- 1
  tirage <- y[sample(1:dim(mots)[2],size=1)]
  tirage <<- tirage
  res <- mots[which(colnames(mots)==tirage)]
  res %>% arrange(desc(res[,1])) -> res
  final <- data.frame("Mot" = rownames(res)[1:1000],
                      "Distance" = res[1:1000,],
                      "Score" = seq(1000,1,-1))
  
  final <<- final
  g <- data.frame(" "," "," "," ")
  colnames(g) <- c("Mot","Score","Commentaire","Progression")
  rownames(g) <- " "
  g <<- g
  d <- data.frame(" ", " "," "," ")
  d[1,] <- c(" "," "," "," ")
  d[2,] <- c("Mot","Score","Commentaire","Progression")
  d[3,] <- c(" "," "," "," ")
  d[4,] <- c("———————————————","——————————————","——————————————————","—————————————————————")
  rownames(d) <- c(" ","  ","   ","          ")
  colnames(d) <- c(" "," "," "," ")
  d <<- d
}


i <- 1
k <- 96
b <- 1
Indice <- function(){
  if(i <= 10 && k >= 5 && b >= 10){
    colnames(d) <- c(paste("Indice",paste0("n°",i),":"),paste0(final[k,][,1],paste0(rep(" ",nchar("——————————————")-nchar(final[k,][,1])),collapse = "")),"   ","    ")
    i <<- i + 1
    k <<- k - 10
    b <<- 1
    d
  }else{
    d
  }
}








ui <- navbarPage("Motamo",theme = shinytheme("flatly"), # flatly, ##paper
                 tabPanel("Jouer",
                          headerPanel(""),
                          sidebarPanel(
                            useShinyjs(),
                            img(src="motamo.png",width="260",height="110"),
                            helpText(""),
                            helpText(""),
                            helpText(""),
                            
                            
                            fluidRow(
                              
                              column(10,
                                     br(),
                                     tags$h4("Options"),
                                     br(),
                                     tags$h6("Débloquer un indice"),
                                     tags$head(tags$style(HTML("#submit4{ background-color: rgb(44,62,80); color: white}"))),
                                     disabled(actionButton("submit4", label = "Indice")),
                                     br()),
                              
                              
                              column(10,
                                     tags$h6("Afficher les résultats"),
                                     tags$head(tags$style(HTML("#submit5{ background-color: rgb(44,62,80); color: white}"))),
                                     disabled(actionButton("submit5", label = "Voir les résultats")),
                                     br()),
                              
                              column(12,
                                     tags$h6("Abandonner et afficher le tableau final"),
                                     tags$head(tags$style(HTML("#submit3{ background-color: rgb(44,62,80); color: white}"))),
                                     actionButton("submit3", label = "J'abandonne"),
                                     br()),
                              
                              column(10,
                                     tags$h6("Lancer une nouvelle partie"),
                                     tags$head(tags$style(HTML("#submit2{ background-color: rgb(44,62,80); color: white}"))),
                                     actionButton("submit2", label = "Nouvelle partie"))
                              
                              
                              
                              
                            )
                          , width = 3),
                          
                          mainPanel(
                            
                            textInput("user_text", label = " ", placeholder = "Entrer un mot :"),
                            tags$head(tags$style(HTML("#submit{ background-color: rgb(44,62,80); color: white}"))),
                            actionButton("submit", label = "Envoyer"),
                            tags$head(tags$style(HTML("#text{ color: rgb(38,65,71)}"))),
                            verbatimTextOutput("text")
                          , width = 7)
                 ),
                 tabPanel("Aide",
                          headerPanel(""),
                          sidebarPanel(
                            useShinyjs(),
                            img(src="motamo.png",width="260",height="110"),
                            helpText(""),
                            helpText(""),
                            helpText(""),
                            
                            
                            fluidRow(
                              
                              column(10,
                                     br(),
                                     tags$h4("Options"),
                                     br(),
                                     tags$h6("Débloquer un indice"),
                                     tags$head(tags$style(HTML("#submit4{ background-color: rgb(44,62,80); color: white}"))),
                                     disabled(actionButton("submit4", label = "Indice")),
                                     br()),
                              
                              
                              column(10,
                                     tags$h6("Afficher les résultats"),
                                     tags$head(tags$style(HTML("#submit5{ background-color: rgb(44,62,80); color: white}"))),
                                     disabled(actionButton("submit5", label = "Voir les résultats")),
                                     br()),
                              
                              column(12,
                                     tags$h6("Abandonner et afficher le tableau final"),
                                     tags$head(tags$style(HTML("#submit3{ background-color: rgb(44,62,80); color: white}"))),
                                     disabled(actionButton("submit3", label = "J'abandonne")),
                                     br()),
                              
                              column(10,
                                     tags$h6("Lancer une nouvelle partie"),
                                     tags$head(tags$style(HTML("#submit2{ background-color: rgb(44,62,80); color: white}"))),
                                     disabled(actionButton("submit2", label = "Nouvelle partie")))
                              
                              
                              
                              
                            )
                            , width = 3),
                          
                          mainPanel(
                            
                            tags$head(tags$style(HTML("#aide{ color: rgb(38,65,71)}"))),
                            verbatimTextOutput("aide")
                            
                            , width = 9)
                 ),
                 tabPanel("Informations",
                          headerPanel(""),
                          sidebarPanel(
                            useShinyjs(),
                            img(src="motamo.png",width="260",height="110"),
                            helpText(""),
                            helpText(""),
                            helpText(""),
                            
                            
                            fluidRow(
                              
                              column(10,
                                     br(),
                                     tags$h4("Options"),
                                     br(),
                                     tags$h6("Débloquer un indice"),
                                     tags$head(tags$style(HTML("#submit4{ background-color: rgb(44,62,80); color: white}"))),
                                     disabled(actionButton("submit4", label = "Indice")),
                                     br()),
                              
                              
                              column(10,
                                     tags$h6("Afficher les résultats"),
                                     tags$head(tags$style(HTML("#submit5{ background-color: rgb(44,62,80); color: white}"))),
                                     disabled(actionButton("submit5", label = "Voir les résultats")),
                                     br()),
                              
                              column(12,
                                     tags$h6("Abandonner et afficher le tableau final"),
                                     tags$head(tags$style(HTML("#submit3{ background-color: rgb(44,62,80); color: white}"))),
                                     disabled(actionButton("submit3", label = "J'abandonne")),
                                     br()),
                              
                              column(10,
                                     tags$h6("Lancer une nouvelle partie"),
                                     tags$head(tags$style(HTML("#submit2{ background-color: rgb(44,62,80); color: white}"))),
                                     disabled(actionButton("submit2", label = "Nouvelle partie")))
                              
                              
                              
                              
                            )
                            , width = 3),
                          
                          mainPanel(
                            
                            tags$head(tags$style(HTML("#info{ color: rgb(38,65,71)}"))),
                            verbatimTextOutput("info")
                            
                            , width = 9)
                 )
)

server <- function(input, output, session) {
  
  
  
  
  
  Jouer <- eventReactive( input$submit, {
    x <- input$user_text
    if(b >= 10){
      enable("submit4")
    }
    if(x==tirage){
      enable("submit5")
      disable("submit")
      disable("submit3")
      disable("submit4")
    }
    Play(x)
  })
  
  
  NouvelleP <- eventReactive(input$submit2,{
    disable("submit5")
    enable("submit")
    enable("submit3")
    NouvellePartie()
    z <- data.frame(" ")
    rownames(z) <- " "
    colnames(z) <- "Une nouvelle partie a démarré, écrivez un mot !"
    print(z,quote = FALSE)
  })
  
  
  Abandon <- eventReactive(input$submit3, {
    disable("submit")
    final[1:1000,]
  })
  
  VoirResultats <- eventReactive(input$submit5, {
    final[1:1000,]
  })
  
  Ind <- eventReactive(input$submit4, {
    disable("submit4")
    Indice()
  })
  
  
  
  w <- data.frame(" ")
  rownames(w) <- " "
  colnames(w) <- "Écrivez un mot !"
  
  
  texty <- reactiveVal(print(w,quote = FALSE))
  
  observeEvent(input$submit, {
    
    texty(Jouer())
    updateTextInput(session, "user_text",  label = " ", value = "")
  })
  
  
  observeEvent(input$submit2, {
    
    texty(NouvelleP())
  })
  
  observeEvent(input$submit3, {
    
    texty(Abandon())
  })
  
  observeEvent(input$submit4, {
    
    texty(Ind())
  })
  
  observeEvent(input$submit5, {
    
    texty(VoirResultats())
  })
  
  output$text <- renderPrint({
    texty()
  })
  
  output$aide <- renderPrint({
    cat("\n","But du jeu",
        "\n",
        "\n",
        "Le but du jeu est de trouver le mot secret en essayant de s’en approcher le plus possible contextuellement.", "\n", 
        "À chaque partie, un tableau final contenant le mot secret et les 999 mots les plus proches est généré,","\n",
        "le but est de trouver un maximum de mots proches pour deviner le mot secret.","\n",
        "Si votre mot se trouve dans les 999 mots les plus proches, un indice de progression de 1 à 1000 apparaîtra,","\n",
        "avec le 1000ème qui correspond au mot secret.","\n",
        "Une fois le mot secret trouvé, vous pourrez visualiser le tableau final des résultats.","\n",
        "Le jeu ne prend pas en compte les majuscules.",
        "\n",
        "\n",
        "\n",
        "Indices",
        "\n",
        "\n",
        "Pour vous aider dans votre tâche, un système d'indices à été mis en place.","\n",
        "Un indice se débloquera pour la première fois après 10 mots entrés puis tous les 10 mots à partir du moment","\n",
        "où le bouton Indice est actionné.","\n",
        "Au total, il y a 10 indices par partie et chacun d'entre eux est un mot contenu dans le tableau final.","\n",
        "Chaque indice est de plus en plus proche du mot secret, le premier étant le 95ème et le dernier étant le 5ème.",
        "\n",
        "\n",
        "\n",
        "Abandon et nouvelle partie",
        "\n",
        "\n",
        "Vous avez la possibilité d'abandonner, dans ce cas le tableau final contenant le mot secret ainsi que les 999","\n",
        "mots les plus proches apparaîtra.","\n",
        "Vous avez aussi la possibilité de lancer une nouvelle partie, un nouveau tirage sera alors effectuer et vous","\n",
        "pourrez rejouer et deviner un nouveau mot secret.","\n"
    )
    
  })
  
  output$info <- renderPrint({
    cat("\n","Ce jeu à été implémenté par Idir SADAOUI et Hamady CISSÉ dans le cadre d'un projet de master encadré","\n",
        "par Antoine CHAMBAZ à l'Université de Paris Cité.","\n",
        "Les données ont été extraites du site de Jean-Philippe FAUCONNIER, nous les avons nettoyé et avons utilisé","\n",
        "la distance cosine pour calculer la 'distance' entre les mots.","\n",
        "Notre jeu de données contient 10.000 mots.",
        "\n",
        "\n",
        "\n",
        "Informations et liens utiles :",
        "\n",
        "\n",
        "Site de Jean-Philippe FAUCONNIER : https://fauconnier.github.io/#data","\n",
        "Données utilisées : frWac_non_lem_no_postag_no_phrase_200_cbow_cut100.bin","\n",
        "Github Idir SADAOUI : https://github.com/idirsadaoui","\n",
        "Distance Cosine : https://en.wikipedia.org/wiki/Cosine_similarity","\n",
        "Fonction Cosine utilisé sur R : cosine() du package lsa",
        "\n",
        "\n",
        "\n",
        "2022")
  })
}

shinyApp(ui = ui, server = server)
