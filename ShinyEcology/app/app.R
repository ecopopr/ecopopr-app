library("shiny")
library("ggplot2")
library("deSolve")
library("shinythemes")

# Must be saved in UTF-8 to show Portugese accents correctly.

ui <- fluidPage( #início do código da interface gráfica 
  theme = shinytheme("flatly"),
  titlePanel("Ecologia Interativa"), # título do aplicativo
  h5("Nathália Hohl e Gabriel Santos"),
  h5("Publicado sob a licença Atribuição-NãoComercial-CompartilhaIgual 4.0 Internacional."),
  uiOutput("license"),
  
  br(),
  
  tabsetPanel(type = "tabs",
              tabPanel(title = "Crescimento Geométrico", sidebarLayout(
                sidebarPanel(sliderInput("N0GeoIn", "N0",
                                         min = 1, max = 100, value = 5),
                             numericInput("RGeoIn", "R",
                                          value = 1),
                             sliderInput("tGeoIn", "t",
                                         min = 0, max = 100, value = 50),
                             br(),
                             
                             h4(textOutput("geoOut"))
                ),
                mainPanel(plotOutput("geoGplot"),
                          includeHTML("CrescimentoGeometrico.html")
                )
              )),
  
  tabPanel(title = "Exponencial", sidebarLayout( #início do próximo modelo
    sidebarPanel(sliderInput("N0ExpIn", "N0",
                             min = 1, max = 100, value = 1),
                 numericInput("rExpIn", "r",
                             value = 0.1, step = 0.01),
                 sliderInput("tExpIn", "t",
                             min = 0, max = 100, value = 50),
                 h4(textOutput("expOut")), h4(textOutput("expDoublingTime")
                 )),
    mainPanel(plotOutput("expGPlot"),
              br(),
              includeHTML("CrescimentoExponencial.html"))) 
  ),
  
  tabPanel(title = "Logístico", sidebarLayout(
    sidebarPanel(sliderInput("N0LogIn", "N0",
                             min = 1, max = 100, value = 1),
                 numericInput("rLogIn", "r",
                             min = -3, max = 3, value = 0.2, step = 0.01),
                 sliderInput("tLogIn", "t",
                             min = 0, max = 100, value = 80),
                 sliderInput("kLogIn", "K", min = 0, max = 100, value = 50),
                 br(),
                 h4(textOutput("logOut"))
                 ),
    mainPanel(plotOutput("logGPlot"),
              br(),
              includeHTML("Logistico.html"),)
  )),
  
  tabPanel(title = "Competição", sidebarLayout(
    sidebarPanel(numericInput("N01Comp", "N01",
                             min = 1, max = 3000, value = 300, step = 1),
                 numericInput("N02Comp", "N02",
                                          min = 1, max = 3000, value = 900, step = 1),
                 numericInput("r1Comp", "r1",
                             min = -5, max = 5, value = 0.5, step = 0.01),
                 numericInput("r2Comp", "r2",
                             min = -5, max = 5, value = 0.5, step = 0.01),
                 numericInput("K1Comp", "K1", min = 1, max = 1000, value = 1000, step = 1),
                 numericInput("K2Comp", "K2", min = 0, max = 1000, value = 1000, step = 1),
                 numericInput("alphaComp", "alpha", min = 0, max = 100, value = 2, step = 0.005),
                 numericInput("betaComp", "beta", min = 0, max = 100, value = 2, step = 0.005),
                 sliderInput("tComp", "t", min = 1, max = 1000, value = 100),
                 br(),
                 h4(textOutput("compOut1")),
                 h4(textOutput("compOut2"))
                 
    ),
    mainPanel(plotOutput("compPlot"),
              br(),
              plotOutput("compIsoPlot"),
              br(),
              includeHTML("Competicao.html")
              ),

  )),

  tabPanel(title = "Predação", sidebarLayout(
    sidebarPanel(
      numericInput("P0Pred", "População inicial de Predadores - P0",
                   min = 0, max = 1000, value = 300, step = 1),
      numericInput("V0Pred", "População inicial de Vítimas - V0",
                   min = 0, max = 1000, value = 900, step = 1),
      numericInput("rPred", "Taxa de cresc. intríns. da vítima - r)",
                   min = -3, max = 3, value = 0.3, step = 0.05),
      numericInput("cPred", "Eficência de captura do predador (c ou alpha)",
                   min = 0, max = 3, value = 0.001, step = 0.001),
      numericInput("aPred", "Eficência de conversão do predador (a ou beta)",
                   min = 0, max = 3, value = 0.001, step = 0.001),
      numericInput("mPred", "Taxa de mortalidade do predador (m ou q)",
                   min = 0, max = 3, value = 0.6, step = 0.05),
      numericInput("tPred", "t (de 0 a 1000)", 
                   min = 0, max = 1000, value = 100),
      br(),
      h4(textOutput("predOut1")),
      h4(textOutput("predOut2"))
    ),
    mainPanel(plotOutput("predPlot"),
              br(),
              plotOutput("predIsoPlot"),
              br(),
              includeHTML("Predacao.html"),
              br()
    )
  )
)
)

)


server <- function(input, output){
  
  #link for the license:
  
  url <- a("CC BY-NC-SA 4.0", 
           href="https://creativecommons.org/licenses/by-nc-sa/4.0/deed.pt")
    output$license <- renderUI({
      tagList("Confira as regras de reprodução e modificação:", url)})
  
  #----- GEOMETRIC GROWTH
  geoGdf <- reactive({ # função reativa em que o modelo é implementado
    
    N0 <- input$N0GeoIn # variáveis determinadas pelo input do usuário
    R <- input$RGeoIn
    t <- seq(0, input$tGeoIn, by = 1) 
    
    n <- N0 * (1 + R) ^ t # implementação do modelo
    
    df <- data.frame(t,n) # data frame que será utilizado para gerar o gráfico
  })
  
  #renders the N(t) tag for geometric growth
  output$geoOut <- renderText({
    
    Nt <- tail(geoGdf()$n, n = 1)
    paste0("N(t) = ", round(Nt))
    
  })
  
  
  #renders the GEOMETRIC GROWTH plot
  # output do modelo
  output$geoGplot <- renderPlot({ 
    ggplot(geoGdf(), aes(x = t, y = n)) + # função que utiliza o resultado da
      #função reativa para gerar o gráfico
    geom_point(colour='lightsteelblue4', size = 3) + # início dos parâmetros
                                                     #estéticos do gráfico
    geom_line(linetype = "dashed") +
    ggtitle("Crescimento Geométrico (Em Passos)") +
    xlab("t") +
    ylab("N(t)") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
    theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
            axis.title = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 16, face = "bold")
    )
    
  })
  
  
  
  #-----CONTINUOUS EXPONENTIAL GROWTH
  
  expGdf <- reactive({
    N0 <- input$N0ExpIn
    r <- input$rExpIn
    
    t <- seq(0, input$tExpIn, by = 1)
    Nt <- N0 * exp((r * t))
    
    
    df <- data.frame(t, Nt)
  })

  #renders the N(t) tag for exponential growth
  
  output$expOut <- renderText({
    
    Nt <- tail(expGdf()$Nt, n = 1)
    paste0("N(t) = ", round(Nt))
    
  })
  
  # renders the EXPONENTIAL GROWTH PLOT
  output$expGPlot<-renderPlot({
    ggplot(expGdf(),aes(x = t, y = Nt)) +
    geom_line(color = "lightsteelblue4") +
    geom_point(colour='lightsteelblue4', size = 3) +
    ggtitle("Crescimento Exponencial Contínuo") +
    xlab("t") +
    ylab("N(t)") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
    theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
          axis.title = element_text(size = 20, face = "bold"),
          axis.text = element_text(size = 16, face = "bold")
    )
    })
  output$expDoublingTime <- renderText(c("Tempo de duplicação =", round(log(2)/input$rExpIn , 2)))

  #-----LOGISTIC (DENSITY-DEPENDENT) GROWTH
  logGdf <- reactive({
    
    k <- input$kLogIn
    N0 <- input$N0LogIn
    r <- input$rLogIn
    
    t <- seq(0, input$tLogIn, by = 1)
    Nt <- k / (1 + ((k - N0) / N0) * exp(-r * t))
    
    df <- data.frame(t, Nt)
  })

  
  #renders the N(t) tag for logistic growth
  output$logOut <- renderText({
    
    Nt <- tail(logGdf()$Nt, n = 1)
    paste0("N(t) = ", round(Nt))
    
  })

  #renders the LOGISTIC GROWTH PLOT
  output$logGPlot<-renderPlot({
    ggplot(logGdf(),aes(x=t,y=Nt)) +
      geom_point(colour='lightsteelblue4', size = 3) +
      geom_hline(yintercept = input$kLogIn, linetype = "dashed") +
      annotate("text", x = input$tLogIn, y = (input$kLogIn - input$kLogIn/20 ),
               label = "K", size = 6) + # y moves K to improve legibility
      ggtitle("Crescimento Logístico") +
      xlab("t") +
      ylab("N(t)") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
            axis.title = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 16, face = "bold")
      )
  })  

  
  #--------------- INTERESPECIFIC COMPETITION  (with continuous logistic growth)

  Compdf <- reactive({
    
    # From the ode() help:
    # If func is an R-function, it must be defined as: 
    # func <- function(t, y, parms,...). 
    # t is the current time point in the integration
    # y is the current estimate of the variables in the ODE system.
    
    steps <- seq(0, input$tComp, by = 1) # número passos a serem calculados

    parms <- c(r1 = input$r1Comp, # parâmetros do modelo segundo input
               r2 = input$r2Comp,
               K1 = input$K1Comp,
               beta = input$betaComp, 
               K2 = input$K2Comp, 
               alpha = input$alphaComp)
    
    initialN <- c(input$N01Comp,input$N02Comp) # N inicial de cada espécie
 
    lv.comp2 <- function(t, n, parms) { # função que representa o modelo
      
      with(as.list(parms), {
        
        dn1dt <- r1 * n[1] * ((K1 - n[1] - alpha * n[2]) / K1)
        dn2dt <- r2 * n[2] * ((K2 - n[2] - beta * n[1]) / K2)
        list(c(dn1dt, dn2dt))
        
      })

    }
    out <- ode(y = initialN, times = steps, func = lv.comp2, parms = parms)
    N1 = rep("N1",length(steps))
    N2 = rep("N2",length(steps))
    
    result <- data.frame(out, N1, N2) #N1 and N2 are used by the aes function to color the lines
    print(head(result))
    result
  })
  
  
  
  
  # renders the COMPETITION PLOT
  output$compPlot<-renderPlot({
    ggplot(Compdf(),aes(x=time,y=X1)) + #X1 and X2 come from ode() returned values
      geom_point(aes(x = time, y = X1, colour=N1), size = 2) +
      geom_point(size = 2, aes(x = time, y = X2, colour = N2)) +
      expand_limits(x = 0, y = 0) + #forces the plot origin to be 0,0
      geom_hline(yintercept = input$K1Comp, linetype = "dashed", color = "black") +
      annotate("text", x = input$tLogIn, y = (input$K1Comp - (input$K1Comp)/20 ),
               label = "K1", size = 6, color = "black") +
      geom_hline(yintercept = input$K2Comp, linetype = "dashed", color = "black") +
      annotate("text", x = input$tLogIn, y = (input$K2Comp - (input$K2Comp)/20 ),
               label = "K2", size = 6, color = "black") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      ggtitle("Competição Interespecífica") +
      xlab("t") +
      ylab("N(t)") +
      theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
            axis.title = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 16, face = "bold"),
            legend.text = element_text(size = 16, face = "bold"),
            legend.title = element_blank()
      )
  })
  
  
  #renders the N(t) tag for intraspecific competition
  output$compOut1 <- renderText({
    N1t <- tail(Compdf()$X1, n = 1)
    paste0("N1(t) = ", round(N1t))
  })
  
  output$compOut2 <- renderText({
    N2t <- tail(Compdf()$X2, n = 1)
    paste0("N2(t) = ", round(N2t))
  })
  
 #--------- COMPETITION ISOCLINES
  # Uses the same input from the Competition Plot to generate the correspondent isoclines.

  xy <- reactiveValues() # This is used to create reactive values from anywhere that can be accessed through reactive functions. 

  Iso1data <- reactive({ #species N1 isocline
    
    k1 <- input$K1Comp
    k2 <- input$K2Comp
    alpha <- input$alphaComp
    beta <- input$betaComp
    
    n1 <- seq(0, k1/alpha, by = 1)
    n2 <- seq(0, k1/alpha, by = 1)
    iso1 <- data.frame(n1, n2)

    
    
    iso1$n1 <- eval(expression(( (k1 - alpha * n2 ) )))
    
    
    

    xy$lim <- max(c(k1/input$alphaComp, input$K2Comp, input$K2Comp/input$betaComp,
                    input$K1Comp))
    # gets the max number any isocline will reach (as the greatest value on
    # either x or y axis). We'll use it as the plot area limits.
    
    iso1
  })
  
  Iso2data <- reactive({ #species N2 isocline
    k1 <- input$K1Comp
    k2 <- input$K2Comp
    alpha <- input$alphaComp
    beta <- input$betaComp
    
    n1 <- seq(0, k2/beta, by = 1)
    n2 <- seq(0, k2/beta, by = 1)
    
    iso2 <- data.frame(n1, n2)

    
    iso2$n2 <- eval(expression(k2 - beta * iso2$n1  ))

    iso2
  })
  
  # renders the COMPETITION ISOCLINES plot
  output$compIsoPlot<-renderPlot({
    ggplot(Iso1data(), aes(x = n1, y = n2)) + #N1 iso
      geom_line(size = 0.8, color = "#F8766D") + # hard coded the default ggplot color, since they're plotted from different dataframes
      geom_line(data = Iso2data(), aes(x = n1, y = n2), #N2 iso
                size = 0.8, color = "#00BFC4") +
      expand_limits(x = 0, y = 0) + #forces the plot origin to be 0,0
      geom_point(x = input$N01Comp, y = input$N02Comp, size = 3) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      ggtitle("Isoclinas") +
      xlab("N1") +
      ylab("N2") +
      xlim(0,xy$lim) +
      ylim(0, xy$lim) +
      theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
            axis.title = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 16, face = "bold"),
            legend.text = element_text(size = 16, face = "bold"),
            legend.title = element_blank()
      )
      }) 

  



  
  
  # PREDATION x PREY MODEL ----------------------
  
  Pred.df <- reactive({
    
    steps <- seq(0, input$tPred, by = 1) #used later as the "times" for ode function and to create the color columns through repetition
    
    parms <- c(r = input$rPred, V = input$V0Pred, c = input$cPred,
               m = input$mPred, P = input$P0Pred, a = input$aPred)
    initialN <- c(input$V0Pred,input$P0Pred)
    
    # See comments on Competition for help.
    lv.pred <- function(t, n, parms) {
      with(as.list(parms), {
        dV.dt <- r * n[1] - c * n[1] * n[2]
    
        
        dP.dt <- a * n[1] * n[2] - m * n[2]
        
        list(c(dV.dt, dP.dt))
      })
    }
    
    # General Solver of Ordinary Differential Equations
    # ode(y, times, func, parms, [...]) See Competition for more.
    
    out <- ode(y = initialN, times = steps, func = lv.pred, parms = parms)
    
    V = rep("V",length(steps)) #used in plot legend and colors
    P = rep("P",length(steps))
    
    result <- data.frame(out, V, P) #V and P are used by the aes function to color 
    #the lines.
    
    #print(head(result, 3))
    #print(tail(result, 3))
    result
  })
  
  output$predOut1 <- renderText({
    Vt <- tail(Pred.df()$X1, n = 1)
    paste0("V(t) = ", round(Vt))
  })

  output$predOut2 <- renderText({
    Pt <- tail(Pred.df()$X2, n = 1)
    paste0("P(t) = ", round(Pt))
  })
  
  
  predIsodf <- reactive({
    
    m <- input$mPred
    a <- input$aPred
    c <- input$cPred
    
    r <- input$rPred
    c <- input$cPred
    
    isoP <- r / c

    isoV <- m / a
    
    df <- data.frame(isoV, isoP)
    df
  })
  
  # ------- Predation N(t) plot
  
  output$predPlot <- renderPlot({
    ggplot(data = Pred.df()) +
      geom_point(aes(x = time, y = X1, color = V), size = 2) +
      geom_line(aes(x = time, y = X1, color = V), size = 0.5) +
      geom_point(aes(x = time, y = X2, color = P), size = 2) +
      geom_line(aes(x = time, y = X2, color = P), size = 0.5) +
      ggtitle("Predador x Presa") +
      xlab("t") +
      ylab("Tamanho das Populações de P e V\n") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
            axis.title = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 16, face = "bold"),
            legend.text = element_text(size = 16, face = "bold"),
            legend.title = element_blank()
            )
  })
  
  # Phase Space Plot (Predation x Prey) with isoclines
  output$predIsoPlot <- renderPlot({
    ggplot() +
      geom_vline(data = predIsodf(), aes(xintercept = isoV)) +
      geom_hline(data = predIsodf(), aes(yintercept = isoP)) +
      geom_point(data = Pred.df(), aes(x = X1, y = X2),
                 color = c("lightsteelblue4"), alpha = 0.4, size = 3) +
      geom_path(data = Pred.df(), aes(x = X1, y = X2),
                color = "lightsteelblue4", alpha = 0.6) +
      ggtitle("Espaço de Fase") +
      xlab("Pop. de Presas (V)") +
      ylab("Pop. de Predadores (P)") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
            axis.title = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 16, face = "bold"),
            legend.text = element_text(size = 16, face = "bold"),
            legend.title = element_blank()
      )
    
    
  })


  
} #end of server, don't exclude (again)

shinyApp(ui = ui, server = server)

# rtsudio Version 1.1.442
# R version 3.4.3 (2017-11-30) Kite-Eating Tree 