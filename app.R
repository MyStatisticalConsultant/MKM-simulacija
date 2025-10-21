library(shiny)
library(copula)
library(mixtools)
library(ggplot2)
library(plotly)
library(pROC)
library(reshape2)
library(viridis)
library(markdown)
library(htmlwidgets)
library(future)
library(future.apply)
library(parallel)
library(mvtnorm)

# ---------- pomoćne ----------
simBetaCopula <- function(n, a_t, b_t, a_v, b_v, rho) {
  gaussCop <- normalCopula(rho, dim = 2)
  u <- rCopula(n, gaussCop)
  t <- qbeta(u[, 1], a_t, b_t)
  v <- qbeta(u[, 2], a_v, b_v)
  data.frame(t = t, v = v)
}

calc_entropy <- function(post) {
  ent <- -rowSums(post * log(post + 1e-15))
  mean(ent)
}

safe_logit <- function(p) {
  eps <- 1e-10
  p <- pmax(pmin(p, 1 - eps), eps)
  log(p/(1-p))
}

named_sigma <- function(S) {
  colnames(S) <- rownames(S) <- c("logit_t","logit_v")
  S
}

ilogit <- function(x) 1/(1 + exp(-x))

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("Interaktivni MKM model (Mebane 3 klase) - Izborna forenzika"),
  sidebarLayout(
    sidebarPanel(
      tags$h4("Simulacija i kontrola"),
      numericInput("seed", "Seed za reproduktivnost:", value = 1234),
      numericInput("n1", "Broj tačaka Regular:", 150, min = 10, max = 5000),
      numericInput("n2", "Broj tačaka Incremental:", 100, min = 10, max = 5000),
      numericInput("n3", "Broj tačaka Extreme:", 80,  min = 10, max = 5000),
      tags$hr(),
      sliderInput("alpha1", "α_t Regular:", 1, 10, 5, 0.1),
      sliderInput("beta1",  "β_t Regular:", 1, 10, 3, 0.1),
      sliderInput("alpha2", "α_t Incremental:", 1, 10, 4, 0.1),
      sliderInput("beta2",  "β_t Incremental:", 1, 10, 5, 0.1),
      sliderInput("alpha3", "α_t Extreme:", 1, 10, 2, 0.1),
      sliderInput("beta3",  "β_t Extreme:", 1, 10, 7, 0.1),
      sliderInput("rho1", "Korelacija Regular:",     0, 0.9, 0.4, 0.05),
      sliderInput("rho2", "Korelacija Incremental:", 0, 0.9, 0.6, 0.05),
      sliderInput("rho3", "Korelacija Extreme:",     0, 0.9, 0.8, 0.05),
      tags$hr(),
      actionButton("simulate", "Pokreni simulaciju", class = "btn-primary"),
      actionButton("cancel",   "Zaustavi",           class = "btn-warning"),
      actionButton("reset",    "Reset",              class = "btn-danger"),
      
      tags$hr(),
      downloadButton("dl_raw_csv", "Preuzmi sirove podatke (CSV)"),
      downloadButton("dl_params_csv", "Preuzmi parametre (CSV)"),
      tags$hr(),
      
      tags$hr(),
      tags$h5("BIC/ICL podešavanje"),
      numericInput("kmax", "Max K (BIC/ICL):", value = 3, min = 1, max = 8, step = 1),
      actionButton("run_bic", "Izračunaj BIC/ICL", class = "btn-secondary"),
      tags$hr(),
      downloadButton("dl_fit_rds", "Sačuvaj simulaciju (RDS)"),
      fileInput("ul_fit_rds", "Učitaj simulaciju (RDS)", accept = ".rds")
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Instrukcije",  withMathJax(includeMarkdown("www/Tab1.md"))),
                  tabPanel("Originalni prostor",
                           withMathJax(includeMarkdown("www/Tab2.Rmd")),
                           downloadButton("dl_original", "Preuzmi PNG"),
                           plotOutput("originalPlot", height = "600px", width = "100%"),
                           tags$style(HTML(".tab-content {font-size: 16px;}"))
                  ),
                  tabPanel("Logit prostor",
                           withMathJax(includeMarkdown("www/Tab3.md")),
                           downloadButton("dl_logit", "Preuzmi PNG"),
                           plotOutput("logitPlot", height = "600px", width = "100%"),
                           tags$style(HTML(".tab-content {font-size: 16px;}"))
                  ),
                  tabPanel("Marginalni rasporedi",
                           withMathJax(includeMarkdown("www/Tab4.md")),
                           # Row: download buttons for PNGs
                           fluidRow(
                             column(3, downloadButton("dl_hist_t",        "Preuzmi t (PNG)")),
                             column(3, downloadButton("dl_hist_v",        "Preuzmi v (PNG)")),
                             column(3, downloadButton("dl_hist_logit_t",  "Preuzmi logit(t) (PNG)")),
                             column(3, downloadButton("dl_hist_logit_v",  "Preuzmi logit(v) (PNG)"))
                           ),
                           br(),
                           
                           # Row: two histograms (original space)
                           fluidRow(
                             column(6, plotOutput("hist_t", height = "350px")),
                             column(6, plotOutput("hist_v", height = "350px"))
                           ),
                           
                           # Row: two histograms (logit space)
                           fluidRow(
                             column(6, plotOutput("hist_logit_t", height = "350px")),
                             column(6, plotOutput("hist_logit_v", height = "350px"))
                           ),
                           br(),
                           
                           # Moments table + CSV
                           fluidRow(
                             column(12, uiOutput("orig_moments_ui"))
                           ),
                           
                           tags$style(HTML(".tab-content {font-size: 16px;}"))
                  ),
                  tabPanel("Parametri ocenjenog modela",
                           withMathJax(includeMarkdown("www/Tab5.md")),
                           verbatimTextOutput("modelParams")
                  ),
                  tabPanel("BIC / ICL analiza",
                           withMathJax(includeMarkdown("www/Tab6.md")),
                           downloadButton("dl_bicicl", "Preuzmi PNG"),
                           plotOutput("bicIclPlot", height = "600px", width = "100%"),
                           tags$style(HTML(".tab-content {font-size: 16px;}"))
                  ),
                  tabPanel("ROC krive",
                           withMathJax(includeMarkdown("www/Tab7.md")),
                           downloadButton("dl_roc", "Preuzmi PNG"),
                           plotOutput("rocPlot", height = "600px", width = "100%"),
                           tags$style(HTML(".tab-content {font-size: 16px;}"))
                  ),
                  tabPanel("3D vizualizacija",
                           withMathJax(includeMarkdown("www/Tab8.md")),
                           downloadButton("dl_3dhtml", "Preuzmi HTML"),
                           plotlyOutput("plotly3d", height = "700px", width = "100%")
                  ),
                  tabPanel("Entropija i zakoni verovatnoće",
                           withMathJax(includeMarkdown("www/Tab9.md")),
                           textOutput("entropyText"),
                           downloadButton("dl_density", "Preuzmi PNG"),
                           plotOutput("densityContours", height = "600px", width = "100%"),
                           tags$style(HTML(".tab-content {font-size: 16px;}"))
                  )
      )
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  # Debounce za dugme "Pokreni simulaciju"
  debounced_simulate <- reactive(input$simulate) %>% debounce(500)
  
  # Keš i kontrola toka
  fitRV       <- reactiveVal(NULL)   # lista: data, data_logit, res
  bicIclRV    <- reactiveVal(NULL)   # data.frame K-BIC-ICL
  runningRV   <- reactiveVal(FALSE)  # da ignorišemo duple klikove
  cancelFlag  <- reactiveVal(FALSE)  # korisnički prekid dugih računanja
  
  # Reset svih ulaza i brisanje keša
  observeEvent(input$reset, {
    updateNumericInput(session, "seed", value = 1234)
    updateNumericInput(session, "n1",   value = 150)
    updateNumericInput(session, "n2",   value = 100)
    updateNumericInput(session, "n3",   value = 80)
    updateSliderInput(session, "alpha1", value = 5)
    updateSliderInput(session, "beta1",  value = 3)
    updateSliderInput(session, "alpha2", value = 4)
    updateSliderInput(session, "beta2",  value = 5)
    updateSliderInput(session, "alpha3", value = 2)
    updateSliderInput(session, "beta3",  value = 7)
    updateSliderInput(session, "rho1",   value = 0.4)
    updateSliderInput(session, "rho2",   value = 0.6)
    updateSliderInput(session, "rho3",   value = 0.8)
    updateNumericInput(session, "kmax",  value = 3)
    
    origMomentsRV(NULL)  # hide moments UI after reset
    
    fitRV(NULL); bicIclRV(NULL)
    cancelFlag(FALSE); runningRV(FALSE)
    showNotification("Aplikacija resetovana.", type = "message")
  })
  
  # Helper: uzmi parametre iz levog panela (i vreme)
  collect_params <- reactive({
    list(
      seed   = input$seed,
      n1 = input$n1, n2 = input$n2, n3 = input$n3,
      alpha1 = input$alpha1, beta1 = input$beta1, rho1 = input$rho1,
      alpha2 = input$alpha2, beta2 = input$beta2, rho2 = input$rho2,
      alpha3 = input$alpha3, beta3 = input$beta3, rho3 = input$rho3,
      kmax   = input$kmax,
      timestamp = as.character(Sys.time())
    )
  })
  
  # CSV: sirovi podaci
  output$dl_raw_csv <- downloadHandler(
    filename = function() paste0("sirovi_podaci_", Sys.Date(), ".csv"),
    content = function(file) {
      f <- fitRV(); validate(need(!is.null(f), "Prvo pokrenite simulaciju."))
      # Uključimo i “istinu” (class), i inverzne t, v; po želji dodaj i logite:
      df <- f$data
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # CSV: parametri
  output$dl_params_csv <- downloadHandler(
    filename = function() paste0("parametri_modela_", Sys.Date(), ".csv"),
    content = function(file) {
      p <- collect_params()
      # Jedan red (wide) – lak za pregled
      df <- as.data.frame(p, optional = TRUE)
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # Sačuvaj RDS (fit + parametri)
  output$dl_fit_rds <- downloadHandler(
    filename = function() paste0("simulacija_fit_", Sys.Date(), ".rds"),
    content = function(file) {
      f <- fitRV(); validate(need(!is.null(f), "Prvo pokrenite simulaciju."))
      params <- collect_params()
      saveRDS(list(fit = f, params = params), file = file)
    }
  )
  
  # Učitaj RDS i postavi stanje
  observeEvent(input$ul_fit_rds, {
    req(input$ul_fit_rds$datapath)
    obj <- try(readRDS(input$ul_fit_rds$datapath), silent = TRUE)
    if (inherits(obj, "try-error") || !is.list(obj) || is.null(obj$fit)) {
      showNotification("Nevažeći RDS fajl.", type = "error"); return()
    }
    # postavi fit u keš
    fitRV(obj$fit)
    bicIclRV(NULL)
    
    # opciono: update UI inpute iz sačuvanih parametara (ako želiš potpunu replikaciju)
    if (!is.null(obj$params)) {
      p <- obj$params
      safe_update <- function(id, val, fun) if (!is.null(p[[id]])) fun(session, id, value = p[[id]])
      safe_update("seed", p$seed, updateNumericInput)
      safe_update("n1", p$n1, updateNumericInput)
      safe_update("n2", p$n2, updateNumericInput)
      safe_update("n3", p$n3, updateNumericInput)
      safe_update("alpha1", p$alpha1, updateSliderInput)
      safe_update("beta1",  p$beta1,  updateSliderInput)
      safe_update("alpha2", p$alpha2, updateSliderInput)
      safe_update("beta2",  p$beta2,  updateSliderInput)
      safe_update("alpha3", p$alpha3, updateSliderInput)
      safe_update("beta3",  p$beta3,  updateSliderInput)
      safe_update("rho1",   p$rho1,   updateSliderInput)
      safe_update("rho2",   p$rho2,   updateSliderInput)
      safe_update("rho3",   p$rho3,   updateSliderInput)
      safe_update("kmax",   p$kmax,   updateNumericInput)
    }
    
    showNotification("Simulacija/fit je učitan iz RDS fajla.", type = "message")
  })
  
  
  
  # Cancel dugme
  observeEvent(input$cancel, {
    cancelFlag(TRUE)
    showNotification("Zaustavljanje je zatraženo. Sačekajte završetak tekuće iteracije…", type = "warning")
  })
  
  # Jedan EM-fit po kliku; dodat guard protiv duplih klikova
  observeEvent(debounced_simulate(), {
    if (isTRUE(runningRV())) {
      showNotification("Računanje je već u toku – zanemarujem novi klik.", type = "warning")
      return()
    }
    runningRV(TRUE); cancelFlag(FALSE)
    
    withProgress(message = "Pokrećem simulaciju i EM…", value = 0, {
      incProgress(0.05, detail = "Generišem podatke")
      set.seed(input$seed)
      
      d1 <- simBetaCopula(input$n1, input$alpha1, input$beta1, 5,   3, input$rho1); d1$class <- "Regular"
      d2 <- simBetaCopula(input$n2, input$alpha2, input$beta2, 4,   2, input$rho2); d2$class <- "Incremental"
      d3 <- simBetaCopula(input$n3, input$alpha3, input$beta3, 1.5, 7, input$rho3); d3$class <- "Extreme"
      data <- rbind(d1, d2, d3)
      data$class <- factor(data$class, levels = c("Regular","Incremental","Extreme"))
      
      data$t <- as.numeric(data$t); data$v <- as.numeric(data$v)
      if (anyNA(data$t) || anyNA(data$v)) {
        showNotification("Podaci sadrže NA – prekid.", type = "error")
        runningRV(FALSE); return()
      }
      data$t <- 1 - data$t; data$v <- 1 - data$v
      
      if (isTRUE(cancelFlag())) { runningRV(FALSE); return() }
      
      incProgress(0.15, detail = "Transformišem u logit prostor")
      data_logit <- data.frame(x = safe_logit(data$t), y = safe_logit(data$v))
      if (any(!is.finite(as.matrix(data_logit)))) {
        showNotification("Nefinitne vrednosti u logit prostoru.", type = "error")
        runningRV(FALSE); return()
      }
      X <- as.matrix(data_logit)
      
      if (isTRUE(cancelFlag())) { runningRV(FALSE); return() }
      
      incProgress(0.30, detail = "K-means inicijalizacija")
      k <- 3
      km <- kmeans(X, centers = k, nstart = 20)
      
      mu_init <- lapply(seq_len(k), function(i) as.numeric(km$centers[i, ]))
      sigma_init <- lapply(seq_len(k), function(i) {
        sub <- X[km$cluster == i, , drop = FALSE]
        if (nrow(sub) < 2) diag(ncol(X)) else cov(sub)
      })
      sigma_init <- lapply(sigma_init, function(S) { S <- as.matrix(S); S + diag(1e-6, ncol(S)) })
      lambda_init <- as.numeric(table(km$cluster) / nrow(X))
      
      if (isTRUE(cancelFlag())) { runningRV(FALSE); return() }
      
      incProgress(0.55, detail = "EM fit (mvnormalmixEM)")
      res_raw <- tryCatch({
        mvnormalmixEM(
          x = X, lambda = lambda_init, mu = mu_init, sigma = sigma_init,
          k = k, epsilon = 1e-06, maxit = 600, verb = FALSE
        )
      }, error = function(e) e)
      
      if (inherits(res_raw, "error")) {
        showNotification(paste("OM greška:", res_raw$message), type = "error")
        runningRV(FALSE); return()
      }
      
      if (isTRUE(cancelFlag())) { runningRV(FALSE); return() }
      
      incProgress(0.80, detail = "Relabel komponenti (Regular/Incremental/Extreme)")
      comp_hat <- max.col(res_raw$posterior, ties.method = "first")
      truth    <- data$class
      C <- table(comp_hat, truth)
      perms <- rbind(
        c(1,2,3), c(1,3,2),
        c(2,1,3), c(2,3,1),
        c(3,1,2), c(3,2,1)
      )
      scores <- apply(perms, 1, function(p) {
        sum(diag(C[p, c("Regular","Incremental","Extreme"), drop = FALSE]))
      })
      best <- perms[which.max(scores), ]
      
      P  <- res_raw$posterior[, best, drop = FALSE]
      MU <- res_raw$mu[best]
      SG <- res_raw$sigma[best]
      LA <- res_raw$lambda[best]
      
      k_hat <- max.col(P, ties.method = "first")
      data_logit$cluster <- factor(paste0("Grupa", k_hat), levels = c("Grupa1","Grupa2","Grupa3"))
      
      # Deskriptivno imenovanje parametara
      names(LA) <- c("Regular","Incremental","Extreme")
      names(MU) <- c("Regular","Incremental","Extreme")
      MU <- lapply(MU, function(m) { names(m) <- c("logit_t","logit_v"); m })
      names(SG) <- c("Regular","Incremental","Extreme")
      SG <- lapply(SG, named_sigma)
      
      res <- res_raw
      res$posterior <- P; res$mu <- MU; res$sigma <- SG; res$lambda <- LA
      
      n_min <- min(nrow(data), nrow(data_logit), nrow(P))
      fitRV(list(
        data       = data[seq_len(n_min), ],
        data_logit = data_logit[seq_len(n_min), ],
        res        = res
      ))
      bicIclRV(NULL)  # poništi stare BIC/ICL
      incProgress(1, detail = "Gotovo")
      runningRV(FALSE)
    })
  })
  
  # ---------- GRAFICI (čitaju iz keša) ----------
  # helperi da ne dupliramo kod za plot i download
  make_original_plot <- reactive({
    f <- fitRV(); req(f)
    ggplot(f$data, aes(t, v, color = class)) +
      geom_point(alpha = 0.75, size = 3) +
      scale_color_manual(values = c(Regular="green3", Incremental="orange", Extreme="red"),
                         name = "Istinita klasa") +
      labs(title = "Simulacija u originalnom prostoru",
           x = "Izlaznost", y = "Udeo glasova") +
      coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
      theme(text = element_text(size = 16))
  })
  
  make_logit_plot <- reactive({
    f <- fitRV(); req(f)
    ggplot(f$data_logit, aes(x, y, color = cluster)) +
      geom_point(alpha = 0.75, size = 3) +
      scale_color_manual(values = c(Grupa1="green3", Grupa2="orange", Grupa3="red"),
                         name = "OM grupa") +
      labs(title = "Grupisanje u logit prostoru",
           x = "logit(izlaznost)", y = "logit(udeo glasova)") +
      theme(text = element_text(size = 16))
  })

  make_hist_t <- reactive({
    f <- fitRV(); req(f)
    ggplot(f$data, aes(t)) +
      geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.6,
                     fill = "gray70", color = "white") +
      geom_density(linewidth = 1.1) +
      labs(title = "Marginalni raspored u ORIGINALNOM prostoru: t (izlaznost)",
           x = "t", y = "Gustina") +
      theme(text = element_text(size = 16))
  })
  
  make_hist_v <- reactive({
    f <- fitRV(); req(f)
    ggplot(f$data, aes(v)) +
      geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.6,
                     fill = "gray70", color = "white") +
      geom_density(linewidth = 1.1) +
      labs(title = "Marginalni raspored u ORIGINALNOM prostoru: v (udeo glasova)",
           x = "v", y = "Gustina") +
      theme(text = element_text(size = 16))
  })
  
  make_hist_logit_t <- reactive({
    f <- fitRV(); req(f)
    df <- transform(f$data, logit_t = safe_logit(t))
    ggplot(df, aes(logit_t)) +
      geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.6,
                     fill = "gray70", color = "white") +
      geom_density(linewidth = 1.1) +
      labs(title = "Marginalni raspored u LOGIT prostoru: logit(t)",
           x = "logit(t)", y = "Gustina") +
      theme(text = element_text(size = 16))
  })
  
  make_hist_logit_v <- reactive({
    f <- fitRV(); req(f)
    df <- transform(f$data, logit_v = safe_logit(v))
    ggplot(df, aes(logit_v)) +
      geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.6,
                     fill = "gray70", color = "white") +
      geom_density(linewidth = 1.1) +
      labs(title = "Marginalni raspored u LOGIT prostoru: logit(v)",
           x = "logit(v)", y = "Gustina") +
      theme(text = element_text(size = 16))
  })
  
  output$hist_t        <- renderPlot({ make_hist_t() })
  output$hist_v        <- renderPlot({ make_hist_v() })
  output$hist_logit_t  <- renderPlot({ make_hist_logit_t() })
  output$hist_logit_v  <- renderPlot({ make_hist_logit_v() })

  origMomentsRV <- reactiveVal(NULL)  # { list(table=df, by_comp=list(mu=..., Sigma=...), mixture=list(mu=..., Sigma=...)) }
  
  # Recompute whenever a new fit is stored
  observeEvent(fitRV(), {
    f <- fitRV()
    if (is.null(f)) { origMomentsRV(NULL); return() }
    
    res <- f$res
    comp_names <- names(res$lambda)
    K <- length(comp_names)
    nsim <- 2500
    
    mu_orig_list <- vector("list", K)
    Sigma_orig_list <- vector("list", K)
    
    for (k in seq_len(K)) {
      z  <- mvtnorm::rmvnorm(nsim, mean = res$mu[[k]], sigma = res$sigma[[k]])
      tv <- cbind(ilogit(z[,1]), ilogit(z[,2]))
      colnames(tv) <- c("t","v")
      mu_orig_list[[k]]    <- colMeans(tv)
      Sigma_orig_list[[k]] <- stats::cov(tv)
    }
    names(mu_orig_list) <- comp_names
    names(Sigma_orig_list) <- comp_names
    
    # Mixture-level
    mu_mix <- Reduce(`+`, Map(function(w, m) w * m, res$lambda, mu_orig_list))
    Sigma_mix <- matrix(0, 2, 2)
    for (k in seq_len(K)) {
      m_k <- mu_orig_list[[k]]
      S_k <- Sigma_orig_list[[k]]
      Sigma_mix <- Sigma_mix + res$lambda[k] * (S_k + tcrossprod(m_k - mu_mix))
    }
    colnames(Sigma_mix) <- rownames(Sigma_mix) <- c("t","v")
    
    # Flatten to a table (component rows + mixture row)
    to_row <- function(name, m, S) {
      data.frame(
        Komponenta = name,
        mean_t = unname(m[1]),
        mean_v = unname(m[2]),
        var_t  = unname(S[1,1]),
        var_v  = unname(S[2,2]),
        cov_tv = unname(S[1,2]),
        check.names = FALSE
      )
    }
    rows <- Map(to_row, comp_names, mu_orig_list, Sigma_orig_list)
    tbl  <- do.call(rbind, rows)
    tbl  <- rbind(tbl, to_row("Mixture", mu_mix, Sigma_mix))
    
    origMomentsRV(list(
      table   = tbl,
      by_comp = list(mu = mu_orig_list, Sigma = Sigma_orig_list),
      mixture = list(mu = mu_mix, Sigma = Sigma_mix)
    ))
  })
  
  output$orig_moments_ui <- renderUI({
    om <- origMomentsRV()
    if (is.null(om)) return(NULL)  # nothing to show after Reset or before first fit
    
    tagList(
      h4("Komponentni momenti u ORIGINALNOM prostoru (t, v)"),
      downloadButton("dl_orig_moments_csv", "Preuzmi momente (CSV)"),
      br(), br(),
      tableOutput("orig_moments_tbl")
    )
  })
  

  make_density_plot <- reactive({
    f <- fitRV(); req(f)
    ggplot(f$data_logit, aes(x, y, color = cluster)) +
      geom_point(alpha = 0.5) +
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon",
                      alpha = 0.3, color = NA) +
      scale_color_manual(values = c(Grupa1="green3", Grupa2="orange", Grupa3="red"),
                         name = "OM Grupe") +
      scale_fill_distiller(palette = "Spectral", direction = -1) +
      labs(title = "Konture zakona verovatnoće u logit prostoru") +
      theme(text = element_text(size = 16))
  })
  
  output$originalPlot <- renderPlot({ make_original_plot() })
  output$logitPlot    <- renderPlot({ make_logit_plot()    })
  output$densityContours <- renderPlot({ make_density_plot() })
  
  output$modelParams <- renderPrint({
    f <- fitRV(); req(f)
    res <- f$res
    
    cat("Ponderi (π_k):\n"); print(res$lambda)
    cat("\nSredine (μ_k) u LOGIT prostoru [logit_t, logit_v]:\n"); print(res$mu)
    cat("\nKovarijacione matrice (Σ_k) u LOGIT prostoru [logit_t, logit_v]:\n"); print(res$sigma)
    
    # ----- ORIGINALNI PROSTOR: Monte-Carlo aproksimacija po komponenti -----
    cat("\n---------------------------------------------\n")
    cat("Procene u ORIGINALNOM prostoru (t, v):\n")
    nsim <- 2500  # balans brzina/tačnost; podižite po potrebi
    
    comp_names <- names(res$lambda)
    K <- length(comp_names)
    
    mu_orig_list <- vector("list", K)
    Sigma_orig_list <- vector("list", K)
    
    for (k in seq_len(K)) {
      z <- mvtnorm::rmvnorm(nsim, mean = res$mu[[k]], sigma = res$sigma[[k]])
      tv <- cbind(ilogit(z[,1]), ilogit(z[,2]))   # transformacija nazad
      colnames(tv) <- c("t","v")
      mu_orig_list[[k]] <- colMeans(tv)
      Sigma_orig_list[[k]] <- stats::cov(tv)
    }
    names(mu_orig_list) <- comp_names
    names(Sigma_orig_list) <- comp_names
    
    cat("\nSredine (t, v) po komponentama:\n")
    print(mu_orig_list)
    
    cat("\nKovarijacione matrice (t, v) po komponentama:\n")
    print(Sigma_orig_list)
    
    # Mešavinski (mixture) momenti u originalnom prostoru
    mu_mix <- Reduce(`+`, Map(function(w, m) w * m, res$lambda, mu_orig_list))
    Sigma_mix <- matrix(0, 2, 2)
    for (k in seq_len(K)) {
      m_k <- mu_orig_list[[k]]
      S_k <- Sigma_orig_list[[k]]
      Sigma_mix <- Sigma_mix + res$lambda[k] * (S_k + tcrossprod(m_k - mu_mix))
    }
    colnames(Sigma_mix) <- rownames(Sigma_mix) <- c("t","v")
    
    cat("\n--- Mixture (global) u ORIGINALNOM prostoru ---\n")
    cat("Mixture sredina (t, v):\n"); print(mu_mix)
    cat("\nMixture kovarijaciona matrica (t, v):\n"); print(Sigma_mix)
  })

  output$orig_moments_tbl <- renderTable({
    om <- origMomentsRV(); req(om)
    # Round for readability
    within(om$table, {
      mean_t <- round(mean_t, 4); mean_v <- round(mean_v, 4)
      var_t  <- round(var_t,  5); var_v  <- round(var_v,  5)
      cov_tv <- round(cov_tv, 5)
    })
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = 's', align = 'c')
  
  output$dl_orig_moments_csv <- downloadHandler(
    filename = function() paste0("original_space_moments_", Sys.Date(), ".csv"),
    content  = function(file) {
      om <- origMomentsRV(); validate(need(!is.null(om), "Nema momenata za izvoz."))
      write.csv(om$table, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  
  output$rocPlot <- renderPlot({
    f <- fitRV(); req(f)
    truth <- f$data$class
    post  <- f$res$posterior
    par(mfrow = c(1, 3), cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.4)
    labz <- levels(truth)
    for (i in seq_along(labz)) {
      y <- as.integer(truth == labz[i])
      ro <- try(pROC::roc(y, post[, i]), silent = TRUE)
      if (!inherits(ro, "try-error")) {
        plot(ro, main = paste("ROC:", labz[i]), col = "blue", lwd = 2)
        abline(a = 0, b = 1, lty = 2, col = "gray")
      }
    }
  })
  
  output$plotly3d <- renderPlotly({
    f <- fitRV(); req(f)
    df_logit <- f$data_logit
    plot_ly(
      df_logit,
      x = ~x, y = ~y, z = ~f$res$posterior[, 3],  # Grupa3 = Extreme
      color = ~cluster, colors = c("green3","orange","red"),
      ##  colors = "Spectral",
      type = "scatter3d", mode = "markers",
      marker = list(size = 4)
    ) %>%
      layout(
        autosize = TRUE,
        scene = list(
          xaxis = list(title = "logit(izlaznost)", titlefont = list(size = 18)),
          yaxis = list(title = "logit(udeo glasova)", titlefont = list(size = 18)),
          zaxis = list(title = "Posterior Grupa3 (Extreme)", titlefont = list(size = 18))
        )
      )
  })
  
  output$entropyText <- renderText({
    f <- fitRV(); req(f)
    ent <- calc_entropy(f$res$posterior)
    paste("Srednja entropija klasifikacije:", round(ent, 5),
          "\nNiža entropija znači sigurniju klasifikaciju.")
  })
  
  # ---------- BIC/ICL: ručno, sa Cancel i kešom ----------
  debounced_bic <- reactive(input$run_bic) %>% debounce(400)
  observeEvent(debounced_bic(), {
    if (isTRUE(runningRV())) {
      showNotification("Pričekajte da se završi tekuće računanje.", type = "warning")
      return()
    }
    f <- fitRV()
    if (is.null(f)) {
      showNotification("Prvo pokrenite simulaciju.", type = "error"); return()
    }
    
    runningRV(TRUE); cancelFlag(FALSE)
    
    withProgress(message = "Računam BIC/ICL (paralelno)…", value = 0, {
      # Priprema podataka
      df <- f$data
      X  <- as.matrix(data.frame(
        x = safe_logit(df$t),
        y = safe_logit(df$v)
      ))
      Ks <- seq_len(max(1, input$kmax))
      p_dim <- ncol(X)
      
      # Plan paralelizacije (Windows: multisession; Linux/Mac: multicore ili multisession)
      old_plan <- future::plan()
      on.exit(future::plan(old_plan), add = TRUE)
      # koristi do (cores-1) radnika, ali minimum 1
      workers <- max(1, parallel::detectCores() - 1)
      future::plan(future::multisession, workers = workers)
      
      # Funkcija za jedan K (ista logika kao ranije u for-petlji)
      one_k <- function(k_cur, X, p_dim) {
        # kmeans init
        set.seed(123 + k_cur)  # stabilnija replikacija po K
        km <- kmeans(X, centers = k_cur, nstart = 10)
        
        mu_init <- lapply(seq_len(k_cur), function(i) as.numeric(km$centers[i, ]))
        sigma_init <- lapply(seq_len(k_cur), function(i) {
          sub <- X[km$cluster == i, , drop = FALSE]
          if (nrow(sub) < 2) diag(p_dim) else stats::cov(sub)
        })
        sigma_init <- lapply(sigma_init, function(S) { S <- as.matrix(S); S + diag(1e-6, p_dim) })
        lambda_init <- as.numeric(table(km$cluster) / nrow(X))
        
        fit_tmp <- tryCatch({
          mvnormalmixEM(
            x = X, lambda = lambda_init, mu = mu_init, sigma = sigma_init,
            k = k_cur, epsilon = 1e-06, maxit = 350, verb = FALSE
          )
        }, error = function(e) NULL)
        
        if (is.null(fit_tmp)) {
          return(list(k = k_cur, ok = FALSE, loglik = NA_real_, icl = NA_real_, bic = NA_real_))
        }
        
        n <- nrow(X)
        n_par <- (k_cur - 1) + k_cur * p_dim + k_cur * (p_dim * (p_dim + 1) / 2)
        logL  <- fit_tmp$loglik
        bic   <- -2 * logL + n_par * log(n)
        ent   <- -sum(fit_tmp$posterior * log(fit_tmp$posterior + 1e-15))
        icl   <- bic + 2 * ent
        
        list(k = k_cur, ok = TRUE, loglik = logL, icl = icl, bic = bic)
      }
      
      # Ako je već tražen cancel pre starta
      if (isTRUE(cancelFlag())) { runningRV(FALSE); return() }
      
      # Paralelno preko K vrednosti
      # Napomena: cancellation tokom futures nije trivijalan; ako kliknete "Zaustavi",
      # samo ćemo IGNORISATI rezultate kad stignu i ostaviti runningRV(FALSE).
      incProgress(0.05, detail = sprintf("Pokrećem do %d radnika…", workers))
      res_list <- future.apply::future_lapply(
        Ks,
        function(k_cur) one_k(k_cur, X, p_dim),
        future.seed = TRUE
      )
      
      # Ako je korisnik zatražio cancel u međuvremenu — ne objavljuj rezultate
      if (isTRUE(cancelFlag())) {
        runningRV(FALSE)
        showNotification("Računanje BIC/ICL je otkazano.", type = "warning")
        return()
      }
      
      # Sklapanje rezultata
      incProgress(0.90, detail = "Sastavljam rezultate…")
      ok_mask <- vapply(res_list, function(z) isTRUE(z$ok), logical(1))
      Ks_ok   <- vapply(res_list, `[[`, numeric(1), "k")
      bic_vec <- vapply(res_list, `[[`, numeric(1), "bic")
      icl_vec <- vapply(res_list, `[[`, numeric(1), "icl")
      
      dfm <- data.frame(K = Ks_ok, BIC = bic_vec, ICL = icl_vec)
      # sortiraj po K (future_lapply može vratiti već u poretku, ali za svaki slučaj)
      dfm <- dfm[order(dfm$K), ]
      
      bicIclRV(dfm)
      incProgress(1, detail = "Gotovo")
      runningRV(FALSE)
    })
  })
  
  
  output$bicIclPlot <- renderPlot({
    dfm <- bicIclRV()
    validate(need(!is.null(dfm), "Kliknite „Izračunaj BIC/ICL“ da biste dobili grafik."))
    dfm_long <- reshape2::melt(dfm, id.vars = "K")
    ggplot(na.omit(dfm_long), aes(K, value, color = variable)) +
      geom_point(size = 3) + geom_line() +
      labs(title = "BIC i ICL vrednosti po broju komponenti",
           x = "Broj komponenti", y = "Vrednost", color = "") +
      theme(text = element_text(size = 16))
  })
  
  # ---------- DOWNLOAD dugmad ----------
  output$dl_original <- downloadHandler(
    filename = function() paste0("original_prostor_", Sys.Date(), ".png"),
    content = function(file) {
      g <- make_original_plot()
      ggsave(file, g, width = 9, height = 6, dpi = 300)
    }
  )
  output$dl_logit <- downloadHandler(
    filename = function() paste0("logit_prostor_", Sys.Date(), ".png"),
    content = function(file) {
      g <- make_logit_plot()
      ggsave(file, g, width = 9, height = 6, dpi = 300)
    }
  )
  
  output$dl_hist_t <- downloadHandler(
    filename = function() paste0("marginal_t_", Sys.Date(), ".png"),
    content = function(file) ggsave(file, make_hist_t(), width = 8, height = 5, dpi = 300)
  )
  
  output$dl_hist_v <- downloadHandler(
    filename = function() paste0("marginal_v_", Sys.Date(), ".png"),
    content = function(file) ggsave(file, make_hist_v(), width = 8, height = 5, dpi = 300)
  )
  
  output$dl_hist_logit_t <- downloadHandler(
    filename = function() paste0("marginal_logit_t_", Sys.Date(), ".png"),
    content = function(file) ggsave(file, make_hist_logit_t(), width = 8, height = 5, dpi = 300)
  )
  
  output$dl_hist_logit_v <- downloadHandler(
    filename = function() paste0("marginal_logit_v_", Sys.Date(), ".png"),
    content = function(file) ggsave(file, make_hist_logit_v(), width = 8, height = 5, dpi = 300)
  )
  
  
  
  output$dl_density <- downloadHandler(
    filename = function() paste0("konture_gustina_", Sys.Date(), ".png"),
    content = function(file) {
      g <- make_density_plot()
      ggsave(file, g, width = 9, height = 6, dpi = 300)
    }
  )
  output$dl_bicicl <- downloadHandler(
    filename = function() paste0("bic_icl_", Sys.Date(), ".png"),
    content = function(file) {
      dfm <- bicIclRV(); validate(need(!is.null(dfm), "Nema podataka za BIC/ICL."))
      dfm_long <- reshape2::melt(dfm, id.vars = "K")
      p <- ggplot(na.omit(dfm_long), aes(K, value, color = variable)) +
        geom_point(size = 3) + geom_line() +
        labs(title = "BIC i ICL vrednosti po broju komponenti",
             x = "Broj komponenti", y = "Vrednost", color = "") +
        theme(text = element_text(size = 16))
      ggsave(file, p, width = 9, height = 6, dpi = 300)
    }
  )
  output$dl_roc <- downloadHandler(
    filename = function() paste0("roc_", Sys.Date(), ".png"),
    content = function(file) {
      f <- fitRV(); req(f)
      png(file, width = 1200, height = 400, res = 150)
      truth <- f$data$class; post <- f$res$posterior
      par(mfrow = c(1, 3), cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.4)
      labz <- levels(truth)
      for (i in seq_along(labz)) {
        y <- as.integer(truth == labz[i])
        ro <- try(pROC::roc(y, post[, i]), silent = TRUE)
        if (!inherits(ro, "try-error")) {
          plot(ro, main = paste("ROC:", labz[i]), col = "blue", lwd = 2)
          abline(a = 0, b = 1, lty = 2, col = "gray")
        }
      }
      dev.off()
    }
  )
  output$dl_3dhtml <- downloadHandler(
    filename = function() paste0("3D_posterior_", Sys.Date(), ".html"),
    content = function(file) {
      f <- fitRV(); req(f)
      df_logit <- f$data_logit
      w <- plot_ly(
        df_logit,
        x = ~x, y = ~y, z = ~f$res$posterior[, 3],
        color = ~cluster, colors = "Spectral",
        type = "scatter3d", mode = "markers",
        marker = list(size = 2)
      )
      saveWidget(w, file, selfcontained = TRUE)
    }
  )
}

shinyApp(ui, server)
