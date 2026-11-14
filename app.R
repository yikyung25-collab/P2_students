# ============================================================
# app.R : Explain PCA (Step 1~6 + cumulative variance)
# engine.R 만 source 한다 (build_pca.R 은 서버에서 돌리지 않음)
# 같은 폴더에 필요한 것: engine.R, pca.rds, shoes_meta.csv, shoes/, selfcheck/ (labels.csv 포함)
# ============================================================
library(shiny)

source("engine.R")   # library(magick), pca, meta, read_shoe, photo_score, rebuild, search_shoe

# ---------- 시작할 때 한 번만 계산 ----------
ve <- pca$sdev^2 / sum(pca$sdev^2)
cv <- cumsum(ve)
k80 <- which(cv >= 0.80)[1]
k90 <- which(cv >= 0.90)[1]
k95 <- which(cv >= 0.95)[1]

cat_f    <- as.factor(meta$category)
cat_cols <- adjustcolor(seq_len(nlevels(cat_f)), 0.5)

# 음수 값이 있는 그림(Step 2, 4)용: 0을 기준으로 대칭 색
show_diff <- function(v) {
  m <- max(abs(v))
  image(matrix(v, 64, 64)[, 64:1], col = hcl.colors(101, "Blue-Red 3"),
        zlim = c(-m, m), axes = FALSE, asp = 1)
}

# 정사각형 + 밝기 고정(0~255)
show_shoe_fixed <- function(x, zlim = c(0, 255)) {
  x <- pmin(pmax(x, zlim[1]), zlim[2])
  image(matrix(x, 64, 64)[, 64:1], col = gray(0:255 / 255),
        zlim = zlim, axes = FALSE, asp = 1)
}

# K=1에서도 에러가 나지 않는 복원 함수
rebuild_safe <- function(z, K) {
  V <- pca$rotation[, 1:K, drop = FALSE]
  as.numeric(pca$center + V %*% z[1:K])
}

sc_files <- list.files("selfcheck", pattern = "\\.(png|jpg|jpeg)$", ignore.case = TRUE)

# ---------- UI ----------
ui <- navbarPage("Explain PCA",
                 tabPanel("Explain PCA",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         radioButtons("src", "Photo", c("Catalogue", "Self-check", "Upload")),
                                         conditionalPanel("input.src == 'Catalogue'",
                                                          numericInput("cat_id", "Catalogue id", 1, 1, 2000)),
                                         conditionalPanel("input.src == 'Self-check'",
                                                          selectInput("sc_file", "Self-check photo", sc_files)),
                                         conditionalPanel("input.src == 'Upload'",
                                                          fileInput("photo", "Upload", accept = c(".jpg", ".jpeg", ".png"))),
                                         sliderInput("K", "Number of PCs (K)", 1, 300, 20)
                            ),
                            mainPanel(width = 9,
                                      p("For the photo chosen in Shoe Search, these steps show how PCA turns a shoe into a few numbers and uses them to find the match."),
                                      
                                      h4("Step 1. Average shoe"),
                                      p("The average of all 2,000 catalogue shoes. Darker means more shoes have a dark pixel there."),
                                      plotOutput("p1", height = 280, width = 280),
                                      
                                      h4("Step 2. Photo − average"),
                                      p("Your photo minus the average shoe. Blue = darker than the average, red = brighter. PCA starts from this difference."),
                                      plotOutput("p2", height = 280, width = 280),
                                      
                                      h4("Step 3. Score map"),
                                      p("Every catalogue shoe placed by its PC1 and PC2 scores. Shoes of the same category tend to sit together, but some categories (e.g. Sandals and Slippers) overlap in these two PCs. Star = your photo, circles = its Top 5."),
                                      plotOutput("p3", height = 420),
                                      
                                      h4("Step 4. Eigenshoes"),
                                      p("Each PC is a pattern of change away from the average shoe. Blue and red parts change in opposite directions. The title shows the share of variance the PC explains."),
                                      plotOutput("p4", height = 360),
                                      p(strong("What each PC captures (our reading):")),
                                      tags$ul(
                                        tags$li("PC1 (30.3%): appears to capture the overall size of the shoe body."),
                                        tags$li("PC2 (15.7%): appears to track the ankle/upper part, i.e. high-top boots vs low shoes."),
                                        tags$li("PC3 (7.0%): appears to contrast the upper part with the sole area."),
                                        tags$li("PC4 and later: finer details such as toe, heel and outlines, explaining less and less variance.")
                                      ),
                                      
                                      h4("Step 5. Scores"),
                                      p("The first K PC scores of your photo (the chart shows up to the first 50; the table lists all K, up to 300). These K numbers replace the 4,096 pixels when we measure distance to other shoes."),
                                      plotOutput("p5", height = 260),
                                      div(style = "max-height:300px; overflow-y:auto;", tableOutput("t5")),
                                      
                                      h4("Step 6. Rebuild"),
                                      p("The photo rebuilt from the average shoe plus the first K eigenshoes. Larger K gives a sharper copy, but gains shrink after a while."),
                                      p(em("Top: the original photo vs the copy rebuilt with the K chosen in the slider.")),
                                      plotOutput("p6k", height = 300, width = 600),
                                      p(em("Bottom: fixed K values (1, 5, and the K needed to keep 80%, 90% and 95% of the variance).")),
                                      plotOutput("p6", height = 240),
                                      
                                      h4("More: cumulative explained variance"),
                                      p("How many PCs are needed to keep 80%, 90% and 95% of the information. The red line is the chosen K."),
                                      plotOutput("p7", height = 360),
                                      p(strong("Why we added this:"),
                                        sprintf(" It shows that about %d PCs keep 90%% of the information, so we do not need all 4,096 pixels. It also helps us choose K: a small K is fast, but too small loses detail.", k90))
                            )
                            )
                            
                 )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  # 선택한 사진의 픽셀(x)과 PC 점수(z): 사진을 바꾸면 자동으로 다시 계산
  sel <- reactive({
    if (input$src == "Catalogue") {
      id <- input$cat_id
      req(id, id >= 1, id <= 2000)
      list(x = read_shoe(sprintf("shoes/%04d.png", id)),
           z = pca$x[id, ], exclude = id)
    } else if (input$src == "Self-check") {
      req(input$sc_file)
      p <- file.path("selfcheck", input$sc_file)
      list(x = read_shoe(p), z = photo_score(p), exclude = NULL)
    } else {
      req(input$photo)
      p <- input$photo$datapath
      list(x = read_shoe(p), z = photo_score(p), exclude = NULL)
    }
  })
  
  x   <- reactive(sel()$x)
  z   <- reactive(sel()$z)
  K   <- reactive(min(input$K, 300, ncol(pca$rotation)))
  res <- reactive(search_shoe(z(), K(), sel()$exclude))
  
  output$p1 <- renderPlot({
    par(mar = c(1, 1, 3, 1))
    show_shoe_fixed(pca$center)
    title("Average shoe")
  })
  
  output$p2 <- renderPlot({
    par(mar = c(1, 1, 3, 1))
    show_diff(x() - pca$center)
    title("Photo - average")
  })
  
  output$p3 <- renderPlot({
    t5 <- res()$top5
    plot(pca$x[, 1], pca$x[, 2], col = cat_cols[cat_f], pch = 16,
         xlab = "PC1", ylab = "PC2", main = "Score map")
    points(pca$x[t5, 1], pca$x[t5, 2], pch = 1, cex = 2.2, lwd = 2)  # Top 5
    points(z()[1], z()[2], pch = 8, cex = 2.5, lwd = 3)              # 내 사진
    legend("topright", legend = c(levels(cat_f), "Your photo", "Top 5"),
           col = c(cat_cols, "black", "black"),
           pch = c(rep(16, nlevels(cat_f)), 8, 1))
  })
  
  output$p4 <- renderPlot({
    par(mfrow = c(2, 4), mar = c(1, 1, 3, 1))
    for (k in 1:8) {
      show_diff(pca$rotation[, k])
      title(sprintf("PC%d (%.1f%%)", k, 100 * ve[k]))
    }
  })
  
  output$p5 <- renderPlot({
    n <- min(K(), 50)   # 막대가 너무 많으면 읽기 어려워 최대 50개
    barplot(z()[1:n], names.arg = 1:n, xlab = "PC number", ylab = "PC score",
            main = sprintf("First %d PC scores of the photo", n),
            col = "grey60", border = NA)
    abline(h = 0)
  })
  
  output$t5 <- renderTable({
    n <- min(K(), length(z()))
    data.frame(`PC number` = paste0("PC", 1:n),
               `PC score`  = round(z()[1:n], 2),
               check.names = FALSE)
  })
  
  output$p6k <- renderPlot({
    par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))
    show_shoe_fixed(x())
    title("Original photo")
    show_shoe_fixed(rebuild_safe(z(), K()))
    title(paste("Rebuilt with K =", K()))
  })
  
  output$p6 <- renderPlot({
    ks <- sort(unique(c(1, 5, k80, k90, k95)))
    par(mfrow = c(1, length(ks)), mar = c(1, 1, 3, 1))
    for (k in ks) {
      show_shoe_fixed(rebuild_safe(z(), k))
      title(paste("K =", k))
    }
  })
  
  output$p7 <- renderPlot({
    n <- min(300, length(cv))
    plot(cv[1:n], type = "l", xlab = "K", ylab = "Cumulative variance",
         main = "Cumulative explained variance")
    abline(h = c(0.8, 0.9, 0.95), lty = 2)
    abline(v = K(), col = "red", lwd = 2)
  })
}

shinyApp(ui, server)
