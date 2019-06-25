library(shiny)
library(shinythemes)
library(RColorBrewer)

df <- read.csv('starbuck.csv') #read csv file
#shadowtext function from https://stackoverflow.com/questions/25631216/r-plots-is-there-any-way-to-draw-border-shadow-or-buffer-around-text-labels
shadowtext <- function(x, y=NULL, labels, col='white', bg='black', 
                       theta= seq(0, 2*pi, length.out=50), r=0.1, ... ) {
  
  xy <- xy.coords(x,y)
  xo <- r*strwidth('A')
  yo <- r*strheight('A')
  
  # draw background text with small shift in x and y in background colour
  for (i in theta) {
    text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
  }
  # draw actual text in exact xy position in foreground colour
  text(xy$x, xy$y, labels, col=col, ... )
}

#fuction that plot starbuck cup
plot_cup <- function(bev_input, bev_size){
  plot.new()
  par(mai = c(1,0,1,0))
  plot.window(xlim = c(0,24), ylim = c(0,24), asp = 1)
  
  nut_gram <- unlist(bev_input)
  nut_dens <- c(0.852, 0.8734, 0.9, 1.35, 1.8, 0.849, 1.5) #Nutrients density for calculate volme from thiere mass. I find these on internet
  nut_vol <- nut_gram/nut_dens
  
  nut_all <- which(bev_input != 0)
  total_nut_vol <- sum(nut_vol[nut_all])
  mypalette<-brewer.pal(length(nut_all), "Accent")
  
  size <- c('Short','Solo','Doppio', 'Tall', 'Grande', 'Venti')
  ml <- c(237, 237, 237, 355, 473, 591)
  names(ml) <- size
  mtoh <- 17/473
  mtoarea <- 255/946
  
  theta <- atan(1.5/(ml[[bev_size]]*mtoh))
  c <- tan(theta)
  h_net <- (-12+sqrt(144+4*2*c*255/473*total_nut_vol))/(4*c)
  b <- 6+2*h_net*c
  y <- c(3,3,rep(h_net,2)+3)
  x <- c(3,9,9+(b-6)/2,3-(b-6)/2)
  y_fix <- c(3,3,rep(h_net,2)+3)
  x_fix <- c(3,9,9+(b-6)/2,3-(b-6)/2)
  polygon(x, y, lwd = 1, col = mypalette[1], asp = 1, lwd = 0.5)
  shadowtext(x[3]+11, y[3]+2,
             paste('Carbohydrates',
                   paste(round(nut_vol[nut_all[length(nut_all)]], 2),
                        'ml',sep = ' '),
                   sep = ' '), cex = 1.2,
             col = mypalette[1], r = 0.03)
  
  j <- length(nut_all)
  k <- 1
  for(i in 1:(length(nut_all)-1)){
    total_nut_vol = total_nut_vol - nut_vol[nut_all[j]]
    h <- (-12+sqrt(144+4*2*c*255/473*total_nut_vol))/(4*c)
    b <- 6+2*h*c
    y <- c(3,3,rep(h,2)+3)
    x <- c(3,9,9+(b-6)/2,3-(b-6)/2)
    
    polygon(x, y, lwd = 1, col = mypalette[i+1], asp = 1, lwd = 0.5)
    shadowtext(x_fix[3]+11, y_fix[3]-i*1.5+2, 
               paste(names(nut_all)[length(nut_all)-i],
                     paste(round(nut_vol[nut_all[length(nut_all)-i]], 2),
                           'ml',sep = ' '),
                     sep = ' '),
               cex = 1.2, col = mypalette[i+1], r = 0.03)
    k <- k + 1
    j <- j - 1
  }
  x <- c(3,9,6+4.5,6-4.5)
  y <- c(3,3,rep(ml[[bev_size]]*mtoh,2)+3)
  polygon(x, y, lwd = 3, col = rgb(160/255,82/255,45/255,0.05), asp = 1)
  require(png)
  img <- readPNG('starbucks.png')
  addImg(img , x = mean(c(x[1],x[2])), y = mean(c(y[3], h_net+3)), width = 6)
}
addImg <- function(
  obj, # an image file imported as an array (e.g. png::readPNG, jpeg::readJPEG)
  x = NULL, # mid x coordinate for image
  y = NULL, # mid y coordinate for image
  width = NULL, # width of image (in x coordinate units)
  interpolate = TRUE # (passed to graphics::rasterImage) A logical vector (or scalar) indicating whether to apply linear interpolation to the image when drawing. 
){
  if(is.null(x) | is.null(y) | is.null(width)){stop("Must provide args 'x', 'y', and 'width'")}
  USR <- par()$usr # A vector of the form c(x1, x2, y1, y2) giving the extremes of the user coordinates of the plotting region
  PIN <- par()$pin # The current plot dimensions, (width, height), in inches
  DIM <- dim(obj) # number of x-y pixels for the image
  ARp <- DIM[1]/DIM[2] # pixel aspect ratio (y/x)
  WIDi <- width/(USR[2]-USR[1])*PIN[1] # convert width units to inches
  HEIi <- WIDi * ARp # height in inches
  HEIu <- HEIi/PIN[2]*(USR[4]-USR[3]) # height in units
  rasterImage(image = obj, 
              xleft = x-(width/2), xright = x+(width/2),
              ybottom = y-(HEIu/2), ytop = y+(HEIu/2), 
              interpolate = interpolate)
}

ui <- fluidPage(
  theme = shinytheme('darkly'),
  titlePanel("Starbuck Menu items Nutrient in Cup"),
  sidebarPanel(
    selectInput('bev_cat_select', 'Select Beverage Category',
                levels(df[[1]])),
    selectInput('bev_select', 'Select Beverage',
                levels(df[[2]])),
    selectInput('bev_size_select', 'Select Beverage Size',
                levels(df[[3]])),
    selectInput('bev_prep_select', 'Select Beverage Prep',
                levels(df[[4]])),
    actionButton('submit', label = 'Submit'),
    tableOutput('fact')
  ),
  mainPanel(
    fluidRow(
      column(7,plotOutput('cup')),
      column(5,plotOutput('img'))
    ),
    plotOutput('mineral')
  )
)

server <- function(input, output, clientData, session) {
  observeEvent(
    input$bev_cat_select,{
      updateSelectInput(session, 'bev_select', choices = 
                          unique(sapply(df[df['Beverage_category'] == input$bev_cat_select,][[2]],
                                        as.character)))
    })
  
  observeEvent(
    input$bev_select,{
      updateSelectInput(session, 'bev_size_select', choices = 
                          unique(sapply(df[df['Beverage'] == input$bev_select,][[3]],
                                        as.character)))
    })
  
  observeEvent(
    input$bev_size_select,{
      updateSelectInput(session, 'bev_prep_select', choices = 
                          unique(sapply(df[df['Beverage'] == input$bev_select 
                                           & df['size'] == input$bev_size_select,][['Beverage_prep']],
                                        as.character)))
    })
  
  bev_input <- eventReactive(input$submit,{
    unlist(subset(df,
                  Beverage_category == input$bev_cat_select &
                    Beverage == input$bev_select &
                    size == input$bev_size_select &
                    (Beverage_prep == input$bev_prep_select |
                       is.na(Beverage_prep)),
                  select = Sodium:Iron))
  })
  
  bev_size <- eventReactive(input$submit,{
    input$bev_size_select
  })
  
  
  output$cup <- renderPlot({
    plot_cup(bev_input()[4:10], bev_size())
  })
  
  
  output$mineral <- renderPlot({
    par(mai = c(1,1,1,1))
    mineral <- c()
    mineral[1] <- bev_input()[1]/2400
    mineral[2] <- bev_input()[2]/300
    mineral[3:6] <- bev_input()[12:15]*100
    names(mineral) <- names(bev_input()[c(1,2,12:15)])
    bar <- barplot(mineral, col = brewer.pal(7, 'Accent')
                   , ylim = c(0,100), main = 'Percent Daily Value (%DV)')
    shadowtext(bar, mineral+4, r = 0.05,
               label = paste(as.character(round(mineral,2)),rep('%',4), sep = ''),
               col = brewer.pal(7, 'Accent'))
    grid()
  })
  
  output$img <- renderPlot({
    plot.new()
    par(mai = c(1,0,1,0))
    plot.window(xlim = c(0,24), ylim = c(0,24), asp = 1)
    require(png)
    img <- readPNG('calories.png')
    addImg(img, x = 8, y = 20, width = 15)
    shadowtext(8, 15, 'Calories', cex = 1.5)
    shadowtext(15, 20, paste(bev_input()[['Calories']], 'kcal'), cex = 1.7)
    
    img2 <- readPNG('caffeine.png')
    addImg(img2, x = 8, y = 8, width = 6)
    shadowtext(8, 3, 'Caffeine', cex = 1.5)
    if(is.na(bev_input()[['Caffeine']])){
      shadowtext(15, 8, 'Varies', cex = 1.7)
    }else{
      shadowtext(15, 8, paste(bev_input()[['Caffeine']], 'mg'), cex = 1.7)
    }
  })
  
  output$fact <- renderTable({
    cbind(Nutrient = c('Sodium (mg)',
                       'Cholesterol (mg)',
                       'Caffeine (mg)',
                       'Saturated Fat (g)',
                       'Trans Fat (g)',
                       'Total Fat (g)',
                       'Protein (g)',
                       'Fiber (g)',
                       'Sugars (g)',
                       'Total Carbohydrates (g)',
                       'Calories (kcal)',
                       'VitA (%DV)',
                       'VitC (%DV)',
                       'Calcium (%DV)',
                       'Iron (%DV)'),
          Unit = bev_input())
  })
}

shinyApp(ui = ui, server = server)

