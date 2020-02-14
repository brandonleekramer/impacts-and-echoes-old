# Read In Packages
for (pkg in c("igraph", "tidyverse", "shiny")) {
  library(pkg, character.only = TRUE)
}

#
UIcharSetEnglish = list(headTitle = "Social Relationship Network Graph",
                        ok = "Click here to begin",
                        person1 = "First Persons (separated by space or comma)",
                        person2 = "Second Persons (separated by space or comma)",
                        #RelationLabel = "Relationship Labels (separed by space or comma)",
                        edge_weight = "Edge weight (separated by space or comma)",
                        mainTitle = "Main Title",
                        vcolor = "Color for Persons",
                        ecolor = "Color for Lines",
                        mainFontSize = "Color for Main Title",
                        vSize = "Size for Persons",
                        eLWD = "Size for Lines",
                        vfcolor = "Color for Labels of Persons",
                        efcolor = "Color for Labels of Lines",
                        vLabelFontSize = "Font Size for Labels of Persons",
                        eLabelFontSize = "Font Size for Labels of Lines")

UIcharSet = UIcharSetEnglish

colorSet <- c("grey","skyblue","turquoise",
              "tomato","tan","slateblue",
              "wheat","sienna","black")

# data 
#edgelist <- read_csv("whc-textnet-edgelist.csv")
#edgelist <- unite(edgelist, col = "new_col", source, target, sep = " ")
edgelist = paste(readLines("whc-textnet-edgelist.txt"), collapse = "\n")

default = paste(readLines("demo.txt"), collapse = "\n")

ui <- shinyUI(pageWithSidebar(
  
  headerPanel(UIcharSet$headTitle),
  
  sidebarPanel(
    checkboxInput("OK", UIcharSet$ok,F),
    tags$textarea(id="textArea", rows="10", cols="36", edgelist),
    textInput("mainTitle",UIcharSet$mainTitle,value=UIcharSet$headTitle),
    selectInput('vcolor', UIcharSet$vcolor, colorSet, selected = "turquoise"),
    selectInput('ecolor', UIcharSet$ecolor, colorSet, selected = "grey"),
    sliderInput('mainFontSize', UIcharSet$mainFontSize, min=1, max=10,
                value=3, step=0.2, round=0),
    sliderInput('vSize', UIcharSet$vSize, min=10, max=100,
                value=5, step=1, round=0),
    sliderInput('eLWD', UIcharSet$eLWD, min=1, max=10,
                value=1, step=0.2, round=0),
    selectInput('vfcolor', UIcharSet$vfcolor, colorSet,selected="black"),
    selectInput('efcolor', UIcharSet$efcolor, colorSet,selected="black"),
    sliderInput('vLabelFontSize', UIcharSet$vLabelFontSize, min=1, max=10,
                value=1.8, step=0.2, round=0),
    sliderInput('eLabelFontSize', UIcharSet$eLabelFontSize, min=1, max=10,
                value=1.2, step=0.2, round=0)
  ),
  
  mainPanel(
    conditionalPanel(condition = "input.OK = true",
                     plotOutput('plot')
    ),
    conditionalPanel(condition = "input.OK = false",
                     htmlOutput('help',inline = T)
    )
  )
))


server <- shinyServer(function(input, output) {
  
  process = function(v)
  {
    v = unlist(strsplit(x = v,split = ",|\\s+",perl = T))
    l = v[1]
    v = v[-1]
    n = length(v)
    p1 = p2 = label = NULL
    for(i in 1:(n-1))
    {
      for(j in (i+1):n)
      {
        p1 = c(p1, v[i])
        p2 = c(p2, v[j])
        label = c(label, l)
      }
    }
    return(list(p1=p1,p2=p2,l=label))
  }
  
  output$plot <- reactivePlot(function() {
    
    data = input$textArea
    data = unlist(strsplit(x = data, split = "\\n"))
    print(data)
    PersonOne = NULL
    PersonTwo = NULL
    #RelationLabel = NULL
    
    for(v in data)
    {
      r = process(v)
      PersonOne = c(PersonOne,r$p1)
      PersonTwo = c(PersonTwo,r$p2)
      #RelationLabel = c(RelationLabel,r$l)
    }
    
    if(input$OK)
    {
      Label = unique(c(PersonOne,PersonTwo))
      PersonOne = match(PersonOne,Label)
      PersonTwo = match(PersonTwo,Label)
      g = graph(as.vector(rbind(PersonOne,PersonTwo)),directed = T)
      
      V(g)$frame.color = "white"
      V(g)$label = Label
      V(g)$label.cex = input$vLabelFontSize
      V(g)$label.color = input$vfcolor
      V(g)$color = input$vcolor
      V(g)$size = input$vSize
      
      E(g)$arrow.mode = 0
      #E(g)$label = RelationLabel
      E(g)$label.cex = input$eLabelFontSize
      E(g)$label.color = input$efcolor
      E(g)$color = input$ecolor
      E(g)$width = input$eLWD
      plot(g,layout = layout.auto)
      title(main = input$mainTitle,cex.main=input$mainFontSize)
      title(sub = "Developed by Kehao Wu",cex.sub=0.8,col.sub="grey")
    }
    
    
  }, height=900)
  
})

shinyApp(ui = ui, server = server)
