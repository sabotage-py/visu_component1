library(shiny)
library(shinydashboard)
library(ggplot2)


movies = read.csv("movie_data.csv")
movies_noh = read.csv("movie_data_notonehot.csv")



ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Movies, visualized!"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Genres", tabName = "genres"),
      menuItem("Budget vs Rating", tabName = "bvr"),
      menuItem("Runtime vs Rating", tabName = "rvr")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "genres",
              h2("Genres"),
              fluidRow(
                tabBox(
                  title = "Plots",
                  id = "tabset1", height = "100%", width = "100%",
                  tabPanel("Relationship between Genres",
                           fluidRow(
                             column(3,
                                    selectInput(
                                      inputId = "select_genre",
                                      "Select Genre",
                                      c("Action" = "Action",
                                        "Adventure" = "Adventure",
                                        "Animation" = "Animation",
                                        "Comedy" = "Comedy",
                                        "Crime" = "Crime",
                                        "Documentary" = "Documentary",
                                        "Drama" = "Drama",
                                        "Family" = "Family",
                                        "Fantasy" = "Fantasy",
                                        "History" = "History",
                                        "Horror" = "Horror",
                                        "Music" = "Music",
                                        "Mystery" = "Mystery",
                                        "Romance" = "Romance",
                                        "Science Fiction" = "Science.Fiction",
                                        "Thriller" = "Thriller",
                                        "War" = "War",
                                        "Western" = "Western"
                                      )
                                    )
                             ),
                             br(),
                             column(3,
                                    actionButton("genre_go", "Show me")
                             )
                           ),
                           fluidRow(
                             column(6, br(),
                                    plotOutput(
                                      outputId = "genre_bar"
                                    )),
                             column(6,
                                    br(),
                                    plotOutput(
                                      outputId = "genre_box"
                                    ))
                           )
                  ),
                  tabPanel("Other plots (by genre)",
                           fluidRow(
                             column(3,
                                    selectInput(
                                      inputId = "og_select",
                                      "Select Plot",
                                      c("Boxplot of Budget by Genre" = "budget",
                                        "Boxplot of Avg. Rating by Genre" = "vote_average",
                                        "Probability density plot of Avg. Rating by Genre" = "pdp_rating"
                                      )
                                    )
                             ),
                             br(),
                             column(3,
                                    actionButton("other_genre_go", "Show me")
                             )
                           ),
                           fluidRow(
                             column(6, align="center", br(),
                                    plotOutput(
                                      outputId = "other_genre_plot"
                                    ))
                           )
                  )
                  
                )
              )
              
      ),
      tabItem(
        tabName = "bvr",
        h2("Budget vs Rating"),
        fluidRow(
          tabBox(
            title = "Plots",
            id = "tabset1", height = "100%", width = "100%",
            tabPanel("High-budget movies (> $10M)",
                     fluidRow(
                       column(6,
                              sliderInput(
                                "high_budget_range",
                                "Budget Range (in million USD)",
                                min=10, max=300, value=c(10, 300), step=5)
                              ),
                       column(6, 
                              sliderInput(
                                "hb_year_range",
                                "Year of release",
                                min = 1930, max=2018, value=c(1980, 2010), step=2)
                              ),
                       column(3,
                              br(),
                              actionButton("high_budget_go", "Show me"))
                     ),
                     fluidRow(
                       column(
                         6,  align = "center",
                         plotOutput(
                           outputId = "high_budget_plot"
                         )
                         
                       )
                     )
            ),
            tabPanel("Low-budget movies ($10K - $10M)",
                     fluidRow(
                       column(6,
                              sliderInput(
                                "low_budget_range",
                                "Budget Range (in 100K USD)",
                                min=0.1, max=100, value=c(0.1, 10), step=0.1)
                       ),
                       column(6, 
                              sliderInput(
                                "lb_year_range",
                                "Year of release",
                                min = 1930, max=2018, value=c(1980, 2010), step=2)
                       ),
                       column(3,
                              br(),
                              actionButton("low_budget_go", "Show me"))
                     ),
                     fluidRow(
                       column(
                         6,  align = "center",
                         plotOutput(
                           outputId = "low_budget_plot"
                         )
                         
                       )
                     )
            )
          )
        )),
      tabItem(
        tabName = "rvr",
        h2("Runtime vs Rating"),
        fluidRow(
          column(6,
                 sliderInput(
                   "runtime_range",
                   "Runtime range (in minutes)",
                   min=24, max=340, value=c(40, 180), step=2)
          ),
          column(6, 
                 sliderInput(
                   "rr_year_range",
                   "Year of release",
                   min = 1930, max=2018, value=c(1980, 2010), step=2)
          ),
          column(3,
                 br(),
                 actionButton("rr_go", "Show me"))
        ),
        fluidRow(
          column(
            6,  align = "center",
            plotOutput(
              outputId = "rr_plot"
            )
            
          )
        )
        )
    )
  )
)

server <- function(input, output) {
  
  genres = c('Action', 'Adventure', 'Animation', 'Comedy', 'Crime', 
             'Documentary', 'Drama', 'Family', 'Fantasy', 'History', 
             'Horror', 'Music', 'Mystery', 'Romance', 'Science.Fiction', 
             'Thriller', 'War', 'Western')
  
  
  observeEvent(input$genre_go, {
    choice = reactive({input$select_genre})
    colour_choice = reactive({
      if (choice() == "Action") {"purple"}
      else if (choice() == "Adventure") {"skyblue3"}
      else if (choice() == "Animation") {"sienna2"}
      else if (choice() == "Comedy") {"tomato"}
      else if (choice() == "Crime") {"slategray4"}
      else if (choice() == "Documentary") {"violet"}
      else if (choice() == "Drama") {"yellow4"}
      else if (choice() == "Family") {"plum2"}
      else if (choice() == "Fantasy") {"orangered"}
      else if (choice() == "History") {"olivedrab"}
      else if (choice() == "Horror") {"saddlebrown"}
      else if (choice() == "Music") {"magenta2"}
      else if (choice() == "Mystery") {"yellow1"}
      else if (choice() == "Romance") {"pink2"}
      else if (choice() == "Science.Fiction") {"blueviolet"}
      else if (choice() == "Thriller") {"violetred4"}
      else if (choice() == "War") {"darkcyan"}
      else if (choice() == 'Western') {'brown3'}
    })
    
    df = subset(movies, movies[choice()] == 1)
    genre_percent = c()
    for (g in genres) {
      genre_percent = append(genre_percent, sum(df[g])/nrow(df))
    }
    
    for (i in 1:(length(genres)-1)) {
      for (j in (i+1):length(genres)) {
        if (genre_percent[i] < genre_percent[j]) {
          temp = genre_percent[i]
          genre_percent[i] = genre_percent[j]
          genre_percent[j] = temp
          temp = genres[i]
          genres[i] = genres[j]
          genres[j] = temp
        }
      }
    }
    
    genres = reorder(genres, genre_percent, decreasing = T)
    df_genre = data.frame("Genre"=genres[2:18], "share"=genre_percent[2:18])
    print(choice())
    print(genres[2])
    plot_var = ggplot(df_genre, aes(x=Genre, y=share)) +
      geom_bar(stat="identity", fill = colour_choice()) +
      coord_flip() +
      scale_x_discrete(limits = genres[2:18]) +
      ylim(c(0, min(1, max(genre_percent[-1]) + 0.1))) +
      geom_text(aes(label = scales::percent(share), hjust = -0.1)) +
      ggtitle(paste("Share of genres with", choice())) +
      ylab('Share ratio')
    #print(choice())
    output$genre_bar = renderPlot({
      isolate(
        plot_var
      )
    }, height = "auto")
    
    output$genre_box = renderPlot({
      isolate(
        ggplot() +
          geom_boxplot(data=df, aes(x=vote_average), fill="yellow2", color=colour_choice()) + 
          labs(title=paste("Boxplot of ratings of", choice(), "movies"), x="Rating (out of 10)") +
          xlim(0, 10)
      )
    }, height = "auto")
  })
  
  boxplot_x_label = reactive({
    if (input$og_select == "budget") {"Budget"}
    else if (input$og_select == "vote_average") {"Avg Rating"}
  })
  boxplot_x_subtitle = reactive({
    if (input$og_select == "budget") {"(million USD)"}
    else if (input$og_select == "vote_average") {"(out of 10)"}
  })
  current_df = reactive({
    if(input$og_select == "vote_average"){
      movies_noh
    }
    else if(input$og_select == "budget"){
      subset(movies_noh, budget > 0.1)
    }
  })
  
  plot_var = reactive({
    if(input$og_select == "pdp_rating"){
      ggplot(movies_noh, aes(x=vote_average, fill=genre)) +
        geom_density() +
        facet_wrap(~genre) +
        ggtitle("Probability density plot for avg. RATING by different GENREs")
    }
    else if(input$og_select == "budget" | input$og_select == "vote_average"){
      ggplot(current_df(), aes_string(y="genre", x=input$og_select)) +
        geom_boxplot(alpha = 0.7,
                     outlier.colour = "black", outlier.shape = 20, aes(fill=genre)) +
        xlab(paste(boxplot_x_label(), boxplot_x_subtitle())) +
        scale_y_discrete(limits = sort(unique(movie$genre), decreasing = T), name = "Genre") +
        ggtitle(paste("Boxplot of", boxplot_x_label(), "by Genre")) + 
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  observeEvent(input$other_genre_go, {
    output$other_genre_plot = renderPlot({
      isolate(
        plot_var()
      )
    }, height = "auto")
  })
  
  
  observeEvent(input$high_budget_go, {
    budget_df = subset(movies, (budget > input$high_budget_range[1]*1000000) & (budget <= input$high_budget_range[2]*1000000))
    budget_df$budget = budget_df$budget / 1000000
    budget_df = subset(budget_df, (year > input$lb_year_range[1]) & (year < input$lb_year_range[2]))
    output$high_budget_plot = renderPlot({
      isolate(
        ggplot(data=budget_df, aes(x=budget, y=vote_average)) + 
          geom_point(shape=16, aes(colour=year))+
          geom_smooth(formula = y~x, method = NULL, linetype="dashed",
                      color="orchid4", fill="orchid")+
          ylab("Average rating (out of 10)")+
          xlab("Budget (million USD)")
      )
    }, width = 600, height = "auto")
    
  })
  
  
  observeEvent(input$low_budget_go, {
    budget_df = subset(movies, (budget > input$low_budget_range[1]*100000) & (budget <= input$low_budget_range[2]*100000))
    budget_df$budget = budget_df$budget / 100000
    budget_df = subset(budget_df, (year > input$lb_year_range[1]) & (year < input$lb_year_range[2]))
    output$low_budget_plot = renderPlot({
      isolate(
        ggplot(data=budget_df, aes(x=budget, y=vote_average)) + 
          geom_point(shape=16, aes(colour=year))+
          geom_smooth(formula = y~x, method = NULL, linetype="dashed",
                      color="orchid4", fill="orchid")+
          ylab("Average rating (out of 10)")+
          xlab("Budget (100K USD)")
      )
    }, width = 600, height = "auto")
    
  })
  
  observeEvent(input$rr_go, {
    rr_df = subset(movies, (runtime > input$runtime_range[1]) & (runtime <= input$runtime_range[2]))
    rr_df = subset(rr_df, (year > input$rr_year_range[1]) & (year < input$rr_year_range[2]))
    output$rr_plot = renderPlot({
      isolate(
        ggplot(data=rr_df, aes(x=runtime, y=vote_average)) + 
          geom_point(shape=16, aes(colour=year))+
          geom_smooth(formula = y~x, method = NULL, linetype="dashed",
                      color="orchid4", fill="orchid")+
          ylab("Average rating (out of 10)")+
          xlab("Runtime (minutes)")
      )
    }, width = 600, height = "auto")
    
  })
  
}

# Start application
shinyApp(ui = ui, server = server)
