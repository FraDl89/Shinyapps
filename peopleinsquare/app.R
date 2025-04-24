#Author: Francesco Di Lauro, PhD
#Email: francesco dot dilauro dot ndm dot ox dot ac dot uk
library(shiny)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Epidemic modelling on a round square"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("speed", "Speed of agents:", 
                  min = 0.1, max = 1.0, value = 0.3, step = 0.1),
      sliderInput("prob_stop", "Interaction probability (q):", 
                  min = 0, max = 1, value = 0.3, step = 0.05),
      sliderInput("average_duration", "Average duration of interaction (μ):", 
                  min = 1, max = 10, value = 3, step = 1),
      sliderInput("contagion_prob", "Probability of infection (p):", 
                  min = 0, max = 1, value = 0.2, step = 0.05),
      sliderInput("effective_distance", "Interaction radius (r):", 
                  min = 0.01, max = 0.2, value = 0.05, step = 0.01),
      sliderInput("in_flux", "In flux of new agents:", 
                  min = 0, max = 0.2, value = 0.05, step = 0.05),
      sliderInput("initial_condition", "Initial number of infected agents:", 
                  min = 1, max = 10, value = 3, step = 1),
      sliderInput("initial_populaiton", "Initial number of people:", 
                  min = 10, max = 100, value = 30, step = 10),
      actionButton("start", "Start Simulation", class = "btn-primary"),
      actionButton("stop", "Stop Simulation", class = "btn-danger"),
      actionButton("reset", "Reset Simulation", class = "btn-warning")
    ),
    
    mainPanel(
      plotOutput("plot_square", height = "500px"),
      plotOutput("plot_stats", height = "200px")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Constants
  square_radius <- 1
  dt <- 0.5  #Time step (the smaller the better in terms of dynamics but the worse in terms of performances)
  drift_strength <- 0.2 # Strength of the drift coefficient of agents
  
  #Simulation status
  values <- reactiveValues(
    sim_running = FALSE,
    sim_time = 0,
    agents = NULL,
    stats = NULL,
    update_counter = 0  # To update the visualisation
  )
  
  #agents initialisation
  init_agents <- function() {
    n_agents <- input$initial_populaiton
    n_infected <- min(input$initial_condition, n_agents)
    
    #When an agent is created, these are its variables
    agents_df <- data.frame(
      id = 1:n_agents,
      x = runif(n_agents, -square_radius, square_radius),
      y = runif(n_agents, -square_radius, square_radius),
      angle = runif(n_agents, 0, 2*pi),
      isinfected = c(rep(TRUE, n_infected), rep(FALSE, n_agents - n_infected)),
      isinteracting = rep(FALSE, n_agents),
      interaction_time = rep(0, n_agents),
      interaction_with = rep(NA, n_agents)
    )
    
    # Agents beging within the square
    for (i in 1:n_agents) {
      while (sqrt(agents_df$x[i]^2 + agents_df$y[i]^2) > square_radius) {
        agents_df$x[i] <- runif(1, -square_radius, square_radius)
        agents_df$y[i] <- runif(1, -square_radius, square_radius)
      }
    }
    
    values$agents <- agents_df
    
    # Statistics
    values$stats <- data.frame(
      time = 0, 
      infected = n_infected, 
      susceptible = n_agents - n_infected
    )
    values$sim_time <- 0
  }
  
  # Core of the model: simulation update step
  update_simulation <- function() {
    values$sim_time <- values$sim_time + dt
    
    #1: Check number of agents
    values$update_counter <- values$update_counter + 1
    
    df <- values$agents
    
    #2: new agents coming in
    if (values$update_counter %% 5 == 0 && runif(1) < input$in_flux) {
      initial_angle <- runif(1, 0, 2*pi)
      new_id <- if(nrow(df) > 0) max(df$id) + 1 else 1
      new_agent <- data.frame(
        id = new_id,
        x = square_radius * cos(initial_angle),
        y = square_radius * sin(initial_angle),
        angle = initial_angle + pi,
        isinfected = runif(1) < 0.05,
        isinteracting = FALSE,
        interaction_time = 0,
        interaction_with = NA
      )
      df <- rbind(df, new_agent)
    }
    
    #3: Update interactions
    isinteracting <- which(df$isinteracting)
    if (length(isinteracting) > 0) {
      df$interaction_time[isinteracting] <- df$interaction_time[isinteracting] - dt
      
      # When the interaction ends, make sure it is over
      end_interaction <- isinteracting[df$interaction_time[isinteracting] <= 0]
      if (length(end_interaction) > 0) {
        for (i in end_interaction) {
          partner_id <- df$interaction_with[i]
          df$isinteracting[i] <- FALSE
          df$interaction_with[i] <- NA
          
          if (!is.na(partner_id)) {
            partner_idx <- which(df$id == partner_id)
            if (length(partner_idx) > 0) {
              df$isinteracting[partner_idx] <- FALSE
              df$interaction_with[partner_idx] <- NA
            }
          }
        }
      }
      
      #4: Interacting agents can pass infection
      active_interacting <- which(df$isinteracting)
      if (length(active_interacting) > 0) {
        for (i in active_interacting) {
          if (!df$isinfected[i]) {
            partner_id <- df$interaction_with[i]
            if (!is.na(partner_id)) {
              partner_idx <- which(df$id == partner_id)
              if (length(partner_idx) > 0 && df$isinfected[partner_idx]) {
                if (runif(1) < input$contagion_prob * dt) {
                  df$isinfected[i] <- TRUE
                }
              }
            }
          }
        }
      }
    }
    
    # 5: Movement of non-interacting agents
    non_interacting_agents <- which(!df$isinteracting)
    if (length(non_interacting_agents) > 0) {
      for (i in non_interacting_agents) {
        #find new direction
        center_direction <- atan2(-df$y[i], -df$x[i])
        noise <- rnorm(1, 0, 0.3)  # some noise
        df$angle[i] <- df$angle[i] * (1-drift_strength) + center_direction * drift_strength + noise
        
        # Movement
        df$x[i] <- df$x[i] + input$speed * cos(df$angle[i]) * dt
        df$y[i] <- df$y[i] + input$speed * sin(df$angle[i]) * dt
        
        #In this version, agents cannot escape the square
        dist_from_center <- sqrt(df$x[i]^2 + df$y[i]^2)
        if (dist_from_center > square_radius) {
          direction_to_center <- atan2(df$y[i], df$x[i])
          df$x[i] <- square_radius * cos(direction_to_center)
          df$y[i] <- square_radius * sin(direction_to_center)
          df$angle[i] <- direction_to_center + pi + rnorm(1, 0, 0.2)
        }
      }
    }
    
    #6: Check if new interactions happen. Here update counter is used to make the code quicker by not checking this often 
    if (length(non_interacting_agents) > 1 && values$update_counter %% 2 == 0) {
      for (i in non_interacting_agents) {
        if (df$isinteracting[i]) next  #If this has changed check it.
        
        #We just want a fraction fo people who can interact to be able to
        candidates_interaction <- sample(non_interacting_agents[non_interacting_agents != i], 
                            min(10, length(non_interacting_agents) - 1))
        
        for (j in candidates_interaction) {
          if (df$isinteracting[j]) next  
          
          dist <- sqrt((df$x[i] - df$x[j])^2 + (df$y[i] - df$y[j])^2)
          if (dist <= input$effective_distance) {
            if (runif(1) < input$prob_stop) {
              duration_interaction <- max(1, rnorm(1, input$average_duration, input$average_duration/3))
              
              df$isinteracting[i] <- TRUE
              df$isinteracting[j] <- TRUE
              df$interaction_time[i] <- duration_interaction
              df$interaction_time[j] <- duration_interaction
              df$interaction_with[i] <- df$id[j]
              df$interaction_with[j] <- df$id[i]
              break
            }
          }
        }
      }
    }
    
    #7: update dataframe and counters
    values$agents <- df
    
    if (values$update_counter %% 2 == 0) {
      new_stats <- data.frame(
        time = values$sim_time,
        infected = sum(df$isinfected),
        susceptible = sum(!df$isinfected)
      )
      values$stats <- rbind(values$stats, new_stats)
    }
  }
  
  # Simulations checks
  observeEvent(input$start, {
    values$sim_running <- TRUE
  })
  
  observeEvent(input$stop, {
    values$sim_running <- FALSE
  })
  
  observeEvent(input$reset, {
    values$sim_running <- FALSE
    values$update_counter <- 0
    init_agents()
  })
  
  #Initialisation if no agents
  observe({
    if (is.null(values$agents)) {
      init_agents()
    }
  })
  
autoInvalidate <- reactiveTimer(500)
#this was suggested from a LLM to speed up the code. Not sure how it works
observe({
  autoInvalidate()
  
    isolate({
      update_simulation()
    })
  
})
  
  # Rendering of the square
  output$plot_square <- renderPlot({
    req(values$agents)
    df <- values$agents
    
    
    if (nrow(df) > 0) {
      ggplot() +

        ggplot2::annotate("path",
                          x = square_radius * cos(seq(0, 2*pi, length.out = 100)),
                          y = square_radius * sin(seq(0, 2*pi, length.out = 100)),
                          color = "black") +
        # agents
        geom_point(data = df, aes(x = x, y = y, color = isinfected, size = isinteracting)) +
        scale_color_manual(values = c("FALSE" = "green", "TRUE" = "red"), 
                           labels = c("FALSE" = "Suscettibile", "TRUE" = "isinfected")) +
        scale_size_manual(values = c("FALSE" = 3, "TRUE" = 5), 
                          labels = c("FALSE" = "Mobile", "TRUE" = "In interazione")) +
        coord_fixed(ratio = 1, xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
        labs(title = paste("Simulazione - time:", round(values$sim_time, 1)),
             subtitle = paste("infected:", sum(df$isinfected), "- susceptible:", sum(!df$isinfected))) +
        theme_minimal() +
        theme(legend.position = "bottom")
    }
  })
  
  # Statistics
  output$plot_stats <- renderPlot({
    req(values$stats)
    df_stats <- values$stats
    
    if (nrow(df_stats) > 1) {
      ggplot(df_stats, aes(x = time)) +
        geom_line(aes(y = infected, color = "infected"), size = 1.2) +
        geom_line(aes(y = susceptible, color = "susceptible"), size = 1.2) +
        scale_color_manual(values = c("infected" = "red", "susceptible" = "green")) +
        labs(x = "time", y = "Numero di agents", color = "Stato") +
        theme_minimal()
    }
  })
}


shinyApp(ui = ui, server = server)