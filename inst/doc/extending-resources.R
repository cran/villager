## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(plotly)
library(villager)

## -----------------------------------------------------------------------------
resource_expiration <- R6::R6Class("resource",
  cloneable = TRUE,
  public = list(
    name = NA,
    quantity = NA,
    creation_date = NA,

    #' Creates a new resource.
    #'
    #' @description Creates a new resource object
    #' @param name The name of the resource
    #' @param quantity The quantity present
    #' @param creation_date The date that the resource was created
    initialize = function(name = NA, quantity = 0, creation_date=NA) {
      self$name <- name
      self$quantity <- quantity
      self$creation_date <- creation_date # New member variable to track the creation date
    },

    #' Returns a data.frame representation of the resource
    #'
    #' @return A data.frame of resources

    as_table = function() {
      return(data.frame(name = self$name, quantity = self$quantity))
    }
  )
)

## -----------------------------------------------------------------------------
initial_condition <- function(current_state, model_data, agent_mgr, resource_mgr) {
  for (i in 1:10) {
    name <- runif(1, 0.0, 100)
    new_agent <- agent$new(first_name <- name, last_name <- "Smith")
    agent_mgr$add_agent(new_agent)
  }
  # Create two new resources at the current date (The first day)
  corn <- resource_expiration$new("Corn", 10, current_state$step)
  rice <- resource_expiration$new("Rice", 20, current_state$step)
  resource_mgr$add_resource(corn)
  resource_mgr$add_resource(rice)
}

## -----------------------------------------------------------------------------
# Create the model that, each day, checks to see whether or not any resource expire
model <- function(current_state, previous_state, model_data, agent_mgr, resource_mgr) {
  # Loop over all of the resources and check if any expire
  for (resource in resource_mgr$get_resources()) {
    # Figure out how many days have passed
    days_passed <- current_state$step - resource$creation_date
    if (resource$name == "Corn") {
      if (days_passed > 10 && resource$quantity > 0) {
        print("Setting Corn quantity to 0")
        resource$quantity <- 0
      }
    } else if (resource$name == "Rice" && resource$quantity > 0) {
      if (days_passed > 20) {
        print("Setting Rice quantity to 0")
        resource$quantity <- 0
      }
    }
  }
}

## -----------------------------------------------------------------------------
# Create the village and simulation
coastal_village <- village$new("Expiring_Resources", initial_condition, model, resource_class=)
simulator <- simulation$new(16, villages = list(coastal_village))
simulator$run_model()

## -----------------------------------------------------------------------------
# Load in data
time_series_data <- readr::read_csv("results/Expiring_Resources/resources.csv")

# Get unique dates
unique_step<- sort(unique(time_series_data$step))

# Get corn & rice quantities and dates
corn_date_quantities <- dplyr::filter(time_series_data, name=="Corn")
rice_date_quantities <- dplyr::filter(time_series_data, name=="Rice")

# create data frame for sorted data
reordered_time_series <- data.frame(step = unique_step, Corn = 0, Rice = 0)
for (i in 1:nrow(reordered_time_series)){
  reordered_time_series[i,2] = corn_date_quantities[which(corn_date_quantities$step == reordered_time_series$step[i]),2]
  reordered_time_series[i,3] = rice_date_quantities[which(rice_date_quantities$step == reordered_time_series$step[i]),2]
}

# Plot graph
plotly::plot_ly(reordered_time_series, x = ~step) %>% 
  plotly::add_trace(y = ~Corn, name = 'Corn', type = 'scatter', mode = 'lines') %>% 
  plotly::add_trace(y = ~Rice, name = 'Rice', type = 'scatter', mode = 'lines') %>%
  plotly::layout(title = 'Resource Counts', xaxis = list(title = 'Time Step'),
       yaxis = list(title = 'Quantity'), legend = list(title=list(text='Crop Type')))

