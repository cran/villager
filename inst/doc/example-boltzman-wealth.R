## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------

library(villager)
library(leaflet)

## -----------------------------------------------------------------------------
bounds <- data.frame(latitude = numeric(0), longitude = numeric(0));
# Add the top left point
bounds[nrow(bounds) + 1,] = c(40.772457, -73.975962)
# Add the top right point
bounds[nrow(bounds) + 1,] = c(40.772457, -73.974053)
# Add the bottom left point
bounds[nrow(bounds) + 1,] = c(40.771430, -73.975962)
# Add the bottom right point
bounds[nrow(bounds) + 1,] = c(40.771430, -73.974053)

# Plot them
leaflet::leaflet() %>% 
  leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
  leaflet::addMarkers (data = bounds) # Add agent locations

## -----------------------------------------------------------------------------
gps_agent <- R6::R6Class("agent",
  inherit = villager::agent,
  public = list(
    age = NULL,
    alive = NULL,
    children = NULL,
    father_id = NULL,
    first_name = NULL,
    gender = NULL,
    health = NULL,
    identifier = NULL,
    last_name = NULL,
    mother_id = NULL,
    partner = NULL,
    profession = NULL,
    latitude = NULL,
    longitude = NULL,

    initialize = function(identifier = NA,
                          first_name = NA,
                          last_name = NA,
                          age = 0,
                          mother_id = NA,
                          father_id = NA,
                          partner = NA,
                          children = vector(mode = "character"),
                          gender = NA,
                          profession = NA,
                          alive = TRUE,
                          health = 100,
                          latitude = 0,
                          longitude = 0) {
    super$initialize(identifier,
                     first_name,
                     last_name,
                     age,
                     mother_id,
                     father_id,
                     partner,
                     children,
                     gender,
                     profession,
                     alive,
                     health)
      self$latitude <- latitude
      self$longitude <- longitude
    },

    as_table = function() {
      agent_table <- data.frame(
        age = self$age,
        alive = self$alive,
        father_id = self$father_id,
        first_name = self$first_name,
        gender = self$gender,
        health = self$health,
        identifier = self$identifier,
        last_name = self$last_name,
        mother_id = self$mother_id,
        partner = self$partner,
        profession = self$profession,
        latitude = self$latitude,
        longitude = self$longitude
      )
      return(agent_table)
    }
  )
)

## -----------------------------------------------------------------------------
initial_condition <- function(current_state, model_data, agent_mgr, resource_mgr) {
  # Set the bounding box coordinates by specifying a single point and the max distance that the other corners are
  model_data$events <- list(top_left=list(40.772457, -73.975962), bottom_right=list(40.771430, -73.974053))
  # Create the initial agents (10 of them)
  for (i in seq(10)){
    agent_lat <- 40.77197975
    agent_long <- -73.9750075
    agent <- gps_agent$new(identifier=i, latitude=agent_lat, longitude=agent_long)
    agent_mgr$add_agent(agent)
    # Create the associated money resource
    money <- villager::resource$new(agent$identifier, quantity=10)
    resource_mgr$add_resource(money)
  }
}

## -----------------------------------------------------------------------------
test_model <- function(current_state, previous_state, model_data, agent_mgr, resource_mgr) {
  # Loop over each agent and move them
  for (agent in agent_mgr$get_living_agents()) {
    # Generate new coordinates
    delta_lat <- runif(1, -0.00006, 0.00006)
    latitude <- agent$latitude + delta_lat
    delta_long <- runif(1, -0.00006, 0.00006)
    longitude <- agent$longitude + delta_long

    # See if the agent runs out of bounds on the North and West sides
    if (longitude < model_data$events$top_left[[2]] ) {
      # The agent is too far West
      longitude <- agent$longitude - delta_long
    }
    if (latitude > model_data$events$top_left[[1]]) {
      # The agent is too far North
      latitude <- agent$latitude - delta_lat
    }
    # See if the agent runs out of bounds on the South and East sides
    if (longitude > model_data$events$bottom_right[[2]]) {
      # The agent is too far East
      longitude <- agent$longitude - abs(delta_long)
    }
    if (latitude < model_data$events$bottom_right[[1]] ) {
      # The agent is too far South
      latitude <- agent$latitude - delta_lat
    }
    
    agent$latitude <- latitude
    agent$longitude <- longitude
    # Avoid trading with itseld
    for (neighbor_agent in agent_mgr$get_living_agents()) {
      if (neighbor_agent$identifier == agent$identifier) {
        next
      }
    # Check to see if the neighbor is within this agent's trading region
    lat_range = abs(neighbor_agent$latitude - agent$latitude)
    long_range = abs(neighbor_agent$longitude - agent$longitude)
    if (lat_range < 0.000001 || long_range < 0.000001 ) {
      # Remove $1 from the agent that's currently moving
      money <- resource_mgr$get_resource(agent$identifier)
      # Skip the transfer if the agent doesn't have enough money
      if (money$quantity <= 0) {
        next
      }
      money$quantity <- money$quantity - 1
      # Give it to the neighbor
      money <- resource_mgr$get_resource(neighbor_agent$identifier)
      money$quantity <- money$quantity + 1
    }
  }
  }
}

## -----------------------------------------------------------------------------
los_angeles <- village$new("Central_Park", initial_condition, test_model, gps_agent)
simulator <- simulation$new(10000, list(los_angeles))
simulator$run_model()

## -----------------------------------------------------------------------------
# Load in data
agent_data <- readr::read_csv("./results/Central_Park/resources.csv")
# Filter the results down to just the last day
data<-agent_data[agent_data$step == 10000, ]
barplot(data$quantity, names.arg=data$name, main="Wealth Distribution", xlab="Agent", ylab="$")

