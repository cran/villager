## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------

library(villager)
library(leaflet)

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
  # Create the initial villagers
  test_agent <- gps_agent$new(first_name="Lewis", last_name="Taylor", age=9125, latitude=33.8785486, longitude=-118.0434921)
  agent_mgr$add_agent(test_agent)
}

## -----------------------------------------------------------------------------
test_model <- function(current_state, previous_state, model_data, agent_mgr, resource_mgr) {
  # Loop over all the agents (just one at the moment)
  for (agent in agent_mgr$get_living_agents()) {
    # Generate new coordinates
    latitude <- agent$latitude + runif(1, 0.01, 0.03)
    longitude <- agent$longitude + runif(1, 0.01, 0.03)
    agent$latitude <- latitude
    agent$longitude <- longitude
  }
}

## -----------------------------------------------------------------------------
los_angeles <- village$new("Test_Village", initial_condition, test_model, gps_agent)
simulator <- simulation$new(10, list(los_angeles))
simulator$run_model()

## -----------------------------------------------------------------------------
# Load in data
agent_data <- readr::read_csv("results/Test_Village/agents.csv")

# Grab just the location data
agent_location <- data.frame(latitude = agent_data$latitude, longitude = agent_data$longitude)

# create a map 
leaflet::leaflet() %>% 
  leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
  leaflet::addMarkers (data = agent_location) # Add agent locations

