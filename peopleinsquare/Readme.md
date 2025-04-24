#Introduction
Epidemic Modeling on a "Round Square" by FDL
This interactive Shiny app simulates the spread of an infectious disease within a dynamic population of agents moving inside a circular area (referred to as a “round square”). The model allows users to explore how individual behaviors and spatial constraints affect transmission dynamics over time.

#Model Overview
The simulation is a spatial agent-based model (ABM), where agents (representing people) move randomly within a bounded circular area and may interact if they come within a defined distance of each other. These interactions may lead to the transmission of infection, depending on configurable epidemiological parameters.

#Features
Real-time animation of agents moving and interacting inside the circular environment.

Configurable parameters for movement speed, infection probability, interaction duration, agent density, and more.

Dynamic inflow of new agents, simulating population turnover.

Statistics panel displaying the evolution of susceptible vs infected individuals over time.


#Parameters:
Speed: how fast agents move.

q (Interaction Probability): how likely two nearby agents are to start an interaction.

μ (Average Interaction Duration): how long interactions typically last.

p (Infection Probability): how likely an interaction transmits the infection.

r (Interaction Radius): how close agents must be to interact.

Inflow: rate at which new agents enter the simulation.

Initial Conditions: starting population and number of infected agents.
