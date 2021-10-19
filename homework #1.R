"Social Network Analytics Empirical Exercise #1"

library(tidyverse)
library(igraph)
options(dplyr.print_max = 25)

#packages:
#igraph https://cran.r-project.org/web/packages/igraph/igraph.pdf (Links to an external site.)
#data.table https://cran.r-project.org/web/packages/data.table/data.table.pdf

df <- read.csv("C:\\Users\\jentl\\Documents\\Emory\\Fall 2021\\Social Network Analytics\\social_and_task_network.csv")

#22 people total
#ego: the person, alter: the other person
#social_tie: directional report of social relationship btw students
#task_tie: did they work together

#who reports highest social? tie?
df %>% group_by(ego) %>% summarise(avg_social_tie=sum(social_tie)) %>% arrange(desc(avg_social_tie))

#Q1 A
"1. First, consider the social and task ties as two separate networks.
(A) Use igraph to generate indegree, outdegree, closeness, betweenness, 
and PageRank centrality statistics for each individual the social and task networks."

#Q1B
"(B) Compute the correlations of the five centrality measures you generate for the socialnetwork with the 
five measures generated for the task network. Which measures in the task network are most closely related to 
those in the socializing network? Name at leastone insight can you draw from the relationships between 
these five measures across thetwo networks."