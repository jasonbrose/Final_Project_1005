#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(fs)
library(sf)
library(gganimate)
library(shinythemes)
library(readr)
library(dplyr)
library(tidyverse)
library(readr)
library(tidyverse)


#These libraries are neccessary 

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # Application title
                titlePanel("Harvard College Course Trends"),
                tabsetPanel(
                    tabPanel("About",
                             mainPanel(
                                 
                                 # Provide information about the data source 
                                 
                                 h2("The Data"),
                                 h5("This data was extracted from the Harvard College FAS Enrollment Page. This page lists all enrollment data from Spring 2020 to Spring 2016."), 
                                 
                                 # Understand what is being presented
                                 
                                 
                                 # Ensure that the minimum relevant background is provided to)
                                 
                                 
                                 # Include information about the app author so that anyone impressed by my Shiny App can contact me to offer me a job
                                 
                                 h2("About the Author: Jason Rose "),
                                 h5("I am a Harvard undergraduate studying Sociology. I was born in New York City but grew up around Harvard - both my brothers attended and it was all they talked about at home. They shared a lot of folk wisdom about the 'best' and 'most popular' courses, so I thought I'd continue the trend but with data-backed assertions."),
                                 h5("Contact me at jasonbellrose@gmail.com or connect with me on", a("LinkedIn", href="https://www.linkedin.com/in/jason-rose-749775111/")),
                                 
                                 # Include a link to the Source Code for reproducibility and credibility
                                 
                                 h2("Overview of the Project"),
                                 h5("I created a video that gives a quick overview of my project which can be accessed through this",a("link.", href="")),
                                 
                                 
                                 # Include a link of my video embedded
                                 
                                 h2("Source Code"),
                                 h5("The source code for this Shiny App can be found at my", a("GitHub", href=""))
                             )),
                    tabPanel("Summary",
                             mainPanel (
                                 h2("The Purpose"),
                                 h5("The purpose of this project is to provide a quick snapshot of Harvard's academic program after 2015. In this project, I provide many summary statistics about Harvard College, such as the largest classes, and also analyze the growth of the various departments. Lastly, I examine the relationship between enjoyability and difficulty of the top 25 courses. This project is the first step in a much more ambitious endeavor to gain insight into Harvard's academic ecosystem and gain a more thorough understanding of the interests, motivations, and experiences of Harvard Students from all corners of the college."),
                                 h2("Conclusions"),
                                 h5("By looking at the data, we can see there have been numerous changes over the past few years. The most noticeable is the meteoric rise in student interest in the Ethnicity, Migration, and Rights department as demonstrated through enrollment  ")
                             )),
                    
                    tabPanel("Biggest Departments",
                             #sidebar 
                             sidebarLayout(
                                 sidebarPanel(
                                     
                                     # different options 
                                     selectInput(
                                         inputId = "input_1",
                                         label = "Biggest Departments",
                                         choices = c(
                                             "By Total Enrollments in Courses" = "By Total Enrollments in Courses",
                                             "By Total Courses Offered" = "By Total Courses Offered"
                                         )
                                     )
                                 ),
                                 
                                 mainPanel(
                                     plotOutput("Biggest_Departments")) 
                             ),     
                    )))
