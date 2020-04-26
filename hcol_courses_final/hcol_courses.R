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
library(gtable)


#These libraries are neccessary 

top_10_depts_e <- read_rds("top_10_depts_e.rds")
top_10_depts_c <- read_rds("top_10_depts_c.rds")
top_10_courses <- read_rds("top_10_courses.rds")
fat_stack_plot <- read_rds("big_10_plot.rds")

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
                    ),
                    
                    tabPanel("Top 10 Courses",
                             #sidebar 
                             sidebarLayout(
                                 sidebarPanel(
                                     
                                     # different options 
                                     selectInput(
                                         inputId = "input_2",
                                         label = "Top 10 Courses",
                                         choices = c(
                                             "By the Numbers" = "By the Numbers",
                                             "% of Total Enrollments" = "% of Total Enrollments"
                                         )
                                     )
                                 ),
                                 
                                 mainPanel(
                                     plotOutput("Top_10_Courses")) 
                             ),     
                    )
                    ))

server <- function(input, output) {
    output$Biggest_Departments <- renderPlot({
        if(input$input_1 == "By Total Enrollments in Courses") {
           top_10_depts_e %>%
                arrange(desc(dept_enrollments))%>%
                slice(1:10)%>%
                ggplot(aes(x = course_department, y = dept_enrollments, fill = course_department))+
                geom_bar(stat = "identity")+
                scale_x_discrete()+
                theme(axis.text.x = element_text(angle = 70, hjust = 1))+
                labs(title = "Total Enrollments in Top 10 Departments", fill = "Department")+
                xlab("")+
                ylab("Total Enrollments")
        }
        else{
            top_10_depts_c %>%
                arrange(desc(course_count))%>%
                slice(1:10)%>%
                ggplot(aes(course_department, course_count, fill = course_department))+
                geom_bar(stat = "identity")+
                geom_text(aes(label=course_count), position=position_dodge(width=0.9), vjust=-0.25)+
                scale_x_discrete()+
                theme(axis.text.x = element_text(angle = 70, hjust = 1))+
                labs(title = "Academic Departments with Most Courses", fill = "Department")+
                xlab("")+
                ylab("Total Courses")
        }
    })
    
    output$Top_10_Courses <- renderPlot({
        if(input$input_2 == "By the Numbers") {
         #DATA TOWN BAYBEEEEE
            top_10_courses %>%
                ggplot(aes(course_name, u_grad, fill = course_name))+
                geom_bar(stat = "identity")+
                geom_text(aes(label=u_grad), position=position_dodge(width=0.9), vjust=-0.25)+
                scale_x_discrete()+
                theme(axis.text.x = element_text(angle = 70, hjust = 1))+
                labs(title = "Top 10 Courses by Undergrad Enrollment", fill = "Course Name")+
                xlab("")+
                ylab("Total Enrollments")
                
            
        }
        else{
            fat_stack_plot%>%
                ggplot(aes(x = 1, y = u_grad, fill = big_10))+
                geom_bar(position = "stack", stat = "identity")+
                scale_x_discrete()+
                annotate("text", x = 1, y = 2400, label = "9%")+
                annotate("text", x = 1, y = 27000, label = "91%")+
                xlab("")+
                ylab("Total Students")+
                labs(title = "% of Course Enrollments during AY 19-20", 
                     fill = "Big 10 Class")+
                theme_classic()
        }
    })
    
    }


# Run the application 
shinyApp(ui = ui, server = server)


