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
library(broom)
library(tidyr)
library(gt)
library(infer)
library(forcats)
library(purrr)
library(skimr)
library(readr)
library(readxl)
library(janitor)
library(ggthemes)


#These libraries are neccessary 

top_10_depts_e <- read_rds("top_10_depts_e.rds")
top_10_depts_c <- read_rds("top_10_depts_c.rds")
top_10_courses <- read_rds("top_10_courses.rds")
big_10_plot <- read_rds("big_10_plot.Rds")
top10dept_eg <- read_rds("top10dept_eg.rds")
top10dept_cg <- read_rds("top10dept_cg.rds")
conc_cat_1920e<- read_rds("conc_cat_1920e.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # Application title
                titlePanel("Harvard College Course Trends"),
                tabsetPanel(
                  tabPanel("Summary",
                           mainPanel (
                             h2("The Purpose"),
                             h5("The purpose of this project is to provide a quick snapshot of Harvard's academic program after 2015. In this project, I provide many summary statistics about Harvard College, such as the largest classes, and also analyze the growth of the various departments by enrollments and course offerings. This project is the first step in a much more ambitious endeavor to gain insight into Harvard's academic ecosystem and gain a more thorough understanding of the interests, motivations, and experiences of Harvard Students from all corners of the college."),
                             h5("There are large discrepancies in fastest growing departments when looked at as fastest growing by average increase in enrollments vs highest average increase in courses offered.

Interestingly, the departments with the highest enrollments are typically not the departments with the highest amount of courses offered. STEM departments typically have the higher enrollments, but they also have fewer courses offered by their departments."),

h5("After running a regression on course count and enrollments (formula was courses ~ enrollment), it was determined that the relationship was 0.044. This indicates the relationship between courses offered by a department and enrollments in those departments is only vaguely positive."),

h2("Conclusion"),
h5("This study is a first step in investigating Harvard’s academic set up. The key insights it offers are in the questions certain discrepancies raise. For example: why is there such a weak relationship between courses offered by departments and enrollments in those departments? Areas for further study include course load of professors by department, the relationship between a professor's course and/or enrollment load and that professor's compensation, the relationship between professor department and professor compensation, and many more."),
                             )),
                  
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
                             h5("The source code for this Shiny App can be found at my", a("GitHub", href="https://github.com/jasonbrose"))
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
                  ),
                  
                  tabPanel("Department Categories",
                           mainPanel (
                             plotOutput("Departments_Stacked")
                           )),
                  
                  
                  tabPanel("Fastest Growing Departments",
                           #sidebar 
                           sidebarLayout(
                             sidebarPanel(
                               
                               # different options 
                               selectInput(
                                 inputId = "input_3",
                                 label = "Fastest Growing Departments",
                                 choices = c(
                                   "By Average Annual % Increase in Enrollments in Courses Offered by Department" = "By Average Annual % Increase in Enrollments in Courses Offered by Department",
                                   "By Average Annual Increase in Courses Offered By Department" = "By Average Annual % Increase in Courses Offered By Department"
                                 )
                               )
                             ),
                             
                             mainPanel(
                               plotOutput("Dept_Growth")) 
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
        labs(title = "Top 10 Courses by Undergrad Enrollment", fill = "Course Name", caption = "NOTE: Ec 10A & 10B combined")+
        xlab("")+
        ylab("Total Enrollments")
      
      
    }
    else{
      big_10_plot%>%
        filter(u_grad > 0)%>%
        ggplot(aes(x = 1, y = u_grad, fill = big_10))+
        geom_bar(position = "stack", stat = "identity")+
        scale_x_discrete()+
        annotate("text", x = 1, y = 2400, label = "9%")+
        annotate("text", x = 1, y = 27000, label = "91%")+
        xlab("")+
        ylab("Enrollments")+
        labs(title = "% of Course Enrollments during AY 19-20", 
             fill = "Top 10 Courses")+
        theme_classic()
    }
  })
  
  output$Dept_Growth <- renderPlot({
    if(input$input_3 == "By Average Annual % Increase in Enrollments in Courses Offered by Department") {
      top10dept_eg %>%
        arrange(desc(mean_growth_pct))%>%
        slice(1:10)%>%
        ggplot(aes(course_department, mean_growth_pct, fill = course_department))+
        geom_bar(stat = "identity")+
        geom_text(aes(label=mean_growth_pct), position=position_dodge(width=0.9), vjust=-0.25)+
        scale_x_discrete()+
        theme(axis.text.x = element_text(angle = 70, hjust = 1))+
        labs(title = "Departments with the Highest Average Increase in Enrollments in Courses YoY",
             subtitle = "2017-2020",
             fill = "Department")+
        xlab("")+
        ylab("% growth")
    }
    else{
      top10dept_cg%>%
        arrange(desc(mean_growth_cc))%>%
        slice(1:10)%>%
        ggplot(aes(course_department, mean_growth_cc, fill = course_department))+
        geom_bar(stat = "identity")+
        geom_text(aes(label=mean_growth_cc), position=position_dodge(width=0.9), vjust=-0.25)+
        scale_x_discrete()+
        theme(axis.text.x = element_text(angle = 70, hjust = 1))+
        labs(title = "Average Yearly Increase In Courses Offered by Fastest Growing Departments",
             subtitle = "2017-2020",
             fill = "Department")+
        xlab("")+
        ylab("Courses Added")
    }
  })
  
  output$Departments_Stacked <- renderPlot({
    conc_cat_1920e %>%
      ggplot(aes(1, cat_enrollment, fill = dept_group))+
      geom_bar(position = "stack", stat = "identity")+
      scale_x_discrete()+
      annotate("text", x = 1, y = 6530, label = "36%")+
      annotate("text", x = 1, y = 21000, label = "37%")+
      annotate("text", x = 1, y = 32000, label = "27%")+
      xlab("")+
      ylab("Total Enrollments")+
      labs(title = "Distribution of Enrollments Across The 3 Department Categories", 
           fill = "Department Category")
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
