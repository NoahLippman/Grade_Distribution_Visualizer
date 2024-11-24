library(shiny)
library(ggplot2)
library(tidyverse)

# Define UI for application that draws a histogram

File_names <- list(
  "~/Downloads/Grad_Data/Fall_2020.csv",
  "~/Downloads/Grad_Data/Fall_2021.csv",
  "~/Downloads/Grad_Data/Fall_2022.csv",
  "~/Downloads/Grad_Data/Fall_2023.csv",
  "~/Downloads/Grad_Data/Spring_2019.csv",
  "~/Downloads/Grad_Data/Spring_2020.csv",
  "~/Downloads/Grad_Data/Spring_2021.csv",
  "~/Downloads/Grad_Data/Spring_2022.csv",
  "~/Downloads/Grad_Data/Spring_2023.csv",
  "~/Downloads/Grad_Data/Spring_2024.csv",
  "~/Downloads/Grad_Data/Summer_2019.csv",
  "~/Downloads/Grad_Data/Summer_2020.csv",
  "~/Downloads/Grad_Data/Summer_2021.csv",
  "~/Downloads/Grad_Data/Summer_2022.csv",
  "~/Downloads/Grad_Data/Summer_2023.csv",
  "~/Downloads/Grad_Data/Summer_2024.csv")


# Scrape initial data frame
binded_data <- Fall_2019 <- read_csv("/Users/noahlippman/Downloads/Grad_Data/Fall_2019.csv", show_col_types = FALSE) %>%
  mutate(CLASS_ID = paste(`TERM DESCRIPTION`, paste(paste(SUBJECT, COURSE, sep = "-"), `INSTRUCTOR NAME`, sep = "-"), sep = "-")) %>%
  select("CLASS_ID", "A+","A","A-","B+","B","B-","C+","C", "C-",
         "D+","D","D-","F")


# Append each data frame to the original
for (i in File_names) {
  print(i)
  binded_data <- rbind(binded_data, read_csv(i, show_col_types = FALSE) %>%
                         mutate(CLASS_ID = paste(`TERM DESCRIPTION`, paste(paste(SUBJECT, COURSE, sep = "-"), `INSTRUCTOR NAME`, sep = "-"), sep = "-")) %>%
                         select("CLASS_ID", "A+","A","A-","B+","B","B-","C+","C", "C-",
                                "D+","D","D-","F"))
}

# Create GPA mapping data_set
gpa_mapping <- data.frame(
  Grade = c("A+", "A", "A-", "B+", "B", "B-", 
            "C+", "C", "C-", "D+", "D", "D-", "F"),
  GPA = c(4.0,4.0,3.7,3.3,3.0,2.7,2.3,2.0,1.7,1.3,1,.7,0)
)

# Convert binded_data to long format

binded_data_long <- binded_data %>%
  pivot_longer(cols = -CLASS_ID, names_to = "Grade", values_to = "Count") %>%
  left_join(gpa_mapping, by = "Grade") %>%
  mutate(Count = as.numeric(Count))

grades_expanded <- binded_data_long %>%
  na.omit() %>%
  uncount(Count)

ui <- fluidPage(
  # Application title
  titlePanel("Grade Distribution Visualizer"),
  
  # Search Bar for Class Code
  textInput("Class_Code", "Class Code", placeholder = ""),
  
  # Search Bar for Semester
  textInput("Semester", "Semester i.e. (SP24, FA23, SU22)", placeholder = ""),
  
  # Search Bar for Teacher Name
  textInput("Teacher_Name (Last Name, First Name)", "Teacher Name", placeholder = ""),
  
  actionButton("Run","Click to Display Plot"),
  plotOutput("Grade_Density")
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observeEvent(input$Run, {
    output$Grade_Density <- renderPlot({
      # Check if Class_Code input is provided
      req(input$Class_Code)
      Class_Code <- input$Class_Code
      Semester <- input$Semester
      
      # Filter data based on Class_Code input
      filtered_data <- subset(grades_expanded, grepl(paste(Semester,Class_Code, sep = "-"), CLASS_ID))
      
      # Check if there is data to plot
      req(nrow(filtered_data) > 0)
      
      # Generate the density plot
      ggplot(filtered_data, aes(x = GPA, fill = CLASS_ID)) +
        geom_density(alpha = .25) +
        labs(title = paste("Density Plot for Class Code:", Class_Code),
             x = "GPA",
             y = "Density") +
        theme_minimal()
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)