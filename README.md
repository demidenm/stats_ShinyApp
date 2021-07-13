# stats_ShinyApp

  This shiny app was created to allow users to play with data. Specifically, for different statistical models, e.g., t-test, regression, CFA, model parameters may be freely tweaked to simulate data and see how it alters the effects in the model. 

  To deploy the app, simply download the contents of this folder and open th app.R file. All necessary libraries are specified at the top and when necessary details of how they are used is specified. By clicking "run" you will be able to run the app locally.

Shiny app application guides advanced layouts: https://shiny.rstudio.com/articles/layout-guide.html https://mastering-shiny.org/action-layout.html
navbar example: https://shiny.rstudio.com/gallery/navbar-example.html
https://shanghai.hosting.nyu.edu/data/r/case-1-3-shinyapp.html

If you did not go through the tutorials at the links above, the shiny app can be broken down into three main components: 
      (1) User Interface
       (2) Local server input/output generation
       (3) Running shiny app
 Here the `navbarPage()` is deployed to create tabulated panels. Within each `tabPanel()` is contained a fluid page with the sidebarLayout. 
   The sidebarLayout consists of the `sidebarPanel()` which invokes user response and the `mainPanel()` which prints out the content.
   `sidebarPanel()` input is used in the local server output generation, whereby `input$<input_label>` populates the value.
   Similar to the input, `mainPanel()` pulls `output$<output_label>` to print the contents.

 In the local server input/output generation, datasets, plots and statistical values are generated:
       (1) datasets are generated using: `eventReactive(input$run1, {})` - eventReactive allows the data to be re-run on button push by user. This is saved into a variable, not an output field.
       (2) plots are generated using: `renderPlot({})` and saved into output$<variable name> which is then called in the `mainpanel()` via `plotOutput(<name>)`
       (3) statistics are generated using using `function()`/`renderPrint()`
       (4) Output tables are generated using `renderTable()` or `renderDataTable()` 
