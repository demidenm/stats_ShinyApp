# Load libraries
library(shiny) # shiny app functionality
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyMatrix) #https://cran.r-project.org/web/packages/shinyMatrix/readme/README.html
library(rhandsontable)
library(thematic)
library(tidyverse)
library(lavaan)
library(simsem)
library(ggplot2)
library(semPlot)
library(patchwork) # stitches ggplots https://patchwork.data-imaginist.com/articles/guides/layout.html
library(grid)
library(ggiraph)
library(see) # assumption plots
library(webr) #https://cran.r-project.org/web/packages/webr/vignettes/plot-htest.html
library(performance) # assumption plots
library(ggrepel) # outlier plots
library(equatiomatic) # provides formulas of models
library(gridExtra) # gridarrange for plots
library(kableExtra)# for table formatting
library(parameters)# lavan output, e.g., cfa/lavaan that is tidy
library(ggiraphExtra)
library(plyr)
library(MASS)
library(NHANES)
library(glue)#print variables within string

# UI -- display that adjusts the dimensions of browser
# Shiny app application guides advanced layouts: https://shiny.rstudio.com/articles/layout-guide.html https://mastering-shiny.org/action-layout.html
# navbar example: https://shiny.rstudio.com/gallery/navbar-example.html
#   https://shanghai.hosting.nyu.edu/data/r/case-1-3-shinyapp.html

# widgets https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

# simulating data notes: https://aosmith.rbind.io/2018/08/29/getting-started-simulating-data/#simulate-data-with-a-difference-among-groups

# If you did not go through the tutorials at the links above, the shiny app can be broken down into three main components: 
#       (1) User Interface
#       (2) Local server input/output generation
#       (3) Running shiny app
# Here the navbarPage() is deployed to create tabulated panels. Within each tabPanel() is contained a fluid page with the sidebarLayout. 
#   The sidebarLayout consists of the sidebarPanel() which invokes user response and the mainPanel() which prints out the content.
#   sidebarPanel() input is used in the local server output generation, whereby input$<input_label> populates the value.
#   Similar to the input, mainPanel() pulls output$<output_label> to print the contents.
# In the local server input/output generation, datasets, plots and statistical values are generated:
#       (1) datasets are generated using: eventReactive(input$run1, {}) - eventReactive allows the data to be re-run on button push by user.
#       (2) plots are generated using: renderPlot({}).
#       (3) statistics are used generated using: renderPrint({})


set.seed(1000)
options(scipen=999) # turning off scientific notiation so extremely small values can be rounded



UserInferface <- navbarPage("Oooooooo, Data So Shiny!",
                            collapsible = TRUE, 
                            inverse = TRUE, 
                            theme = shinytheme("united"),

                            tabPanel("Intro",
                                     fluidPage(
                                         sidebarLayout(position = "right",
                                             sidebarPanel(style = "display: inline-block; float: center",
                                                 actionButton("start", "Onward!",
                                                              style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00;")
                                             ),
                                             mainPanel(
                                                 h2("New Shiny, who dis?"),
                                                 p("This pilot shiny app is a work-in-progress.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("Its purpose? To play with data. There are several tabs, select each tab to simulate some data based one provided input, such as:
                                                 mean, standard deviation, Pearson's r and/or sample size. The app works best after you select parameters and click the 'Onward!' button. 
                                                   This buttoon keeps the lights running. Whether it's to visualize the effect of the crud factor (see Meehl, 1990), variance, N at 
                                                   which a correlation stabilizes (see Schönbrodt & Perugini2013), or the effect of an outlier, click a tab, play around with some parameters, 
                                                   and see how some outputs change. Of note: The data will be simulated each time a sample size value is changed. So if you're wondering, 'why are 
                                                   the data points different?', blame mvrnorm() or my newb skills. Also, you'll notice that the mean/sd will not be *exactly* the specified mean. 
                                                   The sampling is being done from a random normal distribution, so after sampling the specified span the values in the simulated dataset will bounce 
                                                   around those numbers a tiny bit. Thus, they won't be exact, but approximate is the goal. It's for fun, after all...",
                                                   style = "font-family: 'times'; font-size:14px")
                                             )
                                         )
                                     )),
################################ 
                            tabPanel("T-tests", 
                                     fluidPage(
                                         sidebarLayout(position = "right",
                                             sidebarPanel(
                                                 helpText("Select Parameters to Simulate Data"),
                                                 actionButton("run1", "Onward!", 
                                                              style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00"),
                                                 sliderInput("sample", label = "Sample Size",
                                                             min = 5, value = 50, max = 2000, step = 15),
                                                 sliderInput("pop_mean", label = "Population Mean (e.g., H0)",
                                                             min = -20, value = 0, max = 20, step = .5),
                                                 sliderInput("alpha", label = "Alpha α (Type I error rate)",
                                                             min = 0.0001, value = .05, max = .20, step = .025),
                                                 sliderInput("mean1", label = "Var  *1* Mean ",
                                                             min = -20, value = 5, max = 20, step = .5),
                                                 sliderInput("sd1",  label = "Var *1* St Dev",
                                                             min = 0.1, value = 3, max = 25, step = .5),
                                                 sliderInput("mean2", label = "Var *2* Mean",
                                                             min = -20, value = 5, max = 20, step = .5 ),
                                                 sliderInput("sd2", label = "Var *2* St Dev",
                                                             min = 0.1, value = 3, max = 25, step = .5),
                                                 sliderInput("grp_m1", label = "Group01 Mean",
                                                             min = -20, value = 5, max = 20, step = .5),
                                                 sliderInput("grp_sd1",  label = "Group01 St Dev",
                                                             min = 0.1, value = 3, max = 25, step = .5),
                                                 sliderInput("grp_m2", label = "Group02 Mean",
                                                             min = -20, value = 5, max = 20, step = .5 ),
                                                 sliderInput("grp_sd2", label = "Group02 SD",
                                                             min = 0.1, value = 3, max = 25, step = .5)
                                                 
                                             ),
                                             mainPanel(
                                                 h2("Distribution & Effect for Mean-SD-Sample"),
                                                 p("What is the purpose of this tab? Based on the means, standard deviations, and sample size provided, 
                                                 a normally distributed dataset is generated. This data is then used to plot a boxplot of the two variables to visualize means/sd.
                                                 A distribution of this data and the population mean is provided. Then, a one sample t-test is run on each of the two variables with the
                                                 related t-distribution. Then, based on the means and standard deviations selected for two groups, data is simulated. This data is plotted and a two sample
                                                 independent t-test is performed. In general, this tab is to represent changes in the values based on: Mean, Standard Deviation, Sample Size, Alpha, 
                                                   and Population Mean (hypothesized null).",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("Note: To run/rerun, simply specify your values and click 'Onward!'",
                                                   style = "font-family: 'times'; font-size:10px; font-style: italic"),
                                                 br(),
                                                 br(),
                                                 p("The boxplots in the below figures, Figure 1 and Figure 2, represent the means and standard deviations selections.
                                                 Toggle the values observe the central tendency of data with a larger N.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 fluidRow(
                                                     splitLayout(cellWidths = c("50%", "50%"),
                                                                 plotOutput(outputId = "hist_var1"),
                                                                 plotOutput(outputId = "hist_var2")
                                                     )),
                                                 br(),
                                                 h4("Plotted Distribution: Variable 1",
                                                   style = "font-family: 'times'"),
                                                 p("Figure 3 reflects the sample mean distribution +/- 1, 2, and 3 standard deviations relative to that mean. 
                                                 For reference, the selected population mean (that is used in the one sample t-test) is also plotted in this figure. 
                                                 Since the data is resampled for this normal distribution density plot the mean varies a touch.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "t_test_dist"),
                                                 h4("One Sample t-test: Formula & Effect of Parameters",
                                                   style = "font-family: 'times'"),
                                                 p("Keeping in mind how the data shifts around the mean, we can consider the one sample t-test forumla.
                                                 To simplify the formula, our signal is in the numerator (what we want maximize) and the noise is in the denominator 
                                                 (what we want to limit). Maximizing the signal-to-noise ratio is central experimental psych (Cronbach, 1957). 
                                                 To maximize the signal we can increase the sample mean in comparison to the population mean (mu). Another way to increase
                                                 the signal is by minimizing the noise, or our standard deviation/error. This can be done by reducing variability 
                                                 *between* subjects or measurement error. The error can also be minimized by a larger N, as the standard deviation is scaled by sample size. I try to provide an 
                                                 example of how the SD is reduced with increased sample size in Figure 4. You can directly observe the change in the t-statistic and p-value below as you toggle the Var1/Var2 sample mean, standard deviation, sample size, 
                                                   and the population mean.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 uiOutput(outputId = "one_t_formula"),
                                                 tableOutput(outputId = "one_t_table"),
                                                 plotOutput(outputId = "t_test_dist_t"),
                                                 plotOutput(outputId = "t_test_dist_t2"),
                                                 br(),
                                                 p("As mentioned above, consider the denominator of the One Sample t-test formula.
                                     as.              Below, we can plot the selected *Variable 1* and consider how this reduces in size across range of 
                                                   N = 5 to 2000.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 uiOutput(outputId = "one_t_form_denom"),
                                                 plotOutput(outputId = "t_test_reduc_sd_1"),
                                                 br(),
                                                 br(),
                                                 h4("Two Independent Group Means",
                                                   style = "font-family: 'times'"),
                                                 uiOutput(outputId = "two_mean_t_formula"),
                                                 tableOutput(outputId = "two_mean_table"),
                                                 plotOutput(outputId = "two_mean_dist"),
                                                 plotOutput(outputId = "two_mean_test1"),
                                                 plotOutput(outputId = "twomean_reduc_sd_1"),
                                                 br(),
                                                 br(),
                                                 h4("Meaning of p-value in NHST?", style = "font-samily: 'time'"),
                                                 p("In this and other tabs in this shinyapp the p-value is used to represent the impact of means/errors and the 
                                                 crud factor (sample size) on the p-value in the null hypothesis significance testing (NHST) framekwork. As noted in Kruschke & Liddel (2018),
                                                 the p-value is", span("the probability that the observed effect/t-value would be observed if: a) H0/null were true and b) data were 
                                                 sampled according to the same stopping/testing methids as this data.", style = "font-style: italic"), "As mentioned earlier, the mean or 
                                                 mean difference (our signal) is in the numerator and the st dev (the noise) is in the denominator. So larger values in numerator and 
                                                 smaller values in the denominator increase the t-statistic and thus 'significance'. The sample size (in the numerator) also does a lot of heavy 
                                                 lifting as it attenuates the impact of intersubject variability.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 br(),
                                                 p("Packages used - Plotting: ggplot | plotting htest t-distributions: webr via plot()",
                                                   style = "font-family: 'times'; font-size:10px")
                                             )
                                         )
                                     )
                            ),

################################
################################
################################   

                            tabPanel("Correlation",
                                     fluidPage(
                                         sidebarLayout(position = "right",
                                             sidebarPanel(
                                                 h4("Select Parameters to Simulate Corr"),
                                                 actionButton("run2", "Onward!",
                                                              style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00"),
                                                 sliderInput("corr_sample", label = "Sample Size",
                                                             min = 5, value = 50, max = 2000,step = 15),
                                                 sliderInput("corr1",
                                                             label = "Pearson's r between Var 1 & Var 2",
                                                             min = 0, value = .2, max = 1, step = .005),
                                                 sliderInput("corr2",
                                                             label = "Pearson's r between Var 3 & Var 4",
                                                             min = 0, value = .2, max = 1, step = .005),
                                                 sliderInput("corr_m1", label = "Var 1 Mean",
                                                             min = -20, value = 0, max = 20, step = .5),
                                                 sliderInput("corr_m2", label = "Var 2 Mean",
                                                             min = -20, value = 0, max = 20, step = .5 ),
                                                 sliderInput("corr_m3", label = "Var 3 Mean",
                                                             min = -20, value = 0, max = 20, step = .5),
                                                 sliderInput("corr_m4", label = "Var 4 Mean",
                                                             min = -20, value = 0, max = 20, step = .5)
                                                 #selectInput('group_by', label = 'Group By',
                                                 #            choices = c('NULL', 'Sex', 'Race'),
                                                 #            selected = NULL),
                                             ),
                                             mainPanel(
                                                 h2("Correlation plots and effects from specified data"),
                                                 p("What does this tab represent? Similar as the first tab, here a normally distributed dataset generated.",
                                                   "Then the association between Var1~Var2 and Var3~Var4 is plotted.",
                                                   "Below, the effect size and p-value is printed to see if/how these change",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 uiOutput(outputId = "corr_formula_p"),
                                                 plotOutput(outputId = "corr_plot1"),
                                                 plotOutput(outputId = "corr_plot2"),
                                                 br(),
                                                 h4("Person r values for each of the inputs",
                                                   style = "font-family: 'times'"),
                                                 br(),
                                                 tableOutput(outputId = "corr_test"),
                                                 br(),
                                                 h4("Alt to Pearson's r: Spearman's rho",
                                                    style = "font-family: 'times'"),
                                                 p("Alternative to Pearson's r, one can use the spearman's formula which computes the
                                                   correlation by the rank of x and y. The formula is similar with the exception of x = x', 
                                                   whereby x' is the rank(x)"),
                                                 br(),
                                                 uiOutput(outputId = "corr_formula_rho"),
                                                 br(),
                                                 tableOutput(outputId = "corr_test3"),
                                                 br(),
                                                 br(),
                                                 p("Packages used - Plotting: ggplot | Simulating data based on means & correlations: mvnorm() via MASS",
                                                   style = "font-family: 'times'; font-size:10px")
                                             )
                                         )
                                     )
                            ),

################################
################################
################################

                            tabPanel("Stability of Effect",
                                     fluidPage(
                                         sidebarLayout(position = "right",
                                             sidebarPanel(
                                                 h4("Select Parameters to Simulate Data at Specified Pearson's r"),
                                                 actionButton("run3", "Onward!", 
                                                              style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00"),
                                                 sliderInput("corr3",
                                                             label = "Pearson's r between Var 1 & Var 2",
                                                             min = 0, value = .2, max = 1, step = .005),
                                                 sliderInput("samp_range",
                                                             label = "Sample (N) Range for Simulation",
                                                             min = 5, max = 10000, value= c(10,1000)),
                                                 sliderInput("stabcorr_m1", label = "Var 1 Mean",
                                                             min = -20, value = 0, max = 20, step = .5),
                                                 sliderInput("stabcorr_m2", label = "Var 2 Mean",
                                                             min = -20, value = 0, max = 20, step = .5 )
                                                 ),
                                             mainPanel(
                                                 h2("Stability of Correlation based on N"),
                                                 p("What does this tab represent? Provide some values to generate a Pearson's r. Using simulated data for resampled
                                                   values for N participants for specified range, a plot is generated to see how much an effect size can jump around.
                                                   Full disclosure, this tab was inspired by Schönbrodt & Perugini (2013, Journal of Research in Personality).
                                                   The formula here is based on the Pearson's r formula:",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 plotOutput(outputId = "EffNchange"),
                                                 br(),
                                                 plotOutput(outputId = "EffNchange2"),
                                                 br(),
                                                 br(),
                                                 p("Packages used - Plotting: ggplot | Simulating data based on means & correlations: mvnorm() via MASS",
                                                   style = "font-family: 'times'; font-size:10px")
                                                 )
                                             )
                                         )
                                     ),

################################
################################
################################

                                    tabPanel("Regression",
                                             fluidPage(
                                                 sidebarLayout(position = "right",
                                                     sidebarPanel(
                                                         h4("Select Parameters to Simulate Data at Specified Pearson's r"),
                                                         actionButton("run4", "Onward!", 
                                                                      style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00"),
                                                         sliderInput("m_reg_sample",
                                                                     label = "Sample (N) for Simulation",
                                                                     min = 2, max = 10000, value= 100),
                                                         sliderInput("m_reg_m1", label = "DV Mean",
                                                                     min = -20, value = 0, max = 20, step = .5),
                                                         sliderInput("m_reg_m2", label = "IV 1 Mean",
                                                                     min = -20, value = 0, max = 20, step = .5 ),
                                                         sliderInput("m_reg_m3", label = "IV 2 Mean",
                                                                     min = -20, value = 0, max = 20, step = .5),
                                                         sliderInput("m_reg_corr1",
                                                                     label = "Pearson's r between DV & IV 1",
                                                                     min = 0, value = .2, max = 1, step = .005),
                                                         sliderInput("m_reg_corr2",
                                                                     label = "Pearson's r between DV & IV 2",
                                                                     min = 0, value = .2, max = 1, step = .005),
                                                         sliderInput("m_reg_corr3",
                                                                     label = "Pearson's r between IV 1 & IV 2",
                                                                     min = 0, value = .2, max = 1, step = .005),
                                                         selectInput("outlier", label = "Include Outliers on IVs?", 
                                                                     choices = c("None","One","Two","Three"),
                                                                     selected = "None")
                                                         ),
                                                     mainPanel(
                                                         h2("Effect of N and Outliers on Regression"),
                                                         p("What does this tab represent? An example of a simple regression (Y ~ X) and multiple regression (Y ~ X1 + X2).",
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         br(),
                                                         h4("Linear Regression Example: ",
                                                           style = "font-family: 'times'"),
                                                         br(),
                                                         p("Formula for the linear regression model", 
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         uiOutput(outputId = "m_reg_formula_1"),
                                                         p("Plot and coefficients for the linear regression model.",
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         plotOutput(outputId = "m_reg_plot_1"),
                                                         br(),
                                                         tableOutput(outputId = "m_reg_1"),
                                                         br(),
                                                         p("Fitted values and association with residuals",
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         br(),
                                                         plotOutput(outputId = "m_reg_plot_2"),
                                                         br(),
                                                         br(),
                                                         h4("Multiple Regression Example: ",
                                                           style = "font-family: 'times'"),
                                                         p("Formula for the linear regression model", 
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         uiOutput(outputId = "m_reg_formula_2"),
                                                         p("Plot and coefficients for the linear regression model.",
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         plotOutput(outputId = "m_reg_plot_3"),
                                                         tableOutput(outputId = "m_reg_2"),
                                                         br(),
                                                         br(),
                                                         p("Packages used - Plotting: ggplot | Linear/multiple regression via: lm() | simulating data: mvnorm() via MASS | 
                                                           Stitching ggplots via patchwork",
                                                           style = "font-family: 'times'; font-size:10px")
                                                         )
                                                     )
                                                 )
                                             ),
################################
################################
################################
tabPanel("CFA & PCA",
         fluidPage(
             sidebarLayout(position = "left",
                           sidebarPanel(
                               h4("Select Parameters to Simulate Data Factor Loads"),
                               actionButton("run5", "Onward!", 
                                            style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00"),
                               sliderInput("sem_sample",
                                           label = "Sample (N) for Simulation",
                                           min = 2, max = 10000, value= 100),
                               selectInput(inputId = "standardize", label = "Standardize Lavaan Output?",
                                            choices = c('all','latent','FALSE'), selected = "FALSE")
                               #sliderInput("sem_a1", label = "A1 loading on Factor A",
                               #            min = .01, value = 0.5, max = 1, step = .01),
                               #sliderInput("sem_a2", label = "A2 loading on Factor A",
                               #            min = .01, value = 0.5, max = 1, step = .01),
                               #sliderInput("sem_a3", label = "A3 loading on Factor A",
                               #            min = .01, value = 0.5, max = 1, step = .01),
                               #sliderInput("sem_a4", label = "A4 loading on Factor A",
                               #            min = .01, value = 0.5, max = 1, step = .01),
                               #sliderInput("sem_b1", label = "B1 loading on Factor B",
                               #            min = .01, value = 0.5, max = 1, step = .01),
                               #sliderInput("sem_b2", label = "B2 loading on Factor B",
                               #            min = .01, value = 0.5, max = 1, step = .01),
                               #sliderInput("sem_b3", label = "B3 loading on Factor B",
                               #            min = .01, value = 0.5, max = 1, step = .01),
                               #sliderInput("sem_b4", label = "B4 loading on Factor B",
                               #            min = .01, value = 0.5, max = 1, step = .01),
                               #sliderInput("sem_c1", label = "C1 loading on Factor C",
                               #            min = .01, value = 0.5, max = 1, step = .01),
                               #sliderInput("sem_c2", label = "C2 loading on Factor C",
                               #            min = .01, value = 0.5, max = 1, step = .01),
                               #sliderInput("sem_c3", label = "C3 loading on Factor C",
                               #            min = .01, value = 0.5, max = 1, step = .01),
                               #sliderInput("sem_c4", label = "C4 loading on Factor C",
                               #            min = .01, value = 0.5, max = 1, step = .01),
                               #sliderInput("sem_A_to_B", label = "Cov: Factor A ~~ B",
                               #            min = .01, value = 0.5, max = 1, step = .01),
                               #sliderInput("sem_A_to_C", label = "Cov: Factor A ~~ C",
                               #            min = .01, value = 0.5, max = 1, step = .01),
                               #sliderInput("sem_B_to_C", label = "Cov: Factor B ~~ C",
                               #            min = .01, value = 0.5, max = 1, step = .01)
                           ),
                           mainPanel(
                               h2("CFA/PCA: Simulated Data via CFA Factor Structure"),
                               p("What does this tab represent? A data is generated via specified parameter values: loadings, correlations, and residual.
                                 Then, using this data CFA and then PCA models are presented using this simulated data.",
                                 style = "font-family: 'times'; font-size:14px"),
                               br(),
                               p("Please use the below input field to specify laavan syntax of a model that you wish to generate data from.
                                 If you are unfamiliar with laavan syntax, review tips at: https://lavaan.ugent.be/tutorial/syntax1.html.
                                 As noted at the aforementioned link, '=~' is a measured by indicator used for latent variable definitions; '~'
                                 is 'regressed on' used to specify regression models; '~~' is 'correlated with' indicating covariance in cases of Var1~~Var2,
                                 and residuals in cases of Var1~~Var1; and '~ 1' specifies the intercept.
                                 The following text field and the CFA field below operate similar as they would in Rstudio. Non-laavan strings
                                 will not work. However, lavaan code for starts/constraint of parameters should work. Of note, the text fields start you off,
                                 but you can build/revise these by adding additional factors, items, covariances, residuals, etc.",
                                 style = "font-family: 'times'; font-size:14px"),
                               textAreaInput(inputId = "sim_fact_coeff", 
                                             label = "Input Factors, Items loadings and Correlations for Sim Data",
                                             value = "\n#Factor item loadings \nFA1 =~ .5*a1 + .34*a2 + .65*a3 \nFA2 =~ .4*b1 + .33*b2 + .33*b3\n#Factor correlation\nFA1 ~~ .3 * FA2
                                             ", 
                                             width = '100%', rows = 8),
                               h4("Proposed CFA for Simulation",
                                  style = "font-family: 'times'"),
                               textOutput(outputId = "sim_model"),
                               #matrixInput("fact_loadings", label = "Factor Loadings", 
                               #            value = m_factorload, rows = list(
                               #                editableNames = TRUE, extend = TRUE), 
                               #            cols = list(
                               #                    editableNames = TRUE, extend = TRUE)),
                               #dataTableOutput(outputId = "test_data"),
                               #matrixInput("fact_corr", label = "Factor Correlations", 
                               #            value = m_factorcorr, rows = list(
                               #                editableNames = TRUE, extend = TRUE), 
                               #            cols = list(
                               #                editableNames = TRUE, extend = TRUE)),
                               h4("Correlations Among Simulated Data",
                                  style = "font-family: 'times'"),
                               tableOutput(outputId = "sem_corr"),
                               h4("Proposed CFA",
                                  style = "font-family: 'times'"),
                               textAreaInput(inputId = "cfa_model", 
                                             label = "Input lavaan() model for Proposed CFA",
                                             value = "\n#Factor item loadings \nFA1 =~ .5*a1 + .34*a2 + .65*a3 \nFA2 =~ .4*b1 + .33*b2 + .33*b3\n#Factor correlation\nFA1 ~~ FA2
                                             ", 
                                             width = '100%', rows = 8),
                               tableOutput(outputId = "sem_table"),
                               p("Returning Plot with Standardized Values",
                                  style = "font-family: 'times', font-size = 14px"),
                               plotOutput(outputId = "sem_plot"),
                               p("Packages used: Data was simulated via simsem() and lavaan(). Parameter table was generated using parameters(). 
                                 The plot was generated using semPlot()",
                                 style = "font-family: 'times'; font-size:10px")
                           )
             )
         )
)
)




local_server <- function(input, output, session){
    thematic::thematic_shiny() # so plots/tables match shiny theme

    
#################################### 
# Creating Diff data, using Reactive
################################### 
    

    
# simulation data based on user providing 3 means & 3 sds

    data <- eventReactive(input$run1, {
        data<- data.frame(var1 = rnorm(n = input$sample,
                                       mean = input$mean1,
                                       sd = input$sd1),
                          var2 = rnorm(n = input$sample,
                                       mean = input$mean2,
                                       sd = input$sd2))
        
        grp_values <- rnorm(n = input$sample,
                            mean = c(input$grp_m1, input$grp_m2),
                            sd = c(input$grp_sd1, input$grp_sd2))
        groups <- rep(letters[1:2], length.out = input$sample)
        
        grps_df <- data.frame(grp_values, groups)
        
        data.frame(data, grps_df)
    })

################################################################      
# Simulation sample with TWO variables reflective pre specified correlation
    sim_corr <- eventReactive(input$run2, {
        set.seed(1)
        df <- data.frame(
            mvrnorm(n = input$corr_sample, 
                    mu = c(input$corr_m1, input$corr_m2), # mean specified by user
                    Sigma = matrix(c(1, input$corr1, input$corr1, 1), ## corr specified by user
                                   nrow = 2), 
                    empirical = TRUE))
    })
    sim_corr_2 <- eventReactive(input$run2, {
        set.seed(1)
        data.frame(
            mvrnorm(n = input$corr_sample, 
                    mu = c(input$corr_m3, input$corr_m4), # mean specified by user
                    Sigma = matrix(c(1, input$corr2, input$corr2, 1), ## corr specified by user
                                   nrow = 2), 
                    empirical = TRUE))
    })

################################################################   
# Calculating variability in effect across N
    stable_DF <- eventReactive(input$run3, {
        
        df <- data.frame(
            mvrnorm(n = input$samp_range[2], 
                    mu = c(input$stabcorr_m1, input$stabcorr_m2), # means input by user
                    Sigma = matrix(c(1, input$corr3, input$corr3, 1), # correlation specified by user
                                   nrow = 2), 
                    empirical = TRUE))
        
        datalist = list()
        
        for (i in seq(input$samp_range[1],input$samp_range[2],5)) {
            Correl <- sample_n(df, i)
            corr_pear <- cor.test(Correl$X1, Correl$X2, method = "pearson")
            corr_spear <- cor.test(Correl$X1, Correl$X2, method = "spearman")
            corr_kend <- cor.test(Correl$X1, Correl$X2, method = "kendall")
            
            datalist[[i]] <- data.frame(Pearson_r = corr_pear$estimate, 
                                        Pearson_p = corr_pear$p.value,
                                        Spearman_rho = corr_spear$estimate, 
                                        Pearson_p = corr_spear$p.value,
                                        Kendall_t = corr_kend$estimate, 
                                        Kendall_p = corr_kend$p.value,
                                        i)
        }
        do.call(rbind, datalist)
    })

    
################################################################      
# Simulation sample with THREE variables reflective pre specified correlation for multiple regression
    m_reg_data <- eventReactive(input$run4, {
        set.seed(1)
        if (input$outlier=="None") {
            m_reg_df <-
                data.frame(
                    mvrnorm(n = input$m_reg_sample, 
                            mu = c(input$m_reg_m1,input$m_reg_m2,input$m_reg_m3), 
                            Sigma = matrix(c(1, input$m_reg_corr1, input$m_reg_corr2,
                                             input$m_reg_corr1,1,input$m_reg_corr3,
                                             input$m_reg_corr2,input$m_reg_corr3,1), 
                                           nrow = 3), 
                            empirical = TRUE))
            m_reg_df<- rename(m_reg_df, 
                              c("X1"="Drinking", "X2" ="Reward_Seek","X3"="Age"))
            m_reg_df
            
        } else if (input$outlier=="One") {
            m_reg_df <-
                data.frame(
                    mvrnorm(n = input$m_reg_sample, 
                            mu = c(input$m_reg_m1,input$m_reg_m2,input$m_reg_m3), 
                            Sigma = matrix(c(1, input$m_reg_corr1, input$m_reg_corr2,
                                             input$m_reg_corr1,1,input$m_reg_corr3,
                                             input$m_reg_corr2,input$m_reg_corr3,1), 
                                           nrow = 3), 
                            empirical = TRUE))
            m_reg_df<- rename(m_reg_df, 
                              c("X1"="Drinking", "X2" ="Reward_Seek","X3"="Age"))
            rand_row = as.integer(runif(1,min = 1, max = length(m_reg_df$Drinking)))
            rand_col = as.integer(runif(1, min = 1, max = 2))
            m_reg_df[rand_row[1],rand_col[1]]<- m_reg_df[rand_row[1],rand_col[1]] + (5*sd(m_reg_df[,rand_col[1]]))
            m_reg_df
            
        } else if (input$outlier=="Two") {
            m_reg_df <-
                data.frame(
                    mvrnorm(n = input$m_reg_sample, 
                            mu = c(input$m_reg_m1,input$m_reg_m2,input$m_reg_m3), 
                            Sigma = matrix(c(1, input$m_reg_corr1, input$m_reg_corr2,
                                             input$m_reg_corr1,1,input$m_reg_corr3,
                                             input$m_reg_corr2,input$m_reg_corr3,1), 
                                           nrow = 3), 
                            empirical = TRUE))
            m_reg_df<- rename(m_reg_df, 
                              c("X1"="Drinking", "X2" ="Reward_Seek","X3"="Age"))
            rand_row = as.integer(runif(2,min = 1, max = length(m_reg_df$Drinking)))
            rand_col = as.integer(runif(2, min = 1, max = 2))
            m_reg_df[rand_row[1],rand_col[1]]<- m_reg_df[rand_row[1],rand_col[1]] + (5*sd(m_reg_df[,rand_col[1]]))
            m_reg_df[rand_row[2],rand_col[2]]<- m_reg_df[rand_row[2],rand_col[2]] + (5*sd(m_reg_df[,rand_col[2]]))
            m_reg_df
        } else  {
            m_reg_df <-
                data.frame(
                    mvrnorm(n = input$m_reg_sample, 
                            mu = c(input$m_reg_m1,input$m_reg_m2,input$m_reg_m3), 
                            Sigma = matrix(c(1, input$m_reg_corr1, input$m_reg_corr2,
                                             input$m_reg_corr1,1,input$m_reg_corr3,
                                             input$m_reg_corr2,input$m_reg_corr3,1), 
                                           nrow = 3), 
                            empirical = TRUE))
            
            m_reg_df<- rename(m_reg_df, 
                              c("X1"="Drinking", "X2" ="Reward_Seek","X3"="Age"))

            rand_row = as.integer(runif(3,min = 1, max = length(m_reg_df$Drinking)))
            rand_col = as.integer(runif(3, min = 1, max = 2))
            m_reg_df[rand_row[1],rand_col[1]]<- m_reg_df[rand_row[1],rand_col[1]] + (5*sd(m_reg_df[,rand_col[1]]))
            m_reg_df[rand_row[2],rand_col[2]]<- m_reg_df[rand_row[2],rand_col[2]] + (5*sd(m_reg_df[,rand_col[2]]))
            m_reg_df[rand_row[3],rand_col[3]]<- m_reg_df[rand_row[3],rand_col[3]] + (5*sd(m_reg_df[,rand_col[3]]))
            m_reg_df
        }
    })


    
################################################################      
# Simulation sample covariance for specified loadings
    
    sim_cfa_text <- renderText({
        input$sim_fact_coeff
    })
    
    output$sim_model <- renderText({
        sim_cfa_text()
    })
    
    sem_data <- eventReactive(input$run5, {
        set.seed(1) 
        model_population <-paste0(sim_cfa_text())
        
        model_cfa <- gsub("#Factor corr.*","", paste0(sim_cfa_text()))
        
        #model_population <- glue("
        #A =~ {input$sem_a1}*a1 + {input$sem_a2}*a2 + {input$sem_a3}*a3 + {input$sem_a4}*a4
        #B =~ {input$sem_b1}*b1 + {input$sem_b2}*b2 + {input$sem_b3}*b3 + {input$sem_b4}*b4
        #C =~ {input$sem_c1}*c1 + {input$sem_c2}*c2 + {input$sem_c3}*c3 + {input$sem_c1}*c4
    #
        #A~~ {input$sem_A_to_B}*B
        #A~~ {input$sem_A_to_C}*C
        #B~~ {input$sem_B_to_C}*C
        #")
        #
        #model_cfa<- glue("
        #A =~ {input$sem_a1}*a1 + {input$sem_a2}*a2 + {input$sem_a3}*a3 + {input$sem_a4}*a4
        #B =~ {input$sem_b1}*b1 + {input$sem_b2}*b2 + {input$sem_b3}*b3 + {input$sem_b4}*b4
        #C =~ {input$sem_c1}*c1 + {input$sem_c2}*c2 + {input$sem_c3}*c3 + {input$sem_c1}*c4
        #
        #A~~ B
        #A~~ C
        #B~~ C
        #")
        
        sim_data <- as.data.frame(
            sim(nRep = 1, model = model_cfa, n = input$sem_sample, 
                generate = model_population, std.lv = TRUE, lavaanfun = "cfa", 
                dataOnly=T, meanstructure = TRUE, seed=123))
    }
    )
    
    
################################ 
## Mean Histogram plots
################################ 

    
    output$hist_var1 <- renderPlot({
        ggplot(data(), aes(x = var1, y = "")) +
            geom_boxplot() +
            geom_jitter(color="black", size=0.4, alpha=0.9) +
            ggtitle("Fig 1. Var 1 Mean/SD") +
            theme_minimal()
    })
    
    output$hist_var2 <- renderPlot({
        ggplot(data(), aes(x = var2, y = "")) +
            geom_boxplot() +
            geom_jitter(color="black", size=0.4, alpha=0.9) +
            ggtitle("Fig 2. Var 2 Mean/SD") +
            theme_minimal()
    })


##########################
### t - tests
##########################
    
### One Sample t-test
    output$one_t_formula <- renderUI({
        
        withMathJax(
            print(paste0("$$ t = \\frac{m-\\mu}{s//\\sqrt{n}} $$"))
        )
    })
    
    output$one_t_table <- function(){
        data() %>% 
            gather(key = "Variable", value = "Data", var1:var2) %>% 
            group_by(Variable) %>% 
            dplyr::summarise('Sample Size (N)' = n(), 
                             'Mean' = mean(Data),
                             'SDs' = sd(Data)) %>% 
            kable(digits = 2) %>%
            kable_styling("striped",
                          full_width = T, font_size = 12, html_font = 'Times') 
    }
    
    output$t_test_dist <- renderPlot({
        
        data <- data.frame(n_dist = rnorm(n = input$sample, 
                                          mean = input$mean1, 
                                          sd = input$sd1))
        
        ggplot(data = data, aes(x = n_dist)) +
            geom_histogram(aes(y = ..density..), bins = 30,
                           colour = "black",
                           fill = "white") +
            stat_function(fun = dnorm,
                          args = list(mean = mean(data$n_dist), 
                                      sd = sd(data$n_dist))) +
            geom_vline(xintercept = mean(data$n_dist)+sd(data$n_dist), linetype = "dashed",
                       color = "blue", size = .5)+
            geom_vline(xintercept = mean(data$n_dist)-sd(data$n_dist), linetype = "dashed",
                       color = "blue", size = .5)+
            geom_vline(xintercept = mean(data$n_dist)+(sd(data$n_dist)*2), linetype = "dashed",
                       color = "green", size = .5)+
            geom_vline(xintercept = mean(data$n_dist)-(sd(data$n_dist)*2), linetype = "dashed",
                       color = "green", size = .5)+
            geom_vline(xintercept = mean(data$n_dist)+(sd(data$n_dist)*3), linetype = "dashed",
                       color = "black", size = .5)+
            geom_vline(xintercept = mean(data$n_dist)-(sd(data$n_dist)*3), linetype = "dashed",
                       color = "black", size = .5)+
            geom_vline(xintercept = input$pop_mean, linetype = "solid",
                       color = "red", size = 1)+
            labs(title = "Fig 3. Distribution +/- SD around Sample Mean, compared to Population Mean", 
                 caption = "Red solid: Pop Mean; Blue Dashed, +/- 1sd; Green Dashed, +/- 2sd; Black, +/- 3SD")+
            xlab("Normal Distribution")+
            ylab("Density (bin = 30)")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
    })
    
   # output$t_test1 <- renderPrint({
   #     onet_1 <- t.test(x = data()$var1, 
   #                      mu = input$pop_mean,
   #                      conf.level = input$alpha) 
   #     
   #     cat("t-stat:", round(as.numeric(onet_1$statistic),2),
   #         "DF:", round(as.numeric(onet_1$parameter),3),
   #         "p-value: ", as.numeric(onet_1$p.value))
   # })
    
   # output$t_test2 <- renderPrint({
   #     onet_2 <- t.test(x = data()$var2,
   #                      mu = input$pop_mean,
   #                      conf.level = input$alpha) 
   #     
   #     cat("t-stat:", round(as.numeric(onet_2$statistic),2),
   #         "DF:", round(as.numeric(onet_2$parameter),3),
   #         "p-value:", as.numeric(onet_2$p.value))
   # })
    
    output$t_test_dist_t <- renderPlot({
        data <- data()
        onet_1 <- t.test(x = data()$var1,
                         mu = input$pop_mean,
                         conf.level = 1-input$alpha)
        
        plot(onet_1)
    }
    )
    
    output$t_test_dist_t2 <- renderPlot({
        data <- data()
        onet_2 <- t.test(x = data()$var2,
                         mu = input$pop_mean,
                         conf.level = 1-input$alpha)
        
        plot(onet_2)
    }
    )
    
    
    output$one_t_form_denom <- renderUI({
        
        withMathJax(
            print(paste0("$$ denominator = {s//\\sqrt{n}} $$"))
        )
    })
    
    output$t_test_reduc_sd_1 <- renderPlot({
        data <- data()
        datalist = list()
        
        for (i in seq(5,2000,5)) {
            
            sd1 <- (input$sd1)/sqrt(i) 
            datalist[[i]] <- data.frame('SampleSize' = i, "SD" = sd1)
        }
        
        data_sd <- do.call(rbind, datalist)
        
         
            ggplot(data = data_sd, aes(x = SampleSize, y = SD)) +
                       geom_line()+
                       labs(title = "Fig 4. Standard deviation scaled by Sample Size for *Variable 1*", 
                            caption = "Sample Size range: 5 to 2000 in intervals 5")+
                       xlab("Sample Size")+
                       ylab("Denominator: One-Sample t-test")+
                       theme(text = element_text(size = 12, family = "times"))+
                       theme_minimal()
    })
    
### Two sample t-test
    output$two_mean_t_formula <- renderUI({
        
        withMathJax(
            print(paste0("$$ \\frac{m_A - m_B}{\\sqrt{ \\frac{S^2}{n_A} + \\frac{S^2}{n_B} }} $$"))
        )
    })
    
    output$two_mean_dist <- renderPlot({
        
        plot_2grp_corr <- ggplot(data = data(), aes(x = var1, y = grp_values)) +
            geom_point(aes(colour = groups)) +
            geom_smooth(method = "lm", se = FALSE, aes(colour=groups))+
            geom_smooth(method = "lm", se = FALSE, color = "black")+
            labs(title = "Fig 4. Correlation between Var 1 & Var 3 (group values) and Different Groups")+
            xlab("Variable 1")+
            ylab("Variable 3")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
        
        
        plot_2grp_means <- ggplot(data = data(), aes(x = groups, y = grp_values)) +
            geom_boxplot(aes(colour = groups))+
            geom_point(size = 1/input$sample, position = "jitter", aes(colour = groups)) +
            labs(title = "Fig 5. Means and Distribution of Two Group Means for Variable 1")+
            xlab("")+
            ylab("Group Values Observed")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
        
        plot_2grp_corr / plot_2grp_means + plot_layout(guides = 'collect')
    }) 
    
    
    #output$two_mean_test1 <- renderPrint({
    #    data <- data()
    #    two_mean_t <- t.test(data$grp_values ~ data$groups) 
    #    
    #    cat("Two Mean t-stat:", round(as.numeric(two_mean_t$statistic),2),
    #        "DF:", round(as.numeric(two_mean_t$parameter),3),
    #        "p-value:", two_mean_t$p.value,
    #        "Confidence Interval", round(two_mean_t$conf.int[1],3), "to", round(two_mean_t$conf.int[2],3))
    #})
    
    output$two_mean_table <- function(){
        data <- data()
        
        data %>% 
            group_by(groups) %>% 
            dplyr::summarise('Sample Size (N)' = n(), 
                      'Group Means' = mean(grp_values),
                      'Group SDs' = sd(grp_values)) %>% 
            kable(digits = 2) %>%
            kable_styling("striped",
                          full_width = T, font_size = 12, html_font = 'Times') 
    }
    
    
    output$two_mean_test1 <- renderPlot({
        
        data <- data()
        two_mean_t <- t.test(data$grp_values ~ data$groups,
                             conf.level = 1-input$alpha)
        
        plot(two_mean_t)
    }
    )
    
    output$two_mean_t_form_denom <- renderUI({
        
        withMathJax(
            print(paste0("$$ {\\sqrt{ \\frac{S^2}{n_A} + \\frac{S^2}{n_B} }} $$"))
        )
    })
    
    output$twomean_reduc_sd_1 <- renderPlot({
        data <- data()
        datalist = list()
        
        for (i in seq(5,2000,5)) {
            
            sd1 <- sqrt((input$grp_sd1^2/i)+(input$grp_sd2^2/i))
            datalist[[i]] <- data.frame('SampleSize' = i, "SD" = sd1)
        }
        
        data_sd <- do.call(rbind, datalist)
        
        
        ggplot(data = data_sd, aes(x = SampleSize, y = SD)) +
            geom_line()+
            labs(title = "Fig 5. Standard deviation scaled by Sample Size for two groups with equal N", 
                 caption = "Sample Size range: 5 to 2000 in intervals 5")+
            xlab("Sample Size")+
            ylab("Denominator: Two-Sample t-test")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
    })
    
    
    
##########################
### Correlation
##########################
    
    
    output$corr_formula_p <- renderUI({
        
        eq <- paste0("r = \\frac{\\sum{(x-m_x)(y-m_y)}}{\\sqrt{\\sum{(x-m_x)^2}\\sum{(y-m_y)^2}}}")
        eq2 <- paste0("rho = \\frac{\\sum(x' - m_{x'})(y'_i - m_{y'})}{\\sqrt{\\sum(x' - m_{x'})^2 \\sum(y' - m_{y'})^2}}")
        withMathJax(
            print(paste0("$$",eq,"$$"))
        )
    })
    
    output$corr_formula_rho <- renderUI({
        
        eq2 <- paste0("rho = \\frac{\\sum(x' - m_{x'})(y'_i - m_{y'})}{\\sqrt{\\sum(x' - m_{x'})^2 \\sum(y' - m_{y'})^2}}")
        withMathJax(
            print(paste0("$$",eq2,"$$"))
        )
    })
    
    output$corr_test <- function(){
    
            co_t1 <- cor.test(x = sim_corr()$X1, 
                              y = sim_corr()$X2,
                              method = "pearson")
            
        
            co_t2 <- cor.test(x = sim_corr_2()$X1, 
                              y = sim_corr_2()$X2,
                              method = "pearson")
            
            data.frame("Pearson r"= c(round(as.numeric(co_t1$estimate),2),
                                        round(as.numeric(co_t2$estimate),2)), 
                       "p-value"=c(as.numeric(co_t1$p.value),
                                   as.numeric(co_t2$p.value)),
                       row.names = c("Var 1 ~ Var 2", "Var 3 ~ Var 4")) %>% 
                kable() %>% 
                kable_styling("striped",
                          full_width = F, font_size = 12, html_font = 'Times') 
    }
    
   # output$corr_test1 <- renderPrint({
   #     co_t1 <- cor.test(x = sim_corr()$X1, 
   #                       y = sim_corr()$X2,
   #                       method = "pearson")
   #     
   #     cat("r: ",
   #         round(as.numeric(co_t1$estimate),2),
   #         "and p-value: ",
   #         as.numeric(co_t1$p.value))
   # })
   # 
   # output$corr_test2 <- renderPrint({
   #     co_t2 <- cor.test(x = sim_corr_2()$X1, 
   #                       y = sim_corr_2()$X2,
   #                       method = "pearson")
   #     cat("r: ",
   #         round(as.numeric(co_t2$estimate),2),
   #         "and p-value: ",
   #         as.numeric(co_t2$p.value))
   # })
    
    output$corr_test3 <- function(){
        co_t1 <- cor.test(x = sim_corr()$X1, 
                          y = sim_corr()$X2,
                          method = "spearman")
        
        data.frame("Spearman rho"= c(round(as.numeric(co_t1$estimate),2)), 
                    "p-value"=c(as.numeric(co_t1$p.value)),
                    row.names = c("Var 1 ~ Var 2")) %>% 
            kable() %>% 
            kable_styling("striped",
                          full_width = F, font_size = 12, html_font = 'Times')
    }
    
#### Plots
    output$corr_plot1 <- renderPlot({
        
        ggplot(sim_corr(), aes(x = X1, y = X2)) +
            geom_smooth(method = "lm", se = FALSE)+
            geom_point()+
            geom_rug(size = .3, position = "jitter", colour = "sienna4")+
            theme_minimal()
    })
    
    output$corr_plot2 <- renderPlot({
        
        ggplot(sim_corr_2(), aes(x = X1, y = X2)) +
            geom_smooth(method = "lm", se = FALSE)+
            geom_point()+
            geom_rug(size = .3, position = "jitter", colour = "sienna4")+
            labs(title = "Fig 2. Association between Var 3 and Var 4", 
                 caption = "*Data simulated based on specified parameters")+
            theme_minimal()
    })
    
##########################
### Stability Correlation
##########################   
    output$EffNchange <- renderPlot({
        
            ggplot(stable_DF(), aes(x = i, y = Pearson_r)) +
            geom_line(aes(y = Pearson_r)) +
            xlab("Sample Size") +
            ylab("Pearson's r")+
            scale_y_continuous(
                name = "Pearson's r") +
            geom_vline(xintercept = 100, color = "Red", size = .4)+
            labs(title = "Fig 1. Pearson's r stability w/ N", caption = "Red line = Sample Size: 100")+
            theme(text = element_text(size = 30, family = "times"))+
            theme_minimal() 
        
        
    })
    
    output$EffNchange2 <- renderPlot({
        
        stable_DF() %>% 
            gather(key = "CorrelationType", 
                   value = "Effect", 
                   Pearson_r,Spearman_rho,Kendall_t) %>% 
            ggplot(aes(x = i, y = Effect)) +
            geom_line(aes(colour = CorrelationType))+
            xlab("Sample Size") +
            ylab("Correlation Size") +
            labs(title = "Fig 2. Pearson v. Spearman v. Kendall similarity w/ N")+
            theme(text = element_text(size = 30, family = "times"))+
            theme_minimal() 
        
    })

    
##########################
### Linear Regression
##########################
    output$m_reg_1 <- function(){
        fit<-lm(Drinking ~ Reward_Seek, m_reg_data())
        inter<- round(summary(fit)$coefficients[1,1],3)
        p1 <- summary(fit)$coefficients[2,4]
        beta1 <- round(summary(fit)$coefficients[2,1],3)
        r1 <- round(summary(fit)$r.squared,3)
        r2 <- round(summary(fit)$adj.r.squared,3)
        f_stat <- round(as.numeric(summary(fit)$fstatistic[1]),3)
        df1 <- as.numeric(summary(fit)$fstatistic[2])
        df2 <- as.numeric(summary(fit)$fstatistic[3])
        model_p <- 1-pf(q = f_stat, df1 = df1, df2 = df2)
        
        data.frame("Intercept" = c(inter,""),
                   "Beta"= c(beta1,""), 
                   "p-value"=c(p1,""),
                   "F-stat"=c(f_stat,""),
                   "DF"=c(df1,df2),
                   "Model P"=c("",model_p),
                   "R-square"=c(r1,""),
                   "R-adjust"=c("",r2)) %>% 
            kable() %>% 
            kable_styling("striped",
                          full_width = F, font_size = 12, html_font = 'Times') 

    }
    
    output$m_reg_formula_1 <- renderUI({
        fit<-lm(Drinking ~ Reward_Seek, m_reg_data()) 
        eq <- paste0(extract_eq(fit))
        withMathJax(
            print(paste0("$$",eq,"$$"))
            )
    })

##########################
### Multiple Regression
##########################
    
    output$m_reg_2 <- function(){
        fit2<-lm(Drinking ~ Reward_Seek + Age, m_reg_data())
        inter<- round(summary(fit2)$coefficients[1,1],3)
        p1 <- summary(fit2)$coefficients[2,4]
        beta1 <- round(summary(fit2)$coefficients[2,1],3)
        p2 <- round(summary(fit2)$coefficients[3,4],4)
        beta2 <- round(summary(fit2)$coefficients[3,1],3)
        r1 <- round(summary(fit2)$r.squared,3)
        r2 <- round(summary(fit2)$adj.r.squared,3)
        f_stat <- round(as.numeric(summary(fit2)$fstatistic[1]),3)
        df1 <- as.numeric(summary(fit2)$fstatistic[2])
        df2 <- as.numeric(summary(fit2)$fstatistic[3])
        model_p <- 1-pf(q = f_stat, df1 = df1, df2 = df2)
            
            data.frame("Intercept" = c(inter,""),
                       "Beta"= c(beta1,beta2), 
                       "p-value"=c(p1,p2),
                       "F-stat"=c(f_stat,""),
                       "DF"=c(df1,df2),
                       "Model P"=c("",model_p),
                       "R-square"=c(r1,""),
                       "R-adjust"=c("",r2)) %>% 
                kable() %>% 
                kable_styling("striped",
                              full_width = F, font_size = 12, html_font = 'Times') 
            
        }

    output$m_reg_formula_2 <- renderUI({
        fit2<-lm(Drinking ~ Reward_Seek + Age, m_reg_data()) 
        eq <- paste0(extract_eq(fit2))
        withMathJax(
            print(paste0("$$",eq,"$$"))
        )
    })
    

#######      Interactive plot    

    output$m_reg_plot_1 <- renderPlot({
        
        ggplot(m_reg_data(), aes(x = Reward_Seek, y = Drinking)) + 
            geom_point()+
            geom_smooth(method="lm")+
            xlab("Reward Seeking") +
            ylab("Drrinking")+
            labs(title = "Fig 1. Data Plot of Regression Model", caption = "ggplot as ggPredict() not functional")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
        
        #ggiraph(code = print(plt),
        #        hover_css ="fill:red;cursor:pointer;",
        #        selection_type = "multiple",
        #        selected_css = "fill:red;")
    })
    
    output$m_reg_plot_2 <- renderPlot({
        
        fit <- lm(Drinking ~ Reward_Seek, m_reg_data())
        
        m_reg_data_2 <- m_reg_data() %>% 
            mutate(
                predict_vals = fit$fitted.values,
                resid_vals = fit$residuals
            )
        
        plot1<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = predict_vals)) +
            geom_point()+
            geom_smooth(method="lm")+
            labs(title = "Fig 2. Fit Test: Fitted Drinking ~ Reward", 
                 caption = "*Output from lm(Drinking ~ Reward, data = simulated)")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
        
        plot2<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = predict_vals)) +
            geom_line(size = 1)+
            geom_point(aes(x = Reward_Seek, y = Drinking), size =1/input$m_reg_sample) +
            geom_segment(
                aes(xend = Reward_Seek, yend = Drinking),
                size = .2, alpha = .2, lineend = "round")+
            labs(title = "Fig 3. Fit Test: Fitted Drinking ~ Observed Drinking", 
                 caption = "Fitted values out of lm()$fitted.values & observed simulated")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
        
        
        plot3<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = resid_vals)) +
            geom_point(size =1/input$m_reg_sample) +
            geom_smooth(method="loess")+
            labs(title = "Fig 4. Association between Residuals ~ Reward", 
                 caption = "Residuals from lm()$residuals & observed simulated")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
        
        plot4<- ggplot(m_reg_data_2, aes(x = Drinking, y = resid_vals)) +
            geom_point(size =1) +
            geom_smooth(method="loess")+
            labs(title = "Fig 5. Association between Residuals ~ Drinking", 
                 caption = "Residuals from lm()$residuals & observed simulated")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
        
        (plot1 | plot2) / (plot3 | plot4)
    })
    
    
    output$m_reg_plot_3 <- renderPlot({
        
        fit2 <- lm(Drinking ~ Reward_Seek + Age, m_reg_data())
        
        m_reg_data_2 <- m_reg_data() %>% 
            mutate(
                predict_vals = fit2$fitted.values,
                resid_vals = fit2$residuals
            )
        
        plot5<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = predict_vals)) +
            geom_point()+
            geom_smooth(method="lm")+
            labs(title = "Fig 6. Fit Test: Fitted Drinking ~ Reward", 
                 caption = "*Output from lm(Drinking ~ Reward + Age, data = simulated)")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
        
        plot6<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = predict_vals)) +
            geom_line(size = 1)+
            geom_point(aes(x = Reward_Seek, y = Drinking), size =1/input$m_reg_sample) +
            geom_segment(
                aes(xend = Reward_Seek, yend = Drinking),
                size = .2, alpha = .2, lineend = "round")+
            labs(title = "Fig 7. Fit Test: Fitted Drinking ~ Observed Drinking", 
                 caption = "Fitted values out of lm()$fitted.values & observed simulated")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
        
        
        plot7<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = resid_vals)) +
            geom_point(size =1/input$m_reg_sample) +
            geom_smooth(method="loess")+
            labs(title = "Fig 8. Association between Residuals ~ Reward", 
                 caption = "Residuals from lm()$residuals & observed simulated")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
        
        plot8<- ggplot(m_reg_data_2, aes(x = Drinking, y = resid_vals)) +
            geom_point(size =1) +
            geom_smooth(method="loess")+
            labs(title = "Fig 9. Association between Residuals ~ Drinking", 
                 caption = "Residuals from lm()$residuals & observed simulated")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
        
        (plot5 | plot6) / (plot7 | plot8)
    })
    
   # output$m_reg_plot_5 <- renderPlot({
   #     
   #     fit_plot <- lm(Drinking ~ Reward_Seek + Age, m_reg_data())
   #     
   #     plot(check_outliers(fit2))
   #     
   # })
    
    
    
##########################
### CFA/PCA
##########################  
    cfa_text <- renderText({
        input$cfa_model
    })
    
    output$cfa_spec_model <- renderText({
        cfa_text()
    })
    
    output$sem_corr <- function(){
    
    cor <- cor(sem_data(), method = "pearson")
    
    cor %>% 
        kable() %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
    }
    
    
    cfa_data <- eventReactive(input$run5, {
        set.seed(1) 

        model_cfa <-paste0(cfa_text())
        
        cfa(model = model_cfa,
            data = sem_data())
    })
    
    output$sem_table <- renderTable({
        set.seed(1) 
        
        parameters(cfa_data(), standardize = print(input$standardize))

    }
    )

    
    output$sem_plot <- renderPlot({
        set.seed(100) 
        
        semPlot::semPaths(object = cfa_data(),
                 style = "lisrel", whatLabels = "std",intercepts = TRUE,residuals = TRUE,
                 font = 14, edge.label.font = 12, edge.label.cex = 1.2, edge.color = "black",
                 layout = "tree")
    }
    )


}

################################ 
# run app
shinyApp(ui = UserInferface, 
         server = local_server)
