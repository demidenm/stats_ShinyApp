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
library(Superpower)#anova simulate file:///Users/michaeldemidenko/Downloads/0.1_Simulation_Based_Power_Analysis_For_Factorial_ANOVA_Designs.pdf
library(multcomp) # anova tukey

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



UserInferface <- navbarPage("Ooo, So Shiny!",
                            collapsible = TRUE, 
                            inverse = TRUE, 
                            theme = shinytheme("simplex"),

                            tabPanel("Intro",
                                     fluidPage(
                                         sidebarLayout(position = "right",
                                             sidebarPanel(style = "display: inline-block; float: center",
                                                 actionButton("start", "Onward!",
                                                              style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00;")
                                             ),
                                             mainPanel(
                                                 h2("Another day another shiny stat"),
                                                 p("This shiny app is a work-in-progress. Last updated October 21, 2021",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("Its purpose? To play with data. There are several tabs, select each tab to simulate some data based one the input provided, such as:
                                                 means, standard deviations, Pearson's r and/or sample size.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("The app works best after you select parameters and click the 'Onward!' button. This button keeps the lights running. 
                                                 Whether it's to visualize the effect of the crud factor (see Meehl, 1990), variance/standard deviation (SD), sample (N) at which a correlation stabilizes 
                                                 (see Schönbrodt & Perugini (2013)), or the effect of an outlier, click a tab, play around with some parameters and see how 
                                                 some outputs change. Keep in mind, with the exception of when outliers are included, the data is generated using a 
                                                   normal distribution. Often our data -- in particular in the social sciences -- has values that are densely packed around the 
                                                   the mean (leptokurtic, or positive kurtosis), flatly distributed (platykurtic, or negative kurtosis), heavily weighted distribution left to 
                                                   mean values (positive skew), heavily weighted distribution to the right of the mean (negative skew), or zero-inflated values.
                                                   Most simulations DO NOT include the effects of this on models. In future updates, I hope to include tabs on this topic",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("Of note: The data will be simulated each time a sample size value is changed. If you're wondering, why is there so much white space between
                                                 text? This will change once you click the button to run the appropriate simulation. You'll notice that the mean/sd will not be *exactly* the specified mean.
                                                   The sampling is being done from a random normal distribution, so after sampling the specified span the values in the simulated dataset will bounce 
                                                   around those numbers a tiny bit. Furthermore, each time the sample N is changed,
                                                   the data set is resimulated. Thus, they won't be exact, but approximate is the goal. It's for fun, after all...",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 p("Documentation for this Shiny app is available on Github: https://github.com/demidenm/stats_ShinyApp",
                                                   style = "font-family: 'times'; font-size:10px")
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
                                                             min = 0, value = 0, max = 30, step = .5),
                                                 sliderInput("alpha", label = "Alpha α (Type I error rate)",
                                                             min = 0.0001, value = .05, max = .20, step = .025),
                                                 sliderInput("mean1", label = "Var  *1* Mean ",
                                                             min = 0, value = 5, max = 30, step = .5),
                                                 sliderInput("sd1",  label = "Var *1* St Dev",
                                                             min = 0.1, value = 3, max = 25, step = .5),
                                                 sliderInput("mean2", label = "Var *2* Mean",
                                                             min = 0, value = 5, max = 30, step = .5 ),
                                                 sliderInput("sd2", label = "Var *2* St Dev",
                                                             min = 0.1, value = 3, max = 25, step = .5),
                                                 sliderInput("grp_m1", label = "Group A Mean",
                                                             min = 0, value = 5, max = 30, step = .5),
                                                 sliderInput("grp_sd1",  label = "Group A St Dev",
                                                             min = 0.1, value = 3, max = 25, step = .5),
                                                 sliderInput("grp_m2", label = "Group B Mean",
                                                             min = 0, value = 5, max = 30, step = .5 ),
                                                 sliderInput("grp_sd2", label = "Group B SD",
                                                             min = 0.1, value = 3, max = 25, step = .5)
                                                 
                                             ),
                                             mainPanel(
                                                 h2("Description: Distribution & Effect for Mean-SD-Sample"),
                                                 p("What is the purpose of this tab? Based on the means, standard deviations (SD), and sample size (N) that you selected 
                                                   (or their defaults), a normally distributed dataset is generated. This data is then used to plot a boxplot of the two 
                                                   variables to visualize means/SD to see how data points are distributed relative to the mean. Then, the distribution of 
                                                   a single variable and the population mean is plotted. After which, we compute a One-Sample t-test on the mean(s) and SD(s). 
                                                   Thereafter, using our simulated data we consider the unique group means for the combined means of two variables. 
                                                   This data is plotted, and a Two-Sample independent t-test is conducted. For both the One-Sample and Two-Sample t-test 
                                                   their respective formulates are provided and the effect of sample size is considered. In general, this tab is to represent 
                                                   changes in the values based on: Mean, Standard Deviation, Sample Size, Alpha and Population Mean (which is often 
                                                   hypothesized to be nil).",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("Note: To run/rerun, simply specify your values and click 'Onward!'. White space will remain until data can be 
                                                   simulated by pushing the 'Onward!' button and the respective Figures/Tables are populated",
                                                   style = "font-family: 'times'; font-size:10px; font-style: italic"),
                                                 br(),
                                                 br(),
                                                 h4("Simulated output",
                                                    style = "font-family: 'times'"),
                                                 p("First, we started by plotting two boxplots for the simulated Var1 and Var2 in Figures 1 and Figure 2. The boxplots 
                                                   below represent the means and SDs for Var 1 and Var 2. You can see the data points normally distributed around the 
                                                   mean. As you toggle the sample size, you'll notice the central tendency arise. However, the central tendency is more 
                                                   obvious below in Figure 3.",
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
                                                 p("Here, Figure 3 reflects the simulated distribution of Var 1 (based on specified mean) and 
                                                   +/- 1, 2 and 3 standard deviations relative to this mean. For reference, the specified population mean 
                                                   (that is used in the One-Sample t-test below) is also plotted in the figure. Since the data is resampled 
                                                   for this normal distribution (i.e., density plot) the mean will vary a touch. One thing to notice from 
                                                   the normal distribution is where most of the data falls. For example, approx. 68% of the observations 
                                                   are between -/+1 SDs of the sample mean, or the dashed blue line, and approx. 95% of observations are 
                                                   within -/+ 2 SDs of the sample mean, or the green line. Then, approx. 5% of the observations are beyond 
                                                   the -/+2 SD line. These add up to the cumulative distribution function for the normal distribution and 
                                                   may help in calculating outliers, which is the occurrence of extreme and unexpected values.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "t_test_dist"),
                                                 h4("One Sample t-test: Formula & Effect of Parameters",
                                                   style = "font-family: 'times'"),
                                                 p("Keeping in mind how the data shifts around the mean, we can now consider the One-Sample t-test formula. If you ever came 
                                                   across the language, “So-and-so sample demonstrated a response (Mean = 2.9, SD = 2.0) that was significantly greater than the null 
                                                   t(49) = 49/0, p < .00001”, then you had experience a One-Sample t-test. The purpose of the One-Sample t-test is to estimate a 
                                                   t-value (t) based on the population mean (μ) and the sample mean (m) relative to proportion of variance/SD (s) scaled by 
                                                   the population size (√n)",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 uiOutput(outputId = "one_t_formula"),
                                                 p("For example, say I am a grocery store manager that is interested in creating a sign that'll implicitly motivate customers to stop 
                                                   at a particular location inside the store. We're not trying to compare the effect of one sign over the other *yet*, but rather 
                                                   we are curious whether there is any effect of the sign in this part of the store. We created a sign and machine learning 
                                                   (i.e., a research assistant with a stopwatch) that determines when a participant (customer) enters the defined vicinity using their 
                                                   stop watch calculates (approximately) how long they stopped for. After a few long weeks and a really tired research assistant and worn 
                                                   out stop watch, we will have a dataset of length N sample of participants that stopped for an average time (Var 1 mean) and these 
                                                   participants varied in their stopping time to some degree (Var 1 St Dev). Unless we have a prior estimate of how long populations 
                                                   randomly stop in this given area, we will assume that population mean stopping time is zero (implicit assumption in a lot of scientific research). 
                                                   So, we can propose the null (H0) hypothesis, that there would be no difference between how long people normal stop in this space (zero/null) 
                                                   and with our sign there. The alternative (H1) hypothesis would be the opposite, that we expect a significant difference between the population 
                                                   mean and our observed mean with the sign there. ",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("To obtain our estimate of significance we would use the One-Sample t-test. To simplify the formula above, we would like to obtain a 
                                                   t-statistic that compares the signal in the numerator (what we want to maximize, stopping at sign) relative to the noise in the denominator 
                                                   (what we want to limit, the variability between people). This maximization of the signal-to-noise ratio is central to experimental psychology 
                                                   (Cronbach, 1957). To maximize the signal, we can increase the sample mean in comparison to the population mean (μ). Another way to increase 
                                                   the signal is by minimizing the noise, or our SD. This can be done by reducing variability *between* subjects or measurement error. 
                                                   The error can also be minimized by a larger N, as the SD is scaled by sample size (see Figure 4 for example).",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 tableOutput(outputId = "one_t_table"),
                                                 p("The above table provides the sample size (should be equal), means and SDs you input into the fields on the upper right hand. 
                                                   Since we're working with a One-sample t-test, let's pay attention to Var1 for now. You can follow along by entering each value 
                                                   into the One-Sample t-test formula manually (since it’s relatively easy). Such that, for the formula your ‘Var1 Mean’ will replace 
                                                   'm' and ‘Var 1 SD’ will replace ‘s’, your ‘sample size’ will replace ‘n’ and the population mean you entered will replace 'μ'. 
                                                   For simplicity purposes, I have a Probability density plot for a two-tailed t-test below that includes your t-statistic (t), 
                                                   your degrees of freedom (df), which are the number of observations minus 1 (n-1) and the p-value. In the One-Sample t-test 
                                                   probability density plot below, you will see the critical cut off (alpha/p-val) of 95% (p < .05) and the blue dot represents 
                                                   the t-statistics from our variable. If you did your math correctly, your t-statistic should be the same. Unless something went wrong… 
                                                   and it’s not unknown for things to go wrong. By pulling up a t-distribution, you can find the provided significant, too. You simply 
                                                   identify the t-statistic, the degrees of freedom (n – 1) and one-tailed or two-tailed. ",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("What we can gather from this is the significance of our effect for *this* sample and its size and variability. This significance, again, 
                                                   is relative to our population mean. So, while it is common to say that the population mean is zero, this often is not true for many scenarios. 
                                                   For instance, we may find that people in some parts of the store tend to stop for no reason, so the population mean may be, say, 11 seconds. 
                                                   So, if we had known that the population mean was closer to 11 seconds, but assumption that a population mean of zero would be incorrect and 
                                                   thus provide the wrong conclusion. We may, in effect (pun very much intended), find significance when in fact there may be none if we were to 
                                                   use a specific (point estimate) versus a zero (point null) estimate. This topic has been extensively discussed and is a point of contention 
                                                   in several statistical tests.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "t_test_dist_t"),
                                                 p("It’s worthwhile to look at the probability densities for the t-statistics. Given that you have two variables (Var 1 & Var 2) 
                                                   that you can adjust the mean and SD for, play around with these parameters. Look at the above T-test formulate again and observe 
                                                   how the 'significance' of the effect sways based on these values. You can compare the above plot for Var 1 and the below plot for 
                                                   Var 2. While these tests can tell us something meaningful, if we’re not thoughtful when using them we may misunderstand the 
                                                   meaningfulness of an effect because we are at times consumed by finding that “p < .05” golden ticket.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "t_test_dist_t2"),
                                                 br(),
                                                 p("I mentioned earlier to consider the denominator of the One Sample t-test formula. If you messed around with different 
                                                   combinations of mean, SD and sample size (N) values, you may have noticed an important trend: the larger the sample, the 
                                                   larger t-statistic and thus the larger the p-value. This represents the issue of the ‘crud factor’, whereby the larger number 
                                                   of participants the lower the critical threshold for significant is. Thus, larger N provide significance for small effects. 
                                                   Then you reach the point of deciphering, is an effect this tiny important? Sometimes, sadly, the answer is a clear: no. 
                                                   Other times it’s a resounding… maybe?",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("Here the effect of the crud fact is in essence achieved by the SD (our variability) being shrunk by the N. To demonstrate this, 
                                                   we can take the mean of Var 1 and see how the N proportionally reduces the influence of the SD from the denominator on the numerator. 
                                                   In term of fractions, if we make the denominator SMALLER we can make the result of the numerator LARGER. Figure 4 shows how the 
                                                   SD shrinks in magnitude from N = 5 to 2000.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 uiOutput(outputId = "one_t_form_denom"),
                                                 plotOutput(outputId = "t_test_reduc_sd_1"),
                                                 br(),
                                                 br(),
                                                 h4("Two Independent Group Means",
                                                   style = "font-family: 'times'"),
                                                 p("For now, we have focused on the One-Sample t-test, which considers a mean and SD for one sample/group of subjects. However, say we wanted to consider 
                                                 the difference BETWEEN groups, so two means and SDs. Going back to our earlier example from the grocery store (keep coming back to the grocery example, 
                                                 trying to remind myself to get groceries…). Let's say that we wanted to determine whether there was a meaningful difference in stopping by signs that 
                                                 were placed at two stores in different states, say Dallas, Texas and Lincoln, Nebraska. So now we have the same sign, we have the same grocer, same part 
                                                 of the store, but would like to know whether there is [in simple terms] something meaningfully different between the store in Dallas and Lincoln. If our 
                                                 groups consist of independent samples and variability that is similar (i.e., sample of people in each group are unique and SD is comparable), 
                                                 we can conduct a Two-Sample t-test.
                                                   we hade two groups. ",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 uiOutput(outputId = "two_mean_t_formula"),
                                                 p("As we saw for the One-sample t-test two means in the numerator, we see the same in the Two-Sample t-test. However, unlike the One-sample t-test 
                                                   in the Two-Sample t-test these are both samples that we have observed data for, Sample A and Sample B. So, for both samples, unlike the population, 
                                                   we have their actual means but ALSO their SD(s). Effectively, we can follow similar procedures to compare the sample means to determine the difference, 
                                                   or effect, between groups.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 tableOutput(outputId = "two_mean_table"),
                                                 p("Above is a Table that provides information about simulated data for our groups (A & B), their sample size (N), group means and group SDs. 
                                                   The means for Var 1 and Var 2 are simulated in a manner that will provide you with specified means for Group A and Group B. ",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "two_mean_dist"),
                                                 p("Figure 5 are boxplots with the mean and SD for each group (A & B). Like Figure 1 & 2, you can observe the spread of the raw data points 
                                                   that are simulated for each group. Then, Figure 6 is a visual representation how ignoring groups and looking at variables alone may 
                                                   provide a different association (black line) amongst Var1 and Var2 than if you were to consider those associations across groups 
                                                   (green and yellow). In this scenario the contrast may now be as stark, but as you toggle Var 1, Var 2, Group A and Group B, 
                                                   you may notice distinct differences in the association between Var1 and Var1. For example, when the groups are combined the association 
                                                   may be zero, but when separated, the association may be positive for one group and negative for the other. More on correlations in the 
                                                   ‘Correlation’ and “Stability of Effect’ tabs.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "two_mean_test1"),
                                                 p("As we did for the One-Sample t-test, we again can acquire the t-statistic from our formula and determine the p-value, or significance, of the difference 
                                                   between our groups for [this] sample and distribution. Revisiting our example, we may find the means and SD for stopping at a sign 
                                                   between stores are nearly identical. While we may have observed a significant in a One-Sample t-test, or the effect of stopping due to our sign, 
                                                   this did not change between the samples for grocery store A and grocery store B. ",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "twomean_reduc_sd_1"),
                                                 p("As you keep playing with the parameters, you'll find a similar cautionary tale of 'significance' (or the golden ticket, p < .05). 
                                                   For the Two-Sample t-test, the t-statistic that we get can again be impacted by the scaling of SD in the denominator by the N. 
                                                   As we say in Figure 4 for the One-Sample t-test, in Figure 7, for the Two-Sample t-test, we observe a similar scaling by the 
                                                   sample size. ",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 br(),
                                                 h4("Meaning of p-value in NHST?", style = "font-samily: 'time'"),
                                                 p("As we were going along, one thing you may have noticed is the phrasing of ‘for this sample'. This is an important thing to keep in mind. 
                                                 Most statistics we use in the null hypothesis significance testing (NHST) are based on the parameter (mean, SD, Pearson’s r) distributions. These distributions are often specific to our 
                                                 observations/data/samples.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("In this and other tabs in this shinyapp the p-value is used to represent the impact of means/errors and the 
                                                 crud factor (sample size) on the p-value in the NHST framekwork. As noted in Kruschke & Liddel (2018),
                                                 the p-value is", span("the probability that the observed effect/t-value would be observed if: a) H0/null were true and b) data were 
                                                 sampled according to the same stopping/testing methids as this data.", style = "font-style: italic"), "As mentioned earlier, the mean or 
                                                 mean difference (our signal) is in the numerator and the SD (the noise) is in the denominator. So larger values in numerator and 
                                                 smaller values in the denominator increase the t-statistic and thus 'significance'. The sample size (in the numerator) also does a lot of heavy 
                                                 lifting as it attenuates the impact of BETWEEN subject (or intersubject) variability. So when we use the significance is based on NHST,
                                                 if we meet, we are bound the the interpretation within that sample. A sampling or measurement technique from a different sample
                                                   can produce different distributions and hence results. 
                                                   So generalizability should be tested and not assumed.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 br(),
                                                 p("Packages used - Plotting: ggplot w/ tidyverse | plotting htest t-distributions: webr via plot() by Keon-Woong Moon",
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
                                                 p("What does this tab represent? Similar to the T-test tab and normally distributed dataset is simulated. Unlike the T-test
                                                   tab where we simulate the data solely based on your mean and standard deviation (SD), here your data is generated based on your means
                                                   for four variables and some pre-specified correlation between Var1 & Var2 and Var3 & Var4.",
                                                   "Below, the effect size and p-value is printed to see if/how these change",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("The association between two variables (hence forth indicated by '~') is plotted. Specifically, the correlation between 
                                                   Var1 ~ Var2 and Var3 ~ Var4. We could have specified a correlation among other combinations, such as Var1 ~ Var3, but we will 
                                                   simplify things and not get into that just yet. For now, we will plot the two correlations.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 p("Before we get into the plots, let's first take a look at the formulation for the Pearson's r correlation. The pearson correlation
                                                 provides a coefficient for a linear association between two variables, which can be zero, positive, or negative.
                                                   At first glance, the below formula may look a bit overwhelming. But we can simplify the equation into the numerator and 
                                                   the denominator. The denominator calculates the overall covariation among our variables (such as, when Var1 goes up by 2, Var2 
                                                   may go up 4). Let's first break down (x−m x)(y−m y). 'x' and 'y' are the observed values for our variables. So if we had 10 observations
                                                   of X and Y variables, we would have ten 'x' and 'x' values. Let's say in this case X is the measure of how many miles
                                                   that I ran during a span of 2 hrs for a given day and Y is a measure of how tired I reported I was after a run. I did this 10 times across two weeks.
                                                   Then, the 'm x' and 'm y' are the sample means for each of those values. So if I did 10 runs, for X and Y I would get the mean
                                                   across the 10 observations, respectively.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 uiOutput(outputId = "corr_formula_p"),
                                                 p("Because I like fried chicken and am inconsistent with my running, we will see variability in the distance and fatigue that I report.
                                                   This variability we can leverage to determine the magnitude of the relationship between my running and self-reported fatigue. 
                                                   If I always ran the same distance and never varied in my fatigue, first that would be really weird and second that would offer
                                                   no variability that we may leverage. To get the covariance of across values for Var1 (running) and Var2 (fatigue), we would take the ten sets of (x−m x)(y−m y), such as (3 - 2)(9 - 8), (1.5 - 2)(5 - 8), 
                                                   (3 - 2)(9 - 8), (3 - 2)(8.5 - 8), etc... and sum (∑ simply means summation over)  across those ten iterations. Now we have the numerator. 
                                                   Normally, to get the sample covariance the numerator would be (n - 1), so for ten observations we would divide by 10-1, or 9. 
                                                   HOWEVER, this is a sample constrained numerical value and we want something more standardized and interpretable across studies. 
                                                   The Pearson's correlation achieves this standardized unit via the denominator. It standardizes the numerator by the samples standard deviations for the
                                                   sample. By doing so, we achieve values that will always be constrained to +/- 0.0 to 1.0. ",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("Now that we have collected the data, we can calculate the correlation between Var1 and Var2 by hand, or use Excel, R, SPSS or some other software
                                                   to do the math for us. For simplicity, if you have pushed 'Onward' you will see scatter plots of the simulated data for the specified correlation.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "corr_plot1"),
                                                 p("Figure 1 and Figure 2 provides the strength of your correlation represented by the linear line and the Pearson's r coefficient. Both represent the
                                                   direction and magnitude for the associations. You can play around with sample size, means, and Pearson r coefficient sizes to see how things change.
                                                   You'll notice the bar lines on the perimeter of the graph represent the distribution of the data points, so as data points cluster you'll see the bars
                                                   get darker (which is just clustering of values.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("Just like other statistical tests, people like to see whether a correlation, or association, among two variables is significant. So is there a
                                                 significant association between the distance I ran and how tired I am? The table below reports whether the association between your variables is 
                                                 or is not significant (note: if you see p.value = 0, that simply means that p < 0000000). The p-value is calculated using a t-distribution with n – 2 degrees of freedom.
                                                 The statistical test makes the assumption that the correlation between two variables in the population is zero. So when p < .05 between Var1 and Var2, 
                                                 for example, we are saying the association among these variables in our 
                                                   sample is significantly greater than zero.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("An important caveat with the correlation is while we do get an association between two variables, we cannot know whether one causes the other.
                                                   In our example, if we find that the correlation between running distance and self-reported fatigue is p < .0001, one may make the claim
                                                   that the distance caused the fatigue. While it is a safe bet in my case, as I am out of shape. There are caveats in something even as basic as this.
                                                   Perhaps I have an injury that with longer distances is imacting my fatigue. Or, maybe it's that I'm not well hydrated or ate appropriately
                                                   that is causing distant to make me more tired. So it's less the running, my more the issue of not having enough carbs or water.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 tableOutput(outputId = "corr_test"),
                                                 br(),
                                                 p("It is important to keep in mind the importance of a correlation rather than solely focusing on a p-value.
                                                   Funder & Ozer (2019) [https://doi.org/10.1177/2515245919847202], discussed the sense and nonsense of effect sizes.
                                                   How plausible and meaningful of an  effect depends on the context. While we may get really large or really small
                                                   effects in our individual samples, we have to reflect on these values to determine whether it is meaningful to what we are 
                                                   trying to measure. For example, does it make sense that age and height correlate r = .65 in a sample of <25 yr olds, but a measure
                                                   of some gene and some behavior correlate r = .45? Not necessarily -- height and age are a pretty large and observable effects in the
                                                   real world. We can see the association. But genes and behaviors are extremely complex, so to see a large correlation should make us
                                                   think 'how is this plausible?' and 'why did this occur in our sample?' In the tab on 'stability of effects', I try to
                                                   provide an example how large effects may arise that are likely to be small in the real-world",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("One other thing to keep in mind with correlations is to pay attention to our sample, varibles and the correlation we find,
                                                   rather than searching for the p-value. As you can see in Figure 3 below, a Pearson's correlation of r = .15 can be
                                                   non-significant and ignored until a sample threshold is hit. The p-value can be misleading.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "corr_n_p"),
                                                 h4("Alt to Pearson's r: Spearman's rho",
                                                    style = "font-family: 'times'"),
                                                 p("Alternative to Pearson's r, one can use the spearman's formula which computes the
                                                   correlation by the rank of x and y. The formula is similar with the exception of x = x', 
                                                   whereby x' is the rank(x)... Will expand more on this soon..."),
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

tabPanel("ANOVA",
         fluidPage(
             sidebarLayout(position = "right",
                           sidebarPanel(
                               h4("Select Parameters to Simulate Data"),
                               sliderInput("aov_sample", label = "Sample Size",
                                           min = 5, value = 50, max = 2000,step = 15),
                               sliderInput("aov1_m", label = "Between(1): Var 1 Mean",
                                           min = -20, value = 0, max = 20, step = .5),
                               sliderInput("aov1_sd", label = "Between(1): Var 1 SD",
                                           min = .01, value = .5, max = 10, step = .1),
                               sliderInput("aov2_m", label = "Between(1): Var 2 Mean",
                                           min = -20, value = 0, max = 20, step = .5),
                               sliderInput("aov2_sd", label = "Between(1): Var 2 SD",
                                           min = .01, value = .5, max = 10, step = .1),
                               sliderInput("aov3_m", label = "Between(1): Var 3 Mean",
                                           min = -20, value = 0, max = 20, step = .5),
                               sliderInput("aov3_sd", label = "Between(1): Var 3 SD",
                                           min = .01, value = .5, max = 10, step = .1),
                               actionButton("run4", "One-Way, Run!", 
                                            style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00"),
                               sliderInput("aov4_m", label = "Between(2): Var 1 Mean",
                                           min = -20, value = 0, max = 20, step = .5),
                               sliderInput("aov4_sd", label = "Between(2): Var 1 SD",
                                           min = .01, value = .5, max = 10, step = .1 ),
                               sliderInput("aov5_m", label = "Between(2): Var 2 Mean",
                                           min = -20, value = 0, max = 20, step = .5),
                               sliderInput("aov5_sd", label = "Between(2): Var 2 SD",
                                           min = .01, value = .5, max = 10, step = .1),
                               sliderInput("aov6_m", label = "Between(2): Var 3 Mean",
                                           min = -20, value = 0, max = 20, step = .5),
                               sliderInput("aov6_sd", label = "Between(2): Var 3 SD",
                                           min = .01, value = .5, max = 10, step = .1),
                               actionButton("run4.1", "Two-by-Three, Run!"),
                           ),
                           mainPanel(
                               h2("One-way & Two-Way ANOVAs"),
                               p("What does this tab represent? Provide some values to generate a One-way and 2x3 ANOVA.",
                                 style = "font-family: 'times'; font-size:14px"),
                               br(),
                               h4("One-way ANOVA"),
                               uiOutput(outputId = "aov_SST"), 
                               p("The Sum of Squares Total (SST) is the sum of squared difference of each score from the grand mean. This is simply the total
                                 variation of observations in the data relative to the overall mean of the outcome",
                                 style = "font-family: 'times'; font-size:14px"),
                               uiOutput(outputId = "aov_SSB"), # subtract individual grp means from grand mean, sqrd diff by grp N
                               p("The Sum of Scores Between (SSB), or the factor effect, is the sum of squared individual group means (k) minus the grand means. This is
                                 simply the between-group variation,",
                                 style = "font-family: 'times'; font-size:14px"),
                               uiOutput(outputId = "aov_SSW"), # sqrd diff of scores from mean score within group (i.e., SSE)
                               p("The Sum of Scores Within (SSW), or the sum of squares error (SEE), is the squared differences of scores from within the group/factor (k).
                                 This is simply the within-group variation",
                                 style = "font-family: 'times'; font-size:14px"),
                               uiOutput(outputId = "aov_MSB"),
                               p("The mean square error between (MSB) is the SSB divided by it's degrees of freedom, which is groups minus one (k - 1)",
                                 style = "font-family: 'times'; font-size:14px"),
                               uiOutput(outputId = "aov_MSW"),
                               p("The mean square error within (MSW) is the SSW divided by it's degrees of freedom, which is total sample minus groups (N - k)",
                                 style = "font-family: 'times'; font-size:14px"),
                               uiOutput(outputId = "aov_F_form"),
                               p("The F-statistic quantifies the differences between population means (Null = no difference between means). 
                               As such, MSB and MSW would be relatively similar. However, if the means differ, it is expected that the between group
                                 variability is larger (MSB) than within group variability (MSW). The F-distribution then is the ratio of
                                 MSB/MSW with the degrees of freedom, k - 1 and N - k.",
                                 style = "font-family: 'times'; font-size:14px;"),
                               p("One-ANOVA: three group means, standard deviations and sample size (N) - ",
                                 style = "font-family: 'times'; font-size:14px; font-weight:bold"),
                               p("The simulated data for the One-way ANOVA inputs is summarized in the table here:",
                                 style = "font-family: 'times'; font-size:14px;"),
                               tableOutput(outputId = "aov_sim_one"),
                               plotOutput(outputId = "aov_one_plot"),
                               uiOutput(outputId = "aov_one_formula"),
                               tableOutput(outputId = "aov_out_one"),
                               tableOutput(outputId = "aov_out_one_tukey"),
                               plotOutput(outputId = "aov_SST_plot"),
                               plotOutput(outputId = "aov_SSB_plot"),
                               plotOutput(outputId = "aov_SSW_plot"),
                               h4("Two-way ANOVA"), 
                               p("A quick note about what inputs impact which outputs. Var1 M/SD alters Short ~ Control; Var2 M/SD alters Short ~ Treatment;
                                 Var3 M/SD alters Avg ~ Control; Var4 M/SD alters Avg ~ Control; Var5 M/SD alters Tall ~ Control; Var6 M/SD alters Tall ~ Treatment.",
                                 style = "font-family: 'times'; font-size:14px;"),
                               p("The simulated data for the Two-way ANOVA for the inputs is summarized in the table here:",
                                 style = "font-family: 'times'; font-size:14px;"),
                               tableOutput(outputId = "aov_sim_two"),
                               uiOutput(outputId = "aov_two_formula"),
                               tableOutput(outputId = "aov_out_two"),
                               plotOutput(outputId = "aov_two_plot"),
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
                                                         actionButton("run5", "Onward!", 
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
                                                           Stitching ggplots via patchwork by Thomas Lin Pedersen ",
                                                           style = "font-family: 'times'; font-size:10px")
                                                         )
                                                     )
                                                 )
                                             ),
################################
################################
################################

tabPanel("Construct Validation",
         fluidPage(
             sidebarLayout(position = "right",
                           sidebarPanel(
                               h4("Select Parameters to Simulate Data at Specified Pearson's r"),
                               actionButton("runN", "TBD!", 
                                            style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00")
                           ),
                           mainPanel(
                               h2("Construct Validation: TBD"),
                               p("Considering how I would like to present this. The structure and function is TBD. In the meantime, here 
                                 are some quotes on constructs",
                                 style = "font-family: 'times'; font-size:14px"),
                               p("Cronbach & Meehl (1957) 'Validation takes place when research believes an instrument 
                                 reflects a particular construct to which a meaning is attached' (pg 290)",
                                 style = "font-family: 'times'; font-size:14px"),
                               p("Loevinger (1957) 'Traits exist in people; constructs 
                                 (here usually about traits) exist in the minds and magazines of psychologists.' pg 642",
                                 style = "font-family: 'times'; font-size:14px"),
                               p("Shadish, Cook & Campbell (2002), a)	Constructs are almost always 
                                 couched in terms that are more abstract than elements in the experiment ",
                                 style = "font-family: 'times'; font-size:14px"),
                               p("Flake & Fried (2020) Refer to measurement as, “any approach that researchers take to create a number 
                                 to represent a variable under study” pg (458)",
                                 style = "font-family: 'times'; font-size:14px"),
                               p("Proulx & Morey (2021) 'In some cases, psychologists may change theoretical labels to obscure the 
                                 origins in order to meet some level of novelty/ground breaking finding' (pg 673)",
                                 style = "font-family: 'times'; font-size:14px"),
                               br()
                           )
             )
         )
),
################################
################################
################################
tabPanel("CFA & PCA",
         fluidPage(
             sidebarLayout(position = "right",
                           sidebarPanel(
                               h4("Select Parameters to Simulate Data Factor Loads"),
                               actionButton("run6", "Simulate Population!", 
                                            style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00"),
                               sliderInput("sem_sample",
                                           label = "Sample (N) for Simulation",
                                           min = 2, max = 10000, value= 100),
                               sliderInput("sem_reps",
                                           label = "Repititions (N) to Generate Population Data",
                                           min = 1, max = 300, value= 5),
                               selectInput(inputId = "standardize", label = "Standardize Lavaan Output?",
                                            choices = c('all','latent','FALSE'), selected = "FALSE"),
                               actionButton("run6.1", "Run Specified Sample Model", 
                                            style =  "color: #FFF; background-color: #ee9a00; border-color: #8B0000")
                           ),
                           mainPanel(
                               h2("CFA/PCA: Simulated Data via CFA Factor Structure"),
                               p("What does this tab represent? A dataset is generated via specified population parameters: loadings, covariance, and variances.
                                 Then, using this data CFA and then PCA (soon to come) models are presented using this simulated data.",
                                 style = "font-family: 'times'; font-size:14px"),
                               br(),
                               p("Please use the below input field to specify laavan syntax of a model that you wish to generate population data for.
                                 If you are unfamiliar with laavan syntax, review tips at: https://lavaan.ugent.be/tutorial/syntax1.html.
                                 As noted at the aforementioned link, '=~' is a measured by indicator used for latent variable definitions; '~'
                                 is 'regressed on' used to specify regression models; '~~' is 'correlated with' indicating covariance in cases of Var1~~Var2,
                                 and residuals in cases of Var1~~Var1; and '~ 1' specifies the intercept.
                                 The following text field and the CFA field below operate similar as they would in Rstudio. Non-laavan strings
                                 will not work. However, lavaan code for starts/constraint of parameters should work. Of note, the text fields start you off,
                                 but you can build/revise these by adding additional factors, items, covariances, residuals, etc. Depending on the sample size, 
                                 repititions, model complexity and variable numbers, expect delays to (re)generate data.",
                                 style = "font-family: 'times'; font-size:14px"),
                               textAreaInput(inputId = "sim_fact_coeff", 
                                             label = "Input Factors, Items loadings and Correlations for Sim Data",
                                             value = "\n#Factor item loadings \nFA1 =~ .5*a1 + .5*a2 + .5*a3 \nFA2 =~ .5*b1 + .5*b2 + .5*b3\n#Factor correlation\nFA1 ~~ .5*FA2
                                             ", 
                                             width = '100%', rows = 8),
                               h4("Proposed CFA/SEM syntax for Population Data",
                                  style = "font-family: 'times'"),
                               textOutput(outputId = "sim_model"),
                               h4("Correlations Among Simulated Data",
                                  style = "font-family: 'times'"),
                               p("These are the correlations among the correlations in the simulated population papameters averaged
                                 *across* N number of simulations.",
                                 style = "font-family: 'times'; font-size:14px"),
                               tableOutput(outputId = "sem_corr"),
                               h4("Proposed CFA/SEM Sample Model",
                                  style = "font-family: 'times'"),
                               textAreaInput(inputId = "cfa_model", 
                                             label = "Input lavaan() model for Proposed CFA",
                                             value = "\n#Factor item loadings \nFA1 =~ a1 + a2 + a3 \nFA2 =~ b1 + b2 + b3\n#Factor correlation\nFA1 ~~ FA2
                                             ", 
                                             width = '100%', rows = 8),
                               tableOutput(outputId = "sem_table"),
                               p("Returning Plot with Standardized Values",
                                  style = "font-family: 'times', font-size = 14px"),
                               plotOutput(outputId = "sem_plot"),
                               p("Packages used: Data was simulated via simsem() by Sunthud Pornprasertmanit et al. & lavaan() by Yves Rosseel. 
                               Parameter table was generated using parameters() by Daniel Lüdecke et al. 
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
    

    
# T- test: simulation data based on user providing 2 means & 2 sds

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
# Simulation sample for ANOVAs
    
    aov_one <- eventReactive(input$run4, {
        set.seed(1)
        
        dat_one <- ANOVA_design(
            design = "3b", 
            n = c(input$aov_sample),
            mu = c(input$aov1_m, input$aov2_m,input$aov3_m), 
            sd = c(input$aov1_sd,input$aov2_sd,input$aov3_sd),
            labelnames = c("Height", "Short", "Avg", "Tall"),
            plot = FALSE,
            r = .05
        )
        
        data.frame(dat_one$dataframe)
    }
    )
    
    aov_two <- eventReactive(input$run4.1, {
        set.seed(1)
        
        dat_two <- ANOVA_design(
            design = "3b*2b", 
            n = c(input$aov_sample),
            mu = c(input$aov1_m, input$aov2_m,input$aov3_m,
                   input$aov4_m, input$aov5_m,input$aov6_m), 
            sd = c(input$aov1_sd,input$aov1_sd,input$aov1_sd,
                   input$aov4_sd,input$aov5_sd,input$aov6_sd),
            labelnames = c("Height", "Short", "Avg", "Tall",
                           "Condition", "Control", "Treatment"),
            plot = FALSE,
            r = .05
            )
            
            data.frame(dat_two$dataframe)
    }
    )
    
    

################################################################      
# Simulation sample with THREE variables reflective pre specified correlation for multiple regression
    m_reg_data <- eventReactive(input$run5, {
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
    
    sem_data <- eventReactive(input$run6, {
        set.seed(1) 
        model_population <-paste0(sim_cfa_text())
        
        sim <- sim(nRep = input$sem_reps, model = "lavaan", n = input$sem_sample, 
                generate = model_population, std.lv = TRUE, lavaanfun = "sem", 
                dataOnly=T, meanstructure = TRUE, seed=123)
        
        sim_data <- data.frame(aaply(laply(sim, as.matrix), c(2,3), mean))
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
            print(paste0("$$ t = \\frac{m-\\mu}{s/\\sqrt{n}} $$"))
        )
    })
    
    output$one_t_table <- function(){
        data() %>% 
            gather(key = "Variable", value = "Data", var1:var2) %>% 
            group_by(Variable) %>% 
            dplyr::summarise('Sample Size (N)' = n(), 
                             'Mean' = mean(Data),
                             'SDs' = sd(Data),
                             'Population Mean' = input$pop_mean) %>% 
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
        
        plot_2grp_means <- ggplot(data = data(), aes(x = groups, y = grp_values)) +
            geom_boxplot(aes(colour = groups))+
            geom_point(size = 1/input$sample, position = "jitter", aes(colour = groups)) +
            labs(title = "Fig 5. Means and Distribution of Two Group Means for Variable 1")+
            xlab("")+
            ylab("Group Values Observed")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
        
        plot_2grp_corr <- ggplot(data = data(), aes(x = var1, y = grp_values)) +
            geom_point(aes(colour = groups)) +
            geom_smooth(method = "lm", se = FALSE, aes(colour=groups))+
            geom_smooth(method = "lm", se = FALSE, color = "black")+
            labs(title = "Fig 6. Correlation between Var 1 & Var 2 (Grouped) and for Individual Groups")+
            xlab("Variable 1")+
            ylab("Variable 2")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
        
         plot_2grp_means / plot_2grp_corr + plot_layout(guides = 'collect')
    }) 
    
    
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
            labs(title = "Fig 7. Standard deviation scaled by Sample Size for two groups with equal N", 
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
        
        cor1 <- round(
            as.numeric(
            cor.test(x = sim_corr()$X1, 
                          y = sim_corr()$X2,
                          method = "pearson")$estimate),2)
        cor2 <- round(
            as.numeric(
            cor.test(x = sim_corr_2()$X1, 
                          y = sim_corr_2()$X2,
                          method = "pearson")$estimate),2)
        
        corplt1 <- ggplot(sim_corr(), aes(x = X1, y = X2)) +
            geom_smooth(method = "lm", se = FALSE)+
            geom_point()+
            geom_rug(size = .3, position = "jitter", colour = "sienna4")+
            labs(title = "Fig 1. Association between Var 1 and Var 2", 
                caption = "*Data simulated based on specified parameters")+
            xlab("Var1")+
            ylab("Var2")+
            annotate("label", x = mean(sim_corr()$X1)+2, y = mean(sim_corr()$X2)+2, 
                     label = paste0("Pearson's r: ",cor1))+
            theme_minimal()
        
        corplt2 <- ggplot(sim_corr_2(), aes(x = X1, y = X2)) +
           geom_smooth(method = "lm", se = FALSE)+
           geom_point()+
           geom_rug(size = .3, position = "jitter", colour = "sienna4")+
           labs(title = "Fig 2. Association between Var 3 and Var 4", 
                caption = "*Data simulated based on specified parameters")+
            xlab("Var3")+
            ylab("Var4")+
            annotate("label", x = mean(sim_corr_2()$X1)+2, y = mean(sim_corr_2()$X2)+2, 
                     label = paste0("Pearson's r: ",cor2))+
           theme_minimal()
        
        corplt1 / corplt2 + plot_layout(guides = 'collect')
    })
    
    output$corr_n_p <- renderPlot({
       
    data_r_list = list()
       for (i in seq(5,700,5)) {
           
           t_val <- (.15*(sqrt(i-2)))/sqrt(1-(.15^2))
           p_val <- 2*pt(-abs(t_val), df = i-2)
           
           data_r_list[[i]] <- data.frame('SampleSize' = i, "p_value" = p_val)
       }
       
       data_r_p <- do.call(rbind, data_r_list)
       
       
       ggplot(data = data_r_p, aes(x = SampleSize, y = p_value)) +
           geom_line()+
           labs(title = "Fig 3. Effect of Sample size on p-value for Pearson Correlation of r = .15", 
                caption = "Sample Size: 5 to 700; Dashed Red Line, p = .05")+
           xlab("Sample Size")+
           ylab("p-value")+
           geom_hline(yintercept = .05, linetype = "dashed", colour = "red")+
           theme(text = element_text(size = 12, family = "times"))+
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
### ANOVA
##########################
    
    output$aov_one_formula <- renderUI({
        fit<-aov(y ~ Height, data = aov_one()) 
        eq <- paste0(extract_eq(fit))
        withMathJax(
            print(paste0("$$",eq,"$$"))
        )
    })
    
    
    output$aov_SST <- renderUI({
        withMathJax(
            print(paste0("$$ SS_{Total}= \\sum\\limits_{j=1}^{k}\\sum\\limits_{i=1}^{n_i} \\left(x_{ij} - \\overline{X}\\right)^2 $$"))
        )
    })

    
    output$aov_SSB <- renderUI({
        withMathJax(
            print(paste0("$$ SS_{Between} = \\sum\\limits_{j=1}^{k}n_j  \\left(\\overline{x_j} - \\overline{X}\\right)^2 $$"))
        )
    })
    
    output$aov_SSW <- renderUI({
        withMathJax(
            print(paste0("$$ SS_{Within} = \\sum\\limits_{j=1}^{k}\\sum\\limits_{i=1}^{n_j} \\left(x_{ij} - \\overline{x}_{j}\\right)^2 $$"))
        )
    })
    
    output$aov_MSB <- renderUI({
        withMathJax(
            print(paste0("$$ MS_{Between} = \\frac {SS_{Between}}{k - 1} $$"))
        )
    })
    
    output$aov_MSW <- renderUI({
        withMathJax(
            print(paste0("$$ MS_{Within} = \\frac {SS_{Within}}{n - k} $$")) # within / error
        )
    })
    
    output$aov_F_form <- renderUI({
        withMathJax(
            print(paste0("$$ F_{stat} =  \\frac {MS_{Between}}{MS_{Within}} $$"))
        )
    })

    output$aov_sim_one <- function() {
        aov_one() %>% 
            group_by(Height) %>% 
            dplyr::summarise("Group N" = n(),
                             "Group Mean" = mean(y),
                             "Group SD" = sd(y)) %>% 
            kable() %>% 
            kable_styling("striped",
                          full_width = F, font_size = 12, html_font = 'Times')
    }
    
    output$aov_out_one <- function(){
        anova <- aov(y ~ Height, data = aov_one())
        
        broom::tidy(anova) %>% 
            kable() %>% 
            kable_styling("striped",
                          full_width = F, font_size = 12, html_font = 'Times')
    }
    
    output$aov_out_one_tukey <- function(){
        anova <- aov(y ~ Height, data = aov_one())
        
        summary(glht(anova, linfct = mcp(Height = "Tukey"))) %>% 
            broom::tidy() %>% 
            kable() %>% 
            kable_styling("striped",
                          full_width = F, 
                          font_size = 12, 
                          html_font = 'Times')
    }
    
    
    
    output$aov_one_plot <- renderPlot({
        aov_one() %>% 
            ggplot(aes(x = y, y = Height, colour = Height)) +
            geom_boxplot()+
            theme_minimal()
    })
    
    
    output$aov_SST_plot <- renderPlot({
        aov_one <- aov_one()
        
        ## Plot SST
        SST<- round(sum((aov_one$y - mean(aov_one$y))^2),2)
        aov_one %>%
            ggplot(aes(x = "", y = y, colour = Height)) +
            geom_point(aes(x = "", y = y), position = "jitter",
                       size =5/input$aov_sample) +
            annotate("label", x = "", y = mean(aov_one$y)*2, 
                     label = paste0("Sum Sq. Total: ",SST))+
            geom_hline(yintercept = mean(aov_one$y), linetype = "solid",
                       color = "red", size = .5)+
            labs(title = "Fig 1. Sum of Squares Total  (Residuals)",
                 caption = "Red Line = Mean of Outcome")+
            theme(text = element_text(size = 14, family = "times"))+
            theme_minimal()
    })
    
    output$aov_SSW_plot <- renderPlot({
        aov_one <- aov_one()
        
        ## Plot SST
        SSW <-
            aov_one %>% 
            group_by(Height) %>% 
            dplyr::mutate(grp_mean = mean(y)) %>% 
            dplyr::mutate(SSW = (y - grp_mean)^2) %>%
            group_by() %>% 
            dplyr::summarise(SSW = sum(SSW)) %>% 
            round(1)
        
        aov_one %>% 
            ggplot(aes(x = Height, y = y, colour = Height)) +
            geom_boxplot(aes(x = Height, y = y))+
            geom_point(aes(x =  Height, y = y), position = "jitter", size =1) +
            annotate("label", x = "Height_Short", y = mean(aov_one$y)*2,
                     label = paste0("Sum Sq. Within: ",SSW))+
            labs(title = "Fig 3. Sum of Squares Within (Error)")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
        
    }
    )
    
    output$aov_SSB_plot <- renderPlot({
        aov_one <- aov_one()
        
        ## Plot SSB
        SSB<- aov_one %>% 
            dplyr::select(y, Height) %>% 
            mutate(grand_mean = mean(y)) %>% 
            group_by(Height) %>% 
            dplyr::summarize(n = n(),
                             factor_mean = mean(y),
                             grand_mean = mean(grand_mean)) %>% 
            mutate(SS_between = round(n * (factor_mean - grand_mean)^2),2) %>% 
            pull(SS_between) %>% 
            sum()
        
        aov_one %>%
            ggplot(aes(x = Height, y = y)) +
            geom_boxplot(aes(y = y, group = Height, colour = Height))+
            geom_point(aes(x = Height, y = y, colour = Height), size =5/input$aov_sample) +
            annotate("label", x = "Height_Short", 
                     y = mean(aov_one$y)*2, 
                     label = paste0("Sum Sq. Between: ",SSB))+
            geom_hline(yintercept = mean(aov_one$y), linetype = "solid",
                       color = "red", size = .5)+
            labs(title = "Fig 2. Sum of Squares Between (Factor)",
                 caption = "Red Line = Mean of Outcome")+
            theme(text = element_text(size = 12, family = "times"))+
            theme_minimal()
    })
    
   
    output$aov_two_formula <- renderUI({
        fit<-aov(y ~ Height + Condition, data = aov_two()) 
        eq <- paste0(extract_eq(fit))
        withMathJax(
            print(paste0("$$",eq,"$$"))
        )
    })
    
    output$aov_sim_two <- function() {
        aov_two() %>% 
            group_by(Height, Condition) %>% 
            dplyr::summarise("Group N" = n(),
                             "Group Mean" = mean(y),
                             "Group SD" = sd(y)) %>% 
            kable() %>% 
            kable_styling("striped",
                          full_width = F, font_size = 12, html_font = 'Times')
    }
    
    output$aov_out_two <- function(){
        anova <- aov(y ~ Height + Condition, data = aov_two())
        
        broom::tidy(anova) %>% 
            kable() %>% 
            kable_styling("striped",
                          full_width = F, font_size = 12, html_font = 'Times')
    }
    
    output$aov_two_plot <- renderPlot({
        
        aov_two() %>% 
            ggplot(aes(x = y, y = Height, colour = Condition)) +
            geom_boxplot()+
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
    
    
    cfa_data <- eventReactive(input$run6.1, {
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
