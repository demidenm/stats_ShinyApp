# Load libraries
library(shiny) # shiny app functionality
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyMatrix) #https://cran.r-project.org/web/packages/shinyMatrix/readme/README.html
library(rhandsontable)
library(thematic)
library(tidyverse)
library(lavaan)
library(simsem)
library(QuantPsyc)
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
library(ppcor)


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


## Matrices used 
m <- matrix(data = c(100, 5, 2.5, "-", "-", "-",
                     100, "-", "-", 25, "-", "-",
                     100, "-", "-", 25, "-", "-",
                     100, "-", "-", "-", 0, 50), ncol = 6, byrow = TRUE, 
            dimnames = list(
              c("Normal Dist", "T Dist", "Chi-square Dist", "Random Uniform Dist"),
              c("N", "Mean", "St Dev", "DF", "Min", "Max")
            ))

r_ex <- matrix(data = c(5, .2, 
                        4, .2), ncol = 2, byrow = F, 
               dimnames = list(
                 c("Mean", "Correlation var 1 ~ Var 2 (r)"),
                 c("X1", "X2")
                 ))





UserInferface <- navbarPage("Ooo, so, so shiny!",
                            collapsible = TRUE, 
                            inverse = FALSE,
                            fluid = TRUE,
                            windowTitle = "Simulations for Research Stats",
                            theme = shinytheme("flatly"),
                            
                            tabPanel("Intro",
                                             mainPanel(width = "100%", style='padding-left: 30px; padding-right: 30px',
                                                 h2("Another day, another shiny stat"),
                                                 p("This shiny app is a work-in-progress. It was last updated January 20th, 2022",
                                                   style = "font-family: 'times'; font-size:16px"),
                                                 br(),
                                                 p("The purpose of this application is to introduce you, the user, to some basic statistical concepts and tests Specifically, what the numbers mean, how those numbers are calculated,
                                                 and give you the opportunity to re-simulate this data in multiple ways. 
                                                 Statistical tests are influenced by many things, some being the number of participants (i.e. observed datapoints), the variability in these datapoints (i.e. variance),
                                                   and select observations that the majority of the observations for a specific variable (i.e., outliers). To learn about these concepts and issues, click a tab, play around with some numerical inputs 
                                                   and observe how some outputs change, and most importantly, why they change. I try to convey these changes using words, tables, figures and as many examples as possible
                                                 to convey the points. By no means will you leave an expert after going through this application, but I hope you would have learned a couple of new things!",
                                                   style = "font-family: 'times'; font-size:16px"),
                                                 img(src="demo_intro.gif", align = "right", height = '220px', width = '400px'),
                                                 p("There are several tabs that go over statistical tests, such as T-tests, ANOVAs, Correlation, Regression, and hopefully Factor analysis. 
                                                 I also include an explanatory tab, such as the Preliminary tab, and conceptual tabs, such as the 'Stability of Effect' and 'Construct Validation'. The introductory text for each
                                                   tab will help offer a bit of context for what you will be learning in that tab and what numerical values you, the user, can change to (re)simulate new data. 
                                                   While this app works best on desktop/laptop/ipad,  it is mobile compatible. Similar to the button below, 'Onward!', this is to be used to (re)simulate the datasets used.
                                                   In essence, the button is instructs the program to simulate data based on the numbers that are provided. As I remind you in each tab,
                                                   I recommend starting with the default values and then changing them to iteratively learn about each statistical concept.",
                                                   style = "font-family: 'times'; font-size:16px",
                                                   actionButton("start", "Onward!",
                                                                style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00;")),
                                                 br(),
                                                 p("If you are relatively new to statistics and research methods, I encourage you to start at the Preliminary tab and working in sequence.
                                                  This way you start with the definitions for terminology and some examples so it is easier to follow along when this terminology is used on other tabs. 
                                                  If you're wondering, 'why is there is so much white space between text?', this will change once you click the button to simulate the data.",
                                                   style = "font-family: 'times'; font-size:16px"),
                                                 p("Keep in mind, the way we simulate the data in this app follows certain rules. Specifically, we make the assumption that values are normally distributed (Except in cases of outliers), 
                                                 a concept that I will define in the ",
                                                  span("Preliminary tab.", style = "font-weight: bold"),"While the simulated data here will be normally distribute, often the data collected from real participants -- in particular in the 
                                                 social sciences -- contains data points that are densely packed around the average (leptokurtic, or positive kurtosis), spread out equally across possible values 
                                                 (platykurtic, or negative kurtosis), heavily weighted left of the average (positive skew), heavily weighted 
                                                 right of the average (negative skew), or simply contains high frequencies of '0', such as self-reported substance use. Therefore, the simulations DO NOT account for the 
                                                   the effects of this on the resulting statistical models. In future updates, I hope to include tabs on this topic.",
                                                   style = "font-family: 'times'; font-size:16px"),
                                                 br(),
                                                 p("Please complete ", 
                                                   a("this brief survey", href="https://umich.qualtrics.com/jfe/form/SV_80zhUf4cB7wJXWC", target="_blank"),
                                                   "so I can obtain valuable insights from its user(s) that'll help me tailor and refine future versions of the app.",
                                                   style = "font-family: 'times'; font-weight: bold; font-size:16px"),
                                                 br(),
                                                 p("This app is designed using ", 
                                                   a("R studio software", href="https://www.rstudio.com/", target="_blank"),
                                                   "in combination with",
                                                   a("Shiny from R studio.", href="https://shiny.rstudio.com/tutorial/", target="_blank"),
                                                   "If you're interested to learn more about R, I recommend the R resources available online such as the instructional video from ",
                                                   a("Karandeep Singh based on content in his Learning Health Systems course,", href="https://www.youtube.com/channel/UC7eRNr1Pprls2aU5WWPz15Q/videos", target="_blank"),
                                                   "an R introduction by ",
                                                   a("Russell Poldrack,", href="https://statsthinking21.github.io/statsthinking21-R-site/", target="_blank"),
                                                   "R for Data Science and visualization by ",
                                                   a("Hadley Wickham and Garrett Grolemund,", href="https://r4ds.had.co.nz/introduction.html", target="_blank"),
                                                   "the Coursera Course ",
                                                   a("Getting Started with R,", href="https://www.coursera.org/projects/getting-started-with-r", target="_blank"),
                                                   "and the textbook from Andy Field:",
                                                   a("Discovering Statistics Using R.", href="https://www.discoveringstatistics.com/books/discovering-statistics-using-r/", target="_blank"),
                                                   "There is also this very extensive list from Josceline Hidalgo",
                                                   a("Joscelin Rocha Hidalgo", href="https://github.com/Joscelinrocha/Learning-R-Resources/wiki/a.-Learning-R", target="_blank"),
                                                   "If you're new or new-ish to R, these will help your learning of R and can easily be supplemented with Google researches like, 'How to do [insert thing] in R]. 
                                                   Where a large community has contributed to many problems that you would have encountered, directly or indirectly.",
                                                   style = "font-family: 'times'; font-size:16px"),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 p("For Citation & Documentation purposes, code will will be available on",
                                                   a("Github", href="https://github.com/", target="_blank"), "once an initial full version is complete.",
                                                   style = "font-family: 'times'; font-size:10px"),
                                                 p("© 2022 Michael Demidenko. Please contact for more details",
                                                   style = "font-family: 'times'; font-size:10px")
                                             )
                                     ),
                            
                            
# Preliminary
                            tabPanel("Preliminary",
                                     mainPanel(width = "100%", style='padding-left: 30px; padding-right: 30px',
                                             h2("Primer/Glossary"),
                                             p("People browsing this app may different background knowledge when it comes to statistics and research methods. Hence,
                                             it would be helpful to define some terminology and examples so we are 'somewhat' on the same
                                             page as you navigate the item. For instnace, I may use terms such as 'independent variables', 'normal distribution' or 'estimates'. 
                                             Some of you may and others may not be familiar with what I am referring to. So, I will provide some prelimiary definitions here as they relate to: hypothesis testing, 
                                               different types of variables, certain estimates, and terms relating to statistical models.",
                                               style = "font-family: 'times'; font-size:16px"),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             tabsetPanel(id = "prelim_tabset",
                                               tabPanel(title = "Hypothesis Test", value = "hypothesis_test",
                                                        h3("Hypothesis Testing"),
                                                        img(src="H0H1_dist.jpg", align = "right", height = '40%', width = '40%'),
                                                        p("If you have a specific research question, often you will use hypothesis testing to determine whether there is support for or against this hypothesis. 
                                                        While you can subjectively evaluate through graphs or tables whether something is different, statistical tests provide a more objective measure that accounts
                                                        posses more contraints based on your hypothesis. As such, before starting an analysis, it's important to state what the Null (H0) and Alternative Hypothesis (H1) is. 
                                                        Typically, an associated α (i.e., alpha) is used to set as the cut-off for what we define as a significant or non-significant difference  (i.e., our p-value). In the hypothesis testing framework,
                                                          you may propose directed hypotheses, such as that the estimated average for a variable will be significantly lower (or greater) than a population average. This is typically a 
                                                          'one-tailed test'. Alternatively, you may not have a directed hypothesis, but you know that value would be significantly greater than the population average. This would be a 'two-tailed' test.
                                                          In the H0/H1 figure, the directed hypothesis of 'significantly less' is shaded orange on the left tail of the curve and 'significantly greater' shaded on the right tale. Likewise,
                                                          in a non-directed hypothesis, you will find both tales shaded, as values may be significantly lower or greater.", 
                                                          style = "font-family: 'times'; font-size:16px"),
                                                          
                                                          p(span("p-value:", style = "font-weight: bold"), "is the probability of observing an equal or more extreme difference or association in our data 
                                                          based on that data's measures and sampling characteristics. The p-value is the threshold we use to draw whether the observed
                                                          value is significantly greater/lower/different from the population average, as we had hypothesized. Traditionally, p < .05 is used as the threshold. So in our directed hypothesis,
                                                          we would se an α of .05. But in some cases, if you want a more strict threshold, you have the option to lower the α to .01, .005, or .001, as long as the decision is justified for your study..
                                                          It goes without sayng, the p-value is a fairly abstract cut-off, as discussed in Cohen (1994) &  Amrhein et al (2017). 
                                                          The p-value can be influenced by many things, as I will show in later tabs, such as the sample size, extreme values (outliers),
                                                          and incorrect measurement. For a long time, it was common to see only p < .05 but Benjamin et al (2018) proposed a p < .005 cut-off. 
                                                          However, like noted before, Lakens et al (2018) argued in response to this proposal that p-values should be justified in each study. These are complicated issues, but what 
                                                          it boils down to is that the p > .05 and p < .05 is somewhat abstract and so while it's a useful heuristic, one shouldn't only pay attention to this value.", 
                                                            style = "font-family: 'times'; font-size:16px"),
                                                          img(src="lady_Tea.jpg", align = "center", height = '70%', width = '100%'),
                                                          p("The abstractness is even more interesting when you consider that Sir Ronald Aylmer Fisher (Cowles, 1989), who introduced the p-value, arrived at the p-value when considering the
                                                          probability that a lady could tell whether the milk was poured into a cup before or after the tea was poured. Designing his experiment, Fisher than calculated
                                                          the probability that the participant could differentiate whether for that cup of tea the milk or tea was poured first. He considered any event that occurred that had a probability of <5%
                                                          was extremely rare and therefore meaningful. In fact, the lady was able to tell the difference for all cups of tea and has books written about ", 
                                                            a("her importance to contemporary statistical tests.", href="https://www.goodreads.com/book/show/106350.The_Lady_Tasting_Tea", target="_blank"),
                                                            style = "font-family: 'times'; font-size:16px"), 
                                                        p("This account of the p-value is a fun topic, 
                                                          but in statistical terms it's worth to consider how the cut-off determines whether something is or isn't meaningful. Take for instace that the differencee 
                                                          between a p-value of p = .025 and p = .0005 is subtantially larger than the difference in estimates between p = 073 
                                                          and p = .043. However, the conclusion, based on the traditional use of the p-value, is that both are significant (i.e., p <  .05) in the former case but only one is significant
                                                          in the latter case, despite the relative difference between values in the latter case being relatively small. One should be cautious when using p-values. 
                                                          It's important to remember what they mean with respect to your hypothesis and how large a difference is to plausable.",
                                                          style = "font-family: 'times'; font-size:16px"), 
                                                            p(span("Null Hypothesis (H0):", style = "font-weight: bold"), "In statistical tests, the null hypothesis (H0) is that there is no mean difference 
                                                            or association between variables. If we set our α = .05, run a Two-sample T-test that compares two averages and the resulting p-value is .16, 
                                                            we would accept the null hypothesis, that there is no difference.",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                              p(span("Alternative Hypothesis (H1):", style = "font-weight: bold"), "In statistical tests, the alternative hypothesis (H1) is that there is a 
                                                              significant difference between the two averages in a Two sample t-test. If our α = .05, and our Two-sample T-test provides a p-value of .002, 
                                                              we could reject the null hypothesis, which is that there is a difference in the means or our association.",
                                                                style = "font-family: 'times'; font-size:16px"),
                                                                p("There are several caveats with hypothesis testing. In addition to the noted issues with the p-value, we can run into several errors along the way. 
                                                                For instance, a Type-I error (or False Positive) is that based on our measure, sample, and/or sampling we found a significant (p < .05) 
                                                                mean difference or association amongst our variables when in fact there is NO difference. So, we rejected H0 hypothesis and accepted H1 when we should have rejected H1 but
                                                                accepted the H0, that there was no meaningful difference. Essentially, our test misfired and we got result when one didn't really exist... 
                                                                Then, a Type-II error (or False Negative) is based on our measurement, sample, and/or sampling we did NOT find a significant (p > .05) mean difference or association amongst 
                                                                our variables when in fact there is a significanct difference. So, we rejected H1 and accepted H0 when we should have rejected H0 and accepted accepted H1. 
                                                                Finally, there is a Type-III error (or assumption of equivalence) which is the assumption 
                                                                that our test is equivalent to another test that was conducted. Type-III errors are sometimes referred to finding a significant effect in the wrong direction, 
                                                                but here I focus on the term as it relates to test equivalence, or measurement. What I mean by test equivalence is that researchers often perform tests that are similar,
                                                                but differ on the type of sample (i.e. participants), measures, and other study related decisions. This may result in a different conclusion which may 
                                                                be based on the fact that the tests were simply not equivalent between the two studies. This will be important in our discussion on constructs and construct 
                                                                  validation in a later tab.",
                                                                  style = "font-family: 'times'; font-size:16px"),
                                                        br(),
                                                        h4("Cited Work"),
                                                        p("Amrhein, V., Korner-Nievergelt, F., & Roth, T. (2017). The earth is flat (p > 0.05): Significance thresholds and the crisis of unreplicable research. PeerJ, 5. https://doi.org/10.7717/peerj.3544",
                                                          style = "font-family: 'times'; font-size:10px"),
                                                        p("Benjamin, D. J., Berger, J. O., Johannesson, M., Nosek, B. A., Wagenmakers, E.-J., … Johnson, V. E. (2018). Redefine statistical significance. Nature Human Behaviour, 2(1), 6–10. https://doi.org/10.1038/s41562-017-0189-z",
                                                          style = "font-family: 'times'; font-size:10px"),
                                                        p("Cohen, J. (1994). The earth is round (p < .05). American Psychologist, 49(12), 997–1003. https://doi.org/10.1037/0003-066X.49.12.997",
                                                          style = "font-family: 'times'; font-size:10px"),
                                                        p("Cowles, M. (1989). Statistics in psychology: An historical perspective. Lawrence Erlbaum Associates, Inc.",
                                                          style = "font-family: 'times'; font-size:10px"),
                                                        p("Lakens, D., Adolfi, F. G., Albers, C. J., Anvari, F., Apps, M. A. J., Argamon, S. E., Baguley, T., Becker, R. B., Benning, S. D., … Zwaan, R. A. (2018). Justify your alpha. Nature Human Behaviour, 2(3), 168–171. https://doi.org/10.1038/s41562-018-0311-x",
                                                          style = "font-family: 'times'; font-size:10px"),
                                                        p("Rubin, M. (2021). When to adjust alpha during multiple testing: A consideration of disjunction, conjunction, and individual testing. Synthese. https://doi.org/10.1007/s11229-021-03276-4",
                                                          style = "font-family: 'times'; font-size:10px"),
                                                        br(),
                                             ),
                                             tabPanel(title = "Variables", value = "variables",
                                                      h3("Variables"),
                                                      p("Central to ever study are the data that researchers collect. Before each study, researchers define their list of variables that will
                                                      help answer their questions and elaborate on other topics. This part of the design includes defining the cause and outcome of interest and the type of the
                                                      design that'll help answer it. Researchers resort to experimental and/or correlation designs. Experimental designs include a manipulation of a variable that is interest to understand
                                                      what causes an outcome. An example of this is participants receiving a treatment drug or a placebo to see how it helps with a treatment of an illness/disease. 
                                                      Correlational designs include the natural progression/variable of variables in the population. Often experimental and correlational designs leverage a cross-sectional 
                                                      or longitudinal approach to acquire this data.",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      br(),
                                                      img(src="CrossSectional_Longitudinal.jpg", align = "center", height = '70%', width = '100%'),
                                                      br(),
                                                      br(),
                                                      p(span("Cross-sectional Data:", style = "font-weight: bold"), "is a design where data are collected at a single time-point for different participants. This is the most common design. Researchers
                                                        recruit a number of participants that span age and/or demographics and probe the cause and effect using an experimental or correlational analysis. More often than not,
                                                        this design limits conclusions about the progression of a disease or the change of a variable across time.",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      p(span("Longitudinal Design:", style = "font-weight: bold"), "is a design where data are collected across time for the same participants. Whereas in the cross-sectional design participants/subjects are
                                                        tested at a single timepoint, the longitudinal design leverages multiple waves of data collection to probe the change of disease or correlation between variables changes
                                                        across time. A concern of longitudinal designs are how cohorts of a participants impact the change in variables, how practice impacts subsequent data collections, and most importantly which participants
                                                        do/do not drop-out. Often, participants that go through major transitions, incur the greatest burden, or are at higher risk represent a greater proportion
                                                        of dropout in data. This is a problem, because these participants represent the population and so are important to have them be equally represented in hypothesis testing.",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      p(span("Independent Variable (IV):", style = "font-weight: bold"), "is the cause or our predictor of interest. 
                                                      The independent variable, or IV for short, is the variable that we suspect is contributing to an outcome in our research question.
                                                      For example, say we would like to test how much food a participant can eat until they're full. In this scenario, the IV would be
                                                        the amount of food eaten (i.e., our sample).",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      img(src="IVDV.jpg", align = "left", height = '30%', width = '30%'),
                                                        p(span("Dependent Variable (DV):", style = "font-weight: bold"), "is the outcome or the result of interest. 
                                                        If the IV is the predictor/cause, the dependent variable, or DV for short, is the outcome we are testing for in our research question. 
                                                        For example, if we are testing the amount of food a participants eats until they're full, feeling full is the outcome. 
                                                        We would measure to what extent the IV 'causes' the DV. I write cause in quotes because causal relationships 
                                                        are, in most but not all cases, difficult to demonstrate and thus it is more appropriate to refer to them as 'associations'.",
                                                          style = "font-family: 'times'; font-size:16px"),
                                                        p(span("Covariate:", style = "font-weight: bold"), "in research studies there are other IVs which are cooccur with our variable of interest. For example, 
                                                        if we're testing the amount of food eaten (IV) before someone feels full (DV), there are other variables that may impact feeling full, other than the amount eaten. 
                                                        These are covariates, or 'confounding variables'. Covariates can serve as IVs, but because we have a specific question about the association between the amount of food eaten
                                                        and feeling full, 
                                                        we need to ensure these other variables are not the reason for the result we observe. For example, if you have ever had any kind of illness or taken a medication that stimulates your hunger, 
                                                        these may contribute to feeling full or not full. So if I have been nauseous, I'll have a feeling of full no matter what. Better yet, if I arrived to your experiment after eating Burger King,
                                                        I may be full satiated, too. All of these reasons can impact the conclusion we may make about the association between eating and feeling full. In which case, we would need to control, or adjust for 
                                                        these covariates, by including them in our statistical models or by using randomization techniques in order to alleviate differences related to these 
                                                        confounding variables.",
                                                          style = "font-family: 'times'; font-size:16px"),
                                                      br(),
                                                      p("The IV, DV, and confounding variables in our dataset can take different forms. Dependinging on how the variables are measured,
                                                        may often depending on the statistical models we have to use. These variables can be continuous, ordinal, binary or nominal variables.",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      img(src="Var_continuousOrdinalNorminal.jpg", align = "center", height = '100%', width = '100%'),
                                                      p(span("Continuous variable:", style = "font-weight: bold"), "these variables typically denote a numerical value that is on an interval or ratio scale. 
                                                      A continuous variable has an interval, where equal intervals represent equal differences. These values can range from positive to negative. The values can be added, subtract or multiplied, and provide
                                                        an interpretable result. However, for interval continuous variables, zero doesn't indicate an abscence of a value, so it cannot be easily divided. Take for instance
                                                        the temperature, 0F has a meaning that is not an absecence of temperature. Similar to interval variable, ratio variables can be added, subtracted and multipled. However,
                                                        unlike interval variables, ratio variables can also be divided. In the case of the ratio variable, zero means something. Take for example weight, you can measure values from 0 to 150 points.
                                                        Zero is an absence of mass, and the range of values cannot go negative... unless of course your scale is broken. The continuous is discrete when it is finite,
                                                        like money in savings account, but continuous when it is infinite, such as time (but then again, time is finite for all...)",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      p(span("Ordinal variable:", style = "font-weight: bold"), "these variables have categories, but the categories have a logical order. 
                                                      These might be one of the most common variables in psychological research as they relate to many self-report measures, i.e., likert scales. Take for instance a item
                                                        that asks you to reflect on your team morale. You may be given some statements about your team, and you have to indicate whether you strongly disagree, disagree,
                                                        neither agree nor disagree, agree, or strongly agree. These values are categories of responses, but they have a clear low (strongly disagree) to high (strongly agree)
                                                        ranking. Another example can be ranking in a class or race, there is a logical order, but the difference between one value and another is not always equal. 
                                                        As the interval between each cannot be assumed to be equal.",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      p(span("Nominal variable:", style = "font-weight: bold"), "these variables have categories, that have no clear order. So there is no reason to subtract,
                                                      add, multiply or divide the values. Nominal variables can be binary, with two categories, such as biological sex or receiving/not receiving treament. They can
                                                        also have  >2 categories, such a month of the year or race. These variables have no logical order that allows for comparison. For example, there is no ranking
                                                        in race, so you can't add, subtract, multiple or divide these values. Then, while months can be ordered as a timescale, in most cases these values cannot necessarily be easily
                                                        added, subtracted, multiplied, or divided. While dates, on the other can, can serve as ordinal values, the calendar month on it's own is a qualitative measure.",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      img(src="minimize_confounds.jpg", align = "left", height = '20%', width = '20%'),
                                                        p(span("How to minize confounds:", style = "font-weight: bold"), "if we know our research question may contain confoundings variables, 
                                                          it is worthwhile to consider how we may adjust for them in our research design. One approach that is often used is",
                                                          span("randomization.", style = "color: black; font-style: italics"), "By using experimental designs, studies attempt
                                                          to reduce the influence of confounds by recruiting a select number of participants from a population and
                                                          randomly assigning them to either a control or intervention arm of the study. By using a strong randomization approach, 
                                                          we may control for both known and unkown confounds. However, interventions are not appropriate for all types of studies.
                                                          If we're interested in the influence of lead in water on cognitive development, it would be VERY inappropriate to randomly assign 
                                                          people from the population to these groups. An alternative approach may be ", span("matching or restricting", style = "color: black; font-style: italics"),
                                                          "the type of sample that is included. This approach may help you to account for known differences in your sample/population(s) by pairing groups on these variables.
                                                          While matching (such as via propensity scores) helps alleviate some unknowns, it is not a silver bullet. What is commonly done in psychological research is",
                                                          span("controlling", style = "color: black; font-style: italics"), "for the influence of a variable in a statistical model. This is where the a known confound/variable is included as a
                                                          covariate in the model so its influence is partially adjusted in the association between the IV and DV of interest. The meaning of 'partially adjusted' will make sense after
                                                          you review the correlation and regression tabs, but in short, it's subtracting the association between the confound and IV and confound and DV so you're left with the association
                                                          between your IV and DV adjusted for the confound. Nevertheless, just like the other methods noted here, 
                                                          this isn't a perfect technique and has its own drawbacks.",
                                                          style = "font-family: 'times'; font-size:16px"),
                                                        p(span("Operationalization:", style = "font-weight: bold"), "is the process of defining our variables in a study.
                                                        For instance, when we are measuring eating and feeling full, we can quantify eating a weight of food, calories of the food, or some combination. Then for feeling full,
                                                        we can use a scale from 1 (not full at all) to 100 (If I eat anymore I'll puke), ask on a scale whether they are are more or less
                                                        full than normal, or more explicitly asking 'are you full (yes/no)?'. For our covariates, 
                                                        or confounding variables, if we wanted to account for something like feeling sick, how would be go about it? This latter variable is a bit more difficult to gauge,
                                                        because not all illness impact eating in the same manner. So, requires thinking through the types of illness that may impact hunger and ways we can ask about them to give us the best overall value for that variable. 
                                                        While the variables here are a bit more simple, in most research there are often several methodological and theoretical steps that are part of validating a hypothetical variable, such as something abstract
                                                          as emotion or feeling happy (more on this in a later tab).",
                                                          style = "font-family: 'times'; font-size:16px"),
                                                      ),
                                             tabPanel(title = "Coefficients/Estimates", value = "coeff_est",
                                                      h3("Coefficients/Estimates"),
                                                      p("Each variable that we collect provides distinct estimates that", span("represent our data.", style = "font-style: italic"), "These estimates
                                                        can be described in different ways and are leveraged in different statistical models.",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      br(),
                                                        p(span("Mean:", style = "font-weight: bold"), "this is the average across all of our observations. In statistics, this is
                                                        considered as one of the 'moments', or mathematical values that describe the distribution of our data. In Figure 1 (A-C), 
                                                        panel A depicts a dataset that contains 100 observations for 'var1'. If we take all of these 100 values, add them up and 
                                                        divide this number by the sample size (n = 100) we would get the mean, or average. In our example, that mean would be 5.23. 
                                                        The red line in panel B depicts the mean.",
                                                          style = "font-family: 'times'; font-size:16px"),
                                                      p(span("Squared Deviation:", style = "font-weight: bold"), "the deviations are the difference between the data that we collected and the mean
                                                      that best represents that data. Going back to Figure 1, panel C provides an example of how the deviations are calculated for each observation.
                                                      To get the deviation for each participant, we subtract the mean from each participant's observed value (collected data). We can sum all of these values up 
                                                      and try to calculate a 'total deviations'. However, the positive values would cancel out some of the negative values which can result in zero deviations/variability.
                                                      Obviously, this is not an accurate representation of the data. So we should take another step.
                                                      Instead of summing the deviations, we square each participants deviation. By doing this, all of the values will be positive, therefore when we add them we will not
                                                      risk the negative and positive values canceling each other out. 
                                                      When we add up the deviations for our 100 observations, we will get the 'total squared deviation', or the total squared difference between our observed 
                                                      data points and the mean for Var1. This is referred to as the 'Sum of Squared deviatons'.",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      img(src="Deviations_3panel.jpg", align = "left", height = '40%', width = '40%'),
                                                      p(span("Variance:", style = "font-weight: bold"), "this is the variability, or dispersion, in observations around the sample mean for a given variable. 
                                                      Like the mean, this is considered as another moment that describes the range and frequency of the data. The variance is derived from the 
                                                      squared deviations. After we have calculated the squared deviation for all 100 observations in our example above and added these values together, 
                                                      we divide the squared deviation by the sample size minus 1, or N - 1 (in our case 100-1). This gives us the the variance for this particular variable in this sample.",
                                                      style = "font-family: 'times'; font-size:16px"),
                                                      p(span("Standard Deviation:", style = "font-weight: bold"), "similar to the variance, the standard deviation, or SD for short, is the despersion of the
                                                      data around the mean. Remember, to obtain our total deviations, we squared these values. So, the SD the is putting the variance into a standardized united. 
                                                      We standardize the data by taking the square root of the variance for the variable in our sample. By taking the square root,
                                                      the SD is now in units that are similar to the mean. When the SD is larger, that means the observed values for the variable spread out further
                                                      away from the mean, and when SD values are smaller, the observed values for the variable are closer to the mean. In the Figure 2, for Var1, I plot the
                                                      +/- 1 SD (blue lines) surrounding the mean (red line).",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      img(src="Data_100pt_mean_stdev.jpg", align = "right", height = '25%', width = '25%'),
                                                      p("You may be asking yourself,", span("Wait.... the squared deviations, variance, and SD sound very much alike. What gives?", 
                                                        style = "font-style: italic"),"You are correct -- all three represent a similar concept: the fit of the mean to the data, 
                                                        the variability in the data, how well the mean represents the data, and the 'error' in our data, commonly referred to as residuals in statistical models.",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      p(span("Covariance:", style = "font-weight: bold"), "this is the joint variability between two variables. In simple terms, this is a
                                                        measure of how much change in one variable is associated with a second variable. Take for example the ", a("'Our World in Data'.", 
                                                        href="https://ourworldindata.org/grapher/co2-emissions-vs-gdp?yScale=log"), "In this figure they are reporting the association
                                                        between CO2 emissions per capita (the average per person in that country) and GDP per capita. The covariation is, as the GDP per capita increase
                                                        there is an observed increase in annual C02 emissions by that country. So the nature of this fluctuation is related. If these
                                                        values were standardized, we could get the magnitude of the association, or effect size, via a Pearson's correlation (r) value. This
                                                        would give us the strength of the association, or fluctuation, among these two variables.",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      img(src="WorldDataCO2GDP.jpg", align = "center", height = '85%', width = '85%'),
                                                      p(span("Distribution:", style = "font-weight: bold"), "this is the spread in observations/data that we can expect.
                                                      The most common distribution is the Normal Distribution (in the Figure 3 labeled as 'Normal Dist') which forms a symmetric bell-shaped curve. 
                                                      The distribution follows a mathematic formula from which we can calculate the probability for observing a value out of all of the values in our 
                                                      data/sample. There are other distributions, too. For instance, in Figure 3 the data is generated based on input values in the table. 
                                                      For the normal distribution, the distribution is based on the sample mean (m) and standard deviation (SD). Then, the 
                                                      random uniform distribution is based on the distribution between the minimum (min) and maximum (max) values.  
                                                      Unlike the normal distribution, the random uniform distribution follows the probability formula where each observation is equally likely. 
                                                      In other words, drawing a value of 0 and 50 is equally likely in a sample of 100. Then, the t-distribution is what is often used in calculating 
                                                      statistical significance (i.e. p-value). As you can see in the figure below, the t-distribution and normal distribution are awfully similar, 
                                                      both are symmetric and have a bell-shaped curve. However, the t-distribution has wider tails and doesn't assume the population standard deviation. 
                                                      Instead, the t-distribution is defined by degrees of freedom (which are the logically independent values which are free to vary in a sample). 
                                                      Finally, the chi-square distribution is similar to normal and t-distribution. Unlike the normal and t-distribution, the values in chi-squared 
                                                      distribution are squared. Hence the name. So all negative values are zero, and always greater than 0. For more insights on distributions and the 
                                                      sample degrees of freedom (DF)  refer you to the Qunatitude podcast on ", 
                                                        a("'Statistics Degrees of Freedom'.", href="https://podcasts.apple.com/us/podcast/s3e08-statistical-degrees-of-freedom-an-intimate-stranger/id1484406501?i=1000539718170", 
                                                          target="_blank"),
                                                      "Dr. Curran and Dr. Hancock explain these issues in greater detail. Nonetheless, update the values in the table below and see how the values change.
                                                      Notice, different distribution require different population parameters, e.g. sample size, mean, SD, min value, max value and/or DF.",
                                                      style = "font-family: 'times'; font-size:16px"), 
                                                      plotOutput(outputId = "dist_plot"),
                                                      matrixInput(inputId = "Distribution_mat", 
                                                                  value = m, 
                                                                  rows = list(extend = FALSE, names = TRUE), cols = list(extend = FALSE, names = TRUE)),
                                                      br(),
                                                      p(span("Skew:", style = "font-weight: bold"), "this describes the symmetry of the data. Just like the mean and variance,
                                                      the skew is another moment of the data. Figure 4 provides examples of what skewed data would be relative to the normal
                                                      distribution of the data. Whereas the positive skew reflects an increased frequency of smaller values, negative skew reflects an increase
                                                        frequency of larger values, or larger than the mean.",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      img(src="Fig_SkewKurtosis.jpg", align = "right", height = '40%', width = '40%'),
                                                      p(span("Kurtosis:", style = "font-weight: bold"), "this describes the curvature and tails of the data. Like the mean, variance and skew,
                                                      kurtosis is a moment that describes the distribution of the data. Data can have positive and negative kurtosis. Positive kurtosis, known as a 
                                                      leptokurtic distribution, has a tighter peak with less distribution in the tails. Negative kurtosis, known as platkurtic distribution, is a flatter
                                                        distribution relative to the normal distribution with fatter tails. One way to remember the difference is 'platty fatty' for platykurtic",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      ),
                                             tabPanel(title = "Statistical Output", value = "stats_out",
                                                      h3("Values from Statistical Models"),
                                                      p("When statistical models are reported, specific language is often used to refer to the question, the test, the data collected and 
                                                        the value that the statistical model computes",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      p(span("Coefficient:", style = "font-weight: bold"), "this is the numerical value that is reported in a statistical test. For instance, if we perform a t-test,
                                                        we get an estimate of the difference between means, this would be our coefficient from the model that is reported in a table.",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      p(span("Correlation:", style = "font-weight: bold"), "as referred to above, correlation is the association between variables that is 
                                                      standardized. The value is contrained to -1 to 1. This is an interpretable metric of the association between two variables in a study. 
                                                      For example, in Figure 5 below, I simulate 150 observations for the Means and Correlations coefficients in the table above it. 
                                                      The bottom panel reflects the correlation among variables X1 and X2. You can modify the correlation coefficient (r) for the variables between -1 and
                                                      1 to observe the change in the plot. (Note, if r= .3 in column X1, it should also be .3 in X2, as they reflect the same association)",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      p(span("Effect Size:", style = "font-weight: bold"), "this represents how small or large the association (or difference) is. When we 
                                                      compare the standardized covariation between two variables via a Pearson's r correlation coefficient, we can report whether the effect is large, medium,
                                                      or small. There are certain standards, but these are debated as we will note later. On the other hand, when we're comparing group means, 
                                                      we can calculate the magnitude of this effect (or difference) by using a Cohen's D calculation. 
                                                      Unlike the Pearson's r which ranges from -1 to 1, the Cohen's D can exceed one. This is because the association between two variables 
                                                      cannot exceed a perfect linear relationship, which is r = 1.0, however, the difference in means between two variables can get larger and larger, theoretically.
                                                      If you modify the means and correlation (r) values in the table below, you can observe how the correlation can shift even when
                                                        means stay the same, and vice versa. Reflecting their unique characteristic.",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      plotOutput(outputId = "cov_mean_ex"),
                                                      matrixInput(inputId = "cov_examp", 
                                                                  value = r_ex, 
                                                                  rows = list(extend = FALSE, names = TRUE), cols = list(extend = FALSE, names = TRUE)),
                                                      p(span("Estimated v Observed:", style = "font-weight: bold"), "statistical models attempt to estimate the association amongst the observed data.
                                                      For instance, when we calculate a correlation between two variables, like we did in Figure 5, we get one coefficient (or numerical value) 
                                                      that represents the association between those variables. This value reflects the line that is meant to 'best' approximate the data. 
                                                      As you see in the bottom panel of Figure 5, the line is fit through our data, but it doesn't perfectly match with the observed data (i.e.,
                                                      real data or those we simulate and know values for). Thus, the line estimates the values for our observed data. Often times they are not a perfect match",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      p(span("Residuals:", style = "font-weight: bold"), "the difference between what our model estimates and the data we observed are the residual values.
                                                      For example, in Figure 5 we fit the linear line to our data. Some values it perfectly overlaps with and others no so much. Now,
                                                      if we were to take the values that the value estimates for that data point and subtract it from the data point we observed, we would get a residual 
                                                      for that pair. Let's consider this in the example below, where Var3 and Var4 for 20 data points correlates r = - .55. We can take the value 'y', the observed data,
                                                      and the value 'x', the estimated data. The difference (dotted red line) between the estimated and observed points is the residual for this data point. 
                                                      If we do this for all of the values estimated by the line (i.e., model) and those we observed, we can get the total residuals for the model. This provides a reference
                                                      for how well this model explains the relationship, the lower the residuals, the more that the model (line) explains the relationship between the changes in the variables.
                                                      The 'best fit' line are what some statistical methods utilize to minimize the residuals and improve prediction. More on this in the regression tab.",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      img(src="residual_ex.jpg", align = "center", height = '100%', width = '100%'),
                                                      p(span("Confidence Interval:", style = "font-weight: bold"), "this is the range for which an observed estimate in a model, like a difference between two means,
                                                      would vary within an upper and lower bound. Typically, the 95% confidence interval (CI) is used. 
                                                      So, across all random samples for the size of our data, 95% of those samples will have a 95% confidence interval that will contain the estimate value of our data.
                                                      In other words, say we had a sampled 100 participants, got our means
                                                      and SD. If we were to repeatedly sample based on the sample size, mean, and SD we would get an estimate with a lower and upper bound, or our 95% CI. 
                                                      This CI is related to the p-value, as the distributions are based on sample characteristics, such as mean, SD and degrees of freedom. When the 95% CI does 
                                                      not cross zero, then the value is significant (i.e., p < .05), but if the 95% CI does cross zero, then the value is not significant (i.e. p > .05).",
                                                        style = "font-family: 'times'; font-size:16px"),
                                                      ),
                                             tabPanel(title = "Practice", value = "Prelim_practice",
                                                      h3("Practice Problems"),
                                                      p("Now that we have coverd things such as the mean, deviations, variance, sum of squared deviations, and standard deviation, it would be good
                                                        to practice some of these to get more accummed to the concepts.",
                                                        style = "font-family: 'times'; font-size:16px"), 
                                                      p("I will add some problems sets in the linked",
                                                        a("Google Sheet.",  href="https://docs.google.com/spreadsheets/d/1-a6gkWSewdoyRoqaS8anRDLBNfhJGhf1vPORVUmHCLM/edit?usp=sharing", target="_blank"), "For calculating the moments of data,
                                                        please refer to the 'Moments' tab.",
                                                        style = "font-family: 'times'; font-size:16px")
                                                      )
                                             ),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             br(),
                                             p("For Citation & Documentation purposes, code will will be available on",
                                               a("Github", href="https://github.com/", target="_blank"), "once an initial full version is complete.",
                                               style = "font-family: 'times'; font-size:10px"),
                                             p("© 2022 Michael Demidenko. Please contact for more details",
                                               style = "font-family: 'times'; font-size:10px")
                                         )
                                     ),
#T-test text ############################### 
                            tabPanel("T-tests",
                                     
                                     mainPanel(width = "100%", style='padding-left: 30px; padding-right: 30px',
                                               h2("One Sample & Two Sample t-tests"),
                                               p("What is the purpose of this tab? Often we collect data for a sample of participants where we may want to compare groups on some variable or variables. 
                                               These groups will reflect unique averages and variability on a variable. These averages and variability can be harnessed to compare
                                               how the groups meaningfully differ, so we can then derive an empirical conclusion about some difference.",
                                                 style = "font-family: 'times'; font-size:16px"),
                                               p("In this tab, both the One-sample and Two-sample t-tests are reviewed. How will this be done?
                                               Based on the means, standard deviations (SD), and sample size (N) that are provided, 
                                                 a normally distributed dataset is simulated. The hypothetical sample that is simulated is used in the examples below. 
                                                 To see how data points are distributed relative to the mean, two variables are plotted plotted using a boxplot 
                                                 to visualize the means and their relative SDs. Then, the distribution of one variable and the 'population mean' is plotted. 
                                                 After which, we compute a One-Sample t-test on the mean(s) and SD(s). 
                                                 For the One-Sample and Two-Sample t-test, the formulas, associated effects, and output is reviewed. 
                                                 In general, this tab is to provide examples have the numerical values changes based on the Mean, SD , N, 
                                                 Alpha and Population Mean (which is often hypothesized to be nil). ",
                                                 style = "font-family: 'times'; font-size:16px"),
                                               p("Before moving ahead, let's start off by selecting the values that will be used to simulate the data. The sample size determines the number of hypothetical participants
                                                 that data will be simulated for. The population mean is simply what the central tendencies for a variable is in the population. Typically, this value is
                                                 held at zero in psychological research. So, here we start with the value as zero, but feel free to adjust once you've made it through each tab. The alpha value is
                                                 how we define what effect is meaningful, again, traditionally this value is p = .05. Next, we select means and standard deviations for two variables.
                                                 Var 1 Mean/SD, Var 2 Mean/SD are used for the One-sample t-test tab. Group A Mean/SD and Group B Mean/SD are used for the Two-sample t-test, where we compare means between two groups. 
                                                 For now, run the default set values. These will make more sense hopefully after you make through each tab in this section
                                                 and see what toggle they are based on.",
                                                 style = "font-family: 'times'; font-size:16px"),
                                               fluidRow(
                                                 column(width = 4,
                                                        sliderInput("sample", label = "Sample Size",
                                                           min = 5, value = 125, max = 2000, step = 15)
                                                        ),
                                                 column(width = 4,
                                                        sliderInput("pop_mean", label = "Population Mean (e.g., H0)",
                                                           min = 0, value = 0, max = 30, step = .5)
                                                        ),
                                                 column(width = 4,
                                                        sliderInput("alpha", label = "Alpha α (Type I error rate)",
                                                           min = 0.0001, value = .05, max = .20, step = .025)
                                                 ),
                                               ),
                                               br(),
                                               h4("Variables for One-Sample t-test tab",
                                                  style = "font-family: 'times'"),
                                               fluidRow(
                                                 column(width = 3,
                                                        sliderInput("mean1", label = "Var  1 Mean ",
                                                           min = 0, value = 9, max = 30, step = .5)
                                                 ),
                                                 column(width = 3,
                                                        sliderInput("sd1",  label = "Var 1 SD",
                                                           min = 0.1, value = 2.25, max = 25, step = .5)
                                                 ),
                                                 column(width = 3,
                                                        sliderInput("mean2", label = "Var 2 Mean",
                                                           min = 0, value = 20, max = 30, step = .5 )
                                                 ),
                                                 column(width = 3,
                                                        sliderInput("sd2", label = "Var 2 SD",
                                                           min = 0.1, value = 4, max = 25, step = .5)
                                                ),
                                               ),
                                               br(),
                                               h4("Variables for Two-Sample t-test tab",
                                                  style = "font-family: 'times'"),
                                               br(),
                                               fluidRow(
                                                 column(width = 3,
                                                        sliderInput("grp_m1", label = "Group A Mean",
                                                           min = 0, value = 22, max = 30, step = .5)
                                                 ),
                                                 column(width = 3,
                                                        sliderInput("grp_sd1",  label = "Group A SD",
                                                           min = 0.1, value = 6.2, max = 25, step = .5)
                                                 ),
                                                 column(width = 3,
                                                        sliderInput("grp_m2", label = "Group B Mean",
                                                           min = 0, value = 3.5, max = 30, step = .5)
                                                 ),
                                                 column(width = 3,
                                                        sliderInput("grp_sd2", label = "Group B SD",
                                                           min = 0.1, value = 5, max = 25, step = .5)
                                                 ),
                                               ),
                                                        
                                               p("Click the action button below to simulate the data and populate figures in each tab here. If you alter any of the above 
                                                 values, click this button again to repopulate the tables/figures",
                                                 actionButton("run1", "Run T-tests!", 
                                                              style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00; font-size:75%"),
                                                 style = "font-family: 'times'; font-size:16px"),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                                 tabsetPanel(id = "t_test_tabset",
                                                   tabPanel(title = "One-Sample T-test", value = "one_sample_t",
                                                            h3("One Sample T-test", style = "font-family: 'times'"),
                                                            p("In this tab, we focus on the One Sample T-test. First, we start by plotting two boxplots for the simulated Var1 and Var2 in Figures 1 and Figure 2. These values reflect the values for Mean & SD 1 and 2 above. 
                                                            The boxplots below provide visual representation of the means and SDs for Var 1 and Var 2.",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            br(),
                                                            fluidRow(
                                                              splitLayout(cellWidths = c("50%", "50%"),
                                                                          plotOutput(outputId = "hist_var1"),
                                                                          plotOutput(outputId = "hist_var2")
                                                                          )),
                                                            br(),
                                                            h3("Plotted Distribution: Variable 1",
                                                               style = "font-family: 'times'"),
                                                               p("As you observed above in Figure 1 and Figure 2, the data points are [fairly] evenly distributed around the mean following the normal distribution. 
                                                               In other words, there are more data points clustered near the mean than the upper or lower extremes. To make sense of this, let's look at how frequently
                                                               each value occurred. Figure 3 reflects the distribution of the simulated frequencydata for Var 1 (based on specified mean) and 
                                                               +/- 1, 2 and 3 standard deviations relative to the specified mean. For reference, the specified population mean (that is used in the One-Sample t-test below) 
                                                               is also plotted in the figure. Recall, the population mean I set by default to zero, so it should be at zero (unless you updated it). Since the data is resampled 
                                                               from the normal distribution, the mean will vary a touch.",
                                                                 style = "font-family: 'times'; font-size:16px"),
                                                            p("Take a look at the frequently plot and notice from the normal distribution where most 
                                                               of the data falls. For example, approx. 68% of the observations are between -/+1 SDs of the sample mean, or the dashed blue line, 
                                                               and approx. 95% of observations are within -/+ 2 SDs of the sample mean, or the green line. Then, approx. 5% of the observations are beyond the -/+2 SD line.
                                                               When you end up toggling the sample size, you'll observe that the data will follow the 'central tendency' in the data. 
                                                               Specifically, the frequencies of values begin to cluster around the mean and the curve will look like more and more like the normal distribution.",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            plotOutput(outputId = "t_test_dist"),
                                                            p("Let's consider Figure 3 in a real-world example like height. As of my google search from January 2022, the average American height is 69inches. So, then we 
                                                              can say that the population mean of all Americans is 69 inches. Now, say we survey people randomly in our city (or state) and ask them to report their height or measure it manually. 
                                                              At first, a lot of these values
                                                              may be well above or below this mean. Our first set of participants may be a family of six with kids. So we have two average height adults but four children that are substantially shorter.
                                                              While these initial values may not reflect the population mean, they do accurately reflect our biased sample. Nevertheless, 
                                                              if we continue to randomly sample people and ask them about their hate, we will get more and more data points. Over some time, we may observe the central tendency
                                                              in our data. In otherwords, we will get more values at or near 69 inches, the average height of an American. We will also have participants in the lower bounds (that include some youth) and upper bound (of people taller than the average).
                                                              Once we have a large enough sample, we will see the normal distribution and can compare how each person compares to the population average.",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            h3("Formula for One-Sample t-test",
                                                               style = "font-family: 'times'"),
                                                            p("Measuring people's high is fun, but we may want to compare how a variable in our sampel differences from the population estimate.
                                                            Keeping in mind how the data shifts around the mean, we can now consider the One-Sample t-test formula. If you ever came 
                                                            across the language, “Our sample demonstrated a response (Mean = 2.9, SD = 2.0) that was significantly greater than the null 
                                                            t(49) = 49/0, p < .00001”, then you had experienced reading the results from a One-Sample t-test. If you have not, well, now you have. 
                                                            The purpose of the One-Sample t-test is to estimate a t-value (t) based on the population mean (μ) and the sample mean (m) 
                                                            relative to proportion of variability/SD (s) scaled by the sample size (√n)",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            uiOutput(outputId = "one_t_formula"),
                                                            img(src="grocerystore1.jpg", align = "left", height = '40%', width = '40%'),
                                                            p("For example, say I am a grocery store manager that is interested in creating a sign that'll implicitly cause customers to stop 
                                                            at a particular location inside the store. This may advantageous for me, because they may then see the candy bars and buy one.
                                                            We're not trying to compare the effect of one sign over the other *yet*, but rather 
                                                            we are curious whether there is any effect of the sign in this part of the store. We created a sign and got a research assistant 
                                                            with a stopwatch (yes, we're a bit inefficient) that determines when a participant (customer) enters the defined vicinity. Using their 
                                                            stop watch, the assistant calculates (approximately) how long the customer stopped for. After a few long weeks, a really tired research assistant, and worn 
                                                            out stop watch, we will have a dataset with a sample of participants that stopped for an average time (Var 1 mean) and these 
                                                            participants varied in their stopping time to some degree (Var 1 St Dev).",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            p("Unless we have a prior estimate of how long populations 
                                                            randomly stop in this given area, we would specify this. In this case, we will assume that population mean stopping time is zero (implicit assumption 
                                                            in a lot of scientific research). 
                                                            So, we can propose the null (H0) hypothesis, that there would be no difference between how long people normal stop in this space (zero/null) 
                                                            and with our sign there. The alternative (H1) hypothesis would be the opposite, that we expect a significant difference between the population 
                                                            mean and our observed mean with the sign there. ",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            img(src="OneSample_t_H0H1.jpg", align = "right", height = '40%', width = '40%'),
                                                            p("To obtain our estimate of significance we would use the One-Sample t-test. Let's simplify the formula for our example. Our goal is to obtain a 
                                                            t-statistic that compares the signal in the numerator (what we want to maximize, stopping at sign) relative to the noise in the denominator 
                                                            (what we want to limit, the variability between people in stopping). This maximization of the signal-to-noise ratio is central to experimental psychology 
                                                            (Cronbach, 1957). To maximize the signal, we can increase the sample mean in comparison to the population mean (μ). Another way to increase 
                                                            the signal is by minimizing the noise, or our SD. This can be done by reducing variability *between* subjects or our measurement error. Maybe a more attractive sign?
                                                            A larger sign? An electronic sign? A sign that says 'FREE' but with small text so they have to stop to read it? Anyhow, there are many ways to reduce measurement error.
                                                              The error can also be minimized by a larger sample size (i.e. N), as the SD is scaled by N (see Figure 4 for example).",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            h3("Descriptive Statistics & Results for Simulated Data",
                                                               style = "font-family: 'times'"),
                                                            tableOutput(outputId = "one_t_table"),
                                                            p("The above table provides the sample size (should be equal), means, and SDs input into the fields at the start of this page (mean/SD will be off slightly). 
                                                            Since we're working with a One-sample t-test, let's pay attention to Var1 for now. You can follow along by entering each value 
                                                            into the One-Sample t-test formula manually (since it’s relatively easy). In the t-test formula, you will put ‘Var1 Mean’ in plae of 'm' 
                                                            and ‘Var 1 SD’ in place of ‘s’, ‘sample size’ will replace ‘n’, and the population mean will replace 'μ'. 
                                                            I provide an example below, so try this manually for Var1 and Var2, to see what you get.",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            img(src="OneSampleT_formulaExamp.jpg", align = "right", height = '100%', width = '100%'),
                                                            p("I hope you have manually calculated the t-statistic for each of the variables in the table above. 
                                                            Below, for *Var1*, I have a probability density plot for a t-test includes your t-statistic (t), 
                                                            your degrees of freedom (df), which are the number of observations minus 1 (n-1), and the p-value. In the One-Sample t-test 
                                                            probability density plot below, you will see the critical cut off (alpha/p-val) of 95% (p < .05) and the blue dot represents 
                                                            the t-statistics from our variable. If you did your math correctly, your t-statistic very similar to this value, with maybe some rounding differences. 
                                                            Unless, of course, something went wrong… 
                                                            and it’s not uncommon for things to go wrong. By pulling up a t-distribution table, you can obtain the significant value (i.e., p-value), too. You simply 
                                                              identify the t-statistic, the degrees of freedom (n – 1), and one-tailed or two-tailed. In this figure, we use a two-tailed test. ",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            plotOutput(outputId = "t_test_dist_t"),
                                                            p("Given that you have two variables (Var 1 & Var 2) 
                                                            that you can adjust the mean and SD for, compare how the effects differ and that means for the t-statistic and the p-value. 
                                                            You can play around with these on your second round
                                                            though to see how the sample size, mean, and SD change these. Look at the above T-test formulate again and observe 
                                                            how the 'significance' of the effect sways based on these values. You can compare the above plot for Var 1 and the below plot for 
                                                            Var 2. While these tests can tell us something meaningful, if we’re not thoughtful when using them we may misunderstand the 
                                                            meaningfulness of an effect because we are at times consumed by finding that “p < .05” golden ticket.",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            plotOutput(outputId = "t_test_dist_t2"),
                                                            p("What we can gather from this is the significance of our effect for *this* sample and its size and variability. This significance, again, 
                                                            is relative to our population mean. So, while it is common to say that the population mean is zero, this often is not true for many scenarios. 
                                                            For instance, we may find that people in some parts of the store tend to stop for no reason, so the population mean may be, say, 11 seconds. 
                                                            So, if we had known that the population mean was closer to 11 seconds, the assumption that a population mean of zero would be incorrect and 
                                                            thus provide us with the wrong conclusion. We may, in effect, find significance when in fact there may be none if we were to 
                                                            use a specific (point estimate) versus a zero (point null) estimate -- committing a Type I error. 
                                                            This topic has been extensively discussed and is a point of contention in several statistical tests amongst researchers in the field, 
                                                            especially as it relates to the 'crud factor' (Cohen, 1994).
                                                            The crud factor (Meehl, 1990) essentially means that if our population mean is assumed to be zero, as the sample gets larger even
                                                              even the most neglible/trivial differences will be 'significant'.",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            br(),
                                                            h3("Impact of Sample Size on Standard Deviation",
                                                               style = "font-family: 'times'"),
                                                            p("I mentioned earlier to consider the denominator of the One Sample t-test formula. When you around with different 
                                                            combinations of mean, SD, and sample size (N) values, you will nootice an important trend: the larger the sample, the 
                                                            larger t-statistic, and ipso facto the larger the p-value. This represents the issue of the ‘crud factor’, whereby the larger number 
                                                            of participants the lower the critical threshold for significant is.
                                                            As a result of this, you reach a point where you may need to decipher whether an effect is or isn't meaningful? Sometimes, sadly, the answer is a clear: no. 
                                                            Other times it’s a resounding… maybe?",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            p("Here the effect of the crud factor is in essence achieved by the SD (our variability) being shrunk by the N. To demonstrate this, 
                                                            we can take the mean of Var 1 and see how the N proportionally reduces the influence of the SD from the denominator on the numerator. 
                                                            In term of fractions, if we make the denominator SMALLER we can make the result of the numerator LARGER. Figure 4 shows how the 
                                                              SD shrinks in magnitude from N = 5 to 2000.",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            uiOutput(outputId = "one_t_form_denom"),
                                                            plotOutput(outputId = "t_test_reduc_sd_1"),
                                                            br(),
                                                            br(),
                                                            h3("Cited Work"),
                                                            p("Cohen, J. (1994). The earth is round (p <.05). American Psychologist, 49(12), 997–1003. https://doi.org/10.1037/0003-066X.49.12.997",
                                                              style = "font-family: 'times'; font-size:10px"),
                                                            p("Cronbach, L. J. (1957). The two disciplines of scientific psychology. American Psychologist, 12(11), 671–684. https://doi.org/10.1037/h0043943",
                                                              style = "font-family: 'times'; font-size:10px"),
                                                            p("Meehl, P. E. (1990). Why Summaries of Research on Psychological Theories are Often Uninterpretable. Psychological Reports, 66(1), 195-224. 10.2466/pr0.1990.66.1.195",
                                                              style = "font-family: 'times'; font-size:10px"),
                                                   ),
                                                   tabPanel(title = "Two-Sample T-test", value = "two_sample_t",
                                                            h3("Two Independent Group Means",
                                                               style = "font-family: 'times'"),
                                                            
                                                               img(src="TwoSample_Ttest_ex.jpg", align = "right", height = '45%', width = '45%'),
                                                               p("In the One-Sample T-test tab on this page we focused on obtaining a statistical inference for a variable compared to what we'd expect in the population. 
                                                               It considered how a mean and SD for one group of participants in a sample differed in a meaningful way.
                                                               However, often we may want to understand how more than one groups differ on a variable.
                                                               Say we wanted to consider the difference BETWEEN groups, so two means and SDs. Going back to our earlier example from the grocery store, 
                                                               let's say that we wanted to determine whether there was a meaningful 
                                                               difference in stopping by signs that were placed at two stores in different states, say Dallas, Texas and Lincoln, Nebraska. So now we have the same sign, 
                                                               we have the same grocer, same part of the store, but would like to know whether there is [in simple terms] something meaningfully different between
                                                               the store in Dallas and Lincoln. If our groups consist of independent samples and variability that is similar (i.e., sample of people in each group 
                                                               are unique and SD is comparable), we can conduct a Two-Sample t-test.",
                                                                 style = "font-family: 'times'; font-size:16px"),
                                                            h3("Two-Sample T-test Formula",
                                                               style = "font-family: 'times'"),
                                                            img(src="TwoSample_t_H0H1.jpg", align = "left", height = '40%', width = '40%'),
                                                            p("In the One-Sample t-test, you will recall that we focused on a single average, relative to a population estimate. Bulding off of the single group average, the two-sample
                                                              t-test compares the differences between group groups on a variable within a sample. Given these changes, we can now update our H0 and H1 hypothesis. For example, now we're hypothesizing
                                                              a difference between groups. So our null would be no differences between the two locations, Dallas and Lincoln, and an alternative (for simplicity, undirected) hypothesis that there would be a
                                                              significant difference in stopping time.",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            
                                                            p("As we observed two means in the numerator for the One-sample t-test formula, the same is observed in the Two-Sample t-test formula. However, unlike the One-sample
                                                            in the Two-Sample t-test these are both samples that we have observed data for, Sample A and Sample B. So, for both samples, unlike the population, 
                                                            we have their actual means but ALSO their SD(s). So now we can compare how data for observed groups differences. Effectively, we can follow similar procedures to compare the sample means to determine the difference, 
                                                            or effect, between groups.",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            uiOutput(outputId = "two_mean_t_formula"),
                                                            h3("Descriptive Statistics & Results for Simulated Data",
                                                               style = "font-family: 'times'"),
                                                            p("Below is a Table of the descriptive statistics for the simulated data for our groups (A & B), the sample size (N), group means, and group SDs.
                                                              Of note, the combined sample size will be what was specified above. However, we split this across two groups (A/B)",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            tableOutput(outputId = "two_mean_table"),
                                                            p("Below, in Figure 5, you'll find the boxplots for the mean and SD for each group (A & B). Like Figure 1 & 2 in the one-sample t-test tab, you can observe the spread of the observed data points 
                                                            that are simulated for each group. Then, Figure 6 provides a visual representation how the Var1 and Var2 relate to each other across groups. The black line represents the average association between Var1 and Var2, A & B combined. 
                                                            Then the yellow and green represent the association between Var1 and Var2 for groups A and B, respectively. 
                                                              In the 'Correlation' tab I will revisit how these groups difference can impact conclusions.",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            plotOutput(outputId = "two_mean_dist"),
                                                            p("Using the formula above and the provided descriptive statistics, calculate the t-statistics and compare it to the example here.
                                                            As we did for the One-Sample t-test, we again can calculate the t-statistic from our formula and determine the p-value, or significance, of the difference
                                                            between our groups for [this] sample and distribution. You will find the t-statistic, df, and p-value for the Two-sample t-test below.
                                                            Revisiting our example, we may find that the means and SD for stopping at a sign 
                                                            between stores are extremely different. Specifically, that for Group A, or the store in Dallas, the stopping time is significantly large (p < .0001). So for some reason the sign is more effective
                                                            than in Lincoln. To understand the nuance of this, it may be worthwhile to acquire some more information to see how it can be used in A/B testing in the store. If we get customers stopping more, perhaps we can 
                                                              sell more products and increase our profits.",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            plotOutput(outputId = "two_mean_test1"),
                                                            h3("Impact of Sample Size on Standard Deviation",
                                                               style = "font-family: 'times'"),
                                                            p("As you keep playing with the parameters, you'll find a similar cautionary tale of 'significance' (or the golden ticket, p < .05).
                                                            For the Two-Sample t-test, the t-statistic that we obtain may be swayed by the scaling of SD in the denominator by the N.
                                                            As we say in Figure 4 for the One-Sample t-test, in Figure 7, for the Two-Sample t-test, we observe a similar scaling by the 
                                                            sample size. In our case, we found a difference between our groups but we had data for 10,000 customers. Is the difference meaningful? To better understand,
                                                              it'd be worthwhile to collect some more data, determine how it relates to our goal (i.e., sales), and see the sign and stopping time increases sold units.",
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            plotOutput(outputId = "twomean_reduc_sd_1"),
                                                            h3("Meaning of p-value in NHST?", style = "font-samily: 'time'"),
                                                            p("One important detail to remember is that the results from the significance test are related to the present sample. 
                                                            Most statistics used in the null hypothesis significance testing (NHST) are based on the parameter (mean, SD, Pearson’s r) distributions. 
                                                            These distributions are often specific to the respective observations/data/samples.",
                                                              style = "font-family: 'times'; font-size:16px"),
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
                                                              style = "font-family: 'times'; font-size:16px"),
                                                            br(),
                                                            br(),
                                                            h3("Cited Work"),
                                                            p("Kruschke, J. K., & Liddell, T. M. (2018). Bayesian data analysis for newcomers. Psychonomic Bulletin & Review, 25(1), 155–177. https://doi.org/10.3758/s13423-017-1272-1",
                                                              style = "font-family: 'times'; font-size:10px"),
                                                   ),  
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 
                                                 
                                                 p("Packages used - Plotting: ggplot w/ tidyverse | plotting htest t-distributions: webr via plot() by Keon-Woong Moon",
                                                   style = "font-family: 'times'; font-size:10px"),
                                                 p("For Citation & Documentation purposes, code will will be available on",
                                                   a("Github", href="https://github.com/", target="_blank"), "once an initial full version is complete.",
                                                   style = "font-family: 'times'; font-size:10px"),
                                                 p("© 2022 Michael Demidenko. Please contact for more details",
                                                   style = "font-family: 'times'; font-size:10px")
                                                 )
                                     )
                            ),


#ANOVA text ###############################

tabPanel("ANOVA",
         mainPanel(width = "100%", style='padding-left: 30px; padding-right: 30px',
                   h2("One-way & Two-Way ANOVAs"),
                   p("What does this tab represent? In  the 'T-tests' tab, I reviewed the comparison of data with a single mean
                   compared to the population (One-sample t-test) and then the comparison between two sample means (Two-sample t-test).
                   In this tab, I expand on the comparison of means using the Analysis of Variance, or ANOVA. The ANOVA is an extension of mean
                   comparisons. In the case of t-test, we compard 1-2 means, but now, we compare the difference across 2+ means. ANOVA compares the difference on a 
                   continuous (or as often used, ordinal) outcome (DV)  
                   across a categorical predictor (IV), or groups. Say for instance we were running ads online and we wanted to compare the difference in social media engagement, 
                   a continuous vairable, across three or more ad types (i.e., categories). Our categories may be image types, text types, days of week, times of day, etc. 
                   For this example, let's say we will vary on category of photo (i.e., Image type): Nature, City and People. This is categorical value, because it does
                   not have an inherent numerical value that is ordered or continuous.",
                     style = "font-family: 'times'; font-size:16px"),
                   img(src="ANOVA_image_type.jpg", align = "center", height = '100%', width = '100%'),
                   p("We may run several Two Sample T-test to determine how the means for continuous values 
                   (i.e., engagement) may vary across the categorical values (i.e., image types). But, this increases the likelihood of a false positive (or Type II error). To avoid this,
                   we instead considered differences across ALL of our means simulatenously. This would be our One-way (1x3) ANOVA,
                   three means across one level. Then, I extend the One-way ANOVA, to a two by three ANOVA (2x3), 
                     whereby we consider the mean difference across a continous variables across ad types (our three types above) by two group levels, such as sex (male versus female)..",
                     style = "font-family: 'times'; font-size:16px"),
                   h3("Selecting Means/SDs One-Way ANOVA",
                      style = "font-family: 'times'"),
                   p("Let's start off by selecting the sample size used to simulated the observations for the One-Way and Two-Way ANOVAs. Recommended: start off with the default settings 
                   to simulate the data, make your way through each tab and then adjust the default values to see how figures/output changes.
                   For the One-Way ANOVA (three group means), select the between group means and standard deviations for Var 1, Var 2, and Var 3. This will be relevant for the One-Way ANOVA
                     tab below. For our example in the tabs below I will be use the example of average (M)  engagement on social media for images that consist of City, Nature and People.",
                     style = "font-family: 'times'; font-size:16px"),
                   br(),
                   br(),
                   fluidRow(
                     column(width = 10,offset = 4,
                            sliderInput("aov_sample", label = "Sample Size",
                                        min = 5, value = 50, max = 2000,step = 15))
                   ),
                   fluidRow(
                     column(width = 2,
                            sliderInput("aov1_m", label = "Between(1):  Var1 Mean",
                                        min = -20, value = 5, max = 20, step = .5)),
                     column(width = 2,
                            sliderInput("aov1_sd", label = "Between(1):  Var1 SD",
                                        min = .01, value = 1, max = 10, step = .1)),
                     column(width = 2,
                            sliderInput("aov2_m", label = "Between(1):  Var2 Mean",
                                        min = -20, value = 11, max = 20, step = .5)),
                     column(width = 2,
                            sliderInput("aov2_sd", label = "Between(1):  Var2 SD",
                                        min = .01, value = 3.8, max = 10, step = .1)),
                     column(width = 2,
                            sliderInput("aov3_m", label = "Between(1):  Var3 Mean",
                                        min = -20, value = 6.5, max = 20, step = .5)),
                     column(width = 2,
                            sliderInput("aov3_sd", label = "Between(1):  Var3 SD",
                                        min = .01, value = .75, max = 10, step = .1)),
                   ),
                   br(),
                   br(),
                   h3("Selecting Means/SDs Two-Way ANOVA",
                      style = "font-family: 'times'"),
                   p("Continuing with our theme for averages of engagement on social media for City, Nature and People images, for the Two-way (2x3) ANOVA we select the second set of values.
                   In the Two-way ANOVA, for our example, we will compare the three images types between sex (Male/Female). So, the means above (prefix 'Between(1)') we selected the mean/SD values for our first group, `males`. 
                   Then, below (prefix 'Between(2)') we will select the mean/SD for each image type for our second group, `females`. For now, keep the defaults as they are, and it will hopefully be
                   a bit more intuitive when you make it through each tab. Then it'll be more intuitive which toggle affects which associated value in the tables/figures.",
                     style = "font-family: 'times'; font-size:16px"),
                   
                   fluidRow(
                     column(width = 2,
                            sliderInput("aov4_m", label = "Between(2):  Var1 Mean",
                                        min = -20, value = 4.2, max = 20, step = .5)),
                     column(width = 2,
                            sliderInput("aov4_sd", label = "Between(2):  Var1 SD",
                                        min = .01, value = 1.8, max = 10, step = .1 )),
                     column(width = 2,
                            sliderInput("aov5_m", label = "Between(2):  Var2 Mean",
                                        min = -20, value = 17, max = 20, step = .5)),
                     column(width = 2,
                            sliderInput("aov5_sd", label = "Between(2):  Var2 SD",
                                        min = .01, value = 2.7, max = 10, step = .1)),
                     column(width = 2,
                            sliderInput("aov6_m", label = "Between(2):  Var3 Mean",
                                        min = -20, value = 12, max = 20, step = .5)),
                     column(width = 2,
                            sliderInput("aov6_sd", label = "Between(2):  Var3 SD",
                                        min = .01, value = 6, max = 10, step = .1)),
                   ),
                   br(),
                   h3("Action Button: Simulating/Resimulating Data",
                      style = "font-family: 'times'"),
                   p("After you run the default settings, these toggles will make more sense after you make through each tab in this section.
                     To simulate the data for the One-Way ANOVA, push the button:",
                     actionButton("run4", "Run One-Way ANOVA!", 
                                  style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00; font-size:75%"),
                     style = "font-family: 'times'; font-size:16px"),
                   br(),
                   p("To simulate the data for the One-Way ANOVA, push the butthon:",
                     actionButton("run4.1", "Run Two-Way ANOVA!", 
                                  style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00; font-size:75%"),
                     style =  "font-family: 'times'; font-size:16px"),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   
                   tabsetPanel(id = "one_way_tabset",
                               tabPanel(title = "One-way ANOVA", value = "anova_panel",
                                        h3("One-way ANOVA", style = "font-family: 'times'"),
                                        p("Let's start off by considering the formula for an ANOVA. As mentioned above, the ANOVA is comparison of means across 2+ categories.
                                        When estimating the differences across the categorical means, in the One-way ANOVA, 
                                        there are several values that are calculated. Some of these elements are relatively similar to the t-tests, but there is added complexity
                                        with the increased number of groups.",
                                          style = "font-family: 'times'; font-size:16px"),
                                        p("In the One-way ANOVA, we calculate the Sum of Squares total (SST), which 
                                        is the deviation across all of our observed data for our variable compared from the overall mean. 
                                        The Sum of Squares Between (SSB), which is the deviation of the group means (k) relative to the overall scaled by the group size (N). 
                                        The Sum of Squares Within (SSW), which is deviation of the observed scores
                                        for a variable within that groups means. 
                                        Then, the mean squared error within (MSW) and mean squared error between (MSB) are used to obtain
                                        an F-statistic to determine how meaningful the difference is. Let's go over each of these in more detail below.",
                                          style = "font-family: 'times'; font-size:16px"),
                                        br(),
                                        h3("One-way ANOVA Formula", style = "font-family: 'times'"),
                                        p("The full number of deviations in the ANOVA are captured in the Sum of Squares Total (SST).
                                        The formula for this is below. It is the sum of squared deviations (or differences) of each observed score from the grand mean (grand = overall). This
                                        is the total deviations in our dataset that are then decomposed in the deviations of interest, which are the between group differences, 
                                        and deviations of little interest, which are the within group differences. 
                                        As I will show in Figure 2, the SST is simply taking all of your observations, 
                                        across all image types, etc, and calculating your overall mean. Now that you have your overall mean, you calculate your square deviations by 
                                        subtracting the mean and the observed score. This equivalent to the sum of square variance that were shown in the 'Preliminary' tab. 
                                        The only difference here is now we have values assoicated with our groups (k). In the formula, notation for groups is j, and the notation for individual is i. 
                                        So we take each value for each individual from each group, and subtract it from this overall 'grand' (X-bar), square these values, and sum them all up 
                                        to get the total sum of squares [total].",
                                          style = "font-family: 'times'; font-size:16px"),
                                        uiOutput(outputId = "aov_SST"), 
                                        p("Next, we calculate the Sum of Squares Between (SSB), or the factor effect (i.e., category/group). Continuing with our imagine type on the effect of ads, the factor would be the image type. 
                                        Unlike the SST which subtracts each individual score from the grand mean, the SSB is the sum of squared individual group means (k) minus 
                                        the grand mean. I provide an example of this in Figure 3 below. For the SSB, we calculate our overall grand mean (for all observations) and our group means (k). 
                                        Then, we subtract the group mean and the grand mean, square those values, and then multiply (i.e., scale) that by the repective group size N (j).",
                                        style = "font-family: 'times'; font-size:16px"),
                                        uiOutput(outputId = "aov_SSB"), # subtract individual grp means from grand mean, sqrd diff by grp N
                                        p("Next we calculate the Sum of Squares Within (SSW), or the sum of squares error (SEE). This is the squared differences of scores from within the group/factor
                                        (k). Like you would do for a normal mean to calculate the squared deviations, we do this here for each group, independently. This is our within-group variation. 
                                        So, like I show in Figure 4, we calculate the the mean for each group, then within each group we subtract each observation in that group with it's respective group mean 
                                        and get the squared deviation within that group. Next, we add these values across our groups. You'll notice, this is similar to the SST formula, 
                                        with the exception that here we are subtracting the observed values from the group mean (k) instead of the grand mean.",
                                          style = "font-family: 'times'; font-size:16px"),
                                        uiOutput(outputId = "aov_SSW"), # sqrd diff of scores from mean score within group (i.e., SSE)
                                        p("Once we have the SST, SSB and SST, we have the necessary information to proceed to calculate an F-statistic. 
                                        The F-statistic, similar to the T-statistic, is what we use to define the null [normal] distribution for our sample. For ANOVA, we use the F-test
                                        to compare whether the mean and standard deviation of our groups is equal to the normal distribution for the population. Before we get to getting our 
                                        F-statistic, we need to complete two steps",
                                          style = "font-family: 'times'; font-size:16px"),
                                        p("First, we need to calculate the numerator of the F-statistic, which is the Mean Squared Error Between (MSB). The SSB is the difference between 
                                        our groups divide by the number of groups (k-1). So as you can see in the formula below, the size of MSB increases with a greater SSB value and lower 
                                        number of groups (k).",
                                          style = "font-family: 'times'; font-size:16px"),
                                        uiOutput(outputId = "aov_MSB"),
                                        p("Second, we calculate the denominator of the F-statistics, which is the Mean Squared Error Within (MSW). The SSW is the variability within groups 
                                        divided by the total sample (N) minus the number of groups (k). Thus, when the difference between sample size and number of groups is larger, 
                                        MSW is smaller.",
                                          style = "font-family: 'times'; font-size:16px"),
                                        uiOutput(outputId = "aov_MSW"),
                                        p("Now that we have calculated the MSB and MSW, we have the necessary information to plug-in to the formula and calculate the F-statistic. 
                                        The F-stat quantifies the differences between population means (Null = no difference between group means). 
                                        As such, the null hypothesis is that the between group difference (i.e. MSB) and the within group 
                                        difference (i.e. MSW) should be relatively similar. However, if the means differ in a meaningful way, it is expected that the between group variability (i.e. MSB) is larger 
                                        than the within group variability (MSW). The F-distribution then is the ratio of MSB/MSW with the degrees of freedom, k - 1 and N - k. This will hopefully 
                                        become a bit more apparent in the example below.",
                                          style = "font-family: 'times'; font-size:16px;"),
                                        uiOutput(outputId = "aov_F_form"),
                                        br(),
                                        br(),
                                        h3("One-way ANOVA"),
                                        p("Let's use the formula and information from above to provide an explicit example for a One-way ANOVA. If you're using the default values and
                                        pushed 'Run One-way ANOVA!' you should see the tables and figures below. Remember, in the case of a One-way ANOVA, we are testing whether there is a 
                                          meaningful differences on a continuous outcome (Social media engagement) across three or more categories (our factor, image type).
                                          So the null hypothesis (H0) is that there is no meaningful differences in social media engagement across image types, and the alternative (H1) hypothesis
                                          is that there is a meaningful difference in social media engagement across image types",
                                          style = "font-family: 'times'; font-size:16px;"), 
                                        img(src="OneWay_ANOVA_H0H1.jpg", align = "center", height = '85%', width = '100%'),
                                        h3("One-way ANOVA: Example"),
                                        p("First, let's consider our sample and distribution of data first. 
                                        Using the example from the start, we'll consider the engagement for different image types. Say we ran 50 ads for each group, respectively. The average
                                        (M/SD) for engagement for City, Nature and People images are reported in Table 1 below. In addition, I include the overall N, mean, and SD in Table 2. 
                                        I know the engagement is unrealistically low, but imagine that this is MySpace and my ad skills are poor. Now it may seem more realistic :) ",
                                          style = "font-family: 'times'; font-size:16px;"),
                                        tableOutput(outputId = "aov_sim_one"),
                                        tableOutput(outputId = "aov_sim_one_overal"),
                                        p("Okay, so we now have our group sizes, group means, and group SDs. We can visualize this data using a boxplot to see how our means and the range of 
                                        observations for each group. This is plotted for your reference in Figure 1. As you will see, as the Table 1 indicates, that data for People and Nature 
                                        are somewhat similar, in that the distribution has some overlap. However, we see that for City images the mean is nearly 2x higher but the SD is 4x higher.
                                        If we did simple mean calculations and used boxplots, we may make a visual conclusion based on it but we can use the One-way ANOVA to confirm the statistical 
                                          conclusions about this difference.",
                                          style = "font-family: 'times'; font-size:16px;"),
                                        plotOutput(outputId = "aov_one_plot"),
                                        #p("We wont get into here, but it's worthwhile to remember that the ANOVA can be written as a formula linear formula, which we will 
                                        #discuss more in the 'Regression' tab",
                                        #  style = "font-family: 'times'; font-size:16px;"),
                                        #uiOutput(outputId = "aov_one_formula"),
                                        p("Now, let's run the one-way ANOVA model and evaluate whether the mean differences are significant. 
                                        Table 3 is the reported output from the aov() function in R. It provides us with the Terms: Image_Type (our factor/group variable) and residuals. 
                                        Based on the F-statistic (i.e., statistic) and the p-value, this model is signifiance (p < .00001 -- not, when values are lower < .001, the table shows p-vale '0'). 
                                        Instead of going over the Sum of Square and Mean Sum of 
                                        Squares now, I will break each of these down as we work through the figures 2-4 below. I will refer back to values from this table as we go along ",
                                          style = "font-family: 'times'; font-size:16px;"),
                                        tableOutput(outputId = "aov_out_one"),
                                        p("When we discussed the One-way ANOVA formula, the Sum of Squares Total, or SST, was brought up. While we do not see this in the
                                        aov() output above, we can get it by adding the Sum of Squares values for Image Type and Residuals values in Table 3. As described for SST, 
                                        this value is calculated by squaring the difference between each observation in Figure 1 (total obs = 150) and subtracting it from the overall (grand) mean (red line).
                                        Accounting for rounding error, the sum of squares for image type and residuals should sum up to the SST reported in Figure 2. 
                                        For our example, the SST is simply the squared difference between engagements that we observed across ALL of our ad types (i.e. person, nature, city)
                                        and the average engagement across these obsevations.
                                        You can do this manually, but it would take some time, so it's easier to take some shortcuts using the power of technology.",
                                          style = "font-family: 'times'; font-size:16px;"),
                                        plotOutput(outputId = "aov_SST_plot"),
                                        p("We got a visual of how SST is calculated for this simulated data, now let's consider the SSB, or the deviations between our groups. 
                                        If we refer back to the formula for the SSB, the SSB is the factor (or group) effect. In Figure 3, we can see the group means for City, 
                                        Nature and People image types in the boxplots (thicker center line of boxplot is the mean) for each group, and the overall mean across all of the observations (red line).
                                        To get the SSB, for *each* group we subtract the group mean (Table 1) from the overall mean (Table 2), square the values and multiply it by the group size, then add
                                        the result for each group together. For our example, if we go to Table 1 with the means and N for City, Nature,  People and Table 2 for overall mean, we can manually 
                                        get the SSB. Note: ^2 means 'squared'. So, we would do N([City mean]-[overall mean])^2 & Nature N([Nature mean]-[overall mean])^2 
                                        & N([People mean]-[overall mean])^2. Once we have these three values for each image type, we would add them up. This would match the Sum Sq. 
                                        Between value reported in Figure 3 and the value in the aov() reported table above for the row 'Image_Type' and column 'Sum of Squares'. 
                                        In this aov() table we also see the degrees of freedom (DF) reported, which is k-1,  or three image types minus one.",
                                          style = "font-family: 'times'; font-size:16px;"),
                                        plotOutput(outputId = "aov_SSB_plot"),
                                        p("Okay, great. So we now have calculated the SSB with the DF, which will help us in calculating the numerator for the F-statistic, or the  MSB 
                                        (i.e., Mean Squared Error Between). But we still need the denominator, or the MSW (i.e., mean squared error within groups) for the F-statistic. The MSW 
                                        is calculated using the SSW, or sum of squares within. Recall the formula from above and consider Figure 4. We have three group means (our factor), 
                                        and each group has it's own mean, which is the average number of engagements for our ads for the group, respectively. This average is calculated based on the 50 observations
                                        in each group. It is likely that there may be within group variability in how the ad performed for the image type. This variability may 
                                        not be related to our groups but perhaps other factors. So to calculate the SSW, we would calculate the squared deviation by subtracting each observation in the group (i.e., ad), 
                                        or number of engagements, from the group mean, or average number of engagement. Once we have this, we can add it up and get the 
                                        within group (or factor) variation.  This should match to the 'residuals' row and 'Sum of Squares' column from Table 3. Now we have the SSW, but for the 
                                        MSW we also need the DF. If you recall, this is the total sample size (overall N) minus the number of groups (k), in our example then the DF would be 150-3 -- 
                                        which matches the DF in Table 3.", 
                                          style = "font-family: 'times'; font-size:16px;"),
                                        plotOutput(outputId = "aov_SSW_plot"),
                                        p("Now we have all of the ingredients to calculate the MSB and MSW for our F-statistic formula. If you've been following along, you may already have this. 
                                        If you  need a boost, we can refer back to the aov() table, Table 3. You will mind a 'Mean Sum of Squares' column, this contains the information for the MSB 
                                        ('Image_Type' row) and the MSW ('Residuals' row). Since the information is all in the table you can calculate and confirm it yourself. For the numerator, 
                                        ['Image_Type' row & 'Sum of Squares' column] /  ['Image_Type' row & 'DF' column]. Then the denominator, 
                                        ['Residuals' row & 'Sum of Squares' column] /  ['Residuals' row & 'DF' column]. Each row that we're working on should match the value in 'meansq' 
                                        column for the respective role. We divide these, and we will get our F-statistic. Now we can infer whether there is a meaningful differences for amongst our 
                                        factor type (images) in the data we had acquired.",
                                          style = "font-family: 'times'; font-size:16px;"),
                                        br(),
                                        br(),
                                        img(src="coolstory_bro.jpg", align = "left", height = '35%', width = '35%'),
                                        p("But you may be saying, 'Cool story, bro. I still want to know how how these image types meaningful differ between each other. My boss is all over me, 
                                        and I need to the order of performance across these three.' This is an appropriate question. Once we learn that our image types vary in the one-way 
                                        ANOVA, there may not be enough clear evidence from the Figures to see whether one image type, say people, is superior to another, say nature.
                                        So we want to get some  qunatitative measure of this. A way to do this is to contrast the different means as we had done before. Fortunately, something like the
                                        Tukey Test can calculate what is called the 'Tukey's honest Significance Difference test', which is simply the comparison of the group means for our factor. 
                                        The Table 4 reports all of the possible comparisons for our grouping variable. In the table we get each contrast type for our term, i.e. Nature versus City.
                                        We get the associated estimate, which is the direction of the effect, whether mean 1 or mean 2 is greater. For example for Nature-City, -6.0 would indicate the unit difference in means, or number of 
                                        engagements. Compare this back to the reported descriptives in Table 1. Around this unit change, we have the 95% confidence interval. Given the variability 
                                        in our sample, we get a distribution of values, and estimate where 95% of the values are in this distribution for the data if we reran this test 100x. This has the lower 
                                        and upper range. Which is directly associated with the p-value that is estimated from the F-distribution given the sample sample, mean, and SD (i.e., variability).",
                                          style = "font-family: 'times'; font-size:16px;"),
                                        tableOutput(outputId = "aov_out_one_tukey"),
                                        p("With the ANOVA, it is worth to remember that when we have a lot of groups we can end up making many comparsions which would increase the false positive rate.
                                        Therefore, it's worthwhile to consider whether you should perform some type of correction to the significance values (i.e., Bonferroni or Benjamini-Hochberg correction),
                                        or set up some contrasts initially that dont require a comparsion of all group means. But this depends on your question, goal, and concern about making the incorrect 
                                        conclusion (i.e., importance of controlling False positive or False Negative).",
                                          style = "font-family: 'times'; font-size:16px;"),
                                        ),
                               tabPanel(title = "Two-way ANOVA", value = "two_anova_panel",
                                        h3("Two-way ANOVA"), 
                                        p("Quick note: regarding what inputs impact which outputs. The Var 1 to Var 3 mean/SD values for One-way ANOVA, with Prefix 'Between(1) 
                                        will alter the means/SD for Males for City (Var1), Nature (Var2) and People (Var 3). Then, the second set of variables with Prefix
                                        'Between(2)' will alter the means/SD for Females City (Var1), Nature (Var2) and People (Var 3). Again, I suggest observing the defaults and
                                        then playing around with your own values next. If you have not yet, hit 'Run, Two-way ANOVA!' to simulate the tables and results below.",
                                          style = "font-family: 'times'; font-style: italic; font-size:12px;"),
                                        
                                        p("In the first tab we discussed the One-way ANOVA, which compared the difference on a continuous outcome, engagement on ads, across a categorical variables,
                                        our image types. However, we may be interested to add an additional categorical variable, sex. We may have the suspicion, based on available data, that females may engage differently
                                        than males across our three types of ads.",
                                          style = "font-family: 'times'; font-size:16px;"),
                                        img(src="ANOVA_sex.jpg", align = "center", height = '60%', width = '100%'),
                                        br(),
                                        br(),
                                        p("The Two-way ANOVA provides a statistical test to evaluate the difference among multiple group means for categorical 
                                        variables on our outcome (DV), social media engagement, such as image type and sex. 
                                        By knowing whether this difference exists and then the direction of this difference, we may acquire valuable 
                                        information to target our ads with a bit more precision. So, we're trying to estimate the difference between our difference of interest (SSB/MSB) for our groups relative to the deviations 
                                        within groups that we are not interested in (SSW/BSW). In this example, we can pose a comparable hypothesis as for the One-Way ANOVA, whereby
                                          the null hypothesis (H0) is that there is no significant difference on social media engagement across our groups (e.g., image type and sex) and the alternative (H1)
                                          that there is a significant difference on social media engagement across our groups (e.g., image type and sex). We're just controlling our false positive error rate (Type-I), but 
                                          evaluating this question in a single statistical test",
                                          style = "font-family: 'times'; font-size:16px;"),
                                        img(src="TwoWay_ANOVA_H0H1.jpg", align = "center", height = '85%', width = '100%'),
                                       #   p("As we saw for the one-way ANOVA, our Two-way ANOVA can be written as a linear model as well. I will not go into this here, but we will
                                       #   go over this linear formula more in the 'Regression' tab. I provide it here to simply remind you have aov() can also be thought of as
                                       #   a linear model.",
                                       #     style = "font-family: 'times'; font-size:16px;"),
                                       # uiOutput(outputId = "aov_two_formula"),
                                        p("Conceptually, elements don't substantially change from the One-way ANOVA, so we will not cover indepth how to compute each sum of squared residuals and mean sum of squared residuals as we
                                        did  for the One-way ANOVA. The conceptual is quite similar, we are trying to decompose our signal in the numerator
                                        that is related to our factors (or groups). Whereas before we had a single factor, image type, we now have two factors,
                                        image type (A) and sex (B). Then, we also have their interaction AB. So similar to the Figures 2 - 4, we can calculate these values
                                        the same way but now for an additional factor. To not over burden you with statistical formulas, I will not include them here,
                                        but if you are interested in the formula I defer you to the explanation from Stanford's ",
                                          a("Stats 191 course.",  href="https://web.stanford.edu/class/stats191/notebooks/ANOVA.html", target="_blank"),
                                          style = "font-family: 'times'; font-size:16px;"),
                                       h3("Two-way ANOVA: Example"),
                                        p("Similar to what we saw for the One-way ANOVA, we can take a look at our means and SDs for each image type and sex group combination. 
                                        Our image type has 3 levels (city, nature, and people) and sex has 2 levels (female, male), hence we have a 2x3 ANOVA with 6 means and SDs.", 
                                          style = "font-family: 'times'; font-size:16px;"),
                                        p("By glancing at Table 5, we can see that we continue to have differences across image types, but also some subtle differences between sex
                                        for city and people image types. In Table 6 we find the overall N, mean and SD across groups in Table 5.
                                        Now we have a numerical representation, we can visually observe these averages and variability within/between groups in Figure 5.",
                                          style = "font-family: 'times'; font-size:16px;"),
                                        tableOutput(outputId = "aov_sim_two"),
                                        tableOutput(outputId = "aov_sim_two_overal"),
                                        p("Now that we have taken a look at the numerical points of distribution in Table 5, 
                                        it is worthwhile to visually observe the averages and variability within/between groups in Figure 5. We can begin to see how we hace difference
                                        across multiple factors, which may suggest a group-by-group or factor-by-factor interaction. What this interaction suggests is that at difference
                                        levels of image types and sex we observe greater ad engagement. For instance, we can see that the people image tupe is greater than nature and city,
                                          but also that this average is greater for females than males. Furthermore, whereas for city images, the performance is greater for females than males, too.",
                                          style = "font-family: 'times'; font-size:16px;"),
                                        plotOutput(outputId = "aov_two_plot"),
                                        p("To get a statistical representation of this difference, especially in cases where it's difficult to draw these conclusions visually, we an run our 3x2 ANOVA
                                        using the aov() in R as we did before. As we saw in the One-way ANOVA results in Table 3, we see similar values for the Two-way ANOVA in Table 7. The only difference
                                        is we have two additional rows. These two rows are for our second factor (i.e., group), sex, and then the interaction between our two groups, Image Type and
                                        Sex. As before, we have the the SSB values (i.e. Term 'Image_Type', 'Sex' and Interaction Image_Type:Sex), and the SSW value (i.e. reisuals term). Then the association
                                        MSB and MSW across these values.",
                                          style = "font-family: 'times'; font-size:16px;"),
                                        tableOutput(outputId = "aov_out_two"),
                                        p("Given there multiple levels at which we're comparing means, we can compute the estimate between each Image Type and Sex combination to parse where
                                        the specific difference lie and to what magnitude. Note, there are A LOT (!) of combinations. You'll notice, the estimate here, as it was for the one-way
                                          ANOVA, is the mean difference between the two association contrasts. Compare the estimates in Table 8 and Table 5 to observe what these differences mean.",
                                          style = "font-family: 'times'; font-size:16px;"),
                                        tableOutput(outputId = "aov_out_two_tukey")
                                        ),
                               br(),
                               br(),
                               br(),
                               br(),
                               p("Packages used - Plotting: ggplot | Simulating data based on means & correlations: mvnorm() via MASS",
                                 style = "font-family: 'times'; font-size:10px"),
                               br(),
                               br(),
                               p("For Citation & Documentation purposes, code will will be available on",
                                 a("Github", href="https://github.com/", target="_blank"), "once an initial full version is complete.",
                                 style = "font-family: 'times'; font-size:10px"),
                               p("© 2022 Michael Demidenko. Please contact for more details",
                                 style = "font-family: 'times'; font-size:10px")
                               )
         )
         ),


#Correlation text ###############################   
                            tabPanel("Correlation",
                                             mainPanel(width = "100%", style='padding-left: 30px; padding-right: 30px',
                                                 h2("Correlations"),
                                                 p("This tab simulates data for [four] normally distributed variables to demonstrate the concept of Pearson's & Spearman's correlation. It also reviews the
                                                 the effect of sample size and group differences on the associated correlations.
                                                 As it was covered in the preliminary tab, a correlation measures the linear association between two variables. In other words, 
                                                 when one variable goes up or down, how does the other variable change? Correlations can be positive (as Var 1 increases Var 2 Increase), flat (no correlation),
                                                 or negative (i.e., as Var 1 increases Var 2 decreases).",
                                                   style = "font-family: 'times'; font-size:16px"),
                                                 img(src="Correlation_types_ex.jpg", align = "center", height = '60%', width = '90%'),
                                                 p("In the first tab, the Pearson's r is covered. Scatter plots are used to reflect the spread of the observed data points, the magnitude and the direction of the correlation 
                                                 between the pairs of variables. Then, in the next tab I briefly cover the Spearman's rho correlation. Finally, I review the influence of sample size 
                                                 on the significance value (p-value) for the associated correlation. In the same tab, I provide an example to illustrate why it's important to make sure we don't 
                                                 incorrectly group our variables as this may impact the correlation we find.",
                                                   style = "font-family: 'times'; font-size:16px"),
                                                 br(),
                                                 h3("Selecting Correlation/Means/SDs ",
                                                    style = "font-family: 'times'"),
                                                 p("Let's start off by selecting the sample size and correlations size between two sets of 
                                                   variables, Var1 + Var2 and Var3 + Var4 that will be relevant to the subtabs below. As before, use the defaults and come back to alter these ones you made it
                                                   through this section.",
                                                   style = "font-family: 'times'; font-size:16px"),
                                                 
                                                 fluidRow(
                                                   column(width = 4,
                                                          sliderInput("corr_sample", label = "Sample Size",
                                                                      min = 5, value = 50, max = 2000,step = 15)),
                                                   column(width = 4,
                                                          sliderInput("corr1",
                                                                      label = "Pearson's r between Var1 & Var2",
                                                                      min = -1, value = .20, max = 1, step = .005)),
                                                   column(width = 4,
                                                          sliderInput("corr2",
                                                                      label = "Pearson's r between Var3 & Var4",
                                                                      min = -1, value = -.42, max = 1, step = .005)),
                                                 ),
                                                 br(),
                                                 p("Here are the associated means and standard deviations (SD) for Var1, Var2, Var3 and Var4 that we use in the example here.",
                                                   style = "font-family: 'times'; font-size:16px"),
                                                 fluidRow(
                                                   column(width = 3,
                                                          sliderInput("corr_m1", label = "Var1 Mean",
                                                                      min = -20, value = 5, max = 20, step = .5)),
                                                   column(width = 3,
                                                          sliderInput("corr_sd1", label = "Var1 SD",
                                                                      min = .01, value = 1.2, max = 10, step = .1)),
                                                   column(width = 3,
                                                          sliderInput("corr_m2", label = "Var2 Mean",
                                                                      min = -20, value = -8, max = 20, step = .5 )),
                                                   
                                                   column(width = 3,
                                                          sliderInput("corr_sd2", label = "Var2 SD",
                                                                      min = .01, value = 2.2, max = 10, step = .1))
                                                 ),
                                                 fluidRow(
                                                   column(width = 3,
                                                          sliderInput("corr_m3", label = "Var3 Mean",
                                                                      min = -20, value = 3.5, max = 20, step = .5)),
                                                   column(width = 3,
                                                          sliderInput("corr_sd3", label = "Var3 SD",
                                                                      min = .01, value = .75, max = 10, step = .1)),
                                                   column(width = 3,
                                                          sliderInput("corr_m4", label = "Var4 Mean",
                                                                      min = -20, value = -11, max = 20, step = .5)),
                                                   
                                                   column(width = 3,
                                                          sliderInput("corr_sd4", label = "Var4 SD",
                                                                      min = .01, value = 3.3, max = 10, step = .1))
                                                 ),
                                                 br(),
                                                 p("Once you are ready, hit this button to simulate/resimulate the associated data and figures",
                                                   actionButton("run2", "Run Correlations!",
                                                                style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00; font-size:75%"),
                                                   style = "font-family: 'times'; font-size:16px"),
                                                 
                                                 #selectInput('group_by', label = 'Group By',
                                                 #            choices = c('NULL', 'Sex', 'Race'),
                                                 #            selected = NULL),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 tabsetPanel(id = "one_way_tabset",
                                                             tabPanel(title = "Pearson's r", value = "pearsons_tab",
                                                                      h3("Pearson's Formula"),
                                                                      p("In this section we cover two correlation pairs, Var 1 by Var 2 and Var 3 by Var 4. This, again, is simply the linear association
                                                                        between two continuous variable pairs. The association is linear because the unit change in one variable, say weight, is associated with a 
                                                                        proportional increase in another variable, say height.",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      br(),
                                                                      p("Let's first take a look at the formula used to calculate the Pearson's r correlation. The pearson correlation
                                                                      provides a coefficient for a linear association between two variables, which can be zero, positive, or negative.
                                                                      At first glance, the below formula may look a bit overwhelming. But we can simplify the equation into the numerator and 
                                                                      the denominator. The denominator calculates the overall covariation among our variables (such as, when Var1 goes up by 2, Var2 
                                                                      may go up 4). Let's first break down (x − x-bar)(y - y-bar) (bar = the overal mean for value). x 'i' and y 'i' are the observed values 
                                                                      for our variables. So if we had 10 observations of X and Y variables, we would have ten 'x i' and 'y i' values. Let's say in this case 
                                                                      X is the measure of how many miles that I ran during a span of 2 hrs for a given day and Y is a measure of how tired I reported I was after a run. 
                                                                      I did this 10 times across two weeks. Then, the 'm x' and 'm y' are the sample means for each of those values. So if I did 10 runs, 
                                                                      for X and Y I would get the mean across the 10 observations, respectively.",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      uiOutput(outputId = "corr_formula_p"),
                                                                      p("In the 'Pearsons_r' tab in the linked",
                                                                        a("google sheet",  href="https://docs.google.com/spreadsheets/d/1gT__bcJ2sjKlAS4-RqSTzBqPp8LmKlfRvCh9iBxwFSY/edit?usp=sharing", target="_blank"),
                                                                        ", I provide an example using some made up numbers for two variables, x and y. Across these observations, I calculate a mean for X and Y variables.
                                                                        Then, I calculate the sum of X and Y for the numerator, and then their product. I also calculate the denominator of X and Y form the formulate.
                                                                        I color code the numerator with dark blue/teal and the denominator with orange. Finally, the manual Pearson's r coefficient is calculated,
                                                                        and compared to what we may get if we simply used Excel's function 'Correl' to calculate r between X and Y. Hopefully, this breaks down the steps of the 
                                                                        formula with a by hand example. ",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      img(src="GoogleSheet_PearsonEx.jpg", align = "center", height = '80%', width = '100%'),
                                                                      p("Let's move back a real-world theoretical example. Because I like fried chicken and am inconsistent with my running, we would expect see variability in the distance and fatigue that I report.
                                                                      We can leverage this variability to determine the magnitude of the relationship between my running and self-reported fatigue. 
                                                                      If I always ran the same distance and never varied in my fatigue, there would be little variation. Given that we would see variation if we did measure these variables, we can estimate how the covary.
                                                                      To get the covariance of across values for Var1 (running) and Var2 (fatigue), we would take the ten sets of 
                                                                      (x - x mean)(y - y mean), such as (3 - 2)(9 - 8), (1.5 - 2)(5 - 8), 
                                                                      (3 - 2)(9 - 8), (3 - 2)(8.5 - 8), etc... and sum (∑ simply means summation over)  across those ten iterations. Now we have the numerator. 
                                                                      Normally, to get the sample covariance the numerator would be (n - 1), so for ten observations we would divide by 10-1, or 9. 
                                                                      HOWEVER, this is a sample constrained numerical value and we want something more standardized and interpretable across studies. 
                                                                      The Pearson's correlation achieves this standardized unit via the denominator. It standardizes the numerator by the samples standard deviations for the
                                                                        variabeles By doing so, we achieve values that will always be constrained to +/- 0.0 to 1.0. ",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      p("Now that we have collected the data, we can calculate the correlation between Var1 and Var2 by hand, or use Excel (as I showed in the linked example), R, SPSS or some other software
                                                                      to do the math for us. For simplicity, if you have pushed 'Onward' you will see scatter plots of the simulated data for the specified correlation.",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      plotOutput(outputId = "corr_plot1"),
                                                                      p("Figure 1 and Figure 2 provides the strength of your correlation represented by the linear line and the Pearson's r coefficient. Both represent the
                                                                      direction and magnitude for the associations. You can play around with sample size, means, and Pearson r coefficient sizes to see how things change.
                                                                      You'll notice the bar lines on the perimeter of the graph represent the distribution of the data points, so as data points cluster you'll see the bars
                                                                      get darker (which is just clustering of values.",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      p("Just like other statistical tests, people like to see whether a correlation, or association, among two variables is significant. So is there a
                                                                      significant association between the distance I ran and how tired I am? The table below reports whether the association between your variables is 
                                                                      or is not significant (note: if you see p.value = 0, that simply means that p < 0000000). The p-value is calculated using a t-distribution with 
                                                                      n – 2 degrees of freedom. The statistical test makes the assumption that the correlation between two variables in the population is zero. 
                                                                      So when p < .05 between Var1 and Var2, for example, we are saying the association among these variables in our sample is significantly greater than zero.",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      p("An important caveat with the correlation is while we do get an association between two variables, we cannot know whether one causes the other.
                                                                      In our example, if we find that the correlation between running distance and self-reported fatigue is p < .0001, one may make the claim
                                                                      that the distance caused the fatigue. While it is a safe bet in my case, as I am out of shape. There are caveats in something even as basic as this.
                                                                      Perhaps I have an injury that with longer distances is imacting my fatigue. Or, maybe it's that I'm not well hydrated or ate appropriately
                                                                      that is causing distant to make me more tired. So it's less the running, my more the issue of not having enough carbs or water.",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      br(),
                                                                      tableOutput(outputId = "corr_test"),
                                                             ),
                                                             
                                                             tabPanel(title = "Spearman's rho", value = "spearman_panel",
                                                                      h3("Spearman's rho Correlation",
                                                                         style = "font-family: 'times'"),
                                                                         p("Alternative to Pearson's r, one can use the spearman's formula which computes the
                                                                         correlation by the rank of x and y (fun fact, in the early 1900s, Pearson and Spearman were at odds on several topics). 
                                                                         Sphearman's formula is similar to Pearson's  with the exception of x = x', whereby x' is the rank(x). While Pearson's r determines the association
                                                                           between two continous variables, Spearman's rho detects the differences between the rank between two continous or ordinal measures. The association is not
                                                                           assumed to be linear, but instead is monotonic. Monotonic simply means that the unit change in one variable is associated with an increase/decrease
                                                                           of another variable in a rate that doesn't have to be constant.",
                                                                           style = "font-family: 'times'; font-size:16px"),
                                                                      br(),
                                                                      img(src="Spearmans_monotonic.jpg", align = "center", height = '80%', width = '100%'),
                                                                      br(),
                                                                      h3("Spearman's rho: Formula",
                                                                         style = "font-family: 'times'"),
                                                                      p("As mentioned above, the Spearman's rho coefficient is the associated rank between two variables. The formula is below.
                                                                        The key first step is identify d^2 for each individual observation (i). To do this, you take your two variables, X and Y,
                                                                        and rank the observations. Specifically, take your observations for X variable and in a new column note it's rank (e.g., 1st or 10th), x'. Then, you 
                                                                        do the same thing for your second variable, Y, and rank those values in a new column, y'. Once you have calculated columns 
                                                                        x' and y' (ranks) for each of the two observations, you would take the difference (d) between x' - y' and square this value. This
                                                                        gives you the d^2 value for each observations of X & Y. When you add all of these d^2 values together, you multiply it by 6 and you have the numerator
                                                                        of the formula.",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      uiOutput(outputId = "corr_formula_rho_2"),
                                                                      p("So you can follow along manually, in the 'Spearman_rho' tab I calculated the correlation of spearman ranks in the",
                                                                        a("google sheet.",  href="https://docs.google.com/spreadsheets/d/1gT__bcJ2sjKlAS4-RqSTzBqPp8LmKlfRvCh9iBxwFSY/edit?usp=sharing", target="_blank"),
                                                                        "I calculated the ranks using excel's 'RANK.AVG' function. After calculating sphearmans rho, I also calculate the
                                                                        Pearson's correlation of the data (note, the X & Y data is a n=10 random sample from the NAHNES dataset). You'll the spearman's rho and pearson's r are nearly identical. The differences are
                                                                        largely due to tied ranks that the spearman calculation adjusts for. In cases where there are a lot of ranks, it may not always be advised to use this coefficient,
                                                                        as it may impact the interpretation.",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      img(src="Spearman_rho_ex.jpg", align = "center", height = '70%', width = '100%'),
                                                                      p("Spearman's rho is not all too different from Pearson's r. With the exception of the ranks of the data, it derived from comparable calculations as is described",
                                                                        a("here.",  href="https://towardsdatascience.com/discover-the-strength-of-monotonic-relation-850d11f72046", target="_blank"),
                                                                        "Similar to the Pearson's formula presented on the previous tab, the formula can be thought of as covariance among the ranked X (x') and ranked Y (y') variables,
                                                                        divided by the standard deviations of the ranked X (x') and ranked Y (y') variables.",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      uiOutput(outputId = "corr_formula_rho_1"),
                                                                      
                                                                      br(),
                                                                      tableOutput(outputId = "corr_test3"),
                                                             ),
                                                             tabPanel(title = "Sig. & Groups", value = "anova_panel",
                                                                      h3("P-value and Groups in Correlations"),
                                                                      p("It is important to keep always take note of the correlation coefficient and not be misled by the underlying p-value.
                                                                      Funder & Ozer (2019), discussed the sense and nonsense of effect sizes.
                                                                      How plausible and meaningful of an  effect depends on the context. While we may get really large or really small
                                                                      effects in our individual samples, we have to reflect on these values to determine whether it is meaningful to what we are 
                                                                      trying to measure. For example, does it make sense that age and height correlate r = .65 in a sample of <25 yr olds, but a measure
                                                                      of some gene and some behavior correlate r = .45? Not necessarily -- height and age are a pretty large and observable effects in the
                                                                      real world. We can see the association. But genes and behaviors are extremely complex, so to see a large correlation should make us
                                                                      think 'how is this plausible?' and 'why did this occur in our sample?' In the tab on 'stability of effects', I try to
                                                                      provide an example how large effects may arise that are likely to be small in the real-world",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      p("One other thing to keep in mind with correlations is to pay attention to our sample, varibles and the correlation we find,
                                                                      rather than searching for the p-value. As you can see in Figure 3 below, a Pearson's correlation of r = .15 can be
                                                                      non-significant and ignored until a sample threshold is hit. The p-value can be misleading.",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      plotOutput(outputId = "corr_n_p"),
                                                                      p("Then, the types of people and/or groups that we include for an 'overall' correlation may
                                                                      skew our interpretation from the underlying group correlations, esp. when these groups differ
                                                                      on means. For example, say we have a sample of 150 who self-reported on two variables. The below table
                                                                      provides the means for each of these two variables. In the overall sample, the means are 6.3 for X1 and 4.8 for X2,
                                                                      and the correlation between these variables is r = - .13. So as X1 goes up, X1 goes down by some small amount. However,
                                                                      in this sample, we have three distinct groups. For simplicity, I made these groups equal in size. Group 1 and Group 2 have correlations
                                                                      that are positive, X1 mean is similar and X2 mean is different between the two groups. On the otherhand, group 2 has a negative correlation
                                                                      between X1 and X2, which is different from both group 1 and group 2. Then for group 2 the mean for X1 is higher than the other two, 
                                                                        but X2 is somewhere in the middle.",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      tableOutput(outputId = "corr_grp_tbl"),
                                                                      p("What we observe when we plot all of the data points (n = 150), is this relatively flat but negative association between X1 and X2.
                                                                      But when we parse this out by groups, we can see that for most of our participants there is a positive association, but because of the
                                                                      difference in the means between these groups, this masks their relative associations. Hence, when calculating correlations, it's good to plot the data
                                                                      and then consider what important information you are missing, e.g., treatment groups and sampling nuance.",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      plotOutput(outputId = "corr_grp_plt"),
                                                                      br(),
                                                                      h4("Cited Work"),
                                                                      p("Funder, D. C., & Ozer, D. J. (2019). Evaluating Effect Size in Psychological Research: Sense and Nonsense. Advances in Methods and Practices in Psychological Science, 2(2), 156–168. https://doi.org/10.1177/2515245919847202",
                                                                        style = "font-family: 'times'; font-size:10px"),
                                                             ),
                                                             br(),
                                                             br(),
                                                             br(),
                                                             br(),
                                                             p("Packages used - Plotting: ggplot | Simulating data based on means & correlations: mvnorm() via MASS",
                                                               style = "font-family: 'times'; font-size:10px"),
                                                             br(),
                                                             br(),
                                                             br(),
                                                             p("For Citation & Documentation purposes, code will will be available on",
                                                               a("Github", href="https://github.com/", target="_blank"), "once an initial full version is complete.",
                                                               style = "font-family: 'times'; font-size:10px"),
                                                             p("© 2021 Michael Demidenko. Please contact for more details",
                                                               style = "font-family: 'times'; font-size:10px")
                                                 )
                                             )
                            ),


#Stability Correlation  text ###############################

                            tabPanel("Stability of Effect",
                                             mainPanel(width = "100%", style='padding-left: 30px; padding-right: 30px',
                                                 h2("Stability of Correlation based on N"),
                                                 p("In the correlations tabe we cover the Pearson's and Spearson's correlations. We discussed what those terms mean, how we visualize them,
                                                 the influence of sample size and issues with groups. This tap extends on that tab by covering the variability in the magnitude of a correlation
                                                 betweeo two variables in a sample. While we may get a correlation value, that value may by off changes be larger or small than what we would get
                                                 if get collected data on more participants.",
                                                   style = "font-family: 'times'; font-size:16px"),
                                                 p("Here we use some values (start with defaults) to generate a normally distributed dataset for a specified Pearson's r correlation. 
                                                 Here the sample size is held constant, N = 1500. Using this simulated data and the specified correlation, we randomly sample a subsample from our 
                                                 1500 at intervals from 5 to 1500 and calculate the correlation between our variables. A plot is created to visually demonstrate 
                                                 how much a correlation can deviate from the a full sample correlation. Full disclosure, this tab was inspired by ",
                                                   a("Schönbrodt & Perugini (2013).", href="
                                                 http://www.sciencedirect.com/science/article/pii/S0092656613000858/", target="_blank"), 
                                                 " The formula here is based on the Pearson's r formula, but for comparison a Spearman rho 
                                                 is also provided. I also provide a real-world example using the publicly available National Health and Nutrition Examination Survey",
                                                 a("(NHANES) dataset", href="https://wwwn.cdc.gov/nchs/nhanes/", target="_blank"), 
                                                   "to bring the point across",
                                                   style = "font-family: 'times'; font-size:16px"),
                                                 h3("Select Parameters to Simulate Data at Specified Pearson's r",
                                                    style = "font-family: 'times'"),
                                                 p("Here we will select a number of parameters to simulate our Pearson's r correlations. First and foremost, we have the correlation
                                                   between Var1 and Var2. Start with the default and then alter it as you see fit. Then, you may specify the associated mean and standard deviation
                                                   (SD) for Var1 and Var2. While the latter two parameters are not crucial to this example, I've still included them here.",
                                                   style = "font-family: 'times'; font-size:16px"),
                                                 br(),
                                                 fluidRow(
                                                   column(width = 10,offset = 4,
                                                          sliderInput("corr3",
                                                                      label = "Pearson's r between Var1 & Var2",
                                                                      min = 0, value = .27, max = 1, step = .005))
                                                 ),
                                                 fluidRow(
                                                   column(width = 3,
                                                          sliderInput("stabcorr_m1", label = "Var1 Mean",
                                                                      min = 0, value = 5, max = 30, step = .5)),
                                                   column(width = 3,
                                                          sliderInput("stabcorr_sd1", label = "Var1 SD",
                                                                      min = .01, value = 1.2, max = 10, step = .1)),
                                                   column(width = 3,
                                                          sliderInput("stabcorr_m2", label = "Var2 Mean",
                                                                      min = 0, value = 9, max = 30, step = .5)),
                                                   column(width = 3,
                                                          sliderInput("stabcorr_sd2", label = "Var2 SD",
                                                                      min = .01, value = 2.2, max = 10, step = .1))
                                                 ),
                                                 p("Now that each value is selected, hit the run simulation button to simulate/resimulate the data and populate the fields
                                                   in the tabs below",
                                                   actionButton("run3", "Run Simulations!", 
                                                                style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00; font-size:75%"),
                                                   style = "font-family: 'times'; font-size:16px"),
                                                 p("Note: We are simulating data, subsampling data numerously from this simulation, subsampling from the NHANES data and running several functions that take time. 
                                                 So, if it's taking a few seconds for the figures to load, this is normal! It can take up to 10 seconds for the figures/tables to populate the first time.
                                                   However, when you come back and play around with the default values, things should run a tad bit faster.",
                                                   style = "font-family: 'times'; font-size:16px"),
                                                 br(),
                                                 br(),
                                                 tabsetPanel(id = "stable_corrs_sim",
                                                             tabPanel(title = "Sim. Stable Pearson's/Spearman's", value = "stability_corr",
                                                                      h3("Stability of Pearson's r & Spearman's rho",
                                                                         style = "font-family: 'times'"),
                                                                         p("In research, it's often difficult to get large enough samples (due to budget constraints) to estimate the 'true'
                                                                         population value. When we do collect our samples, ensuring a representative sample is also difficult... 
                                                                         among other issues. Consider reading more about the persistent
                                                                         biases of samples in Developmental Psychology by Nielsen et al (2017). 
                                                                         Because of this, small and/or not representative samples of the population may result in associations that are true 
                                                                         in to the sample (i.e data collected data), but perhaps deviate a bit (to quite a bit) from what may be observed for 
                                                                         the entire  population (or different samples). This can be demonstrated in the stability of a correlation.",
                                                                           style = "font-family: 'times'; font-size:16px"),
                                                                        p("In this tab, for the first example a dataset is generated for N = 1500 based on the correlation and means provided.
                                                                        From this sample, we can randomly select samples (i.e. subsample) for the correlation between two variables, Var1 and Var2.
                                                                        By doing this repeatedly from sample size 5 to 1500, and re-estimating the Pearson's correlation, we can see how much the 
                                                                        correlation deviates from what you pre-specified. Figure 1 captures this variability using the correlation that is specified. 
                                                                        In the figure you will likely observe the most variability variability in samples that contain less than 100 participants.
                                                                        However, as you increase the sample size, the correlation that is true (i.e., what you know is true, because we specified it!), 
                                                                        this value becomes apparent.",
                                                                          style = "font-family: 'times'; font-size:16px"),
                                                                        p("You might be thinking, 'well, in the Pearson's correlation tab you talked about Pearson's and Spearmans correlations, 
                                                                        does one correlation method provide better results than another?' That's a fair question. To provide this example here, 
                                                                        in Figure 2 I plot the stability using both Perason and Spearman correlation. Compare them for the correlation you specified.
                                                                        Unfortunately, what you will likely notice is a consistent pattern across both correlations methods. While Spearson uses ranking 
                                                                        to calculate the correlation between two variables, this doesn't necessarily improve the problem we observe here.",
                                                                          style = "font-family: 'times'; font-size:16px"),
                                                                      plotOutput(outputId = "EffNchange"),
                                                                      br(),
                                                                      p("One thing you may be thinking is we one sampled a 'single' value at each sample size. Surely this randomness 
                                                                          may influence what we see. This is a good point. Fortunately, since we're working with simulated data here, we can restimate the 
                                                                          correlation at a given sample multiple times get an average of the correlation for each sample size. In other words, Figure 1 and Figure 2 
                                                                          are based on individual correlations for each sample size.  We selected sample size of 5, 10, 15, 20, etc., only one time and calculated a 
                                                                          correlation. Instead, we can repeat this sampling 25 times for each sample size. So we can randomly select 25 different datasets that have a 
                                                                          sample size of 5, 10, 15, 20, etc. To do this, I simulate a sample size of 6,000 for a correlation  of r = .35. Then, for a sample 
                                                                          size of 5 to 1000, I repeatedly sample (25x) a random number of participants from the dataset with 6,000 participants and calculate  25 correlations 
                                                                          between Var1 and Var2 at that sample size. This way we can get an average of the 25 correlations for each sample size and the respective confidence interval 
                                                                          (or range of correlations). I plot thes results in Figure 3. Similar to Figure 1 and Figure 2, you will observe variability at smaller samples, 
                                                                          with a wider range of values, and more smoothing out with a larger sample size. Of note, the dashed line in Figure 3 is an average of 25 correlations 
                                                                          so it will be a tad more smooth than what appears in Figures 1 & 2. Since for this example we estimated 5,000 Pearson's r values 
                                                                          between Var1 and Var2, we can plot the density, or frequency, with which certain values occur. This frequence/density plot is provided in 
                                                                          Figure 4. You will see that the most frequent occurening r value is at or near r = .35 (what I simulated the data to have), however, around 
                                                                          this there is also a range of values. Some of these are near .35, others, substantially away from it. In the 'Stability in Real Data', we revisit this in
                                                                            a real dataset.",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                      plotOutput(outputId = "EffNchange2"),
                                                                      br(),
                                                                      br(),
                                                                      h4("Cited Work"),
                                                                      p("Nielsen, M., Haun, D., Kärtner, J., & Legare, C. H. (2017). The persistent sampling bias in developmental psychology: A call to action. Journal of Experimental Child Psychology, 162, 31–38. https://doi.org/10.1016/j.jecp.2017.04.017",
                                                                        style = "font-family: 'times'; font-size:10px")
                                                                      ),
                                                             tabPanel(title = "Stability in Real Data", value = "stability_corr_NHANES",
                                                                      h3("Stability of Pearson's r in [real] NHANES Dataset",
                                                                         style = "font-family: 'times'"),
                                                                         p("In the previous tab, you might have been thinking, 'Yeah, well, you know, that's just like, your opinion, man. 
                                                                              This is simulated data that is inherently not real.' To which I would reply, you are right. The Dude abides, 
                                                                              so let me provide a real world example. We can take the ublicly available NHANES dataset (https://wwwn.cdc.gov/nchs/nhanes/). 
                                                                              The data has over 9000 participants, so we can randomly select 6,000 to match our simulation. To compare an r value that 
                                                                              is closer to the .35 in Figure 3, I consider two variables:  average Systolic and average Diastolic blood pressure readings. 
                                                                              These values in the NHANES dataset represent Pearson's r = .39. So this is close enough to what we use for the Figure 3 simulation. 
                                                                              It's not an apples-to-apples comparison, but it's close enough.",
                                                                           style = "font-family: 'times'; font-size:16px"),
                                                                         p("As we did with a random simulated dataset with two variables with a correlation of r = 35 in Figure 3, we do the same thing for 
                                                                              the NHANES data. We repeatedly (25x) sample a subsample of participants, ranging from 5 to 1000, and calcualte a correlation between 
                                                                              the two blood pressure ratings, Systolic and Diastolic blood pressure. In Figure 4, as in Figure 3, I plot the average of these 
                                                                              correlations for each sample size and the range (95% confidence interval). What you will notice in the real world data is quite similar 
                                                                              to what we observed in the simulated data! The correlation, again, varies between r = .30 to r = .50 until roughly a sampe size of 
                                                                              ~200 when it begins to stabilize a bit Eventually, we see more stability. This is important, because you may have seen in the 
                                                                              Pearson's correlation tab, that what is and isn't significant may jump around. Furthermore, when these values become smaller and 
                                                                              more noisy, you will see correlations between two variables change from positive to negative. You can see this in the frequency 
                                                                              plot in Figure 4, most r values are positive, but a few were negative. This nuance is critical, because depending on our sample it 
                                                                              may directly impact our conclusions.",
                                                                           style = "font-family: 'times'; font-size:16px"),
                                                                      plotOutput(outputId = "EffNchange3"),
                                                                      p("Now, does this mean that what you found in your dataset for a small sample is incorrect? Well, no. You found an estimate that 
                                                                              is true for YOUR dataset/sample. Whether this value is true or not in subjects in the real world is... less clear. Issues of small 
                                                                              samples and eratic findings (or spurious effects, as it is commonly referred to) is not all that new in research. 
                                                                              As discussed in Button et al (2013), Ioannidis (2005); 
                                                                              Vul et al. (2009), abnormally large correlations in small sample sizes shouldn't be all that 
                                                                              surprising. Empirical research for a while has relied on p-values to quantify what is and isn't important. However, as shown in the T-test 
                                                                              and Pearson's correlation tab, p-values are susceptible to your sample size (or power) but also the size an effect size (or the magnitude 
                                                                              of an association). So, if you have a small sample size, say 60 participants, for a two-tailed significance test 
                                                                              and power of 80 (i.e., 80%), the correlation that will meet this p < .05 cut-off is r = .35. In other words, the only associations that 
                                                                              you will highlight as significant are those that met your p < .05 threshold, so they will have no choice but to be large.",
                                                                        style = "font-family: 'times'; font-size:16px"),
                                                                        p("As I hopefully have demonstrated, it doesn't mean you are unlikely to find significant correlations in small samples. 
                                                                                What you will find are the spurious correlations that deviate from the true correlation. Does this mean that for your sample 
                                                                                the correlation is wrong? Again, no. But if you would have collected more data, your correlation of r = .65 could very well 
                                                                                become r = .15, or worse, r = -.15. There is a lot of variability between samples, so what may be driving these 'spurious effects' 
                                                                                are issues of measuremen or other unique characteristics of your sample. This is why
                                                                                it is important to interpret large effects with some level of caution. Like noted in Funder & Ozer (2019),
                                                                                it's imperative that we consider what is a plausible and meaningful relationship (or effect size) in your respective field. If what you find
                                                                                something that deviates from what is expected, it's good to evaluate what contributed to this deviation in your data or the prior
                                                                                reported literature.",
                                                                          style = "font-family: 'times'; font-size:16px"),
                                                                          br(),
                                                                          br(),
                                                                          h4("Cited Work"),
                                                                          p("Button, K. S., Ioannidis, J. P. A., Mokrysz, C., Nosek, B. A., Flint, J., Robinson, E. S. J., & Munafò, M. R. (2013). Power failure: Why small sample size undermines the reliability of neuroscience. Nature Reviews Neuroscience, 14(5), 365–376. https://doi.org/10.1038/nrn3475",
                                                                            style = "font-family: 'times'; font-size:10px"),
                                                                          p("Funder, D. C., & Ozer, D. J. (2019). Evaluating Effect Size in Psychological Research: Sense and Nonsense. Advances in Methods and Practices in Psychological Science, 2(2), 156–168. https://doi.org/10.1177/2515245919847202",
                                                                            style = "font-family: 'times'; font-size:10px"),
                                                                          p(" Ioannidis, J. P. A. (2005). Why Most Published Research Findings Are False. PLOS Medicine, 2(8), e124. https://doi.org/10.1371/journal.pmed.0020124",
                                                                            style = "font-family: 'times'; font-size:10px"),
                                                                          p("Vul, E., Harris, C., Winkielman, P., & Pashler, H. (2009). Puzzlingly High Correlations in fMRI Studies of Emotion, Personality, and Social Cognition. Perspectives on Psychological Science: A Journal of the Association for Psychological Science, 4(3), 274–290. https://doi.org/10.1111/j.1745-6924.2009.01125.x",
                                                                            style = "font-family: 'times'; font-size:10px"),
                                                                      ),
                                                                      br(),
                                                                      
                                                                      br(),
                                                                      p("Packages used - Plotting: ggplot | Simulating data based on means & correlations: mvnorm() via MASS",
                                                                        style = "font-family: 'times'; font-size:10px"),
                                                                      br(),
                                                                      br(),
                                                                      br(),
                                                                      p("For Citation & Documentation purposes, code will will be available on",
                                                                        a("Github", href="https://github.com/", target="_blank"), "once an initial full version is complete.",
                                                                        style = "font-family: 'times'; font-size:10px"),
                                                                      p("© 2021 Michael Demidenko. Please contact for more details",
                                                                        style = "font-family: 'times'; font-size:10px")
                                                                      )
                                                             )
                                                 ),


#Regression text###############################


                                    tabPanel("Regression",
                                             mainPanel(width = "100%", style='padding-left: 30px; padding-right: 30px',
                                               h2("Regression"),
                                               p("The previous tabs cover mean differences in the context of T-tests and ANOVA and linear associations via 
                                               correlations. In some of those tabs I referred to 'linear' models/formulas, so in this tab we will cover Regression model which
                                               measures the association between continuous variables.
                                               First, we will go over linear regression, whereby we explain the association between a single IV and a DV variable. Then,
                                               we consider multiple regression, which is the association between more than one IV and a DV. The linear regression will provide conceptual
                                               information that we will build on with multiple regression. Given how linear regression coefficients are calculated, as discussed in
                                                 the",
                                                 a("S3E09 Quanatitude podcast.", href="https://www.buzzsprout.com/639103/9460323", target="_blank"),
                                               "The tab here simulates data
                                               using a normal distribution based on means and their correlations. So you can control the association between your IVs and DVs
                                               to determine how these associations impact your results. I also include an 'outlier' option which adds in up to three random values
                                               that are five SDs larger than than the mean for that data. For the linear regression I try to provide the context of how these
                                               models related to what we discussed before, correlations. Finally, I will include a section on logistic regression which measures
                                               the probability, or odds, of a binary outcome (0/1) occuring given some IVs.",
                                               style = "font-family: 'times'; font-size:16px"),
                                               p("As before, consider starting with the default values and then adjusting them once you've made it through this section to see how
                                                 estimates change depending on the values we entered.",
                                                           style = "font-family: 'times'; font-size:16px"),
                                               # Sample Linear Regression
                                               fluidRow(
                                                 column(width = 4,
                                                        sliderInput("m_reg_sample",
                                                                    label = "Sample (N) for Simulation",
                                                                    min = 2, max = 10000, value= 150)),
                                                 column(width = 4,
                                                        sliderInput("m_reg_corr1",
                                                                    label = "Pearson's r between DV & IV 1",
                                                                    min = 0, value = .22, max = 1, step = .005))
                                               ),
                                               fluidRow(
                                                 column(width = 3,
                                                        sliderInput("m_reg_m1", label = "DV Mean",
                                                                    min = -20, value = 5, max = 20, step = .5)),
                                                 column(width = 3,
                                                        sliderInput("m_reg_sd1", label = "DV SD",
                                                                    min = .01, value = 1.15, max = 10, step = .1)),
                                                 column(width = 3,
                                                        sliderInput("m_reg_m2", label = "IV 1 Mean",
                                                                    min = -20, value = 7.5, max = 20, step = .5 )),
                                                 column(width = 3,
                                                        sliderInput("m_reg_sd2", label = "IV 1 SD",
                                                                    min = .01, value = 2.2, max = 10, step = .1))
                                               ),
                                               
                                               ## Adding, IV for multiple regression
                                               fluidRow(
                                                 column(width = 3,
                                                        sliderInput("m_reg_m3", label = "IV 2 Mean",
                                                                    min = -20, value = 19.2, max = 20, step = .5)),
                                                 column(width = 3,
                                                        sliderInput("m_reg_sd3", label = "IV 2 SD",
                                                                    min = .01, value = 3.8, max = 10, step = .1)),
                                                 column(width = 3,
                                                        sliderInput("m_reg_corr2",
                                                                    label = "Pearson's r between DV & IV 2",
                                                                    min = 0, value = .45, max = 1, step = .005)),
                                                 column(width = 3,
                                                        sliderInput("m_reg_corr3",
                                                                    label = "Pearson's r between IV 1 & IV 2",
                                                                    min = 0, value = .41, max = 1, step = .005))
                                               ),
                                               #outliers?
                                               fluidRow(
                                                 column(width = 10,offset = 4,
                                                        selectInput("outlier", label = "Include Outliers on IVs?", 
                                                                    choices = c("None","One","Two","Three"),
                                                                    selected = "None"))
                                               ),
                                               
                                               p("Now that each value is selected, hit the run simulation button to simulate/resimulate the data and populate the fields
                                                   in the tabs below",
                                                 actionButton("run5", "Run Regression!", 
                                                              style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00; font-size:75%"),
                                                 style = "font-family: 'times'; font-size:16px"),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               tabsetPanel(id = "regression_tab",
                                                           tabPanel(title = "Linear Regression", value = "linear_reg_tab",
                                                                    h3("Linear Regression", style = "font-family: 'times'"),
                                                                    p("When we have two variables, we may be interested to estimate their relationship, or more explicitly, how one variable predicts another.
                                                                    This can, for instance, be drinking quantities and reward seeking behaviors. There is plenty of empirical
                                                                    evidence that reward seeking behaviors are associated with increased drinking quanities. As researchers, we may survey a 
                                                                    sample of high school and undergraduate students and estimate how well reward seeking predicts drinking. If we're only interested in the
                                                                    explicit association between reward seeking (IV) and the outcome (DV) we can run a linear regression on the observed data to quantify
                                                                      this relationship.", 
                                                                      style = "font-family: 'times'; font-size:16px"),
                                                                    h3("Linear Regression Formula", style = "font-family: 'times'"),
                                                                    p("A linear regression model is expressed in the equation below. Using our example, we can obtain the estimated value of drinking by identifying 
                                                                    the intercept (β0) where our line intersects the vertical axis and slope for our IV (β1), the change in drinking with change in reward seeking, 
                                                                    with some amount of error/variability (ϵ). While our observations are known, our intercept, slope and error are unknown.So we have to estimate these.
                                                                    Linear regression uses ordinary least squares (OLS) to estimate these 'unknowns'. In essence what this means is the OLS attempts to fit
                                                                    a linear line to our IV that explains the maximum amount of variation (sums of squares) in our DV. Commonly referred to as the 'line of best fit'. 
                                                                    This line reduces the deviations between the predicted values and observed observed values. The line of best fit is intended to performed better
                                                                    than something like just a mean. For instance, we can explain more deviations (i.e., sum of squared deviations) for drinking behaviors by fitting a line
                                                                    that uses a variable to predict this outcome. If this predictor helps us explain more of the deviations in drinking behaviors, we improve our prediction over the mean model.
                                                                      This will make a bit more sense when we go over the figures below.", 
                                                                      style = "font-family: 'times'; font-size:16px"),
                                                                    uiOutput(outputId = "m_reg_formula_1"),
                                                                    p("To better understand our regression coefficients, and see how they are related to correlations, we can break down the formula above a bit. I break down the 
                                                                    formula below and provide a by-hand example in the 'Regression_Coeff' tab linked in the",
                                                                      a("google sheet",  href="https://docs.google.com/spreadsheets/d/1gT__bcJ2sjKlAS4-RqSTzBqPp8LmKlfRvCh9iBxwFSY/edit?usp=sharing", target="_blank"),
                                                                      ", I use the same made up values for x and y, as in the 'Pearson_r' tab to make it easier to see the similarities. The tab includes the step
                                                                      in calculating the regression slope (β1), the intercept (β0), and the standardized β1.",
                                                                      style = "font-family: 'times'; font-size:16px"),
                                                                      img(src="Googlesheet_RegressionEx.jpg", align = "right", height = '40%', width = '50%'),
                                                                    p("Now, let's break down the formula presented here. To get  β1, or estimated β1 (b-hat), you will need to calculate the covariation between the independent variable (x)
                                                                    and the dependent variable (y). This value is then scaled by the variance of our independent variable (x). Once we have β1, we can calculate β0 fairly easily. 
                                                                    β0 is the mean of our dependent variable (y) minus the product of the weighted (by β1) mean of our independent variable (x). Once we have calculated β1 and β0,
                                                                      we have the regression coefficients for our model that respresents the data for variables x and y. I provide an example in the above linked google sheet.", 
                                                                      style = "font-family: 'times'; font-size:16px"),
                                                                    uiOutput(outputId = "m_reg_calc_b0"),
                                                                    h3("Linear Regression Simulated Example", style = "font-family: 'times'"),
                                                                    p("Now that we have discussed the formula, we can use the data we simulated earlier to provide a visual representation
                                                                    of what is happening and how we interpret the resulting coefficients. Below are the descriptives statistics for the data we simulated, such as the 
                                                                    sample size, mean and SD for the data used in the models below. Of note, in the values may not be exactly what you specified above. This is because we're 
                                                                    simulating data from those values. This shouldn't be a problem here. Another thing to remember is that when the outliers option is used, 
                                                                      the values in this table will shift a bit more.", 
                                                                      style = "font-family: 'times'; font-size:16px"),
                                                                    tableOutput(outputId = "m_reg_1_descr"),
                                                                    p("Now, we can plot the observed data for Drinking (DV) and Reward Seeking (IV). Then, we can fit a linear line to this data to get a visual representation
                                                                    of this association. If you had read other tabs to this point and have followed along here, you will see that this looks a lot like a correlation.
                                                                    Which is correct, because both are in the general linear model and represent relationships among two variables. I will expand on this a bit more 
                                                                    in the multiple regression section below, but broadly, in correlational/cross-sectional data the Pearson correlation and the linear regression are 
                                                                    very similar. If you observed the example in the google sheet under the 'Regression_Coef' tab, you may find that I used the shorthand formula for the 
                                                                    regression slope (β1). This shorthand is simply the Pearson's r correlation scaled by the standard deviation in the outcome (y) by the standard deviation in the 
                                                                    independent variable (x). 
                                                                    In linear regression the best fit model attempts to explain a greater amount of variability in the outcome (DV) than a simpler model which calculates only a mean.  
                                                                    So in Figure 1, the best fit line would be the green line and the mean is the redline. While you can't always see it with so many datapoints, 
                                                                      the green line explains more variability in Drinking than the redline.",
                                                                      style = "font-family: 'times'; font-size:16px"),
                                                                    plotOutput(outputId = "m_reg_plot_1"),
                                                                    p("If we were to compute a regression line for our data, predicting Drinking (DV) by Reward Seeking (IV), we would get several associated regression 
                                                                    coefficients. Table 2 reports these coefficients for the linear regression model. First, we get an intercept (β0), which tells us where the line 
                                                                    crosses the vertical axis on the graph. Notice, the β0 coefficient, or our intercept, does not map onto a mean from Table 1. This is because, as the 
                                                                    formula demonstrated above, β0 is the when our IV, reward seeking, is zero.  So β0 is in the abscence of β1. So you can interpret the intercept as 
                                                                    'when Reward seeking is zero, the average drinking quantity is...'. Then we get our  β1, or the estimated coefficient/slope for our predictor, 
                                                                    Reward seeking. This is often interpreted as the main effect of IV on the DV. The interpretation, in the unstandardized coefficient is one 
                                                                    unit increase in our IV (i.e, X), reward seeking, is associated with the slope coefficient increase
                                                                    in our DV (i.e., Y). The units of this scale will depend on your scales/measures. If we standardize this unit, we will get a coefficient that is 
                                                                    identical to the pearson's r. For this predictor we get an associated p-value which is based on our T-statistic. This indicates whether this 
                                                                     unit increase is statistically significant",
                                                                     style = "font-family: 'times'; font-size:16px"),
                                                                    tableOutput(outputId = "m_reg_1_coef"),
                                                                    p("In addition to our regression coefficients, what the linear regression model estimates are several overall model fit statistics. For instance,
                                                                    in Table 2 we get the associated coefficients and their significance values. However, this model doesn't tell us whether this is a meaningful improvement on
                                                                    a simpler model, which would be a mean based model for our DV. What I mean by this is, in Figure 1 we saw the green and red line, representing the 
                                                                    line of best fit and the mean, respectively. The model fit tells us whether this model explains more than just using the mean. 
                                                                    The F-statistic and associated degreees of freedom, DF1 and DF2, allow us to see whether this is a significant improvement. 
                                                                    DF1 is calculated using the number of IVs - 1 (or p - 1), and DF2 is calculated using the sample N - 2. The model also estimates an R-squared value, 
                                                                    or what is also referred to as the 'Coefficicent of Determination' and an adjusted R-squared. The R-squared value tells us about the improvement of 
                                                                    the best fit line over a mean line, and the adjusted R-squared is a penalized version of this. I explain this in greater detail next.",
                                                                      style = "font-family: 'times'; font-size:16px"),
                                                                    tableOutput(outputId = "m_reg_1_model"),
                                                                    p("The R-squared is calculated using the below formula. It is the fraction of Sum of Squares Residuals (SSR) and the Sum of Squares Total (SST). 
                                                                    The value will be between 0 and 1. It is often interpreted as the percent of variance in our DV that can be explained by the regression line.
                                                                    The SST is the maximum amount of deviation that we have around our DV, drinking. This is the mean model. Like we went over in previous tabs,
                                                                    if we were to calculate the mean and subtract each observed value from the mean and square it, the sum of these values would give us the SST. 
                                                                    Now, the SSR (residuals for regression line) are the sum of squared difference between the line predicted value and the observed value. 
                                                                    If our regression line is REALLY good at predicting Drinking (DV), we will find the we get a small value for SSR. Hence, 
                                                                    SSR/SST = 33/320 = 1 - .10 = .90 R squared, or the percent of variance in the DV that the IVs explain. ",
                                                                      style = "font-family: 'times'; font-size:16px"),
                                                                    uiOutput(outputId = "r_sqrd"),
                                                                    p("Now, the more IVs you add you will inherently explain more variance. So the models with more predictors will always 'appear' better when we 
                                                                    look at R-squared. Most models prefer parsimony over complexity, and thus complex models that have more predictors are penalized. 
                                                                    This is done using the formula below. The key value that punishes the R-squared value are the number of predictors (p), or IVs. As the sample gets larger, 
                                                                    the severity of this punishment decreases, but in some cases may still be meaningful. So you are rewarded for sample size and parisomy, 
                                                                      so your model has more parameters to estimate the coefficient from.",
                                                                      style = "font-family: 'times'; font-size:16px"),
                                                                    uiOutput(outputId = "adjst_r_sqrd"),
                                                                    p("One unique thing about the R-squared value in a linear regression is that it will be related to the pearson's correlation.
                                                                    In other words, since we specified a correlation between our IV and DV to be r =.22, if we take the square root of the 
                                                                    r-squared value, this will match our r-value, which is reported in Table 4.",
                                                                      style = "font-family: 'times'; font-size:16px"),
                                                                    tableOutput(outputId = "m_reg_1_corr"),
                                                                    br(),
                                                                    p("If we wanted to, we could extract the the predicted values and the residuals and see how these relate to the 
                                                                    observed data (i.e., what we collected). Figure 2 represents the OLS predicted values by the reward observed values. 
                                                                    These overlap substantially because the reward values were used to predict the outcome. In Figure 3 we can observe the 
                                                                    residuals, or the deviation of the observed from the predicted values. The black line represents our prediction. 
                                                                    You will see that not all observations are along this line. The deviations are the black lines that connect the observed 
                                                                    point to the fitted line. If we wanted to calculate them manually and add them up, we would get our SSR. In Figure 4 we have 
                                                                    the distribution of residuals across our IV, reward seeking. These should not have a relationship and the residuals 
                                                                    should be evenly scattered across. If there was some relationship, there may be some distinct thing going on at different 
                                                                    levels of our predictor which may influence our estimates. Finally, Figure 5 are the associations between our residuals and 
                                                                    drinking. Given that the regression model explains a small amount of variance, the observed drinking value and the difference
                                                                    between the predicted and observed are highly related. On the otherhand, if the regression model explained a large mount of 
                                                                      variance, then the difference between predicted and observed values would not be as related.",
                                                                      style = "font-family: 'times'; font-size:16px"),
                                                                    plotOutput(outputId = "m_reg_plot_2"),
                                                                    br()
                                                           ),
                                                           tabPanel(title = "Multiple Regression", value = "multi_reg_tab",
                                                                             h3("Multiple Regression", style = "font-family: 'times'"),
                                                                             p("Up until now, we have focused on the linear relationship between a single predictor (IV) and outcome (DV) 
                                                                             using linear regression. However, what if we wanted to predict drinking in high school students and undergraduate 
                                                                             students but we knew other variables were at play? For instance, there is a literature on Reward Seeking being a
                                                                             predictor of Drinking, but there is also a literature on Age predicting Drinking. Moreover, this Age variable is 
                                                                             frequently reported to be interrelated to Reward Seeking, too. So we may want to predict Drinking using Reward 
                                                                             Seeking but accounting for other known variables. This is where Multiple Regression comes into play.",
                                                                               style = "font-family: 'times'; font-size:16px"),
                                                                             p("Multiple regression is an extension of linear regression. As is apparent in the formula below, multiple 
                                                                             regression is the linear regression formula with an added predictor. Whereas in linear regression we estimated 
                                                                             a line of best fit for only Reward Seeking (β1), in multiple regression we keep β1 but also include Age, or β2. 
                                                                             If we added another, we'd have a β3, β4, etc. The critical difference between linear and multiple regression is 
                                                                             that the outcome (DV) is predicted by a combination of the intercept (β0), beta coefficients (β2, β3, etc...) 
                                                                             and the error (ϵ) . However, the goal remains the same -- we want a linear line that uses the combination of IVs,
                                                                               Age and Reward Seeking, to maximally predict the outcome, Drinking.",
                                                                               style = "font-family: 'times'; font-size:16px"),
                                                                            uiOutput(outputId = "m_reg_formula_2"),
                                                                            p("Above, we discussed how linear regression is very much alike to correlation. Specifically, I provided an 
                                                                            example of how β1 is covariation that is scaled by the IV (x). Below is the reminder of the Pearson's correlation..",
                                                                              style = "font-family: 'times'; font-size:16px"),
                                                                            uiOutput(outputId = "mult_corr"),
                                                                            p("Now, consider the partial correlation of xy when control for z (expressed as yx.z below). The numerator adjusts 
                                                                            the yx correlation by accounting for relationship that z has with x and y. Then, the denominator keeps the 
                                                                            variable (z) constant for x and y. Multiple regression using a similar framework, it estimates the coefficient for 
                                                                            an indepdent variable while keeping all other values constant (or at zero). We will discuss this more below.",
                                                                              style = "font-family: 'times'; font-size:16px"),
                                                                            uiOutput(outputId = "mult_part_corr"),
                                                                            p("The descriptives are in the table below. Notice, all of the information is similar to what we had for 
                                                                            the linear regression, now we just added an IV, Age.",
                                                                              style = "font-family: 'times'; font-size:16px"),
                                                                            tableOutput(outputId = "m_reg_2_descr"),
                                                                            p("Now we can take a look at the regression coefficients for our model in Table 5 consisting of DV (Drinking), 
                                                                            IV1 (Reward) and IV2 (Age). The intercept reflects the same thing as in the linear regression, it is at the point 
                                                                            that the line crosses the vertical axis, which is when both Reward Seeking and Age are at zero, or help constant. 
                                                                            The interpretation of the regression coefficients (or estimates), is the same here as in the linear regression, 
                                                                            EXCEPT now we adjust for the other variable. We still interpret the coefficient as a one unit increase in the IV, say 
                                                                            Age, is related to the coefficient increase in the DV, drinking, when all else is held constant. In our case, all else
                                                                            is the other IV, or age. So when interpreting Reward Seeking, we say that a one unit increase on our scale 
                                                                            Reward Seeking is related to the [coefficient] increase in our on scale of Drinking when Age is at zero.",
                                                                              style = "font-family: 'times'; font-size:16px"),
                                                                            tableOutput(outputId = "m_reg_2_coef"),
                                                                            p("As before, we can request the model fit statistics, which tell us how much better we're doing than a mean 
                                                                            based model of Drinking. We get a significance value based on our F-statistic and degrees of freedomn, the 
                                                                            associated p-value (FYI - this will show zero when it exceed several zeros), and the R squared and adjusted R squared, 
                                                                            our variance explained. Using the coefficients we can gauge the magnitude of the relationship and the
                                                                            model fit information provides us with information on how meaningful our model is. Again, mean often depends on 
                                                                              context -- what you're studying.",
                                                                              style = "font-family: 'times'; font-size:16px"),
                                                                            tableOutput(outputId = "m_reg_2_model"),
                                                                            p("We can also calculate our standardized coefficients, which adjusts for our scale variability. Providing a 
                                                                            value that is more interpretable between studies. This standardized value with be comparable to what we'd estimate 
                                                                             using a partial correlation, which adjusts for a third variable.",
                                                                             style = "font-family: 'times'; font-size:16px"),
                                                                            tableOutput(outputId = "m_reg_2_std_beta"),
                                                                            p("I report the partial correlation among the variables below, which uses the ppcor package in R. You'll notice that 
                                                                            these values will approximate our standardized coefficients. Given that we're using a combination of 
                                                                            lm.beta() and pcor() functions here, the calculations will not be perfect. But it 
                                                                              nicely demonstrates how these two linear problems are related -- which is pretty cool!",
                                                                              style = "font-family: 'times'; font-size:16px"),
                                                                          tableOutput(outputId = "m_reg_2_semi_par"),
                                                                          p("To be continued...",
                                                                            style = "font-family: 'times'; font-size:16px"),
                                                                          plotOutput(outputId = "m_reg_plot_3")
                                                           ),
                                                           tabPanel(title = "Logistic Regression", value = "Logit_reg_tab",
                                                                    h3("Logistic Regression", style = "font-family: 'times'"),
                                                                    p("Coming soon.... soonish?",
                                                                      style = "font-family: 'times'; font-size:16px")
                                                           ),
                                                           br(),
                                                           br(),
                                                           p("Packages used - Plotting: ggplot | Linear/multiple regression via: lm() | simulating data: mvnorm() via MASS | 
                                                             Stitching ggplots via patchwork by Thomas Lin Pedersen ",
                                                             style = "font-family: 'times'; font-size:10px"),
                                                           br(),
                                                           br(),
                                                           br(),
                                                           p("For Citation & Documentation purposes, code will will be available on",
                                                             a("Github", href="https://github.com/", target="_blank"), "once an initial full version is complete.",
                                                             style = "font-family: 'times'; font-size:10px"),
                                                           p("© 2021 Michael Demidenko. Please contact for more details",
                                                             style = "font-family: 'times'; font-size:10px")
                                                         )
                                                     )
                                                 ),
#ConstructValid Text ###############################

tabPanel("Construct Validation",
         fluidPage(
             sidebarLayout(position = "left",
                           sidebarPanel(
                               h3("Select Parameters to Simulate Data at Specified Pearson's r"),
                               actionButton("runN", "Wut to run?!?", 
                                            style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00")
                           ),
                           mainPanel(style='padding-left: 30px; padding-right: 30px',
                               h2("Construct Validation: TBD"),
                               p("Considering how I would like to present this. The structure and function is TBD. In the meantime, here 
                                 are some quotes on constructs",
                                 style = "font-family: 'times'; font-size:16px"),
                               p("Cronbach & Meehl (1957) 'Validation takes place when research believes an instrument 
                                 reflects a particular construct to which a meaning is attached' (pg 290)",
                                 style = "font-family: 'times'; font-size:16px"),
                               p("Loevinger (1957) 'Traits exist in people; constructs 
                                 (here usually about traits) exist in the minds and magazines of psychologists.' pg 642",
                                 style = "font-family: 'times'; font-size:16px"),
                               p("Shadish, Cook & Campbell (2002), a)	Constructs are almost always 
                                 couched in terms that are more abstract than elements in the experiment ",
                                 style = "font-family: 'times'; font-size:16px"),
                               p("Flake & Fried (2020) Refer to measurement as, “any approach that researchers take to create a number 
                                 to represent a variable under study” pg (458)",
                                 style = "font-family: 'times'; font-size:16px"),
                               p("Proulx & Morey (2021) 'In some cases, psychologists may change theoretical labels to obscure the 
                                 origins in order to meet some level of novelty/ground breaking finding' (pg 673)",
                                 style = "font-family: 'times'; font-size:16px"),
                               br()
                           )
             )
         )
),
#CFA_PCA Text ###############################

tabPanel("CFA & PCA",
         fluidPage(
             sidebarLayout(position = "right",
                           sidebarPanel(
                               h3("Select Parameters to Simulate Data Factor Loads"),
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
                           mainPanel(style='padding-left: 30px; padding-right: 30px',
                               h2("CFA/PCA: Simulated Data via CFA Factor Structure"),
                               p("This tab will be developed soon... When complete, a dataset is generated via specified parameters: loadings, covariance, and variances.
                                 Then, using this data CFA and then PCA models are explained and presented using this simulated data.",
                                 style = "font-family: 'times'; font-size:16px"),
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
                                 style = "font-family: 'times'; font-size:16px"),
                               textAreaInput(inputId = "sim_fact_coeff", 
                                             label = "Input Factors, Items loadings and Correlations for Sim Data",
                                             value = "\n#Factor item loadings \nFA1 =~ .5*a1 + .5*a2 + .5*a3 \nFA2 =~ .5*b1 + .5*b2 + .5*b3\n#Factor correlation\nFA1 ~~ .5*FA2
                                             ", 
                                             width = '100%', rows = 8),
                               h3("Proposed CFA/SEM syntax for Population Data",
                                  style = "font-family: 'times'"),
                               textOutput(outputId = "sim_model"),
                               h3("Correlations Among Simulated Data",
                                  style = "font-family: 'times'"),
                               p("These are the correlations among the correlations in the simulated population papameters averaged
                                 *across* N number of simulations.",
                                 style = "font-family: 'times'; font-size:16px"),
                               tableOutput(outputId = "sem_corr"),
                               h3("Proposed CFA/SEM Sample Model",
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


#Creating Diff data, using Reactive ###################################     


  
  
#Preliminary and terminology examples ################################  
  
  ## Distributions
  output$dist_plot <- renderPlot({
    m <- input$Distribution_mat
    
    set.seed(1)
    Random_Normal = data.frame(Value = rnorm(n = as.numeric(m[1,1]), mean = as.numeric(m[1,2]), sd = as.numeric(m[1,3])), 
                               Distribution = "Normal Dist")
    T_Distribution = data.frame(Value = rt(n = as.numeric(m[2,1]), df = as.numeric(m[2,4])), 
                                Distribution = "T-Distribution")
    Chi_Square = data.frame(Value = rchisq(n = as.numeric(m[3,1]), df = as.numeric(m[3,4])), 
                            Distribution = "Chi-Square Dist")
    Random_Uniform = data.frame(Value = runif(n = as.numeric(m[4,1]), min =as.numeric(m[4,5]), max = as.numeric(m[4,6])), 
                                Distribution = "Random Uniform Dist")
    
    data_d = rbind(Random_Normal, T_Distribution, Chi_Square, Random_Uniform)
    data_d %>% 
      ggplot(aes(x = Value, colour = Distribution)) +
      geom_histogram(bins = 40, aes(fill = Distribution), colour = "black",) +
      facet_wrap(~Distribution, scale = "free") +
      ggtitle("Figure 3 - Distribution plot based on simulated data from table input") +
      theme_minimal()
    
  })
  
  # Mean versus covariation differences
  
  
  output$cov_mean_ex <- renderPlot({
    r_ex <- input$cov_examp
    
    set.seed(1)
    r_dat <- data.frame(  
      mvrnorm(n = 150, # input$samp_range[2]
              mu = c(as.numeric(r_ex[1,1]),as.numeric(r_ex[1,2])), # means input by user
              Sigma = matrix(c(1,as.numeric(r_ex[2,1]),as.numeric(r_ex[2,2]),1), # correlation specified by user
                             nrow = 2), 
              empirical = TRUE)
    )
    
    cohens_d <- function(mean_1, mean_2, sd_1, sd_2){
      pooled_sd<- sqrt((sd_1^2+sd_2^2)/2)
      d <- (mean_1 - mean_2)/pooled_sd
      print(paste("Cohens d = ",round(d)))
    }
    
    mean_d <- cohens_d(mean(r_dat$X1), mean(r_dat$X2), sd(r_dat$X1), sd(r_dat$X2))
    cor_r <- round(as.numeric(cor(r_dat$X1, r_dat$X2)),2)
    
    theme_set(theme_minimal())
    ex1 <- r_dat %>% 
      gather(key = "Variables", value = "Scores", X1:X2) %>% 
      ggplot(aes(x = Variables, y = Scores, colour = Variables)) +
      geom_boxplot(show.legend = FALSE) +
      geom_jitter() + 
      ggtitle("Figure 5 - Plotting Means (Top) and Correlation (Bottom)")+
      annotate("label", x = "Effect", y = mean(r_dat$X1), 
               label = paste0(mean_d))+
      theme(legend.position = "none")
    
    ex2 <- r_dat %>% 
      ggplot(aes(x = X1, X2)) +
      geom_point()+
      geom_smooth(method = "lm", formula = 'y ~ x', se = F, colour = "red") +
      annotate("label", x = max(r_dat$X1)*.9, y = max(r_dat$X2)*.9, 
               label = paste0("Pearson's r: ",cor_r))+
      xlab("X1") +
      ylab("X2")
    
    
    
    ex1 / ex2 + plot_layout(guides = 'collect')
    
    
    })


#T-test code ###################################    

  
  ### simulation data based on user providing 2 means & 2 sds
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
  

  ### Mean Histogram plots
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
      labs(title = "Fig 4. Standard deviation scaled by Sample Size for Variable 1", 
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

  
  
# ANOVA Simulate ###################################  

  
  # Simulation sample for ANOVAs
  
  aov_one <- eventReactive(input$run4, {
    set.seed(1)
    
    dat_one <- ANOVA_design(
      design = "3b", 
      n = c(input$aov_sample),
      mu = c(input$aov1_m, input$aov2_m,input$aov3_m), 
      sd = c(input$aov1_sd,input$aov2_sd,input$aov3_sd),
      labelnames = c("Image_Type", "Nature", "City", "People"),
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
      mu = c(input$aov1_m, input$aov4_m,
             input$aov2_m, input$aov5_m,
             input$aov3_m, input$aov6_m), 
      sd = c(input$aov1_sd,input$aov4_sd,
             input$aov2_sd,input$aov5_sd,
             input$aov3_sd,input$aov6_sd),
      labelnames = c("Image_Type", "Nature", "City", "People",
                     "Sex", "Male", "Female"),
      plot = FALSE,
      r = .05
    )
    
    data.frame(dat_two$dataframe)
  }
  )
  
  # ANOVA formula ################################### 
  
  output$aov_one_formula <- renderUI({
    fit<-aov(y ~ Image_Type, data = aov_one()) 
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
  
  # ANOVA desr/stat ################################### 
  output$aov_sim_one <- function() {
    aov_one() %>% 
      group_by(Image_Type) %>% 
      dplyr::summarise("Group N" = n(),
                       "Group Mean" = mean(y),
                       "Group SD" = sd(y)) %>% 
      kable(caption = "Table 1. By Factor (i.e. group) Sample Descriptives") %>% 
      kable_styling("striped",
                    full_width = F, font_size = 12, html_font = 'Times')
  }
  output$aov_sim_one_overal <- function() {
    aov_one() %>% 
      dplyr::summarise("Overall N" = n(),
                       "Overall Mean" = mean(y),
                       "Overall SD" = sd(y)) %>% 
      kable(caption = "Table 2. Overall Sample Descriptives") %>% 
      kable_styling("striped",
                    full_width = F, font_size = 12, html_font = 'Times')
  }
  
  output$aov_out_one <- function(){
    set.seed(1)
    anova <- aov(y ~ Image_Type, data = aov_one())
    
    broom::tidy(anova) %>% 
      kable(caption = "Table 3. One-way ANOVA Results", 
            col.names = c("Term", "DF", "Sum of Squares", "Mean Sum of Squares",
                          "F-statistics", "p-Value")) %>% 
      kable_styling("striped",
                    full_width = F, font_size = 12, html_font = 'Times')
  }
  
  output$aov_out_one_tukey <- function(){
    set.seed(1)
    anova <- aov(y ~ Image_Type, data = aov_one())
    
    TukeyHSD(anova, which = c("Image_Type")) %>% 
      broom::tidy() %>% 
      kable(caption = "Table 4. Tukey's HSD contrast results for Two-way ANOVA", 
            col.names = c("Term", "Contrast", "Null Val", "Estimate",
                          "Lower 95% CI", "Upper 95% CI", "Adjusted p-value")
      ) %>% 
      kable_styling("striped",
                    full_width = F, 
                    font_size = 12, 
                    html_font = 'Times')
  }
  
  
  
  output$aov_one_plot <- renderPlot({
    aov_one() %>% 
      ggplot(aes(x = y, y = Image_Type, colour = Image_Type)) +
      geom_boxplot()+
      labs(title = "Fig 1. Boxplot for groups means")+
      theme_minimal() +
      theme(axis.text.y=element_blank())
  })
  
  
  output$aov_SST_plot <- renderPlot({
    aov_one <- aov_one()
    
    ## Plot SST
    SST<- round(sum((aov_one$y - mean(aov_one$y))^2),2)
    aov_one %>%
      ggplot(aes(x = "", y = y, colour = Image_Type)) +
      geom_point(aes(x = "", y = y), position = "jitter",
                 size =5/input$aov_sample) +
      annotate("label", x = "", y = mean(aov_one$y)*2, 
               label = paste0("Sum Sq. Total: ",SST))+
      geom_hline(yintercept = mean(aov_one$y), linetype = "solid",
                 color = "red", size = .5)+
      labs(title = "Fig 2. Sum of Squares Total  (Residuals)",
           caption = "Red Line = Mean of Outcome")+
      theme_minimal()+
      theme(axis.text.y=element_blank())
    
  })
  
  
  output$aov_SSB_plot <- renderPlot({
    aov_one <- aov_one()
    
    ## Plot SSB
    SSB<- aov_one %>% 
      dplyr::select(y, Image_Type) %>% 
      mutate(grand_mean = mean(y)) %>% 
      group_by(Image_Type) %>% 
      dplyr::summarize(n = n(),
                       factor_mean = mean(y),
                       grand_mean = mean(grand_mean)) %>% 
      mutate(SS_between = round(n * (factor_mean - grand_mean)^2),2) %>% 
      pull(SS_between) %>% 
      sum()
    
    
    aov_one %>%
      ggplot(aes(x = Image_Type, y = y)) +
      geom_boxplot(aes(y = y, group = Image_Type, colour = Image_Type))+
      geom_point(aes(x = Image_Type, y = y, colour = Image_Type), size =5/input$aov_sample) +
      annotate("label", x = "SSB", 
               y = mean(aov_one$y)*1.2, 
               label = paste0("Sum Sq. Between: ",SSB))+
      geom_hline(yintercept = mean(aov_one$y), linetype = "solid",
                 color = "red", size = .5)+
      labs(title = "Fig 3. Sum of Squares Between (Factor)",
           caption = "Red Line = Mean of Outcome")+
      theme_minimal() + 
      theme(axis.text.x=element_blank())
    
  })
  
  output$aov_SSW_plot <- renderPlot({
    aov_one <- aov_one()
    
    ## Plot SST
    SSW <-
      aov_one %>% 
      group_by(Image_Type) %>% 
      dplyr::mutate(grp_mean = mean(y)) %>% 
      dplyr::mutate(SSW = (y - grp_mean)^2) %>%
      group_by() %>% 
      dplyr::summarise(SSW = sum(SSW)) %>% 
      round(1)
    
    
    aov_one %>% 
      ggplot(aes(x = Image_Type, y = y, colour = Image_Type)) +
      geom_boxplot(aes(x = Image_Type, y = y))+
      geom_point(aes(x =  Image_Type, y = y), position = "jitter", size =1) +
      annotate("label", x = "SSW", y = mean(aov_one$y),
               label = paste0("Sum Sq. Within: ",SSW))+
      labs(title = "Fig 4. Sum of Squares Within (Error)")+
      theme_minimal() +
      theme(axis.text.x=element_blank())
    
    
  }
  )
  
  
  output$aov_two_formula <- renderUI({
    fit<-aov(y ~ Image_Type + Sex, data = aov_two()) 
    eq <- paste0(extract_eq(fit))
    withMathJax(
      print(paste0("$$",eq,"$$"))
    )
  })
  
  output$aov_sim_two <- function() {
    aov_two() %>% 
      group_by(Image_Type, Sex) %>% 
      dplyr::summarise("Group N" = n(),
                       "Group Mean" = mean(y),
                       "Group SD" = sd(y)) %>% 
      kable(caption = "Table 5. By Factor Descriptives Two-way ANOVA") %>% 
      kable_styling("striped",
                    full_width = F, font_size = 12, html_font = 'Times')
  }
  
  output$aov_sim_two_overal <- function() {
    aov_two() %>% 
      dplyr::summarise("Overall N" = n(),
                       "Overall Mean" = mean(y),
                       "Overall SD" = sd(y)) %>% 
      kable(caption = "Table 6. Overall Descriptives Two-way ANOVA") %>% 
      kable_styling("striped",
                    full_width = F, font_size = 12, html_font = 'Times')
    
  }
  
  output$aov_out_two <- function(){
    anova <- aov(y ~ Image_Type + Sex + Image_Type:Sex, data = aov_two())
    
    broom::tidy(anova) %>% 
      kable(caption = "Table 7. Two-way ANOVA Results", 
            col.names = c("Term", "DF", "Sum of Squares", "Mean Sum of Squares",
                          "F-statistics", "p-value")) %>% 
      kable_styling("striped",
                    full_width = F, font_size = 12, html_font = 'Times')
  }
  
  output$aov_two_plot <- renderPlot({
    
    aov_two() %>% 
      ggplot(aes(x = y, y = Image_Type, colour = Sex)) +
      geom_boxplot()+
      labs(title = "Fig 5. Plot of Means Across 3x2 factors, Image Type and Sex")+
      scale_y_discrete(labels=c("People","Nature","City"))+
      theme_minimal() +
      theme(axis.text.x=element_blank(),
            axis.text=element_text(size = 10))
  })
  
  output$aov_out_two_tukey <- function(){
    set.seed(1)
    anova <- aov(y ~ Image_Type + Sex + Image_Type:Sex, data = aov_two())
    
    TukeyHSD(anova, which = c("Image_Type","Sex", "Image_Type:Sex")) %>% 
      broom::tidy() %>% 
      kable(caption = "Table 8. Tukey's HSD contrast results for Two-way ANOVA", 
            col.names = c("Term", "Contrast", "Null Vall", "Estimate",
                          "Lower 95% CI", "Upper 95% CI", "Adjusted p-value")) %>% 
      kable_styling("striped",
                    full_width = F, 
                    font_size = 12, 
                    html_font = 'Times')
  }
  
  
# Correlation Simulate ###################################    
   

    ### Simulation sample with TWO variables reflective pre specified correlation
    sim_corr <- eventReactive(input$run2, {
        set.seed(123)
        n = input$corr_sample; 
        m1 = input$corr_m1; m2 = input$corr_m2; mu <- c(m1, m2)
        sd1 = input$corr_sd1; sd2 = input$corr_sd2; stddev <- c(sd1, sd2)
        r1 = input$corr1
        
        corMat <- matrix(c(1, r1, 
                           r1,1),
                         ncol = 2)
        covMat <- stddev %*% t(stddev) * corMat
      
        df <- as.data.frame(
          mvrnorm(n = n, 
                  mu = mu, 
                  Sigma = covMat, 
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
        
        set.seed(123)
        n = input$corr_sample; 
        m3 = input$corr_m3; m4 = input$corr_m4; mu <- c(m3, m4)
        sd3 = input$corr_sd3; sd4 = input$corr_sd4; stddev <- c(sd3, sd4)
        r2 = input$corr2
        
        corMat <- matrix(c(1, r2, 
                           r2,1),
                         ncol = 2)
        covMat <- stddev %*% t(stddev) * corMat
        
        df <- as.data.frame(
          mvrnorm(n = n, 
                  mu = mu, 
                  Sigma = covMat, 
                  empirical = TRUE))
    })

    
    corr_grp_dat <- eventReactive(input$run2, {
        
        set.seed(123)
        n1 = 50; 
        m1 = 5; m2 = 7; mu1 <- c(m1, m2)
        sd1 = 2.2; sd2 = 2.5; stddev1 <- c(sd1, sd2)
        r1 = .4
        
        corMat1 <- matrix(c(1, r1, 
                           r1,1),
                         ncol = 2)
        
        covMat1 <- stddev1 %*% t(stddev1) * corMat1
        
        grp1 <- as.data.frame(
          mvrnorm(n = n1, 
                  mu = mu1, 
                  Sigma = covMat1, 
                  empirical = TRUE))
      
        
        grp1$group <- 1
        
        
        m3 = 6; m4 = 2.5; mu2 <- c(m3, m4)
        sd3 = 1.2; sd4 = .75; stddev2 <- c(sd3, sd4)
        r2 = .3
        
        corMat2 <- matrix(c(1, r2, 
                            r2,1),
                          ncol = 2)
        
        covMat2 <- stddev2 %*% t(stddev2) * corMat2
        
        grp2 <- as.data.frame(
          mvrnorm(n = n1, 
                  mu = mu2, 
                  Sigma = covMat2, 
                  empirical = TRUE))
        grp2$group <- 2
        
        m5 = 8; m6 = 4.5; mu3 <- c(m5, m6)
        sd5 = 1.; sd6 = .5; stddev3 <- c(sd5, sd6)
        r3 = -.15
        
        corMat3 <- matrix(c(1, r3, 
                            r3,1),
                          ncol = 2)
        
        covMat3 <- stddev3 %*% t(stddev3) * corMat3
        
        grp3 <- as.data.frame(
          mvrnorm(n = n1, 
                  mu = mu3, 
                  Sigma = covMat3, 
                  empirical = TRUE))
        grp3$group <- 3
        
        data <- data.frame(rbind(grp1, grp2, grp3))
        data$X1 <- data$V1
        data$X2 <- data$V2
        data
    })

    
    # Correlation formula ################################### 
    output$corr_formula_p <- renderUI({
      
      eq <- paste0("r = \\frac{\\sum{(x_i- \\overline{x})(y_i-\\overline{y})}}{\\sqrt{\\sum{(x_i-\\overline{x})^2}\\sum{(y_i-\\overline{y})^2}}}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    output$corr_formula_rho_1 <- renderUI({
      
      eq2 <- paste0("rho = \\frac{\\sum(x_i' - \\overline{x'})(y'_i - \\overline{y'})}{\\sqrt{\\sum(x_i' - \\overline{x'})^2 \\sum(y_i' - \\overline{y'})^2}}")
      withMathJax(
        print(paste0("$$",eq2,"$$"))
      )
    })
    
    output$corr_formula_rho_2 <- renderUI({
      
      eq2 <- paste0("rho = 1 - \\frac{6\\sum{d_i}^2}{n({n}^2 - 1)}")
      withMathJax(
        print(paste0("$$",eq2,"$$"))
      )
    })
    
    ### correlation tests
    output$corr_test <- function(){
      
      co_t1 <- cor.test(x = sim_corr()$V1, 
                        y = sim_corr()$V2,
                        method = "pearson")
      
      
      co_t2 <- cor.test(x = sim_corr_2()$V1, 
                        y = sim_corr_2()$V2,
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
      co_t1 <- cor.test(x = sim_corr()$V1, 
                        y = sim_corr()$V2,
                        method = "spearman")
      
      data.frame("Spearman rho"= c(round(as.numeric(co_t1$estimate),2)), 
                 "p-value"=c(as.numeric(co_t1$p.value)),
                 row.names = c("Var 1 ~ Var 2")) %>% 
        kable() %>% 
        kable_styling("striped",
                      full_width = F, font_size = 12, html_font = 'Times')
    }
    
    ### Plots/Tables
    output$corr_plot1 <- renderPlot({
      
      cor1 <- round(
        as.numeric(
          cor.test(x = sim_corr()$V1, 
                   y = sim_corr()$V2,
                   method = "pearson")$estimate),2)
      cor2 <- round(
        as.numeric(
          cor.test(x = sim_corr_2()$V1, 
                   y = sim_corr_2()$V2,
                   method = "pearson")$estimate),2)
      
      corplt1 <- ggplot(sim_corr(), aes(x = V1, y = V2)) +
        geom_smooth(method = "lm", se = FALSE)+
        geom_point()+
        geom_rug(size = .3, position = "jitter", colour = "sienna4")+
        labs(title = "Fig 1. Association between Var 1 and Var 2", 
             caption = "*Data simulated based on specified parameters")+
        xlab("Var1")+
        ylab("Var2")+
        annotate("label", x = mean(sim_corr()$V1)+1, y = mean(sim_corr()$V2)+1, 
                 label = paste0("Pearson's r: ",cor1))+
        theme_minimal()
      
      corplt2 <- ggplot(sim_corr_2(), aes(x = V1, y = V2)) +
        geom_smooth(method = "lm", se = FALSE)+
        geom_point()+
        geom_rug(size = .3, position = "jitter", colour = "sienna4")+
        labs(title = "Fig 2. Association between Var 3 and Var 4", 
             caption = "*Data simulated based on specified parameters")+
        xlab("Var3")+
        ylab("Var4")+
        annotate("label", x = mean(sim_corr_2()$V1)+1, y = mean(sim_corr_2()$V2)+1, 
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
    
    output$corr_grp_plt <- renderPlot({
      
      plot1 <- corr_grp_dat() %>% 
        ggplot(aes(x = X1, y = X2)) +
        geom_point() +
        geom_smooth(method = "lm", formula = 'y ~ x', se = F) +
        labs(title = "Fig 4. Correlation between X1 and X2 overall in sample")+
        xlab("X 1")+
        ylab("X 2")+
        theme_minimal()
      
      plot2 <- corr_grp_dat() %>% 
        ggplot(aes(x = X1, y = X2, colour = as.factor(group))) +
        geom_point() +
        geom_smooth(method = "lm", formula = 'y ~ x', se = F, aes(colour = as.factor(group))) +
        labs(title = "Fig 5. Correlation between X1 and X2 across three groups in sample")+
        xlab("X 1")+
        ylab("X 2")+
        theme_minimal()
      plot1 / plot2 + plot_layout(guides = 'collect')
      
    })
    
    
    output$corr_grp_tbl <- function(){
      tab1 <- corr_grp_dat() %>% 
        group_by(group) %>% 
        dplyr::summarise(N = n(), r = round(cor(X1, X2),3), 
                         Mean_X1 = mean(X1), Mean_X2 = mean(X2)) %>% 
        data.frame()
      
      tab2 <- corr_grp_dat() %>% 
        dplyr::summarise(N = n(), r = round(cor(X1, X2),3),
                         Mean_X1 = mean(X1), Mean_X2 = mean(X2)) %>% 
        mutate(group = "Overall") %>% 
        data.frame()
      
      rbind(tab1, tab2) %>% 
        kable() %>% 
        kable_styling("striped",
                      full_width = F, font_size = 12, html_font = 'Times') 
    }
    
    
    
    
# Corr Stab Simulate formula ################################### 

    # simulate data
    stable_DF <- eventReactive(input$run3, {
        
        set.seed(123)
        n = 1500; 
        m1 = input$stabcorr_m2; m2 = input$stabcorr_m2; mu <- c(m1, m2)
        sd1 = input$stabcorr_sd1; sd2 = input$stabcorr_sd2; stddev <- c(sd1, sd2)
        r1 = input$corr3
        
        corMat <- matrix(c(1, r1, 
                           r1,1),
                         ncol = 2)
        
        covMat <- stddev %*% t(stddev) * corMat
        
        df <- as.data.frame(
          mvrnorm(n = n, 
                  mu = mu, 
                  Sigma = covMat, 
                  empirical = TRUE))
        
        
        datalist = list()
        
        for (i in seq(5,1500,5)) { #input$samp_range[1],input$samp_range[2]
            Correl <- sample_n(df, i)
            corr_pear <- cor.test(Correl$V1, Correl$V2, method = "pearson")
            corr_spear <- cor.test(Correl$V1, Correl$V2, method = "spearman")
            #corr_kend <- cor.test(Correl$X1, Correl$X2, method = "kendall")
            
            datalist[[i]] <- data.frame(Pearson_r = corr_pear$estimate, 
                                        Pearson_p = corr_pear$p.value,
                                        Spearman_rho = corr_spear$estimate, 
                                        Searson_p = corr_spear$p.value,
                                        #endall_t = corr_kend$estimate, 
                                        #endall_p = corr_kend$p.value,
                                        i)
        }
        do.call(rbind, datalist)
    })


    ### Plot first   
    output$EffNchange <- renderPlot({
      
      stab1 <- stable_DF() %>% 
        ggplot(aes(x = i, y = Pearson_r)) +
        geom_line(aes(y = Pearson_r)) +
        xlab("Sample Size") +
        ylab("Pearson's r")+
        scale_y_continuous(
          name = "Pearson's r") +
        geom_vline(xintercept = 100, color = "Red", size = .4)+
        labs(title = "Fig 1. Pearson's r stability w/ N", caption = "Red line = Sample Size: 100")+
        theme(text = element_text(size = 30, family = "times"))+
        theme_minimal() 
      
      stab2 <- stable_DF() %>% 
        gather(key = "CorrelationType", 
               value = "Effect", 
               Pearson_r,Spearman_rho) %>% 
        ggplot(aes(x = i, y = Effect)) +
        geom_line(aes(colour = CorrelationType))+
        xlab("Sample Size") +
        ylab("Correlation Size") +
        labs(title = "Fig 2. Pearson v. Spearman similarity w/ N")+
        theme(text = element_text(size = 30, family = "times"))+
        theme_minimal() 
      
      stab1 / stab2 + plot_layout(guides = 'collect')
      
      
    })
    

    output$EffNchange2 <- renderPlot({
      
      df <- data.frame(
        mvrnorm(n = 6000, 
                mu = c(5, 8), 
                Sigma = matrix(c(1, .35, .35, 1), 
                               nrow = 2), 
                empirical = TRUE))
      
      set.seed(123)
      r_corr_loop <- function() {
        datalist = list()
        for (i in seq(5,1000,10)) { #input$samp_range[1],input$samp_range[2]
          
          Correl <- sample_n(df, i)
          corr_pear <- cor.test(Correl$X1, Correl$X2, method = "pearson")
          
          datalist[[i]] <- data.frame(Pearson_r = corr_pear$estimate,
                                      i)
        }
        return(datalist)
      }
      
      data <- do.call(rbind,replicate(30, r_corr_loop())) 
      
      plot1 <- data %>% 
        ggplot(aes(x = i, y = Pearson_r)) + 
        stat_summary(geom="ribbon", fun.data=mean_cl_boot, 
                     fill="black", alpha = .1)+
        stat_summary(geom="line", fun=mean, linetype="dashed")+
        xlab("Sample Size") +
        ylab("Correlation Size") +
        labs(title = "Fig 3. Repeated (n=30) Sampling for r = .35 from N = 6,000 data w/ 95 Conf Interval")+
        theme_minimal()
      plot2 <- data %>% 
        ggplot(aes(y = Pearson_r)) + 
        geom_histogram(bins = 80, fill = "blue", alpha = .8)+
        geom_hline(yintercept = .35, color = "red", size = .3)+
        xlab("Frequency") +
        ylab("Pearson's r value") +
        labs(title = "Fig 4. Density of r values", caption = "Red line indicate r = .35 (bin size 80)")+
        theme_minimal()
      
      plot1 / plot2 + plot_layout(guides = 'collect')
      
    })
    
    ### NHANES example 
    output$EffNchange3 <- renderPlot({
      library(foreign)
      df <- read.csv("./sub6000_nhanes.csv")
      
      #df <- sample_n(data, 6000) %>% 
      #    dplyr::select(ID, BPSysAve, BPDiaAve)
      
      
      set.seed(123)
      r_corr_loop2 <- function() {
        datalist = list()
        for (i in seq(5,1000,10)) { #input$samp_range[1],input$samp_range[2]
          
          Correl <- sample_n(df, i)
          corr_pear <- cor.test(Correl$BPSysAve, Correl$BPDiaAve, method = "pearson")
          
          datalist[[i]] <- data.frame(Pearson_r = corr_pear$estimate,
                                      i)
        }
        return(datalist)
      }
      
      do.call(rbind,replicate(30, r_corr_loop2())) %>% 
        ggplot(aes(x = i, y = Pearson_r)) + 
        stat_summary(geom="ribbon", fun.data=mean_cl_boot, 
                     fill="blue", alpha = .1)+
        stat_summary(geom="line", fun=mean, linetype="dashed")+
        xlab("Sample Size") +
        ylab("Correlation Size") +
        labs(title = "Fig 4. NHANES, Repeated (n=30) Sampling for BP Systolic & Diastolic ")+
        theme_minimal()
      
      
    })
    

    
# Regression Simulate ################################### 
    
    ### simulate data
    m_reg_data <- eventReactive(input$run5, {
      set.seed(123)
      n = input$m_reg_sample; 
      m1 = input$m_reg_m1; m2 = input$m_reg_m2; m3 = input$m_reg_m3; mu <- c(m1, m2, m3)
      sd1 = input$m_reg_sd1; sd2 = input$m_reg_sd2; sd3 = input$m_reg_sd3; stddev <- c(sd1, sd2, sd3)
      r1 = input$m_reg_corr1; r2 =  input$m_reg_corr2; r3 = input$m_reg_corr3
      
      corMat <- matrix(c(1, r1, r2,
                         r1, 1, r3,
                         r2, r3, 1),
                       ncol = 3)
      covMat <- stddev %*% t(stddev) * corMat
      
      dat1 <- as.data.frame(
        mvrnorm(n = n, 
                mu = mu, 
                Sigma = covMat, 
                empirical = TRUE)
      )
      if (input$outlier=="None") {
        m_reg_df <-
          data.frame(
            mvrnorm(n = n, 
                    mu = c(m1, m2, m3),
                    Sigma = covMat, 
                    empirical = TRUE))
        
            m_reg_df<- rename(m_reg_df, 
                              c("X1"="Drinking", "X2" ="Reward_Seek","X3"="Age"))
            m_reg_df
            
        } else if (input$outlier=="One") {
          m_reg_df <-
            data.frame(
              mvrnorm(n = n, 
                      mu = c(m1, m2, m3),
                      Sigma = covMat, 
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
              mvrnorm(n = n, 
                      mu = c(m1, m2, m3),
                      Sigma = covMat, 
                      empirical = TRUE))
          
            m_reg_df<- rename(m_reg_df, 
                              c("X1"="Drinking", "X2" ="Reward_Seek","X3"="Age"))
            rand_row = as.integer(runif(2,min = 1, max = length(m_reg_df$Drinking)))
            rand_col = as.integer(runif(2, min = 1, max = 2))
            m_reg_df[rand_row[1],rand_col[1]]<- m_reg_df[rand_row[1],rand_col[1]] + (-5*sd(m_reg_df[,rand_col[1]]))
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
            m_reg_df[rand_row[2],rand_col[2]]<- m_reg_df[rand_row[2],rand_col[2]] + (-5*sd(m_reg_df[,rand_col[2]]))
            m_reg_df[rand_row[3],rand_col[3]]<- m_reg_df[rand_row[3],rand_col[3]] + (5*sd(m_reg_df[,rand_col[3]]))
            m_reg_df
        }
    })
    
    

    # linear formula ################################### 
    output$m_reg_formula_1 <- renderUI({
      
      eq <- eq <- paste0("Drinking = {\\beta}0 + {\\beta}1_{Reward}+ {\\epsilon}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    output$m_reg_calc_b0 <- renderUI({
      
      eq <- paste0("\\hat{\\beta}0 = {\\overline{y} - {\\hat{\\beta}1}{\\overline{x}}}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    output$m_reg_calc_b1 <- renderUI({
      
      eq <- paste0("\\hat{\\beta}1 = \\frac{\\sum{(x_i - \\overline{x})(y_i-\\overline{x})}}{\\sum{(x_i-\\overline{x})^2}}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    output$r_sqrd <- renderUI({
      
      eq <- paste0("R^2 = {1-}\\frac{SS_{Residual}}{SS_{Total}}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    output$adjst_r_sqrd <- renderUI({
      
      eq <- paste0("Adjusted R^2 = {1}-\\frac{(1-R^2)(N-1)}{N-p-1}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    # linear output ################################### 
    
    output$m_reg_1_descr <- function(){
      m_reg_data() %>% 
      dplyr::summarize("N" = n(), "Drinking Mean" = mean(Drinking), "Drinking SD" = sd(Drinking), 
                "Reward Seek Mean" = mean(Reward_Seek), "Reward Seek SD" = sd(Reward_Seek)) %>% 
        kable(caption = "Table 1. Descriptive Statistics for Simulated Data") %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
    }
    
    output$m_reg_1_coef <- function(){
      fit<-lm(Drinking ~ Reward_Seek, m_reg_data())

      summary(fit)$coefficients %>% 
        kable(caption = "Table 2. Linear Regression Coefficients", 
              col.names = c("Estimate", "Std. Error", "T-Statistics", "p-value")) %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
      
    }
    
    output$m_reg_1_model <- function(){
      fit<-lm(Drinking ~ Reward_Seek, m_reg_data())
      r1 <- round(summary(fit)$r.squared,3)
      r2 <- round(summary(fit)$adj.r.squared,3)
      f_stat <- round(as.numeric(summary(fit)$fstatistic[1]),3)
      df1 <- as.numeric(summary(fit)$fstatistic[2])
      df2 <- as.numeric(summary(fit)$fstatistic[3])
      model_p <- 1-pf(q = f_stat, df1 = df1, df2 = df2)
      
      
      data.frame("F-statistic" = f_stat, "df1"= df1, "df2" =df2,
               "p-value"= model_p,"R-squared"= r1,"R-adjust"= r2) %>% 
        kable(caption = "Table 3. Linear Regression Model Fit",
              col.names = c("F-statistic", "df1", "df2", "p-value", "R squared", "adjusted R squared")) %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
      
    }
    
    output$m_reg_1_corr <- function(){
      fit<-lm(Drinking ~ Reward_Seek, m_reg_data())
      cor(m_reg_data()$Drinking,m_reg_data()$Reward_Seek) %>%  
        kable(caption = "Table 4. Correlation Between Drinking (DV) and Reward Seeking (IV)",
              col.names = "Pearson's r") %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
    }
    
    
    

    # Multiple Formula ################################### 
    output$mult_corr <- renderUI({
      
      eq <- paste0("r = \\frac{\\sum{(x-\\overline{x})(y-\\overline{y})}}{\\sqrt{\\sum{(x-\\overline{x})^2}\\sum{(y-\\overline{y})^2}}}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    output$mult_part_corr <- renderUI({
      
      eq <- paste0("r_{yx.z} = \\frac{r_{yx}-(r_{yz})(r_{xz})}{\\sqrt{1-r_{yz}^2}\\sqrt{1-r_{xz}^2}}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    # Multiple output ################################### 
    
    output$m_reg_2_descr <- function(){
      m_reg_data() %>% 
        dplyr::summarize("N" = n(), "Drinking Mean" = mean(Drinking), "Drinking SD" = sd(Drinking), 
                         "Reward Seek Mean" = mean(Reward_Seek), "Reward Seek SD" = sd(Reward_Seek),
                         "Age Mean" = mean(Age), "Age SD" = sd(Age)) %>% 
        kable(caption = "Table 4. Descriptive Statistics for Simulated Data") %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
    }
    
    
    output$m_reg_2_coef <- function(){
      fit<-lm(Drinking ~ Reward_Seek + Age, m_reg_data())
      
      summary(fit)$coefficients %>% 
        kable(caption = "Table 5. Multiple Regression Coefficients", 
              col.names = c("Estimate", "Std. Error", "T-Statistics", "p-value")) %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
      
    }
    
    output$m_reg_2_model <- function(){
      fit<-lm(Drinking ~ Reward_Seek + Age, m_reg_data())
      r1 <- round(summary(fit)$r.squared,3)
      r2 <- round(summary(fit)$adj.r.squared,3)
      f_stat <- round(as.numeric(summary(fit)$fstatistic[1]),3)
      df1 <- as.numeric(summary(fit)$fstatistic[2])
      df2 <- as.numeric(summary(fit)$fstatistic[3])
      model_p <- 1-pf(q = f_stat, df1 = df1, df2 = df2)
      
      
      data.frame("F-statistic" = f_stat, "df1"= df1, "df2" =df2,
                 "p-value"= model_p,"R-squared"= r1,"R-adjust"= r2) %>% 
        kable(caption = "Table 6. Multiple Regression Model Fit",
              col.names = c("F-statistic", "df1", "df2", "p-value", "R squared", "adjusted R squared")) %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
      
    }
    
    output$m_reg_2_std_beta <- function(){
      fit<-lm(Drinking ~ Reward_Seek + Age, m_reg_data())
      
      round(lm.beta(fit),2) %>% 
        kable(caption = "Table 7. Multiple Regression Standardized Coefficients (lm.beta in R)",
              #col.names = c("Reward Seek", "Age")
              ) %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
      
    }
    
    output$m_reg_2_semi_par <- function(){
      p_cor <- as.data.frame(pcor(m_reg_data(), "pearson")[1])
      round(p_cor,2) %>% 
        kable(caption = "Table 8. Partial Pearson Correlation for Regression Variables (pcor in R)",
              col.names = c("Drinking", "Reward_Seek", "Age")
              ) %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
      
    }
    
    output$m_reg_formula_2 <- renderUI({
       
      eq <- eq <- paste0("Drinking = {\\beta}0 + {\\beta}1_{Reward}+ {\\beta}2_{Age} + {\\epsilon}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    
    ###     Interactive plot    
    output$m_reg_plot_1 <- renderPlot({
      DV_mean = mean(m_reg_data()$Drinking)
      ggplot(m_reg_data(), aes(x = Reward_Seek, y = Drinking)) + 
        geom_point()+
        geom_smooth(method="lm", se = F)+
        xlab("Reward Seeking") +
        ylab("Drrinking")+
        geom_hline(yintercept = DV_mean, color = "red", linetype = 'dotted')+
        labs(title = "Fig 1. Data Plot of Regression Model", caption = "Red line is mean for DV, Drinking")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      

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
        labs(title = "Fig 2. OLS Predicted ~ Reward", 
             caption = "*Output from lm(Drinking ~ Reward, data = simulated)")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      plot2<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = predict_vals)) +
        geom_line(size = 1)+
        geom_point(aes(x = Reward_Seek, y = Drinking), size =1/input$m_reg_sample) +
        geom_segment(
          aes(xend = Reward_Seek, yend = Drinking),
          size = .2, alpha = .2, lineend = "round")+
        labs(title = "Fig 3. OLS Predicted ~ Observed Drinking", 
             caption = "Fitted values out of lm()$fitted.values & observed simulated")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      
      plot3<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = resid_vals)) +
        geom_point(size =1/input$m_reg_sample) +
        geom_smooth(method="loess")+
        labs(title = "Fig 4.  Residuals ~ Reward", 
             caption = "Residuals from lm()$residuals & observed simulated")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      plot4<- ggplot(m_reg_data_2, aes(x = Drinking, y = resid_vals)) +
        geom_point(size =1) +
        geom_smooth(method="loess")+
        labs(title = "Fig 5. Residuals ~ Drinking", 
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
        labs(title = "Fig 6. OLS Predicted ~ Reward", 
             caption = "*Output from lm(Drinking ~ Reward + Age, data = simulated)")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      plot6<- ggplot(m_reg_data_2, aes(x = Age, y = predict_vals)) +
        geom_point()+
        geom_smooth(method="lm")+
        labs(title = "Fig 7. OLS Predicted  ~ Age", 
             caption = "*Output from lm(Drinking ~ Reward + Age, data = simulated)")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      plot7<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = predict_vals)) +
        geom_line(size = 1)+
        geom_point(aes(x = Reward_Seek, y = Drinking), size =1/input$m_reg_sample) +
        geom_segment(
          aes(xend = Reward_Seek, yend = Drinking),
          size = .2, alpha = .2, lineend = "round")+
        labs(title = "Fig 8. OLS Predicted ~ Observed Drinking", 
             caption = "Fitted values out of lm()$fitted.values & observed simulated")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      
      plot8<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = resid_vals)) +
        geom_point(size =1/input$m_reg_sample) +
        geom_smooth(method="loess")+
        labs(title = "Fig 9. Residuals ~ Reward", 
             caption = "Residuals from lm()$residuals & observed simulated")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      plot9<- ggplot(m_reg_data_2, aes(x = Age, y = resid_vals)) +
        geom_point(size =1/input$m_reg_sample) +
        geom_smooth(method="loess")+
        labs(title = "Fig 10. Residuals ~ Age", 
             caption = "Residuals from lm()$residuals & observed simulated")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      plot10<- ggplot(m_reg_data_2, aes(x = Drinking, y = resid_vals)) +
        geom_point(size =1) +
        geom_smooth(method="loess")+
        labs(title = "Fig 10. Residuals ~ Drinking", 
             caption = "Residuals from lm()$residuals & observed simulated")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      (plot5 | plot6) / (plot7 | plot8) / (plot9 | plot10)
    })
    

    
# Construct Validation ###############################################################      
    

# CFA_PCA Simulate ###############################################################      
    
    
    ### Simulation sample covariance for specified loadings
    
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
    
    
    
# CFA_PCA output #########################
 
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
