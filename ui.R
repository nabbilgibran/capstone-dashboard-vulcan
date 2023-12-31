
#header---------------------------------------------------------------------------
header <-
  dashboardHeader(title = "WORLD SUICIDE RATES OVERVIEW 1985-2016",
                  titleWidth = 400)

#sidebar--------------------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text = "Global Overview",
      tabName = "global",
      icon = icon("lightbulb")
    ),
    menuItem(
      text = "Human-Economic Index",
      tabName = "human",
      icon = icon("users")
    ),
    menuItem(
      text = "Distribution",
      tabName = "map",
      icon = icon("globe-americas")
    ),
    menuItem(
      text = "Data",
      tabName = "data",
      icon = icon("table")
    )
  ))

#body ----------------------------------------------------------------------------
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "global",
      fluidRow(
        box(
          title = "WORLDWIDE OVERVIEW",
          width = 12,
          valueBoxOutput(outputId = "numsui", width = 3),
          valueBoxOutput(outputId = "ratesui", width = 3),
          valueBoxOutput(outputId = "ratemaxcont", width = 3),
          valueBoxOutput(outputId = "ratemaxcountry", width = 3)
        )
      ),
      fluidRow(
        column(
          width = 12,
          plotlyOutput(outputId = "lineContinent")
        )
      ),
      fluidRow(
        box(
          title = "CONTINENT OVERVIEW",
          width = 2,
          height = "500px",
          radioButtons(
            inputId = "continentrate",
            label = "Select the Continent:",
            choices = unique(suicide$continent),
            selected = "Europe",
            inline=F
          ),
          valueBoxOutput(outputId = "numsuicont", width = 12),
          valueBoxOutput(outputId = "ratesuicont", width = 12)
        ),
        box(
          title = "Timeline Number of Suicides by Continent",
          width = 5,
          height = "500px",
          plotlyOutput("suitimeline")
        ),
        box(
          title = "Timeline Number of Suicides/100k Population by Continent",
          width = 5,
          height = "500px",
          plotlyOutput("suiyearly")
        )
      ),
      fluidRow(
        box(
          title = "COUNTRY OVERVIEW",
          width = 2,
          height = "465px",
          selectInput(
            inputId = "countryrate",
            label = "Select the Country:",
            choices = unique(suicide$country),
            selected = "Luxembourg",
          ),
          valueBoxOutput(outputId = "numsuicountry", width = 12),
          valueBoxOutput(outputId = "ratesuicountry", width = 12)
        ),
        box(
          title = "Timeline Number of Suicides by Country",
          width = 5,
          plotlyOutput("suitimelinecountry")
        ),
        box(
          title = "Timeline Number of Suicides/per 100k Population by Country ",
          width = 5,
          plotlyOutput("suiage")
        )
      )
    ),
    tabItem(
      tabName = "human",
      fluidRow(
        column(
          width = 9,
          plotlyOutput("scatterGDP")
        ),
        box(
          width = 3,
          title = p("GDPPC", style = "font-weight:bold; font-size: 30px;"),
          background = "aqua",
          tagList(
            a("Gross Domestic Product per Capita",
              href = "https://www.who.int/data/gho/indicator-metadata-registry/imr-details/1145",
              target = "_blank",
              style = "color:blue; text-decoration:underline; font-weight:bold; font-size: 18px;"),
            p("is the average per capita market value of the sum of gross values added of all resident institutional units engaged in production, for a given national economy, at a given period in time, usually a year, expressed in international dollars using purchasing power parity rates.",
              style = "font-size: 18px;"),
          )
        )
      ),
      
      br(),
      br(),
      
      fluidRow(
        box(
          width = 3,
          title = p("HDI", style = "font-weight:bold; font-size: 30px;"),
          background = "aqua",
          tagList(
            a("Human Development Index",
              href = "http://hdr.undp.org/en/content/human-development-index-hdi",
              target = "_blank",
              style = "color:blue; text-decoration:underline; font-weight:bold; font-size: 18px;"),
            p("is a summary measure of average achievement in key dimensions of human development: a long and healthy life, being knowledgeable and have a decent standard of living. The HDI is the geometric mean of normalized indices for each of the three dimensions.",
              style = "font-size: 18px;"),
          )
        ),
        column(
          width = 9,
          plotlyOutput("scatterHDI")
        )
      )
    ),
    tabItem(
      tabName = "map",
      fluidRow(
        box(
          title = "WORLDWIDE SUICIDE DISTRIBUTION MAP", "Zoom and Hover to see the number of suicides and the country",
          width = 8,
          selectInput(
            inputId = "peta_bundir",
            label = "Select the Year:",
            choices = sort(unique(suicide$date_year)),
            selected="1993"
          ),
          height = "920px",
          plotlyOutput(outputId = "petamapsui",width="100%",height="700px" )
        ),
        box(
          title = "TOTAL NUMBER OF SUICIDES RECORDED ON SELECTED YEAR",
          width = 4,
          height = "330px",
          valueBoxOutput(outputId = "totalcountry", width = 6),
          valueBoxOutput(outputId = "totalnumyear", width = 6),
          valueBoxOutput(outputId = "totalrateyear", width = 12)
        ),
        box(
          title = "TOTAL DEMOGRAPHIC ON SELECTED YEAR", "Hover the cursor over the plot to display the detail",
          width = 4,
          height = "570px",
          plotlyOutput("totaldemographic",height="480px")
        )
      )
    ),
    tabItem(
      tabName = "data",
      
      fluidRow(
        box(
          title = "Data Source", " link source: https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016", "",
          width = 12,
          dataTableOutput(outputId = "datasuicide" )
        )
      )
    )
    
  )
)

#dashboard-----------------------------------------------------------------------

dashboardPage(header, sidebar, body, skin = "purple")
