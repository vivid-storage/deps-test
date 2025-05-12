library(shiny)
library(reticulate)

# Initialize Python first
reticulate::py_discover_config()

# Define required R packages
required_r_packages <- c("shiny", "naniar", "ggplot2", "DT", "bslib")

# Function to get R packages and versions
get_r_packages <- function() {
  pkgs <- as.data.frame(installed.packages()[, c("Package", "Version")])
  
  # Split into required and non-required packages
  required_pkgs <- pkgs[pkgs$Package %in% required_r_packages, ]
  other_pkgs <- pkgs[!pkgs$Package %in% required_r_packages, ]
  
  # Sort each group
  required_pkgs <- required_pkgs[order(required_pkgs$Package), ]
  other_pkgs <- other_pkgs[order(other_pkgs$Package), ]
  
  # Add "required" label to required packages
  required_pkgs$Label <- paste0(required_pkgs$Package, " (required)")
  other_pkgs$Label <- other_pkgs$Package
  
  # Combine required (first) and non-required packages
  combined_pkgs <- rbind(required_pkgs, other_pkgs)
  
  return(combined_pkgs)
}

# Function to get Python packages and versions
get_python_packages <- function() {
  # First, explicitly check for required packages
  py_run_string("
# Explicitly check for packages in requirements.txt
required_packages = ['numpy', 'pandas', 'matplotlib', 'dummy_pkg']
req_status = []

for pkg in required_packages:
    try:
        # Try to import the package
        module = __import__(pkg)
        version = getattr(module, '__version__', 'Installed (version unknown)')
        req_status.append((pkg, version, True))  # True for required
    except ImportError:
        req_status.append((pkg, 'NOT INSTALLED', True))

# Then try to get all packages
import pkg_resources
all_packages = sorted([(p.key, p.version, False) for p in pkg_resources.working_set])  # False for not required

# Remove packages we already checked
all_packages = [(name, ver, is_req) for name, ver, is_req in all_packages 
               if name not in required_packages]

# Combine required and other packages
packages = req_status + all_packages
")
  py_packages <- py$packages
  return(data.frame(
    Package = sapply(py_packages, function(x) x[[1]]),
    Version = sapply(py_packages, function(x) x[[2]]),
    Required = sapply(py_packages, function(x) x[[3]]),
    stringsAsFactors = FALSE
  ))
}

# Get Python version
get_python_version <- function() {
  version <- py_config()$version
  
  # Also check Python path
  py_run_string("
import sys
python_path = sys.executable
python_version = '.'.join(map(str, sys.version_info[:3]))
  ")
  path <- py$python_path
  
  return(paste0(version, " (", path, ")"))
}

ui <- fluidPage(
  titlePanel("Installed Packages for R and Python"),
  
  fluidRow(
    column(12,
      wellPanel(
        h3("Package Installation Requirements"),
        p(paste("Required R packages:", paste(required_r_packages, collapse=", "))),
        p("Required Python packages: numpy, pandas, matplotlib, dummy_pkg"),
        p("Check below to see if these packages are properly installed.")
      )
    )
  ),
  
  fluidRow(
    column(6,
      wellPanel(
        h3(paste0("R version: ", R.version$version.string)),
        uiOutput("r_packages_list")
      )
    ),
    column(6,
      wellPanel(
        h3(textOutput("python_version_text")),
        uiOutput("python_packages_list")
      )
    )
  )
)

server <- function(input, output) {
  # Python version
  output$python_version_text <- renderText({
    paste0("Python version: ", get_python_version())
  })
  
  # Display R packages as a list
  output$r_packages_list <- renderUI({
    pkgs <- get_r_packages()
    
    # Limit to 100 packages to avoid performance issues
    if(nrow(pkgs) > 100) {
      pkgs <- pkgs[1:100,]
    }
    
    # Create list items with packages and versions in the same element
    pkgs_list <- lapply(1:nrow(pkgs), function(i) {
      pkg_name <- pkgs$Label[i]
      version <- pkgs$Version[i]
      is_required <- pkg_name != pkgs$Package[i]  # If Label != Package, it's required
      
      if (is_required) {
        # Required package - highlight in green
        tags$div(
          HTML(paste0("<span style='color:green'><b>", pkg_name, "</b>: ", version, "</span>")),
          class = "package-item"
        )
      } else {
        # Regular package
        tags$div(
          HTML(paste0("<b>", pkg_name, "</b>: ", version)),
          class = "package-item"
        )
      }
    })
    
    # Return all packages in a div with some CSS for better spacing
    tagList(
      tags$style(".package-item { margin-bottom: 5px; }"),
      tags$div(class = "packages-container", pkgs_list),
      if(nrow(get_r_packages()) > 100) {
        p("(Showing first 100 packages)")
      }
    )
  })
  
  # Display Python packages as a list
  output$python_packages_list <- renderUI({
    pkgs <- get_python_packages()
    
    # Create list items with packages and versions in the same element
    pkgs_list <- lapply(1:nrow(pkgs), function(i) {
      pkg_name <- pkgs$Package[i]
      version <- pkgs$Version[i]
      is_required <- pkgs$Required[i]
      
      if (is_required) {
        if (grepl("NOT INSTALLED", version, fixed = TRUE)) {
          # Not installed required package - highlight in red
          tags$div(
            HTML(paste0("<span style='color:red'><b>", pkg_name, " (required)</b>: ", version, "</span>")),
            class = "package-item"
          )
        } else {
          # Installed required package - highlight in green
          tags$div(
            HTML(paste0("<span style='color:green'><b>", pkg_name, " (required)</b>: ", version, "</span>")),
            class = "package-item"
          )
        }
      } else {
        # Regular package
        tags$div(
          HTML(paste0("<b>", pkg_name, "</b>: ", version)),
          class = "package-item"
        )
      }
    })
    
    # Return all packages in a div with some CSS for better spacing
    tagList(
      tags$style(".package-item { margin-bottom: 5px; }"),
      tags$div(class = "packages-container", pkgs_list),
      if(nrow(pkgs) > 100) {
        p("(Showing first 100 packages)")
      }
    )
  })
}

shinyApp(ui = ui, server = server)
