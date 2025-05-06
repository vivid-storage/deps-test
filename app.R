library(shiny)
library(reticulate)

# Initialize Python first
tryCatch({
  reticulate::py_discover_config()
}, error = function(e) {
  # Failed to initialize Python - app will handle this gracefully
})

# Function to get R packages and versions
get_r_packages <- function() {
  pkgs <- as.data.frame(installed.packages()[, c("Package", "Version")])
  pkgs <- pkgs[order(pkgs$Package), ]
  return(pkgs)
}

# Function to get Python packages and versions - with error handling
get_python_packages <- function() {
  if (!py_available()) {
    return(data.frame(Package = "Python not available", Version = "Error"))
  }
  
  tryCatch({
    # First, explicitly check for required packages
    py_run_string("
# Explicitly check for packages in requirements.txt
required_packages = ['numpy', 'pandas', 'matplotlib']
req_status = []

for pkg in required_packages:
    try:
        # Try to import the package
        module = __import__(pkg)
        version = getattr(module, '__version__', 'Installed (version unknown)')
        req_status.append((f'{pkg} (required)', version))
    except ImportError:
        req_status.append((f'{pkg} (required)', 'NOT INSTALLED'))

# Then try to get all packages
try:
    import pkg_resources
    all_packages = sorted([(p.key, p.version) for p in pkg_resources.working_set])
    
    # Remove packages we already checked
    all_packages = [(name, ver) for name, ver in all_packages 
                   if name not in required_packages]
    
    # Combine required and other packages
    packages = req_status + all_packages
except:
    packages = req_status
    ")
    py_packages <- py$packages
    return(data.frame(
      Package = sapply(py_packages, function(x) x[[1]]),
      Version = sapply(py_packages, function(x) x[[2]])
    ))
  }, error = function(e) {
    return(data.frame(
      Package = c("Error retrieving packages"),
      Version = c(as.character(e))
    ))
  })
}

# Get Python version with error handling
get_python_version <- function() {
  tryCatch({
    version <- py_config()$version
    
    # Also check Python path
    py_run_string("
import sys
python_path = sys.executable
python_version = '.'.join(map(str, sys.version_info[:3]))
    ")
    path <- py$python_path
    
    return(paste0(version, " (", path, ")"))
  }, error = function(e) {
    return("Python not available")
  })
}

ui <- fluidPage(
  titlePanel("Installed Packages for R and Python"),
  
  fluidRow(
    column(12,
      wellPanel(
        h3("Package Installation Requirements"),
        p("Required Python packages: numpy, pandas, matplotlib"),
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
  # Python version with error handling
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
    
    pkgs_html <- lapply(1:nrow(pkgs), function(i) {
      p(HTML(paste0("<b>", pkgs$Package[i], "</b>: ", pkgs$Version[i])))
    })
    
    tagList(
      pkgs_html,
      if(nrow(get_r_packages()) > 100) {
        p("(Showing first 100 packages)")
      }
    )
  })
  
  # Display Python packages as a list
  output$python_packages_list <- renderUI({
    pkgs <- get_python_packages()
    
    # Highlight required packages
    pkgs_html <- lapply(1:nrow(pkgs), function(i) {
      pkg_name <- pkgs$Package[i]
      version <- pkgs$Version[i]
      
      if (grepl("required", pkg_name, fixed = TRUE)) {
        # Required package - highlight it
        if (grepl("NOT INSTALLED", version, fixed = TRUE)) {
          # Not installed - highlight in red
          p(HTML(paste0("<b style='color:red'>", pkg_name, "</b>: ", version)))
        } else {
          # Installed - highlight in green
          p(HTML(paste0("<b style='color:green'>", pkg_name, "</b>: ", version)))
        }
      } else {
        # Regular package
        p(HTML(paste0("<b>", pkg_name, "</b>: ", version)))
      }
    })
    
    # Add installation instructions if packages are missing
    missing_pkgs <- pkgs[grepl("NOT INSTALLED", pkgs$Version, fixed = TRUE), ]
    installation_help <- NULL
    
    if (nrow(missing_pkgs) > 0) {
      missing_names <- gsub(" \\(required\\)", "", missing_pkgs$Package)
      install_cmd <- paste0("pip install ", paste(missing_names, collapse = " "))
      
      installation_help <- div(
        style = "background-color: #f8f9fa; padding: 10px; border: 1px solid #ddd; margin-top: 15px;",
        h4("Missing Required Packages"),
        p("Some required packages are not installed. You can install them with:"),
        code(install_cmd),
        p("Then restart the application.")
      )
    }
    
    tagList(
      pkgs_html,
      installation_help,
      if(nrow(pkgs) > 100) {
        p("(Showing first 100 packages)")
      }
    )
  })
}

shinyApp(ui = ui, server = server)
