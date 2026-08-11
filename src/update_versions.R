# 1. Install and load the yaml package if not already installed
if (!requireNamespace("yaml", quietly = TRUE)) install.packages("yaml")
library(yaml)

file_path <- "../environment.yml"

# 2. Read the existing environment.yml file
env_data <- read_yaml(file_path)

# 3. Get currently installed conda packages via system call
conda_list <- system("conda list", intern = TRUE)
installed_pkgs <- list()

for (line in conda_list) {
  if (nzchar(line) && !startsWith(line, "#")) {
    parts <- strsplit(line, "\\s+")[[1]]
    if (length(parts) >= 2) {
      installed_pkgs[[parts[1]]] <- parts[2]
    }
  }
}

# 4. Update versions for R packages listed in dependencies
updated_deps <- list()

for (dep in env_data$dependencies) {
  # Only process standard string dependencies (skip nested blocks like pip)
  if (is.character(dep)) {
    # Extract base package name by stripping any existing version pinning
    pkg_name <- strsplit(dep, "=")[[1]][1]
    
    # Check if it is an R package (starts with 'r-') and is installed
    if (startsWith(pkg_name, "r-") && !is.null(installed_pkgs[[pkg_name]])) {
      current_version <- installed_pkgs[[pkg_name]]
      updated_deps <- c(updated_deps, paste0(pkg_name, "=", current_version))
      next
    }
  }
  # Keep non-R packages or nested lists exactly as they were
  updated_deps <- c(updated_deps, list(dep))
}

# 5. Save the updated data back to the file
env_data$dependencies <- updated_deps
write_yaml(env_data, file_path)

cat("Successfully updated R package versions in environment.yml!\n")
