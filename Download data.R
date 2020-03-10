covid19.data <- function(last.day = "03-09-2020") {
  oldw <- getOption("warn")
  options(warn = -1)
  librares = "readr"
  for (i in librares)
    if (i %in% rownames(installed.packages()) == FALSE)
      install.packages(i)
  
  progress <- function(it,
                       min = 1,
                       max = 100,
                       title = "Coding by Ahad Alizadeh",
                       text = "Hello World!") {
    if (it == min)
      progression...... <<- winProgressBar(
        min = min,
        label = "% done",
        max = max,
        width = 300
      )
    
    setWinProgressBar(
      progression......,
      it,
      title =
        paste0(title, ", ", round((it + 1) / (max) * 100), "% done."),
      label = text
    )
    
    if (it == max)
      close(progression......)
    
  }
  
  date = format(seq.Date(
    as.Date("01-22-2020", "%m-%d-%Y"),
    as.Date(last.day, "%m-%d-%Y"),
    1
  ), "%m-%d-%Y")
  line = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
  data = list()
  name <- Sys.getenv("USERNAME")
  for (i in 1:length(date)) {
    progress(
      i,
      min = 1,
      max = length(date),
      title = "OUR COVID19 GROUP",
      text = paste0(name, " wait, please!")
    )
    D = try(readr::read_csv(paste0(line, date[i], ".csv"), progress = FALSE), silent =
              TRUE)
    if (class(D)[1] == "try-error")
      next()
    data[[i]] <- cbind(D,
                       date = date[i], day = i)
    if ("Latitude" %in% names(data[[i]]))
      data[[i]]$Latitude <- NULL
    if ("Longitude" %in% names(data[[i]]))
      data[[i]]$Longitude <- NULL
  }
  data <- as.data.frame(do.call(rbind, data))
  options(warn = oldw)
  data
}

COVID19 <- covid19.data()
cat("\n\nAhad: You can find data in `COVID19` variable!\n\n")
