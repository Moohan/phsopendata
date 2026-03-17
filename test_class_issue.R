
df <- data.frame(t = Sys.time())
tryCatch({
  types <- purrr::map_chr(df, class)
  print(types)
}, error = function(e) {
  cat("Error as expected:", e$message, "\n")
})
