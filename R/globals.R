# Global variable declarations to satisfy R CMD check
# These are used in data.table expressions with non-standard evaluation

utils::globalVariables(c(
  # data.table column names used in non-standard evaluation
  "pfun",
  "title",
  "analysis_type",
  "date_range",
  "info",
  "filename",
  "version",
  "type",
  "keep",
  "file_path",
  "download_date",
  "file_size",
  "link_text",

  # data.table special symbols
  ":=",
  ".SD",
  ".N",
  "..analysis_type"
))
