# the below code is from Hugh:
# https://stackoverflow.com/questions/48492200/latex-expression-replacement-in-r

library(data.table) # for shift
library(TeXCheckR)  # for parse_tex

locate_over <- function(doc_parsed) {
  lead <- function(x, n) data.table::shift(x, n = n, type = "lead", fill = "")
  char <- .subset2(doc_parsed, "char")
  which(char == "\\" &
          lead(char == "o", 1L) &
          lead(char == "v", 2L) &
          lead(char == "e", 3L) &
          lead(char == "r", 4L))
}

over2frac <- function(lines, verbose = FALSE) {
  out <- lines
  for (i in seq_along(lines)) {
    if (grepl("\\over", lines[i], fixed = TRUE)) {
      i_parsed <- parse_tex(lines[i])

      # Find lhs
      for (j in locate_over(i_parsed)) {
        lhs_start <- max(which(.subset2(i_parsed, "char") %chin% c("$", "{") &
                                 .subset2(i_parsed, "column") < j &
                                 .subset2(i_parsed, "tex_group") == .subset2(i_parsed[j], "tex_group")))

        rhs_end <- min(which(.subset2(i_parsed, "char") %chin% c("$", "}") &
                               .subset2(i_parsed, "column") > j + 4L &
                               .subset2(i_parsed, "tex_group") == .subset2(i_parsed[j], "tex_group")))

        i_parsed[lhs_start, char := "{\\frac{"]
        i_parsed[rhs_end, char := "}}"]
      }
      res <- paste0(i_parsed[["char"]], collapse = "")
      res <- gsub("\\over", "}{", res, fixed = TRUE)
      out[i] <- res
    }
  }
  out
}

maxima <- function(options) {
	code <- paste(options$code, collapse = "\n")
	prg <- Sys.which("maxima")

	# input echo formatting

	# output formatting 
	# insert newline before each (%i|ox)

	if(options$engine.opts$latex == TRUE) {
		# delete trailling ";" or "$"
		latex_code <- gsub(x = options$code,
				   pattern = "(\\$|;)$",
				   replacement = "")

		# remoce empty lines
		latex_code <- latex_code[nchar(latex_code) > 0]

		# wrapp each code line into tex()
		latex_code <- gsub(x = latex_code,
				   pattern = "^([[:print:]]*)$",
				   replacement = "tex(\\1)")

		# join each code line into one string
		latex_code <- paste0(latex_code, separator = "$", collapse = "")

		# send code to maxima 
		out <- system(paste0(prg, " -q ", " --batch-string=", shQuote(latex_code)), intern = TRUE)

		# remove each line that echos input
		out <- gsub(x = out,
			    pattern = "^\\(%(i|o)[[:digit:]]*\\)[[:print:]]*$",
			    replacement = "")

		# remove empty lines
		out <- out[nchar(out) > 0]

		# replace \over with \frac commands
		out <- over2frac(lines = paste(out, collapse = "\n"))

		# replace greek letter with their latex commands
		# out <- gsub(x = out,
		# 	    pattern = "alpha",
		# 	    replacement = "\\alpha")

		browser()

	}
	else { 
		out <- system(paste0(prg, " -q ", " --batch-string=", shQuote(code)), intern = TRUE)
	} 
	
	# out <- gsub(pattern = "([[:print:]]*)(\\(\\%(i|o)[[:digit:]]+\\))", 
	# 		    replacement = "\\1\n\\2", 
	# 		    x = out)

	# if(options$echo == TRUE) 
	# 	if(options$engine.opts$latex == TRUE)
	# 	paste(c(code, out), collapse = "\n")
	# else
	# 	paste(out, collapse = "\n")

	# paste(c(code, out))
}
