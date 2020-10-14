# the code of locate_over and over2frac is from Hugh:
# https://stackoverflow.com/questions/48492200/latex-expression-replacement-in-r

require(data.table) # for shift
require(TeXCheckR)  # for parse_tex

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

	if(options$engine.opts$latex == TRUE) {
		lc <- options$code

		# # delete trailling ";" or "$"
		# latex_code <- gsub(x = options$code,
		# 		   pattern = "(\\$|;)$",
		# 		   replacement = "")

		# remove empty lines
		lc <- lc[nchar(lc) > 0]

		# wrap each code line into tex() maintaining trailing ; or $ 
		lc <- gsub(x = lc,
				   pattern = "^([[:print:]]*)(;|\\$){1}$",
				   replacement = "tex(\\1)\\2")

		# join each code line into one string
		lc <- paste0(lc, collapse = "")

		# send code to maxima 
		out <- system(paste0(prg, " -q ", " --batch-string=", shQuote(lc)), intern = TRUE)

		# handle maxima returning code with line breaks
		# if a line in out starts with an white space then concatenate it
		# with its predecessor
		lbs <- grepl(x = out, 
			     pattern = "^\\s+")

		for(i in length(lbs):2){
			if(lbs[i] == TRUE){
				out[i] <- gsub(x = out[i], pattern = "^\\s+", replacement = "")
				out[i-1] <- paste0(out[i-1], out[i])
				out[i] <- ""
			}
		}

		# remove each line that echos input or echo marked code
		if(options$echo == FALSE){ 
			out <- gsub(x = out, 
				    pattern = "^\\(%(i|o)[[:digit:]]*\\)[[:print:]]*$", 
				    replacement = "")
		} else {
			# output original code lines instead of tex()-wrapped ones
			# i.e. remove tex()-wrapping
			out <- gsub(x = out,
				    pattern = "^(\\(%i[[:digit:]]+\\)) tex\\(([[:print:]]+)\\)",
				    replacement = "\\1 \\2")

			out <- gsub(x = out,
				    pattern = "^\\(%o[[:digit:]]*\\)[[:print:]]*$", 
				    replacement = "")

			out <- gsub(x = out,
				    pattern = "^(\\(%i[[:digit:]]*\\)[[:print:]]*)$", 
				    replacement = "```\\1```")


			# handle when tex decides too verbatim a command, i.e. when it contains ":="

			# handle too long lines


		}

		# remove empty lines
		out <- out[nchar(out) > 0]

		# replace \over with \frac commands
		out <- over2frac(lines = paste(out, collapse = "\n"))
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

	# browser()

	engine_output(options, code = "1+1", out = "[1] 2")
	out
}
