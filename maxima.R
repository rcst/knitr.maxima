
# the code of locate_over and over2frac is from Hugh:
# https://stackoverflow.com/questions/48492200/latex-expression-replacement-in-r

require(data.table) # for shift
require(TeXCheckR)  # for parse_tex
require(stringr)

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
	prg <- Sys.which("maxima")
	code <- options$code

	# remove empty lines
	mcode <- code[nchar(code) > 0]

	# replacement definitions ":=" with "="
	iapp <- which(grepl(x = mcode, pattern = ":=") == TRUE, arr.ind = TRUE)

	if(length(iapp) > 0) { 
		mcode <- append(x = mcode, values = mcode[iapp], after = iapp) 
		mcode[iapp] <- gsub(x = mcode[iapp], 
				    pattern = "([[:print:]]+):=([[:print:]])", 
				    replacement = "\\1=\\2") 
		
		# wrap each code line into tex() maintaining trailing ; or $ 
		mcode[-(iapp+1)] <- gsub(x = mcode[-(iapp+1)], 
					 pattern = "^([[:print:]]*)(;|\\$){1}$", 
					 replacement = "tex(\\1)\\2")

		# surpress output from inserted code
		mcode[iapp+1] <- gsub(x = mcode[iapp+1],
				      pattern = "^([[:print:]]+)\\;$",
				      replacement = "\\1$")

	} else {
		# wrap each code line into tex() maintaining trailing ; or $ 
		mcode <- gsub(x = mcode, 
			      pattern = "^([[:print:]]*)(;|\\$){1}$", 
			      replacement = "tex(\\1)\\2")
	}

	# join each code line into one string
	mcode <- paste0(mcode, collapse = "")

	# send code to maxima 
	out <- system(paste0(prg, " -q ", " --batch-string=", shQuote(mcode)), intern = TRUE)

	# options$engine <- "r"

	engine_output(options, code, out)
}

local({ 
	hook_old <- knitr::knit_hooks$get("source")
	knitr::knit_hooks$set(source = function(x, options) {
		hook_old(x, options)
	})

})
 
local({ 
	hook_old <- knitr::knit_hooks$get("output")
	knitr::knit_hooks$set(output = function(x, options) { 

		# split string into single output lines
		x <- unlist(str_split(string = x, pattern = "\\n"))
		x <- gsub(x = x, pattern = "#{0,2} ", replacement = "")

		# if a line doesn't start with a ( or $$ then it is to be joined with it's predecessor
		eqb <- grepl(x = x, pattern = "^\\${2}")
		eqe <- grepl(x = x, pattern = "\\${2}$")

		lbs <- rev(cumsum(rev(eqb-eqe)))

		lbs <- ifelse(lbs == 0, FALSE, TRUE)

		# handle maxima returning code with line breaks 
		# if a line in out starts with an white space then concatenate it 
		# with its predecessor
		for(i in length(lbs):2){
			if(lbs[i] == TRUE){
				x[i] <- gsub(x = x[i], pattern = "^\\s+", replacement = "")
				x[i-1] <- paste0(x[i-1], x[i])
				x[i] <- ""
			}
		}

		# count parenthesis "(" = 1, ")" = -1
		d <- str_count(string = x, pattern = "\\(") - str_count(string = x, pattern = "\\)")
		d <- rev(cumsum(rev(d)))
		lbs <- ifelse(d == 0, FALSE, TRUE)
		for(i in length(lbs):2){
			if(lbs[i] == TRUE){
				x[i] <- gsub(x = x[i], pattern = "^\\s+", replacement = "")
				x[i-1] <- paste0(x[i-1], x[i])
				x[i] <- ""
			}
		}


		x <- gsub(x = x, 
			  pattern = "^\\(%(i|o)[[:digit:]]*\\)[[:print:]]*$", 
			  replacement = "")

		# remove each line that echos input or echo marked code
		# if(options$echo == FALSE){ 
		# 	x <- gsub(x = x, 
		# 		  pattern = "^\\(%(i|o)[[:digit:]]*\\)[[:print:]]*$", 
		# 		  replacement = "")
		# } else {
		# 	# output original code lines instead of tex()-wrapped ones
		# 	# i.e. remove tex()-wrapping
		# 	x <- gsub(x = x,
		# 		    pattern = "^(\\(%i[[:digit:]]+\\)) tex\\(([[:print:]]+)\\)",
		# 		    replacement = "\\1 \\2")

		# 	x <- gsub(x = x, 
		# 		  pattern = "^\\(%o[[:digit:]]*\\)[[:print:]]*$", 
		# 		  replacement = "")

		# 	# out <- gsub(x = out,
		# 	# 	    pattern = "^(\\(%i[[:digit:]]*\\)[[:print:]]*)$", 
		# 	# 	    replacement = "```\\1```")


		# 	# handle when tex decides too verbatim a command, i.e. when it contains ":="

		# 	# handle too long lines
		# }

		# remove empty lines
		x <- x[nchar(x) > 0]

		# x <- gsub(x = x, pattern = "^\\${2}", replacement = "\n\\$\\$")
		# x <- gsub(x = x, pattern = "\\${2}$", replacement = "\\$\\$\n")

		# replace \over with \frac commands
		x <- over2frac(lines = paste(x, collapse = "\n"))

		# browser()

		# wrapp each TeX line in \begin{plain}

		# x <- gsub(x = x,
		# 	  pattern = "^\\${2}([[:print:]]*)\\${2}$",
		# 	  replacement = "$$\\\\begin{plain}\\1\\\\end{plain}$$")

		# replace TeX command \it with \textit{}
		x <- gsub(x = x,
			  pattern = "\\{[[:print:]]+\\it([[:print:]]*)\\}",
			  replacement = "\\\\textit\\{\\1\\}")

		x <- paste(x, collapse = "\n")

		# this causes the everything to get wrapped in a code chunk
		# h <- hook_old(x, options)
	})
})
