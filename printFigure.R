print.figure <- function(writeto,filename,placement="H",caption,label)
{
  sink(writeto)
  cat("\\begin{figure}[",placement,"]\n",sep="")
  cat("\\begin{center}\n",sep="")
  cat("\\includegraphics{",filename,"}\n",sep="")
  cat("\\end{center}\n",sep="")
  if(!missing(caption)) {cat("\\caption{", caption,"}\n",sep="")}
  if(!missing(label)) {cat("\\label{", label,"}\n",sep="")}
  cat("\\end{figure}\n",sep="")
  sink()
  system(paste("cat",writeto))
}