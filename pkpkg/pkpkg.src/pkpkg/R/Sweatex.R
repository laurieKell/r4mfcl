Sweatex <- 
function (filename, extension = "Rnw", command = "pdflatex",
silent = FALSE, preview = FALSE, bibtex = FALSE)
{
latex.path <- dirname(options("latexcmd")[[1]])
path <- as.character(Sys.getenv("PATH"))
if (regexpr(latex.path, path) == -1) {
Sys.putenv(PATH = paste(path, ":", latex.path, sep = ""))
}
if (command == "latex")
command = "simpdftex latex --maxpfb"
extension <- paste(".", extension, sep = "")
Sweave(paste(filename, extension, sep = ""))
if (bibtex) {
system(paste(command, " ", filename, sep = ""), intern = silent)
system(paste("bibtex", " ", filename, sep = ""), intern = silent)
system(paste(command, " ", filename, sep = ""), intern = silent)
}
else system(paste(command, " ", filename, sep = ""), intern = silent)
if (preview) {
system(paste(options("pdfviewer")[[1]], " ", filename,
".pdf", sep = ""))
}
}
