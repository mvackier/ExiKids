library(Ryacas)

x <- Sym("x")
fn <- x^2 + 5*x - 8
TeXForm(fn)
PrettyForm(fn)
PrettyPrinter(fn)
print(fn)
# https://www.r-bloggers.com/using-r-as-a-computer-algebra-system-with-ryacas/
  
as.R <- function(f) {
  eval(parse(text = paste0("function(x) ",as.character(f))))
} 

rfn <- as.R(fn)

x <- Sym("x")
x*x
Integrate(x*x, x)
