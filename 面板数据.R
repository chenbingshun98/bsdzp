# Build panel with income, wage, age, education and several other variables
# [this is the body of the function build.psid()]

install.packages("psidR")
install.packages("data.table")
library(psidR)
library(data.table)
r = system.file(package="psidR")
f = fread(file.path(r,"psid-lists","famvars.txt"))
f
i = fread(file.path(r,"psid-lists","indvars.txt"))
i


# alternatively, use `getNamesPSID`:
# cwf <- read.xlsx("http://psidonline.isr.umich.edu/help/xyr/psid.xlsx")
# Suppose you know the name of the variable in a certain year, and it is
# "ER17013". then get the correpsonding name in another year with
# getNamesPSID("ER17013", cwf, years = 2001)  # 2001 only
# getNamesPSID("ER17013", cwf, years = 2003)  # 2003
# getNamesPSID("ER17013", cwf, years = NULL)  # all years
# getNamesPSID("ER17013", cwf, years = c(2005, 2007, 2009))   # some years

# next, bring into required shape:

i = dcast(i[,list(year,name,variable)],year~name, value.var = "variable")
i
f = dcast(f[,list(year,name,variable)],year~name, value.var = "variable")
f
?dcast
head(i)
head(f) 

?build.panel
d = build.panel(datadir=NULL,
                fam.vars=f,
                ind.vars=i, 
                heads.only = TRUE,
                sample="SRC",
                design="all")
example(build.panel)
