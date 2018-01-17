# This is the main script.

# here, you set the path, load data
# create a dataframe and anything elese that is specific 
# to your system, computer and operation system
path <- 'my cool path'
df <- 'read.table(bla bla bla)'

# you will have to change this your paths etc.
setwd('/home/mirko/Dropbox/Lectures/Datenmanagement/datacourse2018/utils')
source('generic_functions.R')

# now we can use the function here, without re-defining it.
genericFunction('wrong', 'input')

# correct usage
genericFunction(5, 7)
