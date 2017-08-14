# This script will set a variable in your operating system. 
# This tells R to save packages in "D:\" where you have permissions, rather than "C:\"

loc <- "D://R"

Sys.setenv(R_LIBS = loc)
Sys.setenv(R_LIBS_USER = loc)
