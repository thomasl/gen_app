## The intent is that we pass in Makefile variables
## which are strings listing the modules
##  and print out an erlang-style list

## For testing, see what comes in

print "[".join(",", @ARGV)."]";
