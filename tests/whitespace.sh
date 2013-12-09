### End-of-line comment
var1=foo # ; var2=bar
###
var1=foo

### Leading whitespace (including a tab)
    	var=foo
###
var=foo

### Escaped newline
pwd \
var=quux
###
# empty

### Not a comment
var1=corge# var2=grault
###
var1=corge#
var2=grault
