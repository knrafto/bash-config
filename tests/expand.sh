### parameter expansion
var1=foo
var2=$var1
###
var1=foo
var2=foo

### Word splitting
var="foo == foo"
if test $var; then var=pass; else var=fail; fi
###
var=pass

### Expand to empty words
var1=pass $foo
var2=fail "$foo"
###
var1=pass

### Tilde expansion
var=~fail
###
# empty
