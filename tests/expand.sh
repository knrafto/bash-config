### Brace expansion
var1=( a{c,b,d}e )
var2=( {a..e} )
var3=( {01..10..2} )
###
var1=(ace abe ade)
var2=(a b c d e)
var3=(01 03 05 07 09)

### Parameter expansion
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
foo=
var1=pass $foo
var2=fail "$foo"
###
foo=
var1=pass

### Tilde expansion
var=~fail
###
# empty
