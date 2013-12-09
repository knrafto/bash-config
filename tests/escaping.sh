### Escaped newline
var=foo\
bar
###
var=foobar

### Escaped delimiters
var1=foo\ bar
var2=foo\"bar
###
var1='foo bar'
var2='foo"bar'

### Escaped special characters
var=\${foo}
###
var='${foo}'
