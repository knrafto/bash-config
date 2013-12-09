### Single quotes
var='${ `<(\'}
###
var="\${ \`<(\\}"

### Double quotes
var=" \"<('"
###
var=' "<('\'

### ANSI C quoting
var1=$' \' '
var2="$' '"
###
var1=" ' "
var2="$' '"

### Locale quoting
var=$"foo"
###
var="foo"

### Mixed quotes
var=foo'bar'"baz"$'quux'$"corge"grault
###
var=foobarbazquuxcorgegrault
