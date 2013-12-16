### Conditionals
[ ! "foo" == "bar" ] && var1=pass
[ ] || var2=pass
###
var1=pass
var2=pass

### Bash conditionals
foo="foo bar"
[[ $foo == "foo bar" ]] && var1=pass
###
foo="foo bar"
var1=pass
