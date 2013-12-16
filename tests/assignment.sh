### Basic assignment
var=foo
var=bar
###
var=bar

### Multiple assignments
var1=foo var2=bar
###
var1=foo
var2=bar

### Local assignment
var=foo pwd
###
# empty

### Argument, not assignment
echo var=foo
###
# empty

### Empty assignment
var1= var2=bar
###
var1=
var2=bar

### Strange names
_0z=foo
0var=bar # not an assignment
###
_0z=foo

### Compound assignment
var=foo var+=bar
###
var=foobar

### Assignment builtins
export var1=foo
export var2=(foo bar)
###
var1=foo
var2=(foo bar)
