### Function definition
foo () {
    var=bar
}
###
# empty

### Function execution
var=

foo () {
    var+=bar
}

foo
foo
###
var=barbar

### Nested functions
function bar {
    function baz () {
        var1=bar
    }
}

baz && var2=fail
bar
baz && var3=pass
var4=$var1
unknown && baz
###
var3=pass
var4=bar
