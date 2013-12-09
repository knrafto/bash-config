### Decidable if
if true; then
    var=pass
else
    var=fail
fi
###
var=pass

### Unknown paths
var1=fail
var2=fail

if false; then
    unknown
elif unknown; then
    var1=reset
elif true; then
    var2=reset
fi
###
# empty

### Predicates
if var1=pass; var2=fail; unknown; then
    nothing
elif var2=reset; then
    nothing
fi

###
var1=pass
