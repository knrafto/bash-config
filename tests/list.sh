### &&
true && var1=pass
false && var2=fail
var3=fail
unknown && var3=reset
###
var1=pass

### ||
false || var1=pass
true || var2=fail
var3=fail
unknown || var3=reset
###
var1=pass

### Pipelines
! false && var1=pass
! || var2=pass
time ! true | unknown | false && var3=pass
###
var1=pass
var2=pass
var3=pass

### Groups
{ false; unknown; var1=pass; true; } && var2=pass
###
var1=pass
var2=pass

### Subshells
( false; unknown; var1=fail; true ) && var2=pass
###
var2=pass
