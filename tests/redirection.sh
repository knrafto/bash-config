### Basic redirections
cat < /dev/null
[ "foo" != "bar" ] && var1=pass
###
var1=pass

### Redirection numbers
test "foo" != "bar" 15>/dev/null && var1=pass
test "foo" != 15 >/dev/null && var2=pass
###
var1=pass
var2=pass

### Compound commands
{ cd; } <>/dev/null
###
# empty

### Heredocs
cat <<"EOF1"; cat <<-EOF2
var1=baz
EOF1
var2=fail
EOF2 fake
var3=fail
# tab, not spaces
	EOF2
var4=pass
###
var4=pass
