### Until and while
var1=fail
var2=fail
var3=fail
var4=fail

until var1=reset; do
    unknown
    var2=reset
done

while var3=reset; do
    unknown
    var4=reset
done
###
# empty

### For
var1=fail
var2=fail
var3=fail
var4=fail

for name do
    unknown
    var1=reset
done

for name; do
    unknown
    var2=reset
done

for name in; do
    unknown
    var3=reset
done

for name in word1 word2; do
    unknown
    var4=reset
done
###
# empty

### Case
var1=fail
var2=fail

case word in
    pat1|pat2)
        unknown
        var1=reset
        ;;&
    (pat3|pat4) var2=reset ;&
    default)
      ;;
esac
###
# empty

### Select
var1=fail
var2=fail

select name; do
    unknown
    var1=reset
done

select name in word1 word2; do
    unknown
    var2=reset
done
###
# Empty environment
