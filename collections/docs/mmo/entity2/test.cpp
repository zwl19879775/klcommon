///
/// 
///
#include <stdio.h>
#include "SharedPropertyArray.h"

DECLARE_SHAREDPRO_ARRAY (_s) = {
    "hello",
    "world",
    0
};

SharedProArray get_shared () {
    return _s;
}

int main () {
    const SharedProArray &arr = get_shared ();
    TRAVERSE_SHAREDPRO_ARRAY (arr, printf ("%s\n", SA_VALUE));
    return 0;
}

