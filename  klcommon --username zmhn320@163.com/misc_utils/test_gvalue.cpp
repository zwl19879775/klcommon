///
/// @file test_gvalue.cpp
/// @brief to test gvalue.h/cpp
/// @author Kevin Lynx
/// 
#include <stdio.h>
#include "gvalue_util.h"

int main () {
    GValue val = CreateGValue (1.0);
    GValue val2 = val;
    printf ("%g\n", val2.GetNumber());
    val2.Set (true);
    printf ("t: %d, %g\n", val2.Type(), val.GetNumber());

    val = CreateGValue ("Hello world");
    printf ("%s\n", CastString (val).c_str());
    val2 = val;
    printf ("%p, %p\n", val.GetRaw()->p, val2.GetRaw()->p);
    return 0;
}

