///
/// @file SharedPropertyArray.h
/// @author Kevin Lynx
/// @brief Wrap a simple array operations, used in shared properties access.
///
#ifndef ___SHARED_PROPERTY_ARRAY_H_
#define ___SHARED_PROPERTY_ARRAY_H_

/// Usage: DECLARE_SHAREDPRO_ARRAY (_s) = { "hello", "world", 0 };
#define DECLARE_SHAREDPRO_ARRAY(var) \
    static const char* var[] 

/// Usage: SharedProArray func () { return _s; }, and _s is declared as a shared properties array.
typedef const char** SharedProArray;

/// Traverse a shared property array, which implemented as SharedProArray.
#define TRAVERSE_SHAREDPRO_ARRAY(lst, exp) \
    do { \
        const SharedProArray &__lst = lst; \
        for (int i = 0; __lst && __lst[i]; ++i) { \
            exp; \
        } \
    } while(0)

/// Shared array item value
#define SA_VALUE (__lst[i])

#endif

