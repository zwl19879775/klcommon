///
/// @file ValueGenerator.h
/// @brief To generate goods property values.
///
#ifndef ___VALUE_GENERATOR_H_
#define ___VALUE_GENERATOR_H_

#include "TypeDef.h"

template <long defVal>
TypeSet::ValueType GenDefLongVal( GI::Object *obj )
{
    return TypeSet::ValueType( defVal );
}

// more generators...

#endif

