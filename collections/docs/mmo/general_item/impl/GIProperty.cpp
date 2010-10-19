///
/// @file GIProperty.cpp
/// @author Kevin Lynx
///
#include "GIProperty.h"

namespace GI
{
    PropertyTypeSet::PropertyTypeSet() : SelfType()
    {
    }

    void PropertyTypeSet::Add( KeyType key, int type, PropertyType::GenValFunc func )
    {
        PropertyType val( type, func );
        AddProperty( key, val );
    }

    int PropertyTypeSet::GetType( KeyType key )
    {
        PropertyType val = GetValue( key );
        return val.type;
    }

    TypeSet::ValueType PropertyTypeSet::GenValue( KeyType key, Object *obj )
    {
        PropertyType val = GetValue( key );
        if( val.func && IS_DYNAMIC( val.type ) )
        {
            return val.func( obj );
        }
        return TypeSet::ValueType();
    }
}

