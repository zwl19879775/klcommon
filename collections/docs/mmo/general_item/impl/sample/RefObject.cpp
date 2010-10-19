///
/// @file RefObject.cpp
///
///
#include "RefObject.h"
#include "GoodsPropertyType.h"

void RefObject::RefTo( const GI::Object *obj )
{
    id = TypeSet::ValueType::ToID( obj->GetValue( KeySet::IDKey ) );
    index = TypeSet::ValueType::ToLong( obj->GetValue( KeySet::IndexKey ) );
    maxStackCnt = TypeSet::ValueType::ToLong( obj->GetValue( KeySet::MaxStackCntKey ) );
    stackCnt = TypeSet::ValueType::ToLong( obj->GetValue( KeySet::StackCntKey ) );
    pos = TypeSet::ValueType::ToLong( obj->GetValue( TypeSet::KeyType( PCELL_POS ) ) );
}

