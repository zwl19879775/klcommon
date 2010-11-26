///
/// @file Serializer.h
///
///
#ifndef ___SERIALIZER_H_
#define ___SERIALIZER_H_

#include "../../GIForwardRefs.h"
#include "../TypesDef.h"

class BaseCellContainer;
class SubContainer;

namespace GIAdapter
{
    /// Serialize goods to game client.
    bool SerializeObj( const GI::Object *obj, GI::ByteBuffer &buf );

    /// Serialize goods to client region, only for region display.
    bool SerializeGeneral( const GI::Object *obj, GI::ByteBuffer &buf );

    /// Serialize extend dynamic properties.
    bool SerializeExtDynamic( const GI::Object *obj, GI::ByteBuffer &buf );

    /// Serialize goods property value.
    bool SerializeValue( const TypeSet::ValueType &val, GI::ByteBuffer &buf );

    /// Serialize a cell container to game client. 
    bool SerializeContainer( const BaseCellContainer *con, GI::ByteBuffer &buf );

    /// Serialize sub container and these goods in the sub container.
    bool SerializeSubContainer( const SubContainer *con, GI::ByteBuffer &buf );

    bool UnSerializeExtDynamic( GI::Object *obj, GI::ByteBuffer &buf );
}

#endif
