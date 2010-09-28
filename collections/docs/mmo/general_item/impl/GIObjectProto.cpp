///
/// @file GIObjectProto.cpp
/// @author Kevin Lynx
///
#include "GIObjectProto.h"

namespace GI
{
    ObjProtoFactory::ObjProtoFactory( ProtoLoader *loader ) :
        SelfType( NULL ), m_loader( loader )
    {
    }

    ObjProtoFactory::~ObjProtoFactory()
    {
        Release();
    }

    bool ObjProtoFactory::Load( void *u )
    {
        return m_loader->Load( this, u );
    }

    void ObjProtoFactory::Release()
    {
        for( TableType::iterator it = m_properties.begin();
                it != m_properties.end(); ++ it )
        {
            delete it->second;
        }
        Clear();
    }

    const ObjectProto *ObjProtoFactory::GetProto( TypeSet::IndexType index ) const
    {
        return GetValue( index );
    }
}

