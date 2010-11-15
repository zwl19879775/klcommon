///
/// @file GIObjCreator.cpp
/// @author Kevin Lynx
/// @date 11.10.2010
///
#include "GIObjCreator.h"
#include "GIObject.h"
#include "GIObjectProto.h"

#define NOTIFY_LISTENER( c ) if( m_listener ) m_listener->c

namespace GI
{
    Object *ObjCreator::Create()
    {
        Object *obj = new Object( NULL );        
        NOTIFY_LISTENER( OnCreate( obj ) );
        return obj;
    }

    Object *ObjCreator::Clone( const Object *srcObj )
    {
        Object *obj = new Object( NULL );
        srcObj->Clone( obj );
        NOTIFY_LISTENER( OnClone( srcObj, obj ) );
        return obj;
    }

    void ObjCreator::Destroy( Object *obj )
    {
        NOTIFY_LISTENER( OnDestroy( obj ) );
        delete obj;
    }

    struct Creator
    {
        Creator( Object *obj ) : m_obj( obj ) { }

        bool operator() ( ObjectProto::KeyType key, ObjectProto::ValueType val ) 
        {
            PropertyTypeSet &pts = SINGLETON( PropertyTypeSet );
            int t = pts.GetType( key );
            if( IS_DYNAMIC( t ) )
            {
                val = pts.GenValue( key, m_obj );
                m_obj->AddProperty( key, val );
            }
            else if( IS_INDEX( t ) )
            {
                m_obj->AddProperty( key, val );
            }
            return false;
        }
        Object *m_obj;
    };
    
    bool ObjCreator::FillProperty( TypeSet::IndexType index, Object *obj )
    {
        const ObjectProto *proto = SINGLETON( ObjProtoFactory ).GetProto( index );
        if( !proto ) return false;
        obj->SetProto( proto );
        proto->Traverse( Creator( obj ) );
        return true;
    }

    bool ObjCreator::SetProto( Object *obj )
    {
        TypeSet::ValueType indexVal = obj->GetValue( KeySet::IndexKey );
        TypeSet::IndexType index = TypeSet::ValueType::ToIndex( indexVal );
        const ObjectProto *proto = SINGLETON( ObjProtoFactory ).GetProto( index );
        if( !proto ) return false;
        obj->SetProto( proto );
        return true;
    }
}

