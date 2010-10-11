///
/// @file GIObjectFactory.cpp
/// @author Kevin Lynx
#include "GIObjectFactory.h"
#include "GIObjectProto.h"

namespace GI
{
    // Used to add properties to an object.
    struct Creator
    {
        Creator( Object *obj ) : m_obj( obj ) { }

        void operator() ( ObjectProto::KeyType key, ObjectProto::ValueType val ) 
        {
            PropertyTypeSet &pts = PropertyTypeSet::getSingleton();
            int t = pts.GetType( key );
            if( IS_DYNAMIC( t ) )
            {
                val = pts.GenValue( key, NULL );
                m_obj->AddProperty( key, val );
            }
        }
        Object *m_obj;
    };

    struct Destroyer
    {
        void operator() ( TypeSet::KeyType key, Object *obj )
        {
            delete obj;
        }
    };

    static void DelObjs( std::vector<Object*> &objs )
    {
        for( std::vector<Object*>::iterator it = objs.begin();
                it != objs.end(); ++ it )
        {
            delete it->second;
        }
        objs.clear();
    }

    ObjectFactory::ObjectFactory( ObjProtoFactory *proto ) :
        SelfType( NULL ), m_protoFac( proto )
    {
    }

    ObjectFactory::~ObjectFactory()
    {
        DestroyAll();
    }

    Object *ObjectFactory::Create( TypeSet::IndexType index, Object::PListenerType *listener )
    {
        const ObjectProto *proto = m_protoFac->GetProto( index );
        if( !proto ) return NULL;
        Object *obj = new Object( listener );
        obj->SetProto( proto );
        proto->Traverse( Creator( obj ) );
        return obj;
    }

    Object *ObjectFactory::Create( ByteBuffer &buf, Object::PListenerType *listener )
    {
        // the buffer include basic and dynamic properties of an object.
        Object *obj = new Object( listener );
        obj->UnSerializeBasic( buf );
        Object::ValueType indexVal = obj->GetValue( KeySet::IndexKey );
        // type convert.
        TypeSet::IndexType index = TypeSet::ValueType::ToIndex( indexVal );
        const ObjectProto *proto = m_protoFac->GetProto( index );
        if( !proto )
        {
            // fatal error
            delete obj;
            return NULL;
        }
        obj->SetProto( proto );
        obj->UnSerialize( buf );
        return obj;
    }

    std::vector<Object*> ObjectFactory::Create( TypeSet::IndexType index, int count, Object::PListenerType *listener )
    {
        std::vector<Object*> objs;
        const ObjectProto *proto = m_protoFac->GetProto( index );
        if( !proto ) return objs;
        TypeSet::ValueType stackCntVal = proto->GetValue( KeySet::StackCntKey );
        int maxStack = (int) TypeSet::ValueType::ToStackCnt( stackCntVal );
        int createCnt = count > maxStack ? maxStack : count;
        count -= createCnt;
        do
        {
            Object *obj = Create( index, listener );
            if( obj ) 
            {
                objs.push_back( obj );
                obj->SetValue( KeySet::StackCntKey, TypeSet::ValueType( createCnt ) );
                createCnt = count > maxStack ? maxStack : count;
                count -= createCnt;
            }
            else
            {
                // delete all the created objects.
                DelObjs( objs );
                return objs;
            }
        }while( count > 0 );

        return objs;
    }

    void ObjectFactory::Destroy( Object *obj )
    {
        Object::ValueType idVal = obj->GetValue( KeySet::IDKey );
        // type convert.
        TypeSet::IDType id = TypeSet::ValueType::ToID( idVal );
        RemoveProperty( id );
        delete obj;
    }

    void ObjectFactory::DestroyAll()
    {
        Traverse( Destroyer() );
        Clear();
    }
}

