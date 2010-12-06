///
/// @file GIContainer.cpp
/// @author Kevin Lynx
/// 
#include "GIContainer.h"
#include "GIObject.h"
#include "GIContainerListener.h"
#include "GIObjCreator.h"

namespace GI
{
    struct Serializer
    {
        Serializer( GI::ByteBuffer &buf ) : m_buf( buf ) { }

        void operator() ( TypeSet::IDType id, const Object *obj )
        {
            obj->Serialize( m_buf );
        }

        GI::ByteBuffer &m_buf;
    };

    BaseContainer::BaseContainer()
    {
        m_listener = NULL;
    }

    BaseContainer::~BaseContainer()
    {
        DestroyAll();
    }

    void BaseContainer::SetListener( ContainerListener *lis )
    {
        m_listener = lis;
    }

    int BaseContainer::ObjCount() const
    {
        return (int) m_objs.size();
    }

    bool BaseContainer::Move( BaseContainer *srcCon, TypeSet::IDType objID )
    {
        Object *obj = srcCon->Get( objID );
        if( !obj ) return false;
        if( !Add( obj ) ) return false;
        srcCon->Remove( obj );
        NOTIFY_LISTENER( OnMoved( srcCon, this, obj ) );
        return true;
    }

    bool BaseContainer::MoveAll( BaseContainer *srcCon )
    {
        bool ret = true;
        for( ObjectMap::iterator it = srcCon->m_objs.begin();
                it != srcCon->m_objs.end(); )
        {
            bool addRet = Add( it->second );
            if( addRet && srcCon->m_listener )
            {
                srcCon->m_listener->OnRemove( srcCon, it->second );
                srcCon->m_objs.erase( it++ );
            }
            else ++ it;
            ret = addRet && ret;
            NOTIFY_LISTENER( OnMoved( srcCon, this, it->second ) );
        }
        return ret;
    }

    bool BaseContainer::Destroy( TypeSet::IDType objID )
    {
        Object *obj = Get( objID );
        if( !obj ) return false;
        Remove( obj );
        SINGLETON( ObjCreator ).Destroy( obj );
        return true;
    }

    void BaseContainer::DestroyAll()
    {
        size_t size = m_objs.size();
        for( size_t i = 0; i < size; ++ i )
        {
            ObjectMap::iterator it = m_objs.begin();
            Object *obj = it->second;
            Remove( obj );
            SINGLETON( ObjCreator ).Destroy( obj );
        }
    }

    void BaseContainer::Serialize( GI::ByteBuffer &buf ) const
    {
        buf.Push( ObjCount() );
        Traverse( Serializer( buf ) );
    }

    bool BaseContainer::UnSerialize( GI::ByteBuffer &buf )
    {
        DestroyAll();
        long cnt;
        if( !buf.Pop( &cnt ) ) return false;
        for( long i = 0; i < cnt; ++ i )
        {
            Object *obj = SINGLETON( ObjCreator ).Create();
            obj->UnSerialize( buf );
            SINGLETON( ObjCreator ).SetProto( obj );
            if( !Add( obj ) ) AddFailed( obj );
        }
        return true;
    }

    const Object *BaseContainer::GetObject( TypeSet::IDType objID ) const
    {
        ObjectMap::const_iterator it = m_objs.find( objID );
        return it == m_objs.end() ? NULL : it->second;
    }

    Object *BaseContainer::Get( TypeSet::IDType objID )
    {
        ObjectMap::iterator it = m_objs.find( objID );
        return it == m_objs.end() ? NULL : it->second;
    }

    bool BaseContainer::Add( Object *obj )
    {
        NOTIFY_LISTENER( OnAdd( this, obj ) );
        TypeSet::ValueType idVal = obj->GetValue( KeySet::IDKey );
        TypeSet::IDType id = TypeSet::ValueType::ToID( idVal );
        m_objs[id] = obj; 
        return true;
    }

    bool BaseContainer::Remove( Object *obj )
    {
        NOTIFY_LISTENER( OnRemove( this, obj ) );
        TypeSet::ValueType idVal = obj->GetValue( KeySet::IDKey );
        TypeSet::IDType id = TypeSet::ValueType::ToID( idVal );
        m_objs.erase( id );
        return true;
    }

    bool BaseContainer::AgentAdd( BaseContainer *con, Object *obj )
    {
        return con->Add( obj );
    }

    bool BaseContainer::AgentRemove( BaseContainer *con, Object *obj )
    {
        return con->Remove( obj );
    }

    Object *BaseContainer::AgentGet( BaseContainer *con, TypeSet::IDType id )
    {
        return con->Get( id );
    }

    void BaseContainer::AddFailed( Object *obj )
    {
        SINGLETON( ObjCreator ).Destroy( obj );
    }
    ///////////////////////////////////////////////////////////////////////////
    FactoryContainer::FactoryContainer()
    {
    }

    FactoryContainer::~FactoryContainer()
    {
    }

    bool FactoryContainer::Create( TypeSet::IndexType index, Object::PListenerType *listener )
    {
        Object *obj = DoCreate( index, listener );
        if( !obj ) return false;
        if( !Add( obj ) )
        {
            AddFailed( obj );
            return false;
        }
        return true;
    }

    Object *FactoryContainer::DoCreate( TypeSet::IndexType index, Object::PListenerType *listener )
    {
        Object *obj = SINGLETON( ObjCreator ).Create();
        if( !FillProperty( index, obj ) )
        {
            SINGLETON( ObjCreator ).Destroy( obj );
            obj = NULL;
        }
        return obj;
    }

    bool FactoryContainer::FillProperty( TypeSet::IndexType index, Object *obj )
    {
        return SINGLETON( ObjCreator ).FillProperty( index, obj );
    }
    ///////////////////////////////////////////////////////////////////////////
    ModifyContainer::ModifyContainer()
    {
    }

    ModifyContainer::~ModifyContainer()
    {
    }

    bool ModifyContainer::Modify( TypeSet::IDType objID, TypeSet::KeyType key, TypeSet::ValueType value )
    {
        Object *obj = Get( objID );
        if( !obj ) return false;
        obj->SetValue( key, value );
        return true;
    }
    ///////////////////////////////////////////////////////////////////////////
    MergeContainer::MergeContainer()
    {
    }

    MergeContainer::~MergeContainer()
    {
    }

    bool MergeContainer::Merge( TypeSet::IDType objID1, TypeSet::IDType objID2 )
    {
        Object *obj1 = Get( objID1 );
        Object *obj2 = Get( objID2 );
        if( !obj1 || !obj2 ) return false;
        if( obj1->GetValue( KeySet::IndexKey ) !=
            obj2->GetValue( KeySet::IndexKey ) ) return false;

        TypeSet::ValueType maxStack = obj1->GetValue( KeySet::MaxStackCntKey );
        TypeSet::StackCntType maxSCnt = TypeSet::ValueType::ToStackCnt( maxStack );

        TypeSet::StackCntType sCnt1 = GetStackCnt( obj1 );
        TypeSet::StackCntType sCnt2 = GetStackCnt( obj2 );

        if( sCnt1 + sCnt2 > maxSCnt ) return false;
        obj1->SetValue( KeySet::StackCntKey, TypeSet::ValueType( sCnt1 + sCnt2 ) );
        Destroy( objID2 );

        return true;
    }

    bool MergeContainer::Split( TypeSet::IDType objID, TypeSet::StackCntType splitCnt )
    {
        Object *obj = Get( objID );
        if( !obj ) return false;
        TypeSet::StackCntType curCnt = GetStackCnt( obj );
        if( splitCnt >= curCnt ) return false;

        TypeSet::StackCntType retCnt = curCnt - splitCnt;
        obj->SetValue( KeySet::StackCntKey, TypeSet::ValueType( retCnt ) );
        Object *newObj = SINGLETON( ObjCreator ).Clone( obj );
        newObj->SetValue( KeySet::StackCntKey, TypeSet::ValueType( splitCnt ) );
        if( !Add( newObj ) )
        {
            AddFailed( newObj );
            return false;
        }
        return true;
    }

    bool MergeContainer::DecStack( TypeSet::IDType objID, TypeSet::StackCntType decCnt )
    {
        Object *obj = Get( objID );
        if( !obj ) return false;
        TypeSet::StackCntType curCnt = GetStackCnt( obj );
        if( curCnt <= decCnt ) return false;
        TypeSet::StackCntType retCnt = curCnt - decCnt;
        obj->SetValue( KeySet::StackCntKey, TypeSet::ValueType( retCnt ) );
        return true;
    }

    TypeSet::StackCntType MergeContainer::GetStackCnt( Object *obj )
    {
        TypeSet::ValueType sVal = obj->GetValue( KeySet::StackCntKey );
        return TypeSet::ValueType::ToStackCnt( sVal );
    }
}

