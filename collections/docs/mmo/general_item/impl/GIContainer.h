///
/// @file GIContainer.h
/// @author Kevin Lynx
/// @brief Abstract container implementation.
///
#ifndef ___GI_CONTAINER_H_
#define ___GI_CONTAINER_H_

#include "GIConfig.h"
#include "GIObject.h"
#include "GIContainerListener.h"
#include <map>

#define NOTIFY_LISTENER( op ) if( m_listener ) m_listener->op
#define CAST_LISTENER( c ) ((c*) m_listener )
#define NOTIFY_LISTENER_EX( c, op ) if( m_listener ) CAST_LISTENER( c )->op

namespace GI
{

    /// BaseContainer provide object manage and Move/Destroy operation.
    class BaseContainer
    {
    public:
        typedef std::map<TypeSet::IDType, Object*> ObjectMap;
    public:
        BaseContainer();

        virtual ~BaseContainer();

        void SetListener( ContainerListener *lis );

        /// Move the object specified by objID in srcCon to this.
        virtual bool Move( BaseContainer *srcCon, TypeSet::IDType objID );

        /// Move all the objects in srcCon to this.
        virtual bool MoveAll( BaseContainer *srcCon );

        /// Destroy an object in this container.
        virtual bool Destroy( TypeSet::IDType objID );

        /// Destroy all the objects in this container.
        virtual void DestroyAll();

        /// Serialize all the objects in this container.
        virtual void Serialize( GI::ByteBuffer &buf ) const;

        /// Unserialize objects.
        virtual bool UnSerialize( GI::ByteBuffer &buf );

        /// Get the object by its id.
        const Object *GetObject( TypeSet::IDType objID ) const;

        /// Get the object count in this container.
        int ObjCount() const;

        /// Traverse all the objects (const reference)
        /// void fn( TypeSet::IDType id, const Object *obj )
        template <typename T>
        void Traverse( T fn ) const;

    protected: // Only valid between containers.
        /// Add an object to this container.
        virtual bool Add( Object *obj );

        /// Remove an object from this container.
        virtual bool Remove( Object *obj );

        /// Get a writeable object.
        Object *Get( TypeSet::IDType objID );

        /// These Agent-Functions can help classes inherited from BaseContainer
        /// to visit 'con' protected functions. I hope 'future me' can
        /// understand this.
        static bool AgentAdd( BaseContainer *con, Object *obj );

        static bool AgentRemove( BaseContainer *con, Object *obj );

        static Object *AgentGet( BaseContainer *con, TypeSet::IDType objID );

        /// When add an object failed, destroy the object.
        static void AddFailed( Object *obj );
    protected:
        ObjectMap m_objs;
        ContainerListener *m_listener;
    };

    template <typename T>
    void BaseContainer::Traverse( T fn ) const
    {
        for( ObjectMap::const_iterator it = m_objs.begin();
                it != m_objs.end(); ++ it )
            fn( it->first, (const Object*) it->second );
    }

    /// FactoryContainer can create objects.
    class FactoryContainer : public BaseContainer
    {
    public:
        FactoryContainer();

        virtual ~FactoryContainer();

        /// Create an object and add it to this container.
        /// Call 'MoveAll' to move all the created objects to the dest-Container.
        virtual bool Create( TypeSet::IndexType index, Object::PListenerType *listener );

        /// Same as 'Create' but can be customised by a functor.
        /// void fn( Object *obj )
        template <typename T>
        bool Create( TypeSet::IndexType index, Object::PListenerType *listener, T fn );

    protected:
        /// Create object, but donot add it.
        Object *DoCreate( TypeSet::IndexType index, Object::PListenerType *listener );

        /// Fill properties in the object.
        bool FillProperty( TypeSet::IndexType index, Object *obj );
    };

    template <typename T>
    bool FactoryContainer::Create( TypeSet::IndexType index, Object::PListenerType *listener, T fn )
    {
        Object *obj = DoCreate( index, listener );
        if( !obj ) return false;
        fn( obj );
        if( !Add( obj ) ) 
        {
            AddFailed( obj );
            return false;
        }
        return true;
    }

    /// ModifyContainer can modify properties of an object.
    class ModifyContainer : public BaseContainer
    {
    public:
        ModifyContainer();

        virtual ~ModifyContainer();

        /// Modify the specified property value of the object.
        /// The object must be in this container.
        virtual bool Modify( TypeSet::IDType objID, TypeSet::KeyType key, TypeSet::ValueType value );
    };

    /// MergeContainer can merge/split an object.
    class MergeContainer : public BaseContainer
    {
    public:
        MergeContainer();

        virtual ~MergeContainer();

        /// Merge the two objects.
        /// If merge success, destroy obj2.
        virtual bool Merge( TypeSet::IDType obj1, TypeSet::IDType obj2 );

        /// Split the object into two objects. Call 'Add' to add the new 
        /// object into this container.
        /// This function will clone the object,except the stack count is different.
        virtual bool Split( TypeSet::IDType obj, TypeSet::StackCntType splitCnt );

        /// Decrease the stack count of the object. The decCnt must < curCnt.
        virtual bool DecStack( TypeSet::IDType obj, TypeSet::StackCntType decCnt );

    protected:
        /// Get object stack count.
        TypeSet::StackCntType GetStackCnt( Object *obj );
    };
}

#endif

