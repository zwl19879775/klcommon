///
/// @file GIContainer.h
/// @author Kevin Lynx
/// @brief Abstract container implementation.
///
#ifndef ___GI_CONTAINER_H_
#define ___GI_CONTAINER_H_

#include "GIConfig.h"
#include <map>

namespace GI
{
    class Object;
    class ContainerListener;

    /// BaseContainer provide object manage and Move/Destroy operation.
    class BaseContainer
    {
    public:
        typedef std::map<TypeSet::IDType, Object*> ObjectMap;
    public:
        BaseContainer();

        virtual ~BaseContainer();

        void SetListener( ContainerListener *lis );

        /// Move the object specified by objID to the dest-Container.
        virtual bool Move( BaseContainer *destCon, TypeSet::IDType objID );

        /// Move all the objects to the dest-Container.
        virtual bool MoveAll( BaseContainer *destCon );

        /// Destroy an object in this container.
        virtual bool Destroy( TypeSet::IDType objID );

        /// Destroy all the objects in this container.
        virtual void DestroyAll();

        /// Get the object by its id.
        const Object *Get( TypeSet::IDType objID ) const;

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
    };

    template <typename T>
    bool FactoryContainer::Create( TypeSet::IndexType index, Object::PListenerType *listener, T fn )
    {
        Object *obj = Create( index, listener );
        if( !obj ) return false;
        fn( obj );
        if( m_listener ) m_listener->OnCreate( obj );
        Add( obj );
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
        virtual bool Modify( TypeSet::IDType objID, TypeSet::KeyType key, TypeSet::KeyType value );
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
        virtual bool Split( TypeSet::IDType obj, int splitCnt );

        /// Decrease the stack count of the object. The decCnt must < curCnt.
        virtual bool DecStack( TypeSet::IDType obj, int decCnt );

    protected:
        /// Get object stack count.
        int GetStackCnt( Object *obj );
    };
}

#endif

