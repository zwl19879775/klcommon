///
/// @file GIContainerListener.h
/// @author Kevin Lynx
/// @brief Listen on container operations.
///
#ifndef ___GI_CONTAINER_LISTENER_H_
#define ___GI_CONTAINER_LISTENER_H_

#include "GIConfig.h"

namespace GI
{
    class Object;
    class BaseContainer;

    class ContainerListener
    {
    public:
        ContainerListener( BaseContainer *con ) : m_con( con ) { }

        virtual ~ContainerListener() { }

        /// NOTE: Deprecated method.
        /// An object has been created.
        virtual void OnCreate( const Object *obj ) { }

        /// NOTE: Deprecated method.
        /// An object will be destroyed.
        virtual void OnDestroy( const Object *obj ) { }

        /// An object was added to this container.
        virtual void OnAdd( const Object *obj ) { }

        /// An object will be removed from this container.
        virtual void OnRemove( const Object *obj ) { }

        /// NOTE: Deprecated method.
        /// An object in this container will be modified properties.
        virtual void OnModify( const Object *obj, TypeSet::KeyType key, TypeSet::ValueType newVal ) { }

    protected:
        BaseContainer *m_con;
    };    
}

#endif

