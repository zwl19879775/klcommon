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
    class MergeContainer;

    class ContainerListener
    {
    public:
        ContainerListener() { }

        virtual ~ContainerListener() { }

        /// Attach this to a container, means listen on the container.
        void Attach( BaseContainer *con );

        /// Detach the listener.
        void Detach( BaseContainer *con );

        /// NOTE: Deprecated method.
        /// An object has been created.
        virtual void OnCreate( BaseContainer *con, const Object *obj ) { }

        /// NOTE: Deprecated method.
        /// An object will be destroyed.
        virtual void OnDestroy( BaseContainer *con, const Object *obj ) { }

        /// An object was added to this container.
        virtual void OnAdd( BaseContainer *con, const Object *obj ) { }

        /// An object will be removed from this container.
        virtual void OnRemove( BaseContainer *con, const Object *obj ) { }

        /// An move operation has been finished.
        virtual void OnMoved( BaseContainer *srcCon, BaseContainer *destCon,
                const Object *obj ) { }

        /// NOTE: Deprecated method.
        /// An object in this container will be modified properties.
        virtual void OnModify( BaseContainer *con, const Object *obj, 
                TypeSet::KeyType key, TypeSet::ValueType newVal ) { }
    };    

    class MergeConListener : public ContainerListener
    {
    public:
        virtual ~MergeConListener() { }

        virtual void OnMerged( MergeContainer *con, const Object *obj, const Object *mergedObj ) { }
    };
}

#endif

