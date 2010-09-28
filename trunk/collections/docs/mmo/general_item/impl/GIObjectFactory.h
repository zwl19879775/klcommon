///
/// @file GIObjectFactory.h
/// @author Kevin Lynx
/// @brief Object factory, manage objects lifetime.
///
#ifndef ___GI_OBJECT_FACTORY_H_
#define ___GI_OBJECT_FACTORY_H_

#include "GIObject.h"
#include "kl_singleton.h"
#include <vector>

namespace GI
{
    class Object;
    class ObjProtoFactory;

    ///
    /// Singleton class, manage object instance. This class stores objects as 
    /// properties in PropertySet.
    ///
    class ObjectFactory : public PropertySet<TypeSet::IDType, Object*>,
        public kl_common::singleton<ObjectFactory>
    {
    public:
        ObjectFactory( ObjProtoFactory *proto );

        ~ObjectFactory();

        /// Create a new object, will not insert it.
        Object *Create( TypeSet::IndexType index, Object::PListenerType *listener );

        ///
        /// Unserialize a new object, will not insert it.
        /// The buffer format is: index, dynamic properties.
        ///
        Object *Create( ByteBuffer &buf, Object::PListenerType *listener );

        ///
        /// Create a seris of objects. 
        /// This function depents on Object stack count property.
        ///
        std::vector<Object*> Create( TypeSet::IndexType index, int count, Object::PListenerType *listener );

        /// Destroy an object, and remove it automatically.
        void Destroy( Object *obj );

        /// Destroy all the objects.
        void DestroyAll();

    private:
        ObjProtoFactory *m_protoFac;
    };
}

#endif

