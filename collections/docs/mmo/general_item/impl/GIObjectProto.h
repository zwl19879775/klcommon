///
/// @file GIObjectProto.h
/// @author Kevin Lynx
/// @brief Object prototype mantainer.
///
#ifndef ___GI_OBJECT_PROTO_H_
#define ___GI_OBJECT_PROTO_H_

#include "GIProperty.h"

namespace GI
{
    /// 
    /// Maintain object static properties, but it still mantain these 
    /// dynamic properties even they're nil.
    ///
    class ObjectProto : public PropertySet<TypeSet::KeyType, TypeSet::ValueType>
    {
        // add more...
    };

    class ObjProtoFactory;

    /// Interface class, load object prototypes in customer config file.
    class ProtoLoader
    {
    public:
        virtual bool Load( ObjProtoFactory *fac, void *u ) = 0;
    };

    ///
    /// Singleton class, manage all the object prototypes, used to create 
    /// an object instance. The object prototypes are stored as properties
    /// in PropertySet.
    ///
    class ObjProtoFactory : 
        public PropertySet<TypeSet::IndexType, ObjectProto*>,
        public kl_common::singleton<ObjProtoFactory>
    {
    public:
        ObjProtoFactory( ProtoLoader *loader );

        ~ObjProtoFactory();

        /// Use ProtoLoader to load object prototypes.
        bool Load( void *u );

        /// Release all the prototypes, called by destructor.
        void Release();

        /// Get object prototype by object index. 
        /// Return null if the prototype does not exist.
        const ObjectProto *GetProto( TypeSet::IndexType index ) const;

    private:
        ProtoLoader *m_loader;
    };
}

#endif

