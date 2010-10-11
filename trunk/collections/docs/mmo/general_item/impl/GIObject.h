///
/// @file GIObject.h
/// @author Kevin Lynx
/// @date 9.28.2010
///
#ifndef ___GI_OBJECT_H_
#define ___GI_OBJECT_H_

#include "GIBase.h"
#include "GIProperty.h"

namespace GI
{
    class ObjectProto;

    ///
    /// Implement an object instance. Only store dynamic properties, the static 
    /// properties can be queried by ObjectProto.
    ///
    class Object : public PropertySet<TypeSet::KeyType, TypeSet::ValueType>, 
        public SerialData
    {
    public:
        Object( PListenerType *listener );

        virtual ~Object();

        /// Clone this to the object.
        Object *Clone( Object *dest ) const;
        
        /// Add a new property in the table.
        virtual bool AddProperty( KeyType key, ValueType val );

        /// Remove a property from the table.
        virtual void RemoveProperty( KeyType key );

        /// Remove all properties.
        virtual void Clear();

        /// Get a property value, if the property is static, it will get
        /// the value in ObjectProto.
        virtual ValueType GetValue( KeyType key ) const;

        /// Set the object prototype reference.
        void SetProto( const ObjectProto *proto );

        /// (Un)Serialize basic properties like: index, id etc.
        void SerializeBasic( ByteBuffer &buf );
        bool UnSerializeBasic( ByteBuffer &buf );

        /// (Un)Serialize dynamic properties.
        virtual void Serialize( ByteBuffer &buf ) const;
        virtual bool UnSerialize( ByteBuffer &buf );

        /// (Un)Serialize detail properties.
        void SerializeDetail( ByteBuffer &buf );
        bool UnSerializeDetail( ByteBuffer &buf );

        /// (Un)Serialize general properties.
        void SerializeGeneral( ByteBuffer &buf );
        bool UnSerializeGeneral( ByteBuffer &buf );

    protected:
        /// Unserialize a chunk of properties.
        bool UnSerializeProperties( ByteBuffer &buf );

    protected:
        const ObjectProto *m_proto;
        /// Detail properties count.
        int m_detailCnt;
        /// General properties count.
        int m_generalCnt;
    };
}

#endif

