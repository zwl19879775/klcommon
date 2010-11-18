///
/// @file GIProperty.h
/// @author Kevin Lynx
/// @brief GI property relative stuff definitions.
///
#ifndef ___GI_PROPERTY_H_
#define ___GI_PROPERTY_H_

#include "GIConfig.h"
#include <map>

namespace GI
{
    enum 
    {
        PT_NULL     = 0x0000,
        PT_STATIC   = 0x0001,
        PT_DYNAMIC  = 0x0002,
        PT_DETAIL   = 0x0004,
        PT_GENERAL  = 0x0008,
        PT_IDENTIFY = 0x0010,
        PT_INDEX    = 0x0020,
    };

#define IS_STATIC( t ) (t & GI::PT_STATIC)
#define IS_DYNAMIC( t ) (t & GI::PT_DYNAMIC)
#define IS_DETAIL( t ) (t & GI::PT_DETAIL)
#define IS_GENERAL( t ) (t & GI::PT_GENERAL)
#define IS_IDENTIFY( t ) (t & GI::PT_IDENTIFY)
#define IS_INDEX( t ) (t & GI::PT_INDEX)

    template <typename Key, typename Value,
             typename Table>
    class PropertySet;

    /// Listen on properties.
    template <typename Key, typename Value,
             typename Table>
    class PropertyListener
    {
    public:
        typedef PropertySet<Key, Value, Table> OwnerType;
    public:
        PropertyListener() { m_owner = NULL; }

        virtual ~PropertyListener() { }

        /// Called when a new property is added.
        virtual void OnAdd( Key key, Value val ) { }

        /// Called when a property has removed.
        virtual void OnRemove( Key key ) { }

        /// Called when a property value has been changed.
        virtual void OnSet( Key key, Value oldVal, Value newVal ) { }

        void SetOwner( const OwnerType *owner ) { m_owner = owner; }
    protected:
        const OwnerType *m_owner;
    };

    /// Manage a <key, value> table.
    template <typename Key, typename Value,
             typename Table = std::map<Key, Value> >
    class PropertySet
    {
    public:
        typedef Key KeyType;
        typedef Value ValueType;
        typedef Table TableType;
        typedef PropertyListener<KeyType, ValueType, Table> PListenerType;
        typedef PropertySet<KeyType, ValueType> SelfType;
    public:
        PropertySet( PListenerType *listener = NULL );

        virtual ~PropertySet();

        /// Set property listener.
        /// The listener will be destroied automatically.
        void SetListener( PListenerType *listener );

        /// Get the property listener.
        PListenerType GetListener() const { return m_proListener; }

        /// Add a new property in the table.
        virtual bool AddProperty( Key key, Value val );

        /// Remove a property from the table.
        virtual void RemoveProperty( Key key );

        /// Remove all properties.
        virtual void Clear();

        /// Check whether has a property.
        virtual bool HasProperty( Key key ) const;

        /// Get a property value, if the property does not exist, return 
        /// Value::INVALID.
        virtual Value GetValue( Key key ) const;

        /// Set a property value, if the property does not exist, return false.
        virtual bool SetValue( Key key, Value val );

        /// Traverse the whole property table. op( key, value );
        /// The process will stop when op return true.
        template <typename Fn>
        void Traverse( Fn op ) const;

        size_t Size() const { return m_properties.size(); }
    protected:
        /// Table to store <key, value>.
        Table m_properties;
        /// Property listener, can be null.
        PListenerType *m_proListener;
    };

    // Bad pre-declaration.
    class Object;
    struct PropertyType
    {
        int type;
        /// Callback function to generate dynamic properties.
        typedef TypeSet::ValueType (*GenValFunc)( const Object *obj, void *u );
        GenValFunc func;
        void *u;
        PropertyType( int t = PT_NULL, GenValFunc f = NULL, void *_u = NULL ) : 
            type( t ), func( f ), u( _u )
        {
        }
    };

    ///
    /// Singleton class, to manage all the property types.
    /// The property types can be put in a single config file.
    ///
    class PropertyTypeSet : public PropertySet<TypeSet::KeyType, PropertyType>
        MULTI_DEF_SINGLETON( PropertyTypeSet )
    {
    public:
        PropertyTypeSet();

        /// Add a new property type.
        void Add( KeyType key, int type, PropertyType::GenValFunc func, void *u );

        /// Get a property type.
        int GetType( KeyType key );

        /// Generate a property value, the property must be a dynamic 
        /// property.
        TypeSet::ValueType GenValue( KeyType key, const Object *obj );
    };

#include "GIPropertySetImpl.h"
}

#endif

