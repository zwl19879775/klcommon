
enum PropertyType
{
    PT_STATIC   = 0x0001,
    PT_DYNAMIC  = 0x0002,
    PT_DETAIL   = 0x0004,
    PT_GENERAL  = 0x0008,
    PT_IDENTIFY = 0x0010,
    PT_INDEX    = 0x0020,
};

class ByteBuffer
{
public:
    virtual void Push( const void *d, size_t size ) = 0;
    virtual void Pop( void *d, size_t size ) = 0;
};

class SerialData
{
public:
    virtual void Serialize( ByteBuffer &buf );
    virtual bool UnSerialize( ByteBuffer &buf );
};

// config.h
class Key : public SerialData
{
};

class Value : public SerialData
{
};

// must be defined.
struct TypeSet
{
    typedef Key KeyType;
    typedef Value ValueType;
    typedef Value::Index IndexType;
    typedef Value::Identify IDType;
};
// config.h end.

// item property.
struct Property
{
    TypeSet::KeyType key;
    TypeSet::ValueType value;
    // can query in ProperTypes, but for speed reason, keep it here.
    int type;
};

// manage <key, value> properties table.
template <typename Key, typename Value,
         typename Table = std::map<Key, Value> >
class PropertySet
{
public:
    typedef Key KeyType;
    typedef Value ValueType;
    typedef Table TableType;
public:
    bool AddProperty( Key key, Value val );
    void RemoveProperty( Key key );

    Value GetValue( Key key ) const;
    void SetValue( Key key, Value val );

protected:
    Table m_properties;
};

class Object : public PropertySet<TypeSet::KeyType, Property*>, 
    public SerialData
{
public:
    void SetProto( ObjProto *proto );

    // serialize basic properties: index, id, ...
    void SerializeBasic( ByteBuffer &buf );
    bool UnSerializeBasic( ByteBuffer &buf );

    // serialize dynamic properties.
    virtual void Serialize( ByteBuffer &buf );
    virtual bool UnSerialize( ByteBuffer &buf );

    // serialize detail properties.
    void SerializeDetail( ByteBuffer &buf );
    bool UnSerializeDetail( ByteBuffer &buf );

    // serialize general properties.
    void SerializeGeneral( ByteBuffer &buf );
    bool UnSerializeGeneral( ByteBuffer &buf );

protected:
    // can query static properties in it.
    ObjProto *m_proto;
    // PropertySet::m_properties stores dynamic properties.
    // ...
    // reference to detail properties in dynamic properties.
    Table m_detailProperties;
    // reference to general properties in dynamic properties.
    Table m_generalProperties;
};

// item property type, can query property type from here.
struct PropertyType
{
    int type;
    // callback function to generate dynamic properties.
    // if the property is not dynamic, the func is null.
    typedef TypeSet::Value (*GenValFunc)( void *u );
    GenValFunc func;
};

// singleton class, to manage all the property types.
// the property types can be put in a single config file.
class PropertyTypeSet
{
public:
    typedef std::map<TypeSet::Key, PropertyType> TypeSet;
public:
    // add a new property type.
    void Add( Key key, int type, PropertyType::GenValFunc func );
    // get a property type.
    int GetType( Key key );
    // generate a property value.
    TypeSet::Value GenValue( Key key, void *u );
private:
    TypeSet m_types;
};

// implement object static properties.
// manage all the properties of an object, including these dynamic properties.
// but the dynamic properties are null.
class ObjProto : public PropertySet<TypeSet::Key, TypeSet::Value>
{
    // no more
};

// interface class, to load object prototypes in customer config file.
class ProtoLoader
{
public:
    virtual bool Load( ObjProtoFactory *fac, void *u ) = 0;
};

// singleton class, to manage object prototypes.
class ObjProtoFactory : 
    public PropertySet<TypeSet::IndexType, ObjProto*>
{
public:
    ObjProtoFactory( ProtoLoader *loader );

    // call m_loader->Load, and ProtoLoaderImpl::Load will add new ObjProto into
    // factory.
    bool Load( void *u );

private:
    ProtoLoader *m_loader;
};

// singleton class, manage object instance.
class ObjectFactory : public PropertySet<TypeSet::IDType, Object*>
{
public:
    ObjectFactory( ObjProtoFactory *proto );

    // create a new object, will not insert it.
    Object *Create( TypeSet::IndexType index );

    // unserialize a new object, will not insert it.
    Object *Create( ByteBuffer &buf );

    // destroy an object, and remove it automatically.
    void Destroy( Object *obj );

private:
    ObjProtoFactory *m_protoFac;
};

/* container related stuff below */

// the position an object in a container.
typedef int PosType;

// add policy is a policy class used to determinte whether an object can be put
// in the container and where to put it.
class AddPolicy
{
public:
    // the 'add' operation is valid?
    virtual bool Valid( Object *obj ) = 0;
    // where to add.
    virtual PosType Where( Object *obj ) = 0;
    // add it.
    virtual void Add( PosType pos, Object *obj ) = 0;
};

// listen on special container operations.
class OperListener
{
public:
    virtual void OnAdd( Object *obj, PosType pos ) = 0;
    virtual void OnRemove( Object *obj, PosType pos ) = 0;
};

class Container : public PropertySet<TypeSet::Key, TypeSet::Value>
{
public:
    typedef std::vector<Object*> CellList;
public:
    Container( AddPolicy *policy, OperListener *listener );

    // add a new object into the container, the object already exists in
    // ObjFactory.
    bool Add( Object *obj );

    void Remove( Object *obj );

    Object *Get( PosType pos );

private:
    CellList m_objects;
    AddPolicy *m_addPolicy;
    OperListener *m_operListener;
};

