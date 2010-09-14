
enum PropertyType
{
    PT_STATIC   = 0x0001,
    PT_DYNAMIC  = 0x0002,
    PT_DETAIL   = 0x0004,
    PT_GENERAL  = 0x0008,
    PT_IDENTIFY = 0x0010,
    PT_INDEX    = 0x0020,
};

template <typename Key, typename Value>
struct Property
{
    Key key;
    Value value;
    int type;
};

template <typename Key, typename Value, 
         typename Table = std::map<Key, Value> >
class PropertySet
{
public:
    typedef Key KeyType;
    typedef Value ValueType;
    typedef Table TableType;
public:
    bool Add( KeyType key, Value val );
    void Remove( KeyType key );

    Value GetValue( KeyType key );
    void SetValue( KeyType key, Value val );

protected:
    TableType m_properties;
};

// only keep the dynamic properties, the static properties are in
// ObjectProto<Object>
template <typename Key, typename Value, 
         typename Table = std::map<Key, Property<Key, Value>* > >
class Object
{
public:
    typedef Key KeyType;
    typedef Value ValueType;
    typedef typename Value::Identify IDType;
    typedef typename Value::Index IndexType;
    typedef Table TableType;
    typedef Object<KeyType, ValueType, TableType> SelfType;
    typedef ObjectProto<SelfType> ProtoType;
public:
    // set object prototype, the object can query static properties in the
    // prototype.
    void SetProto( ProtoType *proto );

    // insert a dynamic property.
    void InsertProperty( Key key, Value val, int type );

    // get a property value.
    Value GetValue( Key key );

    // serialize basic properties: index, id, ...
    void SerializeBasic( ByteBuffer &buf );
    bool UnSerializeBasic( const ByteBuffer &buf );

    // serialize dynamic properties.
    void SerializeDynamic( ByteBuffer &buf );
    bool UnSerializeDynamic( const ByteBuffer &buf );

    // serialize detail properties.
    void SerializeDetail( ByteBuffer &buf );
    void UnSerializeDetail( const ByteBuffer &buf );

    // serialize general properties.
    void SerializeGeneral( ByteBuffer &buf );
    void UnSerializeGeneral( const ByteBuffer &buf );

protected:
    // dynamic properties.
    Table m_dyProperties;
    // can query static properties in it.
    ProtoType *m_proto;
    // reference to detail properties.
    Table m_detailProperties;
    // reference to general properties.
    Table m_generalProperties;
};

template <typename Value>
struct ValueGen
{
    int type;
    typedef Value (*GenValFunc)( void* );
    GenValFunc func;
};

// implement property types config.
template <typename Key, typename Value, 
         typename Table = std::map<Key, ValueGen<Value> > >
class PropertyTypes
{
public:
    typedef typename ValueGen<Value>::GenValFunc GenValFunc;
public:
    // set property type, if does not exist, add a new property.
    void Set( Key key, int type, GenValFunc func, void *u );

    int GetType( Key key );

    Value GenValue( Key key );

private:
    Table m_types;
};

// implement object static properties.
// manage all the properties of an object, including these dynamic properties.
template <typename Obj>
class ObjectProto
{
public:
    typedef typename Obj::TableType TableType;
private:
    TableType m_properties;
};

// manage object prototype( object static properties ).
template <typename Obj,
         typename ProtoLoader,
         typename Table = std::map<typename Obj::IndexType, ObjectProto<Obj>*> >
class ObjectProtoFactory
{
public:
    typedef Obj::IndexType IndexType;
    typedef ObjectProto<Obj> ProtoType;
public:
    // get a config(object prototype) by index property.
    const ProtoType &GetProto( IndexType index );
private:
    // the config list.
    Table m_protos;
};

// manage object instance.
template <typename Obj,
         typename ProtoFactory,
         typename Table = std::map<typename Obj::IDType, Obj*> >
class ObjectFactory
{
public:
    typedef Obj::IndexType IndexType;
public:
    ObjectFactory( ProtoFactory *protoFactory );

    Object *Create( IndexType index );

    // create from buffer.
    Object *Create( const ByteBuffer &buf );

    void Destroy( Object *obj );

private:
    Table m_objects;
};

/*
initial:  initialize PropertyTypes, insert all property types; -> ObjectProtoFactory, load all object config.
create: ObjectFactory::Create -> find the object prototype by index -> tranverse all the properties in prototype
-> check the property in PropertyTypes, if the property is dynamic, use the ValueGenFunc to create the value,
otherwise use the value in prototype -> InsertProperty in object instance, set prototype in object instance
-> save the object in ObjectFactory by its ID property.
unserialize: object count -> ObjectFactory::Create -> unserialize index -> tranverse all the static properties
-> unserialize dynamic properties.
*/

//
// the AddPolicy implement 'Valild' to check whether the object can be added into the container, 
// 'Where" to determine where to add the object in the container.
// the OperListener listens on special container operations like: 'Add',
// 'Remove' etc.
template <typename Key, 
         typename Value,
         typename Object,
         typename AddPolicy, 
         typename OperListener,
         typename CellList = std::vector<Object*>, 
         typename Table = std::map<Key, Value*> >
class Container
{
public:
    typedef typename Object::IDType IDType;
    typedef CellList CellListType;
public:
    Container( AddPolicy *policy, OperListener *listener );

    // check whether this operation is 'Valid' using AddPolicy, and get the
    // position where to add by using 'Where' method. And fininally notify the
    // listener.
    bool Add( Object *obj );

    void Remove( Object *obj );

    Object *Get( size_t pos );

private:
    // amount/volume/type etc.
    Table m_properties;
    CellList m_objects;
    AddPolicy *m_addPolicy;
    OperListener *m_operListener;
};

/*
*/

