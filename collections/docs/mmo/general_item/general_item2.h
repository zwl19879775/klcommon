
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
// sample key
class Key : public SerialData
{
public:
    // for std::map
    bool operator< () const;

    operator long () const;
private:
    long key;
};

// sample value
class Value : public SerialData
{
public:
    typedef long Index;
    typedef GUID IDType;
public:
    template <typename T>
    T to() 
    {
        return *(T*)value.pv;
    }
private:
    union
    {
        long lv;
        double dv;
        std::string *sv;
        void *pv;
    } value; 
};

// must be defined.
struct TypeSet
{
    typedef Key KeyType;
    typedef Value ValueType;
    typedef Value::Index IndexType;
    typedef Value::Identify IDType;
    typedef Value::Stack StackType;
};

// this base module still need some object properties, but it does not
// know the property id, so list some necessary id here.
// must be defined too.
struct KeySet
{
    static const TypeSet::KeyType IDKey;
    static const TypeSet::KeyType IndexKey;
    static const TypeSet::KeyType StackCntKey;
};

// config.h end.

// item property.
struct Property
{
    TypeSet::KeyType key;
    TypeSet::ValueType value;
    // can query in ProperTypes, but for speed reason, keep it here.
    int type;
    Property( TypeSet::KeyType k, TypeSet::ValueType v, int t ) :
        key( k ), value( v ), type( t ) { }
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

    // get object prototype by index.
    const ObjProto &GetProto( TypeSet::IndexType index ) const;

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

    // create a seris objects. 
    // this function will use PropertyVisitor::StackCnt function.
    std::vector<Object*> Create( TypeSet::IndexType index, int count );

    // destroy an object, and remove it automatically.
    void Destroy( Object *obj );

private:
    ObjProtoFactory *m_protoFac;
};

/*
initial:  initialize PropertyTypeSet, insert all property types; -> ObjProtoFactory, load all object config.
create: ObjectFactory::Create -> find the object prototype by index -> tranverse all the properties in prototype
-> check the property in PropertyTypeSet, if the property is dynamic, use the ValueGenFunc to create the value,
otherwise use the value in prototype -> InsertProperty in object instance, set prototype in object instance
-> save the object in ObjectFactory by its ID property.
unserialize: object count -> ObjectFactory::Create -> unserialize index -> tranverse all the static properties
-> unserialize dynamic properties.
*/

/* container related stuff below */

// the position an object in a container.
typedef int PosType;

// listen on special container operations.
class OperListener
{
public:
    virtual void OnAdd( Object *obj, PosType pos ) = 0;
    virtual void OnRemove( Object *obj, PosType pos ) = 0;
};

class ContainerOperator
{
public:
    struct AddRet 
    {
        enum { FAILED, NEW, CHG_AMOUNT } type;
        PosType pos;
    };
    struct DelRet
    {
        enum { FAILED, WHOLE, CHG_AMOUNT } type;
        PosType pos;
        int cnt;
    };
    // delete object by its index property.
    struct DelOper
    {
        TypeSet::IndexType index;
        int cnt;
    };
public:
    ContainerOperator( Container *c );

    // can add object into the container anymore?
    virtual bool Valid( Object *obj ) const;

    // check add operation
    virtual AddRet CheckAdd( Object *obj );
    
    // rets[i] <-> objs[i]
    virtual std::vector<AddRet> CheckAdd( const std::vector<Object*> &objs );

    // change obj's amount.
    virtual DelRet CheckDel( Object *obj, int cnt );

    virtual std::vector<DelRet> CheckDel( const std::vector<DelOper> &dels );

protected:
    Container *m_container;
};

// all these operations below will use ContainerOperator to check first,
// then if check success operate it.
class Container : public PropertySet<TypeSet::Key, TypeSet::Value>
{
public:
    typedef std::vector<Object*> CellList;
  
public:
    Container( ContainerOperator *op, OperListener *listener );

    // add a new object into the container, the object already exists in
    // ObjFactory. return ContainerOperator::AddRet::type
    int Add( Object *obj );

    // use ContainerOperator::CheckAdd to determine where to add these objects,
    // then add them all.
    int Add( const std::vector<Object*> &objs );

    // add a new object.
    bool Add( Object *obj, PosType pos );

    // only change amount. no valid checking.
    bool SetAmount( PosType pos, TypeSet::StackType c );

    // change specified object's amount.
    // if the amount <= 0, delete the object.
    int Remove( Object *obj, int cnt );

    int Remove( const std::vector<ContainerOperator::DelOper> &dels );

    Object *Get( PosType pos );

    // copy self to a RefContainer.
    void CopyRefContainer( RefContainer &refc );

private:
    CellList m_objects;
    ContainerOperator *m_operator;
    OperListener *m_operListener;
};

// RefContainer is a container reference, it's used for batch operation.
// ContainerOperator will create a temp RefContainer and use it to test.
// only has member data, the operation is in ContainerOperator.
class RefContainer
{
public:
    typedef Container::TableType PropertiesTable;

    // cell info.
    struct Cell
    {
        TypeSet::IndexType index; //object index property.
        int maxAmount;
        int amount;
    };

    typedef std::vector<Cell> CellList;

public:
    PropertiesTable m_properties;
    CellList m_objects;
};

