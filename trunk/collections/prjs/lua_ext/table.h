
#include <map>
#include <string>

// test only
typedef long Value;

class ParamTable {
public:
    typedef std::map<std::string, Value> ValTable;
    /// It's not necessary to copy all properties of an object to this, so
    /// when the property does not exist here, it will call these Getter/Settter
    /// to access the property.
    typedef const Value* (*Getter) (const ParamTable&, const std::string&);
    typedef bool (*Setter) (const ParamTable&, const std::string&, const Value&);
public:
    ParamTable (Getter g = NULL, Setter s = NULL);

    /// Get a property value.
    const Value *Get (const std::string &name) const;

    /// If the value does not exist, add a new one.
    bool Set (const std::string &name, const Value &val);

    /// Dump all cached properties by settter. And clear the cache.
    void Dump ();
private:
    void Add (const std::string &name, const Value &val);
private:
    ValTable m_vals;
    Getter m_getter;
    Setter m_setter;
};

