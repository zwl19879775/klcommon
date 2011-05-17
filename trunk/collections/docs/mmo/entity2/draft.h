/*
 No property table. But still provide a way to have a chance to access all properties.
 There're many components, and they maintain these properties all by themself.
 All communication, either among components or outside entities, all convert the component
 to the concrete component and call their functions directly. i.e:
    // in some component
    SomeComponent *com = (SomeComponent*) m_cs->Get ("ComponentName");
    com->SomeMethod ();
    // outside the entity
    SomeComponent *com = (SomeComponent*) player->GetComponentSet()->Get ("ComponentName");
    com->SomeMethod ();
 There is no observer either, instead we use the concrete component direclty.
*/

class IComponent
{
public:
    typedef std::vector<std::string> SharedPropertyList;
public:
    IComponent (const std::string &name, ComponentSet *cs);

    virtual ~IComponent () { }

    /// When a component is added to the component set, this function will get called, and
    /// the component set will add these shared properties returned by this function to the
    /// shared properties table (in component set).
    virtual SharedPropertyList GetSharedProperty const () { return SharedPropertyList (); }

    /// Provide an uniform property read operation.
    virtual GValue GetPropertyVal (const std::string &name) const { return GValue (); }

    /// Write operation.
    virtual void SetPropertyVal (const std::string &name, const GValue &val) { }
    
private:
    ComponentSet *m_cs;
    std::string m_name;
};

class Mover : public IComponent
{
public:
    virtual SharedPropertyList GetSharedProperty const ()
    {
        SharedPropertyList l;
        l.push_back ("x");
        l.push_back ("y");
        return l;
    }

    virtual GValue GetPropertyVal (const std::string &name)
    {
        if (name == "x") return CreateGValue (m_x);
        if (name == "y") return CreateGValue (m_y);
        return GValue ();
    }

    // Will change the property x and y.
    void DoMove ();

private:
    float m_x;
    float m_y;
};

/// In some component which may use this `DirtyUpdater`, when it want to update dirty
/// properties, here is the usage:
///     DirtyUpdater *com = m_cs->Get<DirtyUpdater> ("DirtyUpdater");
///     com->Add ("hp");
///     com->Add ("mp");
///     com->Update (true);
/// Can we make this much more friendly like automatically update?
class DirtyUpdater : public IComponent
{
public:
    void Add (const std::string &name);

    /// If the flag `now` is false, register a timer to update later, and
    /// because the property value may changed when the timer expires, so
    /// it only hold the property name here
    void Update (bool now)
    {
        // for-each m_dirties do msg.Add (m_cs->GetSharedPropertyVal (name)) end
    }
private:
    std::list<std::string> m_dirties;
};

/// Collect all necessary properties, and encode them.
class Encoder : public IComponent
{
public:
    void Do (ByteArray &buf)
    {
        // here we got a encoding table, tell us which property should be encoded.
        // and what we should do is to traverse the whole table, find the corresponding
        // component by m_cs, and get the property value.
    }
};

/// Script can access all shared properties.
/// Actually script can access shared property values by ComponentSet, so this class is not 
/// necessary.
class ScriptAccessor : public IComponent
{
};

class ComponentSet
{
public:
    /// <component-name, Component>
    typedef std::map<std::string, IComponent*> ComponentTable;
    /// <property-name, Component>
    typedef std::map<std::string, IComponent*> PropertyTable;
public:
    /// for-each com->GetSharedProperty do AddSharedProperty (name, com) end
    void Add (IComponent *com)

    /// Remove a component by its name.
    void Remove (const std::string &name);

    /// Get a component by its name.
    IComponent *Get (const std::string &name);

    /// Find the corresponding component by the property name, and call com->GetPropertyVal .
    GValue GetSharedPropertyVal (const std::string &name);

    void SetSharedPropertyVal (const std::string &name, const GValue &val);

private:
    /// Add a referenced property into PropertyTable
    void AddSharedProperty (const std::string &name, IComponent *com);
    
    /// Remove operation for PropertyTable
    void RemoveSharedProperty (const std::string &name);
private:
    ComponentTable m_components;
    /// Shared properties can be referenced by more than 1 component.
    PropertyTable m_sharedProperties;
};

class Player
{
private:
    ComponentSet *m_components;
};

