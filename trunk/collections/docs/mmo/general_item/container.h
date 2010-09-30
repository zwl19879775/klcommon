
// We donot care the stack count of an object.

class BaseContainer
{
public:
    typedef std::map<TypeSet::IDType, Object*> ObjectMap;
public:
    virtual ~BaseContainer();

    /// Move the object specified by objID to the dest-Container.
    virtual bool Move( BaseContainer *destCon, TypeSet::IDType objID );
    
    /// Move all the objects to the dest-Container.
    virtual bool MoveAll( BaseContainer *destCon );

    /// Destroy an object in this container.
    virtual bool Destroy( TypeSet::IDType objID );

    /// Get the object by its id.
    const Object *Get( TypeSet::IDType objID ) const;

    /// Traverse all the objects (const reference)
    /// void fn( TypeSet::IDType id, const Object *obj )
    template <typename T>
    void Traverse( T fn ) const;

protected: // Only valid between containers.
    /// Add an object to this container.
    virtual bool Add( Object *obj );

    /// Remove an object from this container.
    virtual bool Remove( TypeSet::IDType objID );

protected:
    ObjectMap m_objs;
};

class FactoryContainer : public BaseContainer
{
public:
    virtual ~FactoryContainer();

    /// Create a seris of objects and add them to this container.
    /// Call 'MoveAll' to move all the created objects to the dest-Container.
    virtual bool Create( TypeSet::IndexType index, int cnt, Object::PListenerType *listener );

    /// Same as 'Create' but can be customised by a functor.
    /// void fn( Object *obj )
    template <T>
    bool Create( TypeSet::IndexType index, int cnt, Object::PListenerType *listener, T fn );

protected:
    /// Create object, but donot add it.
    Object *Create( TypeSet::IndexType index, Object::PListenerType *listener );
};

class ModifyContainer : public BaseContainer
{
public:
    virtual ~ModifyContainer();

    /// Modify the specified property value of the object.
    /// The object must be in this container.
    virtual bool Modify( TypeSet::IDType objID, TypeSet::KeyType key, TypeSet::KeyType value );
};

class MergeContainer : public BaseContainer
{
public:
    virtual ~MergeContainer();

    /// Merge the two objects.
    /// If merge success, destroy obj2.
    virtual bool Merge( TypeSet::IDType obj1, TypeSet::IDType obj2 );

    /// Split the object into two objects. Call 'Add' to add the new 
    /// object into this container.
    /// This function will clone the object,except the stack count is different.
    virtual bool Split( TypeSet::IDType obj, int splitCnt );

    /// Decrease the stack count of the object.
    /// If the stack count is 0, Destroy the object.
    virtual bool DecStack( TypeSet::IDType obj, int decCnt );
};

///////////App below/////////////////////////////////////////////////////////
class PlayerContainer : public MergeContainer
{
protected:
    /// May merge the object to an existed object.
    virtual bool Add( Object *obj );
private:
    TypeSet::IDType m_cells[CELL_CNT];
};

class RegionContainer : public FactoryContainer
{
    // ...
};

///////////Use case/////////////////////////////////////////////////////////
// Use case: create objects:  create new object -> add to player package.
/*
 FactoryContainer fc;
 fc.Create( index, cnt );
 fc.MoveAll( &playerContainer );
    playerContainer.Add( obj )...
    if( CanBeMerged( obj ) ) 
        how to merge
        Merge( existObj, obj );
    else
        new position
    endif
*/

// Use case: remove some (stack count) specified index objects:
/*
 // Collect the specified index objects.
 playerContainer.Traverse( CollectIndexObject( index ) ); 
 // calculate how to remove them (change stack amount or destroy)
 if destroy then
    Destroy( obj )
 else
    DecStack( obj, decCnt )
 end
*/

// Use case: merge two objects.
/* playerContainer.Merge( obj1, obj2 ) */

// Use case: split one object.
/* playerContainer.Split( obj, splitCnt ); */

// Use case: drop some objects in player container to the region.
/*
 if drop_cnt == object.cnt then
    playerContainer.Move( &regionContainer, objID )
 else
    playerContainer.DecStack( drop_cnt )
    // SetStackCnt is a callback function(functor) to set the object's stack
    // count.
    regionContainer.Create( drop_index, drop_cnt, listener, SetStackCnt( drop_cnt ) )
 endif

*/

