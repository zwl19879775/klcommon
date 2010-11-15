///
/// @file ObjPropertyNotifier.h
///
///
#ifndef ___OBJPROPERTYNOTIFIER_H_
#define ___OBJPROPERTYNOTIFIER_H_

#include "../../GIObject.h"
#include <vector>

/// Object dirty properties notifier.
class ObjPropertyNotifier : public GI::Object::PListenerType
{
public:
    struct Dirty
    {
        TypeSet::KeyType key;
        long pos;
    };
    typedef std::vector<Dirty> DirtyListT;
public:
    virtual ~ObjPropertyNotifier() { }

    virtual void OnSet( TypeSet::KeyType key, TypeSet::ValueType oldVal, TypeSet::ValueType newVal );

    bool Update( const CGUID &shapeID );
private:
    DirtyListT m_dirtyList;
};

#endif

