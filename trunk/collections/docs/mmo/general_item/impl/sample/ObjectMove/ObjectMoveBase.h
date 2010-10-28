///
/// @file ObjectMoveBase.h
/// @brief Handle object move request from game client.
///
#ifndef ___OBJECT_MOVE_H_
#define ___OBJECT_MOVE_H_

#include "../TypesDef.h"

/// Object container info.
struct ObjConInfo
{
    CGUID ownerID; 
    long ownerType;
    long conID;
    long conPos;
};

/// Object info.
struct ObjInfo
{
    TypeSet::IDType id;
    long type;
    TypeSet::StackCntType cnt;
};

/// C2S object move info.
struct C2SObjectMoveInfo
{
    ObjConInfo src;
    ObjConInfo dest;
    ObjInfo obj;
};

/// S2C object move info.
struct S2CObjectMoveInfo
{
    ObjConInfo src;
    ObjConInfo dest;
    ObjInfo srcObj;
    ObjInfo destObj;
};

#endif

