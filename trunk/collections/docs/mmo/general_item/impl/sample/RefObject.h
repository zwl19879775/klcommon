///
/// @file RefObject.h
/// 
///
#ifndef ___REF_OBJECT_H_
#define ___REF_OBJECT_H_

#include "../GIObject.h"
#include "TypesDef.h"
#include <vector>

struct RefObject
{
    TypeSet::IDType id;
    TypeSet::IndexType index;
    long maxStackCnt;
    long stackCnt;
    long pos;

    void RefTo( const GI::Object *obj );
};

struct DelRet
{
    long pos;
    long dec;
    enum { NONE, CHG_STACKCNT, DEL } op;
};
typedef std::vector<DelRet> DelRetListT;

struct AddRet
{
    TypeSet::IDType id;
    long pos;
    enum { NONE, CHG_STACKCNT, NEW } op;
};
typedef std::vector<AddRet> AddRetListT;

#endif

