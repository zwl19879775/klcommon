///
/// @file C2SObjectMove.h
///
///
#ifndef ___C2SOBJECT_MOVE_H_
#define ___C2SOBJECT_MOVE_H_

#include "ObjectMoveBase.h"
#include "../../GIForwardRefs.h"
#include "../../GIConfig.h"
#include <map>


class ObjOperSender;

class MoveOperator
{
public:
    enum { NONE, MOVE, SPLIT, MERGE, PARTIAL_MERGE, SWAP };
public:
    MoveOperator();

    virtual ~MoveOperator() { }

    void Setup( const C2SObjectMoveInfo *info, ObjOperSender *res );

    virtual bool Check() { return true; }

    virtual bool Move() = 0;

protected:
    int GetCellOperType();

    bool IsSameOwner();

    bool IsSameCon();

    GI::BaseContainer *GetSrcCon();

    GI::BaseContainer *GetDestCon();
protected:
    const C2SObjectMoveInfo *m_info;
    ObjOperSender *m_res;
};

///
/// Dispatch game client object move request to these handlers.
/// Singleton class.
///
class C2SObjectMoveDispatcher 
    DEF_SINGLETON( C2SObjectMoveDispatcher )
{
public:
    typedef unsigned long Identifier;
    typedef std::map<Identifier, MoveOperator*> HandlerTableT;
public:
    C2SObjectMoveDispatcher();

    ~C2SObjectMoveDispatcher();

    void Register( Identifier id, MoveOperator *op );

    bool Run( const C2SObjectMoveInfo &info, ObjOperSender *res );

    static Identifier ToId( long srcType, long srcCon,
            long destType, long destCon );

    static Identifier ToId( const C2SObjectMoveInfo &info );
private:
    HandlerTableT m_funcs;
};

#define REGISTER_HANDLER( st, sid, dt, did, op ) \
    { \
        C2SObjectMoveDispatcher::Identifier id = C2SObjectMoveDispatcher::ToId( \
            st, sid, dt, did ); \
        SINGLETON( C2SObjectMoveDispatcher ).Register( id, op ); \
    }
#endif

