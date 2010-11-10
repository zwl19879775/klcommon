///
/// @file ObjOperSender.h
///
///
#ifndef ___OBJOPERSENDER_H_
#define ___OBJOPERSENDER_H_

#include "../ObjectMove/ObjectMoveBase.h"
#include "../../GIForwardRefs.h"
#include "../ContainerDef.h"

class ObjOperSender
{
public:
    ObjOperSender();

    void Send( const CGUID &shapeID );

    void SetOperType( int op );

    void AddObject( const GI::Object *obj );

    void SetSrcCon( const CGUID &ownerID, long type, long id, long pos );

    void SetDestCon( const CGUID &ownerID, long type, long id, long pos );

    void SetSrcObj( const TypeSet::IDType &id, long cnt );

    void SetDestObj( const TypeSet::IDType &id, long cnt );

private:
    bool AddReFillInfo( GI::ByteBuffer &buf );

public:
    S2CObjectMoveInfo m_info;
private:
    const GI::Object *m_obj;
    int m_op;
};

#endif

