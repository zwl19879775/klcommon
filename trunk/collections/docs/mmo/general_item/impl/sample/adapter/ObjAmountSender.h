///
/// @file ObjAmountSender.h
///
///
#ifndef ___OBJAMOUNTSENDER_H_
#define ___OBJAMOUNTSENDER_H_

#include "../ObjectMove/ObjectMoveBase.h"
#include "../../GIForwardRefs.h"
#include "../ContainerDef.h"

class ObjAmountSender
{
public:
    ObjAmountSender();

    void Send( const CGUID &shapeID );

    void SetCon( long type, const CGUID &id, long conID, long pos );

    void SetObj( const TypeSet::IDType &id, long cnt );

public:
    S2CObjectAmountChangeInfo m_info;
};

#endif

