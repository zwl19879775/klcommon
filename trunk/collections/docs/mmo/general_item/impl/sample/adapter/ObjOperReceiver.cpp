///
/// @file ObjOperReceiver.cpp
///
///
#include "ObjOperReceiver.h"
#include "ObjOperSender.h"
#include "../ObjectMove/C2SObjectMove.h"

void ReceiveObjOperMsg( CMessage *msg )
{
    C2SObjectMoveInfo info;
    ObjOperSender operSender;
    info.src.ownerType = msg->GetLong();
    msg->GetGUID( info.src.ownerID );
    info.src.conID = msg->GetLong();
    info.src.conPos = msg->GetLong();

    info.dest.ownerType = msg->GetLong();
    msg->GetGUID( info.dest.ownerID );
    info.dest.conID = msg->GetLong();
    info.dest.conPos = msg->GetLong();

    info.obj.type = msg->GetLong();
    msg->GetGUID( info.obj.id );
    info.obj.cnt = msg->GetLong();

    SINGLETON( C2SObjectMoveDispatcher ).Run( info, &operSender );
}

