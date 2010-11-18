///
/// @file ObjProtoLoader.h
///
///
#ifndef ___OBJPROTOLOADER_H_
#define ___OBJPROTOLOADER_H_

#include "../GIObjectProto.h"

class ObjProtoLoader : public GI::ProtoLoader
    MULTI_DEF_SINGLETON( ObjProtoLoader )
{
public:
    virtual ~ObjProtoLoader() { }

    virtual bool Load( GI::ObjProtoFactory *fac, void *u );
};

#endif

