///
/// @file RgnConOperator.h
///
///
#ifndef ___RGNCONOPERATOR_H_
#define ___RGNCONOPERATOR_H_

#include "C2SObjectMove.h"

class RgnConOperator : public MoveOperator
{
public:
    RgnConOperator();

    virtual ~RgnConOperator();

    virtual bool Check();

    virtual bool Move();

    static void Register();

protected:
    bool Pickup();

    bool Drop();
};

#endif

