///
/// @file SameConOperator.h
///
///
#ifndef ___SAMECONOPERATOR_H_
#define ___SAMECONOPERATOR_H_

#include "C2SObjectMove.h"

class SameConOperator : public MoveOperator
{
public:
    SameConOperator();

    virtual ~SameConOperator();

    virtual bool Check();

    virtual bool Move();

    static void Register();
private:
    bool MoveInSameCon();

    bool MoveInDiffCon();
};

#endif

