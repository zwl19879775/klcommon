///
/// @file SubConOperator.h
///
///
#ifndef ___SUBCONOPERATOR_H_
#define ___SUBCONOPERATOR_H_

#include "C2SObjectMove.h"

class SubConOperator : public MoveOperator
{
public:
    SubConOperator();

    virtual ~SubConOperator();

    virtual bool Check();

    virtual bool Move();

    static void Register();
protected:
    int GetOperType();

    bool MoveInDiffCon( int op );

    bool MoveInSameCon( int op );
};
#endif

