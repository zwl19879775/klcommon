///
/// @file ScriptCaller.cpp
/// @author Kevin Lynx
///
///
#include "stdafx.h"
#include "ScriptCaller.h"
#include "../../LuaScript/ScriptSys.h"
#include "../../MoveShape.h"
#include "../../../../../Public/ServerPublic/Server/Common/VariableList.h"
#include "../../../../../Public/ServerPublic/Server/Common/VarListUtils.h"

namespace StateAI
{
    ScriptCaller::ScriptCaller( CMoveShape *shape )
    {
		if (shape == NULL)
		{
			LogError(AI_MODULE, "shape is null.");
			return;
		}
        m_Shape = shape;
        m_VarList = NULL;/*m_Shape->GetVariableList();*/
    }

    void ScriptCaller::AddParam( const char *varName, double varVal )
    {
		if (varName == NULL || varName[0] == '\0')
		{
			LogError(AI_MODULE, "invalid varName.");
			return;
		}
        if( !m_VarList->HasVar( varName ) )
        {
            m_VarList->AddVar( varName, varVal );
        }
        else
        {
            m_VarList->SetVarValue( varName, varVal );
        }
    }

    void ScriptCaller::AddParam( const char *varName, const char *varVal )
    {
		if (varName == NULL || varName[0] == '\0')
		{
			LogError(AI_MODULE, "invalid varName.");
			return;
		}
        if( !m_VarList->HasVar( varName ) )
        {
            m_VarList->AddVar( varName, tString( varVal ) );
        }
        else
        {
            m_VarList->SetVarValue( varName, tString( varVal ) );
        }
    }

    void ScriptCaller::AddParam( const char *varName, const CGUID &varVal )
    {
		if (varName == NULL || varName[0] == '\0')
		{
			LogError(AI_MODULE, "invalid varName.");
			return;
		}
        if( !m_VarList->HasVar( varName ) )
        {
            m_VarList->AddVar( varName, varVal );
        }
        else
        {
            m_VarList->SetVarValue( varName, varVal );
        }
    }

    void ScriptCaller::Call( const char *script )
    {
        if( script == NULL || script[0] == '\0' )
        {
			LogError(AI_MODULE, "script filename is invalid.");
            return;
        }
        Script::RunEnv env( m_Shape, NULL, static_cast<CRegion*>( m_Shape->GetFather() ) );
        GetInst( ScriptSys ).RunScript( env, script );
    }
}
