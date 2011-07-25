///
/// @file AIUtilities.cpp
/// @author Kevin Lynx
/// @brief To provide some useful utilities for AI module.
///
#include "StdAfx.h"
#include "AIUtils.h"
#include "../../ServerRegion.h"

namespace AIUtils
{
	size_t FindAroundShape( CServerRegion *pRegion, CShape *pFinder, ShapeListType &outList )
	{
		if (pRegion == NULL)
		{
			LogError(AI_MODULE, "pRegion is null.");
			return 0;
		}
		if (pFinder == NULL)
		{
			LogError(AI_MODULE, "pFinder is null.");
			return 0;
		}
		size_t init_size = outList.size();
		pRegion->FindAroundObject( pFinder, TYPE_MONSTER, outList );
		pRegion->FindAroundObject( pFinder, TYPE_PLAYER, outList );
		return outList.size() - init_size;
	}

	CMoveShape *FindShape( CServerRegion *pRegion, const CGUID &targetID )
	{
		if (pRegion == NULL)
		{
			LogError(AI_MODULE, "pRegion is null.");
			return NULL;
		}
		CMoveShape *pTarget;
		pTarget = static_cast<CMoveShape*>( pRegion->FindChildObject( TYPE_MONSTER, targetID ));
		if( pTarget == NULL )
		{
			pTarget = static_cast<CMoveShape*>( pRegion->FindChildObject( TYPE_PLAYER,targetID ) );
		}

		return pTarget;
	}
}
