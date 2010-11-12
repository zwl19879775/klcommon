///
/// @file ContainerDef.h
///
///
#ifndef ___CONTAINERDEF_H_
#define ___CONTAINERDEF_H_

namespace ConDef
{
    enum ObjOperType
    {
        OT_INVALID,
        OT_MOVE,
        OT_NEW,
        OT_DEL,
        OT_SWITCH_OBJ,
        OT_SWITCH_FILL,
        OT_CHANGE_AMOUNT,
        OT_UNKNOWN
    };

    enum ConType
    {
        PEI_NONE = 0,
        PEI_PACKET = 1,
        PEI_EQUIPMENT,
        PEI_WALLET,
        PEI_SILVERWALLET,
        PEI_GEM,
        PEI_YUANBAO,
        PEI_JIFEN,
        PEI_MEDAL,
        PEI_EQUIP_GOD,
        PEI_EQUIP_LAND,
        PEI_PACK = 10000,
        PEI_PACK1,
        PEI_PACK2,
        PEI_PACK3,
        PEI_PACK4,
        PEI_PACK5,
        PEI_DEPOT_BEGIN = 0xFFFF00,
        PEI_DEPOT_GOLD,
        PEI_DEPOT_SILVER,
        PEI_DEPOT_PACKET,
        PEI_DEPOT_PACK,
        PEI_DEPOT_PACK1,
        PEI_DEPOT_PACK2,
        PEI_DEPOT_PACK3,
        PEI_DEPOT_PACK4,
    };
}

#endif

