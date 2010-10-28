///
/// @file GoodsPropertyType.h
///
#ifndef ___GOODS_PROPERTY_TYPE_H_
#define ___GOODS_PROPERTY_TYPE_H_

/// Property enum type.
enum 
{
    PBASIC_BEGIN = 0,
    PINDEX,
    PID,
    PRGNX,
    PRGNY,
    PORIGNAME,
    PNAME,
    PGOLD_PRICE,
    PSILVER_PRICE,
    PTYPE,
    PUI_PICID,
    PRGN_PICID,
    PEQUIP_PICID,
    PGET_SNDID,
    PNORMAL_SNDID,
    PSPECIAL_SNDID,
    PATTACK_FLAG,
    PEQUIP_ACTION,
    PDESC,
    PMAX_STACKCNT,
    PSTACKCNT,

    PRUNTIME_BEGIN = 127, 
    PCELL_POS,

    PEXTEND_BEGIN = 255,
    PINC_HP,
    PCOOLDOWN,
    PCON_SIZE,

    PALL_END
};

#endif

