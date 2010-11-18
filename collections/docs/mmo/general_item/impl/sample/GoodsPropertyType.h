///
/// @file GoodsPropertyType.h
///
#ifndef ___GOODS_PROPERTY_TYPE_H_
#define ___GOODS_PROPERTY_TYPE_H_

/// Adapter codes.
enum PSection
{
    PSEC_EXT = 0,
    PSEC_BASE = 512,
    PSEC_RUNTIME = 640,
};

/// Property enum type.
enum 
{
    PBASIC_BEGIN = PSEC_BASE,
    PINDEX,
    PID,
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
    PMAKER_NAME, // adapter code
    PBUY_PRICE,
    PBASIC_END,

    PRUNTIME_BEGIN = PSEC_RUNTIME, 
    PCELL_POS,
    PRGNX,
    PRGNY,
    PRUNTIME_END,

    PEXTEND_BEGIN = PSEC_EXT,
    PWEAPON_LEVEL,
    PINC_HP_MIN,
    PINC_HP_MAX,
    PINC_HP,
    PCOOLDOWN,
    PCON_SIZE,
    PMAX_STACKCNT_DEP = 56,
    PEXTEND_END,

    PALL_END
};

/// Adapter code, for these goods without max stack count property.
enum 
{
    DEFAULT_MAX_STACKCNT = 999999
};

#endif

