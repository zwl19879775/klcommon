///
/// 定义GS与MS交互操作的各种类型
/// 该文件属于GS和MS共用文件，其所在目录的结构为：
/// ./GameServer
/// ./MonsterServer
/// ./ms_shared/OperTypeDef.h
///
#ifndef MS_OPERTYPE_DEF_H_
#define MS_OPERTYPE_DEF_H_
/*
 GS到MS的同步操作(SYN_OPER)包括两种类型：
 - 同步数据：一般是单纯的数据，多为静态配置；
 - 同步动作：一般是某种操作，会导致MS方进行一些AI操作的类型，也会带一些属性数据。

所有的同步操作包括：
	场景数据：创建场景时编码场景
		SDT_REGION
	技能部分配置：服务器启动时编码静态技能配置（部分）
		SDT_SKILL
	怪物属性配置：monsterlist.ini
		SDT_MONSTERLIST
	怪物属性：怪物创建时的编码
		SDT_MONSTER
	玩家移动：单纯的改变坐标，MS计算区域切换
		SAT_PLAYER_MOVE
		player_id(CGUID)
		pos_x(float)
		pos_y(float)
		dir(long)
	玩家离开场景：场景对应的MS上会将玩家移除该场景，
		SAT_PLAYER_LEAVERGN
		player_id(CGUID)
		del_flag(byte) : 指示是否需要删除该玩家，当玩家切换场景时，GS比较
		两个场景所在MS是否相同，不相同则指定该标志，玩家退出游戏也指定该标志	
	玩家进入场景：场景对应的MS会将玩家加入新场景
		SAT_PLAYER_ENTERRGN
		player_id(CGUID)
		region_id(CGUID)
		pos_x(float)
		pos_y(float)
		add_flag(byte) : 指示是否需要解码该玩家，一般表示MS上没有此玩家
		raw_data : 玩家详细数据
		
*/
namespace MServer
{
	/// 同步数据(SYN_DATA)类型
	enum SyncDataType
	{
		SDT_REGION,
		SDT_SKILL,
		SDT_MONSTERLIST,
		SDT_MONSTER,
		SDT_MAX
	};

	/// 同步动作(SYN_ACT)类型，其值需要与同步数据不同
	enum SyncActType
	{
		SAT_WAKEUP = SDT_MAX,
		SAT_PLAYER_MOVE,
		SAT_PLAYER_LEAVERGN,
		SAT_PLAYER_ENTERRGN
	};

	/// MS请求类型
	enum RequestType
	{
		RT_MOVE,
		RT_USESKILL
	};

	/// GS回应MS请求类型
	enum ResponseType
	{
		RT_RES_MOVE,
		RT_RES_USESKILL,
	};
}

#endif

