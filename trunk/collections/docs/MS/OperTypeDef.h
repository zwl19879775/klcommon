///
/// 定义GS与MS交互操作的各种类型
/// 该文件属于GS和MS共用文件，其所在目录的结构为：
/// ./GameServer
/// ./MonsterServer
/// ./ms_shared/OperTypeDef.h
///
#ifndef MS_OPERTYPE_DEF_H_
#define MS_OPERTYPE_DEF_H_

namespace MS
{
	/// 同步数据(SYN_DATA)类型
	enum SyncDataType
	{
		SDT_REGION,
		SDT_SKILL,
		SDT_MONSTERLIST,
		SDT_PLAYERPOS,
		SDT_MAX
	};

	/// 同步动作(SYN_ACT)类型，其值需要与同步数据不同
	enum SyncActType
	{
		SAT_WAKEUP = SDT_MAX,
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

