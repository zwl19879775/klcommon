///
/// 提供一些分配场景/怪物的辅助函数
///
namespace MServer
{
	namespace DisHelper
	{
		///
		/// 根据怪物所在场景计算该怪物将被分配到哪个MS
		///
		/// 没找到返回CRgnDisConfiger::NOT_DIS
		long GetMonsterDisMS( CMonster *pMonster );
	}
}
