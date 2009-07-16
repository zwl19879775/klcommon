///
/// 提供处理MS请求的函数集
///
namespace MServer
{
	namespace ReqHandle
	{
		/// 处理怪物移动请求
		void OnMonsterMove( CMessage *pMsg );

		/// 处理怪物使用技能请求
		void OnMonsterUseSkill( CMessage *pMsg );
	}
}
