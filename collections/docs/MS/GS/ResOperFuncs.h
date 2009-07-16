///
/// 提供回应(RESPONSE)操作函数集
///
namespace MServer
{
	namespace ResOper
	{
		/// 回应怪物移动
		void ResMonsterMove( const CGUID &monster, const CGUID &rgn, float x, float y );

		/// 回应怪物使用技能
		void ResMonsterUseSkill( const CGUID &monster, const CGUID &rgn, bool ret );
	}
}
