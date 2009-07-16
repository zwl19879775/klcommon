///
/// 提供请求(REQUEST)操作的函数集
///
namespace ReqOper
{
	/// 请求怪物移动
	void ReqMonsterMove( const CGUID &monster, const CGUID &rgn, long dir );

	/// 请求怪物使用技能
	void ReqMonsterUseSkill( const CGUID &monster, const CGUID &rgn, 
			long skill_id, long skill_lvl, const CGUID &target_id );
}
