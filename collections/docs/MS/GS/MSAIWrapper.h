///
/// 为与现有GS AI架构保持兼容的的AI包装类，负责将一些消息通信封装成
/// AI函数调用。
///
namespace MServer
{
	class CMSAIWrapper : public CMonsterAI
	{
	public:
		/// 怪物出生触发，如果是第一次调用，意味着MS上没有此怪物副本，需要编码；
		/// 否则不需要编码。
		void OnBoring();

		/// 技能结束回调，需要发消息给MS
		BOOL OnSkillEnd( long lAddSub );

		/// 设置攻击目标，兼容技能模块DEBUFF伤害
		void SetTarget( long type, const CGUID &guid );

		/// 激活AI，发消息给MS
		void WakeUp();

		/// 受到伤害，会发送消息
		BOOL WhenBeenHurted( long type, const CGUID &guid, DWORD hurt );

		/// 被打死
		BOOL WhenBeenKilled( long type, const CGUID &guid );

	private:
		/// 标志是否是第一次创建
		bool m_bFirstCreated;
	};
}
