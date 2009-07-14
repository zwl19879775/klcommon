///
/// 负责维护一些配置信息
///
namespace MServer
{
	/// 读取网络配置数据
	class CServerConfiger
	{
	public:
		struct Addr
		{
			std::string ip;
			unsigned short port;
		};
		struct ServerParam
		{
			Addr addr;
			long nMaxFreeSockOperNum;
			// 其他开启网络环境的参数
		};

	public:
		/// 读取GS下的对于MS的网络配置
		bool Load();

		const ServerParam &GetCfg() const
		{
			return m_cfg;
		}
	private:
		ServerParam m_cfg;
	};

	/// 维护场景分配配置
	class CRgnDisConfiger
	{
	public:
		enum
		{
			NOT_DIS = -1
		};
	public:
		/// 解码配置
		void Decode( DBReadSet &db );

		/// 获取场景模板被分配的MS ID，没有找到返回NOT_DIS
		/// 该函数作为场景判断自身是否需要发送到MS上被使用
		long GetDisMS( long rgn_id );

	private:
		/// 建立场景ID到MS ID的映射
		void BuildRgnMSTable();
	private:
		/// 场景到MS ID的映射，可方便以后查询
		typedef std::map<long, long> RgnID2MSID;
	private:
		GSRgnInfo m_rgnCfg;
		RgnID2MSID m_rgnMSTable;
	};
}

