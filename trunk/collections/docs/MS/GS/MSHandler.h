///
/// 维护GS与MS的通讯相关
/// 
namespace MServer
{
	class CMSHandler
	{
	private:
		///  MS结构
		struct MSInfo
		{
			long id;
			long socketid;
			// 可能会添加更多的域
		};
	public:
		/// 初始化网络服务
		bool Init();

		/// 销毁网络服务等
		void Release();

		/// 处理MS的消息队列
		void ProcessMessage();

		/// 新的MS连接上，该函数会编码数据到MS
		void OnConnect( CMessage *msg );

		/// MS断开
		void OnDisconnect( long socketid );

		/// 根据ID获取Socket ID
		long GetSocketByID( long id );

		/// 根据Socket ID获取ID
		long GetIDBySocket( long socket );
	private:
		const MSInfo *GetMSByID( long id );
		const MSInfo *GetMSBySocket( long socketid );
	private:
		/// 提供从ID和SOCKET分别获取MSInfo的能力
		typedef std::vector<MSInfo> MSInfoListType;
		typedef std::map<long, size_t> MSIDTable;	
		typedef std::map<long, size_t> MSSocketTable;
		MSInfoListType m_MSInfos;
		MSIDTable m_MSIDs;
		MSSocketTable m_MSSockets;
	};

}
