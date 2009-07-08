///
/// CConnector类封装网络连接等细节，包括重连等事务。
///
class CConnector
{
public:
	typedef msgQueue MsgQueueType;
public:
	/// 初始化网络底层
	bool Init();

	/// 释放操作，关闭网络层，关闭重连线程
	void Release();

	/// 连接GS，如果连接失败会建立重连线程；
	/// 重连成功会在消息队列中压入重连成功消息 
	bool Connect( const std::string &ip, unsigned short port );

	/// 获取接收到的所有消息
	size_t GetAllMessage( MsgQueueType &msgs );	

private:
	CServer *m_pServer;
};

