
/// 建立处理同步操作的函数表
class CSyncRecvMgr : public MServer::CMsgFuncTable
{
public:
	void BuildFuncTable();
};

/// 建立处理请求回应的函数表
class CResRecvMgr : public MServer::CMsgFuncTable
{
public:
	void BuildFuncTable();
};

