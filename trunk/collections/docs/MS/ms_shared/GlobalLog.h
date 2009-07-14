///
/// 全局日志类，主要用于记录一些程序运行日志。
/// 该文件位于共用目录中，被GS和MS共用之。
///
class CGlobalLog
{
public:
	/// 写入日志文件
	void Write( const char *fmt, ... );

	/// 写入UI
	void WriteToUI( const char *fmt, ... );

	/// 写入日志服务器
	void WriteToLOGS( const char *fmt, ... );
};
