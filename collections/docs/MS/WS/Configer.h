///
/// 提供读取/编码各个GS下的各个MS的场景分配配置，其配置文件大致如下：
/*
 <!--setup/rgn_dispatch.xml-->
 <RgnDisConfig>
 	<GS id="1">
		<!--静态配置的场景-->
		<Static>
			<MS id="1">
				<Rgn id="60101" name="学院"/>
				<Rgn id="10101" name="" />
			</MS>
			<MS id="2">
				<Rgn id="10201" name="" />
				<Rgn id="20101" name="" />
			</MS>
		</Static>
		<!--动态配置的场景，一般为副本-->
		<Dynamic>
			<Rgn id="20001" name="利齿谷"/>
		</Dynamic>
	</GS>
	<GS id="2">
		<Static>
			<MS id="1">
				<Rgn id="30101" name=""/>
			</MS>
		</Static>
	</GS>
 </RgnDisConfig>	
*/

namespace MServer
{
	class CConfiger
	{
	private:
		/// <GS id, GS Rgn list>
		typedef std::map<long, GSRgnInfo> GSRgnTableType;	
	public:
		/// 读取配置
		bool Load();

		/// 编码配置给指定的GS
		/// 先找到gs_id对应的GSRgnInfo，然后编码该结构即可
		void Encode( long gs_id, DBWriteSet &db );
	private:
		GSRgnTableType m_GSRgnCfg;
	};
}

