游戏对象属性详细设计
==========================

:Author: Kevin Lynx
:Date: 5.10.2011

.. Note:: 本文具体描述角色属性模块的结构设计，在具体阅读本文之前可参考 本文_ ，也可 `Google Component-Entity-based Design`_ 。本文有些章节仅用于梳理具体实现，主要的解决方案位于 `Component-based Solution`_ 和 角色属性具体修改方案_ 。

.. Contents::

目前属性种类
------------------

* 基本属性 (Shape, BaseObject)

  * 类型
  * GUID
  * 状态
  * 动作
  * 方向
  * 坐标、所在场景信息
  * 速度、速度功能相关数据

* 扩展功能属性 (MoveShape)

  * 受益者
  * 最后攻击时间
  * 任务保护标志
  * 采集信息
  * 最后攻击者
  * 选择目标信息
  * 技能功能相关数据
  * 无敌标志
  * 愚弄者
  * 强制攻击标志
  * 隐蔽
  * 装死
  * 不可见
  * 自控标志
  * ...

* 角色功能属性 (Player)

  * 基础属性(tagPlayerProperty)，存档，职业、性别、等级等
  * 战斗属性(tagPlayerFightProperty)
  * 战斗基础属性（配置）
  * 附加属性
  * 元素属性
  * 职业功能特殊属性
  * 交易功能数据

其他需求
-----------------

* 属性值有效区间
* 对象状态
* 对象动作
* 特殊职业枚举标示？
* 角色属性枚举(属性脚本绑定)？

Component-based Solution
-----------------------------

以下实现细节，是否带来了厚重的胶水层？

* 一个实体除了纯粹的属性外，还有大量与某个功能相关的数据，这些数据可以与Component本身放置在一起
* 要驱动某个功能，原有的结构只需要直接调用某个接口，而新结构将出现如下的使用方式::

    UpdaterComponent *com = player->GetComponent ("Updater");
    com->Process ();

  即要先通过一次Component查找。这里除了可以直接调用某个Component的函数外，还可以直接给Entity发送消息。消息会被派发到与该消息关联的Component上。

* Component之间可能会涉及到交互，这可以通过向Entity发送消息，Entity将消息派发，使Component之间无耦合。以下代码演示移动Component通知事件响应Component，事件类型为"Move"::

    // MoveComponent::Process
    Message msg;
    msg.SetType ("OnEvent");
    msg.SetArgs ( CreateArgs ("Move", x, y));
    m_entity->RecvMessage (&msg);
    ...
    // Entity::RecvMessage
    IComponent *com = GetComponent (msg->Type ());
    if (com) com->OnMessage (msg);

  这要求，Component并不是用任意字符串标示自己。这个标示应该是一种功能描述。多个Component可能使用相同的标示字符串，表示他们实现的功能相同。当然，一个Entity内，一个标示字符串仅对应一个Component。 

  消息交互是否有必要不采用Component名字作为消息类型的机制？而是采用任意消息类型，Component注册自身是否关注某些消息。

* 属性存数据库，只需要实现一个DBAccessorComponent即可::

    // DBAccessorComponent::Save
    dbPlayer->SetLongAttr ("guid", entity->GetProperty ("guid"));
    ...

  保留目前WorldServer数据库操作相关接口

* 整个游戏对象体系，不再有类派生。所有游戏对象直接是一个Entity，某些Entity可能有相同属性，也有相同Component，这只需将共有属性、Component的添加封装到一个公共函数即可（何况还有配置、脚本）::

    void AddCommonPropertyComponent (Entity *entity) {
        entity->AddProperty ("posx", Value (0.0f));
        entity->AddProperty ("posy", Value (0.0f));
        entity->AddComponent ( componentFac->Create ("Mover"));
        ...
    }

    Entity *CreateMonster () {
        Entity *entity = entityFac->Create ();
        AddCommonPropertyComponent (entity);
        ...
        return entity
    }

    Entity *CreatePlayer () { 
        ...
    }

* Entity的管理，在不考虑新的场景结构的情况下，Entity依然按类型被Area和场景所管理，玩家类Entity依然被全局管理
* 定时器机制，定时器将直接由Component自身管理，按目前定时器的使用方式，需要使用定时器的Component，则派生GameEvent

基本Component
-----------------------

这里描述针对新项目的一些基础Component，用于具体实现。以下Component主要对应MoveShape的功能：

* 速度管理（级别控制？）
* 坐标管理
* 阻挡管理
* *受益者管理?*
* 删除控制
* 属性定时更新
* *技能管理*
* 脚本变量列表
* 编解码

以下Component对应Player的功能：

* 脏数据更新
* 数据广播
* *物品管理*
* 移动管理
* *职业管理*
* *副职业管理*
* *任务*
* *组队*
* *仓库*
* *工会*

以下Component对应Monster的功能：

* 脏数据更新
* 数据广播
* 移动管理
* *战斗数据管理（技能列表、战斗标志信息等）*

角色属性具体修改方案
------------------------

该方案中，暂不对整个Shape体系进行完全的Entity-Component-based修改。这里的修改仅仅主要针对以下内容：

* 对新增的大部分属性采用table管理
* 针对属性操作的Component管理
* 属性映射表的修改
* 编解码相关
* 其他细节

该修改方案希望在结构上能尽量靠近Entity-Component-based的方式，以期在将来能逐步将整个Shape体系修改。设计中依然包含Entity，Shape类体系中将内聚Entity。针对属性及属性操作的逻辑将主要封装于Entity中。::

    class Entity {
        std::map<std::string, Property*> m_properties;
        std::map<std::string, IComponent*> m_components;
        int m_type;
        GUID m_id;
    };


新增属性管理
~~~~~~~~~~~~~~~~~~~

如果为属性增加Observer，那么将需要对属性(Property)作特殊包装。此外，如果有对属性范围限定的功能需求，也会有对属性进行单独包装的要求，所以::

    class Property {
    public:
        void Set (const GValue &val);
        GValue Get () const;
        void AddObserver (IComponent *com);
    private:
        std::string m_name;
        std::list<IComponent*> m_observers;
        GValue m_val;
    };

对Property的管理，如果有必要，可以加入单独的PropertyTable。目前可以暂定让Entity充当这个管理角色::

    class Entity {
    public:
        void AddProperty (const std::string &name, const GValue &initval);
        void RemoveProperty (const std::string &name);
        GValue GetPropertyVal (const std::string &name) const;
        void SetPropertyVal (const std::string &name, const GValue &val);
        void AddPropertyObserver (const std::string &name, IComponent *observer);
    private:
        std::map<std::string, Property*> m_properties;
        std::map<std::string, IComponent*> m_components;
        int m_type;
        GUID m_id;
    };

属性的添加分为静态类添加和动态添加。静态添加用于添加大部分属性，其实现方式可以为一个配置或者初始脚本。在创建一个Entity的时候，就由此配置添加属性。动态添加主要用于职业特殊属性的添加，可能发生于角色转职后，可以通过脚本接口添加。

初始添加的属性，可以采用如下方式实现::

    class PropertyTemplate {
    public:
        // 由脚本调用，脚本内组建ParamTable，然后传入
        void Add (int type, const ParamTable *pt);
        // 根据Entity类型找到对应的属性集，然后给Entity建立属性集
        bool AddToEntity (Entity *entity);4
    private:
        // <type, property-set>
        std::map<int, ParamTable*> m_ptable;
    };

PropertyTemplate作为一个单件，其内部将保存所有类型Entity的属性模板。当新创建一个Entity时，就取出对应的属性集合添加之。在脚本里，可以通过如下方式建立此模板::

    -- 服务器启动时会调用的脚本
    local pt = ParamTable.new ()
    pt.name = "noname"
    pt.hp = 0
    pt.mp = 0
    pt.level = 0
    ...
    AddPropertyTemplate (TYPE_PLAYER, pt)

动态添加的属性，一般在角色转职后触发的脚本里进行，大致过程如下::

    -- 转职触发等脚本
    function OnChangeOccu (entity, occu)
        AddProperty (entity, "energy", 100)
        ...
    end

针对属性操作的Component
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

关于Component的设计，可以参考上文及文末的参考文献。具体到这里的属性集，暂无复杂的Component需求。目前可能包含的Component有：脏属性收集Component。

脏属性收集Component是个被动的Component，它作为一部分属性的Observer存在。::

    class DirtyUpdater : public IComponent {
    public:
        DirtyUpdater (Entity *entity) {
            // 向客户端需要的属性添加Observer
            entity->AddPropertyObserver ("HP", this);
            ...
        }

        // 这里的ParamTable作为附加参数包装，无参数时为NULl
        virtual void OnNotify (const Property &p, const ParamTable *pt) {
            m_dirties[p.Name()] = p->Get ();
            if (pt && pt->Get ("immedi-flag").GetBool ()) {
                // 立即更新
                ... 
            }
            else {
                // 注册定时器延迟更新
            }
        }
    };

Component的设计要求尽可能向脚本编写逻辑的方向靠近。因此，应该提供一种机制，可以让脚本往Entity身上添加一个脚本内实现的Component。

一种可选的实现方案如下::

    -- 创建一个脚本Component
    function create_component ()
        local com = {}
        com.timer_id = 0
        com.OnCreated = function (arg_table)
            ... something
            end
        com.OnNotify = function (arg_table)
            ... something
            end
        com.Process = function (arg_table)
            ... something
            end
        return com
    end

    -- 初始化脚本，当某个Entity刚被创建时调用
    function OnPlayerEntityCreated (entity)
        local com = create_component ()
        AddScriptComponent (entity, com)
    end

程序在实现以上功能时，需要一个script component的包装器::

    class ScriptComponent : public IComponent {
    public:
        ScriptComponent (Entity *entity, int sref) {
            m_script = sref;
            ParamTable pt;
            pt.Set ("entity", entity->GetID ());
            CallMethod ("OnCreated", pt);
        }

        virtual void Process (const ParamTable *pt) {
            CallMethod ("Process", pt);
        }

        virtual void OnNotify (const Property &p, const ParamTable *pt) {
            CallMethod ("OnNotify", pt);
        }

        void CallMethod (const char *method, const ParamTable *args) {
            // 取得script component表本身
            lua_rawgeti (L, LUA_REGISTRYINDEX, m_script);
            // 取得其成员函数
            lua_getfield (L, -1, method);
            // 压入参数
            PushTable (args);
            // 调用之
            lua_pcall (L, 1, 0, 0);
        }
    private:
        int m_script;
        lua_State *L;
    };

而AddScriptComponent，则是创建上面的script component包装器::

    int AddScriptComponent (lua_State *L) {
        // 取出entity
        Entity *entity = xxx
        // 将脚本script component保存起来
        int sref = luaL_ref (L, LUA_REGISTRYINDEX);
        // 将包装器作为component添加到entity
        entity->AddComponent (new ScriptComponent (entity, sref));
        ...
    }

**Component的管理** ，在管理方面，虽然Component同Property一样，即在某种角度来看，应该和Entity隔离。但因为一个Entity的Component不会太多，同时也没多大必要通过配置/脚本来配置一个Entity具体使用哪些Component。一个具有完整功能的Entity，其Component基本上是固定的。所以，综合来看，Component可以不必采用Component factory，或者其他抽象机制去隔离Entity和Component。可以直接使用如下方案::

    Entity *CreatePlayerEntity () {
        Entity *entity = new Entity ();
        entity->SetID (CreateGUID ());
        entity->SetType (TYPE_PLAYER); //同样，在类型方面，因为不存在类型膨胀，所以可以直接使用枚举
        // 为Entity添加属性，属性由脚本事先配置
        GetInst (PropertyTemplate).AddToEntity (entity);
        // 添加MoveShape类别的公共Component
        AddShapeComponents (entity);
        // 添加Player特有Component
        entity->AddComponent (new PlayerMoveCom (entity));
        ...
        return entity;
    }

目前暂时没有动态添加、更换Component的需要。一个Entity在被销毁时，则同时销毁其所有的Component。

属性映射表
~~~~~~~~~~~~~~~~~~~~~~~~~~~

属性映射表指的是将属性以字符串作为标示管理起来，主要用于在脚本环境里通过属性名即可存取该属性的值。新的结构里，因为属性本身就是由字符串来标示，所以完全不需要这个映射表。

但并非所有属性都使用新结构的管理方式。有很多属性直接作为Shape的一个数据成员，同样，也许将来会处于效率考虑，依然将部分属性作为类数据成员表示。所以，这里需要一种机制，让所有属性对脚本的接口表现一致。

解决方法为，为这些属性---姑且称为静态属性---添加Observer。而Property表里一直保存属性值。任意时刻，对这些静态属性的存取，都通过Entity上的属性存取接口。对某个属性改写时，通过触发调用其对应的Observer，在Observer里对静态属性对应的数据成员作真正的赋值。采用这种方式，实质上在慢慢废除这些数据成员实现。如果某个地方直接对数据成员进行了设置，将导致属性表中对应的属性值无法得到更新。针对这个问题，可以采用一种集中更新的方式解决。整个过程对应代码描述如下::

    // i.e Player class
    class Player {
        float m_x;
        float m_y;
        Entity *m_entity;
    }

    Entity *CreatePlayerEntity () {
        ...
        GetInst (PropertyTemplate).AddToEntity (entity);
        // 大部分属性可以通过脚本/配置添加，也有部分可以通过代码添加
        // x<->m_x, y<->m_y
        entity->AddProperty ("x", CreateGValue (0.0));
        entity->AddProperty ("y", CreateGValue (0.0));
        ...
        entity->AddComponent (new StaticMapCom (entity));
        ...
    } 

    StaticMapCom::StaticMapCom (Entity *entity) {
        entity->AddPropertyObserver ("x", this);
        entity->AddPropertyObserver ("y", this);
    }

    void StaticMapCom::OnNotify (const Property &p, const ParamTable *args) {
        if (p.Name() == "x") {
            m_entity->GetOwner()->SetPosX (p.Get ().GetNumber ());
        }
        ...
    }

更多具体的操作，例如属性值在脚本和程序间的传递，均由GValue及其相关逻辑实现，这里不做讨论。

编解码相关
~~~~~~~~~~~~~~~~~~~~~

编解码涉及到与GS与Client、GS与WS之间的数据编解码。虽然WS和GS之间可以完全采用key-value的形式编码属性，但在GS与客户端之间，考虑到数据包大小问题，依然采用只编码值的方式。综合来看，目前暂时不改变所有编解码协议，即全部编码值。

要完成这个功能，可以通过添加若干Component来实现::

    class ClientEncoder : public IComponent {
    public:
        void Process (const ParamTable *pt) {
            DBWriteSet *db = (DBWriteSet*) pt->Get ("DB").p;
            db->AddToByteArray ((long) m_entity->GetPropertyVal ("Hp").GetNumber ());
            ...
        }2
    };

    class ServerEncoder : public IComponent {
        ...
    };

问题
~~~~~~~~~~~~~

* **Component之间的交互方式** ，Component之间通过消息进行交互，消息本质上用于封装交互类型及相关参数。虽然这样做，可以让Component之间彼此隔离，但其厚重的交互方式会不会成为性能隐患？整个交互过程导致为::

    Component A -----> 
                       Construct message ---> 
                                              Find the message observer in entity
                                                        -----> 
                                                               Call the observer (also a  Component)
                                                                        -----> Process it

同样，外界在与一个Entity交互时，本质上也是在与其下的某个Component交互，可选的交互方式为::

    ClientEncoder *encoder = entity->GetComponent ("ClientEncoder");
    encoder->Process (xxx);

因为这类代码一般位于Entity之外（也位于Component之外），所以虽然依赖到具体Component（这里是ClientEncoder），从设计角度来看，也不是问题。也许，Component之间的交互，也可以使用这种方式。但如果Component之间的交互本来就不多，则可以使用消息机制来作更高层的抽象隔离。此外，Component之间的显示依赖，也会导致在一定时间多人的功能开发后，Component彼此的依赖关系变得混乱。最坏的结果就是Component依赖关系严重，依然会改一处动全身。

所以，个人更偏重于，牺牲部分效率，增加中间层，从而让其更具扩展性。

* **对静态属性的兼容设计** ，那些直接是数据成员的属性，即静态属性，上文提到的设计方案，本身偏重于废除这些静态成员。而一个严重问题是，一旦使用直接的接口设置这些数据成员，将使得属性表中对应的属性值得不到更新。一种简单的解决方法就是修改这些接口，使其同时更新数据成员本身，又更新属性表中的值。另一种方法是，在从属性表中请求其值时，检测其值与对应的数据成员的值是否不同，若不同则更新属性表中的值。后一种方法看似更好，修改也更容易---因为只需要在新结构中作修改，但它会破坏结构。因为目前并没有监视“获取属性“这个请求（OnNotify监视了属性变化）。

可酌情考虑加入与OnNotify对应的OnQuery（或统一成OnAccess）。

* **编解码时的查询效率** ，该设计表明，在编解码属性时，会涉及到对所有需要编码的属性做查找。虽然在一个仅有100个属性左右的Entity而言，在使用hash查找的情况下，效率似乎并非在此阶段担心的问题。但相比直接编码一段内存（具体实现是结构体）而言，其效率比还是很大。如果这方面确实需要改进，可以事先建立映射表，该映射表大致实现为::

    // 每个元素指向属性表里的元素，其顺序即为编码时候的顺序
    std::vector<Property*> table;
    // 具体编码实现
    // i.e ID
    db->AddToByteArray (table[0]->Get ().GetNumber ());
    // i.e Level
    db->AddToByteArray (table[0]->Get ().GetNumber ());
    ...

 ;;END;

.. _本文: http://codemacro.com/blog/display?id=8
.. _Google Component-Entity-based Design: http://www.google.com/#hl=en&sugexp=ldymls&xhr=t&q=component+entity+game&cp=21&qe=Y29tcG9uZW50IGVudGl0eSBnYW1l&qesig=Ml_0bTVmlfIrw_qM-Gv-Qw&pkc=AFgZ2tn6lTF-VfKOCecPMgh0UZNuKox4VirPT744C3Oa_WsMiVwpqs2MuEXUCQtpVf_-eOdD8v7ofH3ya5Z5Uhp4BhnerytRMQ&pf=p&sclient=psy&site=&source=hp&aq=0v&aqi=&aql=&oq=component+entity+game&pbx=1&fp=942281ed020bcb5c&biw=1190&bih=619


