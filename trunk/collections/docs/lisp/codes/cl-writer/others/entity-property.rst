MMO游戏对象属性设计
======================

:Author: Kevin Lynx
:Date: 5.2.2011

一般的MMORPG中，游戏对象主要包括怪物和玩家。这两类对象在经过游戏性方面的不断“进化”后，其属性数量及与之相关的逻辑往往会变得很巨大。如何将这一块做得既不损失效率，又能保证结构的灵活、清晰、可维护？本文将提供一种简单的结构。

原始结构
-------------

最原始的结构，极有可能为这样::

    Player:     +---------------+
                | property-1    |
                +---------------+
                | property-2    |
                +---------------+
                |     ...       |
                +---------------+
                | operator-1    |
                +---------------+
                | operator-2    |
                +---------------+
                | ...           |
                +---------------+

也就是，一个对象为一个C++类，然后里面直接塞满了各种属性名，然后是针对这个属性的逻辑操作（函数）。其结果就是Player成为巨类。针对这个情况，一直以来我觉得可以使用一种简单的方法来拆分这个类。冠以官腔，称之为Entity-Component-based Desgin。产生这种想法和我的个人技术积累有一定关系，见下文。

Policy-based Design
-----------------------

Policy-based Design，基于决策的设计。这个概念来源于<Modern C++ Design>。虽然这本书讲述的是针对C++模板的使用及设计技巧。但这种思想依然被我潜意识般地用在其他地方。Policy大致来说就是一个小的组件(Component)。它努力不依赖于其他东西，它可能就是个简单的类，它拥有极少的数据结构，及针对这些数据的极少操作接口。举例而言，玩家MP的自动回复功能，就可封装为一个Policy。将许多Policy组合起来，就可完成一个复杂的功能。

这种思想还可指导很多程序结构方面的设计。例如在做功能的接口拆分时，就将每个函数设计得足够小，小到单纯地完成一个功能。一个功能的入口函数，就将之前实现的小函数全部组合起来，然后共同完成功能点。

当然，<Modern C++ Design>里的Policy在表现形式上有所不同。但其核心思想相同，主要体现在 **组合** 特点上。

Entity-Component-based Design
------------------------------

Entity-Component-based Design按照google到的文章，严格来说算是与OOP完全不同的软件设计方法。不过在这里它将按照我的意思重新被解释。

如果说Policy-based Design极大可能地影响着我们平时的细节编码，那么Entity-Component则是直接对游戏对象的结构设计做直接的说明。 **一个游戏对象就是一个Entity。** Entity拥有很少的属性，也许仅包含一个全局标示的ID。 **一个Component则是Entity的某个行为、或者说某个组成部分。** 其实说白了，以玩家为例，一个玩家对象就是一个Entity，而一个MP的自动回复功能就可被包装为一个Component。这个Component可能包含若干与该功能相关的数据，例如回复时间间隔，每次的回复量等。我们往玩家对象这个Entity添加各种Component，也就是给玩家添加各种逻辑功能。

但是，Component之间可能会涉及到交互，玩家对象之外的模块可能也会与玩家内的某个Component交互。子功能点的拆分，不得不涉及到更多的胶水代码，这也算一种代价。

游戏对象属性设计
---------------------

这份属性结构设计，基本就是参考了上面提到的设计思想。整个系统有如下组件::

    Entity:    +-------------------+
               | property-table    |
               +-------------------+
               | component-table   |
               +-------------------+

    Property:  +-------------------+
               | observer-list     |
               +-------------------+ 

    Component: +--------------------+
               | logic-related data |
               +--------------------+
               | logic-related func |
               +--------------------+

意即，所有Entity都包含一个属性表和组件表。这里的属性表并非硬编码的属性数据成员集合，而是一个key-value形式的表。Property包含一个观察者列表，其实就是一系列回调函数，但是这些观察者本质上也是组件，后面会提到。Component正如上文描述，仅包含Component本身实现的功能所需要的数据和函数。整个结构大致的代码如下::

    class Entity {
    private:
        GUID id;
        std::map<std::string, IComponent*> components;
        std::map<std::string, Property*> properties;
    };

    class Property {
    private:
        std::string name;
        Value val;
        std::vector<IComponent*> observers;
    };

    class IComponent {
    public:
        virtual bool Operate (const Args &args) { return false; }
        virtual void OnNotify (const Property &property, const Args &args) {}
    protected:
        std::string name;
        Entity *entity;
    };

属性本身是抽象的，这完全是因为我们将属性统一地放在了一个表里。从而又导致属性的值也需要做一个抽象包装。

因为Component主要是针对游戏对象属性而存在的。而针对游戏对象属性的操作，我将其分为主动操作和被动操作，其对应的组件就被称为主动组件和被动组件。这里的主被动，主要是针对这个组件是通过哪种方式启动的。如果是某个地方直接需要某个组件启动，那么称为主动，例如玩家角色刚上线，需要编码角色属性到客户端，则启动属性更新组件；如果某个组件是因为某个属性的改变而启动，则称为被动，例如当玩家受到伤害，HP非满值时，则需要启动HP自动回复组件。

针对组件的主被动特性，就得到主动接口Operate，被动接口OnNotify。因为不同的组件实现，依赖的启动参数可能完全不一样，所以这里对参数做了Args的封装。Args具体实现时，可以采用类继承方式根据具体应用塞入具体的参数；也可以直接使用tuple来包装不同类型不同个数的参数。

因为有了个被动属性，所以就在属性身上放置了一个Observer列表。当然这里不一定要采用这种细粒度的方式---将Observer绑在属性身上，而可以将observer列表放到Entity身上。接下来看一些具体的用例。

具体用例
~~~~~~~~~~~~~~

**上线属性更新** ::

    // serialize player to client
    // 取得ClientUpdater组件
    IComponent *updater = entity->GetComponent ("ClientUpdater");
    // 更新至client_socket客户端
    updater->Operate (Args (client_socket));

**玩家属性改变** ::

    // hurt player
    // 取得属性存取组件
    IComponent *accessor = entity->GetComponent ("PropertyAccessor");
    // 改变属性
    accessor->Operate (Args ("HP", Value(10))); // decrease 10 HP
    ...
    // dirty property updator component get called
    void DirtyUpdator::OnNotify (const Property &property, const Args &args) {
        // 将此属性放到脏属性容器，稍候统一更新
        dirties.push (property);
    }

代码到真正应用时，可能会加入更多的接口，以下代码情景不追加叙述。 

**游戏对象刚创建** ::

    // in script, or by config file, or hard code, etc...
    entity->AddProperty ("HP", Args (10)) ;
    entity->AddProperty ("MP", Args (5));
    ... 
    entity->AddComponent (componentFactory->Create ("ClientUpdater"));
    entity->AddComponent (componentFactory->Create ("PropertyAccessor"));
    ...

**组件被创建时** ::

    ClientUpdater::ClientUpdater () {
        entity->GetProperty ("HP")->AddObserver (this);
        ...
    }

;;END;;

