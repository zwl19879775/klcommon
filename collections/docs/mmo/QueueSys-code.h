///
/// QueueSys-code.h
/// Code snippets for QueueSys design.
///
/// Kevin Lynx 3.31.2011
///
/// Glossary:
///     Service
///     Queuer, maybe a player, a team etc.
///     Controller
///

/// entity-relational / use-case 
/// Represent a service, and there're many users who want to be served.
class Service
{
    /// There's a queuer queue of course.
    queue<Queuer> queuers;

    /// There's a timer queue which primarly manage a timeout value for every
    /// element(queuer). When some queuer got served, the Service will remove it
    /// from ``users`` list, and push him im this queue. Sometime later, the
    /// system will check whether someone can be served. 
    timer_queue<Queuer> out_queuers;

    /// A service also has an identify value, which maybe a name.
    string name;

    /// Someone wants to be served, put him in the queue.
    void QueueUp (Queuer queuer);

    /// A service can serve more, this function make this Service to serve the
    /// first queuer now. Actually it only notify the queuer: You can go now.
    bool Serve ();

    /// A valid queuer is someone in ``out_queuers``. After checked, remove him.
    bool IsValidQueuer (Queuer queuer);

    /// @see ServiceMethods
    ServiceMethods *methods;
};

/// This class provides an abstraction level to implement a concrete service.
/// All concrete service may create an inherited class from this, to implement
/// some necessary functions.
class ServiceMethods
{
    /// Find a position in the service queue for the queuer. For instance, the
    /// tail of the queue. Every time the queuer request a service, the system
    /// will call this funciton and put the queuer into the queue by the position.
    virtual int FindPosition (Queuer queuer) = 0;

    /// Different service may have different time out interval. This function is
    /// primarly used for `Service::out_queuers`.
    virtual long GetTimeOut () = 0;

    /// Calculate the remain time (how much time to wait) for a queuer, if this
    /// function return -1, means the service do not care about the time.
    virtual long RemainTime (Queuer queuer) = 0;

    /// Event process: some queuer leave the queue.
    virtual void OnLeave (Queuer queuer) = 0;    

    /// Someone enter the queue.
    virtual void OnEnter (Queuer queuer) = 0;

    /// The queue has been changed, this function may notify the other queuers.
    /// And it will call `RemainTime` to get a notify text message which will be 
    /// sent to game client.
    virtual void OnChanged () = 0;

    /// Attached service.
    Service *service;
};

/// An entity who wants to be served sometime later. Actually, a TEAM maybe a
/// queuer too. An queuer can wait for multiple service at the same time?
class Queuer
{
    /// Every queuer has an idenfity value, which will be most implemented as his
    /// GUID (or TEAM GUID).
    Identify id;

    /// Notify some message to a queuer, the message maybe :
    /// `You can go to the real service now, take the service name.`
    /// `The remain time is xx seconds, and there're x persons waiting.`
    virtual void Notify (string msg) = 0;
};

class QueuerPlayer
{
    void Notify (string msg);
};

class QueuerTeam
{
    /// Notify the message to every teamate in this team.
    void Notify (string msg);

    list<Player> teamates;
};


/// A controller is a global management object, also the primary interface of
/// this module.
class Controller
{
    /// Controller manage a list of ``Service``.
    list<Service> services;

    /// We can register a service to be managed by Controller.
    bool RegisterService (string name, string methodName);

    /// A queuer request a service, this function will find the service and push
    /// the queuer in the service queue.
    bool RequestService (string name, Queuer queuer);

    /// A service can serve more. When a service can serve more, it will call
    /// this function to notify.
    bool Serve (string name);

    /// Check whether a queuer is really can be served by `name` service.
    /// This function will find the service first, and call `IsValidQueuer` to 
    /// check the queuer.
    /// When a queuer has been told he can go to ask the real service, the real
    /// service will call `CheckToServe` to check.
    bool CheckToServe (Queuer queuer, string name);
};

