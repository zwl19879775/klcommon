///
///
#include <stdio.h>

class IComponent {
};

class TimerComponent : public IComponent {
public:
    typedef void (*Callback) (IComponent *t, void*);

    template <typename Fn>
    void Register (IComponent *obj, Fn fn, void *arg) {
        this->arg = arg;
        this->obj = obj;
        this->fn = (Callback) fn;
    }

    void DoCall () {
        fn (this->obj, arg);
    }
public:
    IComponent *obj;    
    Callback fn;
    void *arg;
};

class TestComponent : public IComponent {
public:
    TestComponent () : count (11) { }

    void OnTimeOut (void *arg) {
        printf( "TestComponent::OnTimeOut (%d)(%s)\n", count, (const char*) arg);
    }
    int count;
};

int main () {
    TimerComponent timer;
    TestComponent test;
    char *arg = "hello";
    timer.Register (&test, &TestComponent::OnTimeOut, arg);
    timer.DoCall ();
    return 0;
}

