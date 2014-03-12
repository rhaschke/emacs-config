#include <boost/shared_ptr.hpp>

using namespace boost;

/** function doing stuff */
void func1(int a, double d, std::string sName) {
}

struct A {
	void func() {}
	void func(int a, int b) {}
	float func(int b) {return 1.0}
	float a;
	int   b;
};

int main(int argc, char *argv[])
{
	shared_ptr<A> a;
	float b;
	a->func();
	b = a->fu
	return 0;
}

// (print ac-sources)
// (setq ac-sources '(ac-source-clang))
// (setq ac-sources '(ac-source-semantic))
