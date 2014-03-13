#include <boost/shared_ptr.hpp>
#include <ICLQt/Common.h>
#include <ICLUtils/ConfigFile.h>

using namespace boost;

/** function doing stuff */
void func1(int a, double d, std::string sName) {
}

struct A {
	void func() {}
	void func(int a, int b) {}
	float func(int b) {return 1.0;}
	float a;
	int   b;
};

int main(int argc, char *argv[])
{
	ICLApp app(argc, argv, "");
	shared_ptr<A> a;
	a->func();
	A* pa;
	pa->a;
	float b;
	return 0;
}
