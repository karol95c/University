//============================================================================
// Name        : Vectors.cpp
// Author      :
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
#include <vector>
#include <typeinfo>

using namespace std;

template <typename T>
auto foo(T value) -> T 
{
	return value;
}
int main() {

	vector<double> numbers(0);

	cout << "Size: " << numbers.size() << endl;

	int capacity = numbers.capacity();
	cout << "Capacity: " << capacity << endl;

	for(int i=0; i<10000; i++) {

		if(numbers.capacity() != capacity) {
			capacity = numbers.capacity();
			cout << "Capacity: " << capacity << endl;
		}

		numbers.push_back(i);
	}

	numbers.reserve(100000);
	cout << numbers[2] << endl;
	cout << "Size: " << numbers.size() << endl;
	cout << "Capacity: " << numbers.capacity() << endl;


	auto p = new string("sss");
	cout << typeid(p).name() << endl;
	// decltype(p) s = "JOB";
	// cout << s << endl;
	// cout << typeid(s).name() << endl;
	cout << foo("SS") << endl;
	return 0;
}