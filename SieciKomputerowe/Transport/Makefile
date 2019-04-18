
CC = g++ 
CXXFLAGS = -Wall -Wextra 

TEST_SIZE = 9027928

transport: main.o transport.o
	$(CC) -std=c++11 $(CXXFLAGS) -o transport main.o transport.o

main.o: main.cpp

transport.o: transport.cpp transport.h


run: transport
	/usr/bin/time -v ./transport 40001 output $(TEST_SIZE)

run-debug: transport
	gdb --args ./transport 40001 output $(TEST_SIZE)

run-profile: transport
	valgrind ./transport 40001 output $(TEST_SIZE)

test: transport
	/usr/bin/time -v ./transport 40001 output1 $(TEST_SIZE)
	/usr/bin/time -v ./transport-faster 40001 output2 $(TEST_SIZE)
	diff output1 output2

.PHONY: clean

clean:
	$(RM) transport *.o

distclean:
	rm -rf *.o output output1 output2 transport $(OBJ)
 