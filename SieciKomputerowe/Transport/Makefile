
CC = g++ 
CXXFLAGS = -Wall -Wextra 

transport: main.o transport.o
	$(CC) -std=c++11 $(CXXFLAGS) -o transport main.o transport.o

main.o: main.cpp

transport.o: transport.cpp transport.h

.PHONY: clean

clean:
	$(RM) transport *.o

distclean:
	rm -rf *.o output output1 output2 transport $(OBJ)
 