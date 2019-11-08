traceroute: main.o trace.o transfer.o
	g++ -std=c++11 -Wall -Wextra -o traceroute main.o trace.o transfer.o

main.o: main.cpp

trace.o: trace.cpp trace.h

transfer.o: transfer.cpp transfer.h

.PHONY: clean

clean:
	$(RM) traceroute *.o

