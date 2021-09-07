CXX=g++
CXXFLAGS=-O2 -std=c++17 -Wall -Wextra -pedantic

all: parser_v7

parser_v7: parser_v7.cpp
	$(CXX) $(CXXFLAGS) -o parser_v7 parser_v7.cpp

clean:
	rm -f parser_v7 out.json
