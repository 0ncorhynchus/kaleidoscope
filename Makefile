EXEC = toy
CXX = clang++
CXXFLAGS = -O3 -g $(shell llvm-config --cxxflags --ldflags --system-libs --libs core)

.PHONY: all
all: $(EXEC)

.PHONY: format
format:
	clang-format -i *.cpp *.hpp

.PHNOY: clean
clean:
	rm -rf $(EXEC) *.o

$(EXEC): parser.o
	$(CXX) $(CXXFLAGS) -o $@ $^