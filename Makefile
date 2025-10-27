EXEC = kaleidoscope
CXX = clang++
CFLAGS = -O3 -g

.PHONY: all
all: $(EXEC)

.PHONY: format
format:
	clang-format -i *.cpp *.hpp

.PHNOY: clean
clean:
	rm -rf $(EXEC) *.o

$(EXEC): parser.o
	$(CXX) $(CFLAGS) -o $@ $^