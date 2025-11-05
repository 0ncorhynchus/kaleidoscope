EXEC = toy
BUILD_DIR = ./build
INCLUDE_DIR = ./include
SOURCE_DIR = ./src

CXX = clang++
CXXFLAGS = -O3 -g -I$(INCLUDE_DIR) $(shell llvm-config --cxxflags)
LDFLAGS = $(shell llvm-config --ldflags --system-libs --libs core)

SRCS = $(wildcard $(SOURCE_DIR)/*.cpp)
OBJS = $(SRCS:$(SOURCE_DIR)/%.cpp=$(BUILD_DIR)/%.o)
DEPS = $(OBJS:%.o=%.d)

.PHONY: all
all: $(EXEC)

.PHONY: format
format:
	clang-format -i *.cpp *.hpp

.PHNOY: clean
clean:
	rm -rf $(EXEC) $(BUILD_DIR)

-include $(DEPS)

$(BUILD_DIR)/%.o: $(SOURCE_DIR)/%.cpp
	@mkdir -p $(@D)
	$(COMPILE.cc) -MMD -o $@ $<

$(EXEC): $(OBJS)
	$(LINK.cc) -o $@ $^