#pragma once

#include "llvm/IR/Value.h"
#include <map>
#include <memory>
#include <variant>

struct ExprAST;

/// NumberExprAST - Expression class for numeric literals like "1.0".
struct NumberExprAST {
  NumberExprAST(double Val) : Val(Val) {}
  double Val;
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
struct VariableExprAST {
  VariableExprAST(std::string Name) : Name(Name) {}
  std::string Name;
};

/// BinaryExprAST - Expression class for a binary operator.
struct BinaryExprAST {
  BinaryExprAST(char Op, std::unique_ptr<ExprAST> &&LHS,
                std::unique_ptr<ExprAST> &&RHS)
      : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;
};

/// CallExprAST - Expression class for function calls.
struct CallExprAST {
  CallExprAST(std::string Callee, std::vector<std::unique_ptr<ExprAST>> &&Args)
      : Callee(std::move(Callee)), Args(std::move(Args)) {}
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;
};

using ExprNode =
    std::variant<NumberExprAST, VariableExprAST, BinaryExprAST, CallExprAST>;

struct ExprAST {
  ExprAST(ExprNode &&node) : node(std::move(node)) {}
  ExprNode node;
};

/// PrototypeAST - This class represents teh "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;

public:
  PrototypeAST(const std::string &Name, std::vector<std::string> Args)
      : Name(Name), Args(std::move(Args)) {}

  const std::string &getName() const { return Name; }
  const std::vector<std::string> &getArgs() const { return Args; }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body)
      : Proto(std::move(Proto)), Body(std::move(Body)) {}

  std::unique_ptr<PrototypeAST> &getProto() { return Proto; }
  std::unique_ptr<ExprAST> &getBody() { return Body; }
};

/// getCurrentToken returns the current token the parser is looking at.
int getCurrentToken();

/// getNextToken reads another token from the Lexer
/// and updates the current token with its results.
int getNextToken();

void InitializeBinopPrecedence();

std::unique_ptr<FunctionAST> ParseDefinition();
std::unique_ptr<PrototypeAST> ParseExtern();
std::unique_ptr<FunctionAST> ParseTopLevelExpr();