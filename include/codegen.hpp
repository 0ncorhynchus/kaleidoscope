#pragma once

#include "KaleidoscopeJIT.h"
#include "parser.hpp"
#include "llvm/Analysis/CGSCCPassManager.h"
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include <memory>

class CodeGenerator {
public:
  void Initialize();
  void InitializeModuleAndManagers();
  void InitializeTheJIT();
  void InitializeExternal();
  void PrintModule();

  llvm::Function *getFunction(std::string Name);
  llvm::Value *operator()(NumberExprAST &ast);
  llvm::Value *operator()(VariableExprAST &ast);
  llvm::Value *operator()(BinaryExprAST &ast);
  llvm::Value *operator()(CallExprAST &ast);
  llvm::Value *operator()(IfExprAST &ast);
  llvm::Value *codegen(std::unique_ptr<ExprAST> &ast) {
    return std::visit(*this, ast->node);
  }
  llvm::Function *codegen(const std::unique_ptr<PrototypeAST> &ast);
  llvm::Function *codegen(std::unique_ptr<FunctionAST> ast);

  void HandleDefinition();
  void HandleExtern();
  void HandleTopLevelExpression();

private:
  std::unique_ptr<llvm::LLVMContext> TheContext;
  std::unique_ptr<llvm::IRBuilder<>> Builder;
  std::unique_ptr<llvm::Module> TheModule;
  std::map<std::string, llvm::Value *> NamedValues;
  std::unique_ptr<llvm::FunctionPassManager> TheFPM;
  std::unique_ptr<llvm::LoopAnalysisManager> TheLAM;
  std::unique_ptr<llvm::FunctionAnalysisManager> TheFAM;
  std::unique_ptr<llvm::CGSCCAnalysisManager> TheCGAM;
  std::unique_ptr<llvm::ModuleAnalysisManager> TheMAM;
  std::unique_ptr<llvm::PassInstrumentationCallbacks> ThePIC;
  std::unique_ptr<llvm::StandardInstrumentations> TheSI;
  std::unique_ptr<llvm::orc::KaleidoscopeJIT> TheJIT;
  llvm::ExitOnError ExitOnErr;
  std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
};