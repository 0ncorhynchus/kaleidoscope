#include "KaleidoscopeJIT.h"
#include "lexer.hpp"
#include "parser.hpp"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;

class CodeGenerator {
public:
  void Initialize();
  void InitializeModuleAndManagers();
  void InitializeTheJIT();
  void PrintModule();

  Function *getFunction(std::string Name);
  Value *codegen(NumberExprAST &ast);
  Value *codegen(VariableExprAST &ast);
  Value *codegen(BinaryExprAST &ast);
  Value *codegen(CallExprAST &ast);
  Function *codegen(const std::unique_ptr<PrototypeAST> &ast);
  Function *codegen(std::unique_ptr<FunctionAST> ast);

  void HandleDefinition();
  void HandleExtern();
  void HandleTopLevelExpression();

private:
  std::unique_ptr<LLVMContext> TheContext;
  std::unique_ptr<IRBuilder<>> Builder;
  std::unique_ptr<Module> TheModule;
  std::map<std::string, Value *> NamedValues;
  std::unique_ptr<FunctionPassManager> TheFPM;
  std::unique_ptr<LoopAnalysisManager> TheLAM;
  std::unique_ptr<FunctionAnalysisManager> TheFAM;
  std::unique_ptr<CGSCCAnalysisManager> TheCGAM;
  std::unique_ptr<ModuleAnalysisManager> TheMAM;
  std::unique_ptr<PassInstrumentationCallbacks> ThePIC;
  std::unique_ptr<StandardInstrumentations> TheSI;
  std::unique_ptr<orc::KaleidoscopeJIT> TheJIT;
  ExitOnError ExitOnErr;
  std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
};

void CodeGenerator::InitializeModuleAndManagers() {
  // Open a new context and module.
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>("KaleidoscopeJIT", *TheContext);
  TheModule->setDataLayout(TheJIT->getDataLayout());

  // Create a new builder for the module.
  Builder = std::make_unique<IRBuilder<>>(*TheContext);

  // Create new pass and analysis managers.
  TheFPM = std::make_unique<FunctionPassManager>();
  TheLAM = std::make_unique<LoopAnalysisManager>();
  TheFAM = std::make_unique<FunctionAnalysisManager>();
  TheCGAM = std::make_unique<CGSCCAnalysisManager>();
  TheMAM = std::make_unique<ModuleAnalysisManager>();
  ThePIC = std::make_unique<PassInstrumentationCallbacks>();
  TheSI = std::make_unique<StandardInstrumentations>(*TheContext,
                                                     /* DebugLogging */ true);
  TheSI->registerCallbacks(*ThePIC, TheMAM.get());

  // Add transform passes.
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  TheFPM->addPass(InstCombinePass());
  // Reassociate expressions.
  TheFPM->addPass(ReassociatePass());
  // Eliminate Common SubExpressions.
  TheFPM->addPass(GVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  TheFPM->addPass(SimplifyCFGPass());

  // Register analysis passes used in these transform passes.
  PassBuilder PB;
  PB.registerModuleAnalyses(*TheMAM);
  PB.registerFunctionAnalyses(*TheFAM);
  PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
}

void CodeGenerator::InitializeTheJIT() {
  TheJIT = ExitOnErr(orc::KaleidoscopeJIT::Create());
}

void CodeGenerator::PrintModule() {
  // Print out all of the generated code.
  TheModule->print(errs(), nullptr);
}

// Static variables for codegen.
static CodeGenerator TheGenerator;

/// LogError* - These are little helper functions for error handling.
template <typename T> T LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

Value *LogErrorV(const char *Str) {
  LogError<Value *>(Str);
  return nullptr;
}

Function *CodeGenerator::getFunction(std::string Name) {
  // First, see if the function has already been added to the current module.
  if (auto *F = TheModule->getFunction(Name))
    return F;

  // If not, check whether we can codegen the declaration from some existing
  // prototype.
  auto FI = FunctionProtos.find(Name);
  if (FI != FunctionProtos.end())
    return codegen(FI->second);

  // If no existing prototype exists, return null.
  return nullptr;
}

Value *CodeGenerator::codegen(NumberExprAST &ast) {
  return ConstantFP::get(*TheContext, APFloat(ast.getValue()));
}

Value *CodeGenerator::codegen(VariableExprAST &ast) {
  // Look this variable up in the function.
  Value *V = NamedValues[ast.getName()];
  if (!V)
    LogErrorV("Unknown variable name");
  return V;
}

Value *CodeGenerator::codegen(BinaryExprAST &ast) {
  Value *L = ast.getLHS()->codegen();
  Value *R = ast.getRHS()->codegen();
  if (!L || !R)
    return nullptr;

  switch (ast.getOp()) {
  case '+':
    return Builder->CreateFAdd(L, R, "addtmp");
  case '-':
    return Builder->CreateFSub(L, R, "subtmp");
  case '*':
    return Builder->CreateFMul(L, R, "multmp");
  case '<':
    L = Builder->CreateFCmpULT(L, R, "multmp");
    // Convert bool 0/1 to double 0.0 or 1.0
    return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");

  default:
    return LogErrorV("invalid binary operator");
  }
}

Value *CodeGenerator::codegen(CallExprAST &ast) {
  // Look up the name in the global module table.
  Function *CalleeF = getFunction(ast.getCallee());
  if (!CalleeF)
    return LogErrorV("Unknown function referenced");

  auto &Args = ast.getArgs();

  // If argument mismatch error.
  if (CalleeF->arg_size() != Args.size())
    return LogErrorV("Incorrect # arguments passed");

  std::vector<Value *> ArgsV;
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back())
      return nullptr;
  }

  return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

Function *CodeGenerator::codegen(const std::unique_ptr<PrototypeAST> &ast) {
  auto &Args = ast->getArgs();
  // Make the function type: double(double, double) etc.
  std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext));
  FunctionType *FT =
      FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);
  Function *F = Function::Create(FT, Function::ExternalLinkage, ast->getName(),
                                 TheModule.get());

  // Set names for all arguments.
  unsigned Idx = 0;
  for (auto &Arg : F->args())
    Arg.setName(Args[Idx++]);

  return F;
}

Function *CodeGenerator::codegen(std::unique_ptr<FunctionAST> ast) {
  // Transfer ownership of the prototype to the FunctionProtos map, but keep a
  // reference to it for use below.
  auto &Proto = ast->getProto();
  auto &P = *Proto;
  FunctionProtos[Proto->getName()] = std::move(Proto);
  Function *TheFunction = getFunction(P.getName());
  if (!TheFunction)
    return nullptr;

  if (!TheFunction->empty())
    return (Function *)LogErrorV("Function cannot be redefined.");

  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(BB);

  // Record the function arguments in the NamedValues map.
  NamedValues.clear();
  unsigned Idx = 0;
  for (auto &Arg : TheFunction->args())
    NamedValues[P.getArgs()[Idx++]] = &Arg;

  if (Value *RetVal = ast->getBody()->codegen()) {
    // Finish off the function.
    Builder->CreateRet(RetVal);

    // Validate the generated code, checking for consistency.
    verifyFunction(*TheFunction);

    // Optimize the function.
    TheFPM->run(*TheFunction, *TheFAM);

    return TheFunction;
  }

  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return nullptr;
}

Value *NumberExprAST::codegen() { return TheGenerator.codegen(*this); }
Value *VariableExprAST::codegen() { return TheGenerator.codegen(*this); }
Value *BinaryExprAST::codegen() { return TheGenerator.codegen(*this); }
Value *CallExprAST::codegen() { return TheGenerator.codegen(*this); }

//
// Top-Level parsing and JIT Driver
//

void CodeGenerator::HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (auto *FnIR = codegen(std::move(FnAST))) {
      fprintf(stderr, "Read function definition:");
      FnIR->print(errs());
      fprintf(stderr, "\n");
      ExitOnErr(TheJIT->addModule(
          orc::ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
      InitializeModuleAndManagers();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void CodeGenerator::HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto *FnIR = codegen(ProtoAST)) {
      fprintf(stderr, "Read extern: ");
      FnIR->print(errs());
      fprintf(stderr, "\n");
      FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
    }
  } else {
    // Skip token for eror recovery.
    getNextToken();
  }
}

void CodeGenerator::HandleTopLevelExpression() {
  // Evaluate a top-level expresion into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (codegen(std::move(FnAST))) {
      // Create a ResourceTracker to track JIT'd memory allocated to our
      // anonymous expression -- that way we can free it after executing.
      auto RT = TheJIT->getMainJITDylib().createResourceTracker();

      auto TSM =
          orc::ThreadSafeModule(std::move(TheModule), std::move(TheContext));
      ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
      InitializeModuleAndManagers();

      // Search the JIT for the __anon_expr symbol.
      auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));

      // Get the symbol's address and cast it to the right type (takes no
      // arguments, returns a double) so we can call it as a native function.
      double (*FP)() = ExprSymbol.getAddress().toPtr<double (*)()>();
      fprintf(stderr, "Evaluated to %f\n", FP());

      // Delete the anonymous expression module from the JIT.
      ExitOnErr(RT->remove());
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (getNextToken()) {
    case tok_eof:
      return;
    case ';': // ignore top-level semicolons.
      break;
    case tok_def:
      TheGenerator.HandleDefinition();
      break;
    case tok_extern:
      TheGenerator.HandleExtern();
      break;
    default:
      TheGenerator.HandleTopLevelExpression();
      break;
    }
  }
}

int main() {
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();

  InitializeBinopPrecedence();

  TheGenerator.InitializeTheJIT();

  // Make the module, which holds all the code.
  TheGenerator.InitializeModuleAndManagers();

  // Run the main "interpreter loop" now.
  MainLoop();

  TheGenerator.PrintModule();

  return 0;
}