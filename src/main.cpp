#include "codegen.hpp"
#include "lexer.hpp"
#include "llvm/Support/TargetSelect.h"

/// top ::= definition | external | expression | ';'
static void MainLoop(CodeGenerator &generator) {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (getNextToken()) {
    case tok_eof:
      return;
    case ';': // ignore top-level semicolons.
      break;
    case tok_def:
      generator.HandleDefinition();
      break;
    case tok_extern:
      generator.HandleExtern();
      break;
    default:
      generator.HandleTopLevelExpression();
      break;
    }
  }
}

int main() {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  InitializeBinopPrecedence();

  CodeGenerator generator;
  generator.InitializeTheJIT();

  // Make the module, which holds all the code.
  generator.InitializeModuleAndManagers();

  generator.InitializeExternal();

  // Run the main "interpreter loop" now.
  MainLoop(generator);

  generator.PrintModule();

  return 0;
}