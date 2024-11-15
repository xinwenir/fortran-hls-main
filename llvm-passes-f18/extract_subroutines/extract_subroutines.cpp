/***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
****************************************************************/

#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instruction.h"
#include "llvm/Pass.h"
#include <iostream>
#include <fstream>
#include <llvm/Support/raw_ostream.h>
#include <vector>

using namespace llvm;

struct FunctionExtractor: public ModulePass {
    static char ID;
    FunctionExtractor() : ModulePass(ID) {}

    bool runOnModule(Module &M) override;
    void getAnalysisUsage(AnalysisUsage &AU) const override {
        AU.setPreservesAll();
    }
};

char FunctionExtractor::ID = 0;
static RegisterPass<FunctionExtractor> X("extract_functions", "Extract function bytecode",
        false /* Only looks at CFG */,
        true /* Transformation Pass */);

bool FunctionExtractor::runOnModule(Module &M) {
    std::fstream output_file;

    // Collect declarations. These have to be preserved in the IR for every output file.
    std::vector<Function*> declarations;
    std::string decl_str;
    for(auto &F : M) {
        if(F.isDeclaration()) {
            raw_string_ostream(decl_str) << F;

            declarations.push_back(&F);
        }
    }

    std::string F_str;
    std::fstream globals_file;
    std::fstream decls_file;

    globals_file.open("tmp/globals.ll", std::ios::out);
    for(auto &G: M.getGlobalList()) {
        F_str.clear();
        raw_string_ostream(F_str) << G << "\n";
        globals_file << F_str;
    }
    globals_file.close();

    decls_file.open("tmp/decls.ll", std::ios::out);
    for(auto &decl : declarations) {
        F_str.clear();
        raw_string_ostream(F_str) << *decl;
        decls_file << F_str;
    }
    decls_file.close();

    std::fstream function_names_file;
    function_names_file.open("tmp/_functions_names.tmp", std::ios::out);
    for(auto &F : M) {
        F_str.clear();
        raw_string_ostream(F_str) << F.getName().str() << "\n";
        function_names_file << F_str;

        output_file.open("tmp/" + F.getName().str() + ".ll", std::ios::out);
        for(auto &decl : declarations) {
            F_str.clear();
            raw_string_ostream(F_str) << *decl;    
            output_file << F_str;
        }

        F_str.clear();
        raw_string_ostream(F_str) << F;
        output_file << F_str;
        output_file.close();
    }
    function_names_file.close();

    return true;
}
