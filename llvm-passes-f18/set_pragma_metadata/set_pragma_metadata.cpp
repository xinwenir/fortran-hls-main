/***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
****************************************************************/

#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include <iostream>
#include <fstream>
#include <llvm/Support/raw_ostream.h>
#include <vector>
#include "llvm/Support/Debug.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Constants.h"
#include <regex>
#include <cstdio>

#define HLS_UNROLL 0
#define HLS_PIPELINE 1



using namespace llvm;

struct SetPragmaMetadata: public ModulePass {
    static char ID;
    SetPragmaMetadata() : ModulePass(ID) {}

    bool runOnModule(Module &M) override;
    void getAnalysisUsage(AnalysisUsage &AU) const override {
        AU.setPreservesAll();
    }
};


struct interface_params {
    std::string mode;
    std::string bundle;
    llvm::Value * port;
};

char SetPragmaMetadata::ID = 0;
static RegisterPass<SetPragmaMetadata> X("set_pragma_metadata", "Set metadata corresponding to the hls pragmas used in the Fortran source code.",
        false /* Only looks at CFG */,
        false /* Transformation Pass */);

// hls_type: 0 - unroll
//			 1 - pipeline
void proc_loop(llvm::Loop * loop, llvm::Instruction * instr, int hls_type, Module &M, bool parameter) {
	llvm::Loop * child_loop = nullptr;

	for(auto &S : loop->getSubLoops()) {
		if (S->contains(instr)) {
			child_loop = S;
			break;
		}
	} 
	if (child_loop) {
		proc_loop(child_loop, instr, hls_type, M, parameter);
	}
	else {
		if (hls_type == HLS_UNROLL) {
			SmallVector<Metadata *, 32> Ops;
			Ops.push_back(llvm::MDString::get(M.getContext(), "placeholder"));
			Ops.push_back(llvm::MDString::get(M.getContext(), "placeholder"));
			llvm::MDNode *MDNodeWrapperLoopUnroll= llvm::MDNode::get(M.getContext(), Ops);
            llvm::MDNode *MDNodeLoopUnroll;
            if(parameter)
                MDNodeLoopUnroll = llvm::MDNode::get(M.getContext(), llvm::MDString::get(M.getContext(), "llvm.loop.unroll.count"));
            else
                MDNodeLoopUnroll = llvm::MDNode::get(M.getContext(), llvm::MDString::get(M.getContext(), "llvm.loop.unroll.full"));
			MDNodeWrapperLoopUnroll->getOperand(0);
			MDNodeWrapperLoopUnroll->replaceOperandWith(0, MDNodeWrapperLoopUnroll);
			MDNodeWrapperLoopUnroll->replaceOperandWith(1, MDNodeLoopUnroll);
			loop->setLoopID(MDNodeWrapperLoopUnroll);
		}
		if (hls_type == HLS_PIPELINE) {
			SmallVector<Metadata *, 32> Ops;
			Ops.push_back(llvm::MDString::get(M.getContext(), "placeholder"));
			Ops.push_back(llvm::MDString::get(M.getContext(), "placeholder"));
			llvm::MDNode *MDNodeWrapperLoopUnroll= llvm::MDNode::get(M.getContext(), Ops);

			SmallVector<Metadata *, 32> OpsPipelineNode;
			OpsPipelineNode.push_back(llvm::MDString::get(M.getContext(), "placeholder"));
			OpsPipelineNode.push_back(llvm::MDString::get(M.getContext(), "placeholder"));
			llvm::MDNode *MDNodeLoopPipelineOuter = llvm::MDNode::get(M.getContext(), OpsPipelineNode);
			llvm::MDNode *MDNodeLoopPipelineInner = llvm::MDNode::get(M.getContext(), llvm::MDString::get(M.getContext(), "llvm.loop.pipeline.enable"));

			llvm::MDNode* temp_N = llvm::MDNode::get(M.getContext(), ConstantAsMetadata::get(ConstantInt::get(M.getContext(), llvm::APInt(32, 76, false))));
			MDNodeLoopPipelineOuter->getOperand(0);
			MDNodeLoopPipelineOuter->replaceOperandWith(0, MDNodeLoopPipelineInner);
			MDNodeLoopPipelineOuter->replaceOperandWith(1, temp_N);

			MDNodeWrapperLoopUnroll->getOperand(0);
			MDNodeWrapperLoopUnroll->replaceOperandWith(0, MDNodeWrapperLoopUnroll);
			MDNodeWrapperLoopUnroll->replaceOperandWith(1, MDNodeLoopPipelineInner);
			loop->setLoopID(MDNodeWrapperLoopUnroll);
		}	
	}
}

bool isKernel(Function * F, std::vector<std::string> kernel_names) {
    for (auto &fname : kernel_names) {
        if((F->getName().find(fname) != std::string::npos)) {
            return true;
        }
    }

    return false;
}

bool SetPragmaMetadata::runOnModule(Module &M) {
    std::ifstream f_kernel_names;
    std::vector<std::string> kernel_names;
    std::string kname;

    f_kernel_names.open("kernel_names.txt");
    while(std::getline(f_kernel_names, kname)) {
        kernel_names.push_back(kname);
    }

    //llvm::Instruction * call_inst;

    std::fstream f_ii;
    std::fstream f_unroll_factor;
    std::string F_str;
    f_ii.open("ii.txt", std::ios::out);
    f_unroll_factor.open("unroll_factor.txt", std::ios::out);

    SmallVector<Instruction*, 32> call_inst;


    // Specifies if the user provided a parameter for pragma unroll or pipeline.
    std::map<llvm::CallInst*, bool> pragma_parameter;

    for(auto &F : M) {
        if(isKernel(&F, kernel_names)) {
            for(auto &B : F) {
                //parameter = false;

                for(auto &I : B) {
                    if(isa<llvm::CallInst>(I)) {
                        auto CI = dyn_cast<llvm::CallInst>(&I);
                        pragma_parameter[CI] = false;

                        if(CI->getCalledFunction()->getName().find("unroll") != std::string::npos) {
                            //llvm::dbgs() << "PRAGMA PARAMETER!!!\n";
                            std::smatch sm;
                            std::regex rgx("unroll_(\\d+)");

                            std::string f_name(CI->getCalledFunction()->getName());

                            std::regex_search(f_name, sm, rgx);
                            raw_string_ostream(F_str) << sm[1] << "\n";
                            f_unroll_factor << F_str;
                            f_unroll_factor.close();
                            F_str.clear();
                            
                            if(!sm.empty())
                                pragma_parameter[CI] = true;
                            else
                                pragma_parameter[CI] = false;

                            //llvm::dbgs() << "PRAGMA PARAMETER: " << pragma_parameter[CI] << "\n";


                            call_inst.push_back(&I); 
                        }

                        if(CI->getCalledFunction()->getName().find("pipeline") != std::string::npos) {
                            std::smatch sm;
                            std::regex rgx("pipeline_(\\d+)");

                            std::string f_name(CI->getCalledFunction()->getName());

                            std::regex_search(f_name, sm, rgx);
                            raw_string_ostream(F_str) << sm[1] << "\n";
                            f_ii << F_str;
                            f_ii.close();
                            F_str.clear();

                            if(!sm.empty())
                                pragma_parameter[CI] = true;
                            else
                                pragma_parameter[CI] = false;

                            call_inst.push_back(&I); 
                        }
                    }
                }
            }
        
    
            llvm::DominatorTree DT(F);
            llvm::LoopInfo LI(DT);

            for(auto &L : LI) {
                for(auto &LB : L->getBlocks()) {
                    for(auto &LInst : *LB) {
                        if(isa<llvm::CallInst>(LInst)) {
                            auto CI = dyn_cast<llvm::CallInst>(&LInst);
                            if(CI->getCalledFunction()->getName().find("unroll") != std::string::npos) {
                                //llvm::dbgs() << "PRAGMA PARAMETER PROC LOOP: " << pragma_parameter[CI] << "\n";
                                proc_loop(L, &LInst, HLS_UNROLL, M, pragma_parameter[CI]);
                            }
                            if(CI->getCalledFunction()->getName().find("pipeline") != std::string::npos) {
                                proc_loop(L, &LInst, HLS_PIPELINE, M, pragma_parameter[CI]);
                            }
                        }
                    }
                }
            }
        }
    }
    f_ii.close();

    auto call_inst_it = call_inst.begin();

    for(auto &F : M) {
        if(F.isDeclaration() && ((F.getName().find("unroll") != std::string::npos) || (F.getName().find("pipeline") != std::string::npos))) {
            F.replaceAllUsesWith(llvm::UndefValue::get((llvm::Type*)F.getType()));
        }
    }
    for(auto &it_callinst : call_inst) {
        it_callinst->eraseFromParent();
    }

    // Process interfaces:
    // Generate file with calls to llvm.sideffects
    std::smatch sm;
    std::fstream f_interfaces;
    F_str.clear();
    f_interfaces.open("interfaces.txt", std::ios::out);

    for(auto &F : M) {
        for(auto &B : F) {
            for(auto &I : B) {
                if(isa<CallInst>(I) && dyn_cast<llvm::CallInst>(&I)->getCalledFunction()->getName().find("_interface") != std::string::npos) {
                    auto callee = dyn_cast<llvm::CallInst>(&I)->getCalledFunction();
                    std::regex rgx("_interface_(\\w+)");

                    std::string f_name(callee->getName());

                    std::regex_search(f_name, sm, rgx);

                    std::string interface_params_str = sm[1];
                    struct interface_params iface;
                    
                    std::smatch params_match;
                    std::regex params_rgx("[a-zA-Z0-9]+");
                    regex_search(interface_params_str, params_match, params_rgx);
                    if(params_match[0] == "maxi")
                        iface.mode = "m_axi";
                    else
                        iface.mode = params_match[0];
                    interface_params_str = params_match.suffix();

                    regex_search(interface_params_str, params_match, params_rgx);
                    iface.bundle = params_match[0];
                    interface_params_str = params_match.suffix();

                    iface.port = dyn_cast<CallInst>(&I)->getArgOperand(0);

                    char buffer[200]; 
                    const char * port;
                    port = iface.port->getName().data();
                    if(iface.mode == "m_axi") {
                        raw_string_ostream(F_str) << "call void @llvm.sideeffect() #5 [ \"xlx_" << iface.mode << "\"(" << *iface.port << ", [5 x i8] c\""<< iface.bundle << "\", i64 -1, [5 x i8] c\"slave\", [0 x i8] zeroinitializer, i64 -1, i64 -1, i64 -1, i64 -1, i64 -1, i64 -1) ]\n";
                    }
                    else if(iface.mode == "axis") {
                        raw_string_ostream(F_str) << "call void @llvm.sideeffect() #5 [ \"xlx_axis\"(" << *iface.port << ", i1 false, i64 2, i64 -1, [0 x i8] zeroinitializer, [0 x i8] zeroinitializer) ]\n";
                    }
                }
            }
        }
    }

    f_interfaces << F_str;
    f_interfaces.close();


    llvm::BasicBlock * df_block = nullptr;
    for(auto &F : M) {
        for(auto &B : F) {
            for(auto &I : B) {
                if(isa<CallInst>(I) && dyn_cast<llvm::CallInst>(&I)->getCalledFunction()->getName().find("_dataflow") != std::string::npos) {
                    df_block = &B;
                }
            }
        }
    }

    llvm::Function * p_start_df_call;
    llvm::Function * p_end_df_call;
    // Get function df_call to signal when a function called is part of the dataflow region
    for(auto &F : M) {
        if(F.getName().find("_start_df_call") != std::string::npos) {
            p_start_df_call = &F;
        }
        if(F.getName().find("_end_df_call") != std::string::npos) {
            p_end_df_call = &F;
        }
    } 

    bool start_dataflow = false;
    bool insert_after = false; // used to control insertion before the next instruction in the next iteration
    std::vector<llvm::Instruction*> df_functions;
    if(df_block) {
        for(auto &I : *df_block) {
            if(insert_after) {
                auto type = llvm::Type::getVoidTy(M.getContext());
                auto f_type = llvm::FunctionType::get(type, false);

                llvm::CallInst::Create(p_end_df_call, "", &I);
                insert_after = false;
            }

            if(start_dataflow && isa<CallInst>(I) && dyn_cast<llvm::CallInst>(&I)->getCalledFunction()->getName().find("_dataflow") == std::string::npos) { // != _dataflow
                auto type = llvm::Type::getVoidTy(M.getContext());
                auto f_type = llvm::FunctionType::get(type, false);
                llvm::CallInst::Create(p_start_df_call, "", &I);

                insert_after = true;
            }
            if(isa<CallInst>(I) && dyn_cast<llvm::CallInst>(&I)->getCalledFunction()->getName().find("_dataflow") != std::string::npos)
                start_dataflow = true;
        }
    }


    return true;
}
