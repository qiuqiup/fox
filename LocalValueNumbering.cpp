#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/IR/PassManager.h"

#include <unordered_map>
#include <string>
#include <vector>

using namespace llvm;

//-----------------------------------------------------
// 用于加减乘除的表达式键
//-----------------------------------------------------
struct Expression {
  unsigned Opcode; 
  int LHS;
  int RHS;

  bool operator==(const Expression &other) const {
    return Opcode == other.Opcode && LHS == other.LHS && RHS == other.RHS;
  }
};

namespace std {
  template<>
  struct hash<Expression> {
    size_t operator()(const Expression &expr) const noexcept {
      auto h1 = std::hash<unsigned>()(expr.Opcode);
      auto h2 = std::hash<int>()(expr.LHS);
      auto h3 = std::hash<int>()(expr.RHS);
      size_t result = h1;
      result ^= (h2 + 0x9e3779b9 + (result << 6) + (result >> 2));
      result ^= (h3 + 0x9e3779b9 + (result << 6) + (result >> 2));
      return result;
    }
  };
}

// 帮助函数：把 opcode 转成人类可读字符串
static StringRef getOpcodeName(unsigned op) {
  switch (op) {
    case Instruction::Add:  return "add";
    case Instruction::Sub:  return "sub";
    case Instruction::Mul:  return "mul";
    case Instruction::UDiv: return "udiv";
    case Instruction::SDiv: return "sdiv";
    default:                return "???";
  }
}

// 将 Instruction 转成字符串
static std::string instrToString(const Instruction &I) {
  std::string tmp;
  raw_string_ostream rso(tmp);
  I.print(rso);
  return rso.str();
}

// 帮助函数：给 Value 分配或返回已有编号
static int getOrAssignVN(Value *v,
                         std::unordered_map<Value*, int> &valueNumber,
                         int &currentVN) {
  if (!v) return -1;
  auto it = valueNumber.find(v);
  if (it != valueNumber.end()) {
    return it->second;
  }
  int newId = currentVN++;
  valueNumber[v] = newId;
  return newId;
}

//-----------------------------------------------------
// Pass: LocalValueNumberingPass
//-----------------------------------------------------
struct LocalValueNumberingPass : public PassInfoMixin<LocalValueNumberingPass> {
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &) {
    for (Function &F : M) {
      if (!F.isDeclaration()) {
        runOnFunction(F);
      }
    }
    return PreservedAnalyses::all();
  }

  void runOnFunction(Function &F) {
    errs() << "\nValueNumbering: " << F.getName() << "\n";

    // 1) 收集只关心的指令 => store / load / 四则运算
    std::vector<Instruction*> instructionsToPrint;
    for (auto &BB : F) {
      for (auto &I : BB) {
        if (isa<AllocaInst>(&I) || isa<ReturnInst>(&I) 
            || isa<CallInst>(&I)   || isa<PHINode>(&I)) {
          continue; // 跳过
        }
        // store / load 直接加入
        if (isa<StoreInst>(&I) || isa<LoadInst>(&I)) {
          instructionsToPrint.push_back(&I);
        }
        else if (auto *binOp = dyn_cast<BinaryOperator>(&I)) {
          unsigned opcode = binOp->getOpcode();
          if (opcode == Instruction::Add  ||
              opcode == Instruction::Sub  ||
              opcode == Instruction::Mul  ||
              opcode == Instruction::UDiv ||
              opcode == Instruction::SDiv) {
            instructionsToPrint.push_back(&I);
          }
        }
      }
    }

    // 2) 建立编号表
    std::unordered_map<Value*, int> valueNumber;
    int currentVN = 1;

    // 表达式 -> 编号
    std::unordered_map<Expression, int> expr2VN;

    // 3) 逐条处理并用 formatv 打印
    for (auto *I : instructionsToPrint) {
      std::string instStr = instrToString(*I);

      // 下面演示 formatv("{0,-40}", ...) 来做左对齐、宽度 40
      // 这样后面的编号信息可以对齐到同一列
      // 你也可以根据需要调大或调小这个宽度
      // e.g. "{0,-50}" if你想留更多空格
      errs() << formatv("{0,-40}", instStr);

      if (auto *st = dyn_cast<StoreInst>(I)) {
        // store => ptr = val
        Value *val = st->getValueOperand();
        Value *ptr = st->getPointerOperand();
        int valVN = getOrAssignVN(val, valueNumber, currentVN);
        valueNumber[ptr] = valVN;
        errs() << valVN << " = " << valVN << "\n";
      }
      else if (auto *ld = dyn_cast<LoadInst>(I)) {
        // load => thisInst = ptr
        Value *ptr = ld->getPointerOperand();
        int ptrVN = getOrAssignVN(ptr, valueNumber, currentVN);
        valueNumber[I] = ptrVN;
        errs() << ptrVN << " = " << ptrVN << "\n";
      }
      else if (auto *binOp = dyn_cast<BinaryOperator>(I)) {
        unsigned opcode = binOp->getOpcode();
        int lhsVN = getOrAssignVN(binOp->getOperand(0), valueNumber, currentVN);
        int rhsVN = getOrAssignVN(binOp->getOperand(1), valueNumber, currentVN);

        // 构造表达式
        Expression expr{opcode, lhsVN, rhsVN};
        auto found = expr2VN.find(expr);
        if (found != expr2VN.end()) {
          // 冗余
          int oldVN = found->second;
          valueNumber[I] = oldVN;
          errs() << oldVN << " = " << lhsVN << " "
                 << getOpcodeName(opcode) << " " << rhsVN
                 << " (redundant)\n";
        } else {
          // 新表达式
          int newVN = currentVN++;
          valueNumber[I] = newVN;
          expr2VN[expr] = newVN;
          errs() << newVN << " = " << lhsVN << " "
                 << getOpcodeName(opcode) << " " << rhsVN << "\n";
        }
      }
    }
  }
};

//-----------------------------------------------------
// 插件注册入口
//-----------------------------------------------------
extern "C" LLVM_ATTRIBUTE_WEAK PassPluginLibraryInfo llvmGetPassPluginInfo() {
  return {
    LLVM_PLUGIN_API_VERSION,
    "local-value-numbering",
    LLVM_VERSION_STRING,
    [](PassBuilder &PB) {
      PB.registerPipelineParsingCallback(
        [&](StringRef Name, ModulePassManager &MPM,
            ArrayRef<PassBuilder::PipelineElement>) {
          if (Name == "local-value-numbering") {
            MPM.addPass(LocalValueNumberingPass());
            return true;
          }
          return false;
        }
      );
    }
  };
}
