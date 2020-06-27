//////////////////////////////////////////////////////////////////////
//
//    CodeGenVisitor - Walk the parser tree to do
//                     the generation of code
//
//    Copyright (C) 2019  Universitat Politecnica de Catalunya
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU General Public License
//    as published by the Free Software Foundation; either version 3
//    of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Affero General Public License for more details.
//
//    You should have received a copy of the GNU Affero General Public
//    License along with this library; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
//
//    contact: José Miguel Rivero (rivero@cs.upc.edu)
//             Computer Science Department
//             Universitat Politecnica de Catalunya
//             despatx Omega.110 - Campus Nord UPC
//             08034 Barcelona.  SPAIN
//
//////////////////////////////////////////////////////////////////////

#include "CodeGenVisitor.h"

#include "antlr4-runtime.h"

#include "../common/TypesMgr.h"
#include "../common/SymTable.h"
#include "../common/TreeDecoration.h"
#include "../common/code.h"

#include <string>
#include <cstddef>    // std::size_t

// uncomment the following line to enable debugging messages with DEBUG*
//#define DEBUG_BUILD
#include "../common/debug.h"

// using namespace std;


// Constructor
CodeGenVisitor::CodeGenVisitor(TypesMgr       & Types,
                               SymTable       & Symbols,
                               TreeDecoration & Decorations) :
  Types{Types},
  Symbols{Symbols},
  Decorations{Decorations} {
}

// Methods to visit each kind of node:
//
antlrcpp::Any CodeGenVisitor::visitProgram(AslParser::ProgramContext *ctx) {
  DEBUG_ENTER();
  code my_code;
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  for (auto ctxFunc : ctx->function()) { 
    subroutine subr = visit(ctxFunc);
    my_code.add_subroutine(subr);
  }
  Symbols.popScope();
  DEBUG_EXIT();
  return my_code;
}

antlrcpp::Any CodeGenVisitor::visitFunction(AslParser::FunctionContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  subroutine subr(ctx->ID()->getText());
  codeCounters.reset();
  if (ctx->basictype()) {
      subr.add_param("_result");
  }
  if (ctx->parameters()) {
      std::vector<std::string> params = visit(ctx->parameters());
      for (unsigned int i = 0; i < params.size(); i++)
          subr.add_param(params[i]);
  }
  std::vector<var> && lvars = visit(ctx->declarations());
  for (auto & onevar : lvars) {
      onevar.dump();
      subr.add_var(onevar);
  }
  instructionList && code = visit(ctx->statements());
  code = code || instruction(instruction::RETURN());
  subr.set_instructions(code);
  Symbols.popScope();
  DEBUG_EXIT();
  return subr;
}

antlrcpp::Any CodeGenVisitor::visitParameters(AslParser::ParametersContext *ctx) {
    DEBUG_ENTER();
    std::vector<std::string> params;
    for (unsigned int i = 0; i < ctx->ID().size(); i++) {
        params.push_back(ctx->ID(i)->getText());
    }
    DEBUG_EXIT();
    return params;
}

antlrcpp::Any CodeGenVisitor::visitDeclarations(AslParser::DeclarationsContext *ctx) {
  DEBUG_ENTER();
  std::vector<var> lvars;
  for (auto & varDeclCtx : ctx->variable_decl()) {
    std::vector<var> variousvar = visit(varDeclCtx);
    for (unsigned int i = 0; i < variousvar.size(); i++) {
        lvars.push_back(variousvar[i]);
    }
  }
  DEBUG_EXIT();
  return lvars;
}

antlrcpp::Any CodeGenVisitor::visitVariable_decl(AslParser::Variable_declContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId   t1 = getTypeDecor(ctx->type());
  std::size_t      size = Types.getSizeOfType(t1);
  std::vector<var> lvars;
  for (unsigned int i = 0; i < ctx->ID().size(); i++) {
      var onevar = var{ctx->ID(i)->getText(), size};
      lvars.push_back(onevar);
  }
  DEBUG_EXIT();
  return lvars;
}

antlrcpp::Any CodeGenVisitor::visitStatements(AslParser::StatementsContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  for (auto stCtx : ctx->statement()) {
    instructionList && codeS = visit(stCtx);
    code = code || codeS;
  }
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitAssignStmt(AslParser::AssignStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  
  CodeAttribs && codAtsE1 = visit(ctx->left_expr());
  std::string addr1 = codAtsE1.addr;
  std::string offs1 = codAtsE1.offs;
  instructionList & code1 = codAtsE1.code;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());
  
  CodeAttribs && codAtsE2 = visit(ctx->expr());
  std::string addr2 = codAtsE2.addr;
  std::string offs2 = codAtsE2.offs;
  instructionList & code2 = codAtsE2.code;
  TypesMgr::TypeId tid2 = getTypeDecor(ctx->expr());
  
  if (Types.isIntegerTy(tid2) and Types.isFloatTy(tid1)) {
      std::string tmp = "%" + codeCounters.newTEMP();
      code = code || instruction::FLOAT(tmp, addr2);
      addr2 = tmp;
  }
  
  if (ctx->left_expr()->ident()->expr()) {
      if (not Symbols.isParameterClass(ctx->left_expr()->ident()->ID()->getText())) {
        code = code || instruction::XLOAD(addr1, offs1, addr2);
      }
      else {
          std::string tmp = "%" + codeCounters.newTEMP();
          code = code || instruction::LOAD(tmp, addr1);
          code = code || instruction::XLOAD(tmp, offs1, addr2);
      }
  }
  else if (Types.isArrayTy(tid1) and Types.isArrayTy(tid2)) {
      bool localE1 = Symbols.isLocalVarClass(addr1);
      bool localE2 = Symbols.isLocalVarClass(addr2);
      
      std::string tmpAddrE1 = "%" + codeCounters.newTEMP();
      std::string tmpAddrE2 = "%" + codeCounters.newTEMP();
      
      if (not localE1) code = code || instruction::LOAD(tmpAddrE1, addr1);
      if (not localE2) code = code || instruction::LOAD(tmpAddrE2, addr2);
      
      std::string tmpIndex  = "%"+codeCounters.newTEMP();
      std::string tmpIncrem = "%"+codeCounters.newTEMP();
      std::string tmpSize   = "%"+codeCounters.newTEMP();
      std::string tmpOffset = "%"+codeCounters.newTEMP();
      std::string tmpOffHld = "%"+codeCounters.newTEMP();
      std::string tmpCompar = "%"+codeCounters.newTEMP();
      std::string tmpValue  = "%"+codeCounters.newTEMP();
      
      std::string whileLabel = "while"+codeCounters.newLabelWHILE();
      std::string endWhileLabel = "end"+whileLabel;
      
      code = code || instruction::ILOAD(tmpIndex, "0")
            || instruction::ILOAD(tmpIncrem, "1")
            || instruction::ILOAD(tmpSize, std::to_string(Types.getArraySize(Symbols.getType(addr1))))
            || instruction::ILOAD(tmpOffset, "1");
      
      code = code || instruction::LABEL(whileLabel)
            || instruction::LT(tmpCompar, tmpIndex, tmpSize)
            || instruction::FJUMP(tmpCompar, endWhileLabel)
            || instruction::MUL(tmpOffHld, tmpOffset, tmpIndex);
      
      if (localE2) {
          code = code || instruction::LOADX(tmpValue, addr2, tmpOffHld);
      }
      else {
          code = code || instruction::LOADX(tmpValue, tmpAddrE2, tmpOffHld);
      }
      if (localE1) {
          code = code || instruction::XLOAD(addr1, tmpOffHld, tmpValue);
      }
      else {
          code = code || instruction::XLOAD(tmpAddrE1, tmpOffHld, tmpValue);
      }
      code = code || instruction::ADD(tmpIndex, tmpIndex, tmpIncrem)
            || instruction::UJUMP(whileLabel)
            || instruction::LABEL(endWhileLabel);
  }
  else {
      code = code || instruction::LOAD(addr1, addr2);
  }
  
  code = code1 || code2 || code;
  
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitIfStmt(AslParser::IfStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs     && codAtsE = visit(ctx->expr());
  std::string          addr1 = codAtsE.addr;
  instructionList &    code1 = codAtsE.code;
  instructionList &&   code2 = visit(ctx->statements(0));
  std::string label = codeCounters.newLabelIF();
  std::string labelEndIf = "endif"+label;
  
  if (ctx->ELSE()) {
      std::string labelElse = "else"+label;
      instructionList && code3 = visit(ctx->statements(1));
      code = code1 || instruction::FJUMP(addr1, labelElse) ||
             code2 || instruction::UJUMP(labelEndIf) || instruction::LABEL(labelElse) ||
             code3 || instruction::LABEL(labelEndIf);
  }
  else {
      code = code1 || instruction::FJUMP(addr1, labelEndIf) ||
             code2 || instruction::LABEL(labelEndIf);
  }
  
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWhileStmt(AslParser::WhileStmtContext *ctx) {
    DEBUG_ENTER();
    instructionList code;
    
    CodeAttribs && codAts1 = visit(ctx->expr());
    std::string addr1 = codAts1.addr;
    instructionList & code1 = codAts1.code;
    
    instructionList && code2 = visit(ctx->statements());
    
    std::string label = codeCounters.newLabelWHILE();
    std::string endWhileLabel = "endwhile" + label;
    std::string whileLabel = "while" + label;
    
    code = code || instruction::LABEL(whileLabel);
    code = code || code1;
    code = code || instruction::FJUMP(addr1, endWhileLabel);
    code = code || code2 || instruction::UJUMP(whileLabel);
    code = code || instruction::LABEL(endWhileLabel);
    
    DEBUG_EXIT();
    return code;
}

antlrcpp::Any CodeGenVisitor::visitReadStmt(AslParser::ReadStmtContext *ctx) {
  DEBUG_ENTER();
  
  CodeAttribs && codAtsE = visit(ctx->left_expr());
  std::string addr1 = codAtsE.addr;
  std::string offs1 = codAtsE.offs;
  instructionList & code1 = codAtsE.code;
  
  instructionList & code = code1;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());
  
  bool isArray = false;
  std::string tmp;
  
  if(ctx->left_expr()->ident()->expr()) {
      tmp = addr1;
      addr1 = "%" + codeCounters.newTEMP();
      isArray = true;
  }
  
  if (Types.isCharacterTy(tid1)) {
      code = code || instruction::READC(addr1);
  }
  else if (Types.isIntegerTy(tid1) or Types.isBooleanTy(tid1)) {
      code = code || instruction::READI(addr1);
  }
  else if (Types.isFloatTy(tid1)) { // Float
      code = code || instruction::READF(addr1);
  }
  
  if (isArray) {
      CodeAttribs && codAts = visit(ctx->left_expr()->ident());
      std::string addr = codAts.addr;
      if (not Symbols.isParameterClass(addr)) {
          code = code || instruction::XLOAD(tmp, offs1, addr1);
      }
      else {
          std::string tmp2 = "%" + codeCounters.newTEMP();
          code = code || instruction::LOAD(tmp2, addr);
          code = code || instruction::XLOAD(tmp2, offs1, addr1);
      }
  }
  
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWriteExpr(AslParser::WriteExprContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAt1 = visit(ctx->expr());
  std::string addr1 = codAt1.addr;
  // std::string         offs1 = codAt1.offs;
  instructionList &   code = codAt1.code;
  
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->expr());
  
  if (Types.isCharacterTy(tid1)) {
      code = code || instruction::WRITEC(addr1);
  }
  else if (Types.isFloatTy(tid1)) {
      code = code || instruction::WRITEF(addr1);
  }
  else {
      code = code || instruction::WRITEI(addr1);
  }
  
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWriteString(AslParser::WriteStringContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string s = ctx->STRING()->getText();
  std::string temp = "%"+codeCounters.newTEMP();
  int i = 1;
  while (i < int(s.size())-1) {
    if (s[i] != '\\') {
      code = code ||
	     instruction::CHLOAD(temp, s.substr(i,1)) ||
	     instruction::WRITEC(temp);
      i += 1;
    }
    else {
      assert(i < int(s.size())-2);
      if (s[i+1] == 'n') {
        code = code || instruction::WRITELN();
        i += 2;
      }
      else if (s[i+1] == 't' or s[i+1] == '"' or s[i+1] == '\\') {
        code = code ||
               instruction::CHLOAD(temp, s.substr(i,2)) ||
	       instruction::WRITEC(temp);
        i += 2;
      }
      else {
        code = code ||
               instruction::CHLOAD(temp, s.substr(i,1)) ||
	       instruction::WRITEC(temp);
        i += 1;
      }
    }
  }
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitReturnStmt(AslParser::ReturnStmtContext *ctx) {
    DEBUG_ENTER();
    instructionList code = instructionList();
    if (ctx->expr()) {
        CodeAttribs && codAt1 = visit(ctx->expr());
        std::string addr1 = codAt1.addr;
        instructionList & code1 = codAt1.code;
        code = code1;
        code = code || instruction::LOAD("_result", addr1);
    }
    DEBUG_EXIT();
    return code;
}

antlrcpp::Any CodeGenVisitor::visitFuncStmt(AslParser::FuncStmtContext *ctx) {
    DEBUG_ENTER();
    CodeAttribs && codAts = visit(ctx->func());
    DEBUG_EXIT();
    return codAts.code;
}

antlrcpp::Any CodeGenVisitor::visitLeft_expr(AslParser::Left_exprContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->ident());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitFunctionCall(AslParser::FunctionCallContext *ctx) {
    DEBUG_ENTER();
    CodeAttribs && codAts = visit(ctx->func());
    DEBUG_EXIT();
    return codAts;
}

antlrcpp::Any CodeGenVisitor::visitUnary(AslParser::UnaryContext *ctx) {
    DEBUG_ENTER();
    CodeAttribs && codAts = visit(ctx->expr());
    TypesMgr::TypeId t = getTypeDecor(ctx->expr());
    
    std::string addr = codAts.addr;
    instructionList code = codAts.code;
    
    std::string temp;
    if (ctx->SUB()) {
        if (Types.isIntegerTy(t)) {
            temp = "%" + codeCounters.newTEMP();
            code = code || instruction::NEG(temp, addr);
        }
        else {
            temp = "%" + codeCounters.newTEMP();
            code = code || instruction::FNEG(temp, addr);
        }
    }
    else if (ctx->PLUS()) {
        temp = addr;
    }
    else {
        temp = "%" + codeCounters.newTEMP();
        code = code || instruction::NOT(temp, addr);
    }
    
    CodeAttribs codRet(temp, "", code);
    
    DEBUG_EXIT();
    return codRet;
}

antlrcpp::Any CodeGenVisitor::visitArithmetic(AslParser::ArithmeticContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId t = getTypeDecor(ctx);
  
  CodeAttribs && codAt1 = visit(ctx->expr(0));
  std::string addr1 = codAt1.addr;
  instructionList & code1 = codAt1.code;
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  
  CodeAttribs && codAt2 = visit(ctx->expr(1));
  std::string addr2 = codAt2.addr;
  instructionList & code2 = codAt2.code;
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  
  instructionList && code = code1 || code2;
  
  std::string temp = "%" + codeCounters.newTEMP();
  if (Types.isIntegerTy(t)) {
      if (ctx->MUL()) {
          code = code || instruction::MUL(temp, addr1, addr2);
      }
      else if (ctx->DIV()) {
          code = code || instruction::DIV(temp, addr1, addr2);
      }
      else if (ctx->MOD()){
          std::string temp1 = "%" + codeCounters.newTEMP();
          std::string temp2 = "%" + codeCounters.newTEMP();
          code = code || instruction::DIV(temp1, addr1, addr2);
          code = code || instruction::MUL(temp2, temp1, addr2);
          code = code || instruction::SUB(temp, addr1, temp2);
      }
      else if (ctx->PLUS()){
          code = code || instruction::ADD(temp, addr1, addr2);
      }
      else {
          code = code || instruction::SUB(temp, addr1, addr2);
      }
  }
  else {
      std::string faddr1;
      std::string faddr2;

      if (Types.isIntegerTy(t1)) {
          faddr1 = "%" + codeCounters.newTEMP();
          faddr2 = addr2;
          
          code = code || instruction::FLOAT(faddr1, addr1);
      }
      else if (Types.isIntegerTy(t2)) {
          faddr1 = addr1;
          faddr2 = "%" + codeCounters.newTEMP();
          
          code = code || instruction::FLOAT(faddr2, addr2);
      }
      else {
          faddr1 = addr1;
          faddr2 = addr2;
      }
      
      if (ctx->MUL()) {
          code = code || instruction::FMUL(temp, faddr1, faddr2);
      }
      else if (ctx->DIV()) {
          code = code || instruction::FDIV(temp, faddr1, faddr2);
      }
      else if (ctx->MOD()){
          std::string temp1 = "%" + codeCounters.newTEMP();
          std::string temp2 = "%" + codeCounters.newTEMP();
          code = code || instruction::FDIV(temp1, faddr1, faddr2);
          code = code || instruction::FMUL(temp2, temp1, faddr2);
          code = code || instruction::FSUB(temp, faddr1, temp2);
      }
      else if (ctx->PLUS()){
          code = code || instruction::FADD(temp, faddr1, faddr2);
      }
      else {
          code = code || instruction::FSUB(temp, faddr1, faddr2);
      }
  }

  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitLogical(AslParser::LogicalContext *ctx) {
    DEBUG_ENTER();
    
    CodeAttribs && codAt1 = visit(ctx->expr(0));
    std::string addr1 = codAt1.addr;
    instructionList & code1 = codAt1.code;
    CodeAttribs && codAt2 = visit(ctx->expr(1));
    std::string addr2 = codAt2.addr;
    instructionList & code2 = codAt2.code;
    
    instructionList && code = code1 || code2;
    
    std::string temp = "%" + codeCounters.newTEMP();
    if (ctx->AND()) {
        code = code || instruction::AND(temp, addr1, addr2);
    }
    else {
        code = code || instruction::OR(temp, addr1, addr2);
    }
    
    CodeAttribs codF(temp, "", code);
    DEBUG_EXIT();
    return codF;
}

antlrcpp::Any CodeGenVisitor::visitRelational(AslParser::RelationalContext *ctx) {
    DEBUG_ENTER();
    
    CodeAttribs && codAt1 = visit(ctx->expr(0));
    std::string addr1 = codAt1.addr;
    instructionList & code1 = codAt1.code;
    TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
    
    CodeAttribs && codAt2 = visit(ctx->expr(1));
    std::string addr2 = codAt2.addr;
    instructionList & code2 = codAt2.code;
    TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
    
    instructionList && code = code1 || code2;
    
    std::string temp = "%" + codeCounters.newTEMP();
    //Cas on o els dos son enters o els dos son caracters
    if ((Types.isIntegerTy(t1) and Types.isIntegerTy(t2)) or (Types.isCharacterTy(t1) and Types.isCharacterTy(t2))) {
        if (ctx->EQUAL()) {
            code = code || instruction::EQ(temp, addr1, addr2);
        }
        else if (ctx->LT()) {
            code = code || instruction::LT(temp, addr1, addr2);
        }
        else if (ctx->GT()) {
            std::string temp1 = "%" + codeCounters.newTEMP();
            code = code || instruction::LE(temp1, addr1, addr2);
            code = code || instruction::NOT(temp, temp1);
        }
        else if (ctx->LE()) {
            code = code || instruction::LE(temp, addr1, addr2);
        }
        else if (ctx->GE()) {
            std::string temp1 = "%" + codeCounters.newTEMP();
            code = code || instruction::LT(temp1, addr1, addr2);
            code = code || instruction::NOT(temp, temp1);
        }
        else {
            std::string temp1 = "%" + codeCounters.newTEMP();
            code = code || instruction::EQ(temp1, addr1, addr2);
            code = code || instruction::NOT(temp, temp1);
        }
    }
    //Cas on els dos son booleans
    else if (Types.isBooleanTy(t1) and Types.isBooleanTy(t2)) {
        if (ctx->EQUAL()) {
            code = code || instruction::EQ(temp, addr1, addr2);
        }
        else {
            std::string temp1 = "%" + codeCounters.newTEMP();
            code = code || instruction::EQ(temp1, addr1, addr2);
            code = code || instruction::NOT(temp, temp1);
        }
    }
    //Cas on almenys un d'ells es float
    else {
        std::string faddr1;
        std::string faddr2;
        
        if (Types.isIntegerTy(t1)) {
            faddr1 = "%" + codeCounters.newTEMP();
            faddr2 = addr2;
            
            code = code || instruction::FLOAT(faddr1, addr1);
        }
        else if (Types.isIntegerTy(t2)) {
            faddr1 = addr1;
            faddr2 = "%" + codeCounters.newTEMP();
            
            code = code || instruction::FLOAT(faddr2, addr2);
        }
        else {
            faddr1 = addr1;
            faddr2 = addr2;
        }
        
        if (ctx->EQUAL()) {
            code = code || instruction::FEQ(temp, faddr1, faddr2);
        }
        else if (ctx->LT()) {
            code = code || instruction::FLT(temp, faddr1, faddr2);
        }
        else if (ctx->GT()) {
            std::string temp1 = "%" + codeCounters.newTEMP();
            code = code || instruction::FLE(temp1, faddr1, faddr2);
            code = code || instruction::NOT(temp, temp1);
        }
        else if (ctx->LE()) {
            code = code || instruction::FLE(temp, faddr1, faddr2);
        }
        else if (ctx->GE()) {
            std::string temp1 = "%" + codeCounters.newTEMP();
            code = code || instruction::FLT(temp1, faddr1, faddr2);
            code = code || instruction::NOT(temp, temp1);
        }
        else {
            std::string temp1 = "%" + codeCounters.newTEMP();
            code = code || instruction::FEQ(temp1, faddr1, faddr2);
            code = code || instruction::NOT(temp, temp1);
        }
    }
    
    CodeAttribs codAts(temp, "", code);
    DEBUG_EXIT();
    return codAts;
}

antlrcpp::Any CodeGenVisitor::visitValue(AslParser::ValueContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string temp = "%" + codeCounters.newTEMP();
  
  if (ctx->INTVAL()) {
      code = instruction::ILOAD(temp, ctx->getText());
  }
  else if (ctx->FLOATVAL()) {
      code = instruction::FLOAD(temp, ctx->getText());
  }
  else if (ctx->CHARVAL()) {
      std::string c = ctx->CHARVAL()->getText();
      c.pop_back();
      c.erase(c.begin());
      code = instruction::CHLOAD(temp, c);
  }
  else if (ctx->TRUE()) {
      code = instruction::ILOAD(temp, "1");
  }
  else {
      code = instruction::ILOAD(temp, "0");
  }
  
  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitFunc(AslParser::FuncContext *ctx) {
    DEBUG_ENTER();
    CodeAttribs && codAts = visit(ctx->ident());
    TypesMgr::TypeId t = getTypeDecor(ctx->ident());
    std::string addr = codAts.addr;
    std::string offs = codAts.offs;
    instructionList code = instructionList();
    
    if (not Types.isVoidFunction(t)) {
        code = code || instruction::PUSH();
    }
    
    if (ctx->expr().size() > 0) {
        for (unsigned int i = 0; i < ctx->expr().size(); i++) {
            CodeAttribs &&  codAtE = visit(ctx->expr(i));
            std::string addrE = codAtE.addr;
            std::string offsE = codAtE.offs;
            instructionList codeE = codAtE.code;
            
            TypesMgr::TypeId tE = getTypeDecor(ctx->expr(i));
            TypesMgr::TypeId tPE = Types.getParameterType(t,i);
            
            if (Types.isFloatTy(tPE) and Types.isIntegerTy(tE)) {
                std::string tmpAddr = "%" + codeCounters.newTEMP();
                codeE = codeE || instruction::FLOAT(tmpAddr, addrE);
                addrE = tmpAddr;
            }
            
            if (Types.isArrayTy(tPE)) {
                std::string tmpAddr = "%" + codeCounters.newTEMP();
                codeE = codeE || instruction::ALOAD(tmpAddr, addrE);
                addrE = tmpAddr;
            }
            
            code = code || codeE || instruction::PUSH(addrE);
        }
        
        code = code || instruction::CALL(addr);
        
        for (unsigned int i = 0; i < ctx->expr().size(); i++) {
            code = code || instruction::POP();
        }
        
        if (not Types.isVoidFunction(t)) {
            std::string tmpAddr = "%" + codeCounters.newTEMP();
            code = code || instruction::POP(tmpAddr);
            addr = tmpAddr;
        }
    }
    else {
        code = code || instruction::CALL(addr);
    }
    CodeAttribs codAtF(addr, offs, code);
    DEBUG_EXIT();
    return codAtF;
}

antlrcpp::Any CodeGenVisitor::visitExprIdent(AslParser::ExprIdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = visit(ctx->ident());
  std::string addr = codAts.addr;
  std::string offs = codAts.offs;
  instructionList code = codAts.code;
  if (ctx->ident()->expr()) {
      std::string tmp = "%" + codeCounters.newTEMP();
      if (not Symbols.isParameterClass(ctx->ident()->ID()->getText())){
          code = code || instruction::LOADX(tmp, addr, offs);
      }
      else {
          code = code || instruction::LOAD(tmp, addr);
          code = code || instruction::LOADX(tmp, tmp, offs);
      }
      addr = tmp;
  }
  CodeAttribs codAtF(addr, offs, code);
  DEBUG_EXIT();
  return codAtF;
}

antlrcpp::Any CodeGenVisitor::visitParenthesis(AslParser::ParenthesisContext *ctx) {
    DEBUG_ENTER();
    CodeAttribs && codAts = visit(ctx->expr());
    DEBUG_EXIT();
    return codAts;
}

antlrcpp::Any CodeGenVisitor::visitIdent(AslParser::IdentContext *ctx) {
  DEBUG_ENTER();
  std::string text = (ctx->ID()->getText());
  std::string offs = "";
  instructionList code;
  
  if (ctx->expr()) {
      CodeAttribs && codAtsE2 = visit(ctx->expr());
      offs = codAtsE2.addr;
      code = codAtsE2.code;
      
      std::string tmpOff = "%" + codeCounters.newTEMP();
      
      if (Symbols.isLocalVarClass(text)) {
          code = code || instruction::LOAD(tmpOff, "1");
          code = code || instruction::MUL(tmpOff, offs, tmpOff);
      }
      else {
          std::string tmpAddr = "%" + codeCounters.newTEMP();
          code = code || instruction::LOAD(tmpAddr, text);
          code = code || instruction::LOAD(tmpOff, "1");
          code = code || instruction::MUL(tmpOff, offs, tmpOff);
          text = tmpAddr;
      }
      
      offs = tmpOff;
      
      /*CodeAttribs && codAtsE2 = visit(ctx->expr());
      std::string addr = codAtsE2.addr;
      instructionList code1 = codAtsE2.code;
      
      TypesMgr::TypeId type = getTypeDecor(ctx);
      size_t size = Types.getSizeOfType(Types.getArrayElemType(type));
      
      code = code1;
      
      std::string off = "%" + codeCounters.newTEMP();
      code = code || instruction::ILOAD(off, std::to_string(size));
      code = code || instruction::MUL(off, off, addr);
      
      offs = off;*/
  }
  else {
      code = instructionList();
  }
  CodeAttribs codAts(text, offs, code);
  DEBUG_EXIT();
  return codAts;
}


// Getters for the necessary tree node atributes:
//   Scope and Type
SymTable::ScopeId CodeGenVisitor::getScopeDecor(antlr4::ParserRuleContext *ctx) const {
  return Decorations.getScope(ctx);
}
TypesMgr::TypeId CodeGenVisitor::getTypeDecor(antlr4::ParserRuleContext *ctx) const {
  return Decorations.getType(ctx);
}


// Constructors of the class CodeAttribs:
//
CodeGenVisitor::CodeAttribs::CodeAttribs(const std::string & addr,
					 const std::string & offs,
					 instructionList & code) :
  addr{addr}, offs{offs}, code{code} {
}

CodeGenVisitor::CodeAttribs::CodeAttribs(const std::string & addr,
					 const std::string & offs,
					 instructionList && code) :
  addr{addr}, offs{offs}, code{code} {
}
