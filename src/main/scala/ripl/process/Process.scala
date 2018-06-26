package ripl.process

import java.nio.file.Files
import java.nio.file.Path
import java.nio.charset.StandardCharsets

import sys.process._

import reduce._

import ripl.ast._
import ripl.parse.Lex
import ripl.parse.Parse
import ripl.reduce.Reduce
import ripl.codegen.CodeGen
import ripl.llvm.pretty.PrettyPrint

case object LexParse {
  def apply(riplSrc: String) = Parse(Lex(riplSrc))
}

case object LexParseReduce {
  def apply(riplSrc: String) = Reduce(LexParse(riplSrc))
}

case object GenerateLlvmAst {
  def apply(riplSrc: String) = {
    val (ast, errors) = LexParseReduce(riplSrc)
    if (errors.isEmpty) {
      Right(CodeGen(ast))
    } else Left(errors)
  }
}

case object GenerateLlvmIr {
  def apply(riplSrc: String) = GenerateLlvmAst(riplSrc).map(PrettyPrint(_))
}

case object Run {
  def apply(path: Path, riplSrc: String) =
    GenerateLlvmIr(riplSrc).map { llvmIrSrc =>
      Files.createDirectories(path.getParent)
      Files.write(path, llvmIrSrc.getBytes(StandardCharsets.UTF_8))
      Process(Seq("lli", path.toString)).!
    }
}
