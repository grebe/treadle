// See LICENSE for license details.

package treadle

import java.io.{File, PrintWriter}

import firrtl.graph.{CyclicException, DiGraph}
import treadle.vcd.VCD
import logger.Logger
import treadle.chronometry.UTC
import treadle.executable.{ClockInfo, ExecutionEngine, Symbol, SymbolTable, TreadleException}
import treadle.repl._
import treadle.utils.ToLoFirrtl

import scala.collection.mutable.ArrayBuffer
import scala.tools.jline.console.ConsoleReader
import scala.tools.jline.console.history.FileHistory
import scala.tools.jline.{Terminal, TerminalFactory}
import scala.tools.jline.console.completer._
import collection.JavaConverters._
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

abstract class Command(val name: String) {
  def run(args: Array[String]): Unit
  def usage: (String, String)
  def completer: Option[ArgumentCompleter] = {
    Some(new ArgumentCompleter(
      new StringsCompleter({name})
    ))
  }
}

class TreadleRepl(val optionsManager: TreadleOptionsManager with HasReplConfig) {
  val replConfig: ReplConfig = optionsManager.replConfig
  def treadleOptions: TreadleOptions = optionsManager.treadleOptions

  treadle.random.setSeed(treadleOptions.randomSeed)

  val terminal: Terminal = TerminalFactory.create()
  val console = new ConsoleReader
  private val historyPath = "~/.treadle_repl_history".replaceFirst("^~",System.getProperty("user.home"))
  val historyFile = new File(historyPath)
  if(! historyFile.exists()) {
    println(s"creating ${historyFile.getName}")
    historyFile.createNewFile()
  }
  val history = new FileHistory(historyFile)

  history.load(historyFile)
  console.setHistory(history)

  var currentTreadleTesterOpt: Option[TreadleTester] = None
  def currentTreadleTester: TreadleTester = currentTreadleTesterOpt.get

  def engine: ExecutionEngine = currentTreadleTesterOpt match {
    case Some(tester) => tester.engine
    case _ =>
      throw TreadleException(s"No file currently loaded")
  }

  var args = Array.empty[String]
  var done = false

  var inScript = false
  val scriptFactory = ScriptFactory(this)
  var currentScript: Option[Script] = None
  val IntPattern: Regex = """(-?\d+)""".r

  var currentSymbols: String = ""

  var currentVcdScript: Option[VCD] = None
  var replVcdController: Option[ReplVcdController] = None

  var outputFormat: String = optionsManager.replConfig.outputFormat

  def formatOutput(value: BigInt): String = {
    outputFormat match {
      case "d" => value.toString
      case "h" | "x" => f"0x$value%x"
      case "b" => s"b${value.toString(2)}"
    }
  }

  def loadSource(input: String): Unit = {
    currentTreadleTesterOpt = Some(TreadleTester(input, optionsManager))
    currentTreadleTesterOpt.foreach { _ =>
      engine.setVerbose(treadleOptions.setVerbose)
    }
    buildCompletions()
    buildClockInfoList()
  }

  def loadFile(fileName: String): Unit = {
    var file = new File(fileName)
    if(! file.exists()) {
      file = new File(fileName + ".fir")
      if(! file.exists()) {
        throw new Exception(s"file $fileName does not exist")
      }
    }
    val input = io.Source.fromFile(file).mkString
    loadSource(input)
  }

  def loadScript(fileName: String): Unit = {
    currentScript = scriptFactory(fileName)
    currentScript match {
      case Some(script) =>
        console.println(s"loaded script file ${script.fileName} with ${script.length} lines")
      case _ =>
    }
  }

  def loadVcdScript(fileName: String): Unit = {
    val dutName = currentTreadleTesterOpt match {
      case Some(tester) => tester.engine.ast.main
      case None => ""
    }
    try {
      currentVcdScript = Some(VCD.read(fileName, dutName))
      replVcdController = Some(new ReplVcdController(this, this.engine, currentVcdScript.get))
      println(s"vcd script $fileName loaded")
    }
    catch {
      case e: Exception =>
        console.println(s"Failed to load vcd script $fileName, error: ${e.getMessage}")
    }
  }

  def parseNumber(numberString: String): BigInt = {
    def parseWithRadix(numString: String, radix: Int): BigInt = {
      BigInt(numString, radix)
    }

    if(numberString.startsWith("0x"))     { parseWithRadix(numberString.drop(2), 16) }
    else if(numberString.startsWith("h")) { parseWithRadix(numberString.drop(1), 16) }
    else if(numberString.startsWith("o")) { parseWithRadix(numberString.drop(1), 8) }
    else if(numberString.startsWith("b")) { parseWithRadix(numberString.drop(1), 2) }
    else                                  { parseWithRadix(numberString, 10) }
  }

  val resetName: String = treadleOptions.resetName

  val wallTime = UTC()
  wallTime.onTimeChange = () => {
    engine.vcdOption.foreach { vcd =>
      vcd.setTime(wallTime.currentTime)}
  }

  var clockInfoList: Seq[ClockInfo] = Seq.empty

  def buildClockInfoList(): Unit = {
    clockInfoList = if (treadleOptions.clockInfo.isEmpty) {
      if (engine.symbolTable.contains("clock")) {
        Seq(ClockInfo())
      }
      else if (engine.symbolTable.contains("clk")) {
        Seq(ClockInfo("clk"))
      }
      else {
        Seq()
      }
    }
    else {
      treadleOptions.clockInfo
    }

    clockInfoList.foreach { clockInfo =>
      engine.symbolTable.get(clockInfo.name) match {
        case Some(clockSymbol) =>
          val downOffset = clockInfo.initialOffset + (clockInfo.period / 2)

          wallTime.addRecurringTask(clockInfo.period, clockInfo.initialOffset, taskName = s"${clockInfo.name}/up") {
            () =>
            engine.makeUpToggler(clockSymbol).run()
            engine.inputsChanged = true
          }

          wallTime.addRecurringTask(clockInfo.period, downOffset, taskName = s"${clockInfo.name}/down") { () =>
            engine.makeDownToggler(clockSymbol).run()
            engine.inputsChanged = true
          }

        case _ =>
          throw TreadleException(s"Could not find specified clock ${clockInfo.name}")

      }
    }
  }

  val combinationalDelay: Long = 10

  def reset(timeRaised: Long): Unit = {
    engine.setValue(resetName, 1)
    engine.inputsChanged = true

    wallTime.addOneTimeTask(wallTime.currentTime + timeRaised, "reset-task") { () =>
      engine.setValue(resetName, 0)
      if(engine.verbose) {
        println(s"reset dropped at ${wallTime.currentTime}")
      }
      engine.inputsChanged = true
    }
  }

  var cycleCount: Long = 0L

  /**
    * Cycles the circuit n steps (with a default of one)
    * At each step registers and memories are advanced and all other elements recomputed
    *
    * @param n cycles to perform
    */
  def step(n: Int = 1): Unit = {
    currentTreadleTester.step(n)
  }

  // scalastyle:off number.of.methods
  object Commands {
    def getOneArg(failureMessage: String, argOption: Option[String] = None): Option[String] = {
      if(args.length == 2) {
        Some(args(1))
      }
      else if(args.length == 1 && argOption.isDefined) {
        Some(argOption.get)
      }
      else {
        error(failureMessage)
        None
      }
    }
    def getTwoArgs(failureMessage: String,
                   arg1Option: Option[String] = None,
                   arg2Option: Option[String] = None
                  ): (Option[String],Option[String]) = {
      if(args.length == 3) {
        (Some(args(1)), Some(args(2)))
      }
      else if(args.length == 2) {
        (Some(args(1)), arg2Option)
      }
      else if(args.length == 1) {
        (arg1Option, arg2Option)
      }
      else {
        error(failureMessage)
        (None, None)
      }
    }
    //scalastyle:off magic.number
    def getThreeArgs(failureMessage: String,
      arg1Option: Option[String] = None,
      arg2Option: Option[String] = None,
      arg3Option: Option[String] = None
    ): (Option[String],Option[String],Option[String]) = {
      if(args.length == 4) {
        (Some(args(1)), Some(args(2)), Some(args(3)))
      }
      else if(args.length == 3) {
        (Some(args(1)), Some(args(2)), arg3Option)
      }
      else if(args.length == 2) {
        (Some(args(1)), arg2Option, arg3Option)
      }
      else if(args.length == 1) {
        (arg1Option, arg2Option, arg3Option)
      }
      else {
        error(failureMessage)
        (None, None, None)
      }
    }

    //scalastyle:off magic.number
    def getManyArgs(defaults: Option[String]*): List[Option[String]] = {
      val combined: Seq[(Option[String], Option[String])] = args.tail.map(Some(_)).zipAll(defaults, None, None)

      val result = combined.map { case (command, default) => if(command.isDefined) command else default }

      result.toList
    }

    val commands: ArrayBuffer[Command] = ArrayBuffer.empty[Command]
    commands ++= Seq(
      new Command("load") {
        def usage: (String, String) = ("load fileName", "load/replace the current firrtl file")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"load"}),
            new FileNameCompleter
          ))
        }
        def run(args: Array[String]): Unit = {
          getOneArg("load filename") match {
            case Some(fileName) => loadFile(fileName)
            case _ =>
          }
        }
      },
      new Command("script") {
        def usage: (String, String) = ("script fileName", "load a script from a text file")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"script"}),
            new FileNameCompleter
          ))
        }
        def run(args: Array[String]): Unit = {
          getOneArg("script filename") match {
            case Some(fileName) => loadScript(fileName)

            case _ =>
          }
        }
      },
      new Command("run") {
        def usage: (String, String) = ("run [linesToRun|all|list|reset]", "run loaded script")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"run"}),
            new StringsCompleter(jlist(Seq("all", "reset", "list")))
          ))
        }
        def handleList(script: Script, listArg: Option[String]): Unit = {
          val (min, max) = listArg match {
            case Some(IntPattern(intString)) =>
              val windowHalfSize = intString.toInt
              (script.currentLine + 1 - windowHalfSize, script.currentLine + 2 + windowHalfSize)
            case Some(other) =>
              console.println(s"run list parameter=$other, parameter must be an positive integer")
              (0, 0)
            case _ =>
              (0, script.length)
          }
          console.println(
            script.lines.zipWithIndex.flatMap { case (line, index) =>
              if(index >= min && index < max) {
                if (index == script.currentLine + 1) {
                  Some(Console.GREEN + f"$index%3d $line" + Console.RESET)
                }
                else {
                  Some(f"$index%3d $line")
                }
              }
              else {
                None
              }
            }.mkString("\n")
          )
        }
        // scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          currentScript match {
            case Some(script) =>
              getTwoArgs("run [lines|skip [n]|set n|all|reset|list [n]], default is 1 => run 1 line",
                arg1Option = Some("1"), arg2Option = None) match {
                case (Some("all"), _)   =>
                  console.println("run all")
                  if(script.atEnd) { script.reset() }
                  else { script.runRemaining() }
                case (Some("reset"), _) =>
                  script.reset()
                  handleList(script, Some("2"))
                case (Some("list"), listArg) =>
                  handleList(script, listArg)
                case (Some("skip"), listArg) =>
                  val skip = listArg match {
                    case Some(IntPattern(intString)) => intString.toInt
                    case _ => 1
                  }
                  script.setSkipLines(skip)
                case (Some("set"), listArg) =>
                  listArg match {
                    case Some(IntPattern(intString)) =>
                      script.setLine(intString.toInt)
                      handleList(script, Some("2"))
                    case _ =>
                      console.println("must specify set line number")
                  }
                case (Some(IntPattern(intString)), _) =>
                  val linesToRun = intString.toInt
                  script.setLinesToRun(linesToRun)
                case (None, None) =>
                  script.runRemaining()
                case (Some(arg), _) =>
                  error(s"unrecognized run_argument $arg")
              }
            case _ =>
              error(s"No current script")
          }
        }
        // scalastyle:on cyclomatic.complexity

      },
      new Command("vcd") {
        def usage: (String, String) = ("vcd [run|list|test|help]", "control vcd input file")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"vcd"}),
            new AggregateCompleter(
              new StringsCompleter(jlist(Seq("run", "inputs", "list", "test"))),
              new ArgumentCompleter(
                new StringsCompleter({"load"}),
                new FileNameCompleter
              )
            )
          ))
        }
        def run(args: Array[String]): Unit = {
          args.toList match {
            case "load" :: fileName :: _ =>
              loadVcdScript(fileName)
            case _ =>
              replVcdController match {
                case Some(controller) => controller.processListCommand(args)
                case _ => error(s"No current script")
              }
          }
        }
      },
      new Command("record-vcd") {
        def usage: (String, String) = ("record-vcd [<fileName>]|[done]", "treadle.vcd loaded script")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"record-vcd"}),
            new FileNameCompleter
          ))
        }
        def run(args: Array[String]): Unit = {
          getOneArg("treadle.vcd [fileName|done]",
            argOption = Some("out.treadle.vcd")) match {
            case Some("done")   =>
              engine.disableVCD()
            case Some(fileName) =>
              engine.makeVCDLogger(
                fileName, showUnderscored = optionsManager.treadleOptions.vcdShowUnderscored)
            case _ =>
              engine.disableVCD()
          }
        }
      },
      new Command("symbol") {
        private def peekableThings = engine.validNames.toSeq
        def usage: (String, String) = ("symbol regex", "show symbol information")

        override def completer: Option[ArgumentCompleter] = {
          if(currentTreadleTesterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({"symbol"}),
              new StringsCompleter(jlist(peekableThings))
            ))
          }
        }

        //scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          var linesShown = 0
          getOneArg("symbol regex") match {
            case Some(peekRegex) =>
              try {
                val portRegex = peekRegex.r
                val numberOfThingsPeeked = peekableThings.sorted.count { signal =>
                  portRegex.findFirstIn(signal) match {
                    case Some(_) =>
                      try {
                        val value = engine.getValue(signal)
                        val symbol = engine.symbolTable(signal)
                        if(linesShown % 50 == 0) {
                          console.println(s"${Symbol.renderHeader}")
                        }
                        linesShown += 1
                        console.println(s"${symbol.render} ${formatOutput(value)}")
                        true
                      }
                      catch { case _: Exception => false}
                    case _ =>
                      false
                  }
                }
                if(numberOfThingsPeeked == 0) {
                  console.println(s"Sorry no settable ports matched regex $peekRegex")
                }
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
                case a: AssertionError =>
                  error(s"exception ${a.getMessage}")
              }
            case _ =>
          }
        }
      },
      new Command("poke") {
        def usage: (String, String) = ("poke inputPortName value", "set an input port to the given integer value")
        override def completer: Option[ArgumentCompleter] = {
          if(currentTreadleTesterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "poke"
              }),
              new StringsCompleter(
                jlist(engine.getInputPorts ++ engine.getRegisterNames)
              )
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getTwoArgs("poke inputPortName value") match {
            case (Some(portName), Some(valueString)) =>
              try {
                val numberValue = parseNumber(valueString)
                engine.setValue(portName, numberValue)
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("rpoke") {
        private def settableThings = {
          engine.getInputPorts ++ engine.getRegisterNames
        }
        def usage: (String, String) = ("rpoke regex value", "poke value into ports that match regex")
        override def completer: Option[ArgumentCompleter] = {
          if(currentTreadleTesterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "rpoke"
              }),
              new StringsCompleter(jlist(settableThings))
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getTwoArgs("rpoke regex value") match {
            case (Some(pokeRegex), Some(valueString)) =>
              try {
                val pokeValue = parseNumber(valueString)
                val portRegex = pokeRegex.r
                val setThings = settableThings.flatMap { settableThing =>
                  portRegex.findFirstIn(settableThing) match {
                    case Some(_) =>
                      engine.setValue(settableThing, pokeValue)
                      Some(settableThing)
                    case _ => None
                  }
                }
                if(setThings.nonEmpty) {
                  console.println(s"poking value $pokeValue into ${setThings.toList.sorted.mkString(", ")}")
                }
                else {
                  console.println(s"Sorry now settable ports matched regex $pokeRegex")
                }


              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("peek") {
        def usage: (String, String) =
          ("peek componentName [offset]", "show the current value of the signal")

        override def completer: Option[ArgumentCompleter] = {
          if(currentTreadleTesterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "peek"
              }),
              new StringsCompleter(jlist(engine.validNames.toSeq))
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getTwoArgs("peek componentName", arg2Option = Some("0")) match {
            case (Some(componentName), Some(offsetString)) =>
              try {
                val offset = offsetString.headOption match {
                  case Some('b') => BigInt(offsetString.tail, 2).toInt
                  case Some('d') => BigInt(offsetString.tail, 10).toInt
                  case Some('x') => BigInt(offsetString.tail, 16).toInt
                  case Some('h') => BigInt(offsetString.tail, 16).toInt
                  case _ => offsetString.toInt
                }
                if(offset > 0) {
                  val value = engine.getValue(componentName, offset)
                  console.println(s"peek $componentName($offset) ${formatOutput(value)}")
                }
                else {
                  val value = engine.getValue(componentName)
                  console.println(s"peek $componentName ${formatOutput(value)}")
                }
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage}")
                case a: AssertionError =>
                  error(s"exception ${a.getMessage}")
              }
            case _ =>
              println(s"You must specify a signal name")
          }
        }
      },
      new Command("rpeek") {
        private def peekableThings = engine.validNames.toSeq
        def usage: (String, String) = ("rpeek regex", "show the current value of signals matching the regex")
        override def completer: Option[ArgumentCompleter] = {
          if(currentTreadleTesterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "rpeek"
              }),
              new StringsCompleter(jlist(peekableThings))
            ))
          }
        }
        //scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          getOneArg("rpeek regex") match {
            case Some(peekRegex) =>
              try {
                val portRegex = peekRegex.r
                val numberOfThingsPeeked = peekableThings.sorted.count { settableThing =>
                  portRegex.findFirstIn(settableThing) match {
                    case Some(_) =>
                      try {
                        val value = engine.getValue(settableThing)
                        console.println(s"rpeek $settableThing ${formatOutput(value)}")
                        true
                      }
                      catch { case _: Exception => false}
                    case _ =>
                      false
                  }
                }
                if(numberOfThingsPeeked == 0) {
                  console.println(s"Sorry now settable ports matched regex $peekRegex")
                }
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
                case a: AssertionError =>
                  error(s"exception ${a.getMessage}")
              }
            case _ =>
          }
        }
      },
      new Command("randomize") {
        def usage: (String, String) = ("randomize", "randomize all inputs except reset)")
        def run(args: Array[String]): Unit = {
          for(symbol <- engine.symbols) {
            try {
              val newValue = makeRandom(symbol.firrtlType)
              engine.setValue(symbol.name, newValue)
              // console.println(s"setting ${symbol.name} to $newValue")
            }
            catch {
              case e: Exception =>
                console.println(s"Error randomize: setting ${symbol.name}, error ${e.getMessage}")
            }
          }
          console.println(engine.getPrettyString)
        }
      },
      new Command("reset") {
        def usage: (String, String) = ("reset [numberOfSteps]",
          "assert reset (if present) for numberOfSteps (default 1)")
        override def completer: Option[ArgumentCompleter] = {
          if(currentTreadleTesterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "reset"
              })
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("reset [numberOfSteps]", Some("1")) match {
            case Some(numberOfStepsString) =>
              try {
                clockInfoList.headOption match {
                  case Some(clockInfo) =>
                    val extraTime = clockInfo.period * numberOfStepsString.toInt
                    reset(clockInfo.initialOffset + extraTime)
                    wallTime.runToTask("reset-task")
                  case _ =>
                    engine.setValue("reset", 1)
                    engine.advanceTime(combinationalDelay)
                    engine.setValue("reset", 0)
                }
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("radix") {
        def usage: (String, String) = ("reset [b|d|x|h]",
          "Set the output radix to binary, decimal, or hex")
        override def completer: Option[ArgumentCompleter] = {
          if(currentTreadleTesterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "radix"
              }),
              new StringsCompleter(jlist(Seq("b", "d", "h", "x")))
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("radix [b|d|h|x]", Some("d")) match {
            case Some(radix) =>
              if(Seq("b", "d", "h", "x").contains(radix.toLowerCase())) {
                outputFormat = radix
              }
              else {
                console.println(s"Unknown output radix $radix")
              }

            case _ =>
          }
        }
      },
      new Command("step") {
        def usage: (String, String) = ("step [numberOfSteps]",
          "cycle the clock numberOfSteps (default 1) times, and show state")
        def run(args: Array[String]): Unit = {
          getOneArg("step [numberOfSteps]", Some("1")) match {
            case Some(numberOfStepsString) =>
              try {
                val numberOfSteps = numberOfStepsString.toInt
                engine.timer("steps") {
                  for (_ <- 0 until numberOfSteps) {
                    engine.timer("step") {
                      step()
                    }
                  }
                }
                if(! scriptRunning) {
                  // console.println(engine.circuitState.prettyString())
                  console.println(s"step $numberOfSteps in ${engine.timer.prettyLastTime("steps")}")
                }
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("waitfor") {
        def usage: (String, String) = ("waitfor componentName value [maxNumberOfSteps]",
          "wait for particular value (default 1) on component, up to maxNumberOfSteps (default 100)")

        override def completer: Option[ArgumentCompleter] = {
          if(currentTreadleTesterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "waitfor"
              }),
              new StringsCompleter(jlist(engine.validNames.toSeq))
            ))
          }
        }

        def run(args: Array[String]): Unit = {
          getThreeArgs(
            "waitfor componentName [value] [maxNumberOfSteps]",
            arg2Option = Some("1"),
            arg3Option = Some("100")
          ) match {
            case (Some(componentName), Some(valueString), Some(maxNumberOfStepsString)) =>
              try {
                val maxNumberOfSteps = maxNumberOfStepsString.toInt
                val value = valueString.toInt

                var tries = 0
                while(tries < maxNumberOfSteps && engine.getValue(componentName) != BigInt(value)) {
                  step()
                  tries += 1
                }
                if(engine.getValue(componentName) != BigInt(value)) {
                  console.println(
                    s"waitfor exhausted $componentName did not take on" +
                      s" value ${formatOutput(value)} in $maxNumberOfSteps cycles")
                }
                else {
                  console.println(s"$componentName == value ${formatOutput(value)} in $tries cycles")
                }
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("depend") {
        def usage: (String, String) = ("depend [childrenOf|parentsOf] signal [depth] | depend compare signal1 signal2",
          "show dependency relationship to signal or between to signal")

        override def completer: Option[ArgumentCompleter] = {
          if(currentTreadleTesterOpt.isEmpty) {
            None
          }
          else {
            def peekableThings = engine.validNames.toSeq

            Some(new ArgumentCompleter(
              new StringsCompleter({ "depend"}),
              new StringsCompleter(jlist(Seq("childrenOf", "parentsOf", "compare"))),
              new StringsCompleter(jlist(peekableThings)),
              new StringsCompleter(jlist(peekableThings))
            ))
          }
        }

        def showRelated(direction: String, digraph: DiGraph[Symbol], symbolName: String, maxDepth: Int) {
          val table = engine.symbolTable
          val symbol = engine.symbolTable(symbolName)
          val symbolsAtDepth = Array.fill(maxDepth + 1) {
            new mutable.HashSet[Symbol]
          }

          walkGraph(symbol, depth = 0)

          def walkGraph(symbol: Symbol, depth: Int): Unit = {
            symbolsAtDepth(depth) += symbol

            if (depth < maxDepth) {
              digraph.getEdges(symbol).toSeq.sortBy(_.name).foreach { childSymbol =>
                walkGraph(childSymbol, depth + 1)
              }
              if (table.isRegister(symbol.name)) {
                walkGraph(table(SymbolTable.makeRegisterInputName(symbol)), depth + 1)
              }
            }
          }

          val showDepth = symbolsAtDepth.count(_.nonEmpty)
          for (depth <- 0 until showDepth) {
            println(s"$direction signals at distance $depth")
            println(symbolsAtDepth(depth).toSeq.map(_.name).sorted.mkString("\n"))
          }
        }

        def run(args: Array[String]): Unit = {
          val table = engine.symbolTable
          val parsedArgs = getManyArgs(Some("parentsOf"), None, Some("4"))
          parsedArgs match {
            case Some("parentsOf") :: Some(signal1) :: Some(depth) :: _ =>
              showRelated("Parents", table.parentsOf, signal1, maxDepth = depth.toInt)
            case Some("parentsOf") :: _ =>
              console.println(s"""You must specify a signal with command "depend parentsOf" """)
            case Some("childrenOf") :: Some(signal1) :: Some(depth) :: _ =>
              showRelated("Children", table.childrenOf, signal1, maxDepth = depth.toInt)
            case Some("childrenOf") :: _ =>
              console.println(s"""You must specify a signal with command "depend childrenOf" """)
            case Some("compare") :: Some(signal1) :: Some(signal2) :: _ =>
              val (symbol1, symbol2) = (table(signal1), table(signal2))
              def showPath(direction: String, digraph: DiGraph[Symbol]) {
                try {
                  val path = digraph.path(symbol1, symbol2)
                  console.println(s"$signal1 is a $direction of $signal2 via")
                  path.foreach { symbol =>
                    console.println(f"${symbol.name}")
                  }
                }
                catch {
                  case _: firrtl.graph.PathNotFoundException =>
                    console.println(s"$signal1 is not a $direction of $signal2")
                }
              }
              showPath("parent", table.parentsOf)
              showPath("child", table.childrenOf)
            case _ =>
              println(usage)

          }
        }
      },
      new Command("show") {
        def usage: (String, String) = ("show [state|inputs|outputs|firrtl|lofirrtl]", "show useful things")
        override def completer: Option[ArgumentCompleter] = {
          if(currentTreadleTesterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "show"}),
              new StringsCompleter(jlist(Seq("state", "inputs", "outputs", "firrtl", "lofirrtl")))
            ))
          }
        }

        def run(args: Array[String]): Unit = {
          getOneArg("", Some("lofirrtl")) match {
            case Some("lofirrtl") =>
              console.println(ToLoFirrtl.lower(engine.ast, optionsManager).serialize)
            case Some("input") | Some("firrtl") =>
              console.println(engine.ast.serialize)
            case Some("inputs") =>
              console.println(engine.symbolTable.inputPortsNames.toSeq.sorted.mkString("\n"))
            case Some("outputs") =>
              console.println(engine.symbolTable.outputPortsNames.toSeq.sorted.mkString("\n"))
            case _ =>
              console.println(engine.getPrettyString)
          }
        }
      },
      new Command("display") {
        def usage: (String, String) = ("display signal[, signal, ...]", "show computation of symbols")
        override def completer: Option[ArgumentCompleter] = {
          if(currentTreadleTesterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "display"}),
              new StringsCompleter(jlist(engine.symbolTable.keys.toSeq))
            ))
          }
        }

        def run(args: Array[String]): Unit = {
          getOneArg("", Some("state")) match {
            case Some(symbolList) =>
              if(currentTreadleTesterOpt.isDefined) {
                console.println(engine.renderComputation(symbolList, outputFormat))
              }
            case _ =>
              console.println(engine.getPrettyString)
          }
        }
      },
      new Command("info") {
        def usage: (String, String) = ("info", "show information about the circuit")
        def run(args: Array[String]): Unit = {
          console.println(engine.getInfoString)
        }
      },
//      new Command("timing") {
//        def usage: (String, String) = ("timing [clear|bin]", "show the current timing state")
//        override def completer: Option[ArgumentCompleter] = {
//          if(currentTreadleTesterOpt.isEmpty) {
//            None
//          }
//          else {
//            Some(new ArgumentCompleter(
//              new StringsCompleter({ "timing"}),
//              new StringsCompleter(jlist(Seq("clear", "bin")))
//            ))
//          }
//        }
//        // scalastyle:off cyclomatic.complexity
//        def run(args: Array[String]): Unit = {
//          getOneArg("", Some("")) match {
//            case Some("clear") => engine.timer.clear()
//            case Some("bin") =>
//              val names = engine.dependencyGraph.validNames -- engine.dependencyGraph.inputPorts
//
//              val countPerName = new scala.collection.mutable.HashMap[Long, Long]
//              names.foreach { name =>
//                engine.timer.timingLog.get(name).foreach { t =>
//                  if(! countPerName.contains(t.events)) {
//                    countPerName(t.events) = 1
//                  }
//                  else {
//                    countPerName(t.events) = countPerName(t.events) + 1
//                  }
//                }
//              }
//              countPerName.keys.toSeq.sorted.foreach { count: Long =>
//                console.println(f"$count ${countPerName(count)}")
//              }
//            case _ =>
//              val names = engine.dependencyGraph.validNames -- engine.dependencyGraph.inputPorts
//
//              val sortedNames = names.toSeq.sortWith { case (a, b) =>
//                (engine.timer.timingLog.get(a), engine.timer.timingLog.get(b)) match {
//                  case (Some(t1), Some(t2)) =>
//                    if(t1.events == t2.events) {
//                      a < b
//                    }
//                    else {
//                      t1.events < t2.events
//                    }
//                  case (Some(_), None)      => false
//                  case (None, Some(_))      => true
//                  case _                    => a < b
//                }
//              }
//              for (name <- sortedNames) {
//                console.println(f"$name%-20s ${engine.timer.prettyEntryForTag(name)}")
//              }
//              console.println(f"${"Total"}%-20s ${engine.timer.prettyEntry(engine.timer.totalEvent)}")
//          }
//        }
//      },
      new Command("verbose") {
        def usage: (String, String) = ("verbose [true|false|toggle]",
          "set evaluator verbose mode (default toggle) during dependency evaluation")
        override def completer: Option[ArgumentCompleter] = {
          if(currentTreadleTesterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "verbose"}),
              new StringsCompleter(jlist(Seq("true", "false", "toggle")))
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("verbose must be followed by true false or toggle", Some("toggle")) match {
            case Some("toggle") => engine.setVerbose(! engine.verbose)
            case Some("true")   => engine.setVerbose()
            case Some("false")  => engine.setVerbose(false)
            case _ =>
          }
          console.println(s"evaluator verbosity is now ${engine.verbose}")
        }
      },
      new Command("snapshot") {
        def usage: (String, String) = ("snapshot",
          "save state of engine")
        override def completer: Option[ArgumentCompleter] = {
          if(currentTreadleTesterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "snapshot"}),
              new FileNameCompleter
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("snapshot requires a file name") match {
            case Some(fileName) =>
              val writer = new PrintWriter(new File(fileName))
              writer.write(engine.dataStore.serialize)
              writer.close()
            case _ =>
          }
          console.println(engine.dataStore.serialize)
        }
      },
      new Command("restore") {
        def usage: (String, String) = ("restore",
          "save state of engine")
        override def completer: Option[ArgumentCompleter] = {
          if(currentTreadleTesterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "restore"}),
              new FileNameCompleter
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("snapshot requires a file name") match {
            case Some(fileName) =>
              val jsonSource = Source.fromFile(new File(fileName)).getLines().mkString("\n")
              engine.dataStore.deserialize(jsonSource)
            case _ =>
          }
        }
      },
//      new Command("allow-cycles") {
//        def usage: (String, String) = ("allow-cycles [true|false|toggle]",
//          "set evaluator allow combinational loops (could cause correctness problems")
//        override def completer: Option[ArgumentCompleter] = {
//          if(currentTreadleTesterOpt.isEmpty) {
//            None
//          }
//          else {
//            Some(new ArgumentCompleter(
//              new StringsCompleter({ "allow-cycles"}),
//              new StringsCompleter(jlist(Seq("true", "false", "toggle")))
//            ))
//          }
//        }
//        def run(args: Array[String]): Unit = {
//          getOneArg("allow-cycles must be followed by true false or toggle", Some("toggle")) match {
//            case Some("toggle") =>
//              engine.evaluator.allowCombinationalLoops = ! engine.evaluator.allowCombinationalLoops
//            case Some("true")   => engine.evaluator.allowCombinationalLoops = true
//            case Some("false")  => engine.evaluator.allowCombinationalLoops = false
//            case _ =>
//          }
//          console.println(s"evaluator allow combinational loops is now ${engine.evaluator.evaluateAll}")
//        }
//      },
      new Command("help") {
        def usage: (String, String) = ("help", "show available commands")
        def run(args: Array[String]): Unit = {
          val maxColumn1Width = Commands.commands.map(_.usage._1.length).max + 2
          Commands.commands.foreach { command =>
            val (column1, column2) = command.usage
            terminal.getWidth

            console.println(s"$column1${" "*(maxColumn1Width - column1.length)} $column2")
          }
        }
      },
      new Command("quit") {
        def usage: (String, String) = ("quit", "exit the engine")
        def run(args: Array[String]): Unit = {
          if(! history.isEmpty) {
            history.removeLast()
          }
          done = true
        }
      }
    )
    val commandMap: Map[String, Command] = commands.map(command => command.name -> command).toMap
  }
  //scalastyle:on

  def buildCompletions(): Unit = {
    console.setCompletionHandler(new CandidateListCompletionHandler {})
    Commands.commands.flatMap { command =>
      command.completer
    }.foreach { completer =>
      console.addCompleter(completer)
    }
  }

  /**
    * gets the next line from either the current executing script or from the console.
    * Strips comments from the line, may result in empty string, command parser is ok with that
 *
    * @return
    */
  def getNextLine: String = {
    val rawLine = currentScript match {
      case Some(script) =>
        script.getNextLineOption match {
          case Some(line) =>
            console.println(s"[${script.currentLine}:${script.fileName}] $line")
            line
          case _ =>
            console.readLine()
        }
      case _ =>
        console.readLine()
    }
    if(rawLine == null) {
      history.add("quit")
      "quit"
    }
    else {
      rawLine.split("#").head
    }
  }

  def scriptRunning: Boolean = {
    currentScript match {
      case Some(script) => script.hasNext
      case _            => false
    }
  }

  //scalastyle:off method.length
  def run(): Unit = {
    console.setPrompt("treadle>> ")

    try {
      if (replConfig.firrtlSource.nonEmpty) {
        loadSource(replConfig.firrtlSource)
      }
      else if (replConfig.firrtlSourceName.nonEmpty) {
        loadFile(replConfig.firrtlSourceName)
      }
      if (replConfig.scriptName.nonEmpty) {
        loadScript(replConfig.scriptName)
      }
      if (replConfig.useVcdScript) {
        loadVcdScript(optionsManager.getVcdFileName)
      }
    }
    catch {
      case t: TreadleException =>
        console.println(s"Startup: Treadle Exception ${t.getMessage}")
      case _: CyclicException =>
      case e: Throwable =>
        throw e
    }
    buildCompletions()

    if(replConfig.runScriptAtStart) {
      currentScript match {
        case Some(script) =>
          script.reset()
          script.runRemaining()
        case None =>
          console.println(s"Error: fr-run-script-at-startup set, with no script file")
      }
    }

    while (! done) {
      try {
        val line = getNextLine

        line.split(""";""").foreach { subLine =>

          args = subLine.trim.split(" +")

          if (args.length > 0) {
            if (Commands.commandMap.contains(args.head)) {
              Commands.commandMap(args.head).run(args.tail)
            }
            else {
              if (subLine.nonEmpty) error(s"unknown command $subLine, try help")
            }
          }
          else {
            error(s"unknown command: $subLine")
          }
        }
      }
      catch {
        case ie: TreadleException =>
          console.println(s"Treadle Exception occurred: ${ie.getMessage}")
          ie.printStackTrace()
        case _: CyclicException =>
        case e: NullPointerException =>
          error(s"Null pointer exception, please file an issue\n ${e.getMessage}")
          e.printStackTrace()
        case e: Exception =>
          console.println(s"Exception occurred: ${e.getMessage}")
          e.printStackTrace()
      }
    }

    console.println(s"saving history ${history.size()}")
    console.flush()
    history.flush()
    console.shutdown()
    terminal.restore()
  }

  def error(message: String): Unit = {
    console.println(s"Error: $message")
  }

  def jlist(list: Seq[String]): java.util.List[String]= {
    val array = ArrayBuffer.empty[String]
    array ++= list
    array.asJava
  }
}

object TreadleRepl {
  def execute(optionsManager: TreadleOptionsManager with HasReplConfig): Unit = {
    val repl = new TreadleRepl(optionsManager)
    repl.run()
  }

  def main(args: Array[String]): Unit = {
    val optionsManager = new TreadleOptionsManager with HasReplConfig

    if(optionsManager.parse(args)) {
      Logger.makeScope(optionsManager) {
        val repl = new TreadleRepl(optionsManager)
        repl.run()
      }
    }
  }
}
