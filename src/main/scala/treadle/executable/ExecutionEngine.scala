// See LICENSE for license details.

package treadle.executable

import firrtl.PortKind
import firrtl.ir.{Circuit, NoInfo}
import firrtl.transforms.DontCheckCombLoopsAnnotation
import treadle._
import treadle.chronometry.{Timer, UTC}
import treadle.utils.{Render, ToLoFirrtl}
import treadle.vcd.VCD

//scalastyle:off magic.number number.of.methods
class ExecutionEngine(
    val ast             : Circuit,
    val optionsManager  : HasTreadleSuite,
    val symbolTable     : SymbolTable,
    val dataStore       : DataStore,
    val scheduler       : Scheduler,
    val expressionViews : Map[Symbol, ExpressionView],
    val wallTime        : UTC
) {
  val cycleTimeIncrement = 500

  var vcdOption: Option[VCD] = None
  var vcdFileName: String = ""

  val expressionViewRenderer = new ExpressionViewRenderer(
    dataStore,
    symbolTable,
    expressionViews
  )

  var verbose: Boolean = false
  setVerbose(optionsManager.treadleOptions.setVerbose)

  var inputsChanged: Boolean = false

  /* Default dataStore plugins */

  dataStore.addPlugin(
    "show-assigns", new ReportAssignments(this),
    enable = optionsManager.treadleOptions.setVerbose
  )

  dataStore.addPlugin(
    "show-computation",
    new RenderComputations(this, optionsManager.treadleOptions.symbolsToWatch),
    enable = optionsManager.treadleOptions.symbolsToWatch.nonEmpty
  )

  def setLeanMode(): Unit = {
    val canBeLean = ! (verbose || dataStore.hasEnabledPlugins)
    scheduler.setLeanMode(canBeLean)
    scheduler.setVerboseAssign(verbose)
  }

  /**
    * turns on evaluator debugging. Can make output quite
    * verbose.
    *
    * @param isVerbose  The desired verbose setting
    */
  def setVerbose(isVerbose: Boolean = true): Unit = {
    verbose = isVerbose
    setLeanMode()
    dataStore.plugins.get("show-assigns") match {
      case Some(plugin) => plugin.setEnabled(verbose)
      case _ => None
    }
    scheduler.setVerboseAssign(isVerbose)
  }

  val timer = new Timer

  if(verbose) {
    if (scheduler.orphanedAssigns.nonEmpty) {
      Render.headerBar(s"Executing static assignments", offset = 8)
    }
    else {
      Render.headerBar(s"No static assignments", offset = 8)
    }
  }
  scheduler.executeOrphanedAssigns()
  if(verbose) {
    if(scheduler.orphanedAssigns.nonEmpty) {
      Render.headerBar(s"Finished executing static assignments", offset = 8)
    }
  }

  val memoryInitializer = new MemoryInitializer(this)

  def makeVCDLogger(fileName: String, showUnderscored: Boolean): Unit = {
    val vcd = VCD(ast.main, showUnderscoredNames = showUnderscored)

    symbolTable.instanceNames.foreach { name =>
      vcd.scopeRoot.addScope(name)
    }
    vcd.timeStamp = -1
    symbolTable.symbols.foreach { symbol =>
      vcd.wireChanged(symbol.name, dataStore(symbol), symbol.bitWidth)
    }
    vcd.timeStamp = 0

    vcdOption = Some(vcd)
    vcdFileName = fileName

    val vcdPlugIn = new VcdHook(this, vcd)
    dataStore.addPlugin(ExecutionEngine.VCDHookName, vcdPlugIn, enable = true)
  }

  def disableVCD(): Unit = {
    writeVCD()
    vcdOption = None
    vcdFileName = ""
    dataStore.removePlugin(ExecutionEngine.VCDHookName)
  }

  def writeVCD(): Unit = {
    vcdOption.foreach { vcd =>
      vcd.write(vcdFileName)
    }
  }

  def renderComputation(symbolNames: String, outputFormat: String = "d"): String = {
    val renderer = new ExpressionViewRenderer(dataStore, symbolTable, expressionViews)

    val symbols = symbolNames.split(",").map(_.trim).flatMap { s => symbolTable.get(s) }.distinct

    symbols.flatMap { symbol =>
      expressionViews.get(symbol) match {
        case Some(_) =>
          Some(s"${renderer.render(symbol, wallTime.currentTime, outputFormat = outputFormat)}")
        case _ => None
      }
    }.mkString("\n")
  }

  private def runAssigns(): Unit = {
    try {
      scheduler.executeCombinationalAssigns()
      if(lastStopResult.isDefined) {
        writeVCD()
        val stopKind = if(lastStopResult.get > 0) { "Failure Stop" } else { "Stopped" }
        throw StopException(s"$stopKind: result ${lastStopResult.get}")
      }
    }
    catch {
      case throwable: Throwable =>
        writeVCD()
        throw throwable
    }
  }

  def getValue(name: String, offset: Int = 0): BigInt = {
    assert(symbolTable.contains(name),
      s"""Error: getValue("$name") : argument is not an element of this circuit""")

    if(inputsChanged) {
      if(verbose) {
        Render.headerBar(s"peeking", offset = 8)
      }
      inputsChanged = false
      runAssigns()
    }

    val symbol = symbolTable(name)
    if(offset == 0) {
      symbol.normalize(dataStore(symbol))
    }
    else {
      if(offset - 1 > symbol.slots) {
        throw TreadleException(s"get value from ${symbol.name} offset $offset > than size ${symbol.slots}")
      }
      symbol.normalize(dataStore.getValueAtIndex(symbol.dataSize, index = symbol.index + offset))
    }
  }

  /**
    * Update the dataStore with the supplied information.
    * IMPORTANT: This should never be used internally.
    *
    * @param name  name of value to set
    * @param value new concrete value
    * @param force allows setting components other than top level inputs
    * @param registerPoke changes which side of a register is poked
    * @return the concrete value that was derived from type and value
    */
  //scalastyle:off method.length
  def setValue(
    name: String,
    value: BigInt,
    force: Boolean = true,
    registerPoke: Boolean = false,
    offset: Int = 0
  ): BigInt = {
    if(! symbolTable.contains(name)) {
      throw TreadleException(s"setValue: Cannot find $name in symbol table")
    }
    val symbol = symbolTable(name)

    inputsChanged = true

    if(!force) {
      assert(symbol.dataKind == PortKind,
        s"Error: setValue($name) not on input, use setValue($name, force=true) to override")
      return Big0
    }

    val adjustedValue = symbol.valueFrom(value)
    if(offset == 0) {
      if(verbose) {
        if(! inputsChanged) {
          Render.headerBar("Poking")
        }
        println(s"${symbol.name} <= $value")
      }
      dataStore.update(symbol, adjustedValue)
      vcdOption.foreach { vcd =>
        vcd.wireChanged(symbol.name, dataStore(symbol), symbol.bitWidth)
      }
    }
    else {
      if(offset - 1 > symbol.slots) {
        throw TreadleException(s"get value from ${symbol.name} offset $offset > than size ${symbol.slots}")
      }
      if(verbose) {
        if(! inputsChanged) {
          Render.headerBar("Poking")
        }

        println(s"${symbol.name}($offset) <= $value from tester")
      }
      dataStore.setValueAtIndex(symbol.dataSize, symbol.index + offset, value)
    }

    value
  }

  def isRegister(name: String): Boolean = {
    symbolTable.registerNames.contains(name)
  }

  def getRegisterNames: Seq[String] = {
    symbolTable.registerNames.toSeq
  }

  def getInputPorts: Seq[String] = {
    symbolTable.inputPortsNames.toSeq
  }

  def getOutputPorts: Seq[String] = {
    symbolTable.outputPortsNames.toSeq
  }

  def isInputPort(name: String): Boolean = {
    symbolTable.inputPortsNames.contains(name)
  }

  def isOutputPort(name: String): Boolean = {
    symbolTable.outputPortsNames.contains(name)
  }

  def validNames: Iterable[String] = symbolTable.keys
  def symbols: Iterable[Symbol] = symbolTable.symbols

  def evaluateCircuit(): Unit = {
    if(inputsChanged) {
      inputsChanged = false

      if(verbose) {
        Render.headerBar(s"combinational evaluate", offset = 8)
      }
      runAssigns()
      if(verbose) {
        Render.headerBar(s"done combinational evaluate", offset = 8)
      }

    }
  }

  def advanceTime(increment: Long): Unit = {
    if(increment > 0) {
      if(inputsChanged) {
        evaluateCircuit()
      }
//      wallTime.advance(increment)
    }
  }

  private val stopHappenedSymbolOpt = symbolTable.get(StopOp.stopHappenedName)
  /**
    * returns that value specified by a StopOp when
    * its condition is satisfied.  Only defined when
    * circuit is currently stopped.
    * @return
    */
  def lastStopResult: Option[Int] = {
    stopHappenedSymbolOpt match {
      case Some(hasStoppedSymbol) =>
        val stopValue = dataStore(hasStoppedSymbol).toInt
        if (stopValue > 0) {
          Some(stopValue - 1)
        }
        else {
          None
        }
      case _ =>
        None
    }
  }

  /**
    * Is the circuit currently stopped.  StopOp throws a
    * Stop
    * @return
    */
  def stopped: Boolean = {
    lastStopResult.isDefined
  }

  def fieldsHeader: String = {
    "Buf " +
      symbolTable.keys.toArray.sorted.map { name =>
        val s = name.takeRight(9)
        f"$s%10.10s"
      }.mkString("")
  }

  def header: String = {
    fieldsHeader
  }

  def dataInColumns: String = {
    val keys = symbolTable.keys.toArray.sorted

    ("-" * fieldsHeader.length) + "\n" +
        keys.map { name =>
          val symbol = symbolTable(name)
          val value = symbol.normalize(dataStore(symbolTable(name)))
          f" $value%9.9s" }.mkString("") + "\n" +
        ("-" * fieldsHeader.length)
  }

  def getInfoString: String = "Info"  //TODO (chick) flesh this out
  def getPrettyString: String = {
    header + "\n" +
    dataInColumns
  }

  trait ClockToggle {
    def raiseClock(): Unit = {}
    def lowerClock(): Unit = {}
  }

  class NullToggler extends ClockToggle

  def makeUpToggler(symbol: Symbol): Assigner = {
    val assigner = dataStore.AssignInt(symbol, GetIntConstant(1).apply, NoInfo)

    if(vcdOption.isDefined) assigner.setLeanMode(false)
    assigner.setVerbose(verbose)
    assigner
  }

  def makeDownToggler(symbol: Symbol): Assigner = {
    val assigner = dataStore.AssignInt(symbol, GetIntConstant(0).apply, NoInfo)

    if(vcdOption.isDefined) assigner.setLeanMode(false)
    assigner.setVerbose(verbose)
    assigner
  }
}

object ExecutionEngine {

  val VCDHookName = "log-vcd"
  //scalastyle:off method.length
  /**
    * Construct a Firrtl Execution engine
    * @param input           a Firrtl text file
    * @param optionsManager  options that control configuration and behavior
    * @return                the constructed engine
    */
  def apply(
    input          : String,
    optionsManager : HasTreadleSuite = new TreadleOptionsManager,
    wallTime       : UTC
  ): ExecutionEngine = {

    val t0 = System.nanoTime()

    val treadleOptions: TreadleOptions = optionsManager.treadleOptions

    val ast = firrtl.Parser.parse(input.split("\n").toIterator)
    val verbose: Boolean = treadleOptions.setVerbose
    val blackBoxFactories: Seq[ScalaBlackBoxFactory] = treadleOptions.blackBoxFactories
    val timer = new Timer

    val loweredAst: Circuit = if(treadleOptions.lowCompileAtLoad) {
      if(treadleOptions.allowCycles) {
        optionsManager.firrtlOptions = optionsManager.firrtlOptions.copy(
          annotations = optionsManager.firrtlOptions.annotations :+ DontCheckCombLoopsAnnotation
        )
      }
      ToLoFirrtl.lower(ast, optionsManager)
    } else {
      ast
    }

    if(treadleOptions.showFirrtlAtLoad) {
      println("LoFirrtl" + "=" * 120)
      println(loweredAst.serialize)
    }

    val symbolTable: SymbolTable = timer("Build Symbol Table") {
      SymbolTable(loweredAst, blackBoxFactories, treadleOptions.allowCycles)
    }

    val dataStoreAllocator = new DataStoreAllocator

    symbolTable.allocateData(dataStoreAllocator)

    val dataStore = DataStore(treadleOptions.rollbackBuffers, dataStoreAllocator)

    if(verbose) {
      println(s"Symbol table:\n${symbolTable.render}")
    }

    val scheduler = new Scheduler(symbolTable)

    val compiler = new ExpressionCompiler(symbolTable, dataStore, scheduler, treadleOptions, blackBoxFactories)

    timer("Build Compiled Expressions") {
      compiler.compile(loweredAst, blackBoxFactories)
    }

    val expressionViews: Map[Symbol, ExpressionView] = ExpressionViewBuilder.getExpressionViews(
      symbolTable, dataStore, scheduler,
      treadleOptions.validIfIsRandom,
      loweredAst, blackBoxFactories)

    scheduler.organizeAssigners()

    val executionEngine =
      new ExecutionEngine(ast, optionsManager, symbolTable, dataStore, scheduler, expressionViews, wallTime)

    executionEngine.dataStore.setExecutionEngine(executionEngine)

    if(verbose) {
      println(s"\n${scheduler.render(executionEngine)}")
      scheduler.setVerboseAssign(verbose)
    }

    executionEngine.inputsChanged = true

    val t1 = System.nanoTime()
    val total_seconds = (t1 - t0).toDouble / Timer.TenTo9th
    println(s"file loaded in $total_seconds seconds, ${symbolTable.size} symbols, " +
      s"${scheduler.combinationalAssigns.size} statements")

    executionEngine.memoryInitializer.initializeMemoriesFromFiles()

    executionEngine
  }
}
