circuit AbsCircuitWithDelays : 
  module AbsCircuitWithDelays : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt<10>, out : UInt<11>}
    
    clock is invalid
    reset is invalid
    io is invalid

    io.out <= add(io.in, io.in)
